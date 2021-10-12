import TCCSim.Script as Script
import time

ma_scratch_file = ".\\Messages\\MA_Forw_Scratch_Auto_Dist.xml"
ma_ext_repeat_file = ".\\Messages\\MA_Ext_Auto_Dist.xml"

# Constant distance to Ma End
ma_len = 100000

# Initial MA start and end
track_ma_start = 1
pos_ma_start = 50000
track_ma_end = 1
pos_ma_end = ma_len

# Initial Track to add
add_track = track_ma_end + 2

# Send new extension every 100m
dist_between_extensions = 10000

# Each track is 2000m
track_length = 200000

# Number of tracks
number_of_tracks = 5

# Balise ID start
balise_id = 10000

old_pos = 0
old_track = 1

current_pos = 0
current_track = 1

ctr = 0
atp_restarted = 0


# Wait for connection
###########################################
m = engine.GetMessage()
while (m == None):
    m = engine.GetMessage()

# First, Send MA from Scratch
print "Sending " + ma_scratch_file
engine.SendMessage(ma_scratch_file)   
 

print "----------- MA Ext Repeat -------------"
while (current_track <= number_of_tracks):

    m = engine.GetMessage()
    
    if (m == None):
        atp_restarted = 1
        break
    
    print "name = " + m.Name
    
    if (m.Name == "PositionReport"):
       
        current_pos = int(engine.GetFields(m.Data, "D_POSITION")[1])
        current_track = int(engine.GetFields(m.Data, "NID_TRACK")[1])
        
        #print "current_pos = " + str(current_pos)
        #print "old_pos = " + str(old_pos)
       
        #print "current_track = " + str(current_track)
        #print "old_track = " + str(old_track)
        #print "---------------------"
        
            
        # Travelled distance above threshold to send new MA?
        if ((((current_pos - old_pos) > dist_between_extensions) and (old_track == current_track)) or
            ((old_track < current_track) and (((track_length - old_pos) + current_pos) > dist_between_extensions))):
            
            # The extra distance to add for each extension
            #if (old_track == current_track):
            #    # Just add the extra distance
            #    adjust_dist_for_extension = (current_pos - old_pos) - dist_between_extensions
            #else:
            #    # Position was from previous track
            #    adjust_dist_for_extension =  ((track_length - old_pos) + current_pos) - dist_between_extensions
                            
            old_pos = current_pos
            old_track = current_track
           
            # Load and update template for MA extension with proper values.
            ma_ext_repeat = engine.LoadMessage(ma_ext_repeat_file)
            
            # MA End
            engine.SetF(ma_ext_repeat,"NID_TRACK", 0, str(track_ma_end))
            engine.SetF(ma_ext_repeat,"D_POSITION", 0, str(pos_ma_end))
            
            # MA Start
            engine.SetF(ma_ext_repeat,"NID_TRACK", 1, str(track_ma_start))
            engine.SetF(ma_ext_repeat,"D_POSITION", 1, str(pos_ma_start))
    
            # Always Add Track
            engine.SetF(ma_ext_repeat,"NID_TRACK", 2, str(track_ma_end))
            engine.SetF(ma_ext_repeat,"D_POSITION", 2, str(0))
            engine.SetF(ma_ext_repeat,"D_POSITION", 3, str(track_length))
            engine.SetF(ma_ext_repeat,"NID_PREVIOUS_TRACK", 0, str(track_ma_end-1))
    
            # Add a Balise -30m from MA end
            engine.SetF(ma_ext_repeat,"NID_TRACK", 3, str(track_ma_end))
            engine.SetF(ma_ext_repeat,"D_POSITION", 4, str(max(100, pos_ma_end - 3000))) # Balise Pos cannot be <= 0
            engine.SetF(ma_ext_repeat,"NID_BG", 0, str(balise_id))
            balise_id += 1
            
            # Add 1 Gradient -40m from MA end
            engine.SetF(ma_ext_repeat,"NID_TRACK", 4, str(track_ma_end))
            engine.SetF(ma_ext_repeat,"D_POSITION", 5, str(max(200, pos_ma_end - 4000))) # Gradient Pos cannot be <= 0
            engine.SetF(ma_ext_repeat,"G_GRADIENT", 0, str(1))
            
            # Add 2 Gradient -50m from MA end
            engine.SetF(ma_ext_repeat,"NID_TRACK", 5, str(track_ma_end))
            engine.SetF(ma_ext_repeat,"D_POSITION", 6, str(max(300, pos_ma_end - 5000))) # Gradient Pos cannot be <= 0
            engine.SetF(ma_ext_repeat,"G_GRADIENT", 1, str(2))

            # Add 3 Gradient -60m from MA end
            engine.SetF(ma_ext_repeat,"NID_TRACK", 6, str(track_ma_end))
            engine.SetF(ma_ext_repeat,"D_POSITION", 7, str(max(400, pos_ma_end - 6000))) # Gradient Pos cannot be <= 0
            engine.SetF(ma_ext_repeat,"G_GRADIENT", 2, str(3))

            
            print "Sending MA Extension:" + str(ma_ext_repeat)   
            engine.SendMessage(ma_ext_repeat)
    
            # Remember number of extensions to show at end
            ctr += 1
    
            # Calculate Next MA Start/End
            pos_ma_start = pos_ma_end
            pos_ma_end = current_pos + ma_len
            
            track_ma_start = track_ma_end
            
            # Time to change track?
            if (pos_ma_end > track_length):
            
                track_ma_end = current_track + 1    
                
                # Remove the length of one track from previous calculation if track has changed
                pos_ma_end -= track_length
                
                
            
            # Track to add in MA is always 2 tracks ahead        
            add_track = track_ma_end + 2
   
print "Result: " + str(ctr) + " extensions sent to AOS."

	
