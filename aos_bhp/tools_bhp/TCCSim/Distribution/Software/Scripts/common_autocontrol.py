#!/usr/bin/env
import sys
import TCCSim.Script as Script
import time
import config

goto_Send_MA_1 = 0

'''@Author                  Sanjeev Pandita
   @Description             Created and Updated the Code with FFFIS Scenarios '''

def login_seq(engine):
    while (not config.atp_restarted):
        m = engine.GetMessage()
        if (m == None):
            config.atp_restarted = 1
            break
        if (m.Name == "DriverInformation"):
            print "Driver logged in"
            break
        if (m.Name == "PositionReport"):
            print "Sending Login command to AOS"
            engine.sendCommandToAutoControl("DMILogin AK AK")
            
        while(not(engine.getAutoControlResponse())):  
            print "Waiting for the response"
    
            b_train_status = engine.GetFields(m.Data, "B_TRAIN_CORE_STATUS")
            if (b_train_status != None):
                if ((((int)(b_train_status[0]) & 0x40) != 0) and (((int)(b_train_status[0]) & 0x200) == 0)):
                    print "Already registered, try to sent MA_1"
                    goto_Send_MA_1 = 1
                    break

# Send TSetup for 10 Cars to ATP
###########################################
def isStartUpMessageRec(engine):
     retVal = 0
     if (goto_Send_MA_1  == 0):
         while (not config.atp_restarted ):
            m = engine.GetMessage()
            print ("m.name: ",m.Name)
            #print ("m.data: ",m.Data)
            if (m == None):
                config.atp_restarted = 1
                print "ATP Restarting"
                break

            if (m.Name == "StartUpMessage"):
                print "StartUpMessage Received"
                retVal = 1
                break
	         
            if (m.Name == "PositionReport"):
                curr_mode = engine.GetFields(m.Data,"Q_ATP_MODE")
                if (curr_mode != None):
                    print ("curr_mode: ",curr_mode)
                    int_curr_mode = int(curr_mode[0])
                    if (int_curr_mode == 2):
                        print "sending Accept Cars connected on B Side"
                        engine.sendCommandToAutoControl("DMIselect acceptcarsonbside")
						
                        while(not(engine.getAutoControlResponse())):  
                            print "Waiting for the response"
    
                    else:
                        print "sending select configuration"
                        engine.sendCommandToAutoControl("DMIselect config")	

                        while(not(engine.getAutoControlResponse())):  
                            print "Waiting for the response"						
     return retVal

# MA ACK Message
###########################################
def maAckStatus(engine):
    result = 0
    print "Waiting for Message Acknowledge.."
    while (not config.atp_restarted):
        m = engine.GetMessage()
        if (m == None):
            config.atp_restarted = 1
            break
        if (m.Name == "PositionReport"):
            if (engine.IsFieldPresent(m.Data, "Q_MESSAGE_STATUS")):
                b_ack_status = engine.GetFields(m.Data, "Q_MESSAGE_STATUS")
                if (b_ack_status != None):
                    if ((int)(b_ack_status[0]) == 1):
                        print "MA ACK : Message Accepted"
                        result = 1
                        break
                    else:
                        print "MA ACK : Message Not Accepted"
                        result = 0
                        break
    return result

# Normal sequences
###########################################
def normal_sequences(engine):

    count = config.Repetitions

    # Wait for ATP to become "Idling"
    ###########################################
    if (goto_Send_MA_1  == 0):
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                q_atp_mode = engine.GetFields(m.Data, "Q_ATP_MODE")
                if (q_atp_mode != None):
                    if ((int)(q_atp_mode[0]) == 5):
                        print "FullATP"
                        if (config.UseATO == 1):
                            print "Wait for ATO to be in Automatic"
                        break

    # Wait for ATO to become "ATOAutomatic"
    ###########################################
    if (config.UseATO == 1):
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                atoMode = engine.GetFields(m.Data, "Q_ATO_MODE")
                if (atoMode != None):
                    if ((int)(atoMode[0]) == 2):
                        print "ATO in Supervised"
                        break
                    if ((int)(atoMode[0]) == 3):
                        print "ATO in Automatic"
                        break
                        

    # Run MA-loop requested number of times
    ###########################################
    firstMALoop = 1
    while (count > 0 and not config.atp_restarted):
        count = count - 1
        if (config.Repetitions > 1):
            print "Run " + str(config.Repetitions - count) + " of " + str(config.Repetitions)
        
        if((config.Repetitions - count) == 1):
            # Send MA_1
            ###########################################
            if (config.atp_restarted == 0):
                print "Sending " + config.MA_1
                engine.SendMessage(config.message_path + config.MA_1)
                print "Waiting for speed"
                if (config.UseATO == 1):
                    print "Wait two cycles before sending brake release"
        else:
            time.sleep(5)
            if (config.atp_restarted == 0):
                print "Sending " + config.MA_FWD_SC
                engine.SendMessage(config.message_path + config.MA_FWD_SC)
                print "Waiting for speed"
                if (config.UseATO == 1):
                    print "Wait two cycles before sending brake release"
                        

        # Check for message ACK
        ###########################################
        maAckStatus(engine)
               
          
                    
        # Wait for 2 cycles
        ###########################################
        if (config.UseATO == 1 and firstMALoop == 1):
            cnt = 2
            while (not config.atp_restarted):
                m = engine.GetMessage()
                if (m == None):
                    atp_restarted = 1
                    break
                cnt = cnt - 1
                if (cnt <= 0):
                    print "Sending Dispatcher_ReleaseSB"
                    engine.SendMessage(config.message_path + "Dispatcher_ReleaseSB.xml")
                    print "Waiting for speed"
                    break;

        # Wait for some distance to be be covered 
        ###########################################
        print "Waiting for some distance to be covered before sending MA Extension"
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                speed = engine.GetFields(m.Data, "V_SPEED")
                pos = engine.GetFields(m.Data, "D_POSITION")
                print "covering some distance : %d" % int(pos[0])
                if (int(pos[0]) > 10000):
                    break
        
        # Send MA_Forw with Extension
        ###########################################
        if (config.atp_restarted == 0):
            print "Sending " + config.MA_Ext
            engine.SendMessage(config.message_path + config.MA_Ext)
            if (config.UseATO == 1):
                print "Wait two cycles before sending brake release"

        maAckStatus(engine)        
        # Wait for speed
        ###########################################
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                speed = engine.GetFields(m.Data, "V_SPEED")
                if (int(speed[0]) > 0):
                    print "Waiting for idle"
                    break
                    
        # Wait for ATP to become "Idling"
        ###########################################
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                b_train_status = engine.GetFields(m.Data, "B_TRAIN_CORE_STATUS")
                if (b_train_status != None):
                    if (((int)(b_train_status[0]) & 0x20) != 0):
                        print "Train is now Idle"
                        break

        # Send MA_1Rev
        ###########################################
        if (config.atp_restarted == 0):
            print "Sending " + config.MA_2
            engine.SendMessage(config.message_path + config.MA_2)
            print "Waiting for speed"

        maAckStatus(engine)
        # Wait for some distance to be be covered
        ###########################################
        print "Waiting for some distance to be covered before sending MA Extension"
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                speed = engine.GetFields(m.Data, "V_SPEED")
                pos = engine.GetFields(m.Data, "D_POSITION")
                if (int(pos[0]) < 20000):
                    break
                
        # Send MA_REV with Extension
        ##########################################
        if (config.atp_restarted == 0):
             print "Sending " + config.MA_REV_EXT
             engine.SendMessage(config.message_path + config.MA_REV_EXT)
             if (config.UseATO == 1):
                 print "Wait two cycles before sending brake release"

        maAckStatus(engine)
        # Wait for speed
        ###########################################
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                speed = engine.GetFields(m.Data, "V_SPEED")
                if (int(speed[0]) > 0):
                    print "Waiting for idle"
                    break
        
        # Wait for ATP to become "Idling"
        ###########################################
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                b_train_status = engine.GetFields(m.Data, "B_TRAIN_CORE_STATUS")
                if (b_train_status != None):
                    if (((int)(b_train_status[0]) & 0x20) != 0):
                        print "Train is now Idle"
                        break

        # MA Gradient and Speed Profile-Fwd
        ###########################################
        if (config.atp_restarted == 0):
            print "Sending " + config.Grad_Speed_Fwd
            engine.SendMessage(config.message_path + config.Grad_Speed_Fwd)
            print "Waiting for speed"

        maAckStatus(engine)
        # Wait for speed
        ###########################################
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                speed = engine.GetFields(m.Data, "V_SPEED")
                if (int(speed[0]) > 0):
                    print "Waiting for idle"
                    break

        # Wait for ATP to become "Idling"
        ###########################################
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                b_train_status = engine.GetFields(m.Data, "B_TRAIN_CORE_STATUS")
                if (b_train_status != None):
                    if (((int)(b_train_status[0]) & 0x20) != 0):
                        print "Train is now Idle"
                        break
                    
        # Send MA_1Rev
        ###########################################
        if (config.atp_restarted == 0):
            print "Sending " + config.MA_SCRATCH
            engine.SendMessage(config.message_path + config.MA_SCRATCH)
            print "Waiting for speed"

        maAckStatus(engine)
        
        # Wait for speed
        ###########################################
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                speed = engine.GetFields(m.Data, "V_SPEED")
                if (int(speed[0]) > 0):
                    break    

       # Send EA
        ###########################################
        if (config.atp_restarted == 0):
            print "Sending " + config.Send_EA
            engine.SendMessage(config.message_path + config.Send_EA)


        time.sleep(2)

        # EA Status from ATP
        ###########################################
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                b_train_status = engine.GetFields(m.Data, "B_TRAIN_CORE_STATUS")
                if (b_train_status != None):
                    if (((int)(b_train_status[0]) & 0x8000) != 0):
                        print "Emergency Alert Activated"
                        break
        time.sleep(2)
    
        # Revoke EA
        ###########################################
        if (config.atp_restarted == 0):
            print "Revoking now EA.."
            print "Sending " + config.Revoke_EA
            engine.SendMessage(config.message_path + config.Revoke_EA)

        time.sleep(2)
        # EA Status from ATP
        ###########################################
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                b_train_status = engine.GetFields(m.Data, "B_TRAIN_CORE_STATUS")
                if (b_train_status != None):
                    if (((int)(b_train_status[0]) & 0x8000) == 0):
                        print "Emergency Alert Revoked"
                        break


        # Send MA_1Rev
        ###########################################
        if (config.atp_restarted == 0):
            print "Sending " + config.MA_SCRATCH
            engine.SendMessage(config.message_path + config.MA_SCRATCH)
            print "Waiting for speed" 

        #MA ACK
        maAckStatus(engine)
        
	# Wait for speed
        ###########################################
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                speed = engine.GetFields(m.Data, "V_SPEED")
                if (int(speed[0]) > 0):
                    print "Waiting for idle"
                    break
        
        # Wait for ATP to become "Idling"
        ###########################################
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                b_train_status = engine.GetFields(m.Data, "B_TRAIN_CORE_STATUS")
                if (b_train_status != None):
                    if (((int)(b_train_status[0]) & 0x20) != 0):
                        print "Train is now Idle"
                        break
        firstMALoop = 0
        
    # (End of count-while)
    ###########################################
            


    

