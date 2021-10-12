import TCCSim.Script as Script

message_path = ".\\Messages\\"

print "Starting: ContStartup_BHP_TIC_ForwRev"


ValidTSetupMsg  = "TSetup_10Car.xml"
FirstBalise     = 4513
RegMA           = "MA_Reg.xml"
MA_1            = "MA_Forw.xml"
MA_Ext          = "MA_Forw_Extn.xml"
MA_2            = "MA_Rev.xml"
UseATO          = 0
Repetitions     = 10

if (Repetitions > 1):
    print "Repetitions: " + str(Repetitions)

while (1):
    
    print "---------------------------------"
    atp_restarted = 0
    count = Repetitions
    
    # Wait for connection
    ###########################################
    m = engine.GetMessage()
    while (m == None):
        m = engine.GetMessage()


    # LogOn from ATP
    ###########################################
    goto_Send_MA_1 = 0
    while (not atp_restarted):
        m = engine.GetMessage()
        if (m == None):
            atp_restarted = 1
            break
        if (m.Name == "DriverInformation"):
            print "Driver logged in"
            break

        if (m.Name == "PositionReport"):
            b_train_status = engine.GetFields(m.Data, "B_TRAIN_CORE_STATUS")
            if (b_train_status != None):
                if ((((int)(b_train_status[0]) & 0x40) != 0) and (((int)(b_train_status[0]) & 0x200) == 0)):
                    print "Already registered, try to sent MA_1"
                    goto_Send_MA_1 = 1
                    break
    
    # Send TSetup for 10 Cars to ATP
    ###########################################
    if (goto_Send_MA_1  == 0):
        while (not atp_restarted ):
            m = engine.GetMessage()
            if (m == None):
                atp_restarted = 1
                print "ATP Restarting"
                break
            if (m.Name == "StartUpMessage"):
                print "StartUpMessage Received"
                print "Sending " + ValidTSetupMsg
                engine.SendMessage(message_path + ValidTSetupMsg)
                #print "Waiting registration balise (0x11a1)"
                break
    
    # Wait for first balise
    ###########################################
    if (goto_Send_MA_1  == 0):
        while (not atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                atp_restarted = 1
                break
            if (m.Name == "TrainRegistrationInformation"):
                nid_bg = engine.GetFields(m.Data, "NID_BG")
                if (int(nid_bg[0]) == FirstBalise):
                    print "Sending MA_Reg"
                    engine.SendMessage(message_path + RegMA)
                    break
                print "First balise ID not correct, please restart ATP"
    
    # Wait for ATP to become "Idling"
    ###########################################
    if (goto_Send_MA_1  == 0):
        while (not atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                q_atp_mode = engine.GetFields(m.Data, "Q_ATP_MODE")
                if (q_atp_mode != None):
                    if ((int)(q_atp_mode[0]) == 5):
                        print "FullATP"
                        if (UseATO == 1):
                            print "Wait for ATO to be in Automatic"
                        break

    # Wait for ATO to become "ATOAutomatic"
    ###########################################
    if (UseATO == 1):
        while (not atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                atp_restarted = 1
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
    while (count > 0 and not atp_restarted):
        count = count - 1
        if (Repetitions > 1):
            print "Run " + str(Repetitions - count) + " of " + str(Repetitions)

        # Send MA_1
        ###########################################
        if (atp_restarted == 0):
            print "Sending " + MA_1
            engine.SendMessage(message_path + MA_1)
            print "Waiting for speed"
            if (UseATO == 1):
                print "Wait two cycles before sending brake release"
            
            

        
        # Wait for 2 cycles
        ###########################################
        if (UseATO == 1 and firstMALoop == 1):
            cnt = 2
            while (not atp_restarted):
                m = engine.GetMessage()
                if (m == None):
                    atp_restarted = 1
                    break
                cnt = cnt - 1
                if (cnt <= 0):
                    print "Sending Dispatcher_ReleaseSB"
                    engine.SendMessage(message_path + "Dispatcher_ReleaseSB.xml")
                    print "Waiting for speed"
                    break;

        # Wait for some distance to be be covered 
        ###########################################
        print "Waiting for some distance to be covered before sending MA Extension"
        while (not atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                speed = engine.GetFields(m.Data, "V_SPEED")
                pos = engine.GetFields(m.Data, "D_POSITION")
                if (int(pos[0]) > 30500):
                    break
        
        # Send MA_Forw with Extension
        ###########################################
        if (atp_restarted == 0):
            print "Sending " + MA_Ext
            engine.SendMessage(message_path + MA_Ext)
            print "Waiting for speed"
            if (UseATO == 1):
                print "Wait two cycles before sending brake release"
                
        # Wait for speed
        ###########################################
        while (not atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                speed = engine.GetFields(m.Data, "V_SPEED")
                if (int(speed[0]) > 0):
                    print "Waiting for idle"
                    break
                    
        # Wait for ATP to become "Idling"
        ###########################################
        while (not atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                b_train_status = engine.GetFields(m.Data, "B_TRAIN_CORE_STATUS")
                if (b_train_status != None):
                    if (((int)(b_train_status[0]) & 0x20) != 0):
                        print "Train is now Idle"
                        break
                    
        # Send MA_1Rev
        ###########################################
        if (atp_restarted == 0):
            print "Sending " + MA_2
            engine.SendMessage(message_path + MA_2)
            print "Waiting for speed"

        # Wait for speed
        ###########################################
        while (not atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                atp_restarted = 1
                break
            if (m.Name == "PositionReport"):
                speed = engine.GetFields(m.Data, "V_SPEED")
                if (int(speed[0]) > 0):
                    print "Waiting for idle"
                    break
        
        # Wait for ATP to become "Idling"
        ###########################################
        while (not atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                atp_restarted = 1
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
            
        
    # Wait for power down to run once again
    ###########################################
    if (m != None):
        print "Waiting for ATP power down to repeat registration"
    while (m != None):
        m = engine.GetMessage()
        
    print "Restarting script"
    

