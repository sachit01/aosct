import TCCSim.Script as Script

#message_path = "C:\\LabSW\\TCCSim\\Messages\\"
message_path = "C:\\TCCSim\\Distribution\\Software\\Messages\\"

print "Starting: SingleStartup_BHP_TIC"

FirstTSetupMsg  = "TSetup_Empty.xml"
ValidTSetupMsg  = "TSetup_BHP_TIC.xml"
FirstBalise     = 4513
RegMA           = "MA_Reg.xml"

if (1):
    print "---------------------------------"
    atp_restarted = 0

    # Wait for connection
    ###########################################
    m = engine.GetMessage()
    while (m == None):
        m = engine.GetMessage()

    # LogOn from ATP
    ###########################################
    while (not atp_restarted):
        m = engine.GetMessage()
        if (m == None):
            atp_restarted = 1
            break
        if (m.Name == "DriverInformation"):
            print "Driver logged in"
            break
    
    #  Sending initial TSetup to ATP until StartUpMessage received
    ###########################################
    print "Sending " + FirstTSetupMsg
    while (not atp_restarted ):
        engine.SendMessage(message_path + FirstTSetupMsg)
        m = engine.GetMessage()
        if (m == None):
            atp_restarted = 1
            break
        if (m.Name == "StartUpMessage"):
            #print "Got StartUpMessage"
            print "Sending " + ValidTSetupMsg
            engine.SendMessage(message_path + ValidTSetupMsg)
            #print "Waiting registration balise (0x11a1)"
            break
    
    # Wait for first balise
    ###########################################
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
    while (not atp_restarted):
        m = engine.GetMessage()
        if (m == None):
            atp_restarted = 1
            break
        if (m.Name == "PositionReport"):
            b_train_status = engine.GetFields(m.Data, "B_TRAIN_STATUS")
            if (b_train_status != None):
                if (((int)(b_train_status[0]) & 0x40) != 0):
                    print "FullATP"
                    break
                        
