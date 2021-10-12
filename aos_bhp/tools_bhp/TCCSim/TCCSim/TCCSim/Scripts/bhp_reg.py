import TCCSim.Script as Script
from Scripts import common
from Scripts import config

'''@author Sanjeev Pandita '''

while(1):
    config.atp_restarted = 0
    count = config.Repetitions

    print "Starting: Script for Registration.."
    print "------------------------------------"

     # Set the Field for registration
    ###########################################
    msg = config.message_path + config.Position_Req_Report
    fieldName = "Q_INITIATE"
    engine.SetQInitiate(0)
    print "Sending PRR With Registration Field"
    count = config.Repetitions

    # Wait for connection
    ###########################################
    m = engine.GetMessage()
    while (m == None):
        m = engine.GetMessage()

    # Send ConfigutaionMessage.xml
    ###########################################
    print "Sending " + config.Config_Msg
    engine.SendMessage(config.message_path + config.Config_Msg)

    # Login status
    ###########################################
    common.login_seq(engine)

    if (1 == common.isStartUpMessageRec(engine)):
        print "Sending " + config.ValidTSetupMsg
        engine.SendMessage(config.message_path + config.ValidTSetupMsg)

    # Wait for first balise
    ###########################################
    if (common.goto_Send_MA_1 == 0):
        while (not config.atp_restarted):
            m = engine.GetMessage()
            if (m == None):
                config.atp_restarted = 1
                break
            if (m.Name == "TrainRegistrationInformation"):
                nid_bg = engine.GetFields(m.Data, "NID_BG")
                if (int(nid_bg[0]) == config.FirstBalise):

                    print "Sending MA_Reg"
                    engine.SendMessage(config.message_path + config.RegMA)
                    break
                else:
                    print "First balise ID not correct, please restart ATP"
    common.maAckStatus(engine)
    #call comman scenario's
    common.normal_sequences(engine)

    # Wait for power down to run once again
    ###########################################
    if (m != None):
        print "Waiting for ATP power down to repeat registration"
    while (m != None):
        m = engine.GetMessage()

    print "Restarting script"



