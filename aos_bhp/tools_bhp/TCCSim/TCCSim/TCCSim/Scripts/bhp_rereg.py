import TCCSim.Script as Script
from Scripts import common
from Scripts import config
import time

'''@author Sanjeev Pandita updated code with re- registration'''

# Wait for connection
###########################################
while(1):
    print "Starting: Script for Re-Registration.."
    print "--------------------------------------"
    config.atp_restarted = 0
    
    # Set the Field for re-registration
    ###########################################
    msg = config.message_path + config.Position_Req_Report
    fieldName = "Q_INITIATE"
    engine.SetQInitiate(1)
    print "Sending PRR With Re-Registration Field"
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

    # LogOn from ATP
    ###########################################
    common.login_seq(engine)
    time.sleep(3)
    #Re-reg tset up
    if(not config.atp_restarted):
        print "Sending " + config.Tsetup_REREG
        engine.SendMessage(config.message_path + config.Tsetup_REREG)

    # Retry sending MA_RE_REG max 3 times since the acknowledgment is 
    # received only after "Departure test" popup is confirmed on the DMI
    retries = 3
    if(1 == common.isStartUpMessageRec(engine)):
        while(retries > 0):
            retries = retries-1
            time.sleep(2)
            #Start Up message recieved.
            print "Sending " + config.MA_RE_REG
            engine.SendMessage(config.message_path + config.MA_RE_REG)
            if (common.maAckStatus(engine)):
                break
            else:
                print "Trying to send MA_RE_REG again..."


    #call comman scenario's
    common.normal_sequences(engine)

    # Wait for power down to run once again
    ###########################################
    if (m != None):
        print "Waiting for ATP power down to repeat re-registration"
    while (m != None):
        m = engine.GetMessage()

    print "Restarting script"
