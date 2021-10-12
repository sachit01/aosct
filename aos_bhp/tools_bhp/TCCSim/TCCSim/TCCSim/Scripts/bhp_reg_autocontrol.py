import TCCSim.Script as Script
from Scripts import common_autocontrol
from Scripts import config

'''@author Sanjeev Pandita '''

while(1):
    config.atp_restarted = 0
    count = config.Repetitions
    print "Starting: Script for Registration.."
    print "------------------------------------"
    print "Starting: the auto control.."
    print "-------------------------------"
    
    if(config.atp_restarted):
         engine.stopAutoControl()
         break

    count = config.Repetitions
    while (count > 0 and(not engine.startAutoControl())):
         count = count - 1
         print("Trying to connect autocontrol")

 
    engine.sendCommandToAutoControl("start")

	# Set the Field for registration
    ###########################################
    msg = config.message_path + config.Position_Req_Report
    fieldName = "Q_INITIATE"
    engine.SetQInitiate(0)
    print "Sending PRR With Registration Field"


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
    common_autocontrol.login_seq(engine)
    
    if (1 == common_autocontrol.isStartUpMessageRec(engine)):
        print "Sending " + config.ValidTSetupMsg
        engine.SendMessage(config.message_path + config.ValidTSetupMsg)
	
	##Auto Control
	#########################################
    if (common_autocontrol.goto_Send_MA_1 == 0):
       while (1):
            m = engine.GetMessage()
            if (m == None):
                print "ATP Restarting"
                break
                
            if (m.Name == "PositionReport"):
                curr_mode = engine.GetCurrentMode(m.Data,"Q_ATP_MODE")
                if (curr_mode == 2):
                    print "sending Confirm Departure Test"
                    engine.sendCommandToAutoControl("DMIselect confirmdeparture")
					
					#waiting for the response from auto control
                    while(not(engine.getAutoControlResponse())):  
                        print "Waiting for the response"
						
                elif(curr_mode == 3):
                    print "sending Select Orientation"
                    engine.sendCommandToAutoControl("DMIselect ORIENTATION1FORW")
					
					#waiting for the response from auto control
                    while(not(engine.getAutoControlResponse())):  
                        print "Waiting for the response"
						
                elif(curr_mode == 4):
                    print "sending AutoControl on"				    
                    engine.sendCommandToAutoControl("autocontrol on")
                    break
					#waiting for the response from auto control
                    #while(not(engine.getAutoControlResponse())):  
                     #   print "Waiting for the response"
                       # break					
					
    # Wait for first balise
    ###########################################
    if (common_autocontrol.goto_Send_MA_1 == 0):
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
    common_autocontrol.maAckStatus(engine)
    #call comman scenario's
    common_autocontrol.normal_sequences(engine)

    # Wait for power down to run once again
    ###########################################
    if (m != None):
        print "Waiting for ATP power down to repeat registration"
    while (m != None):
        m = engine.GetMessage()

    print "Restarting script"



