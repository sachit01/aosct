import TCCSim.Script as Script

message_path = "C:\Documents and Settings\\rigustaf\Desktop\TCCSimRepo\TCCSim\Messages\\"

found_DLS = 0
while (not found_DLS):
	m = engine.GetMessage()
	found_DLS = m.Name == "DriverInformation"
	print "loop\n"
	print m.Name

for i in range(1,6):
	engine.GetMessage()

engine.SendMessage(message_path + "TSetup_Empty.xml")

# Check Validar
found_startupmessage = 0
while (not found_startupmessage):
	m = engine.GetMessage()
	found_startupmessage = m.Name == "StartUpMessage"

print "Got StartUpMessage"
engine.SendMessage(message_path + "TSetup_Lok.xml")

# Wait for TrainRegistrationInformation.
train_reg = 0
while (not train_reg):
	m = engine.GetMessage()
	if (m.Name == "TrainRegistrationInformation"):
		nid_bg = engine.GetFields(m.Data, "NID_BG")
		train_reg = int(nid_bg[0]) == 0x11a1
		print "TRI\n"
print "Got TRI, sending MA_Reg!\n"

engine.SendMessage(message_path + "MA_Reg.xml")

print "Waiting for train\n"

wait_pr = 0
while (not wait_pr):
	m = engine.GetMessage()
	if (m.Name == "PositionReport"):
		d_position = engine.GetFields(m.Data, "D_POSITION")
		speed = engine.GetFields(m.Data, "V_TRAIN_SPEED")
		if (int(d_position[0]) > 5300 and int(speed[0]) == 0):
			wait_pr = 1
			print "Sending MA1_Forw"
engine.SendMessage(message_path + "MA_1Forw.xml")
