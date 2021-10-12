import TCCSim.Script as Script

has_sent_forw = True
has_sent_backw = False
print "----------- Forever -------------"
while (True):
	m = engine.GetMessage()
	if (m == 0):
		continue
	d_position = engine.GetFields(m.Data, "D_POSITION")
	v_train_speed = engine.GetFields(m.Data, "V_TRAIN_SPEED")

	print "d_position: " + d_position[0] + " v_train_speed: " + v_train_speed[0]

	if (int(d_position[0]) < 3800 and int(v_train_speed[0]) == 0 and not has_sent_forw):
		engine.SendMessage("C:\Documents and Settings\\rigustaf\Desktop\TCCSimRepo\TCCSim\Messages\MA_1Forw.xml")
		has_sent_forw = not has_sent_forw
		has_sent_backw = not has_sent_backw
		print "MA_1Forw"
	if (int(d_position[0]) > 15000 and int(v_train_speed[0]) == 0 and not has_sent_backw):
		engine.SendMessage("C:\Documents and Settings\\rigustaf\Desktop\TCCSimRepo\TCCSim\Messages\MA_1Rev.xml")
		has_sent_forw = not has_sent_forw
		has_sent_backw = not has_sent_backw
		print "MA_1Rev"
