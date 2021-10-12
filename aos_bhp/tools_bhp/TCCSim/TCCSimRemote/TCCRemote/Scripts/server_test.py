import TCCSim.Script as Script
print("running python")
engine.Connect(1, "127.0.0.1:45000", 1, 5)
while(True):
    m = engine.GetMessage()
    if( m == None):
        break
    print(engine.GetMsgName(m.Data))

#engine.Disconnect()
