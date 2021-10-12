import TCCSim.Script as Script
print("running python")
#engine.Connect(1, "127.0.0.1:45000", 1, 5)
engine.SendMessage("d:\my documents\Visual Studio 2008\Projects\TCCSim\TCCSim\Messages\MA_Reg.xml")
while(True):
    m = engine.GetMessage()
    if( m == None):
        break
    print(engine.GetMsgName(m.Data))
    print(engine.GetFields(m.Data, "Q_ERROR_LEVEL")[0])

#engine.Disconnect()
