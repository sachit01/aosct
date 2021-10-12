using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using TCCCom;
using TCCRemote;

namespace TCCSim
{
    class Controller
    {
        private Server MV;
        private Model Mdl;
        private MsgDef MessageXMLHandler;

        public Controller()
        {   
            Mdl = new Model();
            List<MessageFile> FileList = Mdl.InitWatchDir();
            MV = new Server(Mdl);
            Mdl.Worker = MV.Worker;
            Mdl.ScriptWorker = MV.ScriptWorker;
            MV.MVE += new Server.MVEvent(ServerHandler);

            MV.Run();
        }

        public void ServerHandler(EventArgs e)
        {
            if (e is ConnectEventArgs)
            {
                ConnectEventArgs ev = e as ConnectEventArgs;
                if (ev.Connecting)
                    Mdl.Connect(ev.ID, ev.Connect, ev.Timer, ev.Timeout);
                else
                    Mdl.Disconnect();
            }
            else if (e is GetConnectDataEventArgs)
            {
                Mdl.GetConfig();
            }
            else if (e is PauseEventArgs)
            {
                Mdl.Pause(((PauseEventArgs)e).Pause);
            }
            else if (e is SaveTransferEventArgs)
            {
                Mdl.SaveTransfer(((SaveTransferEventArgs)e).File);
            }
            else if (e is LoadTransferEventArgs)
            {
                Mdl.LoadTransfer(((LoadTransferEventArgs)e).File);
            }
            else if (e is ClearDeliverListEventArgs)
            {
                Mdl.ClearDeliverList();
            }
            else if (e is FaultInjectionEventArgs)
            {
                Mdl.SafetyInjection((FaultInjectionEventArgs)e);
            }
            else if (e is SendMessageEventArgs)
            {
                SendMessageEventArgs ev = e as SendMessageEventArgs;
                Mdl.SendFromFile(ev.File);
            }
            else if (e is RunScriptEventArgs)
            {
                RunScriptEventArgs ev = e as RunScriptEventArgs;
                if (ev.Cancel)
                {
                    Mdl.CancelScript();
                }
                else
                    Mdl.RunScript(((RunScriptEventArgs)e).File);
            }
        }
    }
}
