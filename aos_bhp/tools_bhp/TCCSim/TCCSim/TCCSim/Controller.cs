using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Xml.Linq;
using TCCtcp;


namespace TCCSim
{
    class Controller
    {
        private MainView MV;
        private Model Mdl;
        private MsgView MsgV;

        /// <summary>
        /// Inits most of the program
        /// </summary>
        public Controller()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            
            Mdl = new Model();
            //Get list of message files in Messages folder
            List<MessageFile> FileList = Mdl.InitWatchDir();
            MV = new MainView(Mdl, FileList, Mdl.MessageDir, Mdl.GetMessageTypes());

            // Distribute BackgroundWorkers
            Mdl.Worker = MV.Worker;
            Mdl.ScriptWorker = MV.ScriptWorker;

            
            MsgV = new MsgView(Mdl, Mdl.GetStationaryTypes());

            // Register event handler
            MsgV.MsgViewE += new MsgView.MsgViewEvent(MsgViewHandler);
            MV.MVE += new MainView.MVEvent(MainViewHandler);

            Mdl.AutoConnect();
            //GO
            Application.Run(MV);
        }

        /// <summary>
        /// Handles event from MainView
        /// </summary>
        /// <param name="e"></param>
        public void MainViewHandler(EventArgs e)
        {
            if (e is ConnectEventArgs)
            {
                ConnectEventArgs ev = e as ConnectEventArgs;
                if (ev.Connecting)
                    Mdl.Connect(ev.ID, ev.siteID, ev.regionID, ev.Connect, ev.Timer, ev.Timeout, ev.AutoConnect, ev.Crc);
                else
                    Mdl.Disconnect();
            }
            //Gets connections data from config
            else if (e is GetConnectDataEventArgs)
            {
                Mdl.GetConfig();
            }
            else if (e is PauseEventArgs)
            {
                Mdl.Pause(((PauseEventArgs)e).Pause);
            }
            //Save current transfers to file
            else if (e is SaveTransferEventArgs)
            {
                Mdl.SaveTransfer(((SaveTransferEventArgs)e).File);
            }
            // Load transfers from file
            else if (e is LoadTransferEventArgs)
            {
                Mdl.LoadTransfer(((LoadTransferEventArgs)e).File);
            }
            // Clear deliver list
            else if (e is ClearDeliverListEventArgs)
            {
                Mdl.ClearDeliverList();
            }
            // Enable a safety header fault injection
            else if (e is FaultInjectionEventArgs)
            {
                Mdl.SafetyInjection((FaultInjectionEventArgs)e);
            }
            // Send message from file
            else if (e is SendMessageEventArgs)
            {
                SendMessageEventArgs ev = e as SendMessageEventArgs;
                Mdl.SendFromFile(ev.File);
            }
            else if (e is SendBytesEventArgs)
            {

            }
            else if (e is OpenMsgView)
            {
                MsgV.Show();
            }
            // Open message file in MsgView
            else if (e is LoadXMLEArgs)
            {
                // Show after Load causes less flicker on screen / Bo H, 2014-04-15    
                //                MsgV.Show();
                Mdl.MsgVLoadXML((LoadXMLEArgs)e);
                MsgV.Show();
            }
            else if (e is LoadDataEArgs)
            {
                MsgV.Show();
                Mdl.MsgVLoadData((LoadDataEArgs)e);
            }
            else if (e is RunScriptEventArgs)
            {
                RunScriptEventArgs ev = e as RunScriptEventArgs;
                // Cancel script
                if (ev.Cancel)
                {
                    Mdl.CancelScript();
                }
                // Run script
                else
                    Mdl.RunScript(((RunScriptEventArgs)e).File);
            }
            else if (e is SaveFilterEventArgs)
            {
                Mdl.SaveFilterSettings(((SaveFilterEventArgs)e).Filter);
            }
        }

        /// <summary>
        /// Event handler for MsgView
        /// </summary>
        /// <param name="e"></param>
        public void MsgViewHandler(EventArgs e)
        {
            // Get message definition
            if (e is GetMessageEventArgs)
            {
                string MType = ((GetMessageEventArgs)e).MessageType;
                Mdl.GetMessageType(MType);
            }
            // Get block definition
            if (e is GetBlockEventArgs)
            {
                Mdl.GetBlock(((GetBlockEventArgs)e).BlockType);
            }
            if (e is SendXMLEventArgs)
            {
                XElement x = (XElement)((SendXMLEventArgs)e).Message;
                byte[] b = Message.XMLtoByte(x);
                Mdl.SendBytes(b, x.Name.ToString());
            }
        }
    }
}
