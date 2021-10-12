using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Xml;
using System.Xml.Linq;
using TCCCom;

namespace TCCSim
{
    public class Model
    {
        public event MEvent ME;
        public delegate void MEvent(EventArgs e);
        
        private MsgDef MessageXMLHandler;
        private Communication Com;
        private Script ScriptRunning;
        private String DefaultPRR;
        private String DefaultDriverLogonStatus;
        private XElement XMLDriverLogonStatus;
        private String NameDriverLogonStatus;
        public String MessageDir {get; private set;}
        public BackgroundWorker Worker { set; get; }
        public BackgroundWorker ScriptWorker { set; get; }
        public List<DeliverEventArgs> DeliverList { private set; get; }
        public List<EventArgs> DeliverScriptList { private set; get; }
        private short TriggerValue = 128;


        /// Status test

        public bool PauseEnable { private set; get; }
        public bool PauseTag { private set; get; }
        public String PauseText { private set; get; }
        public String PauseLabel { private set; get; }

        public bool ConnectEnable { private set; get; }
        public bool ConnectTag { private set; get; }
        public String ConnectText { private set; get; }
        
        /// Status test end


        //private Dictionary<Byte[], Message> t = new Dictionary<byte[], Message>();

        public Model()
        {
            this.PauseEnable = false;
            this.PauseTag = false;
            this.PauseText = "Pause";
            this.PauseLabel = "";

            this.ConnectEnable = true;
            this.ConnectTag = false;
            this.ConnectText = "Connect";

            this.MessageXMLHandler = new MsgDef();
            this.MessageDir = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                                    @"..\..\Messages\");
            this.DeliverList = new List<DeliverEventArgs>();
        }

        public void Connect(String ID, String Connect, String Timer, String Timeout)
        {
            if (this.Com != null)
                return;
            this.ConnectText = "Disconnect";
            this.ConnectTag = true;
            this.PauseEnable = true;

            // Get XML files for default PRR and DLS
            String Config = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                @"..\..\Config.xml");
            try
            {
                XDocument Conf = XDocument.Load(Config);
                XElement Elem = (from C in Conf.Descendants("Config")
                                 select C).First();
                this.DefaultPRR = this.MessageDir + Elem.Element("PRRMessage").Value;
                this.DefaultDriverLogonStatus = this.MessageDir + Elem.Element("DLSMessage").Value;
            }
            catch (Exception)
            {
                ME(new XMLFileErrorEventArgs("Could not find PRRMessage/DLSMessage in Config.xml \nUsing DefaultPRR.xml and DefaultDriverLogonStatus.xml", false));
                this.DefaultPRR = this.MessageDir + "DefaultPRR.xml";
                this.DefaultDriverLogonStatus = this.MessageDir + "DefaultDriverLogonStatus.xml";
            }

            // And load the default messages
            String LoopName;
            Byte[] LoopMessage;
            try
            {
                XElement XPRR = XElement.Load(DefaultPRR);
                LoopName = XPRR.Name.ToString();
                LoopMessage = Message.XMLtoByte(XPRR);
                this.XMLDriverLogonStatus = XElement.Load(DefaultDriverLogonStatus);
                this.NameDriverLogonStatus = this.XMLDriverLogonStatus.Name.ToString();
            }
            catch(Exception)
            {
                this.ConnectText = "Connect";
                this.ConnectTag = false;
                this.ConnectEnable = true;

                this.PauseEnable = false;
                this.PauseText = "Pause";
                this.PauseTag = false;
                this.PauseLabel = "";
                ME(new XMLFileErrorEventArgs("Could not load default PRR/DriverLogonStatus XML file(s).", true));
                AddToDeliverScript(new SocketParaErrorEventArgs("Could not load default PRR/DriverLogonStatus XML file(s)."));
                return;
            }

            try
            {
                XDocument Conf = XDocument.Load(Config);
                XElement Elem = (from C in Conf.Descendants("Config")
                                 select C).First();
                this.TriggerValue = Int16.Parse(Elem.Element("DIValue").Value);
            }
            catch (Exception)
            {
                ME(new XMLFileErrorEventArgs("Could not find DIValue in Config.xml \nUsing 128", false));
            }

            int BufferLength;
            try
            {
                XDocument Conf = XDocument.Load(Config);
                XElement Elem = (from C in Conf.Descendants("Config")
                                 select C).First();
                BufferLength = Int16.Parse(Elem.Element("BufferLength").Value);
            }
            catch (Exception)
            {
                ME(new XMLFileErrorEventArgs("Could not find BufferLength in Config.xml \nUsing 1000", false));
                BufferLength = -1;
            }

            Worker.ProgressChanged += new ProgressChangedEventHandler(ComHandler);
            Worker.RunWorkerCompleted += new RunWorkerCompletedEventHandler(ComHandler);
            this.Com = new Communication(Timer, Connect, ID, Timeout, LoopMessage, LoopName, Worker, BufferLength);
            Worker.RunWorkerAsync();

            this.PauseEnable = true;
        }

        public void Disconnect()
        {
            this.Com.Pause.Set();
            Worker.CancelAsync();

            ConnectEnable = false;
            PauseEnable = false;
            PauseText = "Pause";
            PauseTag = false;
            PauseLabel = "";
        }

        public void Pause(bool Pause)
        {
            if (this.PauseTag)//Pause)
            {
                this.Com.Pause.Set();
                this.PauseText = "Pause";
                this.PauseLabel = "";
            }
            else
            {
                this.Com.Pause.Reset();
                this.PauseText = "Unpause";
                this.PauseLabel = "(Paused)";
            }
            this.PauseTag = !this.PauseTag;
            ME(new PauseEventArgs(false));
        }

        public void SaveTransfer(String File)
        {
            TextWriter tw = new StreamWriter(File);
            foreach (DeliverEventArgs e in DeliverList)
            {
                String output = e.Time + " ";
                output += e.Name + " ";
                foreach (Byte b in e.Data)
                {
                    output += b.ToString() + ".";
                }
                output = output.TrimEnd('.');
                output += " " + e.Sent.ToString() + " " + e.TSSenderError.ToString() + " " + e.TSRefError.ToString();
                tw.WriteLine(output);
            }
            tw.Close();
        }

        public void LoadTransfer(String File)
        {
            TextReader tr = new StreamReader(File);
            String input;
            DeliverList.Clear();
            int j = 1;
            while ((input = tr.ReadLine()) != null)
            {
                String[] row = input.Split(' ');
                try
                {
                    String[] data = row[2].Split('.');
                    Byte[] bytes = new Byte[data.Length];
                    for (int i = 0; i < data.Length; i++)
                    {
                        bytes[i] = Convert.ToByte(data[i]);
                    }
                    DeliverEventArgs e = new DeliverEventArgs(bytes, Convert.ToBoolean(row[3]), row[0],
                        Convert.ToBoolean(row[4]), Convert.ToBoolean(row[5]));
                    e.Name = row[1];
                    e.ID = DeliverList.Count;
                    DeliverList.Add(e);
                }
                catch (Exception)
                {
                    tr.Close();
                    DeliverList.Clear();
                    ME(new LoadTransferErrorEventArgs("Could not parse row: " + j));
                    return;
                }
                j++;
            }
            tr.Close();
            ME(new UpdateTransfersList());
        }

        private void ComHandler(object sender, EventArgs e)
        {
            if (e is ProgressChangedEventArgs)
            {
                ProgressChangedEventArgs ev = e as ProgressChangedEventArgs;
                if (ev.UserState is DeliverEventArgs)
                {
                    DeliverEventArgs eve = ev.UserState as DeliverEventArgs;
                    TriggerCheck(eve.Data);
                    eve.ID = DeliverList.Count;
                    if (!eve.Sent)
                        eve.Name = Message.string_MTypeFromBytes(eve.Data);
                    //Byte[] CRCData = eve.Data.Skip<Byte>(1).Take<Byte>(eve.Data.Length - 2).ToArray<Byte>();
                    Byte[] CRCData = new Byte[eve.Data.Length - 2];
                    Array.Copy(eve.Data, 1, CRCData, 0, CRCData.Length);
                    eve.CRCError = (Safety.CRC_Calc(CRCData, (uint)0) != 0);
                    eve.BCCError = (Safety.BCC_Calc(eve.Data, (Byte)0) != 0);
                    DeliverList.Add(eve);
                    ME(eve);
                    
                    if (!eve.Sent)
                        AddToDeliverScript(eve);
                }
                else if (ev.UserState is ConnectedEventArgs)
                {
                    ConnectedEventArgs eve = ev.UserState as ConnectedEventArgs;
                    ME(eve);
                    AddToDeliverScript(eve);
                }
                else if (ev.UserState is SocketParaErrorEventArgs)
                {
                    this.PauseEnable = false;
                    SocketParaErrorEventArgs eve = ev.UserState as SocketParaErrorEventArgs;
                    ME(eve);
                    AddToDeliverScript(eve);
                }
                else if (ev.UserState is SocketErrorEventArgs)
                {
                    SocketErrorEventArgs eve = ev.UserState as SocketErrorEventArgs;
                    ME(eve);
                    AddToDeliverScript(eve);
                }
                else if (ev.UserState is UnknownErrorEventArgs)
                {
                    Console.WriteLine("UNKNOWN ERROR: " + ((UnknownErrorEventArgs)ev.UserState).Text);
                    UnknownErrorEventArgs eve = ev.UserState as UnknownErrorEventArgs;
                    ME(eve);
                }
            }
            else if (e is RunWorkerCompletedEventArgs)
            {
                this.ConnectText = "Connect";
                this.ConnectTag = false;
                this.ConnectEnable = true;
                this.PauseEnable = false;
                this.PauseText = "Pause";

                ME(new ConnectedEventArgs(false));
                Worker.ProgressChanged -= new ProgressChangedEventHandler(ComHandler);
                Worker.RunWorkerCompleted -= new RunWorkerCompletedEventHandler(ComHandler);
                this.Com = null;
                if (this.DeliverScriptList != null)
                {
                    lock (this.DeliverScriptList)
                    {
                        this.DeliverScriptList.Clear();
                    }
                }
                
            }
        }

        private void AddToDeliverScript(EventArgs e)
        {
            if (this.DeliverScriptList != null)
            {
                lock (this.DeliverScriptList)
                {
                    this.DeliverScriptList.Add(e);
                }
                this.ScriptRunning.MsgReceived.Set();
            }
        }
        
        private void TriggerCheck(Byte[] Data)
        {
            if (Data[8] == this.TriggerValue)
            {
                lock (this.Com.SendL)
                {
                    this.Com.SendL.Insert(0, new DeliverEventArgs(Message.XMLtoByte(XMLDriverLogonStatus), this.NameDriverLogonStatus));
                }
            }
        }

        public List<MessageFile> InitWatchDir()
        {
            String[] Files = Directory.GetFiles(MessageDir);
            List<MessageFile> FileList = new List<MessageFile>();
            foreach (String f in Files)
            {
                if (Path.GetExtension(f) == ".xml")
                {
                    FileList.Add(new MessageFile(f));
                }
            }
            return FileList;
            
        }

        #region MsgDef-calls
        
        public void GetMessageType(string MType)
        {
            GetTypeEventArgs e = MessageXMLHandler.GetMessageType(MType);
            ME(e);
        }

        public Dictionary<string, FieldType> GetMessageTypes()
        {
            return MessageXMLHandler.GetMessageTypes();
        }

        public IEnumerable<StringInt> GetStationaryTypes()
        {
            return MessageXMLHandler.GetStationaryTypes();
        }

        // Send fields of block with NID_BLOCK_TYPE == BlockID to view.
        public void GetBlock(string BlockID)
        {
            BlockFieldsEventArgs e = MessageXMLHandler.BlockFields(BlockID);
            ME(e);
        }

        public String GetErrorDescription(int Code)
        {
            return MsgDef.GetErrorDescription(Code);
        }

        #endregion


        /// <summary>
        /// Returns Config connect data
        /// </summary>
        public void GetConfig()
        {
            String Config = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                @"..\..\Config.xml");
            try
            {
                XDocument Conf = XDocument.Load(Config);
                IEnumerable<XElement> Elems = from C in Conf.Descendants("Config")
                                              select C;
                XElement Elem = Elems.First<XElement>();
                ME(new ConnectEventArgs(Elem.Element("TrainID").Value, Elem.Element("Connect").Value, Elem.Element("Timer").Value, Elem.Element("Timeout").Value));
            }
            catch (Exception)
            {
                new ConnectEventArgs();
                ME(new ConnectEventArgs());
            }
        }

        public void ClearDeliverList()
        {
            this.DeliverList.Clear();
        }

        public void SafetyInjection(FaultInjectionEventArgs e)
        {
            if (this.Com != null)
            {
                if (e.IT == FaultInjectionEventArgs.InjectionType.Fragment)
                {
                    lock (this.Com.FragmentList)
                    {
                        this.Com.FragmentList.Add(e.FragmentDelay);
                    }
                }
                else
                {
                    this.Com.FaultInjection[(int)e.IT].Set();
                }
            }
        }

        public void SendFromFile(String File)
        {
            if (this.Com != null)
            {
                try
                {
                    XElement xelem = XElement.Load(File);
                    Byte[] Data = Message.XMLtoByte(xelem);
                    if (Data == null)
                        throw new Exception();
                    lock (this.Com.SendL)
                    {
                        this.Com.SendL.Add(new DeliverEventArgs(Data, xelem.Name.ToString()));
                    }
                }
                catch (Exception)
                {
                    XMLFileErrorEventArgs e = new XMLFileErrorEventArgs("Unable to read/parse XML file: " + File, false);
                    ME(e);
                }
            }
        }

        public void SendBytes(Byte[] Data, String Name)
        {
            if (Data == null)
            {
                this.ME(new XMLParseErrorEventArgs("Could not send message"));
            }
            else if (this.Com != null)
            {
                lock (this.Com.SendL)
                {
                    this.Com.SendL.Add(new DeliverEventArgs(Data, Name));
                }
            }
        }

        internal void MsgVLoadXML(LoadXMLEArgs e)
        {
            ME(e);
        }

        public String GetPRErrorBlock()
        {
            String Config = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                @"..\..\Config.xml");
            try
            {
                XDocument Conf = XDocument.Load(Config);
                XElement Elem = (from C in Conf.Descendants("Config")
                                  select C).First();
                return Elem.Element("PRErrorBlock").Value;
            }
            catch (Exception)
            {
                ME(new XMLFileErrorEventArgs("Could not find PRErrorBlock in Config.xml\nUsing ERROR_MESSAGE_DATA", false));
            }
            return null;
        }

        public string GetPRValue()
        {
            String Config = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                @"..\..\Config.xml");
            try
            {
                XElement Conf = XElement.Load(Config);
                return Conf.Element("PRValue").Value;
            }
            catch (Exception)
            {
                ME(new XMLFileErrorEventArgs("Could not find PRValue in Config.xml \nUsing 132", false));
            }
            return null;
        }

        #region Script
        public void RunScript(String File)
        {
            if (this.ScriptRunning == null)
            {
                this.ScriptWorker.RunWorkerCompleted += new RunWorkerCompletedEventHandler(ScriptHandler);
                this.ScriptWorker.ProgressChanged += new ProgressChangedEventHandler(ScriptHandler);
                this.ScriptRunning = new Script(File, this, this.ScriptWorker);
                this.DeliverScriptList = new List<EventArgs>();
                RunScriptEventArgs e = new RunScriptEventArgs();
                e.Cancel = false;
                this.ME(e);
                this.ScriptWorker.RunWorkerAsync();
                //Thread oThread = new Thread(new ThreadStart(this.ScriptRunning.Go));
                //oThread.Start();
            }
        }

        private void ScriptHandler(object sender, EventArgs e)
        {
            if (e is ProgressChangedEventArgs)
            {
                ProgressChangedEventArgs ev = e as ProgressChangedEventArgs;
                if (ev.UserState is ConnectEventArgs)
                {
                    ConnectEventArgs eve = ev.UserState as ConnectEventArgs;
                    if (eve.Connecting)
                        Connect(eve.ID, eve.Connect, eve.Timer, eve.Timeout);
                    else
                        Disconnect();
                }
                else if (ev.UserState is PauseEventArgs)
                {
                    Pause(((PauseEventArgs)ev.UserState).Pause);
                }
                else if (ev.UserState is FaultInjectionEventArgs)
                {
                    SafetyInjection((FaultInjectionEventArgs)ev.UserState);
                }
                else if (ev.UserState is SendMessageEventArgs)
                {
                    SendMessageEventArgs eve = ev.UserState as SendMessageEventArgs;
                    SendFromFile(eve.File);
                }
                else if (ev.UserState is SendBytes)
                {
                    SendBytes(((SendBytes)ev.UserState).Message, Message.string_MTypeFromBytes(((SendBytes)ev.UserState).Message));
                }
                else if (ev.UserState is SendXML)
                {
                    XElement xm = ((SendXML)ev.UserState).Message;
                    byte[] msg = Message.XMLtoByte(xm);
                    SendBytes(msg, xm.Name.ToString());
                }
                else if (ev.UserState is ScriptOutputEventArgs)
                {
                    ME(ev.UserState as ScriptOutputEventArgs);
                }
            }
            else if (e is RunWorkerCompletedEventArgs)
            {
                this.ScriptRunning = null;
                this.DeliverScriptList = null;
                this.ScriptWorker.ProgressChanged -= new ProgressChangedEventHandler(ScriptHandler);
                this.ScriptWorker.RunWorkerCompleted -= new RunWorkerCompletedEventHandler(ScriptHandler);
                this.ME(new RunScriptEventArgs());
            }
        }

        public void CancelScript()
        {
            if(this.ScriptRunning != null)
                this.ScriptRunning.thread.Abort();
        }

        #endregion

        
    }
}
