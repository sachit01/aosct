/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2014-07-18    Hidaji      Added site ID
* 2016-06-28    akushwah    Added region ID
* 2017-03-22    marlundg    Updates related to new CRC64 calculation
* 2017-05-08    skothiya    Updates related to PRR Q_SETUP parameter
*******************************************************************************/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using System.IO;
using System.Reflection;
using System.Threading;
#if TCCSERIAL
using TCCserial;
#else
using TCCtcp;
#endif
using System.ComponentModel;
using System.Windows.Forms;
using System.Xml;
using System.Collections;

namespace TCCSim
{
    public class Model
    {
        public event MEvent ME;
        public delegate void MEvent(EventArgs e);
        public static uint qInitiateState = 0;
        private MsgDef MessageXMLHandler;
        private Communication Com;
        public String DLLCaption;
        private Script ScriptRunning;
        public String DefaultPRR;
        private String DefaultProVer;
        private String DefaultDriverLogonStatus;
        private String YardAcknowledment;
        private String ShuntingAcknowledgment;
        private String PossessionAcknowledgment;
        private XElement XMLDriverLogonStatus;
        private String NameDriverLogonStatus;
        public String MessageDir {get; private set;}
        public BackgroundWorker Worker { set; get; }
        public BackgroundWorker ScriptWorker { set; get; }
        public List<DeliverEventArgs> DeliverList { private set; get; }
        public List<EventArgs> DeliverScriptList { private set; get; }
        private short TriggerValue = 128;
        public bool PauseEnable { private set; get; }
        public bool PauseTag { private set; get; }
        public String PauseText { private set; get; }
        public String PauseLabel { private set; get; }

        public bool ConnectEnable { private set; get; }
        public bool ConnectTag { private set; get; }
        public String ConnectText { private set; get; }

        public string ConfigPath { get; set; }
        private string DriversPath { get; set; }

        private const uint AreaRequestID = 19;
        private const uint RegistrationAreaID = 134;

        private tccCrc crcUsedWithConnection;

        private static UInt32 T_CLOCK_Min_Value = 0;

        /* decrypt key for password obfuscation */

        private static Byte[] decrypt_key = new Byte[]
       { 0x23, 0x74, 0x4e, 0x71, 0x38, 0x2F, 0x11, 0xFB, 0xC3, 0xD9, 0x23, 0x74, 0x4e, 0x71, 0x38, 0x2F, 0x11, 0xFB, 0xC3, 0xD9 };
        public Model()
        {
            this.crcUsedWithConnection = tccCrc.tccCrcRegion;

            this.PauseEnable = false;
            this.PauseTag = false;
            this.PauseText = "Pause";
            this.PauseLabel = "";

            this.ConnectEnable = true;
            this.ConnectTag = false;
            this.ConnectText = "Connect";

            try
            {
                this.MessageXMLHandler = new MsgDef();
            }
            catch (Exception)
            {
                DialogResult result = System.Windows.Forms.MessageBox.Show(
                    "Something is wrong with the message definition XML files.", "Error",
                        MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
#if DEBUG
            this.MessageDir = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                                    @"..\..\Messages\");
#else
            this.MessageDir = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                        @".\Messages\");
#endif
            this.DeliverList = new List<DeliverEventArgs>();
            
            /* Setup config path. */
#if DEBUG
            ConfigPath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                @"..\..\Config.xml");
            DriversPath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                @"..\..\Drivers.xml");
#else
            ConfigPath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                @".\Config.xml");
            DriversPath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                @".\Drivers.xml");
#endif
        }

        #region Communication


        /// <summary>
        /// Validates all CRCs in a message
        /// </summary>
        /// <param name="msg"></param>
        bool validateCRC(byte[] msg, tccCrc crcToUse)
        {
            // Fetch and calculate values used for validating CRC
            UInt16 dataLength = Message.GetDataLength(msg);
            ulong[] crcs = Message.GetCRCArray(msg);

            // Number of CRCS is identical to number of Message Chunks
            UInt16 messageChunks = (UInt16)crcs.Length;
           
            bool crcError = false;

            for (int index = 0; index < messageChunks; index++)
            {
                UInt16 crcLength = 0;
                UInt16 crcPosition = 0;

                if (index == 0U)
                {
                    // Do not include STX, start from ID-field
                    crcPosition = 1;

                    // Calculate length to use for CRC calculation, exclude 1 byte for STX that is not included in calculation.
                    crcLength = (ushort)((Message.expectedHeaderLength - 1) + ((dataLength < Message.maxLengthOfMessageChunks) ? dataLength : Message.maxLengthOfMessageChunks));
                }
                else
                {
                    // Calculate start position in message to use for CRC calculation (Header-STX + number of already processed chunks).
                    crcPosition = (ushort)(Message.expectedHeaderLength + (index * Message.maxLengthOfMessageChunks));

                    // Calculate length to use for CRC calculation, either rest of message or maxLengthOfMessageChunks.
                    crcLength = (ushort)(((index + 1U) == messageChunks) ? (dataLength - ((index * Message.maxLengthOfMessageChunks))) : Message.maxLengthOfMessageChunks);
                }

                // Calculate and compare CRC64 for each chunk of the message.
                ulong crc64;
                if (crcToUse == tccCrc.tccCrcRegion)
                {
                    crc64 = Safety.calculateCRC64(msg.Skip(crcPosition).ToArray(), crcLength);
                } 
                else
                {
                    crc64 = Safety.calculateCRC64Central(msg.Skip(crcPosition).ToArray(), crcLength);
                }

                if (crcs[index] != crc64)
                {
                    crcError = true;
                }
            }
            return crcError;
        }

        /// <summary>
        /// Handle event sent from the communication thread
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void ComHandler(object sender, EventArgs e)
        {
            if (e is ProgressChangedEventArgs)
            {
                ProgressChangedEventArgs ev = e as ProgressChangedEventArgs;
                // A message is being delivered
                if (ev.UserState is DeliverEventArgs)
                {
                    DeliverEventArgs eve = ev.UserState as DeliverEventArgs;

                    // Check what message is is and act from it
                    ArrayList l = Message.ParseBytes(eve.Data);
                    
                    Message.MessageType type = (Message.MessageType)eve.MessageType;
                    //System.Windows.Forms.MessageBox.Show(l, "Message type", MessageBoxButtons.OK, MessageBoxIcon.Information);
                    switch (type)
                    {
                        case Message.MessageType.DriverInfo:
                            int count = l.Count;
                            FieldType Driver = (FieldType)l[count - 2];
                            FieldType Password = (FieldType)l[count - 1];

                            StringBuilder sb = new StringBuilder(Driver.values[0].ToString().TrimEnd('\0'));
                            String usr = sb.ToString();

                             sb = new StringBuilder(Password.values[0].ToString());
                             String pwd = sb.ToString();

                           Byte[] message = Message.XMLtoByte(XMLDriverLogonStatus);
                            message[1] = (Byte) (ValidateDriverinfo(usr, pwd) ? 1 : 0);
                                
                            lock (this.Com.SendL)
                            {
                                this.Com.SendL.Insert(0, new DeliverEventArgs(message, this.NameDriverLogonStatus));
                            }
                            break;

                        case Message.MessageType.PositionReport:
                            //Reply with YardAcknowledgement, PossessionAcknowledgment and ShuntingAcknowledgment messages
                            //If PR recieved with YARD_REQUEST, POSSESSION_REQUEST and SHUNTING_REQUEST blocks
                            //Response will be uploaded from XML present in message folder
                            //Posetive or Negatvie Acknowledgment will be based on data present in XML
                            int fieldCount = l.Count;
                            bool isYardRequestRecieved = false;
                            bool isPossessionReqRec = false;
                            bool isShuntingReqRec = false;
                            for (int i = 1; i < fieldCount; i++) //Searching for YARD_REQUEST, POSSESSION_REQUEST and SHUNTING_REQUEST block in recived message
                            {
                                if (l[fieldCount - i].GetType() == typeof(BlockType))
                                {
                                   BlockType data = (BlockType)l[fieldCount - i];
                                  // System.Windows.Forms.MessageBox.Show(data.name, "Message type", MessageBoxButtons.OK, MessageBoxIcon.Information);
                                    if (data.name == "YARD_REQUEST")
                                    {
                                        //YARD_REQUEST block recieved in message so breaking from the search loop
                                        //Considering iether YARD_REQUEST, POSSESSION_REQUEST and SHUNTING_REQUEST block will be present in message
                                        isYardRequestRecieved = true;
                                        break;
                                    }
                                    else if (data.name == "POSSESSION_REQUEST")
                                    {
                                        isPossessionReqRec = true;
                                        break;
                                    }
                                    else if (data.name == "SHUNTING_REQUEST")
                                    {
                                        isShuntingReqRec = true;
                                    }
                                    else
                                    {
                                        //Do nothing
                                    }

                                }
                            }

                            if (isYardRequestRecieved)
                            {
                                try
                                {
                                    //Uploading YardAcknowledge.xml from message folder
                                    this.YardAcknowledment = this.MessageDir + "YardAcknowledge.xml";
                                }
                                catch (Exception)
                                {
                                    ME(new XMLFileErrorEventArgs("Could not load YardAcknowledge XML file(s).", false));
                                }
                                XElement yardAck = XElement.Load(YardAcknowledment);
                                String YardAckName = yardAck.Name.ToString();
                                Byte[] yardAckmessage = Message.XMLtoByte(yardAck);
                                lock (this.Com.SendL)
                                {
                                    this.Com.SendL.Insert(0, new DeliverEventArgs(yardAckmessage, YardAckName));
                                }
                            }
                            else if (isPossessionReqRec)
                            {
                                try
                                {
                                    this.PossessionAcknowledgment = this.MessageDir + "PossessionAcknowledge.xml";
                                }
                                catch (Exception)
                                {
                                    ME(new XMLFileErrorEventArgs("Could not load PossessionAcknowledge XML file(s).", false));
                                }

                                XElement possessionAck = XElement.Load(this.PossessionAcknowledgment);
                                String possessionAckName = possessionAck.Name.ToString();
                                Byte[] possessionAckmessage = Message.XMLtoByte(possessionAck);
                                lock (this.Com.SendL)
                                {
                                    //Sending response to AOS
                                    this.Com.SendL.Insert(0, new DeliverEventArgs(possessionAckmessage, possessionAckName));
                                }
                            }
                            else if (isShuntingReqRec)
                            {

                                try
                                {
                                    this.ShuntingAcknowledgment = this.MessageDir + "ShuntingAcknowledge.xml";
                                }
                                catch (Exception)
                                {
                                    ME(new XMLFileErrorEventArgs("Could not load ShuntingAcknowledge XML file(s).", false));
                                }

                                XElement shuntingAck = XElement.Load(this.ShuntingAcknowledgment);
                                String shuntingAckName = shuntingAck.Name.ToString();
                                Byte[] shuntingAckmessage = Message.XMLtoByte(shuntingAck);
                                lock (this.Com.SendL)
                                {
                                    this.Com.SendL.Insert(0, new DeliverEventArgs(shuntingAckmessage, shuntingAckName));
                                }

                            }
                            else
                            {
                                //Do nothing
                            }

                            break;
                        case Message.MessageType.PositionReportRequest:
                            break;

                        case Message.MessageType.ProtocolVersion:
                            Byte ProResponse = eve.Data[12];
                            if (ProResponse == 1)
                            {
                                this.Com.ProtocolVerified = true;
                            }

                            break;

                        default:
                            // System.Windows.Forms.MessageBox.Show(type.ToString(), "Message type", MessageBoxButtons.OK, MessageBoxIcon.Information);

                            break;
                    }

                    // Give unique id so i can be looked up in the deliver list
                    eve.ID = DeliverList.Count;
                    
                    // A received message has to be given a file
                    if (!eve.Sent)
                        eve.Name = Message.string_MTypeFromBytes(eve.Data);

                    eve.CRCError = validateCRC(eve.Data, crcUsedWithConnection);

                    DeliverList.Add(eve);
                    ME(eve);

                    // A received message should be delivered to a running script
                    if (!eve.Sent)
                        AddToDeliverScript(eve);
                }
                else if (ev.UserState is ConnectedEventArgs)
                {
                    ConnectedEventArgs eve = ev.UserState as ConnectedEventArgs;
                    // Inform main view and a running script that a connection
                    // has been established or the has been disconnected
                    ME(eve);
                    AddToDeliverScript(eve);
                }
                else if (ev.UserState is SocketParaErrorEventArgs)
                {
                    this.PauseEnable = false;
                    // Something was wrong with the given connection parameters
                    //Main view and a running script is informed
                    SocketParaErrorEventArgs eve = ev.UserState as SocketParaErrorEventArgs;
                    ME(eve);
                    AddToDeliverScript(eve);
                }
                else if (ev.UserState is SocketErrorEventArgs)
                {
                    //The connection has been compromised
                    //Main view and a running script is informed
                    SocketErrorEventArgs eve = ev.UserState as SocketErrorEventArgs;
                    ME(eve);
                    AddToDeliverScript(eve);
                }
                else if (ev.UserState is UnknownErrorEventArgs)
                {
                    // A unknown error has occurred
                    Console.WriteLine("UNKNOWN ERROR: " + ((UnknownErrorEventArgs)ev.UserState).Text);
                    UnknownErrorEventArgs eve = ev.UserState as UnknownErrorEventArgs;
                    ME(eve);
                }
            }
            else if (e is RunWorkerCompletedEventArgs)
            {
                // The communication thread has returned

                this.ConnectText = "Connect";
                this.ConnectTag = false;
                this.ConnectEnable = true;
                this.PauseEnable = false;
                this.PauseText = "Pause";

                ME(new ConnectedEventArgs(false));
                // The backgroundworker might be used again so the eventhandlers are deattached
                Worker.ProgressChanged -= new ProgressChangedEventHandler(ComHandler);
                Worker.RunWorkerCompleted -= new RunWorkerCompletedEventHandler(ComHandler);
                this.Com = null;

                // Clear all input to the script
                if (this.DeliverScriptList != null)
                {
                    lock (this.DeliverScriptList)
                    {
                        this.DeliverScriptList.Clear();
                    }
                }

            }
        }

        private bool ValidateDriverinfo(string usr, string pwd)
        {
            try
            {
                XElement Driverinfo = XElement.Load(DriversPath); 
                
                foreach (XElement driver in Driverinfo.Descendants("Driver"))
                {
                    String name = driver.Element("Name").Value;
                    String password = driver.Element("Password").Value;

                    // Decoding the password with 'decrypt key'

                    StringBuilder sb = new StringBuilder();
                    for (int i = 0; i < pwd.Length; i++)
                        sb.Append((char)(pwd[i] ^ decrypt_key[(i % decrypt_key.Length)]));
                    String decoded_pw = sb.ToString().TrimEnd('\0');

                    if (name.ToLower().Equals(usr.ToLower()) &&
                       password.Equals(decoded_pw))
                    {
                        return true;
                    }
                }
            }
            catch (FileNotFoundException e)
            {
                MessageBox.Show("Drivers.xml are not found at " + DriversPath);
            }
            
            return false;
        }

        /// <summary>
        /// Helper function to extract the Crc role parameter
        /// </summary>
        /// <param name="Conf"></param>
        private tccCrc getTccCrcFromXML(XElement Conf)
        {
            tccCrc crcv = tccCrc.tccCrcRegion;
            try
            {
                string crc = Conf.Element("Crc").Value;
                crc = crc.ToLower();
                if (crc.CompareTo("central") == 0)
                {
                    crcv  = tccCrc.tccCrcCentral;
                }
                else if (crc.CompareTo("region") == 0)
                {
                    crcv = tccCrc.tccCrcRegion;
                }
            }
            catch (Exception)
            {
                crcv = tccCrc.tccCrcRegion;
            }
            return crcv;
        }

        /// <summary>
        /// Wrapper for calls to Connect without autoconnect parameter.
        /// </summary>
        /// <param name="ID"></param>
        /// <param name="_Connect"></param>
        /// <param name="Timer"></param>
        /// <param name="Timeout"></param>
        public void Connect(String ID, String siteID, String regionID, String _Connect, String Timer, String Timeout)
        {
            XElement Conf = XElement.Load(ConfigPath);

            bool autoconn;
            try
            {
                autoconn = bool.Parse(Conf.Element("AutoConnect").Value);
            }
            catch (Exception) { autoconn = false; }

            /* read the Crc setting from config */
            tccCrc crc = getTccCrcFromXML(Conf);

            Connect(ID, siteID, regionID, _Connect, Timer, Timeout, autoconn, crc);
        }
        /// <summary>
        /// Initiates the communication object and start it.
        /// </summary>
        /// <param name="ID"></param>
        /// <param name="Connect"></param>
        /// <param name="Timer"></param>
        /// <param name="Timeout"></param>
        /// <param name="AutoConnect"></param>
        public void Connect(String ID, String siteID, String regionID, String Connect, String Timer, String Timeout, bool AutoConnect, tccCrc crc)
        {
            // register the crc type to use for ComHandler
            this.crcUsedWithConnection = crc;
            
            // Connect twice is not possible
            if (this.Com != null)
                return;

            this.ConnectText = "Disconnect";
            this.ConnectTag = true;
            this.PauseEnable = true;

            // Get XML files for default PRR and DLS
            try
            {
                XDocument Conf = XDocument.Load(ConfigPath);
                XElement Elem = (from C in Conf.Descendants("Config")
                                 select C).First();
                this.DefaultPRR = this.MessageDir + Elem.Element("PRRMessage").Value;
                this.DefaultProVer = this.MessageDir + Elem.Element("ProtocolMessage").Value;
                this.DefaultDriverLogonStatus = this.MessageDir + Elem.Element("DLSMessage").Value;
            }
            catch (Exception)
            {
                ME(new XMLFileErrorEventArgs("Could not find PRRMessage/DLSMessage in Config.xml \nUsing DefaultPRR.xml and DefaultDriverLogonStatus.xml", false));
                this.DefaultPRR = this.MessageDir + "DefaultPRR.xml";
                this.DefaultProVer = this.MessageDir + "ProtocolVersion.xml";
                this.DefaultDriverLogonStatus = this.MessageDir + "DefaultDriverLogonStatus.xml";
                
            }

            // And load the default messages
            String LoopName;
            Byte[] LoopMessage;
            String ProtocolName;
            Byte[] ProtocolMessage;
            try
            {
                XElement XPRR = XElement.Load(DefaultPRR);
                LoopName = XPRR.Name.ToString();
                LoopMessage = Message.XMLtoByte(XPRR);

                XElement XProVer = XElement.Load(DefaultProVer);
                ProtocolName = XProVer.Name.ToString();
                ProtocolMessage = Message.XMLtoByte(XProVer);

                this.XMLDriverLogonStatus = XElement.Load(DefaultDriverLogonStatus);
                this.NameDriverLogonStatus = this.XMLDriverLogonStatus.Name.ToString();
              }
            catch(Exception)
            {
                // Abort connection if the default PRR or DLS could not be loaded.
                this.ConnectText = "Connect";
                this.ConnectTag = false;
                this.ConnectEnable = true;

                this.PauseEnable = false;
                this.PauseText = "Pause";
                this.PauseTag = false;
                this.PauseLabel = "";
                ME(new XMLFileErrorEventArgs("Could not load default PRR/DriverLogonStatus/ProtocolVersion XML file(s).", true));
                AddToDeliverScript(new SocketParaErrorEventArgs("Could not load default PRR/DriverLogonStatus/ProtocolVersion XML file(s)."));
                return;
            }

            // Gets the message type value for which this.XMLDriverLogonStatus should be replied with
            try
            {
                XDocument Conf = XDocument.Load(ConfigPath);
                XElement Elem = (from C in Conf.Descendants("Config")
                                 select C).First();
                this.TriggerValue = Int16.Parse(Elem.Element("DIValue").Value);
            }
            catch (Exception)
            {
                ME(new XMLFileErrorEventArgs("Could not find DIValue in Config.xml \nUsing 128", false));
            }

            // Gets the maximum length of an inbound message
            int BufferLength;
            try
            {
                XDocument Conf = XDocument.Load(ConfigPath);
                XElement Elem = (from C in Conf.Descendants("Config")
                                 select C).First();
                BufferLength = Int16.Parse(Elem.Element("BufferLength").Value);
            }
            catch (Exception)
            {
                ME(new XMLFileErrorEventArgs("Could not find BufferLength in Config.xml \nUsing 1000", false));
                BufferLength = -1;
            }

            // Register communication event handlers and start the communication thread.
            Worker.ProgressChanged += new ProgressChangedEventHandler(ComHandler);
            Worker.RunWorkerCompleted += new RunWorkerCompletedEventHandler(ComHandler);

            crcType crcToUse = (crc == tccCrc.tccCrcCentral) ? crcType.crcTypeCentral : crcType.crcTypeRegion;
            this.Com = new Communication(Timer, Connect, ID, siteID, regionID, Timeout, crcToUse, LoopMessage, LoopName, ProtocolMessage, ProtocolName, Worker, BufferLength);
            DLLCaption = this.Com.DLLCaption();
            Worker.RunWorkerAsync();

            this.PauseEnable = true;
            
            // Update Config.xml with entered values.
            XElement conf_up = XElement.Load(ConfigPath);
            conf_up.Element("TrainID").Value = ID;
            conf_up.Element("SiteID").Value = siteID;
            conf_up.Element("RegionID").Value = regionID;
            conf_up.Element("Connect").Value = Connect;
            conf_up.Element("Timer").Value = Timer;
            conf_up.Element("Timeout").Value = Timeout;
            conf_up.Element("Crc").Value = (crcToUse == crcType.crcTypeRegion) ? "Region" : "Central";

            conf_up.Element("AutoConnect").Value = AutoConnect.ToString();
            try
            {
                conf_up.Save(ConfigPath);
            }
            catch (Exception) { /* To prevent the connection dialogue to be displayed twice if the Config.xml would be write protected. */ }
        }

        /// <summary>
        /// Call asynchronous cancel on communication thread
        /// </summary>
        public void Disconnect()
        {
            this.Com.Pause.Set();
            Worker.CancelAsync();

            this.ConnectEnable = false;
            this.PauseEnable = false;
            this.PauseText = "Pause";
            this.PauseTag = false;
            this.PauseLabel = "";

            this.Com.ProtocolVerified = false;
        }

        /// <summary>
        /// Toggle pause
        /// </summary>
        /// <param name="Pause"></param>
        public void Pause(bool Pause)
        {
            if (this.PauseTag)
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

        /// <summary>
        /// Sends the given byte array given
        /// </summary>
        /// <param name="Data"></param>
        /// <param name="Name"></param>
        public void SendBytes(Byte[] Data, String Name)
        {
            // Not if given data is null
            if (Data == null)
            {
                this.ME(new XMLParseErrorEventArgs("Could not send message"));
            }
            // or there is no Communication object
            else if (this.Com != null)
            {
                //Check if current message is PositionReportRequest
                if ((Name.ToString() == "PositionReportRequest"))
                {
                    changeDefaultPRR();     
                }
                lock (this.Com.SendL)
                {
                    this.Com.SendL.Add(new DeliverEventArgs( Data, Name));
                }
            }
        }

        /// <summary>
        /// Checks if the incoming message should be responded to
        /// with the default DLS message
        /// </summary>
        /// <param name="Data"></param>
        //private void TriggerCheck(Byte[] Data)
        //{
        //    if (Data[8] == this.TriggerValue)
        //    {
        //        lock (this.Com.SendL)
        //        {
        //            this.Com.SendL.Insert(0, new DeliverEventArgs(Message.XMLtoByte(XMLDriverLogonStatus), this.NameDriverLogonStatus));
        //        }
        //    }
        //}

        /// <summary>
        /// Sets fault injection on the next outbound message with flags,
        /// except for fragmentation which uses a list to store the delay time
        /// </summary>
        /// <param name="e"></param>
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

        /// <summary>
        /// Loads an XML file to send
        /// </summary>
        /// <param name="File"></param>
        public void SendFromFile(String File)
        {
            // if there is a Communication object
            if (this.Com != null)
            {
                try
                {
                    // Load the file
                    XElement xelem = XElement.Load(File);
                    Byte[] Data = Message.XMLtoByte(xelem);
                    if (Data == null)
                        throw new Exception();
                    
                    //Check if current message is PositionReportRequest
                    if ((xelem.Name.ToString() == "PositionReportRequest"))
                    {
                        changeDefaultPRR();
                    }

                    lock (this.Com.SendL)
                    {
                        // Add the message to send list
                        this.Com.SendL.Add(new DeliverEventArgs(Data, xelem.Name.ToString(), Path.GetFileName(File)));
                    }
                }
                catch (Exception)
                {
                    XMLFileErrorEventArgs e = new XMLFileErrorEventArgs("Unable to read/parse XML file: " + File, false);
                    ME(e);
                }
            }
        }

        /// <summary>
        /// Clears the list with sent and received messages
        /// </summary>
        public void ClearDeliverList()
        {
            this.DeliverList.Clear();
        }

        #endregion

        #region TransferOptions

        /// <summary>
        /// Saves the current transfered messages into the given path
        /// </summary>
        /// <param name="File"></param>
        public void SaveTransfer(String File)
        {
            // Open file
            TextWriter tw = new StreamWriter(File);
            //Writes each message sent/received on each row of the file
            // Format: <time> <byte.byte..> <sent> <T_SENDER error> <T_REF error>
            foreach (DeliverEventArgs e in DeliverList)
            {
                String output = e.Time + " ";
                output += e.Name + " ";
                foreach (Byte b in e.Data)
                {
                    output += b.ToString() + ".";
                }
                output = output.TrimEnd('.');
                output += " " + e.Sent.ToString() + " " + e.TSSenderError.ToString() + " " + e.TSRefError.ToString() + " " + e.IDError.ToString() + " " + e.siteIDError.ToString() + " " + e.regionIDError.ToString();
                tw.WriteLine(output);
            }
            tw.Close();
        }

        /// <summary>
        /// Clears the current transfered messages and load 
        /// the messages from the file in the given path
        /// </summary>
        /// <param name="File"></param>
        public void LoadTransfer(String File)
        {
            Safety.Init();

            TextReader tr = new StreamReader(File);
            String input;
            // Clear current messages
            DeliverList.Clear();
            int j = 1;
            //Open file and read each line
            while ((input = tr.ReadLine()) != null)
            {
                String[] row = input.Split(' ');
                try
                {
                    String[] data = row[2].Split('.');
                    Byte[] bytes = new Byte[data.Length];
                    // Read the bytes of the row
                    for (int i = 0; i < data.Length; i++)
                    {
                        bytes[i] = Convert.ToByte(data[i]);
                    }
                    // Create a message object with the rest of the data on the row
                    DeliverEventArgs e = new DeliverEventArgs(Message.expectedHeaderLength, bytes, Convert.ToBoolean(row[3]), row[0],
                        Convert.ToBoolean(row[4]), Convert.ToBoolean(row[5]));
                    e.Name = row[1];
                    e.ID = DeliverList.Count;
                    e.CRCError = validateCRC(e.Data, crcUsedWithConnection);
                    e.IDError = Convert.ToBoolean(row[6]); 
                    e.siteIDError = Convert.ToBoolean(row[7]);
                    e.regionIDError = Convert.ToBoolean(row[8]);

                    DeliverList.Add(e);
                }
                //The row could not be parsed
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
        #endregion

        #region MsgDef-calls
        
        /// <summary>
        /// From a message type name it returns
        /// the message definition bound with that name
        /// </summary>
        /// <param name="MType"></param>
        public void GetMessageType(string MType)
        {
            GetTypeEventArgs e = MessageXMLHandler.GetMessageType(MType);
            ME(e);
        }

        /// <summary>
        /// Returns all message types
        /// </summary>
        /// <returns></returns>
        public Dictionary<string, FieldType> GetMessageTypes()
        {
            return MessageXMLHandler.GetMessageTypes();
        }

        /// <summary>
        /// Returns all message types that is stationary
        /// </summary>
        /// <returns></returns>
        public IEnumerable<StringInt> GetStationaryTypes()
        {
            return MessageXMLHandler.GetStationaryTypes();
        }

        /// <summary>
        /// Send fields of block with NID_BLOCK_TYPE == BlockID to view.
        /// </summary>
        /// <param name="BlockID"></param>
        public void GetBlock(string BlockID)
        {
            BlockFieldsEventArgs e = MessageXMLHandler.BlockFields(BlockID);
            ME(e);
        }

        /// <summary>
        /// Returns an error code associated with a code.
        /// </summary>
        /// <param name="Code"></param>
        /// <returns></returns>
        public String GetErrorDescription(int Code)
        {
            return MsgDef.GetErrorDescription(Code);
        }

        public void SaveFilterSettings(Dictionary<string, FieldType> Filter)
        {
            MsgDef.SaveFilters(Filter);
        }

        #endregion

        #region Config


        /// <summary>
        /// Returns Config connect data.
        /// Returns empty data if config cannot be parsed.
        /// </summary>
        public void GetConfig()
        {
            try
            {
                XDocument Conf = XDocument.Load(ConfigPath);
                IEnumerable<XElement> Elems = from C in Conf.Descendants("Config")
                                              select C;
                XElement Elem = Elems.First<XElement>();
                bool autoconn;
                bool.TryParse(Elem.Element("AutoConnect").Value, out autoconn);

                // crc local as default
                tccCrc crcv = tccCrc.tccCrcRegion;
                try
                {
                    string crc = Elem.Element("Crc").Value;
                    crc = crc.ToLower();
                    if (crc.CompareTo("central") == 0)
                    {   // CRC Central
                        crcv = tccCrc.tccCrcCentral;
                    }
                    else
                    {
                        crcv = tccCrc.tccCrcRegion;
                    }
                }
                catch (Exception)
                {
                }

                ME(new ConnectEventArgs(Elem.Element("TrainID").Value, Elem.Element("SiteID").Value, Elem.Element("RegionID").Value, Elem.Element("Connect").Value, Elem.Element("Timer").Value, Elem.Element("Timeout").Value, autoconn, crcv));
            }
            catch (Exception)
            {
                new ConnectEventArgs();
                ME(new ConnectEventArgs());
            }
        }

        /// <summary>
        /// Get the name of PR's error block from config
        /// </summary>
        /// <returns></returns>
        public String GetPRErrorBlock()
        {
            try
            {
                XDocument Conf = XDocument.Load(ConfigPath);
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

        /// <summary>
        /// Gets PR type value from config
        /// </summary>
        /// <returns></returns>
        public int GetPRValue()
        {
            try
            {
                XElement Conf = XElement.Load(ConfigPath);
                return Int32.Parse(Conf.Element("PRValue").Value);
            }
            catch (Exception)
            {
                ME(new XMLFileErrorEventArgs("Could not find PRValue in Config.xml \nUsing 132", false));
            }
            return -1;
        }

        public UInt32 Get_T_CLOCK_Min()
        {
            if (T_CLOCK_Min_Value == 0)
            {
                try
                {
                    XElement Conf = XElement.Load(ConfigPath);
                    T_CLOCK_Min_Value = UInt32.Parse(Conf.Element("T_CLOCK_min").Value);
                }
                catch (Exception)
                {
                    ME(new XMLFileErrorEventArgs("Could not find T_CLOCK_Min in Config.xml \n", false));
                }
            }
            return T_CLOCK_Min_Value;
        }
    #endregion

    #region Script

    /// <summary>
    /// Starts a Backgroundworker and runs a script in it.
    /// </summary>
    /// <param name="File"></param>
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
            }
        }

        /// <summary>
        /// Handles events sent from script thread
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void ScriptHandler(object sender, EventArgs e)
        {
            if (e is ProgressChangedEventArgs)
            {
                ProgressChangedEventArgs ev = e as ProgressChangedEventArgs;
                // Script calles connect/disconnect
                if (ev.UserState is ConnectEventArgs)
                {
                    ConnectEventArgs eve = ev.UserState as ConnectEventArgs;
                    if (eve.Connecting)
                        Connect(eve.ID, eve.siteID, eve.regionID, eve.Connect, eve.Timer, eve.Timeout);
                    else
                        Disconnect();
                }
                // Script calls pause/unpause
                else if (ev.UserState is PauseEventArgs)
                {
                    Pause(((PauseEventArgs)ev.UserState).Pause);
                }
                // Script calls one of the safety header fault injections
                else if (ev.UserState is FaultInjectionEventArgs)
                {
                    SafetyInjection((FaultInjectionEventArgs)ev.UserState);
                }
                // Script sends a message from a file
                else if (ev.UserState is SendMessageEventArgs)
                {
                    SendMessageEventArgs eve = ev.UserState as SendMessageEventArgs;
                    SendFromFile(eve.File);
                }
                // Script sends a message as byte array
                else if (ev.UserState is SendBytesEventArgs)
                {
                    SendBytes(((SendBytesEventArgs)ev.UserState).Message, Message.string_MTypeFromBytes(((SendBytesEventArgs)ev.UserState).Message));
                }
                // Script sends message from a XElement structure
                else if (ev.UserState is SendXMLEventArgs)
                {
                    XElement xm = ((SendXMLEventArgs)ev.UserState).Message;
                    byte[] msg = Message.XMLtoByte(xm);
                    SendBytes(msg, xm.Name.ToString());
                }
                // Script output
                else if (ev.UserState is ScriptOutputEventArgs)
                {
                    ME(ev.UserState as ScriptOutputEventArgs);
                }
            }
            // Script thread returned
            else if (e is RunWorkerCompletedEventArgs)
            {
                RunWorkerCompletedEventArgs ev = e as RunWorkerCompletedEventArgs;
                this.ScriptRunning = null;
                this.DeliverScriptList = null;
                this.ScriptWorker.ProgressChanged -= new ProgressChangedEventHandler(ScriptHandler);
                this.ScriptWorker.RunWorkerCompleted -= new RunWorkerCompletedEventHandler(ScriptHandler);
                this.ME(new RunScriptEventArgs());
            }
        }

        // Abort script thread
        public void CancelScript()
        {
            if(this.ScriptRunning != null)
                this.ScriptRunning.thread.Abort();
        }

        /// <summary>
        /// Adds messages and other events to the scripts incoming queue
        /// </summary>
        /// <param name="e"></param>
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

        #endregion

        /// <summary>
        /// Provides an XML file path for the MsgView to load
        /// </summary>
        /// <param name="e"></param>
        internal void MsgVLoadXML(LoadXMLEArgs e)
        {
            ME(e);
        }

        /// <summary>
        /// Provides a byte array for the MsgView to load
        /// </summary>
        /// <param name="e"></param>
        internal void MsgVLoadData(LoadDataEArgs e)
        {
            ME(e);
        }

        /// <summary>
        /// Reads all message files in the Messages folder
        /// The files are diplayed on the main view's right side
        /// </summary>
        /// <returns></returns>
        public List<MessageFile> InitWatchDir()
        {
            List<MessageFile> FileList = new List<MessageFile>();

            try
            {
                String[] Files = Directory.GetFiles(MessageDir);
                foreach (String f in Files)
                {
                    if (Path.GetExtension(f).ToLower() == ".xml")
                    {
                        FileList.Add(new MessageFile(f));
                    }
                }
            }
            catch (Exception) { }
            return FileList;

        }

        public List<MessageFile> ChangeWatchDir(String path)
        {
            MessageDir = path;

            return InitWatchDir();
        }


        //Changing DefaultPRR value runtime
        public void changeDefaultPRR()
        {
            // And load the default messages
            String LoopName;
            Byte[] LoopMessage;
            try
            {
                XElement XPRR = XElement.Load(DefaultPRR);
                if (!XPRR.Element("Q_INITIATE").IsEmpty)
                {
                    XPRR.Element("Q_INITIATE").Value = qInitiateState.ToString();
                    //   System.Windows.Forms.MessageBox.Show(qSetupType.ToString(), "setting new Q_SETUP", MessageBoxButtons.OK, MessageBoxIcon.Information);

                    LoopName = XPRR.Name.ToString();
                    LoopMessage = Message.XMLtoByte(XPRR);
                    lock (this.Com.LoopMessage)
                    {
                        this.Com.LoopMessage = LoopMessage;
                    }

                    lock (this.Com.LoopName)
                    {
                        this.Com.LoopName = LoopName;
                    }
                }
            }
            catch (Exception)
            {
                // Abort connection if the default PRR or DLS could not be loaded.
                this.ConnectText = "Connect";
                this.ConnectTag = false;
                this.ConnectEnable = true;

                this.PauseEnable = false;
                this.PauseText = "Pause";
                this.PauseTag = false;
                this.PauseLabel = "";
                ME(new XMLFileErrorEventArgs("Could not load default PRR/DriverLogonStatus/ProtocolVersion XML file(s).", true));
                AddToDeliverScript(new SocketParaErrorEventArgs("Could not load default PRR/DriverLogonStatus/ProtocolVersion XML file(s)."));
                return;
            }
        }

        /// Function that checks if autoconnect property is set and if so connects.
        /// 
        public void AutoConnect()
        {
            /* Check if we should autoconnect. */
            XElement Conf = XElement.Load(ConfigPath);
            bool autoconn;
            try
            {
                string ac = Conf.Element("AutoConnect").Value;
                autoconn = bool.Parse(ac);
            }
            catch (Exception) { autoconn = false; }

            /* read the Crc setting from config */
            tccCrc crc = getTccCrcFromXML(Conf);

            if (autoconn)
            {
                Connect(
                    Conf.Element("TrainID").Value
                    , Conf.Element("SiteID").Value
                    , Conf.Element("RegionID").Value
                    , Conf.Element("Connect").Value
                    , Conf.Element("Timer").Value
                    , Conf.Element("Timeout").Value
                    , autoconn, crc);
                ME(new MainViewFixButtonsArgs(Conf.Element("TrainID").Value
                    , Conf.Element("SiteID").Value
                    , Conf.Element("RegionID").Value
                    , Conf.Element("Connect").Value
                    , Conf.Element("Timer").Value
                    , Conf.Element("Timeout").Value, crc));
            }
        }
    }
}
