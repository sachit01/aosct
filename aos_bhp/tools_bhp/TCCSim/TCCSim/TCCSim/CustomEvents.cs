using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using System.Collections;

namespace TCCSim
{

    public enum tccCrc
    {
        tccCrcRegion = 0,
        tccCrcCentral = 1
    };

    /// <summary>
    /// Used when sending requests from MsgView to fetch message descriptions
    /// when changing message type.
    /// </summary>
    public class GetMessageEventArgs: EventArgs
    {
        public string MessageType { set; get; }
        public GetMessageEventArgs(string type)
        {
            MessageType = type;
        }
    }

    /// <summary>
    /// For returning a description of a message type.
    /// Used for responses to GetMessageEventArgs.
    /// </summary>
    public class GetTypeEventArgs : EventArgs
    {
        public IEnumerable<XElement> Message { set; get; }
        public ArrayList Fields { set; get; }
        public GetTypeEventArgs(IEnumerable<XElement> Message, ArrayList Fields)
        {
            this.Message = Message;
            this.Fields = Fields;
        }
    }

    /// <summary>
    /// Request of a block type.
    /// </summary>
    public class GetBlockEventArgs : EventArgs
    {
        public string BlockType { set; get; }
        public GetBlockEventArgs(string BlockType)
        {
            this.BlockType = BlockType;
        }
    }

    /// <summary>
    /// Response of a block type request.
    /// Used when adding block type to MsgView.
    /// </summary>
    public class BlockFieldsEventArgs : EventArgs
    {
        public IEnumerable<XElement> Block { set; get; }
        public ArrayList Fields { get; set; }
        public BlockFieldsEventArgs(IEnumerable<XElement> Block, ArrayList Fields)
        {
            this.Block = Block;
            this.Fields = Fields;
        }
    }

    public class ConnectEventArgs : EventArgs
    {
        public bool Connecting { set; get; }
        public String ID { set; get; }
        public String siteID { set; get; }
        public String regionID { set; get; }
        public String Connect { set; get; }
        public String Timer { set; get; }
        public String Timeout { set; get; }
        public bool AutoConnect { set; get; }
        public tccCrc Crc { set; get;  }

        public ConnectEventArgs()
        {
            this.Connecting = false;
        }
        public ConnectEventArgs(String ID, String siteID, String regionID, String Connect, String Timer, String Timeout)
        {
            this.Connecting = true;
            this.ID = ID;
            this.siteID = siteID;
            this.regionID = regionID;
            this.Connect = Connect;
            this.Timer = Timer;
            this.Timeout = Timeout;
        }
        public ConnectEventArgs(String ID, String siteID, String regionID, String Connect, String Timer, String Timeout, bool AutoConnect, tccCrc Crc)
        {
            this.Connecting = true;
            this.ID = ID;
            this.siteID = siteID;
            this.regionID = regionID;
            this.Connect = Connect;
            this.Timer = Timer;
            this.Timeout = Timeout;
            this.AutoConnect = AutoConnect;
            this.Crc = Crc;
        }
    }

    public class GetConnectDataEventArgs : EventArgs
    {
        public GetConnectDataEventArgs()
        {

        }
    }

    public class PauseEventArgs : EventArgs
    {
        public bool Pause { set; get; }

        public PauseEventArgs(bool Pause)
        {
            this.Pause = Pause;
        }
    }

    public class LoadTransferEventArgs : EventArgs
    {
        public String File { set; get; }

        public LoadTransferEventArgs(String File)
        {
            this.File = File;
        }
    }

    public class SaveTransferEventArgs : EventArgs
    {
        public String File { set; get; }

        public SaveTransferEventArgs(String File)
        {
            this.File = File;
        }
    }

    public class ClearDeliverListEventArgs : EventArgs
    {
        public ClearDeliverListEventArgs()
        {

        }
    }

    public class SendMessageEventArgs : EventArgs
    {
        public String File { set; get; }
        public SendMessageEventArgs(String File)
        {
            this.File = File;
        }
    }

    public class XMLFileErrorEventArgs : EventArgs
    {
        public String Text { set; get; }
        public bool CancelConnection { set; get; }
        public XMLFileErrorEventArgs(String Text, bool CancelConnection)
        {
            this.Text = Text;
            this.CancelConnection = CancelConnection;
        }
    }

    public class UpdateTransfersList : EventArgs
    {
        public UpdateTransfersList() { }
    }

    public class LoadTransferErrorEventArgs : EventArgs
    {
        public String Text { set; get; }
        public LoadTransferErrorEventArgs(String Text) 
        {
            this.Text = Text;
        }
    }

    /// <summary>
    /// Used to request sending of a message formated as XML.
    /// </summary>
    public class SendXMLEventArgs : EventArgs
    {
        public XElement Message { set; get; }
        
        public SendXMLEventArgs(XElement Message)
        {
            this.Message = Message;
        }
    }
    /// <summary>
    /// Used to request sending of a message byte string.
    /// </summary>
    public class SendBytesEventArgs : EventArgs
    {
        public byte[] Message { set; get; }
        public String Name { set; get; }

        public SendBytesEventArgs(byte[] Message)
        {
            this.Message = Message;
        }

        public SendBytesEventArgs(byte[] Message, String Name)
        {
            this.Message = Message;
            this.Name = Name;
        }
    }

    public class OpenMsgView : EventArgs
    {
        public String File { set; get; }

        public OpenMsgView()
        { ;}

        public OpenMsgView(String File)
        {
            this.File = File;
        }
    }

    public class OpenButtonView : EventArgs
    {
        public String File { set; get; }

        public OpenButtonView()
        { ;}

        public OpenButtonView(String File)
        {
            this.File = File;
        }
    }

    public class LoadXMLEArgs : EventArgs
    {
        public string File { get; private set; }
        public LoadXMLEArgs(string File)
        {
            this.File = File;
        }
    }

    public class LoadDataEArgs : EventArgs
    {
        public Byte[] Data { get; private set; }
        public LoadDataEArgs(Byte[] Data)
        {
            this.Data = Data;
        }
    }

    public class RunScriptEventArgs : EventArgs
    {
        public String File { get; set; }
        public bool Cancel { get; set; }
        public RunScriptEventArgs(String File)
        {
            this.Cancel = false;
            this.File = File;
        }

        public RunScriptEventArgs()
        {
            this.Cancel = true;
        }
    }

    public class ScriptOutputEventArgs : EventArgs
    {
        public String Text { set; get; }
        public ScriptOutputEventArgs(String Text)
        {
            this.Text = Text;
        }
    }

    public class XMLParseErrorEventArgs : EventArgs
    {
        public String Text { set; get; }
        public XMLParseErrorEventArgs(String Text)
        {
            this.Text = Text;
        }
    }

    public class SaveFilterEventArgs : EventArgs
    {
        public Dictionary<string, FieldType> Filter {set;get;}
        public SaveFilterEventArgs(Dictionary<string, FieldType> Filter)
        {
            this.Filter = Filter;
        }
    }

    public class MainViewFixButtonsArgs : EventArgs
    {
        public String ID { set; get; }
        public String siteID { set; get; }
        public String regionID { set; get; }
        public String Connect { set; get; }
        public String Timer { set; get; }
        public String Timeout { set; get; }
        public tccCrc Crc { set; get; }

        public MainViewFixButtonsArgs(String ID, String siteID, String regionID, String Connect, String Timer, String Timeout, tccCrc Crc)
        {
            this.ID = ID;
            this.siteID = siteID;
            this.regionID = regionID;
            this.Connect = Connect;
            this.Timer = Timer;
            this.Timeout = Timeout;
            this.Crc = Crc;
        }
    }

    public class ButtonSetEventArgs : EventArgs
    {
        public List<ButtonConfiguration[]> buttonSets { set; get; }

        public ButtonSetEventArgs(List<ButtonConfiguration[]> _buttonSets)
        {
            this.buttonSets = _buttonSets;
        }
    }
}
