using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using System.Collections;

namespace TCCSim
{
    public class GetMessageEventArgs: EventArgs
    {
        public string MessageType { set; get; }
        public GetMessageEventArgs(string type)
        {
            MessageType = type;
        }
    }

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

    public class GetBlockEventArgs : EventArgs
    {
        public string BlockType { set; get; }
        public GetBlockEventArgs(string BlockType)
        {
            this.BlockType = BlockType;
        }
    }

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

    public class BlockAddRemoveEventArgs : EventArgs
    {
        public int BlockID { get; set; }
        public BlockAddRemoveEventArgs() { }
    }



    public class ConnectEventArgs : EventArgs
    {
        public bool Connecting { set; get; }
        public String ID { set; get; }
        public String Connect { set; get; }
        public String Timer { set; get; }
        public String Timeout { set; get; }

        public ConnectEventArgs()
        {
            this.Connecting = false;
        }

        public ConnectEventArgs(String ID, String Connect, String Timer, String Timeout)
        {
            this.Connecting = true;
            this.ID = ID;
            this.Connect = Connect;
            this.Timer = Timer;
            this.Timeout = Timeout;
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

    public class SendXML : EventArgs
    {
        public XElement Message { set; get; }
        
        public SendXML(XElement Message)
        {
            this.Message = Message;
        }
    }
    public class SendBytes : EventArgs
    {
        public byte[] Message { set; get; }

        public SendBytes(byte[] Message)
        {
            this.Message = Message;
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

    public class LoadXMLEArgs : EventArgs
    {
        public string File { get; private set; }
        public LoadXMLEArgs(string File)
        {
            this.File = File;
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
}
