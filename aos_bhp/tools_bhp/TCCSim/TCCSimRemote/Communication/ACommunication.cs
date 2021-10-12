using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Net;
using System.Net.Sockets;
using System.Collections;
using System.ComponentModel;

namespace TCCCom
{
    public abstract class ACommunication
    {
        protected float Time;
        protected float Timeout;
        protected Byte TrainID;
        protected String ConnectData;
        protected Byte[] LoopMessage;
        protected String LoopName;
        protected BackgroundWorker Worker;
        protected int BufferLength = 1000 - 8 - 5;

        public List<DeliverEventArgs> SendL = new List<DeliverEventArgs>();
        //public List<Byte[]> SendL = new List<Byte[]>();
        public ManualResetEvent Pause = new ManualResetEvent(true);
        public List<int> FragmentList = new List<int>();

        public AutoResetEvent[] FaultInjection { protected set; get; }
    }

    public class DeliverEventArgs : EventArgs
    {
        public Byte[] Data { set; get; }
        public bool Sent { set; get; }
        public String Time { set; get; }
        public String Name { set; get; }
        public bool TSSenderError { set; get; }
        public bool TSRefError { set; get; }
        public int ID { set; get; }
        public bool CRCError { set; get; }
        public bool BCCError { set; get; }

        public DeliverEventArgs(Byte[] Data, String Name)
        {
            this.Data = Data;
            this.Name = Name;
            this.Sent = true;
        }

        public DeliverEventArgs(Byte[] Data, bool Sent, String Time, bool TSSenderError, bool TSRefError)
        {
            this.Data = Data;
            this.Sent = Sent;
            this.Time = Time;
            this.TSSenderError = TSSenderError;
            this.TSRefError = TSRefError;
        }

    }

    public class SocketErrorEventArgs : EventArgs 
    {
        public String Text { set; get; }
        public String Code { set; get; }
        public SocketErrorEventArgs(String Text, String Code)
        {
            this.Text = Text;
            this.Code = Code;
        }
    }

    public class SocketParaErrorEventArgs : EventArgs 
    {
        public String Text { set; get; }
        public SocketParaErrorEventArgs(String Text)
        {
            this.Text = Text;
        }
    }

    public class UnknownErrorEventArgs : EventArgs
    {
        public String Text { set; get; }

        public UnknownErrorEventArgs(String Text)
        {
            this.Text = Text;
        }
    }

    public class ConnectedEventArgs : EventArgs
    {
        public bool Connected { set; get; }
        public ConnectedEventArgs(bool Connected)
        {
            this.Connected = Connected;
        }
    }

    public class FaultInjectionEventArgs : EventArgs
    {
        public enum InjectionType { ID, TSSenderReuse, TSSenderDec, TSRefDec, TSRefInc, CRC, BCC, Fragment}

        public InjectionType IT { set; get; }
        public int FragmentDelay { set; get; }

        public FaultInjectionEventArgs(InjectionType IT)
        {
            this.IT = IT;
        }
    }
}
