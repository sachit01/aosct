#pragma ident "@(#) Bombardier Transportation %full_filespec:  ACommunication.cs-3:ascii:arn_006#4 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          ACommunication.cs %
*
*  %version:       3 %
*
*  %created_by:    skothiya %
*
*  %date_created:  2017-05-11 12:55 %
*
*  DESCRIPTION: 
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2014-07-18    Hidaji      Added site ID, adjusted the position of message type 
*                           based on new radio protcol
* 2014-08-18    Bo H        Make TCCSim backward compatible with previous protocol (without SiteID)                          
* 2016-06-28    akushwah    Added Region ID
*******************************************************************************/


using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Net;
using System.Net.Sockets;
using System.Collections;
using System.ComponentModel;

namespace TCCtcp
{
    public abstract class ACommunication
    {
        protected float Time;
        protected float Timeout;
        protected Int16 TrainID;
        protected Byte SiteID;
        protected Byte RegionID;
        protected String ConnectData;
        public Byte[] LoopMessage;
        public String LoopName;
        protected Byte[] ProtocolMessage;
        protected String ProtocolName;
        public bool ProtocolVerified;
        protected BackgroundWorker Worker;
        protected int BufferLength = 1000 - 8 - 5;

        public List<DeliverEventArgs> SendL = new List<DeliverEventArgs>();
        //public List<Byte[]> SendL = new List<Byte[]>();
        public ManualResetEvent Pause = new ManualResetEvent(true);
        public List<int> FragmentList = new List<int>();

        public AutoResetEvent[] FaultInjection { protected set; get; }
    }

    // EventArgs used by TCCCom

    /// <summary>
    /// Object used to deliver messages
    /// </summary>
    public class DeliverEventArgs : EventArgs
    {
        public Byte[] Data { set; get; }
        public bool Sent { set; get; }
        public String Time { set; get; }
        public String Name { set; get; }
        public bool TSSenderError { set; get; }
        public bool TSRefError { set; get; }
        public int ID { set; get; }
        public int siteID { set; get; }
        public int regionID { set; get; }
        public bool CRCError { set; get; }
        public bool IDError { set; get; }
        public bool siteIDError { set; get; }
        public bool regionIDError { set; get; }
        public int MessageType { set; get; }
        public String FileName { set; get; }

        public DeliverEventArgs(Byte[] Data, String Name)
        {
            this.Data = Data;
            this.Name = Name;
            this.Sent = true;
            this.FileName = "";
        }

        public DeliverEventArgs(Byte[] Data, String Name, String FileName)
        {
            this.Data = Data;
            this.Name = Name;
            this.Sent = true;
            this.FileName = FileName;
        }

        public DeliverEventArgs(Byte ExpectedHeaderLength, Byte[] Data, bool Sent, String Time, bool TSSenderError, bool TSRefError)
        {
            this.Data = Data;
            if (Data.Length > ExpectedHeaderLength)
                this.MessageType = Data[ExpectedHeaderLength];
            this.Sent = Sent;
            this.Time = Time;
            this.TSSenderError = TSSenderError;
            this.TSRefError = TSRefError;
            this.FileName = "";
        }

    }

    /// <summary>
    /// Problem with the socket
    /// </summary>
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

    /// <summary>
    /// Problem with the socket parameters or timer values
    /// </summary>
    public class SocketParaErrorEventArgs : EventArgs 
    {
        public String Text { set; get; }
        public SocketParaErrorEventArgs(String Text)
        {
            this.Text = Text;
        }
    }

    /// <summary>
    /// Unknown error occurred
    /// </summary>
    public class UnknownErrorEventArgs : EventArgs
    {
        public String Text { set; get; }

        public UnknownErrorEventArgs(String Text)
        {
            this.Text = Text;
        }
    }

    /// <summary>
    /// Tells when a connection is established or when it's disconnected
    /// </summary>
    public class ConnectedEventArgs : EventArgs
    {
        public bool Connected { set; get; }
        public ConnectedEventArgs(bool Connected)
        {
            this.Connected = Connected;
        }
    }

    /// <summary>
    /// Used by the main thread to send event regarding fault injection
    /// Enumerator is used by Com thread
    /// </summary>
    public class FaultInjectionEventArgs : EventArgs
    {
        public enum InjectionType { ID, siteID, regionID, TSSenderReuse, TSSenderDec, TSRefDec, TSRefInc, CRC, Fragment}

        public InjectionType IT { set; get; }
        public int FragmentDelay { set; get; }

        public FaultInjectionEventArgs(InjectionType IT)
        {
            this.IT = IT;
        }
    }
}
