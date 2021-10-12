/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2014-07-18    Hidaji      Added site ID, adjusted the position of radio header elements 
*                           based on new radio protcol
* 2014-08-18    Bo H        Backwards compatible with previous protocol (without Site ID)                          
* 2016-06-28    akushwah    Added Region ID
*******************************************************************************/

using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net;
using System.Net.Sockets;
using System.Threading;
using System.ComponentModel;
using System.Reflection;

namespace TCCtcp
{
    public enum crcType
    {
        crcTypeRegion = 0,
        crcTypeCentral = 1
    };

    public class Communication : ACommunication
    {
        private IPAddress IP;
        private int Port;
        private Socket s;
        private IPEndPoint ep;
        private bool SendFromL;
        private bool TSSenderError = false;
        private bool TSRefError = false;
        private bool Paused = false;

        private String STrainID;
        private String SSiteID;
        private String SRegionID;
        private String STime;
        private String STimeout;
        private crcType crc;

        private const uint AreaRequestID = 19;
        private const uint RegistrationAreaID = 134;

        public Communication(String Time, String ConnectData, String TrainID, String SiteID, String RegionID, String Timeout, crcType crc, Byte[] LoopMessage, String LoopName, Byte[] ProtocolMessage, String ProtocolName, BackgroundWorker Worker, int BufferLength)
        {
        
            this.Worker = Worker;
            this.Worker.DoWork += new DoWorkEventHandler(Run);
            this.LoopMessage = LoopMessage;
            this.LoopName = LoopName;
            this.ProtocolMessage = ProtocolMessage;
            this.ProtocolName = ProtocolName;
            this.ProtocolVerified = false;
            this.STime = Time;
            this.STrainID = TrainID;
            this.SSiteID = SiteID;
            this.SRegionID = RegionID;
            this.STimeout = Timeout;
            this.crc = crc;
            this.ConnectData = ConnectData;

           // BufferLength contains the length of Buffer excluding Header Size (11 bytes) and Trailer Size (8 bytes)
            if (BufferLength != -1)
                this.BufferLength = BufferLength - Safety.expectedHeaderLength - 8;

            // Init Fault Injection flags
            FaultInjection = new AutoResetEvent[8];
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.CRC] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.ID] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.siteID] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.regionID] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSRefDec] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSRefInc] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSSenderDec] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSSenderReuse] = new AutoResetEvent(false);

        }
        public String DLLCaption()
        {

            return "TCCTcp.Dll v" + 
                Assembly.GetExecutingAssembly().GetName().Version.Major.ToString() + "." + 
                Assembly.GetExecutingAssembly().GetName().Version.Minor.ToString() + "." + 
                Assembly.GetExecutingAssembly().GetName().Version.Build.ToString();

           
        }

        /// <summary>
        /// Main loop of Communication.
        /// Sends and receives data and provides it to Model
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="ev"></param>
        private void Run(Object sender, DoWorkEventArgs ev)
        {
            // If Communcation is restarted, the delegate
            // won't be left but added again
            this.Worker.DoWork -= new DoWorkEventHandler(Run);
            if (!validate())
                return;
            
            int Xmit, Sleep = 0;
            //Timestampvalues so that the interval timer is correct
            long LastMsgSent = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond;// = 0, CurrentTime = 0;
            long CurrentTime = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond;
            Byte[] Complete;
            while (!Worker.CancellationPending)
            {
                if (Pause.WaitOne(0))
                {
                    // Calculate sleep time. Do not sleep if it's a negative value
                    CurrentTime = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond;
                    Sleep = (int)(this.Time * 1000) - (int)(CurrentTime - LastMsgSent);
                    if (!(this.s == null) && Sleep > 0)
                    {
                        Thread.Sleep(Sleep);
                        //Check if user have ordered cancellation or pause during sleep
                        if (Worker.CancellationPending)
                            break;
                        if (!Pause.WaitOne(0))
                            continue;
                    }
                    try
                    {
                        // Connect if not connected
                        if (this.s == null || !this.s.Connected)
                        {
                            Connect();
                        }

                        //////////////
                        // Send part of the loop
                        //////////////
                        
                        DeliverEventArgs SendObject = GetSendData();

                        if (SendObject == null)
                        {
                            // prevent the cpu from overheating
                            LastMsgSent = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond;
                            continue;
                        }

                        //Save all fault injection flags
                        bool CRCFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.CRC].WaitOne(0);
                        bool TSRefDecFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSRefDec].WaitOne(0);
                        bool TSRefIncFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSRefInc].WaitOne(0);
                        bool TSSenderDecFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSSenderDec].WaitOne(0);
                        bool TSSenderReuse = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSSenderReuse].WaitOne(0);
                        bool IDFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.ID].WaitOne(0);
                        bool siteIDFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.siteID].WaitOne(0);
                        bool regionIDFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.regionID].WaitOne(0);

                        // Add wrong id flag to SendObject to let mainview know that it has faulty id.
                        SendObject.IDError = IDFault;
                        SendObject.siteIDError = siteIDFault;
                        SendObject.regionIDError = regionIDFault;

                        Byte[] SendData = Safety.AppendSafetyHeader(SendObject.Data, this.TrainID, this.SiteID, this.RegionID, crc, CRCFault, TSRefDecFault, TSRefIncFault, TSSenderDecFault, TSSenderReuse, IDFault, siteIDFault, regionIDFault);

                        // Fragmentation faults are given through a list
                        int Delay = 0;
                        lock (this.FragmentList)
                        {
                            if (this.FragmentList.Count > 0)
                            {
                                Delay = this.FragmentList[0];
                                this.FragmentList.RemoveAt(0);
                            }
                        }

                        Xmit = SendBytes(SendData, Delay);
                        LastMsgSent = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond;
                        if (Xmit <= 0)
                        {
                            throw new SocketException((int)SocketError.TimedOut);
                        }

                        // Remove sent message from SendList
                        // if not default loop message was sent
                        if (this.SendFromL)
                        {
                            lock (this.SendL)
                            {
                                this.SendL.RemoveAt(0);
                            }
                        }

                        // Update sendobject with message including safety header
                        SendObject.Data = SendData;
                        // Deliver the sent message
                        Deliver(SendObject, TSSenderReuse || TSSenderDecFault, TSRefIncFault || TSRefDecFault);
                        
                        ///////////////
                        // Receive part of the loop
                        ///////////////

                        Complete = GetMessage();

                        // Extract T_REF
                        Byte[] TSRef = new Byte[2];
                        Array.Copy(Complete, Safety.expectedHeaderLength - 2, TSRef, 0, TSRef.Length);

                        if (BitConverter.IsLittleEndian)
                            Array.Reverse(TSRef);

                        // Check if received T_REF is equal to last T_SENDER
                        if (TSRef[0] != Safety.LastTSSender[0] || TSRef[1] != Safety.LastTSSender[1])
                            TSRefError = true;
                        else
                            TSRefError = false;

                        // Extract T_SENDER
                        Byte[] TS = new Byte[2];
                        Array.Copy(Complete, Safety.expectedHeaderLength - 4, TS, 0, TS.Length);

                        if (BitConverter.IsLittleEndian)
                            Array.Reverse(TS);

                        // Check that TS is higher that previous TS
                        // Ignore if it has been paused previously
                        uint TSuint = BitConverter.ToUInt16(TS, 0);
                        uint TSRefuint = BitConverter.ToUInt16(Safety.TSRef, 0);
                        
                        // If received TS is lower than previous and the difference 
                        // is higher than Max(polling interval, timeout)*2 it's not seen
                        // as a wrap around of the timestamp
                        if (!Paused && TSuint < TSRefuint && TSRefuint != 22117 &&
                            (TSuint - TSRefuint + 65535) > ((uint)(Math.Max(this.Time, this.Timeout) * 1000 * 2) >> 4))
                        {
                            TSSenderError = true;
                        }
                        else
                        {
                            TSSenderError = false;
                            Paused = false;
                        }

                        // Save TS to TSRef
                        Safety.TSRef = TS;

                        Deliver(Complete, TSSenderError, TSRefError);
                    }
                    catch (SocketException e)
                    {
                        Worker.ReportProgress(0, new SocketErrorEventArgs(e.Message, e.SocketErrorCode.ToString()));
                        this.s.Close();
                        Thread.Sleep(3000);
                    }
                    catch (Exception e)
                    {
                        Worker.ReportProgress(0, new UnknownErrorEventArgs(e.Message));
                        Thread.Sleep(3000);
                    }
                }
                else
                {
                    Paused = true;
                    Pause.WaitOne();
                }
            }

            // Thread is cancelled
            if (this.s != null)
                this.s.Close();

        }

        #region Receive
        /// <summary>
        /// Reads on socket until a complete message is found.
        /// Returns the byte array that is the message.
        /// </summary>
        /// <returns></returns>
        private Byte[] GetMessage()
        {
            // Temp stores data that is needed
            // when the correct STX was not found
            Byte[] Temp = new Byte[0];
            Byte[] Data, Complete;
            Byte[] Header = new Byte[Safety.expectedHeaderLength];

            // Wait for message until found or timeout
            while (true)
            {
                // Recceived counts the number of bytes received after safety header fields
                int Received = 0;
                // Search for STX
                Temp = ReceiveSTX(Temp);

                // Get rest of header
                if (Temp.Length < Safety.expectedHeaderLength)
                {
                    Array.Copy(Temp, Header, Temp.Length);
                    if (!ReceiveToBuffer(Header, Temp.Length))
                    {
                        Temp = Header.Skip<Byte>(1).ToArray<Byte>();
                        continue;
                    }
                }
                // or copy the header from temp
                else
                {
                    Received = Temp.Length - Safety.expectedHeaderLength;
                    Array.Copy(Temp, Header, Safety.expectedHeaderLength);
                }

                // Get length of payload
                int Length = GetDataLength(Header);

                // Check that the length not exceeds
                // maximum allowed length
                if (Length > this.BufferLength)
                {
                    // Continue looking for STX in Temp.
                    if (Temp.Length >= Safety.expectedHeaderLength)
                        Temp = Temp.Skip<Byte>(1).ToArray<Byte>();
                    else
                        Temp = Header.Skip<Byte>(1).ToArray<Byte>();
                    continue;
                }

                // Calculate the Message Chunks
                UInt16 messageChunks = (UInt16)(Length / Safety.maxLengthOfMessageChunks);

                if (Length % Safety.maxLengthOfMessageChunks != 0)
                {
                    messageChunks += 1;
                }

                // Allocate buffer for payload + crc
                Data = new Byte[Length + (messageChunks * 8)];

                // Copy already received data from Temp to 
                // the Data array
                if (Received > 0)
                {
                    Array.Copy(Temp, Safety.expectedHeaderLength, Data, 0, Math.Min(Received, Data.Length));
                }

                // Fill Data array
                if (!ReceiveToBuffer(Data, Received))
                {
                    // If the allocated buffer was not filled, wrong STX was found.
                    // Continue to look for it in Temp
                    Temp = Header.Skip<Byte>(1).Concat<Byte>(Data).ToArray<Byte>();
                    continue;
                }

                // Append rest of message to header 
                Complete = Header.Concat(Data).ToArray<Byte>();
                return Complete;
            }
        }

        /// <summary>
        /// Reads on socket until the given buffer is full.
        /// Starts writing to buffer at Offset
        /// </summary>
        /// <param name="Buffer"></param>
        /// <param name="Offset"></param>
        /// <returns></returns>
        private bool ReceiveToBuffer(Byte[] Buffer, int Offset)
        {
            if (Buffer == null || Offset < 0)
                return false;
            SocketError se;
            int Xmit = 0;
            while (Offset < Buffer.Length)
            {
                ArrayList listenList = new ArrayList();
                listenList.Add(this.s);
                Socket.Select(listenList, null, null, (int)(this.Timeout * 1000000));
                if (listenList.Count > 0)
                {
                    Xmit = this.s.Receive(Buffer, Offset, Buffer.Length - Offset, 0, out se);
                    if (Xmit == 0)
                        return false;
                }
                else
                {
                    return false;
                }
                Offset += Xmit;
            }
            return true;
        }
            
        /// <summary>
        /// Searches STX in the given array or when not found,
        /// in the socket's incoming buffer.
        /// </summary>
        /// <param name="Temp"></param>
        /// <returns></returns>
        private Byte[] ReceiveSTX(Byte[] Temp)
        {
            int i = 0;
            // Looks for STX in Temp buffer
            while(i < Temp.Length)
            {
                if (Temp[i] == Safety.STX)
                    return Temp.Skip<Byte>(i).ToArray<Byte>();
                i++;
            }

            // Looks for STX in sockets incoming buffer
            Byte[] stx = new Byte[1];
            do
            {
                int Xmit = this.s.Receive(stx, 1, 0);
                if (Xmit == 0)
                {
                    throw new SocketException((int)SocketError.TimedOut);
                }
            } while (stx[0] != Safety.STX);
            return stx;
        }

        /// <summary>
        /// Returns the length of the message given in the safety layer header
        /// </summary>
        /// <param name="Header"></param>
        /// <returns></returns>
        private int GetDataLength(Byte[] Header)
        {
            if (Header == null || Header.Length < Safety.expectedHeaderLength - 5)
                return 0;

            Byte[] Length = Header.Skip<Byte>(Safety.expectedHeaderLength - 6).Take<Byte>(2).ToArray<Byte>();

            if (BitConverter.IsLittleEndian)
                Array.Reverse(Length);
            return Convert.ToInt32(BitConverter.ToUInt16(Length, 0));
        }

        #endregion

        #region Send
        /// <summary>
        /// Adds the appropriate number of 7E's and
        /// sends the message
        /// </summary>
        /// <param name="SendData"></param>
        /// <param name="Delay"></param>
        /// <returns></returns>
        private int SendBytes(Byte[] SendData, int Delay)
        {
            //Send without fragmentation
            if (Delay == 0)
            {
                return this.s.Send(SendData, SendData.Length, 0);
            }
            //Send with fragmentation and delay
            else
            {
                Random Ran = new Random();
                int Cut = 0;
                int Xmit = this.s.Send(SendData, Cut, 0);
                Thread.Sleep(Delay);
                Xmit += this.s.Send(SendData, Cut, SendData.Length - Cut, 0);
                return Xmit;
            }
        }

        /// <summary>
        /// Returns data that should be sent.
        /// If there is data in the list it only peeks. Someone else has
        /// to remove the data once it has been sent.
        /// </summary>
        /// <returns></returns>
        private DeliverEventArgs GetSendData()
        {
            // This is fine as long as this is the only thread
            // doing dequeue.
            lock (this.SendL)
            {
                if (this.SendL.Count == 0)
                {
                    if (crc == crcType.crcTypeRegion)
                    {
                        //ProtocolVerified = true;
                        if (!ProtocolVerified)
                        {
                            this.SendFromL = false;
                            return new DeliverEventArgs(this.ProtocolMessage, this.ProtocolName);
                        }
                        else
                        {
                            this.SendFromL = false;
                            return new DeliverEventArgs(this.LoopMessage, this.LoopName);
                        }
                    }
                }
                else
                {
                    this.SendFromL = true;
                    return this.SendL.First<DeliverEventArgs>();
                }
            }
            return null;
        }

        #endregion

        #region Deliver
        /// <summary>
        /// Delivers messages which has been received
        /// </summary>
        /// <param name="Data"></param>
        private void Deliver(Byte[] Data, bool TSSenderError, bool TSRefError)
        {
            String time = DateTime.Now.TimeOfDay.ToString();
            Worker.ReportProgress(0, new DeliverEventArgs(Safety.expectedHeaderLength, Data, false, time, TSSenderError, TSRefError));
        }

        /// <summary>
        /// Delivers messages which have been sent
        /// </summary>
        /// <param name="Data"></param>
        private void Deliver(DeliverEventArgs DE, bool TSSenderError, bool TSRefError)
        {
            String time = DateTime.Now.TimeOfDay.ToString();
            DE.Time = time;
            DE.TSSenderError = TSSenderError;
            DE.TSRefError = TSRefError;
            Worker.ReportProgress(0, DE);
        }
        #endregion

        /// <summary>
        /// Trys to establish a new connection and sets timeout value
        /// </summary>
        private void Connect()
        {
            this.s = new Socket(this.IP.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
            this.s.Connect(ep);
            // Central TCC will only sent AreaRequest and RegistrationArea which will never change
            this.ProtocolVerified = (crc == crcType.crcTypeCentral);
            s.ReceiveTimeout = (int)(this.Timeout * 1000);
            Worker.ReportProgress(0, new ConnectedEventArgs(true));
            Safety.InitTSref();
        }

        /// <summary>
        /// Validates input data such as IP, Port, Interval Timer
        /// </summary>
        /// <returns></returns>
        private bool validate()
        {
            // Parse polling interval
            try
            {
                this.Time = float.Parse(this.STime);
            }
            catch
            {
                Worker.ReportProgress(0, new SocketParaErrorEventArgs("Polling Interval needs to be numeric"));
                return false;
            }

            // Parse train ID
            try
            {
                this.TrainID = (Int16)Int32.Parse(this.STrainID);
            }
            catch
            {
                Worker.ReportProgress(0, new SocketParaErrorEventArgs("Train ID needs to be integer"));
                return false;
            }

            // Parse site ID
            try
            {
                this.SiteID = (Byte)Int32.Parse(this.SSiteID);
            }
            catch
            {
                Worker.ReportProgress(0, new SocketParaErrorEventArgs("Site ID needs to be integer"));
                return false;
            }

            // Parse region ID
            try
            {
                this.RegionID = (Byte)Int32.Parse(this.SRegionID);
            }
            catch
            {
                Worker.ReportProgress(0, new SocketParaErrorEventArgs("Region ID needs to be integer"));
                return false;
            }

            // Parse the timeout value and check that it's nonnegative.
            try
            {
                this.Timeout = float.Parse(this.STimeout);
                if (this.Timeout < 0)
                    throw new Exception();
            }
            catch
            {
                Worker.ReportProgress(0, new SocketParaErrorEventArgs("Timeout needs to be nonnegative numeric value"));
                return false;
            }

            // Parse connection data
            String[] Split = this.ConnectData.Split(':');
            if (Split.Length == 2)
            {
                IPAddress[] t;
                // Resolve DNS address
                try
                {
                    t = Dns.GetHostAddresses(Split[0]);
                }
                catch (SocketException e)
                {
                    Worker.ReportProgress(0, new SocketParaErrorEventArgs(Split[0] + ": " + e.Message));
                    return false;
                }
                if (t.Length >= 1)
                {
                    this.IP = t[0];
                    // Parse Port number and create end point
                    try
                    {
                        this.Port = Int32.Parse(Split[1]);
                        this.ep = new IPEndPoint(this.IP, this.Port);
                    }
                    catch (Exception)
                    {
                        Worker.ReportProgress(0, new SocketParaErrorEventArgs("Port number needs to be integer in range 1-65535"));
                        return false;
                    }

                }
                else
                {
                    Worker.ReportProgress(0, new SocketParaErrorEventArgs("Could not resolve host address"));
                    return false;
                }

            }
            else
            {
                Worker.ReportProgress(0, new SocketParaErrorEventArgs("Connection data needs to be in form '<host>:<port>'"));
                return false;
            }

            try
            {
                Safety.Init();
            }
            catch (Exception)
            {
                Worker.ReportProgress(0, new UnknownErrorEventArgs("Unexpected error. Probably srprot.dll missing."));
                return false;
            }

            return true;
        }

    }
}