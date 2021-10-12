using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net;
using System.Net.Sockets;
using System.Threading;
using System.ComponentModel;

namespace TCCCom
{
    public class Communication : ACommunication
    {

        private bool First = true;
        
        private IPAddress IP;
        private int Port;
        private Socket s;
        private IPEndPoint ep;
        private bool SendFromL;
        private bool TSSenderError = false;
        private bool TSRefError = false;
        private bool Paused = false;

        private String STrainID;
        private String STime;
        private String STimeout;
        
        public Communication(String Time, String ConnectData, String TrainID, String Timeout, Byte[] LoopMessage, String LoopName, BackgroundWorker Worker, int BufferLength)
        {
            this.Worker = Worker;
            this.Worker.DoWork += new DoWorkEventHandler(Run);
            this.LoopMessage = LoopMessage;
            this.LoopName = LoopName;
            this.STime = Time;
            this.STrainID = TrainID;
            this.STimeout = Timeout;
            this.ConnectData = ConnectData;
            if (BufferLength != -1)
                this.BufferLength = BufferLength - 8 - 5;

            // Init Fault Injection flags
            FaultInjection = new AutoResetEvent[7];
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.BCC] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.CRC] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.ID] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSRefDec] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSRefInc] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSSenderDec] = new AutoResetEvent(false);
            FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSSenderReuse] = new AutoResetEvent(false);

            Safety.Init();
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
            long LastMsgSent = 0, CurrentTime = 0;
            Byte[] Complete;
            while (!Worker.CancellationPending)
            {
                if (Pause.WaitOne(0))
                {
                    CurrentTime = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond;
                    Sleep = (int)(this.Time * 1000) - (int)(CurrentTime - LastMsgSent);
                    if (!First && Sleep > 0)
                    {
                        Thread.Sleep(Sleep);
                        if (Worker.CancellationPending)
                            break;
                        if (!Pause.WaitOne(0))
                            continue;
                    }
                    try
                    {
                        if (First || !this.s.Connected)
                        {
                            Connect();
                            First = false;
                        }

                        DeliverEventArgs SendObject = GetSendData();
                        bool BCCFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.BCC].WaitOne(0);
                        bool CRCFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.CRC].WaitOne(0);
                        bool TSRefDecFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSRefDec].WaitOne(0);
                        bool TSRefIncFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSRefInc].WaitOne(0);
                        bool TSSenderDecFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSSenderDec].WaitOne(0);
                        bool TSSenderReuse = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.TSSenderReuse].WaitOne(0);
                        bool IDFault = FaultInjection[(int)FaultInjectionEventArgs.InjectionType.ID].WaitOne(0);

                        Byte[] SendData = Safety.AppendSafetyHeader(SendObject.Data, this.TrainID, BCCFault, CRCFault, TSRefDecFault, TSRefIncFault, TSSenderDecFault, TSSenderReuse, IDFault);
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
                        SendObject.Data = SendData;
                        Deliver(SendObject, TSSenderReuse || TSSenderDecFault, TSRefIncFault || TSRefDecFault);

                        // Get respons to the previously sent message
                        Complete = GetMessage();

                        // Extract T_REF
                        Byte[] TSRef = Complete.Skip<Byte>(6).Take<Byte>(2).ToArray<Byte>();
                        if (BitConverter.IsLittleEndian)
                            Array.Reverse(TSRef);

                        // Check if Received T_REF is equal to last T_SENDER
                        if (TSRef[0] != Safety.LastTSSender[0] || TSRef[1] != Safety.LastTSSender[1])
                            TSRefError = true;
                        else
                            TSRefError = false;

                        // Extract T_SENDER
                        Byte[] TS = Complete.Skip<Byte>(4).Take<Byte>(2).ToArray<Byte>();
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
                        // Debug
                        //Console.WriteLine("SocketException:\n" + e.Message + "\n----" + e.SocketErrorCode.ToString() + "\n");
                        this.s.Close();
                        Thread.Sleep(3000);
                    }
                    catch (Exception e)
                    {
                        Worker.ReportProgress(0, new UnknownErrorEventArgs(e.Message));
                        // Debug
                        //Console.WriteLine("Exception:\n" + e.Message + "\n-----" + e.Source);
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

        private Byte[] GetMessage()
        {
            // Temp stores data that is needed
            // when the correct stx was not found
            Byte[] Temp = new Byte[0];
            Byte[] Header = new Byte[8];
            Byte[] Data, Complete;
            while (true)
            {
                int Received = 0;
                Temp = ReceiveSTX(Temp);

                // Get rest of header
                if (Temp.Length < 8)
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
                    Received = Temp.Length - 8;
                    Array.Copy(Temp, Header, 8);
                }

                // Get length of whole message
                int Length = GetDataLength(Header);

                // Check that the length not exceeds
                // maximum length of what's allowed
                if (Length > this.BufferLength)
                {
                    if (Temp.Length >= 8)
                        Temp = Temp.Skip<Byte>(1).ToArray<Byte>();
                    else
                        Temp = Header.Skip<Byte>(1).ToArray<Byte>();
                    continue;
                }

                Data = new Byte[Length + 5];
                // Copy already received data from Temp to 
                // the Data array
                if (Received > 0)
                {
                    Array.Copy(Temp, 8, Data, 0, Math.Min(Received, Data.Length));
                }

                // Fill Data array
                if (!ReceiveToBuffer(Data, Received))
                {
                    Temp = Header.Skip<Byte>(1).Concat<Byte>(Data).ToArray<Byte>();
                    continue;
                }

                // Append rest of message to header
                Complete = Header.Concat(Data).ToArray<Byte>();
                if (Safety.BCC_Calc(Complete, 0) == 0)
                {
                    return Complete;
                }
                else
                {
                    if (Temp.Length > Complete.Length)
                        Temp = Temp.Skip<Byte>(1).ToArray<Byte>();
                    else
                        Temp = Complete.Skip<Byte>(1).ToArray<Byte>();
                    Console.WriteLine("BCC Incorrect ");
                }
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
            SocketError se;
            int Xmit;
            while (Offset < Buffer.Length)
            {
                se = new SocketError();
                Xmit = this.s.Receive(Buffer, Offset, Buffer.Length - Offset, 0, out se);
                if (se != SocketError.Success)
                    return false;
                Offset += Xmit;
            }
            return true;
        }
            
        /// <summary>
        /// Searches after STX in the given array or when not found
        /// in the socket's incoming buffer.
        /// </summary>
        /// <param name="Temp"></param>
        /// <returns></returns>
        private Byte[] ReceiveSTX(Byte[] Temp)
        {
            int i = 0;
            while(i < Temp.Length)
            {
                if (Temp[i] == Safety.STX)
                    return Temp.Skip<Byte>(i).ToArray<Byte>();
                i++;
            }
            Byte[] stx = new Byte[1];
            //SocketError se;
            do
            {
                //se = new SocketError();
                int Xmit = this.s.Receive(stx, 1, 0);
                if (Xmit == 0)
                {
                    Console.WriteLine("Fel 4 Xmit: " + Xmit);
                    throw new SocketException((int)SocketError.TimedOut);
                }
            } while (stx[0] != Safety.STX);
            return stx;
        }

        /// <summary>
        /// Adds the appropriate number of 7E's and
        /// sends the message
        /// </summary>
        /// <param name="SendData"></param>
        /// <param name="Delay"></param>
        /// <returns></returns>
        private int SendBytes(Byte[] SendData, int Delay)
        {
            Byte[] SendData7E = new Byte[SendData.Length + 6];
            for (int i = 0; i < 5; i++)
            {
                SendData7E[i] = 0x7E;
            }
            SendData7E[SendData7E.Length - 1] = 0x7E;
            Array.Copy(SendData, 0, SendData7E, 5, SendData.Length);
            //Send without fragmentation
            if (Delay == 0)
            {
                return this.s.Send(SendData7E, SendData7E.Length, 0);
            }
            //Send with fragmentation and delay
            else
            {
                Random Ran = new Random();
                int Cut = Ran.Next(6, SendData7E.Length-1);
                int Xmit = this.s.Send(SendData7E, Cut, 0);
                Thread.Sleep(Delay);
                Xmit += this.s.Send(SendData7E, Cut, SendData7E.Length - Cut, 0);
                return Xmit;
            }
        }

        /// <summary>
        /// Validates input data such as IP, Port, Interval Timer
        /// </summary>
        /// <returns></returns>
        private bool validate()
        {
            try
            {
                //this.Time = Int32.Parse(this.STime);
                this.Time = float.Parse(this.STime);
            }
            catch
            {
                Worker.ReportProgress(0, new SocketParaErrorEventArgs("Polling Interval needs to be numeric"));
                return false;
            }

            try
            {
                this.TrainID = (Byte)Int32.Parse(this.STrainID);
            }
            catch
            {
                Worker.ReportProgress(0, new SocketParaErrorEventArgs("Train ID needs to be integer"));
                return false;
            }

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
            String[] Split = this.ConnectData.Split(':');
            if (Split.Length == 2)
            {
                IPAddress[] t;
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
            return true;
        }

        /// <summary>
        /// Delivers messages which has been received
        /// </summary>
        /// <param name="Data"></param>
        private void Deliver(Byte[] Data, bool TSSenderError, bool TSRefError)
        {
            String time = DateTime.Now.TimeOfDay.ToString();
            Worker.ReportProgress(0, new DeliverEventArgs(Data, false, time, TSSenderError, TSRefError));
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

        /// <summary>
        /// Returns the length of the message given in the safety layer header
        /// </summary>
        /// <param name="Header"></param>
        /// <returns></returns>
        private int GetDataLength(Byte[] Header)
        {
            Byte[] Length = Header.Skip<Byte>(2).Take<Byte>(2).ToArray<Byte>();
            if (BitConverter.IsLittleEndian)
                Array.Reverse(Length);
            return Convert.ToInt32(BitConverter.ToUInt16(Length, 0));
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
                    this.SendFromL = false;
                    return new DeliverEventArgs(this.LoopMessage, this.LoopName);
                }
                else
                {
                    this.SendFromL = true;
                    return this.SendL.First<DeliverEventArgs>();
                }
            }
        }

        /// <summary>
        /// Trys to establish a new connection
        /// </summary>
        private void Connect()
        {
            this.s = new Socket(this.IP.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
            this.s.Connect(ep);
            s.ReceiveTimeout = (int)(this.Timeout * 1000);
            Worker.ReportProgress(0, new ConnectedEventArgs(true));
        }
    }
}