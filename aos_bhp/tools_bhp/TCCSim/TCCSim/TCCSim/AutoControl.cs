using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel;
using System.Net;
using System.Net.Sockets;
using System.Collections;
using System.Xml;
using System.Xml.Linq;

namespace TCCSim
{
    class AutoControl
    {
        private IPAddress IPAddress;
        private int Port;
        private IPEndPoint ep;
        private Socket s;
        private byte[] data;
        private Model Mdl;

        public AutoControl()
        {
            Mdl = new Model();
            try
            {
                XElement Conf = XElement.Load(Mdl.ConfigPath);
                IPAddress = IPAddress.Parse(Conf.Element("AOSRemoteInterfaceIP").Value);
                Port = int.Parse(Conf.Element("AOSRemoteInterfacePort").Value);
            }
            catch (Exception)
            {
                IPAddress = IPAddress.Parse("127.0.0.1");
                Port = 30198;
            }
            
            this.ep = new IPEndPoint(this.IPAddress, this.Port);
           // this.Connect();
            //data = Encoding.ASCII.GetBytes("start");
            //this.sendBytes(data);
        }

        public bool Connect()
        {
            bool ret = false;
            try
            {
                if (this.s == null || !this.s.Connected)
                {
                    this.s = new Socket(this.IPAddress.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
                    this.s.Connect(ep);
                    ret = true;
                }
            }
            catch
            {

            }
            return ret;
        }

        public void sendBytes(Byte[] SendData)
        {
            if (this.s != null && this.s.Connected)
            {
                int Xmit = this.s.Send(SendData, SendData.Length, 0);
            }
        }

        public void closeAutoControl()
        {
            if (this.s != null && this.s.Connected)
            {
                this.s.Close();
            }
        }

        public bool getResponse()
        {
            data = new Byte[100];
            int receivedByte = ReceiveToBuffer(data);
            if (0 < receivedByte )
            {
                string response = Encoding.UTF8.GetString(data, 0, receivedByte);
                //if (response.ToUpper() == "OK")
                //{
                    return true;
                //}
            }
            return false;
        }

        private int ReceiveToBuffer(Byte[] Buffer)
        {
            int noOfBytesRecvd = 0;
            if (this.s != null && this.s.Connected)
            {
                int Offset = 0;
                if (Buffer == null)
                {
                    return 0;
                }

                SocketError se;
                try
                {
                    ArrayList listenList = new ArrayList();
                    listenList.Add(this.s);
                    Socket.Select(listenList, null, null, (int)(1 * 1000000));
                    if (listenList.Count > 0)
                    {
                        noOfBytesRecvd = this.s.Receive(Buffer, Offset, Buffer.Length - Offset, 0, out se);
                    }
                }
                catch
                {
                }
                
            }
            
                return noOfBytesRecvd;
        }
    }
}
