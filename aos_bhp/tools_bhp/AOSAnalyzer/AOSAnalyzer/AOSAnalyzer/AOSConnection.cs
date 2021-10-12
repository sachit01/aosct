/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          AOSConnection.cs %
*
*  %version:       5 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:43 %
*
*  DESCRIPTION:    Instance of an unit connection.
*                  Contains: Socket, ip and port to the connection.
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2011-07-26    Blomqvist   File created
*
*******************************************************************************/
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net.Sockets;
using System.Windows.Forms;

namespace AOSAnalyzer
{
    public class AOSConnection
    {
        public enum ProtocolHeader
        {
            Null,
            Measurebles,
            UnitData,
            Parameters
        }

        public event NewAOSDataEventHandler SendDataEvent;
        public event DisconnectedAOSEventHandler DisconnectedEvent;
        public event MeasuringStatusChanged measuringChanged;

        private TcpClient sock { get; set; }
        private NetworkStream aosstream { get; set; }
        private bool Running { get; set; }
        public bool Measuring { get; private set; }

        private string ip { get; set; }
        private int port { get; set; }

        private string graphSetupPath { get; set; }
        

        public AOSConnection(string _ip, int _port)
        {
            Measuring = false;
            ip = _ip;
            port = _port;
        }

        public AOSConnection(string _ip, int _port, string _path)
        {
            Measuring = false;
            ip = _ip;
            port = _port;
            graphSetupPath = _path;
        }

        /// <summary>
        /// Initiates a socketconnection, sets Running-flag to true and calls the Run()-method.
        /// When dying it raises the DisconnectedEvent and shows a messagebox.
        /// </summary>
        public void Connect()
        {
            sock = new TcpClient();
            try
            {
                sock.Connect(ip, port);
                Running = true;
                Run();
            }
            catch (System.Net.Sockets.SocketException e)
            {
                Console.WriteLine(e.StackTrace);
                DisconnectedEvent(this, EventArgs.Empty);
                MessageBox.Show("Could not connect!");
            }    
        }
        /// <summary>
        /// Closes the stream and the socket and sets the Running-flag to false;
        /// </summary>
        public void Disconnect()
        {
            aosstream.Close();
            sock.Close();
            Running = false;
        }

        // All packages that can be sent.
        #region Packages
        // Send a start-package with the default interval: 1000ms.
        public void Start()
        {
            Start(1000);
        }
        // Send a start-package with the given interval.
        public void Start(int interval)
        {
            string data = "start\n\r";//String.Format("{0} {1}{2}", "start", interval, "\n\r");
            Send(data);
            Measuring = true;
            if (measuringChanged != null) 
                measuringChanged(new AOSMeasuringStatusChangedEventArgs(Measuring));
        }
        // Send a stop-package to stop the measure
        public void Stop()
        {
            Send("stop\n\r");
            Measuring = false; 
            if (measuringChanged != null) 
                measuringChanged(new AOSMeasuringStatusChangedEventArgs(Measuring));
        }
        // Send a get parameters-package to recieve all packages.
        public void GetParameters()
        {
            Send("get parameters\n\r");
        }
        // Send a set parameter-package
        public void SetParameter(string parameter, string value)
        {
            string data = String.Format("{0} {1} {2}{3}", "set", parameter, value, "\n\r");
            Send(data);
        }

        #endregion

        /// <summary>
        /// Send the given string through the socket.
        /// </summary>
        /// <param name="data"></param>
        private void Send(string data)
        {
            byte[] buff = Encoding.ASCII.GetBytes(data);
            try
            {
                aosstream.Write(buff, 0, buff.Length);
            }
            catch
            {
                MessageBox.Show(String.Format("Failed to send {0}", data));
            }
        }

        /// <summary>
        /// Continuously read the socket and call the SendDataEvent when new data is recieved.
        /// </summary>
        private void Run()
        {
            string data;
            int length;
            byte[] buff = new byte[sock.ReceiveBufferSize];
            
            aosstream = sock.GetStream();
            try
            {
                while (sock.Connected && Running)
                {
                    length = aosstream.Read(buff, 0, buff.Length);
                    aosstream.Flush();
                    data = System.Text.Encoding.ASCII.GetString(buff, 0, length);
                    SendDataEvent(this, new AOSDataEventArgs(data));
                }
            }
            catch (Exception) 
            { 
                DisconnectedEvent(this, EventArgs.Empty);
            }      
        }
    }
}
