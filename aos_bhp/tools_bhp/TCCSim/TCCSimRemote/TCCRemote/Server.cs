using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel;
using TCCSim;
using System.Net.Sockets;
using System.Threading;
using System.Collections;
using System.IO;

namespace TCCRemote
{
    class Server
    {
        public event MVEvent MVE;
        public delegate void MVEvent(EventArgs e);

        private Model Mdl;

        public BackgroundWorker Worker { private set; get; }
        public BackgroundWorker ScriptWorker { private set; get; }

        #region Socket stuffs
        private TcpListener _servers;
        private TcpClient _clients;
        private NetworkStream _ns;
        #endregion

        private bool Running = true;

        public Server(Model _M)
        {
            Mdl = _M;
            Mdl.ME += new Model.MEvent(Mdl_ME);

            // Init backgroundworker for Communication and Script
            this.Worker = new BackgroundWorker();
            Worker.WorkerReportsProgress = true;
            Worker.WorkerSupportsCancellation = true;
            this.ScriptWorker = new BackgroundWorker();
            ScriptWorker.WorkerSupportsCancellation = true;
            ScriptWorker.WorkerReportsProgress = true;
        }

        public void Run()
        {
            Console.WriteLine("Waiting for client");
            // Wait for connection.
            _servers = new TcpListener(50123);
            _clients = default(TcpClient);
            _servers.Start();
            _clients = _servers.AcceptTcpClient();
            _ns = _clients.GetStream();

            // Thread handling data from client.
            Thread readerT = new Thread(ReadTCP);
            readerT.Start();

            Console.WriteLine("lawl");
            while (Running)
            {
                string s = Console.ReadLine();
                byte[] b = Encoding.ASCII.GetBytes(s);

                _ns.Write(b, 0, b.Length);
            }
        }

        void ReadTCP()
        {
            while (Running)
            {
                byte[] buff = new byte[(int)_clients.ReceiveBufferSize];
                Console.WriteLine("TCP wait");
                int size = _ns.Read(buff, 0, (int)_clients.ReceiveBufferSize);
                if (size > 0)
                {
                    string instr = Encoding.ASCII.GetString(buff,0,size-1);
                    Console.WriteLine("From client: " + instr);
                    instr.TrimEnd((char)0x0a);
                    if (instr == "read")
                    {
                        MVE(new RunScriptEventArgs(@"d:\My Documents\Visual Studio 2008\Projects\TCCRemote\TCCRemote\Scripts\server_test.py"));
                    }
                    if (instr == "quit")
                    {
                        Running = false;
                        Environment.Exit(0);
                    }

                    // Cancelation of script.
                    if (instr == "cancel")
                    {
                        // End script.
                        MVE(new RunScriptEventArgs());
                        // Disconnect since it will not be handled by script.
                        MVE(new ConnectEventArgs());
                    }

                    // Prof of concept for uploading messages.
                    if (instr == "LOAD")
                    {
                        bool fetch = true;
                        StringBuilder sb = new StringBuilder();
                        while (fetch)
                        {
                            byte[] b = new byte[(int)_clients.ReceiveBufferSize];
                            int s = _ns.Read(b, 0, (int)_clients.ReceiveBufferSize);
                            string l = Encoding.ASCII.GetString(b, 0, s - 1);
                            if (l == "END_LOAD")
                                fetch = false;
                            else
                                sb.AppendLine(l);
                        }
                        using (StreamWriter outfile = new StreamWriter("tmp_script.py"))
                        {
                            outfile.Write(sb.ToString());
                            MVE(new RunScriptEventArgs("tmp_script.py"));
                        }
                    }
                }
            }
        }

        // Handle events from Model.
        private void Mdl_ME(EventArgs e)
        {
            if (e is ScriptOutputEventArgs)
            {
                ScriptOutputEventArgs es = (ScriptOutputEventArgs)e;
                byte[] b = Encoding.ASCII.GetBytes(es.Text);
                _ns.Write(b, 0, b.Length);
                Console.WriteLine(es.Text);
            }
        }

    }
}
