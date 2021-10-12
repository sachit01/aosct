using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using IronPython.Hosting;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;
using System.Reflection;
using System.Xml.Linq;
using System.Threading;
#if TCCSERIAL
using TCCserial;
#else
using TCCtcp;
#endif
using System.ComponentModel;

using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;
using System.IO;

namespace TCCSim
{
    public class Script
    {
        private String File;
        private Model Mdl;
        private AutoControl AutoCtrl;
        private BackgroundWorker ScriptWorker;
        public Thread thread;
        private ScriptStream s;
        
        ScriptEngine engine = Python.CreateEngine();
        ScriptSource source;
        ScriptScope scope;
        ScriptRuntime runtime;

        public AutoResetEvent MsgReceived = new AutoResetEvent(false);

        /// <summary>
        /// Sets up the IronPython environment
        /// </summary>
        /// <param name="File"></param>
        /// <param name="Mdl"></param>
        /// <param name="ScriptWorker"></param>
        public Script(String File, Model Mdl, BackgroundWorker ScriptWorker)
        {
            this.ScriptWorker = ScriptWorker;
            this.ScriptWorker.DoWork += new DoWorkEventHandler(Go);
            this.File = File;
            this.Mdl = Mdl;
            runtime = this.engine.Runtime;
            scope = this.engine.CreateScope();
            
            String path = typeof(Script).Assembly.Location;
            Assembly assembly = Assembly.LoadFile(path);
            runtime.LoadAssembly(assembly);

            path = typeof(Communication).Assembly.Location;
            assembly = Assembly.LoadFile(path);
            runtime.LoadAssembly(assembly);
            
            runtime.LoadAssembly(Assembly.GetExecutingAssembly());
            this.scope.SetVariable("engine", this);
            this.source = this.engine.CreateScriptSourceFromFile(this.File);
            //engine.SetSearchPaths(new string[] { @"C:\Python27\Lib" });

            AutoCtrl = new AutoControl();
            AutoCtrl.closeAutoControl();
        }


        /// <summary>
        /// Redirects output and starts the script
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="ev"></param>
        public void Go(Object sender, DoWorkEventArgs ev)
        {
            this.ScriptWorker.DoWork -= new DoWorkEventHandler(Go);
            this.thread = Thread.CurrentThread;

            MemoryStream ms = new MemoryStream();
            TextWriter tw = new ScriptStream(ms, this.ScriptWorker);
            this.runtime.IO.SetOutput(ms, tw);

            try
            {
                this.source.Execute(this.scope);
            }
            catch (MissingMemberException e)
            {
                Print("Error: Missing Member: " +  e.Message);
            }
            catch (SyntaxErrorException e)
            {
                Print("Syntax Error: Line: " + e.Line + " Column: " + e.Column + "\r\n\t" + e.Message);
            }
            catch (ThreadAbortException)
            {
                ;
            }
            catch (Exception e)
            {
                Print("Error: Unknown: " + e.Message);
            }
            finally
            {
                // If the thread is aborted it should reset that abort and return gracefully
                if ((int)Thread.CurrentThread.ThreadState == 132 // 132 = Background and AbortRequested
                    || Thread.CurrentThread.ThreadState == ThreadState.AbortRequested)
                    Thread.ResetAbort();
            }
        }
        #region SendMessage
        /// <summary>
        /// Send message from file
        /// </summary>
        /// <param name="File"></param>
        public void SendMessage(String File)
        {
            this.ScriptWorker.ReportProgress(0, new SendMessageEventArgs(File));
        }
        /// <summary>
        /// Send message from byte array
        /// </summary>
        /// <param name="bstr"></param>
        public void SendMessage(byte[] bstr)
        {
            ScriptWorker.ReportProgress(0, new SendBytesEventArgs(bstr));
        }
        /// <summary>
        /// Send message from XElement
        /// </summary>
        /// <param name="xm"></param>
        public void SendMessage(XElement xm)
        {
            ScriptWorker.ReportProgress(0, new SendXMLEventArgs(xm));
        }
        #endregion

        /// <summary>
        /// Load message into XElement from file path
        /// </summary>
        /// <param name="path"></param>
        /// <returns></returns>
        public XElement LoadMessage(string path)
        {
            return XElement.Load(path);
        }

        /// <summary>
        /// Sets n:th (index) occurance of a field type (fname) in a XElement describing a message.
        /// Returns true on success.
        /// </summary>
        /// <param name="xm"></param>
        /// <param name="fname"></param>
        /// <param name="index"></param>
        /// <param name="new_value"></param>
        /// <returns></returns>
        public bool SetF(XElement xm, string fname, int index, string new_value)
        {
          
            try
            {
                // Get all fields from message
                var query = from field in xm.Elements(fname)
                            select field;
                int i = 0;
                foreach (XElement elem in query)
                {
                    // Update wanted value.
                    if (i == index)
                    {
                        elem.Value = new_value;
                        return true; // Successful update.
                    }
                    i++;
                }
                return false;
            }
            catch (Exception) { return false; }
        }

        public bool SetF(string strg, string fname, int index, string new_value)
        {
            XElement xm = LoadMessage(strg);

            try
            {
                // Get all fields from message
                var query = from field in xm.Elements(fname)
                            select field;
                int i = 0;
                foreach (XElement elem in query)
                {
                    // Update wanted value.
                    if (i == index)
                    {
                        elem.Value = new_value;
                        return true; // Successful update.
                    }
                    i++;
                }
                return false;
            }
            catch (Exception) { return false; }
        }

        /// <summary>
        /// Sets Q Initiate state.
        /// </summary>
        /// <param name="new_value"></param>
        
        public void SetQInitiate(uint new_value)
        {
            Model.qInitiateState = new_value;
            Mdl.changeDefaultPRR();

        }

        public bool startAutoControl()
        {
            return AutoCtrl.Connect();
        }

        public void stopAutoControl()
        {
            AutoCtrl.closeAutoControl();
        }

        public void sendCommandToAutoControl(string command)
        {
            Byte[] data = Encoding.ASCII.GetBytes(command);
            AutoCtrl.sendBytes(data);
        }
        /// <summary>
        /// Returns true or false depending on existance of the requested field name.
        /// </summary>
        /// <param name="bstr"></param>
        /// <param name="fname"></param>
        /// <returns></returns>
        public bool IsFieldPresent(byte[] bstr, string fname)
    {
      XElement xm = Message.BytesToXML(bstr);
      try
      {
        return xm.Elements(fname).Any();
      }
      catch (Exception) { return false; }
    }

    /// <summary>
    /// Returns an ordered array of all values of  field type (fname).
    /// Else null is returned.
    /// </summary>
    /// <param name="bstr"></param>
    /// <param name="fname"></param>
    /// <returns></returns>
    public string[] GetFields(byte[] bstr, string fname)
        {
            XElement xm = Message.BytesToXML(bstr);
            try
            {
                var query = from fields in xm.Elements(fname)
                            select fields.Value;
                return query.ToArray<string>();
            }
            catch (Exception) { return null; }
        }

    public uint GetCurrentMode(byte[] bstr, string fname)
        {
            string[] currentMode = GetFields(bstr, fname);
            uint intCurrentMode = Convert.ToUInt16(currentMode[0]);
            return intCurrentMode;
        }
    
        public bool getAutoControlResponse()
        {
            return AutoCtrl.getResponse();
        }

        /// <summary>
        /// Returns the next item in the script delivery list
        /// </summary>
        /// <returns></returns>
        private EventArgs ListNext()
        {
            lock (Mdl.DeliverScriptList)
            {
                // If an item is present, return it
                if (Mdl.DeliverScriptList.Count > 0)
                {
                    EventArgs e = Mdl.DeliverScriptList[0];
                    Mdl.DeliverScriptList.RemoveAt(0);
                    return e;
                }
                this.MsgReceived.Reset();
            }
            // If the deliver list was empty,
            // wait for a signal that something has been added
            this.MsgReceived.WaitOne();
            lock (Mdl.DeliverScriptList)
            {
                // This time there should be something to return
                if (Mdl.DeliverScriptList.Count > 0)
                {
                    EventArgs e = Mdl.DeliverScriptList[0];
                    Mdl.DeliverScriptList.RemoveAt(0);
                    return e;
                }
            }
            return null;
        }

        /// <summary>
        /// Returns message type name from byte array
        /// </summary>
        /// <param name="bstr"></param>
        /// <returns></returns>
        public String GetMsgName(Byte[] bstr)
        {
            return Message.string_MTypeFromBytes(bstr);
        }

        /// <summary>
        /// Sends event to main thread to connect
        /// Waits for status report
        /// </summary>
        /// <param name="ID"></param>
        /// <param name="ConnectData"></param>
        /// <param name="Interval"></param>
        /// <param name="Timeout"></param>
        /// <returns></returns>
        public int Connect(int ID, int siteID, int regionID, String ConnectData, int Interval, int Timeout)
        {
            ConnectEventArgs C = new ConnectEventArgs(ID.ToString(), siteID.ToString(), regionID.ToString(), ConnectData, Interval.ToString(), Timeout.ToString());
            this.ScriptWorker.ReportProgress(0, C);
            while (true)
            {
                EventArgs e = ListNext();
                if (e is ConnectedEventArgs)
                    return 0;
                else if (e is SocketParaErrorEventArgs)
                    return -1;
                else if (e is SocketErrorEventArgs)
                    return -2;
            }
        }

        /// <summary>
        /// Returns the next message in the list.
        /// If the connection is broken null will be returned 
        /// </summary>
        /// <returns></returns>
        public DeliverEventArgs GetMessage()
        {
            while (true)
            {
                EventArgs e = ListNext();
                if (e is DeliverEventArgs)
                    return e as DeliverEventArgs;
                else if (e is SocketErrorEventArgs || e is SocketParaErrorEventArgs)
                    return null;
            }
        }

        /// <summary>
        ///  Disconnect current connection
        /// </summary>
        public void Disconnect()
        {
            ConnectEventArgs C = new ConnectEventArgs();
            this.ScriptWorker.ReportProgress(0, C);
        }

        /// <summary>
        /// Pause or unpause
        /// </summary>
        /// <param name="Pause"></param>
        public void Pause(bool Pause)
        {
            this.ScriptWorker.ReportProgress(0, new PauseEventArgs(!Pause));
        }

        /// <summary>
        /// Send event to main thread to print
        /// </summary>
        /// <param name="text"></param>
        private void Print(String text)
        {
            ScriptOutputEventArgs e = new ScriptOutputEventArgs(text);
            this.ScriptWorker.ReportProgress(0, e);
        }

        #region FaultInjection

        public void FaultyCRC()
        {
            this.ScriptWorker.ReportProgress(0, new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.CRC));
        }

        public void ReuseTimestamp()
        {
            this.ScriptWorker.ReportProgress(0, new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.TSSenderReuse));
        }

        public void DecTimestamp()
        {
            this.ScriptWorker.ReportProgress(0, new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.TSSenderDec));
        }

        public void IncRefTimestamp()
        {
            this.ScriptWorker.ReportProgress(0, new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.TSRefInc));
        }

        public void DecRefTimestamp()
        {
            this.ScriptWorker.ReportProgress(0, new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.TSRefDec));
        }

        public void FaultyID()
        {
            this.ScriptWorker.ReportProgress(0, new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.ID));
        }

        public void Fragment(int msDelay)
        {
            FaultInjectionEventArgs e = new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.Fragment);
            e.FragmentDelay = msDelay;
            this.ScriptWorker.ReportProgress(0, e);
        }

        #endregion
    }

    /// <summary>
    /// Class which is used by the script to redirect stdout
    /// </summary>
    public class ScriptStream : StreamWriter
    {
        private BackgroundWorker BGW;
        public ScriptStream(Stream s, BackgroundWorker BGW) : base(s)
        {
            this.BGW = BGW;
        }

        /// <summary>
        /// On write it send an event to the main thread to print the output
        /// </summary>
        /// <param name="value"></param>
        public override void Write(string value)
        {
            if(!value.StartsWith("\n") && !value.StartsWith("\r"))
                BGW.ReportProgress(0, new ScriptOutputEventArgs(value));
        }
    }
}
