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
using TCCCom;
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
        private BackgroundWorker ScriptWorker;
        public Thread thread;
        private ScriptStream s;
        
        ScriptEngine engine = Python.CreateEngine();
        ScriptSource source;
        ScriptScope scope;
        ScriptRuntime runtime;

        public AutoResetEvent MsgReceived = new AutoResetEvent(false);

        public Script(String File, Model Mdl, BackgroundWorker ScriptWorker)
        {
            this.ScriptWorker = ScriptWorker;
            this.ScriptWorker.DoWork += new DoWorkEventHandler(Go);
            this.File = File;
            this.Mdl = Mdl;
            runtime = engine.Runtime;
            scope = engine.CreateScope();
            
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
        }

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
                if ((int)Thread.CurrentThread.ThreadState == 132 // 132 = Background and AbortRequested
                    || Thread.CurrentThread.ThreadState == ThreadState.AbortRequested)
                    Thread.ResetAbort();
            }
            
            
        }

        public void SendMessage(String File)
        {
            this.ScriptWorker.ReportProgress(0, new SendMessageEventArgs(File));
        }
        public void SendMessage(byte[] bstr)
        {
            ScriptWorker.ReportProgress(0, new SendBytes(bstr));
        }
        public void SendMessage(XElement xm)
        {
            ScriptWorker.ReportProgress(0, new SendXML(xm));
        }

        public XElement ReadMessage()
        {
            DeliverEventArgs msg = Mdl.DeliverList.Last();
            XElement lal = Message.BytesToXML(msg.Data);
            return lal;
        }

        public XElement LoadMessage(string path)
        {
            return XElement.Load(path);
        }

        // Sets n:th (index) occurance of a field type (fname) in a XElement describing a message.
        // Returns true on success.
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

        // Returns an ordered array of all values of  field type (fname).
        // Else null is returned.
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

        public EventArgs ListNext()
        {
            lock (Mdl.DeliverScriptList)
            {
                if (Mdl.DeliverScriptList.Count > 0)
                {
                    EventArgs e = Mdl.DeliverScriptList[0];
                    Mdl.DeliverScriptList.RemoveAt(0);
                    return e;
                }
                this.MsgReceived.Reset();
            }
            this.MsgReceived.WaitOne();
            lock (Mdl.DeliverScriptList)
            {
                if (Mdl.DeliverScriptList.Count > 0)
                {
                    EventArgs e = Mdl.DeliverScriptList[0];
                    Mdl.DeliverScriptList.RemoveAt(0);
                    return e;
                }
            }
            return null;
        }

        public String GetMsgName(Byte[] Data)
        {
            return Message.string_MTypeFromBytes(Data);
        }

        public int Connect(int ID, String ConnectData, int Interval, int Timeout)
        {
            ConnectEventArgs C = new ConnectEventArgs(ID.ToString(), ConnectData, Interval.ToString(), Timeout.ToString());
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

        public EventArgs GetMessage()
        {
            while (true)
            {
                EventArgs e = ListNext();
                if (e is DeliverEventArgs)
                    return e;
                else if (e is SocketErrorEventArgs || e is SocketParaErrorEventArgs)
                    return null;
            }
        }

        public void Disconnect()
        {
            ConnectEventArgs C = new ConnectEventArgs();
            this.ScriptWorker.ReportProgress(0, C);
        }

        public void Pause(bool Pause)
        {
            this.ScriptWorker.ReportProgress(0, new PauseEventArgs(!Pause));
        }

        public void Print(String text)
        {
            ScriptOutputEventArgs e = new ScriptOutputEventArgs(text);
            this.ScriptWorker.ReportProgress(0, e);
        }

        #region FaultInjection

        public void FaultyBCC()
        {
            this.ScriptWorker.ReportProgress(0, new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.BCC));
        }

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

    public class ScriptStream : StreamWriter
    {
        private BackgroundWorker BGW;
        public ScriptStream(Stream s, BackgroundWorker BGW) : base(s)
        {
            this.BGW = BGW;
        }

        public override void Write(string value)
        {
            if(!value.StartsWith("\n") && !value.StartsWith("\r"))
                BGW.ReportProgress(0, new ScriptOutputEventArgs(value));
        }
    }
}
