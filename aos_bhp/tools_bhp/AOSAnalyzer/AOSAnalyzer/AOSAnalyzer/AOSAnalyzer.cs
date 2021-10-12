/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          AOSAnalyzer.cs %
*
*  %version:       2 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:43 %
*
*  DESCRIPTION:    The main form that contains all the collections and 
*                  child windows. 
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2012-07-26    Blomqvist   File created
* 2013-06-17    Blomqvist   Removed tag argument when adding a measure to PList,
*                           implemented save and load of Graph Setup and ability to 
*                           cut out a of a measurement.                   
*
*******************************************************************************/

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Threading;
using System.Windows.Forms;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Serialization;
using ZedGraph;

namespace AOSAnalyzer
{
    public partial class AOSAnalyzer : Form
    {
        event GraphdataChanged graphdataChanged; 
        private event AOSHeaderRead headerRead;
        event AOSGraphListChanged graphListChanged;

        private string Title;

        private Thread ConnectionThread { get; set; }
        private AOSSettings settings { get; set; }
        private AOSConnection aosconn { get; set; }

        AboutBox aboutBox;
        AddGraphForm agf;
        CommentForm commf;
        ConnectForm connf;
        CurveForm curvef;
        EditGraphForm egf;
        MarkerForm mf;
        MeasurementInfoForm mif;
        PropertiesForm pf;
        RemoveGraphForm rgf;
        SliceForm sf;
        UnitParameterForm param;
        UnitAboutForm about;

        private String MeasureComment { get; set; }
        private String MeasureDate { get; set; }

        private bool TypesParsed { get; set; }
        private bool ParametersParsed { get; set; }
        private int SweepSpann { get; set; }
        private int GraphHeight { get; set; }
        private bool IsConnected { get; set; }
        private bool IsTracing { get; set; }
        private UnitDescr Unit { get; set; }
        private SymbolType Symboltype { get; set; }
        private AOSMarker SelectedMarker { get; set; }

        private SortedDictionary<String, ParamDescr> parameters;
        private SortedDictionary<String, ParameterChange> parameterchanges;
        private List<TypeDescr> types;
        private List<PointPairList> PList;
        private List<AOSMarker> markers;
        private List<GraphPane> graphpanes;
        private List<LineItem> curves;
        private List<AOSGraph> graphs;
        
        private ZedGraphControl zgc; 
            
        // Load settings and call Init().
        public AOSAnalyzer(string[] args)
        {
            try
            {
                settings = AOSSettings.ReadConfig("Settings.xml");
            }
            catch (Exception)
            {
                // Failed to read settings file, use default.
                settings = new AOSSettings();
            }

            InitializeComponent();

            this.UpdateBounds(Properties.Settings.Default.Bounds.X,
                    Properties.Settings.Default.Bounds.Y,
                    Properties.Settings.Default.Bounds.Width,
                    Properties.Settings.Default.Bounds.Height);
            this.WindowState = Properties.Settings.Default.State;
            this.StartPosition = FormStartPosition.Manual;
            this.Location = Properties.Settings.Default.WindowLocation;
                

            Init();
            if (args.Length > 0)
            {
                String path = Path.GetFullPath(args[0]);
                OpenMeasurement(path);
            }
        }

        // Initialize collections and setting starting values.
        private void Init()
        {
            Title = this.Text;
            TypesParsed = false;
            ParametersParsed = false;

            parameters = new SortedDictionary<String, ParamDescr>();
            parameterchanges = new SortedDictionary<string, ParameterChange>();
            types = new List<TypeDescr>();
            Unit = new UnitDescr();
            markers = new List<AOSMarker>();
            curves = new List<LineItem>();
            graphs = new List<AOSGraph>();
            zgc = new ZedGraphControl();
           
            // Fix vertical scrollbar.
            int vertScrollWidth = SystemInformation.VerticalScrollBarWidth;
            graphContainer.Padding = new Padding(0, 0, vertScrollWidth, 0);
            
            IsConnected = false;

            GraphHeight = Properties.Settings.Default.graphHeight;
            SpannBox.Value = Properties.Settings.Default.DefaultTraceScale;
            // Setup the GraphController
            SetupZGC();

            graphpanes = zgc.MasterPane.PaneList;
            graphdataChanged += new GraphdataChanged(UpdateZGC);
            graphdataChanged += new GraphdataChanged(ScaleMarkers);

            SweepSpann = (int)SpannBox.Value;
            // Setup the LayoutPicker
            foreach (string style in Enum.GetNames(typeof(PaneLayout)))
            {
                ToolStripMenuItem t = new ToolStripMenuItem(style);
                t.Click += new EventHandler(Layoutchanged);
                layoutToolStripMenuItem.DropDownItems.Add(t);
            }
            Symboltype = SymbolType.None;
        }

        // Add eventlisteners, setup ContextMenu and layoutsettings.
        private void SetupZGC()
        {
            zgc.IsShowPointValues = pointValuesOnOffToolStripMenuItem.Checked;
            zgc.IsEnableWheelZoom = false;
            //zgc.MouseWheel += new MouseEventHandler(zgc_MouseWheel);
            zgc.MouseClick += ZedGraphController_MouseClick;
            zgc.SizeChanged += new EventHandler(ZedGraphSizeChanged);
            zgc.ZoomEvent += new ZedGraphControl.ZoomEventHandler(ZedGraphZoom);

            // Setup the size
            zgc.Width = graphContainer.Width;
            zgc.Anchor = AnchorStyles.Top | AnchorStyles.Left | AnchorStyles.Right;

            // Add custom menuitems to the rightclick-menu
            zgc.ContextMenuBuilder += new ZedGraphControl.ContextMenuBuilderEventHandler(AddContextMenuItems);
            
            zgc.IsSynchronizeXAxes = true;
            zgc.MasterPane.SetLayout(zgc.CreateGraphics(), PaneLayout.SingleColumn);
            zgc.Dock = DockStyle.Top;
        }

        // Add Scatter, Zoom all X and Syncronize axes to the rightclick menu.
        private void AddContextMenuItems
            (ZedGraphControl control
            ,ContextMenuStrip menuStrip
            ,Point mousePt
            ,ZedGraphControl.ContextMenuObjectState objState){
            // Add menuitem to toggle the viewing of samplepoints
            ToolStripMenuItem t = new ToolStripMenuItem();
            t.Name = "scatter";
            t.Tag = "scatter";
            t.Text = "Scatter";
            t.CheckOnClick = true;
            t.CheckedChanged += new EventHandler(rightClickScatterItem_CheckedChanged);
            t.Checked = scatterToolStripMenuItem.Checked;
            menuStrip.Items.Add(t);

            // Add menuitem to fit the entire graph on the x-axis
            t = new ToolStripMenuItem();
            t.Name = "zoom_all_x";
            t.Tag = "zoom_all_x";
            t.Text = "Zoom all X";
            t.Click += new EventHandler(zoomAllX);
            menuStrip.Items.Add(t);
        }

        // Opens a measurement into AOSAnalyzer
        private void OpenMeasurement(String path)
        {
            ClearAllData();

            XElement xdata;
            try
            {
                xdata = XElement.Load(path);
            }
            catch (Exception)
            {
                MessageBox.Show("Could not load file: " + path);
                return;
            }

            // Read comment and date
            try
            {
                MeasureComment = xdata.Element("MeasureComment").Value;
                MeasureDate = xdata.Element("MeasureDate").Value;
            }
            catch
            {
                MessageBox.Show("Error parsing the Comment or date for the measure, please check the file for errors");
            }

            // Deserialize Unit
            try
            {
                XmlSerializer deserializer = new XmlSerializer(typeof(UnitDescr));
                Unit = (UnitDescr)deserializer.Deserialize(xdata.Element("UnitDescr").CreateReader());
            }
            catch
            {
                MessageBox.Show("Error parsing the Unit description, please check the file for errors");
                Init();
                return;
            }

            // Deserialize Types
            try
            {
                XmlSerializer deserializer = new XmlSerializer(typeof(List<TypeDescr>));
                types = (List<TypeDescr>)deserializer.Deserialize(xdata.Element("ArrayOfTypeDescr").CreateReader());
            }
            catch
            {
                MessageBox.Show("Error parsing the type descriptions, please check the file for errors");
                Init();
                return;
            }

            // Types parsed, create PList based on types.
            PList = new List<PointPairList>();
            for (int i = 0; i < types.Count; i++)
            {
                PList.Add(new PointPairList());
            }

            // Deserialize Values
            try
            {
                XmlReader reader = xdata.Element("AOSMeasurements").CreateReader();
                String temp = xdata.Element("AOSMeasurements").Value;
                String[] values;
                char[] separator = new char[] { ';' };

                // Read and parse the values
                while (reader.Read())
                {
                    String line = reader.Value;
                    if (line != "")
                    {
                        values = line.Split(separator);
                        double x = Double.Parse(values[0]);
                        for (int i = 1; i < values.Length; i++)
                        {
                            double y = Double.Parse(values[i]);
                            PList[i - 1].Add(new PointPair(x, y));
                        }
                    }
                }
            }
            catch
            {
                MessageBox.Show("Error parsing the values, please check the file for errors");
                Init();
                return;
            }

            CreateCurves(types, PList);

            // Deserialize Graphs
            try
            {
                XmlSerializer deserializer = new XmlSerializer(typeof(List<AOSGraph>));
                graphs = (List<AOSGraph>)deserializer.Deserialize(xdata.Element("ArrayOfAOSGraph").CreateReader());

                foreach (AOSGraph g in graphs)
                {
                    g.UpdateCurves(curves);
                    AddGraph(g);
                }
            }
            catch
            {
                MessageBox.Show("Error parsing the graphs, please check the file for errors");
                Init();
                return;
            }

            SuspendLayout();
            ResumeLayout();
            zoomAllX(this, EventArgs.Empty);
        }

        // Methods for Connection.
        #region Connection
        //Setup and start the socketthread
        private void Connect(string ip, int port)
        {
            ClearAllData();

            aosconn = new AOSConnection(ip, port);
            ConnectionThread = new Thread(new ThreadStart(aosconn.Connect));

            TypesParsed = false;
            ParametersParsed = false;

            // Set up event listening of new data from AOS.
            aosconn.SendDataEvent += new NewAOSDataEventHandler(NewData);
            aosconn.DisconnectedEvent += new DisconnectedAOSEventHandler(Disconnected);
            headerRead += new AOSHeaderRead(Connected);

            ConnectionThread.Start();
            connectToolStripMenuItem.Text = "Disconnect";
            connectedLabel.Text = "Connecting...";

            ConnectTool.Enabled = false;
            ConnectTool.Visible = false;
            DisconnectTool.Enabled = true;
            DisconnectTool.Visible = true;
        }

        // Stops measuring and closes the socketthread
        private void Disconnect()
        {            
            // Tell aos connection to stop measuring.
            StopMeasure();
            // Tell aos connection to disconnect.
            aosconn.Disconnect();
            // Stop aos connection thread
            ConnectionThread.Abort();
            ConnectionThread = null;

            ConnectTool.Visible = true;
            DisconnectTool.Visible = false;
        }

        // Clear eventual old measures and send a start-package
        private void StartMeasure()
        {
            RestoreZoomEvents();
            for (int i = 0; i < PList.Count; i++)
            {
                PList[i].Clear();
            }
            aosconn.Start();
            MeasureDate = System.DateTime.Now.ToString();
            startToolStripMenuItem.Text = "Stop Measure";
        }

        // Clear eventual old measures and send a start-package with given interval
        private void StartMeasure(int interval)
        {
            RestoreZoomEvents();
            for (int i = 0; i < PList.Count; i++)
            {
                PList[i].Clear();
            }
            aosconn.Start(interval);
            startToolStripMenuItem.Text = "Stop Measure";
        }

        // Sends a stop-package
        private void StopMeasure()
        {
            aosconn.Stop();

            StartMeasureTool.Enabled = true;
            StartMeasureTool.Visible = true;
            StopMeasureTool.Enabled = false;
            StopMeasureTool.Visible = false;

            traceBox.Checked = false;
            startToolStripMenuItem.Text = "Start Measure";
        }
        
        #endregion

        // Methods for Graph handling.
        #region Graphs
        // Clear graphs, values and types
        private void ClearAllData()
        {
            if (graphpanes != null)
            {
                graphpanes.Clear();
            }
            if (PList != null)
            {
                PList.Clear();
            }
            if (types != null)
            {
                types.Clear();
            }
            if (parameters != null)
            {
                parameters.Clear();
            }
            if (parameterchanges != null)
            {
                parameterchanges.Clear();
            }
            if (curves != null)
            {
                curves.Clear();
            }
            if (markers != null)
            {
                markers.Clear();
            }
            Unit = new UnitDescr();

            zgc.Height = 0;
            graphContainer.AutoScroll = false;
            graphContainer.AutoScroll = true;
            OnGraphdataChanged();
        }
        // Clear graphs
        private void ClearGraphs()
        {
            if (graphpanes != null)
            {
                graphpanes.Clear();
            }
            graphContainer.AutoScroll = false;
            graphContainer.AutoScroll = true;
            OnGraphdataChanged();
        }
        
        // Redraw the graphs
        private void UpdateZGC()
        {
            foreach (GraphPane gp in zgc.MasterPane.PaneList)
            {
                gp.AxisChange();
            }
            zgc.Invalidate();
        }

        // Redraw the graphs
        private void ZedGraphSizeChanged(object sender, EventArgs e)
        {
            OnGraphdataChanged();
        }
        // Redraw the graphs
        private void ZedGraphZoom(object sender, ZoomState oldState, ZoomState newState)
        {
            OnGraphdataChanged();
        }

        // Setup the graph and add it to the ZedGraphControl
        private void AddGraph(object sender, EventArgs e)
        {
            if (graphContainer.Controls.Count == 0)
            {
                graphContainer.Controls.Add(zgc);
                graphContainer.Invalidate();
            }
            AOSGraph graph = ((AOSVisGraphEventArgs)e).graph;
            if (!graphs.Contains(graph))
            {
                graphs.Add(graph);
            }
            GraphPane gp = graph.GetGraphPane();

            //Setup visual settings
            gp.Title.IsVisible = titleOnOffToolStripMenuItem.Checked;
            gp.Legend.IsVisible = legendOnOffToolStripMenuItem.Checked;
            gp.XAxis.Scale.Max = SweepSpann;
            gp.XAxis.Title.Text = "Seconds";

            // Add to graphlist
            zgc.MasterPane.Add(gp);
            // Add the graph height to the graph controller.
            zgc.Height += GraphHeight;

            // Add markerlisteners to the new graph
            if (mf != null && !mf.IsDisposed)
            {
                mf.markerChanged += new AOSMarkerChanged(graph.OnMarkerChanged);
                mf.markerListChanged += new AOSMarkerListChanged(graph.OnMarkerListChanged);
            }
            // Add the already existing markers
            foreach(AOSMarker marker in markers)
            {
                graph.AddMarker(marker);
            }

            // Notify that a new graph is added.
            OnGraphdataChanged();
            OnGraphListChange(sender);
        }

        // Add the graph to the ZedGraphControl 
        private void AddGraph(AOSGraph graph)
        {
            if (graphContainer.Controls.Count == 0)
            {
                graphContainer.Controls.Add(zgc);
                graphContainer.Invalidate();
            }
            if (!graphs.Contains(graph))
            {
                graphs.Add(graph);
            }

            // Add all markers to the new graph.
            foreach (AOSMarker m in markers)
            {
                graph.AddMarker(m);
            }

            GraphPane gp = graph.GetGraphPane();      

            //Setup visual settings
            gp.Title.IsVisible = titleOnOffToolStripMenuItem.Checked;
            gp.Legend.IsVisible = legendOnOffToolStripMenuItem.Checked;
            gp.XAxis.Title.Text = "Seconds";

            // Add to graphlist
            zgc.MasterPane.Add(gp);
            // Add the graph height to the graph controller.
            zgc.Height += GraphHeight;

            // Add markerlisteners to the new graph
            if (mf != null && !mf.IsDisposed)
            {
                mf.markerChanged += new AOSMarkerChanged(graph.OnMarkerChanged);
                mf.markerListChanged += new AOSMarkerListChanged(graph.OnMarkerListChanged);
            }

            // Notify that a new graph is added.
            OnGraphdataChanged();
            OnGraphListChange(this);
        }

        // Remove from graphlist and force a redraw
        private void RemoveGraph(object sender, EventArgs e)
        {
            AOSGraph graph = ((AOSVisGraphEventArgs)e).graph;
            graphs.Remove(graph);
            graphpanes.Remove(graph.GetGraphPane());
            // Subtract the graph height to the graph controller.
            zgc.Height -= GraphHeight;
            OnGraphdataChanged();
            OnGraphListChange(sender);
        }
       
        // Create one curve for each type
        private void CreateCurves(List<TypeDescr> _t, List<PointPairList> _p)
        {
            curves.Clear();
            PList = _p;
            for (int i = 0; i < _t.Count; i++)
            {
                LineItem c = new LineItem(_t[i].Name
                    , _p[i]
                    , AOSSettings.ColorArray[i % AOSSettings.ColorArray.Length]
                    , Symboltype);
                c.IsSelectable = true;
                if (_t[i].Type == "STATE")
                {
                    c.Line.Style = System.Drawing.Drawing2D.DashStyle.Dash;
                    c.Line.StepType = StepType.ForwardStep;
                    c.IsY2Axis = true;
                }
                c.Line.IsAntiAlias = true; // Make it pretty ;)
                curves.Add(c);
            }
        }
        
        // Changes the scales of the graph and forces redraw
        private void UpdateGraphs()
        {
            try
            {
                int length = PList[0].Count;
                double xValue = PList[0][length - 1].X;
                double start = xValue - SweepSpann > 0 ? xValue - SweepSpann : 0;
                for (int i = 0; i < graphpanes.Count; i++) // graphs should only use integers from 0 up to Count for values.
                {
                    if (IsTracing)
                    {
                        GraphPane gp = graphpanes[i];
                        // Only trace if no zoom is done.
                        if (gp.ZoomStack.Count == 0)
                        {
                            gp.XAxis.Scale.Min = start;
                            gp.XAxis.Scale.Max = start + SweepSpann;
                            ScaleY(gp);
                        }
                    }
                }
                OnGraphdataChanged();
            }
            catch { }
        }

        // Undos all zoom that has been made on the graphs.
        private void RestoreZoomEvents()
        {
            foreach (AOSGraph graph in graphs)
            {
                graph.RestoreZoomEvents();
                ScaleY(graph.GetGraphPane());
            }
            OnGraphdataChanged();
        }
        // Sets scale of Y axis in gp to fit current values.
        private void ScaleY(GraphPane gp)
        {
            gp.YAxis.Scale.MajorStepAuto = true;
            gp.YAxis.Scale.MinorStepAuto = true;
            gp.YAxis.Scale.MagAuto = true;
            gp.YAxis.Scale.FormatAuto = true;
            gp.YAxis.CrossAuto = true;
            gp.AxisChange();
        }
        // Sets scale of the markers to fit the biggest Y-axis.
        private void ScaleMarkers()
        {
            int MaxValueY = 0;
            int MinValueY = 0;

            foreach (GraphPane gp in graphpanes)
            {
                // Get the height for the markers.
                MaxValueY = (int)Math.Max(gp.YAxis.Scale.Max, MaxValueY);

                // Get the Y-location of the markers.
                MinValueY = (int)Math.Min(gp.YAxis.Scale.Min, MinValueY);
            }
            foreach (AOSMarker marker in markers)
            {
                marker.Location.Height = MaxValueY - MinValueY;
                marker.Location.Y = MinValueY;
            }
        }

        // Removes all measurements that is outside the interval and 
        // transposes the remaining measurements to origo.
        public void SliceMeasure(double start, double end)
        {
            PointPairList list = PList[0];
            int startindex = 0;
            int endindex = list.Count - 1;
            int count = list.Count;
            for (int i = 0; i < list.Count; i++)
            {
                if (list[i].X < start)
                    startindex = i;
                if (list[count - 1 - i].X > end)
                    endindex = count - 1 - i;
            }
            for (int i = 0; i < PList.Count; i++)
            {
                PList[i].RemoveRange(endindex, count - endindex);
                PList[i].RemoveRange(0, startindex);

                double offset = PList[i][0].X;

                for (int j = 0; j < PList[i].Count; j++)
                {
                    PList[i][j].X -= offset;
                }
            }
            OnGraphdataChanged();
            zoomAllXToolStripMenuItem.PerformClick();
        }

        // Loads a graph setup from the given path.
        private void LoadGraphSetup(string path)
        {
            XElement xdata;
            try
            {
                xdata = XElement.Load(path);
            }
            catch (Exception)
            {
                MessageBox.Show("Could not load file: " + path);
                return;
            }
            ClearGraphs();
            zgc.Height = 0;
            //Deserialize graph setup
            try
            {
                XmlSerializer deserializer = new XmlSerializer(typeof(List<AOSGraph>));
                graphs = (List<AOSGraph>)deserializer.Deserialize(xdata.Element("ArrayOfAOSGraph").CreateReader());
            }
            catch (Exception)
            {
                MessageBox.Show("Error parsing the graph setup, please check the file for errors");
                return;
            }
            
            graphSetupStatusText.Text = "Current Graph Setup: " + Path.GetFileNameWithoutExtension(path);
        }
        #endregion

        // Methods for Markers.
        #region Markers
        // Adds marker to all graphs
        private void AddMarker(String name, double position)
        {
            AOSMarker m = new AOSMarker(name, position);
            if (mf != null)
            {
                mf.AddMarker(m);
            }
            else
            {
                markers.Add(m);
                foreach (AOSGraph g in graphs)
                {
                    g.AddMarker(m);
                }
                OnAOSMarkerListChanged(this, new AOSMarkerListEventArgs(markers));
            }
        }

        // Removes marker from all graphs
        private void RemoveMarker(AOSMarker marker)
        {
            markers.Remove(marker);
            foreach (AOSGraph g in graphs)
            {
                g.RemoveMarker(marker);
            }
        }

        // Change the x-value for the marker
        public void MoveMarker(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                
            }
        }
        #endregion

        // Methods for the communication.
        #region Communication
        // An headerenum to know what information the unit sends.
        private AOSConnection.ProtocolHeader currentHeader = 
            AOSConnection.ProtocolHeader.Null;

        // Adds a measure to the PointPairLists.
        private void AddMeasure(string line)
        {
            string[] values = line.Split(new char[] { ';' }, StringSplitOptions.RemoveEmptyEntries);
            double x;
            double y;
            // Make sure x value is parsed ok.
            if (!double.TryParse(values[0], out x))
            {
                MessageBox.Show("Bad formated string from AOS: " + line);
                return;
            }
            x = x / 1000;
            // Parse and add each y value.
            for (int i = 0; i < values.Length - 1; ++i)
            {
                double.TryParse(values[i + 1], out y);
                PList[i].Add(x, y);
            } 
            UpdateGraphs();
        }
       
        // Handles new incomming data from AOS.
        private void NewData(object sender, EventArgs e)
        {
            string[] data = ((AOSDataEventArgs)e).Data.Split(new string[] {"\r\n"}, StringSplitOptions.RemoveEmptyEntries);

            foreach (string line in data)
            {
                // "Unknown command:" is recieved when sending wrong data to the unit.
                if (line.StartsWith("Unknown command:"))
                {
                    continue;
                }
                // When measuring no other data can be recieved.
                if (!aosconn.Measuring)
                {
                    switch (line)
                    {
                        case "[MeasurablesStart]":
                            currentHeader = AOSConnection.ProtocolHeader.Measurebles;
                            TypesParsed = false;
                            break;
                        case "[MeasurablesEnd]":
                            List<PointPairList> ppl = new List<PointPairList>();

                            // Initiate list
                            for (int i = 0; i < types.Count; ++i)
                            {
                                ppl.Add(new PointPairList());
                            }
                            CreateCurves(types, ppl);

                            currentHeader = AOSConnection.ProtocolHeader.Null;
                            TypesParsed = true;
                            break;

                        case "[UnitDataStart]":
                            currentHeader = AOSConnection.ProtocolHeader.UnitData;
                            break;
                        case "[UnitDataEnd]":
                            currentHeader = AOSConnection.ProtocolHeader.Null;
                            break;

                        case "[ParametersStart]":
                            currentHeader = AOSConnection.ProtocolHeader.Parameters;
                            break;
                        case "[ParametersEnd]":
                            currentHeader = AOSConnection.ProtocolHeader.Null;
                            if (!ParametersParsed)
                            {
                                ParametersParsed = true;
                                headerRead(this, new EventArgs());
                            }
                            break;
                        // Parse the data based on the last header recieved.
                        default:
                            if (currentHeader ==
                                AOSConnection.ProtocolHeader.Measurebles)
                            {
                                TypeDescr t = new TypeDescr(line);
                                if (!types.Contains(t))
                                {
                                    types.Add(t);
                                }
                            }
                            else if (currentHeader ==
                                AOSConnection.ProtocolHeader.UnitData)
                            {
                                Unit.ParseString(line);
                            }
                            else if (currentHeader ==
                                AOSConnection.ProtocolHeader.Parameters)
                            {
                                if (ParametersParsed)
                                {
                                    ParamDescr _p = new ParamDescr(line);
                                    parameters[_p.Name] = _p;
                                }
                                else
                                {
                                    ParamDescr _p = new ParamDescr(line);
                                    parameters.Add(_p.Name, _p);
                                }
                            }
                            break; //End default:
                    }//End switch
                }
                else //Measuring
                {
                    AddMeasure(line);
                }
            }//End foreach
            return;
        }
        #endregion

        #region Eventhandlers
        // Eventhandlers for Connection events.
        #region Connection
        // Handle the disconnected event from AOS.
        private void Disconnected(object sender, EventArgs e)
        {
            if (this.InvokeRequired)
            {
                try
                {
                    DisconnectCallback d = new DisconnectCallback(Disconnected);
                    Invoke(d, new object[] { sender, e });
                } catch { }
            }
            else
            {
                CloseAllSubForms();

                // Dereigester events
                aosconn.SendDataEvent -= new NewAOSDataEventHandler(NewData);
                aosconn.DisconnectedEvent -= new DisconnectedAOSEventHandler(Disconnected);
                headerRead -= new AOSHeaderRead(Connected);

                // Tell aos connection to disconnect.
                aosconn = null;

                startToolStripMenuItem.Text = "Start Measure";
                // Change name of connect/disconnect button.
                connectToolStripMenuItem.Text = "Connect...";
                ConnectTool.Enabled = true;
                ConnectTool.Visible = true;
                DisconnectTool.Enabled = false;
                DisconnectTool.Visible = false;

                StartMeasureTool.Enabled = false;
                StartMeasureTool.Visible = true;
                StopMeasureTool.Enabled = false;
                StopMeasureTool.Visible = false;

                startToolStripMenuItem.Enabled = false;
                parametersToolStripMenuItem.Enabled = false;
                StartMeasureTool.Enabled = false;
                ParameterTool.Enabled = false;
                // Next time we connect we need to parse types again.
                TypesParsed = false;
                IsConnected = false;
                this.Text = Title;
                connectedLabel.Text = "Disconnected";
                return;
            }
        }

        private void CloseAllSubForms()
        {
            if(agf != null)
                agf.Dispose();
            if (commf != null)
                commf.Dispose();
            if (connf != null)
                connf.Dispose();
            if (curvef != null)
                curvef.Dispose();
            if (egf != null)
                egf.Dispose();
            if (mf != null)
                mf.Dispose();
            if (rgf != null)
                rgf.Dispose();
            if (param != null)
                param.Dispose();
            if (about != null)
                about.Dispose();
        }

        // Setup gui and imported graphs on connect
        private void Connected(object sender, EventArgs e)
        {
            if (this.InvokeRequired)
            {
                AOSHeaderRead d = new AOSHeaderRead(Connected);
                Invoke(d, new object[] { sender, e });
            }
            else
            {
                IsConnected = true;

                StartMeasureTool.Enabled = true;
                ParameterTool.Enabled = true;
                
                startToolStripMenuItem.Enabled = true;
                parametersToolStripMenuItem.Enabled = true;

                connectedLabel.Text = "Connected";
                this.Text = Title + String.Format(" - Connected to {0} with version {1}", Unit.Name, Unit.Version);
                
                if (graphs.Count > 0)
                {
                    foreach (AOSGraph g in graphs)
                    {
                        g.UpdateCurves(curves);
                        AddGraph(g);
                    }
                }

                if (mf != null && !mf.IsDisposed)
                {
                    mf.UpdateValues(types, PList);
                }
                return;
            }
        }
        #endregion

        // Eventhandlers for Graph events.
        #region Graphs
        // Zoom in and out with the mousewheel.
        /*//private void zgc_MouseWheel(object sender, EventArgs e)
        //{
        //    HandledMouseEventArgs ma = (HandledMouseEventArgs)e;
        //    // Prevent default actions from take place.
        //    ((HandledMouseEventArgs)e).Handled = true;

        //    if (ma.Delta > 0)
        //    {
        //        int new_scroll = graphContainer.VerticalScroll.Value - 
        //            (graphContainer.VerticalScroll.SmallChange + graphContainer.VerticalScroll.LargeChange) 
        //            / 2;
        //        if (new_scroll > graphContainer.VerticalScroll.Minimum)
        //            graphContainer.VerticalScroll.Value = new_scroll;
        //        else
        //            graphContainer.VerticalScroll.Value = graphContainer.VerticalScroll.Minimum;
        //    }
        //    if (ma.Delta < 0)
        //    {
        //        int new_scroll = graphContainer.VerticalScroll.Value + (graphContainer.VerticalScroll.SmallChange + graphContainer.VerticalScroll.LargeChange) / 2;
        //        if (new_scroll < graphContainer.VerticalScroll.Maximum)
        //            graphContainer.VerticalScroll.Value = new_scroll;
        //        else
        //            graphContainer.VerticalScroll.Value = graphContainer.VerticalScroll.Maximum;
        //    }
        //}*/

        // Hightlight the curve being clicked
        private void ZedGraphController_MouseClick(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                PointF clickedPoint;
                Object c;
                GraphPane gp;
                int curveindex;
                if (sender is ZedGraphControl)
                {
                    clickedPoint = new PointF(e.X, e.Y);
                    try
                    {
                        ZedGraphControl z = (ZedGraphControl)sender;
                        z.MasterPane.FindNearestPaneObject(clickedPoint, z.CreateGraphics(), out gp, out c, out curveindex);
                        gp.FindNearestObject(clickedPoint, z.CreateGraphics(), out c, out curveindex);
                        LineItem l = ((LineItem)c);
                        if (l != null)
                        {
                            if (l.Line.Width == 2)
                                ((LineItem)c).Line.Width = 1;
                            else
                                ((LineItem)c).Line.Width = 2;
                            OnGraphdataChanged();
                        }
                    }
                    catch { }
                }
            }
        }
        #endregion

        // Eventhandlers for the MenuStrip-items.
        #region MenuStrip_Listeners
        #region File
        // Load an entire measurement
        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (IsConnected)
            {
                MessageBox.Show("Disconnect before loading saved analyzis");
                return;
            }
            
            OpenFileDialog d = new OpenFileDialog();
            d.DefaultExt = ".agm";
            d.Filter = "AOSGraphMeasure file (*.agm)|*.agm|All files (*.*)|*.*";

            if (d.ShowDialog() != DialogResult.OK)
            {
                return;
            }

            OpenMeasurement(d.FileName);
        }
        // Save an entire measurement
        private void saveToolStripMenuItem_Click(object sender, EventArgs e)
        {
            // Get a comment on the measurement
            SimpleTextInputDialog stid = new SimpleTextInputDialog("Measurement comment");
            stid.inputMessage = MeasureComment;
            stid.ShowDialog();
            String comment = stid.inputMessage;

            // Setup save dialog.
            SaveFileDialog d = new SaveFileDialog();
            d.DefaultExt = ".agm";
            d.Filter = "AOSGraphMeasure file (*.agm)|*.agm|All files (*.*)|*.*";



            XElement xdata = new XElement("AOSData");

            if (d.ShowDialog() != DialogResult.OK)
            {
                return;
            }

            {
                xdata.Add(new XElement("MeasureComment", comment));
                xdata.Add(new XElement("MeasureDate", System.DateTime.Now.ToString()));
            }

            {
                XmlSerializer ser = new XmlSerializer(typeof(UnitDescr));
                XDocument doc = new XDocument();
                using (XmlWriter xw = doc.CreateWriter())
                {
                    ser.Serialize(xw, Unit);
                    xw.Close();
                }
                xdata.Add(doc.Root);
            }

            // Serialize types.
            {
                XmlSerializer ser = new XmlSerializer(typeof(List<TypeDescr>));
                XDocument doc = new XDocument();
                using (XmlWriter xw = doc.CreateWriter())
                {
                    ser.Serialize(xw, types);
                    xw.Close();
                }
                xdata.Add(doc.Root);
            }
            // Serialize graphs.
            {
                XmlSerializer ser = new XmlSerializer(graphs.GetType());
                XDocument doc = new XDocument();
                using (XmlWriter xw = doc.CreateWriter())
                {
                    ser.Serialize(xw, graphs);
                    xw.Close();
                }
                xdata.Add(doc.Root);
            }

            // Serialize values.
            {
                XDocument doc = new XDocument();
                XmlWriter xw = doc.CreateWriter();
                String line = "";

                xw.WriteStartElement("AOSMeasurements");
                for (int i = 0; i < PList[0].Count; i++)
                {
                    xw.WriteStartElement("Measure");
                    line = PList[0][i].X.ToString();
                    for (int j = 0; j < PList.Count; j++)
                    {
                        line += ";" + PList[j][i].Y.ToString();
                    }
                    xw.WriteString(line);
                    xw.WriteEndElement();
                }
                xw.WriteEndElement();
                xw.Close();
                xdata.Add(doc.Root);
            }

            try
            {
                xdata.Save(d.FileName);
            }
            catch (Exception)
            {
                MessageBox.Show("Could not open file: " + d.FileName);
                return;
            }
        }

        // Saves a graph setup
        private void SaveGraphSetup(object sender, EventArgs e)
        {
            if (graphs != null)
            {
                SaveFileDialog sd = new SaveFileDialog();
                sd.DefaultExt = ".ags";
                sd.Filter = "AOSGraphSetup file (*.ags)|*.ags|All files (*.*)|*.*";

                if (sd.ShowDialog() != DialogResult.OK)
                {
                    return;
                }

                if (graphs.Count == 0)
                {
                    MessageBox.Show("No graphs to save");
                    return;
                }

                // Serialize graphs.
                XElement xdata = new XElement("AOSGraphSetup");
                XmlSerializer ser = new XmlSerializer(graphs.GetType());
                XDocument doc = new XDocument();
                using (XmlWriter xw = doc.CreateWriter())
                {
                    ser.Serialize(xw, graphs);
                    xw.Close();
                }
                xdata.Add(doc.Root);

                try
                {
                    xdata.Save(sd.FileName);
                }
                catch (Exception)
                {
                    MessageBox.Show("Could not open file: " + sd.FileName);
                    return;
                }
            }
        }

        // Opens a graph setup
        private void LoadGraphSetup(object sender, EventArgs e)
        {
            OpenFileDialog d = new OpenFileDialog();
            d.DefaultExt = ".ags";
            d.Filter = "AOSGraphSetup file (*.ags)|*.ags|All files (*.*)|*.*";
            
            if (d.ShowDialog() != DialogResult.OK)
            {
                return;
            }
            string path = d.FileName;
            XElement xdata;
            try
            {
                xdata = XElement.Load(path);
            }
            catch (Exception)
            {
                MessageBox.Show("Could not load file: " + path);
                return;
            }
            ClearGraphs();
            zgc.Height = 0;
            //Deserialize graph setup
            try
            {
                XmlSerializer deserializer = new XmlSerializer(typeof(List<AOSGraph>));
                graphs = (List<AOSGraph>)deserializer.Deserialize(xdata.Element("ArrayOfAOSGraph").CreateReader());
            }
            catch (Exception)
            {
                MessageBox.Show("Error parsing the graph setup, please check the file for errors");
                return;
            }
            if (graphs.Count > 0)
            {
                foreach (AOSGraph g in graphs)
                {
                    g.UpdateCurves(curves);
                    AddGraph(g);
                }
            }

            if (PList != null)
            {
                zoomAllX(sender, e);
            }
            String[] splittedPath = path.Split(new char[] { '\\' });
            graphSetupStatusText.Text = "Current Graph Setup: " + splittedPath[splittedPath.Length - 1];
        }

        // Export measured comma-separated values to a textfile.
        private void exportMeasuredToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (PList != null && types != null)
            {
                SaveFileDialog sd = new SaveFileDialog();
                sd.DefaultExt = ".csv";
                sd.Filter = "CSV file (*.csv)|*.csv|All files (*.*)|*.*";

                if (sd.ShowDialog() != DialogResult.OK)
                {
                    return;
                }

                if (PList.Count == 0)
                {
                    MessageBox.Show("No values to save");
                    return;
                }

                TextWriter tw = new StreamWriter(sd.FileName);
                string variables = "Time;";
                string units = "s;";

                foreach (TypeDescr t in types)
                {
                    variables += t.Name + ";";
                    units += t.Unit + ";";
                }

                tw.WriteLine(variables);
                tw.WriteLine(units);

                for (int i = 0; i < PList[0].Count; i++)
                {
                    String row = PList[0][i].X + ";";
                    for (int j = 0; j < PList.Count; j++)
		            {
                        row += PList[j][i].Y + ";";
        			}
                    tw.WriteLine(row);
                }
                tw.Close();                
            }
        }

        // Open the Measure info form
        private void ShowMeasureInfoForm(object sender, EventArgs e)
        {
            if (mif == null || mif.IsDisposed)
            {
                mif = new MeasurementInfoForm(MeasureComment, MeasureDate, Unit);
            }
            mif.MCC += new MeasurementInfoForm.MeasureCommentChanged(UpdateMeasureComment);
            mif.Show();
            mif.Activate();
        }

        private void UpdateMeasureComment(String comment)
        {
            this.MeasureComment = comment;
        }

        // Disconnect and save settings, then close the program.
        private void quitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (IsConnected)
            {
                Disconnect();
            }
            settings.Serialize().Save("Config_quit.xml");
            Close();
        }

        // Add stored measured values to the current measure
        private void appendMeasurementToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (IsConnected)
            {
                MessageBox.Show("Disconnect before loading saved analyzis");
                return;
            }

            OpenFileDialog d = new OpenFileDialog();
            d.DefaultExt = ".agm";
            d.Filter = "AOSGraphMeasure file (*.agm)|*.agm|All files (*.*)|*.*";

            if (d.ShowDialog() != DialogResult.OK)
            {
                return;
            }

            XElement xdata;
            try
            {
                xdata = XElement.Load(d.FileName);
            }
            catch (Exception)
            {
                MessageBox.Show("Could not load file: " + d.FileName);
                return;
            }

            // Deserialize Values
            try
            {
                XmlReader reader = xdata.Element("AOSMeasurements").CreateReader();
                String temp = xdata.Element("AOSMeasurements").Value;
                String[] values;
                char[] separator = new char[] { ';' };

                double offset = (PList.Count > 0 && PList[0].Count > 0) ? PList[0][PList[0].Count - 1].X : 0;

                // Read and parse the values
                while (reader.Read())
                {
                    String line = reader.Value;
                    if (line != "")
                    {
                        values = line.Split(separator);
                        double x = Double.Parse(values[0]) + offset;
                        for (int i = 1; i < values.Length; i++)
                        {
                            double y = Double.Parse(values[i]);
                            PList[i - 1].Add(new PointPair(x, y));
                        }
                    }
                }
            }
            catch
            {
                MessageBox.Show("Error parsing the values, please check the file for errors");
                Init();
                return;
            }

            SuspendLayout();
            ResumeLayout();
            zoomAllX(sender, e);
        }

        private void propertiesToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (pf == null || pf.IsDisposed)
            {
                pf = new PropertiesForm();
            }
            pf.Show();
        }
        #endregion

        #region Connect/Disconnect Start/Stop
        // Call method Connect with ip and port recieved in the args.
        private void connectEvent(object sender, AOSConnectEventArgs e)
        {
            Connect(e.ip, e.port);
            if (e.graphSetupPath != "")
            {
                LoadGraphSetup(e.graphSetupPath);
            }
        }

        // Creates, adds a listener and shows the window 
        private void ShowConnectWindow(object sender, EventArgs e)
        {
            if (connectToolStripMenuItem.Text == "Connect...")
            {
                if (connf == null || connf.IsDisposed)
                {
                    connf = new ConnectForm(settings);
                    connf.pressed += new PressedEventHandler(connectEvent);
                }
                connf.Show();
                connf.Activate();
            }
            else
            {
                Disconnect();
            }
        }

        // Start/Stop a measure
        private void startToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (startToolStripMenuItem.Text == "Start Measure")
            {
                IsTracing = true;
                traceBox.Checked = true;
                StartMeasureTool.Enabled = false;
                StartMeasureTool.Visible = false;
                StopMeasureTool.Enabled = true;
                StopMeasureTool.Visible = true;
                if (TypesParsed)
                    StartMeasure();
            }
            else
            {
                StopMeasure();
            }
        }
        #endregion

        #region Graph
        // Creates, adds listener and shows the window to add graphs.
        private void ShowAddGraphWindow(object sender, EventArgs e)
        {
            if (agf == null || agf.IsDisposed)
            {
                agf = new AddGraphForm(curves);
            }
            agf.graphAdded += new AddGraphForm.AOSVisGraphAdded(AddGraph);
            agf.Show();
            agf.Activate();
        }
        // Creates and shows the window to edit graphs.
        private void ShowEditGraphWindow(object sender, EventArgs e)
        {
            if (egf == null || egf.IsDisposed)
            {
                egf = new EditGraphForm(curves, graphs);
                egf.updatedGraph += new GraphdataChanged(UpdateZGC);
                graphListChanged += new AOSGraphListChanged(egf.UpdateGraphs);
            }
            egf.Show();
            egf.Activate();
        }
        // Creates, adds listener and shows the window to remove graphs.
        private void ShowRemoveGraphWindow(object sender, EventArgs e)
        {
            if (rgf == null || rgf.IsDisposed)
            {
                rgf = new RemoveGraphForm(graphs);
                rgf.graphRemoved += new AOSVisGraphAdded(RemoveGraph);
            }            
            rgf.Show();
            rgf.Activate();
        }
        // Creates and shows the window to change curvecolors.
        private void curvesToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (curvef == null || curvef.IsDisposed)
            {
                curvef = new CurveForm(curves);
            }
            curvef.Show();
            curvef.Activate();
        }
        
        // Toggle the visibility of the graphs title,
        private void titleOnOffToolStripMenuItem_Click(object sender, EventArgs e)
        {
            foreach (GraphPane gp in graphpanes)
            {
                gp.Title.IsVisible = !titleOnOffToolStripMenuItem.Checked;
            }
            titleOnOffToolStripMenuItem.Checked = !titleOnOffToolStripMenuItem.Checked;

            OnGraphdataChanged();
        }
        // Toggle the visibility of the graphs legend.
        private void legendOnOffToolStripMenuItem_Click(object sender, EventArgs e)
        {
            foreach (GraphPane gp in graphpanes)
            {
                gp.Legend.IsVisible = !legendOnOffToolStripMenuItem.Checked;
            }
            legendOnOffToolStripMenuItem.Checked = !legendOnOffToolStripMenuItem.Checked;
            OnGraphdataChanged();
        }
        // Toggle the visibility of the graphs pointvalues when hovering a point.
        private void pointValuesOnOffToolStripMenuItem_Click(object sender, EventArgs e)
        {
            zgc.IsShowPointValues = !pointValuesOnOffToolStripMenuItem.Checked;
            pointValuesOnOffToolStripMenuItem.Checked = !pointValuesOnOffToolStripMenuItem.Checked;
            OnGraphdataChanged();
        }
        #endregion

        #region Unit
        // Creates and shows the UnitParameter window
        private void ShowUnitParameterWindow(object sender, EventArgs e)
        {
            if (param == null || param.IsDisposed)
            {
                param = new UnitParameterForm(parameters, parameterchanges, aosconn);
            }
            param.Show();
            param.Activate();
        }
        // Creates and shows the Unitabout window
        private void ShowUnitAboutWindow(object sender, EventArgs e)
        {
            if (about == null || about.IsDisposed)
            {
                about = new UnitAboutForm(Unit);
            }
            about.Show();
            about.Activate();
        }
        #endregion

        #region Tools
        // Show the commentwindow to write/view comments.
        private void commentsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (commf == null || commf.IsDisposed)
            {
                commf = new CommentForm(graphs);
            }
            commf.Show();
            commf.Activate();
        }

        // Show the slicewindow to cut down a measure
        private void sliceToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (mf == null || mf.IsDisposed)
            {
                mf = new MarkerForm(markers, types, PList);
                foreach (AOSGraph graph in graphs)
                {
                    mf.markerChanged += new AOSMarkerChanged(graph.OnMarkerChanged);
                    mf.markerChanged += new AOSMarkerChanged(OnAOSMarkerChanged);
                    mf.markerListChanged += new AOSMarkerListChanged(graph.OnMarkerListChanged);
                    mf.markerListChanged += new AOSMarkerListChanged(OnAOSMarkerListChanged);
                }
            }
            if (sf == null || sf.IsDisposed)
            {
                sf = new SliceForm(markers);
                sf.measuresChanged += new AOSMeasuresChanged(SliceEventHandler);
                mf.markerListChanged += new AOSMarkerListChanged(sf.MarkerListChanged);
            }
            sf.Show();
            sf.Activate();
        }

        void SliceEventHandler(object sender, AOSMeasureSliceEventArgs e)
        {
            SliceMeasure(e.start, e.end);
        }
        #endregion
        // Shows the markerwindow and adds listeners to it's markerevents.
        private void markerToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (mf == null || mf.IsDisposed)
            {
                mf = new MarkerForm(markers, types, PList);
                foreach (AOSGraph graph in graphs)
                {
                    mf.markerChanged += new AOSMarkerChanged(graph.OnMarkerChanged);
                    mf.markerChanged += new AOSMarkerChanged(OnAOSMarkerChanged);
                    mf.markerListChanged += new AOSMarkerListChanged(graph.OnMarkerListChanged);
                }
                mf.markerListChanged += new AOSMarkerListChanged(OnAOSMarkerListChanged);
            }
            mf.Show();
        }

        // Updates the SweepSpann-value when hitting enter in the SpannBox.
        private void SpannBox_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Return)
            {
                SweepSpann = (int)SpannBox.Value;
                OnGraphdataChanged();
            }
        }
        // Updates the SweepSpann-value when changing value in the Spannbox.
        private void SpannBox_ValueChanged(object sender, EventArgs e)
        {
            SweepSpann = (int)SpannBox.Value;
            UpdateGraphs();
        }
        
        // Toggles the tracefunction.
        private void traceBox_CheckedChanged(object sender, EventArgs e)
        {
            IsTracing = traceBox.Checked;
            OnGraphdataChanged(); // Force redraw
        }

        #endregion

        // Toggles the scatterplot. 
        // Toggles checkstatus on MenuStrip and ContextMenu.
        #region Scatter
        private void scatterToolStripMenuItem_CheckedChanged(object sender, EventArgs e)
        {
            if (zgc.ContextMenuStrip.Items.ContainsKey("scatter"))
            {
                ((ToolStripMenuItem)zgc.ContextMenuStrip.Items["scatter"]).Checked = scatterToolStripMenuItem.Checked;
            }
            foreach (LineItem l in curves)
            {
                if (l.Symbol.Type == SymbolType.None)
                {
                    l.Symbol.Type = SymbolType.Default;
                }
                else
                {
                    l.Symbol.Type = SymbolType.None;
                }
            }
            scatterToolStripMenuItem.Checked = ((ToolStripMenuItem)zgc.ContextMenuStrip.Items["scatter"]).Checked;
            OnGraphdataChanged();
        }
        private void rightClickScatterItem_CheckedChanged(object sender, EventArgs e)
        {
            ToolStripMenuItem t = (ToolStripMenuItem)sender;
            scatterToolStripMenuItem.Checked = t.Checked;
        }
        #endregion

        /// <summary>
        /// Performes a zoom on the whole measurement on the X-axis.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void zoomAllX(object sender, EventArgs e)
        {
            if (PList.Count > 0 && PList[0].Count > 0)
            {
                foreach (GraphPane gp in graphpanes)
                {
                    RestoreZoomEvents();
                    int measures = PList[0].Count;
                    int max = (int)PList[0][measures - 1].X;
                    gp.XAxis.Scale.Min = 0;
                    gp.XAxis.Scale.Max = max;
                }
            }
            OnGraphdataChanged();
        }

        // Eventhandlers for StatusStrip-items.
        #region StatusStrip
        /// <summary>
        /// Changes the current PaneLayout
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Layoutchanged(object sender, EventArgs e)
        {
            ToolStripMenuItem menuItem = (ToolStripMenuItem)sender;
            PaneLayout _PL = (PaneLayout)Enum.Parse(typeof(PaneLayout), menuItem.Text);
            zgc.MasterPane.SetLayout(zgc.CreateGraphics(), _PL);
            menuItem.CheckState = CheckState.Checked;
            layoutStatusText.Text = "Current Layout: " + menuItem.Text;
            OnGraphdataChanged();
        }
        #endregion

        /// <summary>
        /// Show analyzers aboutbox
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void aboutToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (aboutBox == null || aboutBox.IsDisposed)
            {
                aboutBox = new AboutBox();
            }
            aboutBox.ShowDialog();
        }
        /// <summary>
        /// Asks for the graphheigt and then sets the graphcontrollers height.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void graphHeightToolStripMenuItem_Click(object sender, EventArgs e)
        {
            string graphHeight = "";
            graphHeight = InputDialogBox.Show("Enter the wished height for a single graph.", "", "Graph height").Trim();
            int temp;
            if (int.TryParse(graphHeight, out temp))
            {
                GraphHeight = temp;

                zgc.Height = (int)(
                    (temp + zgc.MasterPane.InnerPaneGap)
                    * zgc.MasterPane.PaneList.Count);
            }
        }

        /// <summary>
        /// Disconnect and save settings before closing.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void AOSVisualizer_FormClosing(object sender, FormClosingEventArgs e)
        {

            Properties.Settings.Default.State = this.WindowState;
            if (this.WindowState == FormWindowState.Normal)
            {
                Properties.Settings.Default.Bounds = this.Bounds;
                Properties.Settings.Default.WindowLocation = this.Location;
            }
            Properties.Settings.Default.Save();
            if (IsConnected)
            {
                Disconnect();
            }
            try
            {
                settings.Serialize().Save(Application.StartupPath+"/Settings.xml");
            }
            catch (Exception)
            {
                MessageBox.Show("Could not write settings.xml");
            }
        }

        private void undoAllZoomToolStripMenuItem_Click(object sender, EventArgs e)
        {
            foreach (GraphPane gp in graphpanes)
            {
                gp.ZoomStack.PopAll(gp);
            }
            OnGraphdataChanged();
        }

        private void AOSVisualizer_KeyUp(object sender, KeyEventArgs e)
        {
            if (e.Modifiers == Keys.Control)
            {
                switch (e.KeyCode)
                {
                    case Keys.A:
                        AddGraphTool.PerformClick();
                        break;
                    case Keys.C:
                        if (IsConnected)
                        {
                            DisconnectTool.PerformClick();
                        }
                        else
                        {
                            ConnectTool.PerformClick();
                        }
                        break;
                    case Keys.E:
                        EditGraphTool.PerformClick();
                        break;
                    case Keys.M:
                        if (StartMeasureTool.Enabled)
                        {
                            StartMeasureTool.PerformClick();
                        }
                        else
                        {
                            StopMeasureTool.PerformClick();
                        }
                        break;
                    case Keys.O:
                        OpenTool.PerformClick();
                        break;
                    case Keys.P:
                        ParameterTool.PerformClick();
                        break;
                    case Keys.R:
                        RemoveGraphTool.PerformClick();
                        break;
                    case Keys.S:
                        SaveTool.PerformClick();
                        break;
                    default:
                        break;
                }
            }
        }
                         
        /// <summary>
        /// Calls the event graphdataChanged() if it's not null.
        /// </summary>
        private void OnGraphdataChanged()
        {
            if (graphdataChanged != null)
                graphdataChanged();
        }
        /// <summary>
        /// Calls OnGraphdataChanged since we just want to refresh the graphs.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void OnAOSMarkerChanged(object sender, AOSMarkerEventArgs e)
        {
            OnGraphdataChanged();
        }
        /// <summary>
        /// Updates DropDownItems of the MarkerPicker 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void OnAOSMarkerListChanged(object sender, AOSMarkerListEventArgs e)
        {
            String selected = MarkerPicker.Text;
            SelectedMarker = null;
            MarkerPicker.Text = "Select marker";
            MarkerPicker.DropDownItems.Clear();
            foreach (AOSMarker marker in e.markers)
            {
                ToolStripItem t = new ToolStripButton(marker.name);
                t.Tag = marker;
                t.DisplayStyle = ToolStripItemDisplayStyle.Text;
                t.Click += new EventHandler(SelectedMarkerChanged);
                MarkerPicker.DropDownItems.Add(t);
                if (t.Text == selected)
                {
                    MarkerPicker.Text = selected;
                    SelectedMarker = marker;
                }
            }
        }
        private void SelectedMarkerChanged(object sender, EventArgs e)
        {
            ToolStripButton t = (ToolStripButton)sender;
            MarkerPicker.Text = t.Text;
            SelectedMarker = (AOSMarker)t.Tag;
        }
        private void ToolStripMarkerMove(object sender, EventArgs e)
        {
            ToolStripButton b = (ToolStripButton)sender;
            if (SelectedMarker != null)
            {
                double xval = SelectedMarker.Location.X;
                int movement = 0;
                switch (b.Text)
                {
                    case "<<":
                        movement -= Int32.Parse(Properties.Settings.Default.markerStepValue.ToString());
                        break;
                    case "<":
                        movement += -1;
                        break;
                    case ">":
                        movement += 1;
                        break;
                    case ">>":
                        movement += Int32.Parse(Properties.Settings.Default.markerStepValue.ToString());
                        break;
                    default:
                        break;
                }
                if(PList.Count > 0)
                {
                    int index =
                        PList[0].FindIndex(point => point.X >= xval);
                    if (index >= 0 && 
                        index+movement > 0 &&
                        PList[0].Count > (index + movement))
                    {
                        SelectedMarker.Location.X = PList[0][index + movement].X;
                    }
                    else
                    {
                        SelectedMarker.Location.X += movement;
                    }
                }
            }
            OnGraphdataChanged();
        }

        /// <summary>
        /// Tells all listeners that the list of graphs has changed
        /// and sends the new list as EventArg.
        /// </summary>
        /// <param name="sender"></param>
        private void OnGraphListChange(object sender)
        {
            if (graphListChanged != null)
            {
                graphListChanged(sender, new AOSGraphListEventArgs(graphs));
            }
        }
        #endregion

        private void AddMarkerButton_Click(object sender, EventArgs e)
        {
            SimpleInputDialog sid = new SimpleInputDialog("Enter the name of the marker");
            sid.inputMessage = "Marker 1";
            sid.ShowDialog();
            AddMarker(sid.inputMessage, 10);
        }
    }
}