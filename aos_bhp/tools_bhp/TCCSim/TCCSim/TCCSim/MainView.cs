/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2014-08-18    Hidaji      Backward compatible with previous protocol (no SiteID)
* 2016-06-28    akushwah    Added Region ID
*******************************************************************************/
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using System.IO;
using ExpandableLayoutPanel;
using TCCtcp;
using System.Threading;
using System.Collections;
using System.Xml.Linq;

namespace TCCSim
{
    public partial class MainView : Form
    {
        private string TCCSimTitle = "TCCSIM";
        private string TCCSimExeVer = "";

        public event MVEvent MVE;
        public delegate void MVEvent(EventArgs e);

        private ButtonView BtnV;

        private Model Mdl;
        private List<MessageFile> FileList;
        private Dictionary<string, FieldType> MessageDict;
        public BackgroundWorker Worker { private set; get; }
        public BackgroundWorker ScriptWorker { private set; get; }

        private CheckBox AutoScrollCheckBox = new CheckBox();

        private int PRValue = 132;
        private String PRErrorBlock = "ERROR_MESSAGE_DATA";
        private Dictionary<String, int> ErrorLevelValue = new Dictionary<string, int>();

        private bool FirstPR = true; // When first PR is received PRTable is initialized
        private List<Label> PRLabels = new List<Label>(); // Keep track of value labels in PRTable
        private List<ToolTip> PRToolTips = new List<ToolTip>(); // ToolTips for all value labels in PRTable
        private List<Control> MessageControls = new List<Control>(); // For disposing of MsgInfoTable controls
        private List<ButtonConfiguration[]> ButtonSets = new List<ButtonConfiguration[]>();
        private ButtonConfiguration[] activeButtonSet;
        private List<Button> MacroButtons = new List<Button>();

        int ErrorNr = 0;

        private String messagePath;
        String ButtonSetPath;
        private bool comboBoxSetupDone = false;

        public MainView(Model M, List<MessageFile> FileList, String MessageDir, Dictionary<string, FieldType> MessageDict)
        {
            Mdl = M;
            Mdl.ME += new Model.MEvent(Mdl_ME);
            for (int i = 0; i < Screen.AllScreens.Length; i++)
            {
                if (Screen.AllScreens[i].Bounds.Contains(Properties.Settings.Default.Bounds.Location))
                    this.StartPosition = FormStartPosition.Manual;
            }

            InitializeComponent();


            this.UpdateBounds(Properties.Settings.Default.Bounds.X,
                    Properties.Settings.Default.Bounds.Y,
                    Properties.Settings.Default.Bounds.Width,
                    Properties.Settings.Default.Bounds.Height);
            this.WindowState = Properties.Settings.Default.State;

            this.FileList = FileList;
            this.MessageDict = MessageDict;

            this.messagePath = Path.GetFullPath(MessageDir.TrimEnd('\\'));
            this.pathLabel.Text = messagePath;

            this.ConnectButton.Tag = false;
            this.PauseButton.Tag = false;
            this.RunScriptButton.Tag = false;

            MessageMenu.Items.Add("Send", null, MessageMenuHandler);
            MessageMenu.Items.Add("Edit", null, MessageMenuHandler);
            MessageMenu.Items.Add("Delete", null, MessageMenuHandler);

            TransferMenu.Items.Add("Open in MessageViewer", null, TransferMenuHandler);

            //Init filter button
            foreach (KeyValuePair<string, FieldType> kvp in MessageDict)
            {
                ToolStripMenuItem Item = new ToolStripMenuItem(kvp.Value.name, null, FilterHandler);
                Item.Tag = kvp.Key;
                Item.Checked = !(bool)kvp.Value.values[0];
                FilterButton.DropDownItems.Add(Item);
            }

            // Get PR value and error block
            String s;
            if ((s = Mdl.GetPRErrorBlock()) != null)
                this.PRErrorBlock = s;
            int v;
            if ((v = Mdl.GetPRValue()) != -1)
                PRValue = v;


            MessageBox.DrawMode = DrawMode.OwnerDrawFixed;
            MessageBox.DrawItem += new DrawItemEventHandler(MessageBox_DrawItem);
            UpdateMessageBox();
            MessageWatcher.Path = MessageDir;

            // Init backgroundworker for Communication and Script
            this.Worker = new BackgroundWorker();
            Worker.WorkerReportsProgress = true;
            Worker.WorkerSupportsCancellation = true;
            this.ScriptWorker = new BackgroundWorker();
            ScriptWorker.WorkerSupportsCancellation = true;
            ScriptWorker.WorkerReportsProgress = true;

            // Init ErrorCodeValue Dict
            ErrorLevelValue.Add("Fatal", 70000);
            ErrorLevelValue.Add("Minor", 60000);
            ErrorLevelValue.Add("Log", 50000);

            //Add autoscroll checkbox to toolstrip
            this.AutoScrollCheckBox.Checked = true;
            this.AutoScrollCheckBox.Text = "Auto Scroll";
            ToolStripControlHost t = new ToolStripControlHost(this.AutoScrollCheckBox);
            toolStrip1.Items.Add(t);

            // Settings for Transfer Dialogs
            SaveDialog.Filter = "TXT file (*.txt)|*.txt|All files (*.*)|*.*";
            LoadDialog.Filter = "TXT file (*.txt)|*.txt|All files (*.*)|*.*";
            SaveDialog.InitialDirectory = AppDomain.CurrentDomain.BaseDirectory;
            LoadDialog.InitialDirectory = AppDomain.CurrentDomain.BaseDirectory;

            // Setting for Run Script Dialog
            RunScriptDialog.InitialDirectory = AppDomain.CurrentDomain.BaseDirectory;

            // Fix tables horizontal scrollblar
            int vertScrollWidth = SystemInformation.VerticalScrollBarWidth;
            this.PRTable.Padding = new Padding(0, 0, vertScrollWidth, 0);
            this.MsgInfoTable.Padding = new Padding(0, 0, vertScrollWidth, 0);
            this.HexTable.Padding = new Padding(0, 7, vertScrollWidth, 0);


            // Assemble title including version from Assembly / Bo H
            TCCSimExeVer = Assembly.GetExecutingAssembly().GetName().Version.Major.ToString() + "." +
                     Assembly.GetExecutingAssembly().GetName().Version.Minor.ToString() + "." +
                     Assembly.GetExecutingAssembly().GetName().Version.Build.ToString();

            TCCSimTitle = "TCCSIM v" + TCCSimExeVer;


            Text = TCCSimTitle;

            foreach (DriveInfo d in DriveInfo.GetDrives())
            {
                if (d.DriveType != DriveType.CDRom)
                {
                    comboBox1.Items.Add(d);

                    if (d.RootDirectory.Root.ToString().Equals(Path.GetPathRoot(MessageDir), StringComparison.OrdinalIgnoreCase))
                        comboBox1.SelectedIndex = comboBox1.Items.Count - 1;
                }
            }
            comboBoxSetupDone = true;

            ButtonSetPath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
            @"ButtonSets.xml");

            ButtonSets = ReadButtonSets(ButtonSetPath);

            BtnV = new ButtonView(ButtonSets, ButtonSetPath);
            BtnV.buttonSetChanged += new ButtonView.ButtonSetsChanged(BtnV_buttonSetChanged);

            // Setup macrobuttons
            for (int i = 0; i < MacroButtonPanel.ColumnCount - 1; i++)
            {
                Button b = new Button();
                b.Dock = DockStyle.Fill;
                b.Click += new EventHandler(MacroButtonClicked);
                MacroButtonPanel.Controls.Add(b, i, 0);
                MacroButtons.Add(b);
            }

            ButtonSetBox.Items.Clear();
            for (int i = 0; i < ButtonSets.Count; i++)
            {
                ButtonSetBox.Items.Add(ButtonSets[i][0]);
            }
            ButtonSetBox.SelectedIndex = 0;

            this.splitContainer.SplitterDistance = Properties.Settings.Default.SplitDist;
        }

        private List<ButtonConfiguration[]> ReadButtonSets(String path)
        {
            List<ButtonConfiguration[]> ButtonSets = new List<ButtonConfiguration[]>();

            ButtonConfiguration[] tmp;
            try
            {
                XElement xml = XElement.Load(path);

                foreach (XElement buttonset in xml.Descendants("ButtonSet"))
                {
                    tmp = new ButtonConfiguration[10];
                    tmp[0] = new ButtonConfiguration(buttonset.Attribute("Name").Value, "");
                    int i = 1;
                    foreach (XElement button in buttonset.Descendants("Button"))
                    {
                        tmp[i++] = new ButtonConfiguration(button.Attribute("Name").Value, button.Value);
                    }
                    ButtonSets.Add(tmp);
                }
            }
            catch
            {
                tmp = new ButtonConfiguration[10];
                System.Windows.Forms.MessageBox.Show("ButtonSets.xml are not found at " + path);
                for (int i = 0; i < tmp.Length; i++)
                {
                    tmp[i] = new ButtonConfiguration();
                }
                tmp[0].Name = "Default";
                ButtonSets.Add(tmp);
            }
            return ButtonSets;
        }

        #region ModelEvents

        /// <summary>
        /// Handles events sent from Model
        /// </summary>
        /// <param name="e"></param>
        private void Mdl_ME(EventArgs e)
        {
            //New message should be shown
            if (e is DeliverEventArgs)
            {
                DeliverEventArgs ev = e as DeliverEventArgs;
                AddMsgToTransfers(ev, false);
            }
            // Connectbutton has been pressed and the Model
            // maybe sends connectdata from the config
            else if (e is ConnectEventArgs)
            {
                ConnectEventArgs ev = e as ConnectEventArgs;
                ConnectDialog CD;
                // Config could be parsed
                if (ev.Connecting)
                {
                    CD = new ConnectDialog(ev.ID, ev.siteID, ev.regionID, ev.Connect, ev.Timer, ev.Timeout, ev.AutoConnect, ev.Crc);
                }
                // Config could not be parsed
                else
                {
                    CD = new ConnectDialog();
                    StatusLabel.Text = "Unable to read Config.xml";
                }
                // Result from the connection dialog is sent to Model
                if (CD.ShowDialog() == DialogResult.OK)
                {
                    StatusLabel.Text = "Connecting";
                    ConnectEventArgs CE = new ConnectEventArgs(CD.ID, CD.siteID, CD.regionID, CD.Connect, CD.Timer, CD.Timeout, CD.AutoConnect, CD.crc);

                    TrainIDStatusLabel.Text = "Train ID: " + CD.ID;
                    SiteIDStatusLabel.Text = "Site ID: " + CD.siteID;
                    RegionIDStatusLabel.Text = "Region ID: " + CD.regionID;

                    IPStatusLabel.Text = "Connected to: " + CD.Connect;
                    PollingStatusLabel.Text = "Polling Interval: " + CD.Timer;
                    TimeoutStatusLabel.Text = "Timeout: " + CD.Timeout;
                    CrcStatusLabel.Text = "Crc: " + ((CD.crc == tccCrc.tccCrcCentral) ? "Central" : "Region");

                    MVE(CE);
                    SetPauseStatus();
                    SetConnectStatus();
                }
            }
            // Model tells MainView the state of the connection
            // is changed
            else if (e is ConnectedEventArgs)
            {
                // A connection has been established
                if (((ConnectedEventArgs)e).Connected)
                {
                    StatusLabel.Image = Properties.Resources.connected;
                    StatusLabel.Text = "Connected";

                    TCCSimTitle = "TCCSIM v" + TCCSimExeVer + "/" + Mdl.DLLCaption;
                    Text = TCCSimTitle;
                }
                // The connection has been terminated
                else
                {
                    StatusLabel.Image = Properties.Resources.disconnected;
                    StatusLabel.Text = "Disconnected";
                    SetPauseStatus();
                    SetConnectStatus();
                    this.RunScriptButton.Enabled = true;
                    TrainIDStatusLabel.Text = "";
                    SiteIDStatusLabel.Text = "";
                    RegionIDStatusLabel.Text = "";
                    IPStatusLabel.Text = "";
                    PollingStatusLabel.Text = "";
                    TimeoutStatusLabel.Text = "";
                    CrcStatusLabel.Text = "";

                    TCCSimTitle = "TCCSIM v" + TCCSimExeVer + "/" + Mdl.DLLCaption;
                    Text = TCCSimTitle;
                }
            }
            // Connection could not be established
            // or something went wrong with the connection
            else if (e is SocketErrorEventArgs)
            {
                StatusLabel.Image = Properties.Resources.disconnected;
                SocketErrorEventArgs ev = e as SocketErrorEventArgs;
                StatusLabel.Text = ev.Text;
                // If the text from the socket exception is to long
                // just the error code will be printed.
                if (StatusLabel.Width > 820)
                    StatusLabel.Text = ev.Code;
                TCCSimTitle = "TCCSIM v" + TCCSimExeVer + "/" + Mdl.DLLCaption;
                Text = TCCSimTitle;
            }
            // Something wrong with the connection parameters
            // from ConnectionDialog
            else if (e is SocketParaErrorEventArgs)
            {
                //PauseButton.Enabled = false;
                SocketParaErrorEventArgs ev = e as SocketParaErrorEventArgs;
                String message = ev.Text;
                String caption = "Unable to connect.";
                System.Windows.Forms.MessageBox.Show(message, caption, MessageBoxButtons.OK, MessageBoxIcon.Error);
                if ((bool)this.RunScriptButton.Tag)
                {
                    EnableButtonsScript();
                    this.RunScriptButton.Text = "Run Script";
                    this.RunScriptButton.Tag = false;
                }
                SetConnectStatus();
                SetPauseStatus();
                TCCSimTitle = "TCCSIM v" + TCCSimExeVer + "/" + Mdl.DLLCaption;
                Text = TCCSimTitle;
            }
            // Could not load the XML message file that was given
            else if (e is XMLFileErrorEventArgs)
            {
                XMLFileErrorEventArgs ev = e as XMLFileErrorEventArgs;
                String message = ev.Text;
                String caption = "Cannot read file";
                System.Windows.Forms.MessageBox.Show(message, caption, MessageBoxButtons.OK, MessageBoxIcon.Error);
                if (ev.CancelConnection)
                {
                    //ConnectButton.Enabled = false;

                    StatusLabel.Image = Properties.Resources.disconnected;
                    StatusLabel.Text = "Disconnected";

                    this.RunScriptButton.Tag = false;
                    this.RunScriptButton.Text = "Run Script";
                    this.RunScriptButton.Enabled = true;
                    SetConnectStatus();
                    SetPauseStatus();
                }
            }
            // A transfer i successully loaded from a file 
            else if (e is UpdateTransfersList)
            {
                UpdateTransfersList(true);
            }
            // Unable to read/parse transfer file
            else if (e is LoadTransferErrorEventArgs)
            {
                String ErrorMessage = ((LoadTransferErrorEventArgs)e).Text;
                String ErrorCaption = "Unable to parse document";
                System.Windows.Forms.MessageBox.Show(ErrorMessage, ErrorCaption, MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            else if (e is PauseEventArgs)
            {
                if (this.Mdl.PauseTag)
                    this.PauseLabel.Text = "(Paused)";
                else
                    this.PauseLabel.Text = "";
            }
            else if (e is RunScriptEventArgs)
            {
                RunScriptEventArgs ev = e as RunScriptEventArgs;
                if (ev.Cancel)
                {
                    EnableButtonsScript();
                    this.RunScriptButton.Text = "Run Script";
                    this.RunScriptButton.Tag = false;
                    this.RunScriptButton.Enabled = true;
                    SetPauseStatus();
                    SetConnectStatus();
                }
            }
            else if (e is UnknownErrorEventArgs)
            {
                String ErrorMessage = ((UnknownErrorEventArgs)e).Text;
                String ErrorCaption = "Unknown error occurred!";
                System.Windows.Forms.MessageBox.Show(ErrorMessage, ErrorCaption, MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            else if (e is ScriptOutputEventArgs)
            {
                ScriptOutputEventArgs ev = e as ScriptOutputEventArgs;
                //String[] jk = { ev.Text };
                //ListViewItem lvi = new ListViewItem(jk);
                //ScriptOutputTemp.Items.Add(lvi);
                //if (this.AutoScrollCheckBox.Checked)
                //    ScriptOutputTemp.EnsureVisible(ScriptOutputTemp.Items.Count-1);
                if (ScriptOutput.Text == "")
                    ScriptOutput.Text += ev.Text;
                else
                    ScriptOutput.Text += "\r\n" + ev.Text;
                ScriptOutput.SelectionStart = ScriptOutput.Text.Length;
                ScriptOutput.ScrollToCaret();
                ScriptOutput.Refresh();
            }
            else if (e is MainViewFixButtonsArgs)
            {
                SetPauseStatus();
                SetConnectStatus();

                MainViewFixButtonsArgs mcf = (MainViewFixButtonsArgs)e;
                TrainIDStatusLabel.Text = "Train ID: " + mcf.ID;
                SiteIDStatusLabel.Text = "Site ID: " + mcf.siteID;
                RegionIDStatusLabel.Text = "Region ID: " + mcf.regionID;

                IPStatusLabel.Text = "Connected to: " + mcf.Connect;
                PollingStatusLabel.Text = "Polling Interval: " + mcf.Timer;
                TimeoutStatusLabel.Text = "Timeout: " + mcf.Timeout;
                CrcStatusLabel.Text = "Crc: " + ((mcf.Crc == tccCrc.tccCrcCentral) ? "Central" : "Region");
            }

        }

        /// <summary>
        /// Completetly resets and refills the ListView with transfered
        /// messages. This function is called when a transfer file is loaded
        /// and when filter options has been changed.
        /// </summary>
        private void UpdateTransfersList(bool All)
        {
            TransfersView.SuspendLayout();
            ErrorView.SuspendLayout();
            TransfersView.BeginUpdate();
            //

            TransfersView.Items.Clear();
            if (All)
            {
                ErrorView.BeginUpdate();
                ErrorView.Items.Clear();
            }
            foreach (DeliverEventArgs Msg in Mdl.DeliverList)
            {
                AddMsgToTransfers(Msg, All);
            }
            TransfersView.EndUpdate();
            if (All)
                ErrorView.EndUpdate();
            TransfersView.ResumeLayout();
            ErrorView.ResumeLayout();
        }

        /// <summary>
        /// Adds a single message to the ListView with transfered messages.
        /// Ignores the message if it is filtered.
        /// If the message is of type PRValue the status table for 
        /// Position Report messages is updates as well as error blocks
        /// are added to ErrorView.
        /// </summary>
        /// <param name="Msg"></param>
        private void AddMsgToTransfers(DeliverEventArgs Msg, bool All)
        {
            if (All)
                TransfersView.BeginUpdate();
            string MType = Msg.Name; // Message.string_MTypeFromBytes(Msg.Data);

            try
            {
                Msg.Time = Msg.Time.Substring(0, 11);
            }
            catch (Exception) { Msg.Time = "Error"; }

            // Adds message to TransfersView if it's not filtered or it's of type unknown
            // or if any header error is detected.
            if ((MessageDict.ContainsKey(MType) && !(bool)MessageDict[MType].values[0])
                || MType == "Unknown"
                || Msg.CRCError || Msg.TSRefError || Msg.TSSenderError || Msg.IDError || Msg.siteIDError || Msg.regionIDError)
            {
                String first;
                String second;
                if (Msg.Sent)
                {
                    first = "TCCSim";
                    if (Message.expectedHeaderLength == 11)
                        second = Message.GetTrainSiteID(Msg.Data);
                    else
                        second = Message.GetTrainID(Msg.Data).ToString();
                }
                else
                {
                    if (Message.expectedHeaderLength == 11)
                        first = Message.GetTrainSiteID(Msg.Data);
                    else
                        first = Message.GetTrainID(Msg.Data).ToString();

                    second = "TCCSim";
                }
                // ErrorString shows what safety layer error that are present 
                String CommentString = ((Msg.TSSenderError) ? "T_SENDER " : "") +
                                    ((Msg.TSRefError) ? "T_REF " : "") +
                                    (Msg.CRCError ? "CRC " : "") +
                                    (Msg.IDError ? "TrainID" : "") +
                                    (Msg.siteIDError ? "SiteID" : "") +
                                    (Msg.regionIDError ? "RegionID" : "");
                if (CommentString == "")
                    CommentString = Msg.FileName;
                CommentString = CommentString.Trim().Replace(" ", ", ");

                // Columns are { Time, From, To, MessageType, SafetyErrors }
                String[] jk = { Msg.Time, first, second, Msg.Name, CommentString };
                //                String[] jk = { Msg.Time.Substring(0, 11), first, second, Msg.Name, ErrorString };
                ListViewItem lvi = new ListViewItem(jk);

                // Every sent and received message is given a unique ID for every transfer.
                // The ID is used for locating the associated Byte-array in a List by index
                lvi.Tag = Msg.ID;
                TransfersView.Items.Add(lvi);

                // If autoscroll is enabled the newly added message will be visible in TransfersView
                if (this.AutoScrollCheckBox.Checked)
                    TransfersView.EnsureVisible(TransfersView.Items.Count - 1);
            }
            if (All)
                TransfersView.EndUpdate();

            // If message is of value PRValue, display 
            // info in PRTable and ErrorView

            if (Msg.Data[Message.expectedHeaderLength] == this.PRValue)
            {
                // Get ArrayList with all fields and blocks
                ArrayList l = Message.ParseBytes(Msg.Data);
                int i = 0, j = 0;
                PRTable.Parent.SuspendLayout();
                PRTable.SuspendLayout();

                // Init PRTable / status table
                if (FirstPR)
                {
                    FirstPR = false;
                    foreach (Object o in l)
                    {
                        if (j < 7)
                        {
                            j++;
                            continue;
                        }
                        if (o is FieldType)
                        {
                            FieldType f = o as FieldType;
                            if (i != 0)
                            {
                                PRTable.InsertRow(i);
                            }
                            // Add field name to table
                            Label la = new Label();
                            la.Text = f.name;
                            la.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
                            la.Padding = new Padding(3);
                            la.Font = new Font(la.Font.Name, 7,
                                FontStyle.Bold, la.Font.Unit,
                                la.Font.GdiCharSet, la.Font.GdiVerticalFont);
                            la.AutoEllipsis = true;
                            la.MouseDown += new MouseEventHandler(PRTable_MouseDown);
                            PRTable.Controls.Add(la, 0, i);

                            // Add field value label to table
                            la = new Label();
                            la.Text = "";
                            la.Padding = new Padding(3);
                            la.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
                            la.AutoEllipsis = true;
                            la.MouseDown += new MouseEventHandler(PRTable_MouseDown);
                            PRTable.Controls.Add(la, 1, i);
                            // All value labels are put in a list.
                            // The fields will come in the same order every time.
                            PRLabels.Add(la);

                            ToolTip labelsToolTip = new ToolTip(new Container());
                            PRToolTips.Add(labelsToolTip);
                        }
                        else
                            break;
                        i++;
                    }
                }

                i = 0;
                j = 0;
                foreach (Object o in l)
                {
                    if (j < 7)
                    {
                        j++;
                        continue;
                    }
                    // Update each field in PRTable
                    if (o is FieldType)
                    {
                        // If a previous PR has fewer fields than the current,
                        // all fields will not be displayed
                        if (i == PRLabels.Count)
                            continue;

                        FieldType f = o as FieldType;

                        // If a field has more than one value those will be
                        // displayed in a line after each other.
                        if (f.values.Count > 1)
                        {
                            PRLabels[i].Text = "";
                            String Tip = "";
                            foreach (Object ob in f.values)
                            {
                                // 30 characters of every bit description
                                // is displayed if more than one bit is set
                                String bit = ob.ToString();
                                PRLabels[i].Text += ((bit.Length > 30)
                                    ? bit.ToString().Substring(0, 30) + "..." : bit) + ", ";
                                Tip += bit + "\n";
                            }
                            PRToolTips[i].SetToolTip(PRLabels[i], Tip.TrimEnd('\n'));
                            PRLabels[i].Text = PRLabels[i].Text.TrimEnd(',', ' ');
                        }
                        else if (f.values.Count == 1)
                        {
                            if ("B_DIRECTION" == f.name)
                            {
                                String StringCode = f.values[0].ToString();
                                if (String.Compare(StringCode, "A end facing cars ") == 0)
                                {
                                    PRToolTips[i].SetToolTip(PRLabels[i], "Forward, loco closest to leg 1, A end facing cars ");
                                    PRLabels[i].Text = "Forward, loco closest to leg 1, A end facing cars ";
                                }
                                else if (String.Compare(StringCode, "loco closest to leg 0") == 0)
                                {
                                    PRToolTips[i].SetToolTip(PRLabels[i], "Forward, loco closest to leg 0, B end facing cars ");
                                    PRLabels[i].Text = "Forward, loco closest to leg 0, B end facing cars ";
                                }
                                else if (String.Compare(StringCode, "Reverse Direction") == 0)
                                {
                                    PRToolTips[i].SetToolTip(PRLabels[i], "Reverse Direction, loco closest to leg 1, B end facing cars ");
                                    PRLabels[i].Text = "Reverse Direction, loco closest to leg 1, B end facing cars ";
                                }
                            }
                            else
                            {
                                PRToolTips[i].SetToolTip(PRLabels[i], f.values[0].ToString());
                                PRLabels[i].Text = f.values[0].ToString();
                            }
                        }
                        else
                        {
                            PRToolTips[i].SetToolTip(PRLabels[i], "No value for this field");
                            PRLabels[i].Text = "-";
                            if ("B_DIRECTION" == f.name)
                            {
                                PRToolTips[i].SetToolTip(PRLabels[i], "Forward, loco closest to leg 1, B end facing cars ");
                                PRLabels[i].Text = "Forward, loco closest to leg 1, B end facing cars ";
                            }
                        }

                    }
                    // Add error blocks to ErrorView
                    else if (o is BlockType)
                    {
                        BlockType b = o as BlockType;
                        ErrorView.BeginUpdate();
                        AddErrorToList(b, Msg.Time);
                        ErrorView.EndUpdate();
                    }
                    i++;
                }
                PRTable.ResumeLayout();
                PRTable.Parent.ResumeLayout();
            }
        }

        /// <summary>
        /// If the given block is of type PRErrorBlock it
        /// will be displayed in ErrorView
        /// </summary>
        /// <param name="Block"></param>
        /// <param name="Time"></param>
        private void AddErrorToList(BlockType Block, String Time)
        {
            if (Block.name == this.PRErrorBlock)
            {
                String ErrorLevel = ((FieldType)Block.values[0]).values[0].ToString();
                String StringCode = String.Format("0x{0:X}", ((FieldType)Block.values[1]).values[0]);
                String Description;
                try
                {
                    //int Code = ErrorLevelValue[ErrorLevel] + Int32.Parse(StringCode);
                    uint Code = (uint)(((FieldType)Block.values[1]).values[0]);
                    Description = Mdl.GetErrorDescription((int)Code);
                }
                catch
                {
                    Description = "Unknown";
                }
                // Columns are { Time, Level, Code, Description }
                String[] jk = { Time, ErrorLevel, StringCode, Description };
                //                String[] jk = { Time.Substring(0, 11), ErrorLevel, StringCode, Description };
                ListViewItem lvi = new ListViewItem(jk);
                ErrorView.Items.Add(lvi);
                // If autoscroll is enabled it will make sure that the newly added
                // error will be visible.
                if (AutoScrollCheckBox.Checked)
                    ErrorView.EnsureVisible(ErrorView.Items.Count - 1);

                if (this.tabControl1.SelectedTab.Name != "PRErrors")
                {
                    this.ErrorNr++;
                    UpdatePRErrorsText();
                }
            }
        }

        #endregion

        #region GUIEventHandlers

        #region ShowMessage


        /// <summary>
        /// When a selection is made in TransfersView the method will
        /// handle that. It will update MsgInfoTable will all field and
        /// blocks of the selected message and update HexTable with the
        /// raw data of the message
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void TransfersView_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (TransfersView.SelectedItems.Count > 0)
            {
                DeliverEventArgs DE = Mdl.DeliverList[(int)TransfersView.SelectedItems[0].Tag];

                // Update raw data
                UpdateHexTable(DE.Data);
                ArrayList MsgInfo = Message.ParseBytes(DE.Data, DE.Name);

                MsgInfoTable.SuspendLayout();
                // First dispose of all current controls to release memory
                foreach (Control c in MessageControls)
                    c.Dispose();
                MessageControls.Clear();
                // Update MsgInfoTable with correct number of rows and correct Header
                MsgInfoTable.RowCount = MsgInfo.Count + 1;
                InfoHead.Text = DE.Name;
                UpdateMsgInfoTableHeader(MsgInfo.GetRange(0, 7), MsgInfoTable, 1);
                // Now update rest of table
                UpdateMsgInfoTable(MsgInfo.GetRange(7, MsgInfo.Count - 7), MsgInfoTable, 2);
                MsgInfoTable.ResumeLayout();

                // Fixes resize of table
                MsgInfoTable.AutoScroll = false;
                MsgInfoTable.AutoScroll = true;
            }
        }

        private void UpdateMsgInfoTableHeader(ArrayList Values, TableLayoutPanel MainTa, int Index)
        {
            ExpandableLayoutPanel.ExpandableLayoutPanel elp = new ExpandableLayoutPanel.ExpandableLayoutPanel("Header");
            TableLayoutPanel ta = elp.GetTableLayoutPanel();
            MessageControls.Add(elp);
            MainTa.Controls.Add(elp);

            ta.ColumnCount = 2;
            ta.RowCount = Values.Count;
            ta.RowStyles.Add(new System.Windows.Forms.RowStyle(
                        System.Windows.Forms.SizeType.Absolute, 16F));
            ta.AutoSize = true;
            ta.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
            ta.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(
                System.Windows.Forms.SizeType.Absolute, 200F));
            // To make the table mouse-scrollable
            ta.MouseDown += new MouseEventHandler(MsgInfoTable_MouseDown);

            int i = 0;
            foreach (Object o in Values)
            {
                // Since a FieldType can have multiple values each FieldType is given its own
                // table in MainTa. In the left column is the field name and in the right column
                // is the field values. All values are put on a separate row.
                if (o is FieldType)
                {
                    FieldType field = o as FieldType;
                    // Add field name
                    Label l = new Label();
                    l.Text = field.name;
                    l.Font = new Font(l.Font.Name, 7,
                            FontStyle.Bold, l.Font.Unit,
                            l.Font.GdiCharSet, l.Font.GdiVerticalFont);
                    l.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
                    l.AutoEllipsis = true;
                    // To make the table mouse-scrollable
                    l.MouseDown += new MouseEventHandler(MsgInfoTable_MouseDown);
                    MessageControls.Add(l);

                    ta.Controls.Add(l, 0, i);

                    // Add field value

                    l = new Label();
                    l.Text = field.values[0].ToString();
                    l.Font = new Font(l.Font.Name, (float)8.25,
                        FontStyle.Regular, l.Font.Unit,
                        l.Font.GdiCharSet, l.Font.GdiVerticalFont);
                    l.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
                    l.AutoEllipsis = true;
                    // To make the table mouse-scrollable
                    l.MouseDown += new MouseEventHandler(MsgInfoTable_MouseDown);
                    MessageControls.Add(l);
                    ta.Controls.Add(l, 1, i);
                    i++;
                }
            }
        }


        /// <summary>
        /// Disposes of old lables and updates with new ones 
        /// for the raw data table, HexTable.
        /// Prints 10 bytes on every row of the table.
        /// </summary>
        /// <param name="Data"></param>
        private void UpdateHexTable(Byte[] Data)
        {
            String hex = BitConverter.ToString(Data);
            // Dispose of old lables in table
            foreach (Control c in HexTable.Controls)
            {
                c.Dispose();
            }
            if (HexTable.Controls.Count > 0)
                HexTable.Controls.Clear();

            // Update number of rows that the table will need
            HexTable.RowCount = (int)Math.Ceiling(hex.Length / (float)30);
            Label l;

            // Fill all rows
            for (int i = 0; i < HexTable.RowCount; i++)
            {
                // Show byte count
                l = new Label();
                l.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
                l.Text = (i * 10).ToString() + ":";
                // MouseDown event is added to make the table mouse scroll after
                // it has been clicked
                l.MouseDown += new MouseEventHandler(HexTable_MouseDown);
                HexTable.Controls.Add(l, 0, i);


                l = new Label();
                l.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
                // Show 10 bytes
                if (hex.Length > 29)
                {
                    l.Text = hex.Substring(0, 29);
                    hex = hex.Remove(0, 30);
                }
                // or show rest of string (last loop)
                else
                    l.Text = hex;
                // MouseDown event is added to make the table mouse scroll after
                // it has been clicked
                l.MouseDown += new MouseEventHandler(HexTable_MouseDown);
                HexTable.Controls.Add(l, 1, i);
            }
            HexTable.AutoScroll = false;
            HexTable.AutoScroll = true;
        }


        /// <summary>
        /// This is a recursive method.
        /// It takes and ArrayList of values, a table to put the values in and an row index for starting point
        /// The ArrayList is filled with FieldType and BlockType
        /// Each FieldType is put in a cell in the given table.
        /// For each BlockType a GroupBox is created. In the GroupBox a new Table is put. The ArrayList
        /// of BlockType is passed to this method with the new table and and index of 0.
        /// The GroupBox is put in the given table.
        /// </summary>
        /// <param name="Values"></param>
        /// <param name="MainTa"></param>
        /// <param name="Index"></param>
        private void UpdateMsgInfoTable(ArrayList Values, TableLayoutPanel MainTa, int Index)
        {
            int i = Index;
            foreach (Object o in Values)
            {
                // Since a FieldType can have multiple values each FieldType is given its own
                // table in MainTa. In the left column is the field name and in the right column
                // is the field values. All values are put on a separate row.
                if (o is FieldType)
                {
                    // Init new table
                    FieldType field = o as FieldType;
                    TableLayoutPanel ta = new TableLayoutPanel();
                    ta.ColumnCount = 2;
                    ta.RowCount = field.values.Count;
                    for (int k = 0; k <= field.values.Count; k++)
                    {
                        ta.RowStyles.Add(new System.Windows.Forms.RowStyle(
                        System.Windows.Forms.SizeType.Absolute, 16F));
                    }
                    ta.AutoSize = true;
                    ta.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
                    ta.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(
                        System.Windows.Forms.SizeType.Absolute, 200F));
                    // To make the table mouse-scrollable
                    ta.MouseDown += new MouseEventHandler(MsgInfoTable_MouseDown);
                    MessageControls.Add(ta);

                    // Add field name
                    Label l = new Label();
                    l.Text = field.name;
                    l.Font = new Font(l.Font.Name, 7,
                            FontStyle.Bold, l.Font.Unit,
                            l.Font.GdiCharSet, l.Font.GdiVerticalFont);
                    l.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
                    l.AutoEllipsis = true;
                    // To make the table mouse-scrollable
                    l.MouseDown += new MouseEventHandler(MsgInfoTable_MouseDown);
                    MessageControls.Add(l);

                    ta.Controls.Add(l, 0, 0);

                    // Add field value(s)
                    int j = 0;
                    foreach (Object ob in field.values)
                    {
                        l = new Label();
                        l.Text = ob.ToString();
                        l.Font = new Font(l.Font.Name, (float)8.25,
                            FontStyle.Regular, l.Font.Unit,
                            l.Font.GdiCharSet, l.Font.GdiVerticalFont);
                        l.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
                        l.AutoEllipsis = true;
                        // To make the table mouse-scrollable
                        l.MouseDown += new MouseEventHandler(MsgInfoTable_MouseDown);
                        MessageControls.Add(l);
                        ta.Controls.Add(l, 1, j);
                        j++;
                    }
                    MainTa.Controls.Add(ta, 0, i);
                    //i++;
                }
                // Each block is put in a in a table in a GroupBox and this method is called again
                else if (o is BlockType)
                {
                    BlockType b = o as BlockType;

                    // Init GroupBox
                    GroupBox g = new GroupBox();
                    g.Text = b.name;
                    g.Font = new Font(g.Font, FontStyle.Bold);
                    g.Size = new Size(100, 50);
                    g.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
                    g.AutoSize = true;
                    // To make the table mouse-scrollable
                    g.MouseDown += new MouseEventHandler(MsgInfoTable_MouseDown);
                    MessageControls.Add(g);

                    // Init new table
                    TableLayoutPanel ta = new TableLayoutPanel();
                    ta.ColumnCount = 1;
                    ta.RowCount = b.values.Count;
                    for (int k = 0; k <= ta.RowCount; k++)
                    {
                        ta.RowStyles.Add(new System.Windows.Forms.RowStyle(
                        System.Windows.Forms.SizeType.AutoSize));
                    }
                    ta.AutoSize = true;
                    ta.Location = new Point(1, 15);
                    ta.Dock = DockStyle.Fill;
                    // To make the table mouse-scrollable
                    ta.MouseDown += new MouseEventHandler(MsgInfoTable_MouseDown);
                    MessageControls.Add(ta);

                    // Recursive call
                    UpdateMsgInfoTable(b.values, ta, 0);

                    g.Controls.Add(ta);
                    MainTa.Controls.Add(g, 0, i);
                }
                i++; // i should be updated after each iteration. Not just for field types.
            }
        }

        private void tabControl1_Click(object sender, EventArgs e)
        {
            if (this.tabControl1.SelectedTab.Name == "PRErrors")
            {
                this.ErrorNr = 0;
                UpdatePRErrorsText();
            }
        }

        private void UpdatePRErrorsText()
        {
            if (this.ErrorNr == 0)
                this.PRErrors.Text = "PR Errors";
            else
                this.PRErrors.Text = "PR Errors (" + this.ErrorNr + ")";
        }

        #endregion

        #region MessageMenu
        /// <summary>
        /// Handles events from the right click menu, MessageMenu, of MessageBox
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void MessageMenuHandler(Object sender, EventArgs e)
        {
            if (sender is ToolStripMenuItem)
            {
                ToolStripMenuItem t = (ToolStripMenuItem)sender;
                // A file has been selected to be sent.
                if (t.Text == "Send")
                {
                    String f = ((MessageFile)MessageBox.SelectedItem).FilePath;
                    if (f.EndsWith(".xml"))
                    {
                        SendMessageEventArgs ev = new SendMessageEventArgs(f);
                        MVE(ev);
                    }
                }
                // Selected file should be given till to MsgView 
                else if (t.Text == "Edit")
                {
                    String f = ((MessageFile)MessageBox.SelectedItem).FilePath;
                    MVE(new LoadXMLEArgs(f));

                }
                // Selected file should be deleted
                else if (t.Text == "Delete")
                {
                    // First a confirmation dialog
                    String message = MessageBox.SelectedItem.ToString() + " will be deleted.";
                    String caption = "Are you sure?";
                    DialogResult result = System.Windows.Forms.MessageBox.Show(message, caption,
                        MessageBoxButtons.OKCancel, MessageBoxIcon.Warning);
                    if (result == DialogResult.OK)
                    {
                        // Try to delete file
                        try
                        {
                            File.Delete(((MessageFile)MessageBox.SelectedItem).FilePath);
                        }
                        catch (Exception ex)
                        {
                            String ErrorMessage = ex.Message;
                            String ErrorCaption = "Something went wrong";
                            System.Windows.Forms.MessageBox.Show(ErrorMessage, ErrorCaption,
                                MessageBoxButtons.OK, MessageBoxIcon.Error);
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Marks a file in MessagesBox when right mouse button is pressed down
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void MessageBox_MouseDown(object sender, System.Windows.Forms.MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Right)
            {
                int index = MessageBox.IndexFromPoint(e.X, e.Y);
                MessageBox.SelectedIndex = index;
            }
        }

        /// <summary>
        /// Displays MessageMenu when right mouse button is released
        /// and there is a file selected
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void MessageBox_MouseUp(object sender, System.Windows.Forms.MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Right &&
                MessageBox.SelectedIndex != -1 &&
                MessageBox.SelectedItem is MessageFile)
            {
                // Send is grayed if the state is disconnected
                if ((bool)ConnectButton.Tag)
                    MessageMenu.Items[0].Enabled = true;
                else
                    MessageMenu.Items[0].Enabled = false;
                MessageMenu.Show(MousePosition);
            }
        }

        private void MessageBox_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            if (MessageBox.SelectedIndex != -1)
            {
                ListBox listbox = (ListBox)sender;
                if (listbox.Text == "..")
                {
                    messagePath = Directory.GetParent(messagePath).FullName;
                    messagePath.TrimEnd(new char[] { '/' });

                    FileList = Mdl.ChangeWatchDir(messagePath);
                    MessageWatcher.Path = messagePath;
                    pathLabel.Text = messagePath;
                    UpdateMessageBox();
                }
                else if (listbox.Text.EndsWith("/"))
                {
                    messagePath = Path.Combine(messagePath, listbox.Text.TrimEnd(new char[] { '/' }));
                    FileList = Mdl.ChangeWatchDir(messagePath);
                    MessageWatcher.Path = messagePath;
                    pathLabel.Text = messagePath;
                    UpdateMessageBox();
                }
                else
                {
                    String f = ((MessageFile)MessageBox.SelectedItem).FilePath;
                    MVE(new LoadXMLEArgs(f));
                }
            }
        }

        #endregion
        /// <summary>
        /// Handles events from the right click menu, TransferMenu, of TransfersView
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void TransferMenuHandler(Object sender, EventArgs e)
        {
            if (sender is ToolStripMenuItem)
            {
                ToolStripMenuItem t = (ToolStripMenuItem)sender;
                // A transfer has been selected to be viewed.
                if (t.Text == "Open in MessageViewer")
                {
                    if (TransfersView.SelectedItems.Count != 0)
                    {
                        ListViewItem lvi = TransfersView.SelectedItems[0];

                        DeliverEventArgs d = Mdl.DeliverList[(int)lvi.Tag];
                        Byte[] data = d.Data;

                        MVE(new LoadDataEArgs(d.Data));
                    }
                }
            }
        }

        /// <summary>
        /// Marks a file in TransfersView when right mouse button is pressed down
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void TransfersView_MouseDown(object sender, System.Windows.Forms.MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Right)
            {
                //int index = TransfersView.IndexFromPoint(e.X, e.Y);
                //TransfersView.SelectedIndex = index;
            }
        }

        /// <summary>
        /// Displays TransferMenu when right mouse button is released
        /// and there is a file selected
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void TransfersView_MouseUp(object sender, System.Windows.Forms.MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Right && MessageBox.SelectedIndex != -1)
            {
                // Send is grayed if the state is disconnected
                if ((bool)ConnectButton.Tag)
                    MessageMenu.Items[0].Enabled = true;
                else
                    MessageMenu.Items[0].Enabled = false;
                MessageMenu.Show(MousePosition);
            }
        }
        #region TransferMenu

        #endregion

        #region CreateMessage

        /// <summary>
        /// Opens MsgView (TODO)
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void CreateMessageButton_Click(object sender, EventArgs e)
        {
            MVE(new OpenMsgView());
        }
        #endregion

        #region Communication

        /// <summary>
        /// Connect button is clicked.
        /// Event to get config data for ConnectionDialog
        /// or event to disconnect.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void ConnectButton_Click(object sender, EventArgs e)
        {
            if (this.Mdl.ConnectTag)
            {
                ConnectEventArgs CE = new ConnectEventArgs();
                MVE(CE);
                SetPauseStatus();
                SetConnectStatus();
                this.RunScriptButton.Enabled = false;
            }
            else
                MVE(new GetConnectDataEventArgs());
        }

        /// <summary>
        /// Pause button was clicked.
        /// Handles PauseLabel in statusrow and pausebutton
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void PauseButton_Click(object sender, EventArgs e)
        {
            MVE(new PauseEventArgs((bool)PauseButton.Tag));
            SetPauseStatus();

        }

        private void SetPauseStatus()
        {
            this.PauseButton.Text = this.Mdl.PauseText;
            this.PauseButton.Tag = this.Mdl.PauseTag;
            this.PauseButton.Enabled = this.Mdl.PauseEnable;
            this.PauseLabel.Text = this.Mdl.PauseLabel;
        }

        private void SetConnectStatus()
        {
            this.ConnectButton.Text = this.Mdl.ConnectText;
            this.ConnectButton.Tag = this.Mdl.ConnectTag;
            this.ConnectButton.Enabled = this.Mdl.ConnectEnable;
        }

        #endregion

        #region Filter
        /// <summary>
        /// Event that are created by FilterButton is handled here.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void FilterHandler(Object sender, EventArgs e)
        {
            if (sender is ToolStripMenuItem)
            {
                ToolStripMenuItem t = (ToolStripMenuItem)sender;
                t.Checked = !t.Checked;
                // Dictionary FilterMap keeps track of what messages are filtered
                MessageDict[(string)t.Tag].values[0] = !(bool)MessageDict[(string)t.Tag].values[0];
                UpdateTransfersList(false);
            }
        }

        #endregion

        #region TransferButtons


        /// <summary>
        /// Clears the current transfer with a warning befor it's executed.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void ClearTransferButton_Click(object sender, EventArgs e)
        {
            String message = "This will clear the current transfer.";
            String caption = "Are you sure?";
            DialogResult result = System.Windows.Forms.MessageBox.Show(message, caption, MessageBoxButtons.OKCancel, MessageBoxIcon.Warning);
            if (result == DialogResult.OK)
            {
                // Event to tell Model to clear DeliverList
                MVE(new ClearDeliverListEventArgs());
                // Clear ListView with transfers
                TransfersView.Items.Clear();
                // Clear ListView with errors from PR
                ErrorView.Items.Clear();
                // Reset PRTable labels
                foreach (Label l in PRLabels)
                {
                    l.Text = "";
                }
                // Reset counter on PRR errors tab.
                ErrorNr = 0;
                UpdatePRErrorsText();
            }
        }

        /// <summary>
        /// Saves current transfer
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void SaveTransferButton_Click(object sender, EventArgs e)
        {
            DialogResult Result = SaveDialog.ShowDialog();
            if (Result == DialogResult.OK)
            {
                // Save current transfer event
                MVE(new SaveTransferEventArgs(SaveDialog.FileName));
            }
        }

        /// <summary>
        /// Loads a transfer from a file
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void LoadTransferButton_Click(object sender, EventArgs e)
        {
            DialogResult Result = LoadDialog.ShowDialog();
            if (Result == DialogResult.OK)
            {
                // Reset counter on PRR errors tab.
                ErrorNr = 0;
                UpdatePRErrorsText();
                // Event to load a selected file
                MVE(new LoadTransferEventArgs(LoadDialog.FileName));
            }
        }

        #endregion

        #region InjectButtons

        /// <summary>
        /// Send next message to wrong train ID
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void InjectIDButton_Click(object sender, EventArgs e)
        {
            FaultInjectionEventArgs FI = new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.ID);
            MVE(FI);
        }

        /// <summary>
        /// Send next message to wrong site ID
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void InjectSiteIDButton_Click(object sender, EventArgs e)
        {
            FaultInjectionEventArgs FI = new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.siteID);
            MVE(FI);
        }

        /// <summary>
        /// Send next message to wrong Region ID
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void InjectRegionIDButton_Click(object sender, EventArgs e)
        {
            FaultInjectionEventArgs FI = new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.regionID);
            MVE(FI);
        }



        /// <summary>
        /// Send next message with CRC error
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void InjectCRCButton_Click(object sender, EventArgs e)
        {
            FaultInjectionEventArgs FI = new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.CRC);
            MVE(FI);
        }

        private void reuseToolStripMenuItem_Click(object sender, EventArgs e)
        {
            FaultInjectionEventArgs FI = new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.TSSenderReuse);
            MVE(FI);
        }

        private void decreaseTimeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            FaultInjectionEventArgs FI = new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.TSSenderDec);
            MVE(FI);
        }

        private void decreasedToolStripMenuItem_Click(object sender, EventArgs e)
        {
            FaultInjectionEventArgs FI = new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.TSRefDec);
            MVE(FI);
        }

        private void increasedToolStripMenuItem_Click(object sender, EventArgs e)
        {
            FaultInjectionEventArgs FI = new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.TSRefInc);
            MVE(FI);
        }

        private void msDelayToolStripMenuItem_Click(object sender, EventArgs e)
        {
            FaultInjectionEventArgs FI = new FaultInjectionEventArgs(FaultInjectionEventArgs.InjectionType.Fragment);
            FI.FragmentDelay = Int32.Parse(((ToolStripMenuItem)sender).Tag.ToString());
            MVE(FI);
        }

        #endregion

        #region FocusTables

        /// <summary>
        /// When MsgInfoTable or any of its controls is clicked
        /// it will be focused ie mouse scroll will be activated
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void MsgInfoTable_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                MsgInfoTable.Focus();
            }
        }

        /// <summary>
        /// When HexTable or any of its controls is clicked
        /// it will be focused ie mouse scroll will be activated
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void HexTable_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                HexTable.Focus();
            }
        }

        /// <summary>
        /// When PRTable or any of its controls is clicked
        /// it will be focused ie mouse scroll will be activated
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void PRTable_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                PRTable.Focus();
            }
        }

        #endregion

        #endregion

        #region FileEvents

        /// <summary>
        /// Updates FileList in the event of create/delete/rename
        /// of any .XML-files in the Messages folder.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void MessageWatcherHandler(object sender, FileSystemEventArgs e)
        {
            switch (e.ChangeType)
            {
                case WatcherChangeTypes.Created:
                    if (!this.FileList.Contains(new MessageFile(e.FullPath)))
                    {
                        this.FileList.Add(new MessageFile(e.FullPath));
                        FileList.Sort();
                    }
                    break;
                case WatcherChangeTypes.Deleted:
                    if (this.FileList.Contains(new MessageFile(e.FullPath)))
                    {
                        this.FileList.Remove(new MessageFile(e.FullPath));
                    }
                    break;
                case WatcherChangeTypes.Renamed:
                    RenamedEventArgs ev = (RenamedEventArgs)e;
                    if (this.FileList.Contains(new MessageFile(ev.OldFullPath)))
                    {
                        this.FileList.Remove(new MessageFile(ev.OldFullPath));
                        this.FileList.Add(new MessageFile(ev.FullPath));
                        FileList.Sort();
                    }
                    break;
            }
            UpdateMessageBox();
        }

        /// <summary>
        /// Updates MessageBox
        /// </summary>
        private void UpdateMessageBox()
        {
            MessageBox.Items.Clear();
            MessageBox.Items.Add("..");
            try
            {
                foreach (String d in Directory.GetDirectories(messagePath))
                {
                    String name = Path.GetFileName(d) + "/";
                    MessageBox.Items.Add(name);
                }

                foreach (MessageFile m in this.FileList)
                {
                    MessageBox.Items.Add(m);
                }
            }
            catch (Exception) { }
        }

        private void MessageBox_DrawItem(object sender, DrawItemEventArgs e)
        {
            e.DrawBackground();
            if (e.Index != -1)
            {
                if (MessageBox.Items[e.Index].ToString().EndsWith(".xml"))
                {
                    e.Graphics.DrawString(MessageBox.Items[e.Index].ToString(), new Font("Microsoft Sans Serif", 8.25f), Brushes.Black, e.Bounds);
                }
                else
                {
                    e.Graphics.DrawString(MessageBox.Items[e.Index].ToString(), new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold), Brushes.Black, e.Bounds);
                }
            }
            e.DrawFocusRectangle();
        }

        #endregion

        #region Script

        private void RunScriptButton_Click(object sender, EventArgs e)
        {
            if (!(bool)this.RunScriptButton.Tag)
            {
                DialogResult Result = RunScriptDialog.ShowDialog();
                if (Result == DialogResult.OK)
                {
                    // Save current transfer event
                    this.RunScriptButton.Tag = true;
                    this.RunScriptButton.Text = "Cancel Script";
                    MVE(new RunScriptEventArgs(RunScriptDialog.FileName));
                    scriptNameLabel.Text = System.IO.Path.GetFileNameWithoutExtension(RunScriptDialog.FileName);
                    Text = TCCSimTitle + " - " + System.IO.Path.GetFileNameWithoutExtension(RunScriptDialog.FileName);
                    DisableButtonsScript();
                }
            }
            else
            {
                this.RunScriptButton.Tag = false;
                this.RunScriptButton.Enabled = false;
                MVE(new RunScriptEventArgs());
            }
        }

        private void DisableButtonsScript()
        {
            this.ConnectButton.Enabled = false;
            this.PauseButton.Enabled = false;
            this.TransferButton.Enabled = false;
            this.FaultInjectionButton.Enabled = false;
        }

        private void EnableButtonsScript()
        {
            this.ConnectButton.Enabled = true;
            this.PauseButton.Enabled = true;
            this.TransferButton.Enabled = true;
            this.FaultInjectionButton.Enabled = true;
            scriptNameLabel.Text = "";
            Text = TCCSimTitle;
        }

        private void ClearScript_Click(object sender, EventArgs e)
        {
            ScriptOutput.Text = "";
        }
        #endregion

        private void MainView_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Save filter settings
            MVE(new SaveFilterEventArgs(MessageDict));

            Properties.Settings.Default.SplitDist = splitContainer.SplitterDistance;
            // Set windows size, location and state
            Properties.Settings.Default.State = this.WindowState;
            if (this.WindowState == FormWindowState.Normal)
                Properties.Settings.Default.Bounds = this.Bounds;
            Properties.Settings.Default.Save();
        }


        private void comboBox1_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (comboBoxSetupDone)
            {
                ComboBox c = (ComboBox)sender;
                DriveInfo d = (DriveInfo)c.SelectedItem;
                messagePath = Path.GetFullPath(c.Text);

                FileList = Mdl.ChangeWatchDir(messagePath);
                MessageWatcher.Path = messagePath;
                pathLabel.Text = messagePath;
                UpdateMessageBox();
            }
        }

        private void TransfersView_DoubleClick(object sender, EventArgs e)
        {
            ListView l = (ListView)sender;
            if (l.SelectedItems != null)
            {

            }
        }

        private void buttonSetupToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (BtnV.IsDisposed)
            {
                BtnV = new ButtonView(ButtonSets, ButtonSetPath);
                BtnV.buttonSetChanged += new ButtonView.ButtonSetsChanged(BtnV_buttonSetChanged);
            }
            BtnV.Show(this);
        }

        void BtnV_buttonSetChanged(ButtonSetEventArgs e)
        {
            this.ButtonSets = e.buttonSets;
            int index = ButtonSetBox.SelectedIndex;

            ButtonSetBox.Items.Clear();
            if (ButtonSets.Count > 0)
            {
                for (int i = 0; i < ButtonSets.Count; i++)
                {
                    ButtonSetBox.Items.Add(ButtonSets[i][0]);
                }
                ButtonSetBox.SelectedIndex = index < ButtonSetBox.Items.Count ? index : 0;
            }
        }

        private void ButtonSetBox_SelectedIndexChanged(object sender, EventArgs e)
        {
            activeButtonSet = ButtonSets[ButtonSetBox.SelectedIndex];
            for (int i = 1; i < activeButtonSet.Length; i++)
            {
                MacroButtons[i - 1].Text = activeButtonSet[i].Name;
                MacroButtons[i - 1].Visible = !(activeButtonSet[i].Name == "");
                MacroButtons[i - 1].BackColor = Form.DefaultBackColor;
                if (activeButtonSet[i].Data.EndsWith(".py"))
                {
                    MacroButtons[i - 1].BackColor = Color.FromArgb(194, 214, 154);
                }
            }
        }

        private void MacroButtonClicked(object sender, EventArgs e)
        {
            Button b = (Button)sender;
            int index = MacroButtons.IndexOf(b);
            ButtonConfiguration bConf = activeButtonSet[index + 1];
            String path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, bConf.Data);
            if (path.Length > 0)
            {
                if (path.EndsWith(".xml"))
                {
                    MVE(new SendMessageEventArgs(path));
                }
                else if (path.EndsWith(".py"))
                {
                    this.RunScriptButton.Tag = true;
                    this.RunScriptButton.Text = "Cancel Script";
                    MVE(new RunScriptEventArgs(path));
                    scriptNameLabel.Text = Path.GetFileNameWithoutExtension(path);
                    Text = TCCSimTitle + " - " + Path.GetFileNameWithoutExtension(path);
                    DisableButtonsScript();
                }
            }
        }

        private void MainView_KeyUp(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.F1:
                    MacroButtons[0].PerformClick();
                    break;
                case Keys.F2:
                    MacroButtons[1].PerformClick();
                    break;
                case Keys.F3:
                    MacroButtons[2].PerformClick();
                    break;
                case Keys.F4:
                    MacroButtons[3].PerformClick();
                    break;
                case Keys.F5:
                    MacroButtons[4].PerformClick();
                    break;
                case Keys.F6:
                    MacroButtons[5].PerformClick();
                    break;
                case Keys.F7:
                    MacroButtons[6].PerformClick();
                    break;
                case Keys.F8:
                    MacroButtons[7].PerformClick();
                    break;
                case Keys.F9:
                    MacroButtons[8].PerformClick();
                    break;
                case Keys.F12:
                    ButtonSetBox.SelectedIndex = (ButtonSetBox.SelectedIndex + 1) % ButtonSetBox.Items.Count;
                    break;
                default:
                    break;
            }
        }

    }
}
