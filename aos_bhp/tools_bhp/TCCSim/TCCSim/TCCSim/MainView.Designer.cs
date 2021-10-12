using System.IO;
using System.Windows.Forms;
namespace TCCSim
{
    partial class MainView
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainView));
            this.MessageContainer = new System.Windows.Forms.GroupBox();
            this.pathLabel = new System.Windows.Forms.Label();
            this.comboBox1 = new System.Windows.Forms.ComboBox();
            this.MessageBox = new System.Windows.Forms.ListBox();
            this.toolStrip1 = new System.Windows.Forms.ToolStrip();
            this.ConnectButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator4 = new System.Windows.Forms.ToolStripSeparator();
            this.PauseButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.FilterButton = new System.Windows.Forms.ToolStripDropDownButton();
            this.CreateMessageButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.TransferButton = new System.Windows.Forms.ToolStripDropDownButton();
            this.ClearTransferButton = new System.Windows.Forms.ToolStripMenuItem();
            this.SaveTransferButton = new System.Windows.Forms.ToolStripMenuItem();
            this.LoadTransferButton = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator6 = new System.Windows.Forms.ToolStripSeparator();
            this.toolStripSeparator8 = new System.Windows.Forms.ToolStripSeparator();
            this.FaultInjectionButton = new System.Windows.Forms.ToolStripDropDownButton();
            this.InjectIDButton = new System.Windows.Forms.ToolStripMenuItem();
            this.InjectSiteIDButton = new System.Windows.Forms.ToolStripMenuItem();
            this.InjectRegionIDButton = new System.Windows.Forms.ToolStripMenuItem();
            this.InjectCRCButton = new System.Windows.Forms.ToolStripMenuItem();
            this.InjectTSButton = new System.Windows.Forms.ToolStripMenuItem();
            this.reuseToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.decreaseTimeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.referenceTimestampToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.decreasedToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.increasedToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.fragmentMessageWithToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.msDelayToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.msDelayToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.msDelayToolStripMenuItem2 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
            this.RunScriptButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator5 = new System.Windows.Forms.ToolStripSeparator();
            this.ButtonsButton = new System.Windows.Forms.ToolStripDropDownButton();
            this.buttonSetupToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator7 = new System.Windows.Forms.ToolStripSeparator();
            this.toolStripSeparator9 = new System.Windows.Forms.ToolStripSeparator();
            this.MessageWatcher = new System.IO.FileSystemWatcher();
            this.MessageMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.StatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.PauseLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.TrainIDStatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.SiteIDStatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.RegionIDStatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.CrcStatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.IPStatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.PollingStatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.TimeoutStatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.scriptNameLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.SaveDialog = new System.Windows.Forms.SaveFileDialog();
            this.LoadDialog = new System.Windows.Forms.OpenFileDialog();
            this.RunScriptDialog = new System.Windows.Forms.OpenFileDialog();
            this.ButtonSetMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.MacroButtonPanel = new System.Windows.Forms.TableLayoutPanel();
            this.ButtonSetBox = new System.Windows.Forms.ComboBox();
            this.splitContainer = new System.Windows.Forms.SplitContainer();
            this.TransfersContainer = new System.Windows.Forms.GroupBox();
            this.TransfersView = new System.Windows.Forms.ListView();
            this.Time = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.From = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.To = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.MessageType = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.Comments = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.TransferMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.InfoContainer = new System.Windows.Forms.GroupBox();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.PRInfo = new System.Windows.Forms.TabPage();
            this.PRTable = new System.Windows.Forms.TableLayoutPanel();
            this.PRErrors = new System.Windows.Forms.TabPage();
            this.ErrorView = new System.Windows.Forms.ListView();
            this.ETime = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ErrorLevel = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ErrorNumber = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ErrorDescription = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ReadableTab = new System.Windows.Forms.TabPage();
            this.MsgInfoTable = new System.Windows.Forms.TableLayoutPanel();
            this.InfoHead = new System.Windows.Forms.Label();
            this.RawTab = new System.Windows.Forms.TabPage();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.HexLabel = new System.Windows.Forms.Label();
            this.HexTable = new System.Windows.Forms.TableLayoutPanel();
            this.ScriptTab = new System.Windows.Forms.TabPage();
            this.ClearScript = new System.Windows.Forms.Button();
            this.ScriptOutput = new System.Windows.Forms.TextBox();
            this.MessageContainer.SuspendLayout();
            this.toolStrip1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.MessageWatcher)).BeginInit();
            this.statusStrip1.SuspendLayout();
            this.MacroButtonPanel.SuspendLayout();
            this.splitContainer.Panel1.SuspendLayout();
            this.splitContainer.Panel2.SuspendLayout();
            this.splitContainer.SuspendLayout();
            this.TransfersContainer.SuspendLayout();
            this.InfoContainer.SuspendLayout();
            this.tabControl1.SuspendLayout();
            this.PRInfo.SuspendLayout();
            this.PRErrors.SuspendLayout();
            this.ReadableTab.SuspendLayout();
            this.MsgInfoTable.SuspendLayout();
            this.RawTab.SuspendLayout();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.ScriptTab.SuspendLayout();
            this.SuspendLayout();
            // 
            // MessageContainer
            // 
            this.MessageContainer.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.MessageContainer.Controls.Add(this.pathLabel);
            this.MessageContainer.Controls.Add(this.comboBox1);
            this.MessageContainer.Controls.Add(this.MessageBox);
            this.MessageContainer.Location = new System.Drawing.Point(646, 28);
            this.MessageContainer.Name = "MessageContainer";
            this.MessageContainer.Size = new System.Drawing.Size(241, 452);
            this.MessageContainer.TabIndex = 0;
            this.MessageContainer.TabStop = false;
            this.MessageContainer.Text = "Saved Messages";
            // 
            // pathLabel
            // 
            this.pathLabel.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.pathLabel.AutoEllipsis = true;
            this.pathLabel.Location = new System.Drawing.Point(56, 23);
            this.pathLabel.Name = "pathLabel";
            this.pathLabel.Size = new System.Drawing.Size(177, 13);
            this.pathLabel.TabIndex = 12;
            this.pathLabel.Text = "Path";
            // 
            // comboBox1
            // 
            this.comboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBox1.FormattingEnabled = true;
            this.comboBox1.Location = new System.Drawing.Point(6, 19);
            this.comboBox1.Name = "comboBox1";
            this.comboBox1.Size = new System.Drawing.Size(44, 21);
            this.comboBox1.TabIndex = 11;
            this.comboBox1.SelectedIndexChanged += new System.EventHandler(this.comboBox1_SelectedIndexChanged);
            // 
            // MessageBox
            // 
            this.MessageBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.MessageBox.FormattingEnabled = true;
            this.MessageBox.Location = new System.Drawing.Point(8, 54);
            this.MessageBox.Name = "MessageBox";
            this.MessageBox.Size = new System.Drawing.Size(225, 381);
            this.MessageBox.TabIndex = 10;
            this.MessageBox.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.MessageBox_MouseDoubleClick);
            this.MessageBox.MouseDown += new System.Windows.Forms.MouseEventHandler(this.MessageBox_MouseDown);
            this.MessageBox.MouseUp += new System.Windows.Forms.MouseEventHandler(this.MessageBox_MouseUp);
            // 
            // toolStrip1
            // 
            this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ConnectButton,
            this.toolStripSeparator4,
            this.PauseButton,
            this.toolStripSeparator1,
            this.FilterButton,
            this.CreateMessageButton,
            this.toolStripSeparator2,
            this.TransferButton,
            this.toolStripSeparator6,
            this.toolStripSeparator8,
            this.FaultInjectionButton,
            this.toolStripSeparator3,
            this.RunScriptButton,
            this.toolStripSeparator5,
            this.ButtonsButton,
            this.toolStripSeparator9});
            this.toolStrip1.Location = new System.Drawing.Point(0, 0);
            this.toolStrip1.Name = "toolStrip1";
            this.toolStrip1.Size = new System.Drawing.Size(899, 25);
            this.toolStrip1.TabIndex = 0;
            this.toolStrip1.TabStop = true;
            this.toolStrip1.Text = "toolStrip1";
            // 
            // ConnectButton
            // 
            this.ConnectButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.ConnectButton.Image = ((System.Drawing.Image)(resources.GetObject("ConnectButton.Image")));
            this.ConnectButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.ConnectButton.Name = "ConnectButton";
            this.ConnectButton.Size = new System.Drawing.Size(56, 22);
            this.ConnectButton.Text = "Connect";
            this.ConnectButton.Click += new System.EventHandler(this.ConnectButton_Click);
            // 
            // toolStripSeparator4
            // 
            this.toolStripSeparator4.Name = "toolStripSeparator4";
            this.toolStripSeparator4.Size = new System.Drawing.Size(6, 25);
            // 
            // PauseButton
            // 
            this.PauseButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.PauseButton.Enabled = false;
            this.PauseButton.Image = ((System.Drawing.Image)(resources.GetObject("PauseButton.Image")));
            this.PauseButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.PauseButton.Name = "PauseButton";
            this.PauseButton.Size = new System.Drawing.Size(42, 22);
            this.PauseButton.Text = "Pause";
            this.PauseButton.Click += new System.EventHandler(this.PauseButton_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 25);
            // 
            // FilterButton
            // 
            this.FilterButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.FilterButton.Image = ((System.Drawing.Image)(resources.GetObject("FilterButton.Image")));
            this.FilterButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.FilterButton.Name = "FilterButton";
            this.FilterButton.Size = new System.Drawing.Size(100, 22);
            this.FilterButton.Text = "Filter Messages";
            // 
            // CreateMessageButton
            // 
            this.CreateMessageButton.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right;
            this.CreateMessageButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.CreateMessageButton.Image = ((System.Drawing.Image)(resources.GetObject("CreateMessageButton.Image")));
            this.CreateMessageButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.CreateMessageButton.Name = "CreateMessageButton";
            this.CreateMessageButton.Size = new System.Drawing.Size(94, 22);
            this.CreateMessageButton.Text = "Create Message";
            this.CreateMessageButton.Click += new System.EventHandler(this.CreateMessageButton_Click);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(6, 25);
            // 
            // TransferButton
            // 
            this.TransferButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.TransferButton.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ClearTransferButton,
            this.SaveTransferButton,
            this.LoadTransferButton});
            this.TransferButton.Image = ((System.Drawing.Image)(resources.GetObject("TransferButton.Image")));
            this.TransferButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.TransferButton.Name = "TransferButton";
            this.TransferButton.Size = new System.Drawing.Size(108, 22);
            this.TransferButton.Text = "Transfer Options";
            // 
            // ClearTransferButton
            // 
            this.ClearTransferButton.Name = "ClearTransferButton";
            this.ClearTransferButton.Size = new System.Drawing.Size(147, 22);
            this.ClearTransferButton.Text = "Clear Transfer";
            this.ClearTransferButton.Click += new System.EventHandler(this.ClearTransferButton_Click);
            // 
            // SaveTransferButton
            // 
            this.SaveTransferButton.Name = "SaveTransferButton";
            this.SaveTransferButton.Size = new System.Drawing.Size(147, 22);
            this.SaveTransferButton.Text = "Save Transfer";
            this.SaveTransferButton.Click += new System.EventHandler(this.SaveTransferButton_Click);
            // 
            // LoadTransferButton
            // 
            this.LoadTransferButton.Name = "LoadTransferButton";
            this.LoadTransferButton.Size = new System.Drawing.Size(147, 22);
            this.LoadTransferButton.Text = "Load Transfer";
            this.LoadTransferButton.Click += new System.EventHandler(this.LoadTransferButton_Click);
            // 
            // toolStripSeparator6
            // 
            this.toolStripSeparator6.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right;
            this.toolStripSeparator6.Name = "toolStripSeparator6";
            this.toolStripSeparator6.Size = new System.Drawing.Size(6, 25);
            // 
            // toolStripSeparator8
            // 
            this.toolStripSeparator8.Name = "toolStripSeparator8";
            this.toolStripSeparator8.Size = new System.Drawing.Size(6, 25);
            // 
            // FaultInjectionButton
            // 
            this.FaultInjectionButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.FaultInjectionButton.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.InjectIDButton,
            this.InjectSiteIDButton,
            this.InjectRegionIDButton,
            this.InjectCRCButton,
            this.InjectTSButton,
            this.referenceTimestampToolStripMenuItem,
            this.fragmentMessageWithToolStripMenuItem});
            this.FaultInjectionButton.Image = ((System.Drawing.Image)(resources.GetObject("FaultInjectionButton.Image")));
            this.FaultInjectionButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.FaultInjectionButton.Name = "FaultInjectionButton";
            this.FaultInjectionButton.Size = new System.Drawing.Size(95, 22);
            this.FaultInjectionButton.Text = "Fault Injection";
            // 
            // InjectIDButton
            // 
            this.InjectIDButton.Name = "InjectIDButton";
            this.InjectIDButton.Size = new System.Drawing.Size(202, 22);
            this.InjectIDButton.Text = "Wrong Train ID";
            this.InjectIDButton.Click += new System.EventHandler(this.InjectIDButton_Click);
            // 
            // InjectSiteIDButton
            // 
            this.InjectSiteIDButton.Name = "InjectSiteIDButton";
            this.InjectSiteIDButton.Size = new System.Drawing.Size(202, 22);
            this.InjectSiteIDButton.Text = "Wrong Site ID";
            this.InjectSiteIDButton.Click += new System.EventHandler(this.InjectSiteIDButton_Click);
            // 
            // InjectRegionIDButton
            // 
            this.InjectRegionIDButton.Name = "InjectRegionIDButton";
            this.InjectRegionIDButton.Size = new System.Drawing.Size(202, 22);
            this.InjectRegionIDButton.Text = "Wrong Region ID";
            this.InjectRegionIDButton.Click += new System.EventHandler(this.InjectRegionIDButton_Click);
            // 
            // InjectCRCButton
            // 
            this.InjectCRCButton.Name = "InjectCRCButton";
            this.InjectCRCButton.Size = new System.Drawing.Size(202, 22);
            this.InjectCRCButton.Text = "Faulty CRC";
            this.InjectCRCButton.Click += new System.EventHandler(this.InjectCRCButton_Click);
            // 
            // InjectTSButton
            // 
            this.InjectTSButton.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.reuseToolStripMenuItem,
            this.decreaseTimeToolStripMenuItem});
            this.InjectTSButton.Name = "InjectTSButton";
            this.InjectTSButton.Size = new System.Drawing.Size(202, 22);
            this.InjectTSButton.Text = "Timestamp";
            // 
            // reuseToolStripMenuItem
            // 
            this.reuseToolStripMenuItem.Name = "reuseToolStripMenuItem";
            this.reuseToolStripMenuItem.Size = new System.Drawing.Size(148, 22);
            this.reuseToolStripMenuItem.Text = "Reuse";
            this.reuseToolStripMenuItem.Click += new System.EventHandler(this.reuseToolStripMenuItem_Click);
            // 
            // decreaseTimeToolStripMenuItem
            // 
            this.decreaseTimeToolStripMenuItem.Name = "decreaseTimeToolStripMenuItem";
            this.decreaseTimeToolStripMenuItem.Size = new System.Drawing.Size(148, 22);
            this.decreaseTimeToolStripMenuItem.Text = "Decrease time";
            this.decreaseTimeToolStripMenuItem.Click += new System.EventHandler(this.decreaseTimeToolStripMenuItem_Click);
            // 
            // referenceTimestampToolStripMenuItem
            // 
            this.referenceTimestampToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.decreasedToolStripMenuItem,
            this.increasedToolStripMenuItem});
            this.referenceTimestampToolStripMenuItem.Name = "referenceTimestampToolStripMenuItem";
            this.referenceTimestampToolStripMenuItem.Size = new System.Drawing.Size(202, 22);
            this.referenceTimestampToolStripMenuItem.Text = "Reference Timestamp";
            // 
            // decreasedToolStripMenuItem
            // 
            this.decreasedToolStripMenuItem.Name = "decreasedToolStripMenuItem";
            this.decreasedToolStripMenuItem.Size = new System.Drawing.Size(128, 22);
            this.decreasedToolStripMenuItem.Text = "Decreased";
            this.decreasedToolStripMenuItem.Click += new System.EventHandler(this.decreasedToolStripMenuItem_Click);
            // 
            // increasedToolStripMenuItem
            // 
            this.increasedToolStripMenuItem.Name = "increasedToolStripMenuItem";
            this.increasedToolStripMenuItem.Size = new System.Drawing.Size(128, 22);
            this.increasedToolStripMenuItem.Text = "Increased";
            this.increasedToolStripMenuItem.Click += new System.EventHandler(this.increasedToolStripMenuItem_Click);
            // 
            // fragmentMessageWithToolStripMenuItem
            // 
            this.fragmentMessageWithToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.msDelayToolStripMenuItem,
            this.msDelayToolStripMenuItem1,
            this.msDelayToolStripMenuItem2});
            this.fragmentMessageWithToolStripMenuItem.Name = "fragmentMessageWithToolStripMenuItem";
            this.fragmentMessageWithToolStripMenuItem.Size = new System.Drawing.Size(202, 22);
            this.fragmentMessageWithToolStripMenuItem.Text = "Fragment Message With";
            // 
            // msDelayToolStripMenuItem
            // 
            this.msDelayToolStripMenuItem.Name = "msDelayToolStripMenuItem";
            this.msDelayToolStripMenuItem.Size = new System.Drawing.Size(143, 22);
            this.msDelayToolStripMenuItem.Tag = "100";
            this.msDelayToolStripMenuItem.Text = "100 ms Delay";
            this.msDelayToolStripMenuItem.Click += new System.EventHandler(this.msDelayToolStripMenuItem_Click);
            // 
            // msDelayToolStripMenuItem1
            // 
            this.msDelayToolStripMenuItem1.Name = "msDelayToolStripMenuItem1";
            this.msDelayToolStripMenuItem1.Size = new System.Drawing.Size(143, 22);
            this.msDelayToolStripMenuItem1.Tag = "500";
            this.msDelayToolStripMenuItem1.Text = "500 ms Delay";
            this.msDelayToolStripMenuItem1.Click += new System.EventHandler(this.msDelayToolStripMenuItem_Click);
            // 
            // msDelayToolStripMenuItem2
            // 
            this.msDelayToolStripMenuItem2.Name = "msDelayToolStripMenuItem2";
            this.msDelayToolStripMenuItem2.Size = new System.Drawing.Size(143, 22);
            this.msDelayToolStripMenuItem2.Tag = "700";
            this.msDelayToolStripMenuItem2.Text = "700 ms Delay";
            this.msDelayToolStripMenuItem2.Click += new System.EventHandler(this.msDelayToolStripMenuItem_Click);
            // 
            // toolStripSeparator3
            // 
            this.toolStripSeparator3.Name = "toolStripSeparator3";
            this.toolStripSeparator3.Size = new System.Drawing.Size(6, 25);
            // 
            // RunScriptButton
            // 
            this.RunScriptButton.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right;
            this.RunScriptButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.RunScriptButton.Image = ((System.Drawing.Image)(resources.GetObject("RunScriptButton.Image")));
            this.RunScriptButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.RunScriptButton.Name = "RunScriptButton";
            this.RunScriptButton.Size = new System.Drawing.Size(65, 22);
            this.RunScriptButton.Text = "Run Script";
            this.RunScriptButton.Click += new System.EventHandler(this.RunScriptButton_Click);
            // 
            // toolStripSeparator5
            // 
            this.toolStripSeparator5.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right;
            this.toolStripSeparator5.Name = "toolStripSeparator5";
            this.toolStripSeparator5.Size = new System.Drawing.Size(6, 25);
            // 
            // ButtonsButton
            // 
            this.ButtonsButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.ButtonsButton.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.buttonSetupToolStripMenuItem,
            this.toolStripSeparator7});
            this.ButtonsButton.Image = ((System.Drawing.Image)(resources.GetObject("ButtonsButton.Image")));
            this.ButtonsButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.ButtonsButton.Name = "ButtonsButton";
            this.ButtonsButton.Size = new System.Drawing.Size(61, 22);
            this.ButtonsButton.Text = "Buttons";
            // 
            // buttonSetupToolStripMenuItem
            // 
            this.buttonSetupToolStripMenuItem.Name = "buttonSetupToolStripMenuItem";
            this.buttonSetupToolStripMenuItem.Size = new System.Drawing.Size(152, 22);
            this.buttonSetupToolStripMenuItem.Text = "Button Setup...";
            this.buttonSetupToolStripMenuItem.Click += new System.EventHandler(this.buttonSetupToolStripMenuItem_Click);
            // 
            // toolStripSeparator7
            // 
            this.toolStripSeparator7.Name = "toolStripSeparator7";
            this.toolStripSeparator7.Size = new System.Drawing.Size(149, 6);
            // 
            // toolStripSeparator9
            // 
            this.toolStripSeparator9.Name = "toolStripSeparator9";
            this.toolStripSeparator9.Size = new System.Drawing.Size(6, 25);
            // 
            // MessageWatcher
            // 
            this.MessageWatcher.EnableRaisingEvents = true;
            this.MessageWatcher.Filter = "*.xml";
            this.MessageWatcher.SynchronizingObject = this;
            this.MessageWatcher.Created += new System.IO.FileSystemEventHandler(this.MessageWatcherHandler);
            this.MessageWatcher.Deleted += new System.IO.FileSystemEventHandler(this.MessageWatcherHandler);
            // 
            // MessageMenu
            // 
            this.MessageMenu.Name = "MessageMenu";
            this.MessageMenu.Size = new System.Drawing.Size(61, 4);
            // 
            // statusStrip1
            // 
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.StatusLabel,
            this.PauseLabel,
            this.TrainIDStatusLabel,
            this.SiteIDStatusLabel,
            this.RegionIDStatusLabel,
            this.CrcStatusLabel,
            this.IPStatusLabel,
            this.PollingStatusLabel,
            this.TimeoutStatusLabel,
            this.scriptNameLabel});
            this.statusStrip1.Location = new System.Drawing.Point(0, 530);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(899, 22);
            this.statusStrip1.TabIndex = 0;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // StatusLabel
            // 
            this.StatusLabel.Image = global::TCCSim.Properties.Resources.disconnected;
            this.StatusLabel.Name = "StatusLabel";
            this.StatusLabel.Size = new System.Drawing.Size(95, 17);
            this.StatusLabel.Text = "Disconnected";
            // 
            // PauseLabel
            // 
            this.PauseLabel.Name = "PauseLabel";
            this.PauseLabel.Size = new System.Drawing.Size(0, 17);
            // 
            // TrainIDStatusLabel
            // 
            this.TrainIDStatusLabel.BorderSides = System.Windows.Forms.ToolStripStatusLabelBorderSides.Left;
            this.TrainIDStatusLabel.Name = "TrainIDStatusLabel";
            this.TrainIDStatusLabel.Size = new System.Drawing.Size(4, 17);
            // 
            // SiteIDStatusLabel
            // 
            this.SiteIDStatusLabel.BorderSides = System.Windows.Forms.ToolStripStatusLabelBorderSides.Left;
            this.SiteIDStatusLabel.Name = "SiteIDStatusLabel";
            this.SiteIDStatusLabel.Size = new System.Drawing.Size(4, 17);
            // 
            // RegionIDStatusLabel
            // 
            this.RegionIDStatusLabel.BorderSides = System.Windows.Forms.ToolStripStatusLabelBorderSides.Left;
            this.RegionIDStatusLabel.Name = "RegionIDStatusLabel";
            this.RegionIDStatusLabel.Size = new System.Drawing.Size(4, 17);
            // 
            // CrcStatusLabel
            // 
            this.CrcStatusLabel.BorderSides = System.Windows.Forms.ToolStripStatusLabelBorderSides.Left;
            this.CrcStatusLabel.Name = "CrcStatusLabel";
            this.CrcStatusLabel.Size = new System.Drawing.Size(4, 17);
            // 
            // IPStatusLabel
            // 
            this.IPStatusLabel.BorderSides = System.Windows.Forms.ToolStripStatusLabelBorderSides.Left;
            this.IPStatusLabel.Name = "IPStatusLabel";
            this.IPStatusLabel.Size = new System.Drawing.Size(4, 17);
            // 
            // PollingStatusLabel
            // 
            this.PollingStatusLabel.BorderSides = System.Windows.Forms.ToolStripStatusLabelBorderSides.Left;
            this.PollingStatusLabel.Name = "PollingStatusLabel";
            this.PollingStatusLabel.Size = new System.Drawing.Size(4, 17);
            // 
            // TimeoutStatusLabel
            // 
            this.TimeoutStatusLabel.BorderSides = System.Windows.Forms.ToolStripStatusLabelBorderSides.Left;
            this.TimeoutStatusLabel.Name = "TimeoutStatusLabel";
            this.TimeoutStatusLabel.Size = new System.Drawing.Size(4, 17);
            // 
            // scriptNameLabel
            // 
            this.scriptNameLabel.BorderSides = System.Windows.Forms.ToolStripStatusLabelBorderSides.Left;
            this.scriptNameLabel.Font = new System.Drawing.Font("Tahoma", 8.25F);
            this.scriptNameLabel.Name = "scriptNameLabel";
            this.scriptNameLabel.Size = new System.Drawing.Size(4, 17);
            // 
            // SaveDialog
            // 
            this.SaveDialog.Title = "Safe Transfer";
            // 
            // LoadDialog
            // 
            this.LoadDialog.Title = "Load Transfer";
            // 
            // RunScriptDialog
            // 
            this.RunScriptDialog.Filter = "Python (*.py)|*.py|All files (*.*)|*.*";
            // 
            // ButtonSetMenu
            // 
            this.ButtonSetMenu.Name = "ButtonSetMenu";
            this.ButtonSetMenu.Size = new System.Drawing.Size(61, 4);
            // 
            // MacroButtonPanel
            // 
            this.MacroButtonPanel.CellBorderStyle = System.Windows.Forms.TableLayoutPanelCellBorderStyle.Single;
            this.MacroButtonPanel.ColumnCount = 10;
            this.MacroButtonPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 10F));
            this.MacroButtonPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 10F));
            this.MacroButtonPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 10F));
            this.MacroButtonPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 10F));
            this.MacroButtonPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 10F));
            this.MacroButtonPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 10F));
            this.MacroButtonPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 10F));
            this.MacroButtonPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 10F));
            this.MacroButtonPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 10F));
            this.MacroButtonPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 10F));
            this.MacroButtonPanel.Controls.Add(this.ButtonSetBox, 9, 0);
            this.MacroButtonPanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.MacroButtonPanel.Location = new System.Drawing.Point(0, 481);
            this.MacroButtonPanel.Name = "MacroButtonPanel";
            this.MacroButtonPanel.RowCount = 1;
            this.MacroButtonPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.MacroButtonPanel.Size = new System.Drawing.Size(899, 49);
            this.MacroButtonPanel.TabIndex = 4;
            // 
            // ButtonSetBox
            // 
            this.ButtonSetBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ButtonSetBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.ButtonSetBox.FormattingEnabled = true;
            this.ButtonSetBox.Location = new System.Drawing.Point(805, 4);
            this.ButtonSetBox.Name = "ButtonSetBox";
            this.ButtonSetBox.Size = new System.Drawing.Size(90, 21);
            this.ButtonSetBox.TabIndex = 0;
            this.ButtonSetBox.SelectedIndexChanged += new System.EventHandler(this.ButtonSetBox_SelectedIndexChanged);
            // 
            // splitContainer
            // 
            this.splitContainer.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.splitContainer.BackgroundImage = global::TCCSim.Properties.Resources.splitterHorizontal;
            this.splitContainer.Location = new System.Drawing.Point(0, 28);
            this.splitContainer.Name = "splitContainer";
            this.splitContainer.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer.Panel1
            // 
            this.splitContainer.Panel1.Controls.Add(this.TransfersContainer);
            // 
            // splitContainer.Panel2
            // 
            this.splitContainer.Panel2.Controls.Add(this.InfoContainer);
            this.splitContainer.Size = new System.Drawing.Size(640, 452);
            this.splitContainer.SplitterDistance = 216;
            this.splitContainer.SplitterWidth = 6;
            this.splitContainer.TabIndex = 2;
            // 
            // TransfersContainer
            // 
            this.TransfersContainer.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.TransfersContainer.Controls.Add(this.TransfersView);
            this.TransfersContainer.Location = new System.Drawing.Point(3, 3);
            this.TransfersContainer.Name = "TransfersContainer";
            this.TransfersContainer.Size = new System.Drawing.Size(634, 210);
            this.TransfersContainer.TabIndex = 2;
            this.TransfersContainer.TabStop = false;
            this.TransfersContainer.Text = "Transfers";
            // 
            // TransfersView
            // 
            this.TransfersView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.Time,
            this.From,
            this.To,
            this.MessageType,
            this.Comments});
            this.TransfersView.ContextMenuStrip = this.TransferMenu;
            this.TransfersView.Cursor = System.Windows.Forms.Cursors.Default;
            this.TransfersView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TransfersView.FullRowSelect = true;
            this.TransfersView.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
            this.TransfersView.HideSelection = false;
            this.TransfersView.Location = new System.Drawing.Point(3, 16);
            this.TransfersView.MultiSelect = false;
            this.TransfersView.Name = "TransfersView";
            this.TransfersView.Size = new System.Drawing.Size(628, 191);
            this.TransfersView.TabIndex = 1;
            this.TransfersView.UseCompatibleStateImageBehavior = false;
            this.TransfersView.View = System.Windows.Forms.View.Details;
            this.TransfersView.SelectedIndexChanged += new System.EventHandler(this.TransfersView_SelectedIndexChanged);
            this.TransfersView.DoubleClick += new System.EventHandler(this.TransfersView_DoubleClick);
            // 
            // Time
            // 
            this.Time.Text = "Time";
            this.Time.Width = 75;
            // 
            // From
            // 
            this.From.Text = "From";
            this.From.Width = 53;
            // 
            // To
            // 
            this.To.Text = "To";
            this.To.Width = 53;
            // 
            // MessageType
            // 
            this.MessageType.Text = "Message Type";
            this.MessageType.Width = 128;
            // 
            // Comments
            // 
            this.Comments.Text = "Comments";
            this.Comments.Width = 300;
            // 
            // TransferMenu
            // 
            this.TransferMenu.Name = "TransferMenu";
            this.TransferMenu.Size = new System.Drawing.Size(61, 4);
            // 
            // InfoContainer
            // 
            this.InfoContainer.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.InfoContainer.Controls.Add(this.tabControl1);
            this.InfoContainer.Location = new System.Drawing.Point(3, 3);
            this.InfoContainer.Name = "InfoContainer";
            this.InfoContainer.Size = new System.Drawing.Size(634, 228);
            this.InfoContainer.TabIndex = 3;
            this.InfoContainer.TabStop = false;
            this.InfoContainer.Text = "Message Information";
            // 
            // tabControl1
            // 
            this.tabControl1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.tabControl1.Controls.Add(this.PRInfo);
            this.tabControl1.Controls.Add(this.PRErrors);
            this.tabControl1.Controls.Add(this.ReadableTab);
            this.tabControl1.Controls.Add(this.RawTab);
            this.tabControl1.Controls.Add(this.ScriptTab);
            this.tabControl1.Location = new System.Drawing.Point(6, 19);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(622, 203);
            this.tabControl1.TabIndex = 3;
            this.tabControl1.Click += new System.EventHandler(this.tabControl1_Click);
            // 
            // PRInfo
            // 
            this.PRInfo.Controls.Add(this.PRTable);
            this.PRInfo.Location = new System.Drawing.Point(4, 22);
            this.PRInfo.Name = "PRInfo";
            this.PRInfo.Padding = new System.Windows.Forms.Padding(3);
            this.PRInfo.Size = new System.Drawing.Size(614, 177);
            this.PRInfo.TabIndex = 2;
            this.PRInfo.Text = "PR Data";
            this.PRInfo.UseVisualStyleBackColor = true;
            // 
            // PRTable
            // 
            this.PRTable.AutoScroll = true;
            this.PRTable.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.PRTable.ColumnCount = 2;
            this.PRTable.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 200F));
            this.PRTable.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.PRTable.Dock = System.Windows.Forms.DockStyle.Fill;
            this.PRTable.Location = new System.Drawing.Point(3, 3);
            this.PRTable.Name = "PRTable";
            this.PRTable.RowCount = 1;
            this.PRTable.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.PRTable.Size = new System.Drawing.Size(608, 171);
            this.PRTable.TabIndex = 4;
            this.PRTable.MouseDown += new System.Windows.Forms.MouseEventHandler(this.PRTable_MouseDown);
            // 
            // PRErrors
            // 
            this.PRErrors.Controls.Add(this.ErrorView);
            this.PRErrors.Location = new System.Drawing.Point(4, 22);
            this.PRErrors.Name = "PRErrors";
            this.PRErrors.Padding = new System.Windows.Forms.Padding(3);
            this.PRErrors.Size = new System.Drawing.Size(614, 177);
            this.PRErrors.TabIndex = 3;
            this.PRErrors.Text = "PR Errors";
            this.PRErrors.UseVisualStyleBackColor = true;
            // 
            // ErrorView
            // 
            this.ErrorView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.ErrorView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.ETime,
            this.ErrorLevel,
            this.ErrorNumber,
            this.ErrorDescription});
            this.ErrorView.FullRowSelect = true;
            this.ErrorView.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
            this.ErrorView.HideSelection = false;
            this.ErrorView.Location = new System.Drawing.Point(6, 6);
            this.ErrorView.Name = "ErrorView";
            this.ErrorView.Size = new System.Drawing.Size(604, 165);
            this.ErrorView.TabIndex = 5;
            this.ErrorView.UseCompatibleStateImageBehavior = false;
            this.ErrorView.View = System.Windows.Forms.View.Details;
            // 
            // ETime
            // 
            this.ETime.Text = "Time";
            this.ETime.Width = 75;
            // 
            // ErrorLevel
            // 
            this.ErrorLevel.Text = "Level";
            this.ErrorLevel.Width = 44;
            // 
            // ErrorNumber
            // 
            this.ErrorNumber.Text = "Number";
            this.ErrorNumber.Width = 70;
            // 
            // ErrorDescription
            // 
            this.ErrorDescription.Text = "Description";
            this.ErrorDescription.Width = 374;
            // 
            // ReadableTab
            // 
            this.ReadableTab.Controls.Add(this.MsgInfoTable);
            this.ReadableTab.Location = new System.Drawing.Point(4, 22);
            this.ReadableTab.Name = "ReadableTab";
            this.ReadableTab.Padding = new System.Windows.Forms.Padding(3);
            this.ReadableTab.Size = new System.Drawing.Size(614, 177);
            this.ReadableTab.TabIndex = 0;
            this.ReadableTab.Text = "Readable Data";
            this.ReadableTab.UseVisualStyleBackColor = true;
            // 
            // MsgInfoTable
            // 
            this.MsgInfoTable.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.MsgInfoTable.AutoScroll = true;
            this.MsgInfoTable.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.MsgInfoTable.ColumnCount = 1;
            this.MsgInfoTable.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.MsgInfoTable.Controls.Add(this.InfoHead, 0, 0);
            this.MsgInfoTable.Location = new System.Drawing.Point(6, 6);
            this.MsgInfoTable.Name = "MsgInfoTable";
            this.MsgInfoTable.RowCount = 1;
            this.MsgInfoTable.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.MsgInfoTable.Size = new System.Drawing.Size(604, 165);
            this.MsgInfoTable.TabIndex = 6;
            this.MsgInfoTable.MouseDown += new System.Windows.Forms.MouseEventHandler(this.MsgInfoTable_MouseDown);
            // 
            // InfoHead
            // 
            this.InfoHead.AutoSize = true;
            this.InfoHead.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.InfoHead.ForeColor = System.Drawing.SystemColors.ControlText;
            this.InfoHead.Location = new System.Drawing.Point(3, 0);
            this.InfoHead.Name = "InfoHead";
            this.InfoHead.Size = new System.Drawing.Size(0, 16);
            this.InfoHead.TabIndex = 0;
            // 
            // RawTab
            // 
            this.RawTab.Controls.Add(this.splitContainer1);
            this.RawTab.Location = new System.Drawing.Point(4, 22);
            this.RawTab.Name = "RawTab";
            this.RawTab.Padding = new System.Windows.Forms.Padding(3);
            this.RawTab.Size = new System.Drawing.Size(614, 177);
            this.RawTab.TabIndex = 1;
            this.RawTab.Text = "Raw Data";
            this.RawTab.UseVisualStyleBackColor = true;
            // 
            // splitContainer1
            // 
            this.splitContainer1.BackgroundImage = global::TCCSim.Properties.Resources.splitter;
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(3, 3);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.HexLabel);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.BackColor = System.Drawing.SystemColors.Control;
            this.splitContainer1.Panel2.Controls.Add(this.HexTable);
            this.splitContainer1.Size = new System.Drawing.Size(608, 171);
            this.splitContainer1.SplitterDistance = 88;
            this.splitContainer1.TabIndex = 0;
            // 
            // HexLabel
            // 
            this.HexLabel.AutoSize = true;
            this.HexLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.HexLabel.Location = new System.Drawing.Point(3, 9);
            this.HexLabel.Name = "HexLabel";
            this.HexLabel.Size = new System.Drawing.Size(83, 13);
            this.HexLabel.TabIndex = 0;
            this.HexLabel.Text = "Hexadecimal:";
            // 
            // HexTable
            // 
            this.HexTable.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.HexTable.AutoScroll = true;
            this.HexTable.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.HexTable.ColumnCount = 2;
            this.HexTable.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 50F));
            this.HexTable.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.HexTable.Location = new System.Drawing.Point(3, 3);
            this.HexTable.Name = "HexTable";
            this.HexTable.Padding = new System.Windows.Forms.Padding(0, 3, 0, 0);
            this.HexTable.RowCount = 1;
            this.HexTable.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.HexTable.Size = new System.Drawing.Size(510, 165);
            this.HexTable.TabIndex = 7;
            this.HexTable.MouseDown += new System.Windows.Forms.MouseEventHandler(this.HexTable_MouseDown);
            // 
            // ScriptTab
            // 
            this.ScriptTab.BackColor = System.Drawing.SystemColors.Control;
            this.ScriptTab.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch;
            this.ScriptTab.Controls.Add(this.ClearScript);
            this.ScriptTab.Controls.Add(this.ScriptOutput);
            this.ScriptTab.Location = new System.Drawing.Point(4, 22);
            this.ScriptTab.Name = "ScriptTab";
            this.ScriptTab.Padding = new System.Windows.Forms.Padding(3);
            this.ScriptTab.Size = new System.Drawing.Size(614, 177);
            this.ScriptTab.TabIndex = 4;
            this.ScriptTab.Text = "Script Output";
            // 
            // ClearScript
            // 
            this.ClearScript.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.ClearScript.Location = new System.Drawing.Point(561, 6);
            this.ClearScript.Name = "ClearScript";
            this.ClearScript.Size = new System.Drawing.Size(47, 165);
            this.ClearScript.TabIndex = 9;
            this.ClearScript.Text = "Clear";
            this.ClearScript.UseVisualStyleBackColor = true;
            this.ClearScript.Click += new System.EventHandler(this.ClearScript_Click);
            // 
            // ScriptOutput
            // 
            this.ScriptOutput.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.ScriptOutput.BackColor = System.Drawing.Color.Black;
            this.ScriptOutput.Font = new System.Drawing.Font("Courier New", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.ScriptOutput.ForeColor = System.Drawing.Color.White;
            this.ScriptOutput.Location = new System.Drawing.Point(3, 6);
            this.ScriptOutput.Multiline = true;
            this.ScriptOutput.Name = "ScriptOutput";
            this.ScriptOutput.ReadOnly = true;
            this.ScriptOutput.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.ScriptOutput.Size = new System.Drawing.Size(552, 168);
            this.ScriptOutput.TabIndex = 8;
            // 
            // MainView
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(899, 552);
            this.Controls.Add(this.MacroButtonPanel);
            this.Controls.Add(this.statusStrip1);
            this.Controls.Add(this.splitContainer);
            this.Controls.Add(this.toolStrip1);
            this.Controls.Add(this.MessageContainer);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.KeyPreview = true;
            this.Name = "MainView";
            this.Text = "TCCSim";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.MainView_FormClosing);
            this.KeyUp += new System.Windows.Forms.KeyEventHandler(this.MainView_KeyUp);
            this.MessageContainer.ResumeLayout(false);
            this.toolStrip1.ResumeLayout(false);
            this.toolStrip1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.MessageWatcher)).EndInit();
            this.statusStrip1.ResumeLayout(false);
            this.statusStrip1.PerformLayout();
            this.MacroButtonPanel.ResumeLayout(false);
            this.splitContainer.Panel1.ResumeLayout(false);
            this.splitContainer.Panel2.ResumeLayout(false);
            this.splitContainer.ResumeLayout(false);
            this.TransfersContainer.ResumeLayout(false);
            this.InfoContainer.ResumeLayout(false);
            this.tabControl1.ResumeLayout(false);
            this.PRInfo.ResumeLayout(false);
            this.PRErrors.ResumeLayout(false);
            this.ReadableTab.ResumeLayout(false);
            this.MsgInfoTable.ResumeLayout(false);
            this.MsgInfoTable.PerformLayout();
            this.RawTab.ResumeLayout(false);
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel1.PerformLayout();
            this.splitContainer1.Panel2.ResumeLayout(false);
            this.splitContainer1.ResumeLayout(false);
            this.ScriptTab.ResumeLayout(false);
            this.ScriptTab.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.GroupBox MessageContainer;
        private System.Windows.Forms.ToolStrip toolStrip1;
        private System.Windows.Forms.ToolStripButton ConnectButton;
        private System.Windows.Forms.ToolStripDropDownButton FilterButton;
        private System.Windows.Forms.GroupBox TransfersContainer;
        private System.IO.FileSystemWatcher MessageWatcher;
        private System.Windows.Forms.ListBox MessageBox;
        private System.Windows.Forms.ContextMenuStrip MessageMenu;
        private ListView TransfersView;
        private ColumnHeader From;
        private ColumnHeader To;
        private ColumnHeader MessageType;
        private ColumnHeader Comments;
        private ToolStripSeparator toolStripSeparator1;
        private ToolStripButton CreateMessageButton;
        private SplitContainer splitContainer;
        private GroupBox InfoContainer;
        private TabControl tabControl1;
        private TabPage ReadableTab;
        private TabPage RawTab;
        private StatusStrip statusStrip1;
        private ToolStripStatusLabel StatusLabel;
        private ToolStripSeparator toolStripSeparator2;
        private ToolStripSeparator toolStripSeparator4;
        private ToolStripButton PauseButton;
        private ToolStripSeparator toolStripSeparator6;
        private ToolStripStatusLabel PauseLabel;
        private SaveFileDialog SaveDialog;
        private OpenFileDialog LoadDialog;
        private SplitContainer splitContainer1;
        private Label HexLabel;
        private ToolStripDropDownButton TransferButton;
        private ToolStripMenuItem ClearTransferButton;
        private ToolStripMenuItem SaveTransferButton;
        private ToolStripMenuItem LoadTransferButton;
        private ToolStripSeparator toolStripSeparator8;
        private ToolStripDropDownButton FaultInjectionButton;
        private ToolStripMenuItem InjectIDButton;
        private ToolStripMenuItem InjectTSButton;
        private ToolStripMenuItem InjectCRCButton;
        private ToolStripSeparator toolStripSeparator3;
        private ColumnHeader Time;
        private TabPage PRInfo;
        private TabPage PRErrors;
        private ListView ErrorView;
        private ColumnHeader ETime;
        private ColumnHeader ErrorLevel;
        private ColumnHeader ErrorNumber;
        private ColumnHeader ErrorDescription;
        private TableLayoutPanel PRTable;
        private Label InfoHead;
        private TableLayoutPanel MsgInfoTable;
        private TableLayoutPanel HexTable;
        private ToolStripMenuItem reuseToolStripMenuItem;
        private ToolStripMenuItem decreaseTimeToolStripMenuItem;
        private ToolStripMenuItem referenceTimestampToolStripMenuItem;
        private ToolStripMenuItem decreasedToolStripMenuItem;
        private ToolStripMenuItem increasedToolStripMenuItem;
        private ToolStripMenuItem fragmentMessageWithToolStripMenuItem;
        private ToolStripMenuItem msDelayToolStripMenuItem;
        private ToolStripMenuItem msDelayToolStripMenuItem1;
        private ToolStripMenuItem msDelayToolStripMenuItem2;
        private ToolStripButton RunScriptButton;
        private ToolStripSeparator toolStripSeparator5;
        private OpenFileDialog RunScriptDialog;
        private TabPage ScriptTab;
        private TextBox ScriptOutput;
        private Button ClearScript;
        private ToolStripStatusLabel scriptNameLabel;
        private ComboBox comboBox1;
        private Label pathLabel;
        private ContextMenuStrip ButtonSetMenu;
        private ToolStripDropDownButton ButtonsButton;
        private ToolStripMenuItem buttonSetupToolStripMenuItem;
        private ToolStripSeparator toolStripSeparator7;
        private ToolStripSeparator toolStripSeparator9;
        private TableLayoutPanel MacroButtonPanel;
        private ComboBox ButtonSetBox;
        private ToolStripStatusLabel IPStatusLabel;
        private ToolStripStatusLabel TrainIDStatusLabel;
        private ToolStripStatusLabel PollingStatusLabel;
        private ToolStripStatusLabel TimeoutStatusLabel;
        private ContextMenuStrip TransferMenu;
        private ToolStripStatusLabel SiteIDStatusLabel;
        private ToolStripMenuItem InjectSiteIDButton;
        private ToolStripStatusLabel RegionIDStatusLabel;
        private ToolStripMenuItem InjectRegionIDButton;
        private ToolStripStatusLabel CrcStatusLabel;
    }
}

