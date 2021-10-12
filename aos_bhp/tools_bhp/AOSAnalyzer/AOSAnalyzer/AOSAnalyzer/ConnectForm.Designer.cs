namespace AOSAnalyzer
{
    partial class ConnectForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ConnectForm));
            this.cancelButton = new System.Windows.Forms.Button();
            this.connectButton = new System.Windows.Forms.Button();
            this.newButton = new System.Windows.Forms.Button();
            this.removeButton = new System.Windows.Forms.Button();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.pathBox = new System.Windows.Forms.TextBox();
            this.savedConnections = new System.Windows.Forms.ListView();
            this.saveButton = new System.Windows.Forms.Button();
            this.label10 = new System.Windows.Forms.Label();
            this.nameBox = new System.Windows.Forms.TextBox();
            this.label8 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.portBox = new System.Windows.Forms.TextBox();
            this.ipBox = new System.Windows.Forms.TextBox();
            this.browseButton = new System.Windows.Forms.Button();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.SuspendLayout();
            // 
            // cancelButton
            // 
            this.cancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cancelButton.Location = new System.Drawing.Point(348, 3);
            this.cancelButton.Name = "cancelButton";
            this.cancelButton.Size = new System.Drawing.Size(84, 22);
            this.cancelButton.TabIndex = 6;
            this.cancelButton.Text = "Cancel";
            this.cancelButton.UseVisualStyleBackColor = true;
            this.cancelButton.Click += new System.EventHandler(this.cancelButton_Click);
            // 
            // connectButton
            // 
            this.connectButton.Location = new System.Drawing.Point(63, 3);
            this.connectButton.Name = "connectButton";
            this.connectButton.Size = new System.Drawing.Size(84, 22);
            this.connectButton.TabIndex = 5;
            this.connectButton.Text = "Connect";
            this.connectButton.UseVisualStyleBackColor = true;
            this.connectButton.Click += new System.EventHandler(this.connect);
            // 
            // newButton
            // 
            this.newButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.newButton.Location = new System.Drawing.Point(11, 286);
            this.newButton.Name = "newButton";
            this.newButton.Size = new System.Drawing.Size(96, 23);
            this.newButton.TabIndex = 12;
            this.newButton.Text = "New";
            this.newButton.UseVisualStyleBackColor = true;
            this.newButton.Click += new System.EventHandler(this.newButton_Click);
            // 
            // removeButton
            // 
            this.removeButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.removeButton.Location = new System.Drawing.Point(115, 286);
            this.removeButton.Name = "removeButton";
            this.removeButton.Size = new System.Drawing.Size(96, 23);
            this.removeButton.TabIndex = 13;
            this.removeButton.Text = "Remove";
            this.removeButton.UseVisualStyleBackColor = true;
            this.removeButton.Click += new System.EventHandler(this.removeButton_Click);
            // 
            // splitContainer1
            // 
            this.splitContainer1.Location = new System.Drawing.Point(12, 12);
            this.splitContainer1.Name = "splitContainer1";
            this.splitContainer1.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.browseButton);
            this.splitContainer1.Panel1.Controls.Add(this.pathBox);
            this.splitContainer1.Panel1.Controls.Add(this.savedConnections);
            this.splitContainer1.Panel1.Controls.Add(this.saveButton);
            this.splitContainer1.Panel1.Controls.Add(this.label10);
            this.splitContainer1.Panel1.Controls.Add(this.nameBox);
            this.splitContainer1.Panel1.Controls.Add(this.label8);
            this.splitContainer1.Panel1.Controls.Add(this.label1);
            this.splitContainer1.Panel1.Controls.Add(this.label7);
            this.splitContainer1.Panel1.Controls.Add(this.portBox);
            this.splitContainer1.Panel1.Controls.Add(this.ipBox);
            this.splitContainer1.Panel1.Controls.Add(this.removeButton);
            this.splitContainer1.Panel1.Controls.Add(this.newButton);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.connectButton);
            this.splitContainer1.Panel2.Controls.Add(this.cancelButton);
            this.splitContainer1.Size = new System.Drawing.Size(448, 350);
            this.splitContainer1.SplitterDistance = 318;
            this.splitContainer1.TabIndex = 15;
            // 
            // pathBox
            // 
            this.pathBox.Location = new System.Drawing.Point(247, 125);
            this.pathBox.MaxLength = 30;
            this.pathBox.Name = "pathBox";
            this.pathBox.Size = new System.Drawing.Size(158, 20);
            this.pathBox.TabIndex = 21;
            // 
            // savedConnections
            // 
            this.savedConnections.FullRowSelect = true;
            this.savedConnections.HideSelection = false;
            this.savedConnections.Location = new System.Drawing.Point(3, 3);
            this.savedConnections.MultiSelect = false;
            this.savedConnections.Name = "savedConnections";
            this.savedConnections.Size = new System.Drawing.Size(217, 277);
            this.savedConnections.TabIndex = 0;
            this.savedConnections.UseCompatibleStateImageBehavior = false;
            this.savedConnections.View = System.Windows.Forms.View.List;
            this.savedConnections.VirtualMode = true;
            this.savedConnections.SelectedIndexChanged += new System.EventHandler(this.savedConnections_SelectedIndexChanged);
            this.savedConnections.DoubleClick += new System.EventHandler(this.savedConnections_DoubleClick);
            this.savedConnections.RetrieveVirtualItem += new System.Windows.Forms.RetrieveVirtualItemEventHandler(this.savedConnections_RetrieveVirtualItem);
            // 
            // saveButton
            // 
            this.saveButton.Location = new System.Drawing.Point(376, 152);
            this.saveButton.Name = "saveButton";
            this.saveButton.Size = new System.Drawing.Size(56, 22);
            this.saveButton.TabIndex = 4;
            this.saveButton.Text = "Save";
            this.saveButton.UseVisualStyleBackColor = true;
            this.saveButton.Click += new System.EventHandler(this.saveButton_Click);
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(244, 21);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(38, 13);
            this.label10.TabIndex = 20;
            this.label10.Text = "Name:";
            // 
            // nameBox
            // 
            this.nameBox.Location = new System.Drawing.Point(288, 18);
            this.nameBox.MaxLength = 30;
            this.nameBox.Name = "nameBox";
            this.nameBox.Size = new System.Drawing.Size(144, 20);
            this.nameBox.TabIndex = 1;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(244, 47);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(20, 13);
            this.label8.TabIndex = 17;
            this.label8.Text = "IP:";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(244, 109);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(70, 13);
            this.label1.TabIndex = 18;
            this.label1.Text = "Graph Setup:";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(244, 73);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(29, 13);
            this.label7.TabIndex = 18;
            this.label7.Text = "Port:";
            // 
            // portBox
            // 
            this.portBox.Location = new System.Drawing.Point(288, 70);
            this.portBox.MaxLength = 30;
            this.portBox.Name = "portBox";
            this.portBox.Size = new System.Drawing.Size(144, 20);
            this.portBox.TabIndex = 3;
            // 
            // ipBox
            // 
            this.ipBox.Location = new System.Drawing.Point(288, 44);
            this.ipBox.MaxLength = 30;
            this.ipBox.Name = "ipBox";
            this.ipBox.Size = new System.Drawing.Size(144, 20);
            this.ipBox.TabIndex = 2;
            // 
            // browseButton
            // 
            this.browseButton.Location = new System.Drawing.Point(411, 122);
            this.browseButton.Name = "browseButton";
            this.browseButton.Size = new System.Drawing.Size(21, 23);
            this.browseButton.TabIndex = 22;
            this.browseButton.Text = "...";
            this.browseButton.UseVisualStyleBackColor = true;
            this.browseButton.Click += new System.EventHandler(this.browseButton_Click);
            // 
            // ConnectForm
            // 
            this.AcceptButton = this.connectButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.cancelButton;
            this.ClientSize = new System.Drawing.Size(470, 371);
            this.Controls.Add(this.splitContainer1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "ConnectForm";
            this.ShowIcon = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Connect";
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel1.PerformLayout();
            this.splitContainer1.Panel2.ResumeLayout(false);
            this.splitContainer1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button cancelButton;
        private System.Windows.Forms.Button connectButton;
        private System.Windows.Forms.Button newButton;
        private System.Windows.Forms.Button removeButton;
        private System.Windows.Forms.SplitContainer splitContainer1;
        private System.Windows.Forms.Button saveButton;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.TextBox nameBox;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox portBox;
        private System.Windows.Forms.TextBox ipBox;
        private System.Windows.Forms.ListView savedConnections;
        private System.Windows.Forms.TextBox pathBox;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button browseButton;

    }
}