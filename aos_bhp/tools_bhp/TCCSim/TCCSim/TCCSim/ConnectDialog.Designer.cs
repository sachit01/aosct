namespace TCCSim
{
    partial class ConnectDialog
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
            this.IDLabel = new System.Windows.Forms.Label();
            this.ConnectData = new System.Windows.Forms.Label();
            this.IDBox = new System.Windows.Forms.TextBox();
            this.ConnectBox = new System.Windows.Forms.TextBox();
            this.OkButton = new System.Windows.Forms.Button();
            this.CancelButton1 = new System.Windows.Forms.Button();
            this.TimerLabel = new System.Windows.Forms.Label();
            this.TimerBox = new System.Windows.Forms.TextBox();
            this.TimeOutLabel = new System.Windows.Forms.Label();
            this.TimeOutBox = new System.Windows.Forms.TextBox();
            this.AutoConnBox = new System.Windows.Forms.CheckBox();
            this.siteIDBox = new System.Windows.Forms.TextBox();
            this.regionIDBox = new System.Windows.Forms.TextBox();
            this.siteIDLabel = new System.Windows.Forms.Label();
            this.regionIDLabel = new System.Windows.Forms.Label();
            this.Region = new System.Windows.Forms.RadioButton();
            this.Central = new System.Windows.Forms.RadioButton();
            this.groupBoxCRC = new System.Windows.Forms.GroupBox();
            this.groupBoxCRC.SuspendLayout();
            this.SuspendLayout();
            // 
            // IDLabel
            // 
            this.IDLabel.AutoSize = true;
            this.IDLabel.Location = new System.Drawing.Point(15, 16);
            this.IDLabel.Name = "IDLabel";
            this.IDLabel.Size = new System.Drawing.Size(45, 13);
            this.IDLabel.TabIndex = 0;
            this.IDLabel.Text = "Train ID";
            // 
            // ConnectData
            // 
            this.ConnectData.AutoSize = true;
            this.ConnectData.Location = new System.Drawing.Point(15, 146);
            this.ConnectData.Name = "ConnectData";
            this.ConnectData.Size = new System.Drawing.Size(73, 13);
            this.ConnectData.TabIndex = 1;
            this.ConnectData.Text = "Connect Data";
            // 
            // IDBox
            // 
            this.IDBox.Location = new System.Drawing.Point(120, 12);
            this.IDBox.Name = "IDBox";
            this.IDBox.Size = new System.Drawing.Size(149, 20);
            this.IDBox.TabIndex = 1;
            this.IDBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.KeyPressed);
            // 
            // ConnectBox
            // 
            this.ConnectBox.Location = new System.Drawing.Point(120, 139);
            this.ConnectBox.Name = "ConnectBox";
            this.ConnectBox.Size = new System.Drawing.Size(149, 20);
            this.ConnectBox.TabIndex = 2;
            this.ConnectBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.KeyPressed);
            // 
            // OkButton
            // 
            this.OkButton.Location = new System.Drawing.Point(120, 348);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(63, 22);
            this.OkButton.TabIndex = 5;
            this.OkButton.Text = "Connect";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // CancelButton1
            // 
            this.CancelButton1.Location = new System.Drawing.Point(206, 348);
            this.CancelButton1.Name = "CancelButton1";
            this.CancelButton1.Size = new System.Drawing.Size(63, 22);
            this.CancelButton1.TabIndex = 6;
            this.CancelButton1.Text = "Cancel";
            this.CancelButton1.UseVisualStyleBackColor = true;
            this.CancelButton1.Click += new System.EventHandler(this.CancelButton_Click);
            // 
            // TimerLabel
            // 
            this.TimerLabel.AutoSize = true;
            this.TimerLabel.Location = new System.Drawing.Point(15, 183);
            this.TimerLabel.Name = "TimerLabel";
            this.TimerLabel.Size = new System.Drawing.Size(90, 13);
            this.TimerLabel.TabIndex = 8;
            this.TimerLabel.Text = "Polling Interval (s)";
            // 
            // TimerBox
            // 
            this.TimerBox.Location = new System.Drawing.Point(120, 180);
            this.TimerBox.Name = "TimerBox";
            this.TimerBox.Size = new System.Drawing.Size(149, 20);
            this.TimerBox.TabIndex = 3;
            this.TimerBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.KeyPressed);
            // 
            // TimeOutLabel
            // 
            this.TimeOutLabel.AutoSize = true;
            this.TimeOutLabel.Location = new System.Drawing.Point(15, 220);
            this.TimeOutLabel.Name = "TimeOutLabel";
            this.TimeOutLabel.Size = new System.Drawing.Size(59, 13);
            this.TimeOutLabel.TabIndex = 9;
            this.TimeOutLabel.Text = "Timeout (s)";
            // 
            // TimeOutBox
            // 
            this.TimeOutBox.Location = new System.Drawing.Point(120, 217);
            this.TimeOutBox.Name = "TimeOutBox";
            this.TimeOutBox.Size = new System.Drawing.Size(149, 20);
            this.TimeOutBox.TabIndex = 4;
            this.TimeOutBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.KeyPressed);
            // 
            // AutoConnBox
            // 
            this.AutoConnBox.AutoSize = true;
            this.AutoConnBox.Location = new System.Drawing.Point(120, 243);
            this.AutoConnBox.Name = "AutoConnBox";
            this.AutoConnBox.Size = new System.Drawing.Size(87, 17);
            this.AutoConnBox.TabIndex = 11;
            this.AutoConnBox.Text = "Autoconnect";
            this.AutoConnBox.UseVisualStyleBackColor = true;
            // 
            // siteIDBox
            // 
            this.siteIDBox.Location = new System.Drawing.Point(120, 57);
            this.siteIDBox.Name = "siteIDBox";
            this.siteIDBox.Size = new System.Drawing.Size(149, 20);
            this.siteIDBox.TabIndex = 13;
            // 
            // regionIDBox
            // 
            this.regionIDBox.Location = new System.Drawing.Point(120, 100);
            this.regionIDBox.Name = "regionIDBox";
            this.regionIDBox.Size = new System.Drawing.Size(149, 20);
            this.regionIDBox.TabIndex = 15;
            // 
            // siteIDLabel
            // 
            this.siteIDLabel.AutoSize = true;
            this.siteIDLabel.Location = new System.Drawing.Point(15, 60);
            this.siteIDLabel.Name = "siteIDLabel";
            this.siteIDLabel.Size = new System.Drawing.Size(39, 13);
            this.siteIDLabel.TabIndex = 12;
            this.siteIDLabel.Text = "Site ID";
            // 
            // regionIDLabel
            // 
            this.regionIDLabel.AutoSize = true;
            this.regionIDLabel.Location = new System.Drawing.Point(15, 107);
            this.regionIDLabel.Name = "regionIDLabel";
            this.regionIDLabel.Size = new System.Drawing.Size(55, 13);
            this.regionIDLabel.TabIndex = 14;
            this.regionIDLabel.Text = "Region ID";
            // 
            // Region
            // 
            this.Region.AutoSize = true;
            this.Region.Location = new System.Drawing.Point(6, 42);
            this.Region.Name = "Region";
            this.Region.Size = new System.Drawing.Size(59, 17);
            this.Region.TabIndex = 16;
            this.Region.TabStop = true;
            this.Region.Text = "Region";
            this.Region.UseVisualStyleBackColor = true;
            this.Region.CheckedChanged += new System.EventHandler(this.Local_CheckedChanged);
            // 
            // Central
            // 
            this.Central.AutoSize = true;
            this.Central.Location = new System.Drawing.Point(6, 19);
            this.Central.Name = "Central";
            this.Central.Size = new System.Drawing.Size(58, 17);
            this.Central.TabIndex = 17;
            this.Central.TabStop = true;
            this.Central.Text = "Central";
            this.Central.UseVisualStyleBackColor = true;
            this.Central.CheckedChanged += new System.EventHandler(this.Central_CheckedChanged);
            // 
            // groupBox1
            // 
            this.groupBoxCRC.Controls.Add(this.Region);
            this.groupBoxCRC.Controls.Add(this.Central);
            this.groupBoxCRC.Location = new System.Drawing.Point(120, 270);
            this.groupBoxCRC.Name = "groupBoxCRC";
            this.groupBoxCRC.Size = new System.Drawing.Size(149, 72);
            this.groupBoxCRC.TabIndex = 18;
            this.groupBoxCRC.TabStop = false;
            this.groupBoxCRC.Text = "Crc";
            // 
            // ConnectDialog
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSize = true;
            this.ClientSize = new System.Drawing.Size(352, 391);
            this.Controls.Add(this.groupBoxCRC);
            this.Controls.Add(this.regionIDBox);
            this.Controls.Add(this.regionIDLabel);
            this.Controls.Add(this.siteIDBox);
            this.Controls.Add(this.siteIDLabel);
            this.Controls.Add(this.AutoConnBox);
            this.Controls.Add(this.TimeOutBox);
            this.Controls.Add(this.TimeOutLabel);
            this.Controls.Add(this.TimerBox);
            this.Controls.Add(this.TimerLabel);
            this.Controls.Add(this.CancelButton1);
            this.Controls.Add(this.OkButton);
            this.Controls.Add(this.ConnectBox);
            this.Controls.Add(this.IDBox);
            this.Controls.Add(this.ConnectData);
            this.Controls.Add(this.IDLabel);
            this.Name = "ConnectDialog";
            this.Text = "Connect Information";
            this.groupBoxCRC.ResumeLayout(false);
            this.groupBoxCRC.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label IDLabel;
        private System.Windows.Forms.Label ConnectData;
        private System.Windows.Forms.TextBox IDBox;
        private System.Windows.Forms.TextBox ConnectBox;
        private System.Windows.Forms.Button OkButton;
        private System.Windows.Forms.Button CancelButton1;
        private System.Windows.Forms.Label TimerLabel;
        private System.Windows.Forms.TextBox TimerBox;
        private System.Windows.Forms.Label TimeOutLabel;
        private System.Windows.Forms.TextBox TimeOutBox;
        private System.Windows.Forms.CheckBox AutoConnBox;
        private System.Windows.Forms.TextBox siteIDBox;
        private System.Windows.Forms.Label siteIDLabel;
        private System.Windows.Forms.TextBox regionIDBox;
        private System.Windows.Forms.Label regionIDLabel;
        private System.Windows.Forms.RadioButton Region;
        private System.Windows.Forms.RadioButton Central;
        private System.Windows.Forms.GroupBox groupBoxCRC;
    }
}