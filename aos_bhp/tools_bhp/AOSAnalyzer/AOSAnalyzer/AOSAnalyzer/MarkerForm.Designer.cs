namespace AOSAnalyzer
{
    partial class MarkerForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MarkerForm));
            this.xPos = new System.Windows.Forms.NumericUpDown();
            this.markerList = new System.Windows.Forms.ComboBox();
            this.label1 = new System.Windows.Forms.Label();
            this.dataView = new System.Windows.Forms.ListView();
            this.MarkerName = new System.Windows.Forms.ColumnHeader();
            this.MarkerValue = new System.Windows.Forms.ColumnHeader();
            this.MarkerUnit = new System.Windows.Forms.ColumnHeader();
            this.pickColor = new System.Windows.Forms.Button();
            this.addMarker = new System.Windows.Forms.Button();
            this.diffMarkerList = new System.Windows.Forms.ComboBox();
            this.deltaCheckButton = new System.Windows.Forms.CheckBox();
            this.label2 = new System.Windows.Forms.Label();
            this.removeMarker = new System.Windows.Forms.Button();
            this.signChangerBox = new System.Windows.Forms.CheckBox();
            this.label3 = new System.Windows.Forms.Label();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.label5 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.diffXPos = new System.Windows.Forms.NumericUpDown();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            ((System.ComponentModel.ISupportInitialize)(this.xPos)).BeginInit();
            this.groupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.diffXPos)).BeginInit();
            this.groupBox2.SuspendLayout();
            this.SuspendLayout();
            // 
            // xPos
            // 
            this.xPos.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.xPos.Location = new System.Drawing.Point(68, 39);
            this.xPos.Maximum = new decimal(new int[] {
            268435455,
            1042612833,
            542101086,
            0});
            this.xPos.Name = "xPos";
            this.xPos.Size = new System.Drawing.Size(75, 20);
            this.xPos.TabIndex = 1;
            this.xPos.ValueChanged += new System.EventHandler(this.xPos_ValueChanged);
            // 
            // markerList
            // 
            this.markerList.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.markerList.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.markerList.FormattingEnabled = true;
            this.markerList.Location = new System.Drawing.Point(231, 5);
            this.markerList.Name = "markerList";
            this.markerList.Size = new System.Drawing.Size(142, 21);
            this.markerList.TabIndex = 0;
            this.markerList.SelectedIndexChanged += new System.EventHandler(this.markerList_SelectedIndexChanged);
            // 
            // label1
            // 
            this.label1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(6, 41);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(56, 13);
            this.label1.TabIndex = 2;
            this.label1.Text = "X position:";
            // 
            // dataView
            // 
            this.dataView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.dataView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.MarkerName,
            this.MarkerValue,
            this.MarkerUnit});
            this.dataView.GridLines = true;
            this.dataView.Location = new System.Drawing.Point(2, 5);
            this.dataView.MultiSelect = false;
            this.dataView.Name = "dataView";
            this.dataView.Size = new System.Drawing.Size(217, 328);
            this.dataView.TabIndex = 3;
            this.dataView.UseCompatibleStateImageBehavior = false;
            this.dataView.View = System.Windows.Forms.View.Details;
            // 
            // MarkerName
            // 
            this.MarkerName.Text = "Name";
            // 
            // MarkerValue
            // 
            this.MarkerValue.Text = "Value";
            this.MarkerValue.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
            // 
            // MarkerUnit
            // 
            this.MarkerUnit.Text = "Unit";
            // 
            // pickColor
            // 
            this.pickColor.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.pickColor.Location = new System.Drawing.Point(68, 18);
            this.pickColor.Name = "pickColor";
            this.pickColor.Size = new System.Drawing.Size(75, 18);
            this.pickColor.TabIndex = 4;
            this.pickColor.UseVisualStyleBackColor = true;
            this.pickColor.Click += new System.EventHandler(this.pickColor_Click);
            // 
            // addMarker
            // 
            this.addMarker.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.addMarker.Location = new System.Drawing.Point(231, 32);
            this.addMarker.Name = "addMarker";
            this.addMarker.Size = new System.Drawing.Size(66, 23);
            this.addMarker.TabIndex = 5;
            this.addMarker.Text = "Add";
            this.addMarker.UseVisualStyleBackColor = true;
            this.addMarker.Click += new System.EventHandler(this.addMarker_Click);
            // 
            // diffMarkerList
            // 
            this.diffMarkerList.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.diffMarkerList.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.diffMarkerList.Enabled = false;
            this.diffMarkerList.FormattingEnabled = true;
            this.diffMarkerList.Location = new System.Drawing.Point(6, 73);
            this.diffMarkerList.Name = "diffMarkerList";
            this.diffMarkerList.Size = new System.Drawing.Size(137, 21);
            this.diffMarkerList.TabIndex = 6;
            this.diffMarkerList.SelectedIndexChanged += new System.EventHandler(this.diffMarkerList_SelectedIndexChanged);
            // 
            // deltaCheckButton
            // 
            this.deltaCheckButton.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.deltaCheckButton.AutoSize = true;
            this.deltaCheckButton.Location = new System.Drawing.Point(6, 17);
            this.deltaCheckButton.Name = "deltaCheckButton";
            this.deltaCheckButton.Size = new System.Drawing.Size(59, 17);
            this.deltaCheckButton.TabIndex = 7;
            this.deltaCheckButton.Text = "Enable";
            this.deltaCheckButton.UseVisualStyleBackColor = true;
            this.deltaCheckButton.CheckedChanged += new System.EventHandler(this.deltaCheckButton_CheckedChanged);
            // 
            // label2
            // 
            this.label2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(28, 21);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(34, 13);
            this.label2.TabIndex = 8;
            this.label2.Text = "Color:";
            // 
            // removeMarker
            // 
            this.removeMarker.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.removeMarker.Location = new System.Drawing.Point(306, 32);
            this.removeMarker.Name = "removeMarker";
            this.removeMarker.Size = new System.Drawing.Size(67, 23);
            this.removeMarker.TabIndex = 9;
            this.removeMarker.Text = "Remove";
            this.removeMarker.UseVisualStyleBackColor = true;
            this.removeMarker.Click += new System.EventHandler(this.removeMarker_Click);
            // 
            // signChangerBox
            // 
            this.signChangerBox.AutoSize = true;
            this.signChangerBox.CheckAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.signChangerBox.Enabled = false;
            this.signChangerBox.Location = new System.Drawing.Point(98, 54);
            this.signChangerBox.Name = "signChangerBox";
            this.signChangerBox.Size = new System.Drawing.Size(40, 17);
            this.signChangerBox.TabIndex = 10;
            this.signChangerBox.Text = "+/-";
            this.signChangerBox.UseVisualStyleBackColor = true;
            this.signChangerBox.CheckedChanged += new System.EventHandler(this.signChangerBox_CheckedChanged);
            // 
            // label3
            // 
            this.label3.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(227, 291);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(103, 13);
            this.label3.TabIndex = 11;
            this.label3.Text = "Marker to differ with:";
            // 
            // groupBox1
            // 
            this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox1.Controls.Add(this.label5);
            this.groupBox1.Controls.Add(this.label4);
            this.groupBox1.Controls.Add(this.diffXPos);
            this.groupBox1.Controls.Add(this.signChangerBox);
            this.groupBox1.Controls.Add(this.diffMarkerList);
            this.groupBox1.Controls.Add(this.deltaCheckButton);
            this.groupBox1.Location = new System.Drawing.Point(225, 205);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(149, 127);
            this.groupBox1.TabIndex = 12;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Difference properties";
            // 
            // label5
            // 
            this.label5.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(6, 104);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(56, 13);
            this.label5.TabIndex = 10;
            this.label5.Text = "X position:";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(3, 53);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(65, 13);
            this.label4.TabIndex = 11;
            this.label4.Text = "Diff. Marker:";
            // 
            // diffXPos
            // 
            this.diffXPos.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.diffXPos.Location = new System.Drawing.Point(68, 102);
            this.diffXPos.Maximum = new decimal(new int[] {
            268435455,
            1042612833,
            542101086,
            0});
            this.diffXPos.Name = "diffXPos";
            this.diffXPos.Size = new System.Drawing.Size(75, 20);
            this.diffXPos.TabIndex = 9;
            this.diffXPos.ValueChanged += new System.EventHandler(this.diffXPos_ValueChanged);
            // 
            // groupBox2
            // 
            this.groupBox2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox2.Controls.Add(this.label2);
            this.groupBox2.Controls.Add(this.pickColor);
            this.groupBox2.Controls.Add(this.label1);
            this.groupBox2.Controls.Add(this.xPos);
            this.groupBox2.Location = new System.Drawing.Point(225, 74);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(149, 69);
            this.groupBox2.TabIndex = 13;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Marker properties";
            // 
            // MarkerForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSize = true;
            this.ClientSize = new System.Drawing.Size(377, 345);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.removeMarker);
            this.Controls.Add(this.addMarker);
            this.Controls.Add(this.dataView);
            this.Controls.Add(this.markerList);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "MarkerForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Markers";
            this.TopMost = true;
            ((System.ComponentModel.ISupportInitialize)(this.xPos)).EndInit();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.diffXPos)).EndInit();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.NumericUpDown xPos;
        private System.Windows.Forms.ComboBox markerList;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ListView dataView;
        private System.Windows.Forms.ColumnHeader MarkerName;
        private System.Windows.Forms.ColumnHeader MarkerValue;
        private System.Windows.Forms.Button pickColor;
        private System.Windows.Forms.Button addMarker;
        private System.Windows.Forms.ComboBox diffMarkerList;
        private System.Windows.Forms.CheckBox deltaCheckButton;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Button removeMarker;
        private System.Windows.Forms.CheckBox signChangerBox;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.NumericUpDown diffXPos;
        private System.Windows.Forms.ColumnHeader MarkerUnit;
    }
}