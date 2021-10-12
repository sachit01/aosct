namespace AOSAnalyzer
{
    partial class EditGraphForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(EditGraphForm));
            this.saveButton = new System.Windows.Forms.Button();
            this.cancelButton = new System.Windows.Forms.Button();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.curvePage = new System.Windows.Forms.TabPage();
            this.curveList = new System.Windows.Forms.CheckedListBox();
            this.propertyPage = new System.Windows.Forms.TabPage();
            this.label4 = new System.Windows.Forms.Label();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.Auto = new System.Windows.Forms.CheckBox();
            this.label2 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.graphMin = new System.Windows.Forms.NumericUpDown();
            this.graphMax = new System.Windows.Forms.NumericUpDown();
            this.graphTitle = new System.Windows.Forms.TextBox();
            this.graphList = new System.Windows.Forms.ListView();
            this.label3 = new System.Windows.Forms.Label();
            this.tabControl1.SuspendLayout();
            this.curvePage.SuspendLayout();
            this.propertyPage.SuspendLayout();
            this.groupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.graphMin)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.graphMax)).BeginInit();
            this.SuspendLayout();
            // 
            // saveButton
            // 
            this.saveButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.saveButton.Location = new System.Drawing.Point(36, 296);
            this.saveButton.Name = "saveButton";
            this.saveButton.Size = new System.Drawing.Size(75, 23);
            this.saveButton.TabIndex = 7;
            this.saveButton.Text = "Save";
            this.saveButton.UseVisualStyleBackColor = true;
            this.saveButton.Click += new System.EventHandler(this.saveButton_Click);
            // 
            // cancelButton
            // 
            this.cancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.cancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cancelButton.Location = new System.Drawing.Point(210, 296);
            this.cancelButton.Name = "cancelButton";
            this.cancelButton.Size = new System.Drawing.Size(75, 23);
            this.cancelButton.TabIndex = 8;
            this.cancelButton.Text = "Close";
            this.cancelButton.UseVisualStyleBackColor = true;
            this.cancelButton.Click += new System.EventHandler(this.cancelButton_Click);
            // 
            // tabControl1
            // 
            this.tabControl1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.tabControl1.Controls.Add(this.curvePage);
            this.tabControl1.Controls.Add(this.propertyPage);
            this.tabControl1.Location = new System.Drawing.Point(143, 3);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(193, 282);
            this.tabControl1.TabIndex = 2;
            // 
            // curvePage
            // 
            this.curvePage.Controls.Add(this.curveList);
            this.curvePage.Location = new System.Drawing.Point(4, 22);
            this.curvePage.Name = "curvePage";
            this.curvePage.Padding = new System.Windows.Forms.Padding(3);
            this.curvePage.Size = new System.Drawing.Size(185, 256);
            this.curvePage.TabIndex = 0;
            this.curvePage.Text = "Curves";
            this.curvePage.UseVisualStyleBackColor = true;
            // 
            // curveList
            // 
            this.curveList.CheckOnClick = true;
            this.curveList.Dock = System.Windows.Forms.DockStyle.Fill;
            this.curveList.FormattingEnabled = true;
            this.curveList.Location = new System.Drawing.Point(3, 3);
            this.curveList.Name = "curveList";
            this.curveList.Size = new System.Drawing.Size(179, 244);
            this.curveList.TabIndex = 1;
            // 
            // propertyPage
            // 
            this.propertyPage.Controls.Add(this.label4);
            this.propertyPage.Controls.Add(this.groupBox1);
            this.propertyPage.Controls.Add(this.graphTitle);
            this.propertyPage.Location = new System.Drawing.Point(4, 22);
            this.propertyPage.Name = "propertyPage";
            this.propertyPage.Padding = new System.Windows.Forms.Padding(3);
            this.propertyPage.Size = new System.Drawing.Size(185, 256);
            this.propertyPage.TabIndex = 1;
            this.propertyPage.Text = "Properties";
            this.propertyPage.UseVisualStyleBackColor = true;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(10, 20);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(27, 13);
            this.label4.TabIndex = 16;
            this.label4.Text = "Title";
            // 
            // groupBox1
            // 
            this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox1.Controls.Add(this.Auto);
            this.groupBox1.Controls.Add(this.label2);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.graphMin);
            this.groupBox1.Controls.Add(this.graphMax);
            this.groupBox1.Location = new System.Drawing.Point(9, 53);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(164, 97);
            this.groupBox1.TabIndex = 11;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Graph properies";
            // 
            // Auto
            // 
            this.Auto.AutoSize = true;
            this.Auto.Checked = true;
            this.Auto.CheckState = System.Windows.Forms.CheckState.Checked;
            this.Auto.Location = new System.Drawing.Point(6, 19);
            this.Auto.Name = "Auto";
            this.Auto.Size = new System.Drawing.Size(48, 17);
            this.Auto.TabIndex = 4;
            this.Auto.Text = "Auto";
            this.Auto.UseVisualStyleBackColor = true;
            this.Auto.CheckedChanged += new System.EventHandler(this.Auto_CheckedChanged);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(4, 45);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(34, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "Y Min";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(4, 71);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(37, 13);
            this.label1.TabIndex = 3;
            this.label1.Text = "Y Max";
            // 
            // graphMin
            // 
            this.graphMin.Enabled = false;
            this.graphMin.Location = new System.Drawing.Point(74, 42);
            this.graphMin.Maximum = new decimal(new int[] {
            268435455,
            1042612833,
            542101086,
            0});
            this.graphMin.Minimum = new decimal(new int[] {
            268435455,
            1042612833,
            542101086,
            -2147483648});
            this.graphMin.Name = "graphMin";
            this.graphMin.Size = new System.Drawing.Size(84, 20);
            this.graphMin.TabIndex = 5;
            // 
            // graphMax
            // 
            this.graphMax.Enabled = false;
            this.graphMax.Location = new System.Drawing.Point(74, 68);
            this.graphMax.Maximum = new decimal(new int[] {
            268435455,
            1042612833,
            542101086,
            0});
            this.graphMax.Minimum = new decimal(new int[] {
            268435455,
            1042612833,
            542101086,
            -2147483648});
            this.graphMax.Name = "graphMax";
            this.graphMax.Size = new System.Drawing.Size(84, 20);
            this.graphMax.TabIndex = 6;
            this.graphMax.Value = new decimal(new int[] {
            50,
            0,
            0,
            0});
            // 
            // graphTitle
            // 
            this.graphTitle.Location = new System.Drawing.Point(53, 17);
            this.graphTitle.MaxLength = 30;
            this.graphTitle.Name = "graphTitle";
            this.graphTitle.Size = new System.Drawing.Size(111, 20);
            this.graphTitle.TabIndex = 3;
            // 
            // graphList
            // 
            this.graphList.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)));
            this.graphList.HideSelection = false;
            this.graphList.Location = new System.Drawing.Point(5, 25);
            this.graphList.MultiSelect = false;
            this.graphList.Name = "graphList";
            this.graphList.Size = new System.Drawing.Size(131, 260);
            this.graphList.TabIndex = 0;
            this.graphList.UseCompatibleStateImageBehavior = false;
            this.graphList.View = System.Windows.Forms.View.List;
            this.graphList.VirtualMode = true;
            this.graphList.SelectedIndexChanged += new System.EventHandler(this.graphList_SelectedIndexChanged);
            this.graphList.RetrieveVirtualItem += new System.Windows.Forms.RetrieveVirtualItemEventHandler(this.graphList_RetrieveVirtualItem);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(5, 8);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(41, 13);
            this.label3.TabIndex = 6;
            this.label3.Text = "Graphs";
            // 
            // EditGraphForm
            // 
            this.AcceptButton = this.saveButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.cancelButton;
            this.ClientSize = new System.Drawing.Size(348, 326);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.graphList);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.cancelButton);
            this.Controls.Add(this.saveButton);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "EditGraphForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Edit Graphs";
            this.tabControl1.ResumeLayout(false);
            this.curvePage.ResumeLayout(false);
            this.propertyPage.ResumeLayout(false);
            this.propertyPage.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.graphMin)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.graphMax)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button saveButton;
        private System.Windows.Forms.Button cancelButton;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage curvePage;
        private System.Windows.Forms.TabPage propertyPage;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.CheckBox Auto;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.NumericUpDown graphMin;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox graphTitle;
        private System.Windows.Forms.NumericUpDown graphMax;
        private System.Windows.Forms.ListView graphList;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.CheckedListBox curveList;
    }
}