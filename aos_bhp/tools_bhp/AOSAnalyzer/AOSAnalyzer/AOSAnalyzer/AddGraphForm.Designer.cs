namespace AOSAnalyzer
{
    partial class AddGraphForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(AddGraphForm));
            this.addGraphButton = new System.Windows.Forms.Button();
            this.graphMin = new System.Windows.Forms.NumericUpDown();
            this.graphMax = new System.Windows.Forms.NumericUpDown();
            this.label1 = new System.Windows.Forms.Label();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.curveSelection = new System.Windows.Forms.ListView();
            this.curveName = new System.Windows.Forms.ColumnHeader();
            this.label4 = new System.Windows.Forms.Label();
            this.graphTitle = new System.Windows.Forms.TextBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.Auto = new System.Windows.Forms.CheckBox();
            this.label2 = new System.Windows.Forms.Label();
            this.cancelButton = new System.Windows.Forms.Button();
            ((System.ComponentModel.ISupportInitialize)(this.graphMin)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.graphMax)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // addGraphButton
            // 
            this.addGraphButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.addGraphButton.Location = new System.Drawing.Point(6, 140);
            this.addGraphButton.Name = "addGraphButton";
            this.addGraphButton.Size = new System.Drawing.Size(87, 28);
            this.addGraphButton.TabIndex = 6;
            this.addGraphButton.Text = "Add Graph";
            this.addGraphButton.UseVisualStyleBackColor = true;
            this.addGraphButton.Click += new System.EventHandler(this.addGraphButton_Click);
            // 
            // graphMin
            // 
            this.graphMin.Enabled = false;
            this.graphMin.Location = new System.Drawing.Point(74, 42);
            this.graphMin.Maximum = new decimal(new int[] {
            -1593835521,
            466537709,
            54210,
            0});
            this.graphMin.Minimum = new decimal(new int[] {
            -402653185,
            -1613725636,
            54210108,
            -2147483648});
            this.graphMin.Name = "graphMin";
            this.graphMin.Size = new System.Drawing.Size(84, 20);
            this.graphMin.TabIndex = 3;
            // 
            // graphMax
            // 
            this.graphMax.Enabled = false;
            this.graphMax.Location = new System.Drawing.Point(74, 68);
            this.graphMax.Maximum = new decimal(new int[] {
            1661992959,
            1808227885,
            5,
            0});
            this.graphMax.Minimum = new decimal(new int[] {
            -1593835521,
            466537709,
            54210,
            -2147483648});
            this.graphMax.Name = "graphMax";
            this.graphMax.Size = new System.Drawing.Size(84, 20);
            this.graphMax.TabIndex = 4;
            this.graphMax.Value = new decimal(new int[] {
            50,
            0,
            0,
            0});
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(4, 71);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(59, 13);
            this.label1.TabIndex = 3;
            this.label1.Text = "Graph Max";
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.FixedPanel = System.Windows.Forms.FixedPanel.Panel2;
            this.splitContainer1.Location = new System.Drawing.Point(0, 0);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.curveSelection);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.label4);
            this.splitContainer1.Panel2.Controls.Add(this.graphTitle);
            this.splitContainer1.Panel2.Controls.Add(this.groupBox1);
            this.splitContainer1.Panel2.Controls.Add(this.cancelButton);
            this.splitContainer1.Panel2.Controls.Add(this.addGraphButton);
            this.splitContainer1.Size = new System.Drawing.Size(420, 385);
            this.splitContainer1.SplitterDistance = 220;
            this.splitContainer1.TabIndex = 4;
            // 
            // curveSelection
            // 
            this.curveSelection.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.curveSelection.CheckBoxes = true;
            this.curveSelection.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.curveName});
            this.curveSelection.FullRowSelect = true;
            this.curveSelection.Location = new System.Drawing.Point(3, 3);
            this.curveSelection.Name = "curveSelection";
            this.curveSelection.Size = new System.Drawing.Size(214, 379);
            this.curveSelection.TabIndex = 0;
            this.curveSelection.UseCompatibleStateImageBehavior = false;
            this.curveSelection.View = System.Windows.Forms.View.Details;
            // 
            // curveName
            // 
            this.curveName.Text = "Name";
            this.curveName.Width = 75;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(24, 10);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(27, 13);
            this.label4.TabIndex = 14;
            this.label4.Text = "Title";
            // 
            // graphTitle
            // 
            this.graphTitle.Location = new System.Drawing.Point(67, 7);
            this.graphTitle.MaxLength = 30;
            this.graphTitle.Name = "graphTitle";
            this.graphTitle.Size = new System.Drawing.Size(111, 20);
            this.graphTitle.TabIndex = 1;
            // 
            // groupBox1
            // 
            this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox1.Controls.Add(this.Auto);
            this.groupBox1.Controls.Add(this.label2);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.graphMin);
            this.groupBox1.Controls.Add(this.graphMax);
            this.groupBox1.Location = new System.Drawing.Point(20, 33);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(164, 95);
            this.groupBox1.TabIndex = 10;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Graph properties";
            // 
            // Auto
            // 
            this.Auto.AutoSize = true;
            this.Auto.Checked = true;
            this.Auto.CheckState = System.Windows.Forms.CheckState.Checked;
            this.Auto.Location = new System.Drawing.Point(6, 19);
            this.Auto.Name = "Auto";
            this.Auto.Size = new System.Drawing.Size(48, 17);
            this.Auto.TabIndex = 2;
            this.Auto.Text = "Auto";
            this.Auto.UseVisualStyleBackColor = true;
            this.Auto.CheckedChanged += new System.EventHandler(this.Auto_CheckedChanged);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(4, 45);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(56, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "Graph Min";
            // 
            // cancelButton
            // 
            this.cancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.cancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cancelButton.Location = new System.Drawing.Point(103, 140);
            this.cancelButton.Name = "cancelButton";
            this.cancelButton.Size = new System.Drawing.Size(87, 28);
            this.cancelButton.TabIndex = 7;
            this.cancelButton.Text = "Cancel";
            this.cancelButton.UseVisualStyleBackColor = true;
            this.cancelButton.Click += new System.EventHandler(this.cancelButton_Click);
            // 
            // AddGraphForm
            // 
            this.AcceptButton = this.addGraphButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.cancelButton;
            this.ClientSize = new System.Drawing.Size(420, 385);
            this.Controls.Add(this.splitContainer1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "AddGraphForm";
            this.ShowIcon = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "New Graph";
            ((System.ComponentModel.ISupportInitialize)(this.graphMin)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.graphMax)).EndInit();
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            this.splitContainer1.Panel2.PerformLayout();
            this.splitContainer1.ResumeLayout(false);
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button addGraphButton;
        private System.Windows.Forms.NumericUpDown graphMin;
        private System.Windows.Forms.NumericUpDown graphMax;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.SplitContainer splitContainer1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Button cancelButton;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.CheckBox Auto;
        private System.Windows.Forms.ListView curveSelection;
        private System.Windows.Forms.ColumnHeader curveName;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox graphTitle;
    }
}