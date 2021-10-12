namespace AOSAnalyzer
{
    partial class SliceForm
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
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.markertab = new System.Windows.Forms.TabPage();
            this.toMarkerBox = new System.Windows.Forms.ComboBox();
            this.label3 = new System.Windows.Forms.Label();
            this.fromMarkerBox = new System.Windows.Forms.ComboBox();
            this.label4 = new System.Windows.Forms.Label();
            this.markerSliceButton = new System.Windows.Forms.Button();
            this.valuetab = new System.Windows.Forms.TabPage();
            this.toValueBox = new System.Windows.Forms.TextBox();
            this.fromValueBox = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.valueSliceButton = new System.Windows.Forms.Button();
            this.tabControl1.SuspendLayout();
            this.markertab.SuspendLayout();
            this.valuetab.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.markertab);
            this.tabControl1.Controls.Add(this.valuetab);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(194, 169);
            this.tabControl1.TabIndex = 0;
            // 
            // markertab
            // 
            this.markertab.Controls.Add(this.toMarkerBox);
            this.markertab.Controls.Add(this.label3);
            this.markertab.Controls.Add(this.fromMarkerBox);
            this.markertab.Controls.Add(this.label4);
            this.markertab.Controls.Add(this.markerSliceButton);
            this.markertab.Location = new System.Drawing.Point(4, 22);
            this.markertab.Name = "markertab";
            this.markertab.Padding = new System.Windows.Forms.Padding(3);
            this.markertab.Size = new System.Drawing.Size(186, 143);
            this.markertab.TabIndex = 1;
            this.markertab.Text = "Markers";
            this.markertab.UseVisualStyleBackColor = true;
            // 
            // toMarkerBox
            // 
            this.toMarkerBox.FormattingEnabled = true;
            this.toMarkerBox.Location = new System.Drawing.Point(41, 39);
            this.toMarkerBox.Name = "toMarkerBox";
            this.toMarkerBox.Size = new System.Drawing.Size(121, 21);
            this.toMarkerBox.TabIndex = 8;
            this.toMarkerBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.onMarkerKeyDown);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(5, 42);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(23, 13);
            this.label3.TabIndex = 7;
            this.label3.Text = "To:";
            // 
            // fromMarkerBox
            // 
            this.fromMarkerBox.FormattingEnabled = true;
            this.fromMarkerBox.Location = new System.Drawing.Point(41, 6);
            this.fromMarkerBox.Name = "fromMarkerBox";
            this.fromMarkerBox.Size = new System.Drawing.Size(121, 21);
            this.fromMarkerBox.TabIndex = 6;
            this.fromMarkerBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.onMarkerKeyDown);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(5, 9);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(33, 13);
            this.label4.TabIndex = 5;
            this.label4.Text = "From:";
            // 
            // markerSliceButton
            // 
            this.markerSliceButton.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.markerSliceButton.Location = new System.Drawing.Point(3, 117);
            this.markerSliceButton.Name = "markerSliceButton";
            this.markerSliceButton.Size = new System.Drawing.Size(180, 23);
            this.markerSliceButton.TabIndex = 1;
            this.markerSliceButton.Text = "Slice";
            this.markerSliceButton.UseVisualStyleBackColor = true;
            this.markerSliceButton.Click += new System.EventHandler(this.markerSliceButton_Click);
            // 
            // valuetab
            // 
            this.valuetab.Controls.Add(this.toValueBox);
            this.valuetab.Controls.Add(this.fromValueBox);
            this.valuetab.Controls.Add(this.label2);
            this.valuetab.Controls.Add(this.label1);
            this.valuetab.Controls.Add(this.valueSliceButton);
            this.valuetab.Location = new System.Drawing.Point(4, 22);
            this.valuetab.Name = "valuetab";
            this.valuetab.Padding = new System.Windows.Forms.Padding(3);
            this.valuetab.Size = new System.Drawing.Size(186, 143);
            this.valuetab.TabIndex = 0;
            this.valuetab.Text = "Values";
            this.valuetab.UseVisualStyleBackColor = true;
            // 
            // toValueBox
            // 
            this.toValueBox.Location = new System.Drawing.Point(41, 39);
            this.toValueBox.Name = "toValueBox";
            this.toValueBox.Size = new System.Drawing.Size(121, 20);
            this.toValueBox.TabIndex = 6;
            this.toValueBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.onValueKeyDown);
            // 
            // fromValueBox
            // 
            this.fromValueBox.Location = new System.Drawing.Point(41, 6);
            this.fromValueBox.Name = "fromValueBox";
            this.fromValueBox.Size = new System.Drawing.Size(121, 20);
            this.fromValueBox.TabIndex = 5;
            this.fromValueBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.onValueKeyDown);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(5, 42);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(23, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "To:";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(5, 9);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(33, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "From:";
            // 
            // valueSliceButton
            // 
            this.valueSliceButton.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.valueSliceButton.Location = new System.Drawing.Point(3, 117);
            this.valueSliceButton.Name = "valueSliceButton";
            this.valueSliceButton.Size = new System.Drawing.Size(180, 23);
            this.valueSliceButton.TabIndex = 0;
            this.valueSliceButton.Text = "Slice";
            this.valueSliceButton.UseVisualStyleBackColor = true;
            this.valueSliceButton.Click += new System.EventHandler(this.valueSliceButton_Click);
            // 
            // SliceForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(194, 169);
            this.Controls.Add(this.tabControl1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
            this.Name = "SliceForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Slice";
            this.tabControl1.ResumeLayout(false);
            this.markertab.ResumeLayout(false);
            this.markertab.PerformLayout();
            this.valuetab.ResumeLayout(false);
            this.valuetab.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage valuetab;
        private System.Windows.Forms.TabPage markertab;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button valueSliceButton;
        private System.Windows.Forms.ComboBox toMarkerBox;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.ComboBox fromMarkerBox;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Button markerSliceButton;
        private System.Windows.Forms.TextBox toValueBox;
        private System.Windows.Forms.TextBox fromValueBox;
    }
}