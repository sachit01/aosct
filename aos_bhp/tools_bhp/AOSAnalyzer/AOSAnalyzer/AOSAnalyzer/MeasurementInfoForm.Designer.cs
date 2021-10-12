namespace AOSAnalyzer
{
    partial class MeasurementInfoForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MeasurementInfoForm));
            this.label1 = new System.Windows.Forms.Label();
            this.UnitNameLabel = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.UnitVersionLabel = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.UnitProtocolLabel = new System.Windows.Forms.Label();
            this.saveButton = new System.Windows.Forms.Button();
            this.MeasureCommentBox = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.MeasureDateLabel = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(12, 10);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(61, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Unit name: ";
            // 
            // UnitNameLabel
            // 
            this.UnitNameLabel.AutoSize = true;
            this.UnitNameLabel.Location = new System.Drawing.Point(85, 10);
            this.UnitNameLabel.Name = "UnitNameLabel";
            this.UnitNameLabel.Size = new System.Drawing.Size(0, 13);
            this.UnitNameLabel.TabIndex = 1;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(12, 24);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(69, 13);
            this.label2.TabIndex = 0;
            this.label2.Text = "Unit version: ";
            // 
            // UnitVersionLabel
            // 
            this.UnitVersionLabel.AutoSize = true;
            this.UnitVersionLabel.Location = new System.Drawing.Point(85, 24);
            this.UnitVersionLabel.Name = "UnitVersionLabel";
            this.UnitVersionLabel.Size = new System.Drawing.Size(0, 13);
            this.UnitVersionLabel.TabIndex = 1;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(12, 38);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(73, 13);
            this.label4.TabIndex = 0;
            this.label4.Text = "Unit protocol: ";
            // 
            // UnitProtocolLabel
            // 
            this.UnitProtocolLabel.AutoSize = true;
            this.UnitProtocolLabel.Location = new System.Drawing.Point(85, 38);
            this.UnitProtocolLabel.Name = "UnitProtocolLabel";
            this.UnitProtocolLabel.Size = new System.Drawing.Size(0, 13);
            this.UnitProtocolLabel.TabIndex = 1;
            // 
            // saveButton
            // 
            this.saveButton.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.saveButton.Location = new System.Drawing.Point(107, 331);
            this.saveButton.Name = "saveButton";
            this.saveButton.Size = new System.Drawing.Size(203, 23);
            this.saveButton.TabIndex = 2;
            this.saveButton.Text = "Save";
            this.saveButton.UseVisualStyleBackColor = true;
            this.saveButton.Click += new System.EventHandler(this.saveButton_Click);
            // 
            // MeasureCommentBox
            // 
            this.MeasureCommentBox.AcceptsReturn = true;
            this.MeasureCommentBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.MeasureCommentBox.Location = new System.Drawing.Point(12, 103);
            this.MeasureCommentBox.Multiline = true;
            this.MeasureCommentBox.Name = "MeasureCommentBox";
            this.MeasureCommentBox.Size = new System.Drawing.Size(396, 218);
            this.MeasureCommentBox.TabIndex = 3;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(12, 87);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(120, 13);
            this.label3.TabIndex = 4;
            this.label3.Text = "Measurement comment:";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(12, 62);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(98, 13);
            this.label5.TabIndex = 4;
            this.label5.Text = "Measurement date:";
            // 
            // MeasureDateLabel
            // 
            this.MeasureDateLabel.AutoSize = true;
            this.MeasureDateLabel.Location = new System.Drawing.Point(120, 62);
            this.MeasureDateLabel.Name = "MeasureDateLabel";
            this.MeasureDateLabel.Size = new System.Drawing.Size(0, 13);
            this.MeasureDateLabel.TabIndex = 5;
            // 
            // MeasurementInfoForm
            // 
            this.AcceptButton = this.saveButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(420, 360);
            this.Controls.Add(this.MeasureDateLabel);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.MeasureCommentBox);
            this.Controls.Add(this.saveButton);
            this.Controls.Add(this.UnitProtocolLabel);
            this.Controls.Add(this.UnitVersionLabel);
            this.Controls.Add(this.UnitNameLabel);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.Name = "MeasurementInfoForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Measurement Info";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label UnitNameLabel;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label UnitVersionLabel;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label UnitProtocolLabel;
        private System.Windows.Forms.Button saveButton;
        private System.Windows.Forms.TextBox MeasureCommentBox;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label MeasureDateLabel;
    }
}