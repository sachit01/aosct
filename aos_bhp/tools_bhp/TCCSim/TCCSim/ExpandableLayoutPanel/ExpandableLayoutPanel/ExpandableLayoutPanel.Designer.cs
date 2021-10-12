namespace ExpandableLayoutPanel
{
    partial class ExpandableLayoutPanel
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

        #region Component Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.HeaderText = new System.Windows.Forms.Label();
            this.contentPanel = new System.Windows.Forms.Panel();
            this.expandBox = new System.Windows.Forms.CheckBox();
            this.SuspendLayout();
            // 
            // HeaderText
            // 
            this.HeaderText.AutoSize = true;
            this.HeaderText.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, ((System.Drawing.FontStyle)((System.Drawing.FontStyle.Bold | System.Drawing.FontStyle.Underline))), System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.HeaderText.Location = new System.Drawing.Point(34, 10);
            this.HeaderText.Name = "HeaderText";
            this.HeaderText.Size = new System.Drawing.Size(48, 13);
            this.HeaderText.TabIndex = 0;
            this.HeaderText.Text = "Header";
            // 
            // contentPanel
            // 
            this.contentPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.contentPanel.AutoSize = true;
            this.contentPanel.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.contentPanel.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.contentPanel.Location = new System.Drawing.Point(3, 32);
            this.contentPanel.Name = "contentPanel";
            this.contentPanel.Size = new System.Drawing.Size(2, 2);
            this.contentPanel.TabIndex = 2;
            this.contentPanel.Visible = false;
            // 
            // expandBox
            // 
            this.expandBox.Appearance = System.Windows.Forms.Appearance.Button;
            this.expandBox.AutoSize = true;
            this.expandBox.Cursor = System.Windows.Forms.Cursors.Arrow;
            this.expandBox.Location = new System.Drawing.Point(5, 5);
            this.expandBox.Margin = new System.Windows.Forms.Padding(0);
            this.expandBox.Name = "expandBox";
            this.expandBox.Size = new System.Drawing.Size(29, 23);
            this.expandBox.TabIndex = 3;
            this.expandBox.Text = "∨";
            this.expandBox.UseVisualStyleBackColor = true;
            this.expandBox.CheckedChanged += new System.EventHandler(this.expandBox_CheckedChanged);
            // 
            // ExpandableLayoutPanel
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSize = true;
            this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.Controls.Add(this.expandBox);
            this.Controls.Add(this.contentPanel);
            this.Controls.Add(this.HeaderText);
            this.Name = "ExpandableLayoutPanel";
            this.Size = new System.Drawing.Size(85, 37);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label HeaderText;
        private System.Windows.Forms.Panel contentPanel;
        private System.Windows.Forms.CheckBox expandBox;

    }
}
