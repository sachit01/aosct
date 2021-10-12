namespace AOSAnalyzer
{
    partial class CurveForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CurveForm));
            this.curveList = new System.Windows.Forms.ListView();
            this.curveName = new System.Windows.Forms.ColumnHeader();
            this.curveColor = new System.Windows.Forms.ColumnHeader();
            this.pickColor = new System.Windows.Forms.Button();
            this.closeButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // curveList
            // 
            this.curveList.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.curveName,
            this.curveColor});
            this.curveList.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
            this.curveList.HideSelection = false;
            this.curveList.Location = new System.Drawing.Point(12, 12);
            this.curveList.MultiSelect = false;
            this.curveList.Name = "curveList";
            this.curveList.Size = new System.Drawing.Size(224, 316);
            this.curveList.Sorting = System.Windows.Forms.SortOrder.Ascending;
            this.curveList.TabIndex = 0;
            this.curveList.UseCompatibleStateImageBehavior = false;
            this.curveList.View = System.Windows.Forms.View.Details;
            this.curveList.VirtualMode = true;
            this.curveList.SelectedIndexChanged += new System.EventHandler(this.curveList_SelectedIndexChanged);
            this.curveList.DoubleClick += new System.EventHandler(this.curveList_DoubleClick);
            this.curveList.RetrieveVirtualItem += new System.Windows.Forms.RetrieveVirtualItemEventHandler(this.curveList_RetrieveVirtualItem);
            // 
            // curveName
            // 
            this.curveName.Text = "Name";
            this.curveName.Width = 136;
            // 
            // curveColor
            // 
            this.curveColor.Text = "Color";
            this.curveColor.Width = 84;
            // 
            // pickColor
            // 
            this.pickColor.BackColor = System.Drawing.SystemColors.Control;
            this.pickColor.Location = new System.Drawing.Point(257, 12);
            this.pickColor.Name = "pickColor";
            this.pickColor.Size = new System.Drawing.Size(21, 18);
            this.pickColor.TabIndex = 1;
            this.pickColor.UseVisualStyleBackColor = false;
            this.pickColor.Click += new System.EventHandler(this.pickColor_Click);
            // 
            // closeButton
            // 
            this.closeButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.closeButton.Location = new System.Drawing.Point(257, 301);
            this.closeButton.Name = "closeButton";
            this.closeButton.Size = new System.Drawing.Size(92, 26);
            this.closeButton.TabIndex = 2;
            this.closeButton.Text = "Close";
            this.closeButton.UseVisualStyleBackColor = true;
            this.closeButton.Click += new System.EventHandler(this.closeButton_Click);
            // 
            // CurveForm
            // 
            this.AcceptButton = this.closeButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.closeButton;
            this.ClientSize = new System.Drawing.Size(358, 338);
            this.Controls.Add(this.closeButton);
            this.Controls.Add(this.pickColor);
            this.Controls.Add(this.curveList);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "CurveForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Edit Colors";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.ListView curveList;
        private System.Windows.Forms.ColumnHeader curveName;
        private System.Windows.Forms.ColumnHeader curveColor;
        private System.Windows.Forms.Button pickColor;
        private System.Windows.Forms.Button closeButton;
    }
}