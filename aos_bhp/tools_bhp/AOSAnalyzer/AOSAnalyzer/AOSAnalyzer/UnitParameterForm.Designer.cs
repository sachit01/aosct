namespace AOSAnalyzer
{
    partial class UnitParameterForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(UnitParameterForm));
            this.sendButton = new System.Windows.Forms.Button();
            this.valueBox = new System.Windows.Forms.NumericUpDown();
            this.parameterList = new System.Windows.Forms.ListView();
            this.pName = new System.Windows.Forms.ColumnHeader();
            this.pValue = new System.Windows.Forms.ColumnHeader();
            this.pUnit = new System.Windows.Forms.ColumnHeader();
            this.pMin = new System.Windows.Forms.ColumnHeader();
            this.pMax = new System.Windows.Forms.ColumnHeader();
            this.pType = new System.Windows.Forms.ColumnHeader();
            this.changeList = new System.Windows.Forms.ListView();
            this.Param = new System.Windows.Forms.ColumnHeader();
            this.Old = new System.Windows.Forms.ColumnHeader();
            this.New = new System.Windows.Forms.ColumnHeader();
            ((System.ComponentModel.ISupportInitialize)(this.valueBox)).BeginInit();
            this.SuspendLayout();
            // 
            // sendButton
            // 
            this.sendButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.sendButton.Location = new System.Drawing.Point(703, 45);
            this.sendButton.Name = "sendButton";
            this.sendButton.Size = new System.Drawing.Size(51, 24);
            this.sendButton.TabIndex = 2;
            this.sendButton.Text = "Send";
            this.sendButton.UseVisualStyleBackColor = true;
            this.sendButton.Click += new System.EventHandler(this.sendButton_Click);
            // 
            // valueBox
            // 
            this.valueBox.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.valueBox.Location = new System.Drawing.Point(639, 12);
            this.valueBox.Maximum = new decimal(new int[] {
            -1304428545,
            434162106,
            542,
            0});
            this.valueBox.Minimum = new decimal(new int[] {
            -559939585,
            902409669,
            54,
            -2147483648});
            this.valueBox.Name = "valueBox";
            this.valueBox.Size = new System.Drawing.Size(120, 20);
            this.valueBox.TabIndex = 1;
            // 
            // parameterList
            // 
            this.parameterList.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.parameterList.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.pName,
            this.pValue,
            this.pUnit,
            this.pMin,
            this.pMax,
            this.pType});
            this.parameterList.FullRowSelect = true;
            this.parameterList.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
            this.parameterList.HideSelection = false;
            this.parameterList.Location = new System.Drawing.Point(12, 12);
            this.parameterList.MultiSelect = false;
            this.parameterList.Name = "parameterList";
            this.parameterList.ShowItemToolTips = true;
            this.parameterList.Size = new System.Drawing.Size(519, 378);
            this.parameterList.TabIndex = 0;
            this.parameterList.UseCompatibleStateImageBehavior = false;
            this.parameterList.View = System.Windows.Forms.View.Details;
            this.parameterList.SelectedIndexChanged += new System.EventHandler(this.parameterList_SelectedIndexChanged);
            this.parameterList.DoubleClick += new System.EventHandler(this.parameterList_DoubleClick);
            // 
            // pName
            // 
            this.pName.Text = "Name";
            this.pName.Width = 40;
            // 
            // pValue
            // 
            this.pValue.Text = "Value";
            this.pValue.Width = 39;
            // 
            // pUnit
            // 
            this.pUnit.Text = "Unit";
            this.pUnit.Width = 31;
            // 
            // pMin
            // 
            this.pMin.Text = "Min";
            this.pMin.Width = 29;
            // 
            // pMax
            // 
            this.pMax.Text = "Max";
            this.pMax.Width = 32;
            // 
            // pType
            // 
            this.pType.Text = "Type";
            this.pType.Width = 36;
            // 
            // changeList
            // 
            this.changeList.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.changeList.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.Param,
            this.Old,
            this.New});
            this.changeList.Location = new System.Drawing.Point(537, 94);
            this.changeList.MultiSelect = false;
            this.changeList.Name = "changeList";
            this.changeList.Size = new System.Drawing.Size(230, 296);
            this.changeList.TabIndex = 3;
            this.changeList.UseCompatibleStateImageBehavior = false;
            this.changeList.View = System.Windows.Forms.View.Details;
            this.changeList.VirtualMode = true;
            this.changeList.DoubleClick += new System.EventHandler(this.changeList_DoubleClick);
            this.changeList.RetrieveVirtualItem += new System.Windows.Forms.RetrieveVirtualItemEventHandler(this.changeList_RetrieveVirtualItem);
            // 
            // Param
            // 
            this.Param.Text = "Param";
            // 
            // Old
            // 
            this.Old.Text = "Old";
            // 
            // New
            // 
            this.New.Text = "New";
            // 
            // UnitParameterForm
            // 
            this.AcceptButton = this.sendButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(771, 402);
            this.Controls.Add(this.changeList);
            this.Controls.Add(this.parameterList);
            this.Controls.Add(this.valueBox);
            this.Controls.Add(this.sendButton);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "UnitParameterForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Parameters";
            ((System.ComponentModel.ISupportInitialize)(this.valueBox)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button sendButton;
        private System.Windows.Forms.NumericUpDown valueBox;
        private System.Windows.Forms.ListView parameterList;
        private System.Windows.Forms.ColumnHeader pName;
        private System.Windows.Forms.ColumnHeader pType;
        private System.Windows.Forms.ColumnHeader pUnit;
        private System.Windows.Forms.ColumnHeader pMin;
        private System.Windows.Forms.ColumnHeader pMax;
        private System.Windows.Forms.ColumnHeader pValue;
        private System.Windows.Forms.ListView changeList;
        private System.Windows.Forms.ColumnHeader Param;
        private System.Windows.Forms.ColumnHeader Old;
        private System.Windows.Forms.ColumnHeader New;



    }
}