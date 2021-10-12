using System;
using System.Windows.Forms;

namespace AOSManager
{
   partial class AOSManager
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
         this.components = new System.ComponentModel.Container();
         System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(AOSManager));
         this.groupBoxTrains = new System.Windows.Forms.GroupBox();
         this.listViewTrains = new System.Windows.Forms.ListView();
         this.TrainNumber = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
         this.RunningStatus = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
         this.SelectWindowOperation = new System.Windows.Forms.ContextMenuStrip(this.components);
         this.toolStripMinimize = new System.Windows.Forms.ToolStripButton();
         this.toolStripRestore = new System.Windows.Forms.ToolStripButton();
         this.groupBoxTrains.SuspendLayout();
         this.SelectWindowOperation.SuspendLayout();
         this.SuspendLayout();
         // 
         // groupBoxTrains
         // 
         this.groupBoxTrains.Controls.Add(this.listViewTrains);
         this.groupBoxTrains.Dock = System.Windows.Forms.DockStyle.Fill;
         this.groupBoxTrains.Font = new System.Drawing.Font("Arial Rounded MT Bold", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
         this.groupBoxTrains.Location = new System.Drawing.Point(0, 0);
         this.groupBoxTrains.Name = "groupBoxTrains";
         this.groupBoxTrains.Size = new System.Drawing.Size(614, 445);
         this.groupBoxTrains.TabIndex = 2;
         this.groupBoxTrains.TabStop = false;
         this.groupBoxTrains.Text = "Train Running Status";
         // 
         // listViewTrains
         // 
         this.listViewTrains.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.TrainNumber,
            this.RunningStatus});
         this.listViewTrains.Dock = System.Windows.Forms.DockStyle.Fill;
         this.listViewTrains.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
         this.listViewTrains.FullRowSelect = true;
         this.listViewTrains.HideSelection = false;
         this.listViewTrains.Location = new System.Drawing.Point(3, 22);
         this.listViewTrains.Name = "listViewTrains";
         this.listViewTrains.Size = new System.Drawing.Size(608, 420);
         this.listViewTrains.TabIndex = 0;
         this.listViewTrains.UseCompatibleStateImageBehavior = false;
         this.listViewTrains.View = System.Windows.Forms.View.Details;
         this.listViewTrains.MultiSelect = false;
         this.listViewTrains.MouseClick += new System.Windows.Forms.MouseEventHandler(this.trainGUIvisibilty_RightClick);
         // 
         // TrainNumber
         // 
         this.TrainNumber.Text = "Train ID";
         this.TrainNumber.Width = 150;
         // 
         // RunningStatus
         // 
         this.RunningStatus.Text = "Running Status";
         this.RunningStatus.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
         this.RunningStatus.Width = 452;
         // 
         // SelectWindowOperation
         // 
         this.SelectWindowOperation.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMinimize,
            this.toolStripRestore});
         this.SelectWindowOperation.Name = "SelectWindowOperation";
         this.SelectWindowOperation.Size = new System.Drawing.Size(161, 48);
         // 
         // toolStripMinimize
         // 
         this.toolStripMinimize.Name = "toolStripMinimize";
         this.toolStripMinimize.Size = new System.Drawing.Size(60, 19);
         this.toolStripMinimize.Text = "Minimize";
         this.toolStripMinimize.Click += new EventHandler(this.Minimize_Click);
         // 
         // toolStripRestore
         // 
         this.toolStripRestore.Name = "toolStripRestore";
         this.toolStripRestore.Size = new System.Drawing.Size(50, 19);
         this.toolStripRestore.Text = "Restore";
         this.toolStripRestore.Click += new EventHandler(this.Restore_Click);
         // 
         // AOSManager
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(614, 445);
         this.Controls.Add(this.groupBoxTrains);
         this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
         this.Name = "AOSManager";
         this.Text = "AOSManager v1.0.4";
         this.groupBoxTrains.ResumeLayout(false);
         this.SelectWindowOperation.ResumeLayout(false);
         this.ResumeLayout(false);
      }
      #endregion

      private System.Windows.Forms.GroupBox      groupBoxTrains;
      private  System.Windows.Forms.ListView     listViewTrains;
      private System.Windows.Forms.ColumnHeader  RunningStatus;
      private System.Windows.Forms.ColumnHeader  TrainNumber;
      private ContextMenuStrip                   SelectWindowOperation;
      private ToolStripButton                    toolStripMinimize;
      private ToolStripButton                    toolStripRestore;
   }
}

