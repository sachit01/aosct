using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace ExpandableLayoutPanel
{
    public partial class ExpandableLayoutPanel : UserControl
    {
        TableLayoutPanel tlp;

        public ExpandableLayoutPanel(String name)
        {
            InitializeComponent();

            HeaderText.Text = name;

            tlp = new TableLayoutPanel();
            contentPanel.Controls.Add(tlp);
        }

        public TableLayoutPanel GetTableLayoutPanel()
        {
            return tlp;
        }

        private void expandBox_CheckedChanged(object sender, EventArgs e)
        {
            expandBox.Text = expandBox.Checked ? "∧" : "∨";
            contentPanel.Visible = expandBox.Checked;
        }
    }
}
