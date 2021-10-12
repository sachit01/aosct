using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace AOSAnalyzer
{
    public partial class PropertiesForm : Form
    {
        public PropertiesForm()
        {
            InitializeComponent();
            TraceScaleBox.Text = Properties.Settings.Default.DefaultTraceScale.ToString();
            GraphHeightBox.Text = Properties.Settings.Default.graphHeight.ToString();
            MarkerStepBox.Text = Properties.Settings.Default.markerStepValue.ToString();
        }

        private void SaveButton_Click(object sender, EventArgs e)
        {
            try
            {
                Properties.Settings.Default.DefaultTraceScale = Int32.Parse(TraceScaleBox.Text);
                Properties.Settings.Default.graphHeight = Int32.Parse(GraphHeightBox.Text);
                Properties.Settings.Default.markerStepValue = Int32.Parse(MarkerStepBox.Text);
                Properties.Settings.Default.Save();
                Close();
            }
            catch
            {
                MessageBox.Show("Can't parse values, check for errors and try again");
            }
        }

        private void cancelButton_Click(object sender, EventArgs e)
        {
            Close();
        }
    }
}
