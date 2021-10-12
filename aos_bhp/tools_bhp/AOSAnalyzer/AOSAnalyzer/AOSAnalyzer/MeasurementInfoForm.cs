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
    public partial class MeasurementInfoForm : Form
    {
        public event MeasureCommentChanged MCC;
        public delegate void MeasureCommentChanged(String comment);

        public MeasurementInfoForm(String comment, String date, UnitDescr unit)
        {
            InitializeComponent();

            this.MeasureCommentBox.Text = comment;
            this.MeasureDateLabel.Text = date;

            this.UnitNameLabel.Text = unit.Name;
            this.UnitProtocolLabel.Text = unit.Protocol;
            this.UnitVersionLabel.Text = unit.Version;
            this.MeasureCommentBox.Focus();
        }

        private void saveButton_Click(object sender, EventArgs e)
        {
            this.MCC(MeasureCommentBox.Text);
            Close();
        }
    }
}
