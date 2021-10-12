/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2014-07-18    Hidaji      Added site ID
* 2016-06-28    akushwah    Added Region ID
*******************************************************************************/
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace TCCSim
{
    public partial class ConnectDialog : Form
    {
        public String ID { private set; get; }
        public String siteID { private set; get; }
        public String regionID { private set; get; }
        public String Connect { private set; get; }
        public String Timer { private set; get; }
        public String Timeout { private set; get; }
        public bool AutoConnect { private set; get; }
        public tccCrc crc { set; get; }

        /// <summary>
        /// Open with no data
        /// </summary>
        public ConnectDialog()
        {
            this.StartPosition = FormStartPosition.CenterParent;
            InitializeComponent();
            this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.CancelButton1.DialogResult = System.Windows.Forms.DialogResult.Cancel;
        }

        /// <summary>
        ///  Open with default data
        /// </summary>
        /// <param name="ID"></param>
        /// <param name="Connect"></param>
        /// <param name="Timer"></param>
        /// <param name="Timeout"></param>
        /// <param name="AutoConnect"></param>
        public ConnectDialog(String ID, String siteID, String regionID, String Connect, String Timer, String Timeout, bool AutoConnect, tccCrc crc)
        {
            this.StartPosition = FormStartPosition.CenterParent;
            InitializeComponent();
            this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.CancelButton1.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.IDBox.Text = ID;
            this.siteIDBox.Text = siteID;
            this.regionIDBox.Text = regionID;
            this.ConnectBox.Text = Connect;
            this.TimerBox.Text = Timer;
            this.TimeOutBox.Text = Timeout;
            AutoConnBox.Checked = AutoConnect;
            this.crc = crc;
            if (crc == tccCrc.tccCrcCentral)
            {
                this.Central.Checked = true;
                this.Region.Checked = false;
            }
            else
            {
                this.Central.Checked = false;
                this.Region.Checked = true;
            }
        }

        /// <summary>
        /// Save data and close
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void OkButton_Click(object sender, EventArgs e)
        {
            this.ID = IDBox.Text;
            this.siteID = siteIDBox.Text;
            this.regionID = regionIDBox.Text;
            this.Connect = ConnectBox.Text;
            this.Timer = TimerBox.Text;
            this.Timeout = TimeOutBox.Text;
            AutoConnect = AutoConnBox.Checked;
            this.crc = this.Central.Checked ? tccCrc.tccCrcCentral : tccCrc.tccCrcRegion;
            Close();
        }

        private void CancelButton_Click(object sender, EventArgs e)
        {
            Close();
        }

        /// <summary>
        /// Pressing enter is equivalent to pressing OK
        /// Pressing escape is quuivalent to pressing Cancel
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void KeyPressed(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                DialogResult = DialogResult.OK;
                OkButton_Click(sender, e);
            }
            else if (e.KeyCode == Keys.Escape)
            {
                DialogResult = DialogResult.Cancel;
                CancelButton_Click(sender, e);
            }
        }

        /// <summary>
        /// Only one of Central or Local can be selected
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Central_CheckedChanged(object sender, EventArgs e)
        {
            this.Region.Checked = !this.Central.Checked;
        }

        /// <summary>
        /// Only one of Central or Local can be selected
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Local_CheckedChanged(object sender, EventArgs e)
        {
            this.Central.Checked = ! this.Region.Checked;
        }
    }
}
