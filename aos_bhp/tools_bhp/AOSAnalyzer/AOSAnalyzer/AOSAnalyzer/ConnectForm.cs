/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          ConnectForm.cs %
*
*  %version:       3 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:43 %
*
*  DESCRIPTION:    Manages and connects to units with the given details
*                  that the user specifies.
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2011-07-26    Blomqvist   File created
*
*******************************************************************************/
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Xml.Serialization;

namespace AOSAnalyzer
{
    public delegate void PressedEventHandler(object sender, AOSConnectEventArgs e);

    public partial class ConnectForm : Form
    {
        public event PressedEventHandler pressed;

        private AOSConnections connections;

        public ConnectForm(AOSSettings _settings)
        {
            InitializeComponent();

            // Read the connections from xml
            try
            {
                connections = AOSConnections.ReadConnections(Application.StartupPath+"/Connections.xml");
            }
            catch (Exception)
            {
                connections = new AOSConnections();
            }

            savedConnections.VirtualListSize = connections.unitconnections.Count;
        }

        // Gives the listview the selected item when called.
        private void savedConnections_RetrieveVirtualItem(object sender, RetrieveVirtualItemEventArgs e)
        {
            UnitConn unit = connections.unitconnections[e.ItemIndex];
            e.Item = new ListViewItem(unit.name);
        }
        // Update textboxes when a new item is selected.
        private void savedConnections_SelectedIndexChanged(object sender, EventArgs e)
        {
            UpdateTextBoxes();
        }

        // Returns true if the input is correctly formatted.
        private bool SaveCurrentData()
        {
            int i;
            nameBox.Text = nameBox.Text.Trim();
            ipBox.Text = ipBox.Text.Trim();
            portBox.Text = portBox.Text.Trim();
            pathBox.Text = pathBox.Text.Trim();
            if (savedConnections.SelectedIndices.Count == 1 && 
                nameBox.Text != "" &&
                ipBox.Text != "" &&
                portBox.Text != "")
            {
                if (Int32.TryParse(portBox.Text.Trim(), out i))
                {
                    UnitConn unit = connections.unitconnections[savedConnections.SelectedIndices[0]];
                    unit.name = nameBox.Text;
                    unit.ip = ipBox.Text;
                    unit.port = portBox.Text;
                    unit.graphSetup = pathBox.Text;
                }
                else
                {
                    MessageBox.Show("Please enter a number in the port field");
                    return false;
                }
                return true;
            }
            return false;            
        }
        // Takes the selected items values and puts in the textboxes.
        private void UpdateTextBoxes()
        {
            if (savedConnections.SelectedIndices.Count == 1)
            {
                UnitConn unit = connections.unitconnections[savedConnections.SelectedIndices[0]];
                nameBox.Text = unit.name;
                ipBox.Text = unit.ip;
                portBox.Text = unit.port;
                pathBox.Text = unit.graphSetup;
            }
            else
            {
                ClearTextBoxes();
            }
        }
        // Empty the textboxes.
        private void ClearTextBoxes()
        {
            nameBox.Text = "";
            ipBox.Text = "";
            portBox.Text = "";
            pathBox.Text = "";
        }
        // Save the connections to file.
        private void SaveConnections()
        {
            try
            {
                connections.Serialize().Save(
                    Application.StartupPath + "/Connections.xml"
                    , System.Xml.Linq.SaveOptions.None);
            }
            catch (Exception)
            {
                MessageBox.Show("Could not open connectionfile");
            }
        }

        private bool IsPortValid(string port)
        {
            try
            {
                Int32.Parse(port);
                return true;
            }
            catch (Exception)
            {
                return false;
            }
        }

        #region Events
        // Saves all data and telling the Vizualizer where to connect.
        private void connect(object sender, EventArgs e)
        {
            if (SaveCurrentData())
            {
                string _name = nameBox.Text.Trim();
                AOSSettings.latestConnected = _name;
                SaveConnections();
                pressed(connectButton, new AOSConnectEventArgs(ipBox.Text, Int32.Parse(portBox.Text.Trim()), pathBox.Text));
                Close();
            }
            else if (ipBox.Text != "" && IsPortValid(portBox.Text.Trim()))
            {
                pressed(connectButton, new AOSConnectEventArgs(ipBox.Text, Int32.Parse(portBox.Text.Trim()), pathBox.Text));
            }
            else
            {
                MessageBox.Show("Please enter IP and port or press cancel");
            }
        }
        // Closes the window.
        private void cancelButton_Click(object sender, EventArgs e)
        {
            Close();
        }
        // Revmoves a connection from the list.
        private void removeButton_Click(object sender, EventArgs e)
        {
            if(savedConnections.SelectedIndices.Count == 1)
            {
                connections.unitconnections.RemoveAt(savedConnections.SelectedIndices[0]);
                savedConnections.VirtualListSize = connections.unitconnections.Count;

                savedConnections.SelectedIndices.Clear();
                savedConnections.SelectedIndices.Add(savedConnections.VirtualListSize - 1);

                UpdateTextBoxes();
            }
        }
        // Adds a connection from the list.
        private void newButton_Click(object sender, EventArgs e)
        {
            string name = "";
            name = InputDialogBox.Show("Enter a name", "").Trim();
            if (name.Length > 0)
            {
                UnitConn unit = new UnitConn(name, "127.0.0.1", "55182");

                connections.unitconnections.Add(unit);
                savedConnections.VirtualListSize = connections.unitconnections.Count;

                savedConnections.SelectedIndices.Clear();
                savedConnections.SelectedIndices.Add(savedConnections.VirtualListSize - 1);

                UpdateTextBoxes();
            }
        }
        // Saves the textboxes values to the selected connection.
        private void saveButton_Click(object sender, EventArgs e)
        {
            SaveCurrentData();
            savedConnections.Invalidate();
        }
        // Connects to the connection clicked on.
        private void savedConnections_DoubleClick(object sender, EventArgs e)
        {
            if (savedConnections.SelectedIndices.Count == 1)
            {
                if (SaveCurrentData())
                {
                    SaveConnections();
                    UnitConn u = connections.unitconnections[savedConnections.SelectedIndices[0]];
                    pressed(sender, new AOSConnectEventArgs(u.ip, Int32.Parse(portBox.Text.Trim()), pathBox.Text));
                    Hide();
                }
            }
        }
        #endregion

        private void browseButton_Click(object sender, EventArgs e)
        {
            OpenFileDialog od = new OpenFileDialog();
            od.DefaultExt = ".ags";
            od.Filter = "AOSGraphSetup file (*.ags)|*.ags|All files (*.*)|*.*";

            if (od.ShowDialog() == DialogResult.OK)
            {
                pathBox.Text = od.FileName;
            }            
        }
    }
}