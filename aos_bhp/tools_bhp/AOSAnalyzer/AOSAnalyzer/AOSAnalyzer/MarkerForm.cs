/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          MarkerForm.cs %
*
*  %version:       2 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:43 %
*
*  DESCRIPTION:    Lists all markers and gives you all values on the markers
*                  current position on the X-axis. 
*                  Supports to give the difference between all values on 
*                  respective markers position on the X-axis.
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
using System.Windows.Forms;
using ZedGraph;

namespace AOSAnalyzer
{
    public partial class MarkerForm : Form
    {
        public event AOSMarkerListChanged markerListChanged;
        public event AOSMarkerChanged markerChanged;

        private List<AOSMarker> markers;        
        private List<TypeDescr> types;
        private List<PointPairList> ppl;

        private AOSMarker currentMarker;

        private int signChanger;
        private int selectedIndex;

        public MarkerForm(List<AOSMarker> _markers, List<TypeDescr> _types, List<PointPairList> _ppl)
        {
            InitializeComponent();
            markers = _markers;
            types = _types;
            ppl = _ppl;
            selectedIndex = 0;
            signChanger = 1;
            UpdateMarkerList();
        }
        // Called when something changed outside the markerform.
        public void UpdateValues(List<TypeDescr> _types, List<PointPairList> _ppl)
        {
            types = _types;
            ppl = _ppl;
            UpdateMarkers();
        }
        // Clear the list and add the markers again.
        private void UpdateMarkerList()
        {
            int diffmarkerindex = diffMarkerList.SelectedIndex;

            markerList.Items.Clear();
            diffMarkerList.Items.Clear();
            foreach (AOSMarker marker in markers)
            {
                markerList.Items.Add(marker);
                diffMarkerList.Items.Add(marker);
            }
            if (markerList.Items.Count > selectedIndex)
            {
                markerList.SelectedIndex = selectedIndex;
            }
            if (diffMarkerList.Items.Count > diffmarkerindex)
            {
                diffMarkerList.SelectedIndex = diffmarkerindex;
            }
        }
        // Update all controls.
        private void UpdateMarkers()
        {
            UpdateMarkerList();
            RefreshValues(this, EventArgs.Empty);
        }
        // Update all controls.
        private void UpdateMarkers(object sender, EventArgs e)
        {
            UpdateMarkerList();
            RefreshValues(sender, e);
        }
        // Update all values in the listview.
        private void RefreshValues(object sender, EventArgs e)
        {
            dataView.Items.Clear();
            
            int x = (int)currentMarker.Location.X;

            if (currentMarker != null && ppl != null && ppl.Count > 0)
            {
                AOSMarker diffMarker = (AOSMarker)diffMarkerList.SelectedItem;

                ListViewItem l = new ListViewItem(new string[] { "X: ", x.ToString() });
                dataView.Items.Add(l);

                // Showing the difference between the markers.
                if (deltaCheckButton.Checked && diffMarker != null)
                {
                    int diffX = (int)diffMarker.Location.X;

                    for (int i = 0; i < types.Count; i++)
                    {
                        if (x < ppl[i].Count && diffX < ppl[i].Count)
                        {
                            l = new ListViewItem(new string[] { 
                                types[i].Name, 
                                Math.Round((ppl[i][x].Y - ppl[i][diffX].Y) * signChanger).ToString(), 
                                types[i].Unit});
                            dataView.Items.Add(l);
                        }
                    }
                }
                // Showing just the markervalues.
                else
                {
                    for (int i = 0; i < types.Count; i++)
                    {
                        if (x < ppl[i].Count)
                        {
                            l = new ListViewItem(new string[] { 
                                types[i].Name, 
                                Math.Round(ppl[i][x].Y).ToString(), 
                                types[i].Unit});
                            dataView.Items.Add(l);
                        }
                    }
                }
            }

            // Resize the columns.
            dataView.AutoResizeColumns(ColumnHeaderAutoResizeStyle.HeaderSize);
        }
        // Add a marker
        public void AddMarker(AOSMarker marker)
        {
            this.markers.Add(marker);
            UpdateMarkerList();
            markerListChanged(this, new AOSMarkerListEventArgs(markers));
        }
        // Shows the colordialog and sets the new color to the marker.
        // Notifies listeners that it has changed.
        private void pickColor_Click(object sender, EventArgs e)
        {
            if (markerList.SelectedItem != null)            
            {
                ColorDialog colorDlg = new ColorDialog();
                colorDlg.AllowFullOpen = false;
                colorDlg.AnyColor = false;
                colorDlg.SolidColorOnly = true;

                if (colorDlg.ShowDialog() == DialogResult.OK)
                {
                    pickColor.BackColor = colorDlg.Color;
                    AOSMarker _m = markers[markerList.SelectedIndex];
                    _m.Line.Color = pickColor.BackColor;
                    OnMarkerChanged();
                }
            }
        }
        // Changes the X-value on current marker and notify listeners that it has changed.
        private void xPos_ValueChanged(object sender, EventArgs e)
        {
            if (currentMarker != null)
            {
                currentMarker.Location.X = (int)xPos.Value;
                UpdateMarkers();
                OnMarkerChanged();
            }
        }
        // Changes the X-value on current diffmarker and notify listeners that it has changed.
        private void diffXPos_ValueChanged(object sender, EventArgs e)
        {
            if (diffMarkerList.SelectedItem != null)
            {
                AOSMarker marker = (AOSMarker)diffMarkerList.SelectedItem;
                marker.Location.X = (int)diffXPos.Value;
                UpdateMarkers();
                OnMarkerChanged();
            }
        }
        // Update all controls and refresh values.
        private void markerList_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (markerList.SelectedItem != null)
            {
                selectedIndex = markerList.SelectedIndex;
                currentMarker = markers[selectedIndex];
                xPos.Value = (decimal)currentMarker.Location.X;
                pickColor.BackColor = markers[markerList.SelectedIndex].Line.Color;
            }
        }
        // Update the values when changed diffmarker.
        private void diffMarkerList_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (diffMarkerList.SelectedItem != null)
            {
                AOSMarker marker = (AOSMarker)diffMarkerList.SelectedItem;
                diffXPos.Value = (decimal)marker.Location.X;
            }
            RefreshValues(this, EventArgs.Empty);
        }
        // Enables/Disables the diffmarkerlist and the signchanger.
        private void deltaCheckButton_CheckedChanged(object sender, EventArgs e)
        {
            diffMarkerList.Enabled = deltaCheckButton.Checked;
            signChangerBox.Enabled = deltaCheckButton.Checked;
            RefreshValues(this, EventArgs.Empty);
        }
        // Multiply signChanger with -1 to change the sign.
        private void signChangerBox_CheckedChanged(object sender, EventArgs e)
        {
            signChanger = signChanger * -1;
            RefreshValues(this, EventArgs.Empty);
        }

        // Add a marker to the list and notify listeners that the list has changed.
        private void addMarker_Click(object sender, EventArgs e)
        {
            // Give the marker a name
            string name = InputDialogBox.Show("Enter a name", "").Trim();
            name = name == "" ? "Marker" : name;

            // Create the marker and add it to the list.
            AOSMarker marker = new AOSMarker(name);
            markers.Add(marker);

            UpdateMarkerList();
            markerList.SelectedIndex = (markerList.Items.Count - 1);
            OnMarkerListChanged();
        }
        // Removes current marker from the list and notify listeners that the list has changed.
        private void removeMarker_Click(object sender, EventArgs e)
        {
            if (currentMarker != null)
            {
                markers.Remove(currentMarker); 
                UpdateMarkers();
                OnMarkerListChanged();                
            }
        }

        private void OnMarkerChanged()
        {
            if (markerChanged != null)
                markerChanged(this, new AOSMarkerEventArgs(currentMarker));
        }
        private void OnMarkerListChanged()
        {
            if (markerListChanged != null)
                markerListChanged(this, new AOSMarkerListEventArgs(markers));
        }

        
    }
}