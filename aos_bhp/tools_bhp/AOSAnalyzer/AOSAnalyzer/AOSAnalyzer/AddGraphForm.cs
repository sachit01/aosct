/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          AddGraphForm.cs %
*
*  %version:       3 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:43 %
*
*  DESCRIPTION:    Lists all curves and properties avaialble to
*                  make a graph with to the user.
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
using ZedGraph;

namespace AOSAnalyzer
{
    public partial class AddGraphForm : Form
    {
        public event AOSVisGraphAdded graphAdded;
        public delegate void AOSVisGraphAdded(object sender, EventArgs e);
        private Dictionary<String, LineItem> curves;

        // Store the curves and add them to the listview.
        public AddGraphForm(List<LineItem> _curves)
        {
            InitializeComponent();
            curves = new Dictionary<string, LineItem>();
            if (_curves != null)
            {
                foreach (LineItem c in _curves)
                {
                    String curveName = c.Label.Text;
                    curves.Add(curveName, c);
                    curveSelection.Items.Add(curveName);
                }
            }
            curveSelection.AutoResizeColumns(ColumnHeaderAutoResizeStyle.HeaderSize);
        }

        // Close the window.
        private void cancelButton_Click(object sender, EventArgs e)
        {
            Close();
        }

        // Create an AOSGraph with the choosen properties and curves.
        // Raises the graphAdded-event with the graph as argument.
        // Closes the window.
        private void addGraphButton_Click(object sender, EventArgs e)
        {
            if (curveSelection.CheckedItems.Count > 0)
            {
                AOSGraph graph;
                if (Auto.Checked)
                {
                    graph = new AOSGraph(graphTitle.Text);
                }
                else
                {
                    int _graphmin = (int)graphMin.Value;
                    int _graphmax = (int)graphMax.Value;

                    // Swap min and max if max < min.
                    if (_graphmax < _graphmin)
                    {
                        graph = new AOSGraph(graphTitle.Text, _graphmin, _graphmax);
                    }
                    else
                    {
                        graph = new AOSGraph(graphTitle.Text, _graphmax, _graphmin);
                    }

                }

                foreach (ListViewItem _l in curveSelection.CheckedItems)
                {
                    String curvename = _l.SubItems[0].Text;
                    LineItem c = curves[curvename];
                    graph.AddCurve(curvename, c);
                }
                
                graphAdded(this, new AOSVisGraphEventArgs(graph));
            }
            Close();
        }

        // Enables/Disables textboxes
        private void Auto_CheckedChanged(object sender, EventArgs e)
        {
            graphMax.Enabled = !Auto.Checked;
            graphMin.Enabled = !Auto.Checked;
        }
    }
}