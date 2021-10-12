/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          EditGraphForm.cs %
*
*  %version:       3 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:43 %
*
*  DESCRIPTION:    Gives the user the posibility to customize the graphs.
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
    public partial class EditGraphForm : Form
    {
        public event GraphdataChanged updatedGraph;
        private List<AOSGraph> graphs;
        private List<LineItem> curves;

        public EditGraphForm(List<LineItem> _curves, List<AOSGraph> _graphs)
        {
            InitializeComponent();
            curves = _curves;
            foreach (LineItem l in curves)
            {
                curveList.Items.Add(l.Label.Text);
            }

            graphs = _graphs;
            graphList.VirtualListSize = graphs.Count;
        }
        // Updates the textboxes.
        private void graphList_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (graphList.SelectedIndices.Count == 1)
            {
                AOSGraph graph = graphs[graphList.SelectedIndices[0]];
               
                graphTitle.Text = graph.Title;
                Auto.Checked = graph.IsAuto;
                if (graphMax.Value > graphMin.Value)
                {
                    graphMax.Value = (decimal)graph.GraphMax;
                    graphMin.Value = (decimal)graph.GraphMin;
                }
                else
                {
                    graphMax.Value = (decimal)graph.GraphMin;
                    graphMin.Value = (decimal)graph.GraphMax;
                }
                for (int i = 0; i < curveList.Items.Count; i++)
			    {
                    string curvename = curves[i].Label.Text;
                    curveList.SetItemChecked(i, graph.CointainsCurve(curvename));
			    }                
            }             
        }
        // Sets the properties and adds the selected curves to the graph.
        private void saveButton_Click(object sender, EventArgs e)
        {
            if(graphList.SelectedIndices.Count == 1 && curveList.CheckedItems.Count > 0)
            {
                AOSGraph graph = graphs[graphList.SelectedIndices[0]];
                graph.ClearCurves();
                
                foreach (int index in curveList.CheckedIndices.Cast<int>())
                {
                    LineItem c = curves[index];
                    graph.AddCurve(c.Label.Text, c);
                }

                graph.Title = graphTitle.Text;
                graph.IsAuto = Auto.Checked;
                if (!Auto.Checked)
                {
                    int _max = (int)graphMax.Value;
                    int _min = (int)graphMin.Value;
                    if(_max < _min)
                    {                    
                        graph.GraphMax = _min;
                        graph.GraphMin = _max;
                    }
                    else
                    {
                        graph.GraphMax = _max;
                        graph.GraphMin = _min;
                    }
                }
                OnUpdatedGraph();
            }
        }

        // Updates the graphList if the list changes from outside.
        public void UpdateGraphs(object sender, AOSGraphListEventArgs e)
        {
            graphs = e.graphs;
            graphList.VirtualListSize = e.graphs.Count;
            graphList.Refresh();
        }

        private void OnUpdatedGraph()
        {
            if (updatedGraph != null)
                updatedGraph();
        }
        // Closes the window.
        private void cancelButton_Click(object sender, EventArgs e)
        {
            Close();
        }
        // Enables/Disables textboxes.
        private void Auto_CheckedChanged(object sender, EventArgs e)
        {
            graphMax.Enabled = !Auto.Checked;
            graphMin.Enabled = !Auto.Checked;
        }
        
        private void graphList_RetrieveVirtualItem(object sender, RetrieveVirtualItemEventArgs e)
        {
            string s = String.Format("{0}. {1}", e.ItemIndex+1, graphs[e.ItemIndex]);
            e.Item = new ListViewItem(s);
        }
    }
}