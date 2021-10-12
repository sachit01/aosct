/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          RemoveGraphForm.cs %
*
*  %version:       2 %
*
*  %created_by:    lantback %
*
*  %date_created:  2012-08-02 12:46 %
*
*  DESCRIPTION:    Lists all graphs for the user and removes the choosen graphs.
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
    public partial class RemoveGraphForm : Form
    {
        public event AOSVisGraphAdded graphRemoved;
        private List<AOSGraph> graphs;
        
        public RemoveGraphForm(List<AOSGraph> _graphs)
        {
            InitializeComponent();
            graphs = _graphs;
            foreach (AOSGraph graph in _graphs)
            {
                string graphName = graph.Title;

                // Add the objects to the checklist
                string text = String.Format("{0}. {1}", (graphList.Items.Count+1).ToString(), graphName);
                graphList.Items.Add(text);
            }
        }
        // Closes the window.
        private void cancelButton_Click(object sender, EventArgs e)
        {
            Close();
        }
        // Remove every graph that is checked.
        private void removeButton_Click(object sender, EventArgs e)
        {
            for (int index = graphList.Items.Count - 1; index >= 0; index--)
            {
                if (graphList.CheckedIndices.Contains(index))
                {
                    AOSGraph graph = graphs[index];
                    graphRemoved(this, new AOSVisGraphEventArgs(graph));
                }
            }
            Close();
        }
    }
}