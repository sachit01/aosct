/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          CommentForm.cs %
*
*  %version:       3 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:43 %
*
*  DESCRIPTION:    Comment the graphs before saving the measurement to
*                  don't forget what you should analyse.
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

namespace AOSAnalyzer
{
    public partial class CommentForm : Form
    {
        List<AOSGraph> graphs;

        // Save the graphs and add them to the graphlist.
        public CommentForm(List<AOSGraph> _graphs)
        {
            InitializeComponent();
            graphs = _graphs;
            if(graphs != null && graphs.Count > 0)
            {            
                foreach (AOSGraph graph in graphs)
                {
                    graphList.Items.Add(graph);
                }
                graphList.SelectedIndex = 0;
            }
        }

        // Load the selected item's text to the textbox.
        private void graphList_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (graphList.SelectedItem != null)
            {
                commentField.Text = graphs[graphList.SelectedIndex].Comment;
            }
        }       
        // Save the current text to the selected item.
        private void saveButton_Click(object sender, EventArgs e)
        {
            if (graphList.SelectedItem != null)
            {
                ((AOSGraph)graphList.SelectedItem).Comment = commentField.Text;
            }
        }
        // Close the window.
        private void closeButton_Click(object sender, EventArgs e)
        {
            Close();
        }
    }
}
