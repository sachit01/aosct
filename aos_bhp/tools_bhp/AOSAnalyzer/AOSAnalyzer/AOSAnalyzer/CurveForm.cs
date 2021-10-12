/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          CurveForm.cs %
*
*  %version:       3 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:44 %
*
*  DESCRIPTION:    Gives the user possibility of changing color of the curves.
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
    public partial class CurveForm : Form
    {
        List<LineItem> curves;

        // Saves the curves and settingup the ListView.
        public CurveForm(List<LineItem> _curves)
        {
            InitializeComponent();
            curves = _curves;
            curveList.VirtualListSize = curves.Count;
            curveList.AutoResizeColumns(ColumnHeaderAutoResizeStyle.HeaderSize);
        }

        // Change the color of the colorpicker
        private void curveList_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (curveList.SelectedIndices.Count == 1)
            {
                pickColor.BackColor = curves[curveList.SelectedIndices[0]].Color;
            }
        }

        // Shows the colordialog and sets the new color to the curve.
        private void pickColor_Click(object sender, EventArgs e)
        {
            if (curveList.SelectedIndices.Count == 1)
            {
                ColorDialog colorDlg = new ColorDialog();
                colorDlg.AllowFullOpen = false;
                colorDlg.AnyColor = false;
                colorDlg.SolidColorOnly = true;

                if (colorDlg.ShowDialog() == DialogResult.OK)
                {
                    pickColor.BackColor = colorDlg.Color;
                    LineItem _l = curves[curveList.SelectedIndices[0]];
                    _l.Color = pickColor.BackColor;
                }
                curveList.Invalidate();
            }
        }

        // Closes the window.
        private void closeButton_Click(object sender, EventArgs e)
        {
            Close();
        }

        // Gives the correct item to the curveList based on the virtual index.
        private void curveList_RetrieveVirtualItem(object sender, RetrieveVirtualItemEventArgs e)
        {
            LineItem c = curves[e.ItemIndex];
            String[] row = {c.Label.Text, " "};
            ListViewItem l = new ListViewItem(row);
            l.UseItemStyleForSubItems = false;
            l.SubItems[1].BackColor = c.Color;
            e.Item = l;
        }

        // Shortcut to pick a color for a curve
        private void curveList_DoubleClick(object sender, EventArgs e)
        {
            if (curveList.SelectedIndices.Count == 1)
            {
                pickColor.PerformClick();
            }
        } 
    }
}