/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          UnitParameterForm.cs %
*
*  %version:       3 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:43 %
*
*  DESCRIPTION:    Shows the units parameters to the user and 
*                  changes the parameters that the user tells it to do.
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
using System.Linq;
using System.Windows.Forms;

namespace AOSAnalyzer
{   
    public partial class UnitParameterForm : Form
    {
        AOSConnection aosconn;
        private SortedDictionary<String, ParamDescr> parameters;
        private SortedDictionary<String, ParameterChange> parameterchanges;
        /// <summary>
        /// Form for reading and changing parametervalues.
        /// </summary>
        /// <param name="_parameters">A dictionary of parameters to view/change.</param>
        /// <param name="_aosconn">AOSConnection to the Unit.</param>
        public UnitParameterForm(SortedDictionary<String, ParamDescr> _parameters
            , SortedDictionary<String, ParameterChange> _parameterchanges
            , AOSConnection _aosconn)
        {
            InitializeComponent();
            aosconn = _aosconn;
            parameters = _parameters;
            parameterchanges = _parameterchanges;
            changeList.VirtualListSize = parameterchanges.Count;
            sendButton.Enabled = !aosconn.Measuring;
            aosconn.measuringChanged += new MeasuringStatusChanged(MeasuringStatusChanged);
            UpdateValues();
        }
        // Send the change and re-read the parameters.
        private void SendParameter(ParamDescr _p)
        {
            if (parameters.ContainsKey(_p.Name))
            {
                aosconn.SetParameter(_p.Name, _p.Value);
                aosconn.GetParameters();
            }
        }

        #region ParameterList
        // Clear and insert the parameters in the ParameterList
        private void UpdateValues()
        {
            parameterList.Items.Clear();
            ListViewGroup state = new ListViewGroup("States");
            foreach (ParamDescr p in parameters.Values)
            {
                string[] temp = { p.Name, p.Value, p.Unit, p.Min, p.Max, p.Type };//, p.Description};
                ListViewItem l = new ListViewItem(temp);
                l.ToolTipText = p.Description;
                if (p.Type == "STATE")
                {
                    state.Items.Add(l);
                }
                parameterList.Items.Add(l);
            }
            parameterList.AutoResizeColumns(ColumnHeaderAutoResizeStyle.HeaderSize);
            //listView1.Groups.Add(state);
        }
        #endregion

        #region ChangeList
        // Method for updating the data in the ChangeList
        private void ChangesMade(string parameterName, string oldValue, string newValue)
        {
            if (oldValue != newValue)
            {
                if (parameterchanges.ContainsKey(parameterName))
                {
                    ParameterChange pc = parameterchanges[parameterName];
                    pc.newValue = newValue;
                    if (pc.newValue == pc.oldValue)
                    {
                        parameterchanges.Remove(parameterName);
                        changeList.VirtualListSize--;
                    }
                    else
                    {
                        parameterchanges[parameterName] = pc;
                    }
                }
                else
                {
                    ParameterChange pc = new ParameterChange(parameterName, oldValue, newValue);
                    parameterchanges.Add(parameterName, pc);
                    changeList.VirtualListSize = parameterchanges.Count;
                }
                changeList.AutoResizeColumns(ColumnHeaderAutoResizeStyle.HeaderSize);
            }
        }

        // Gives the ChangeList the item to display
        private void changeList_RetrieveVirtualItem(object sender, RetrieveVirtualItemEventArgs e)
        {
            try
            {
                ParameterChange pc = ((ParameterChange)parameterchanges.ElementAt(e.ItemIndex).Value);
                e.Item = new ListViewItem(pc.GetData());
            }
            catch
            {
                string[] temp = { "", "", "" };
                e.Item = new ListViewItem(temp);
            }

        }
        #endregion

        #region Events
        // Send the parameterchange to the AOSConnection and update the lists.
        private void sendButton_Click(object sender, EventArgs e)
        {
            if (parameterList.SelectedItems.Count == 1)
            {
                int index = parameterList.SelectedIndices[0];
                ParamDescr _p = parameters[parameterList.SelectedItems[0].Text];
                if (valueBox.Value > decimal.Parse(_p.Min) && valueBox.Value < decimal.Parse(_p.Max))
                {
                    string oldValue = _p.Value;
                    _p.Value = valueBox.Value.ToString();
                    SendParameter(_p);
                    ChangesMade(_p.Name, oldValue, _p.Value);
                    UpdateValues();
                }
                else
                {
                    MessageBox.Show(String.Format("Please enter a value between {0} and {1}", _p.Min, _p.Max));
                }
                parameterList.Items[index].Selected = true;
                valueBox.Focus();
            }
        }

        // Enable/Disable the Sendbutton
        public void MeasuringStatusChanged(AOSMeasuringStatusChangedEventArgs e)
        {
            sendButton.Enabled = !e.measuring;
        }
        
        private void changeList_DoubleClick(object sender, EventArgs e)
        {
            if (changeList.SelectedIndices.Count == 1)
            {
                int index = changeList.SelectedIndices[0];
                ParameterChange pc = ((ParameterChange)parameterchanges.ElementAt(index).Value);
                parameterList.FindItemWithText(pc.parameterName).Selected = true;
                valueBox.Focus();
                valueBox.Select(0, valueBox.Value.ToString().Length);
            }
        }

        private void parameterList_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (parameterList.SelectedIndices.Count == 1)
            {
                decimal d;
                decimal.TryParse(parameters[parameterList.SelectedItems[0].Text].Value, out d);
                valueBox.Value = d;
            }
        }

        private void parameterList_DoubleClick(object sender, EventArgs e)
        {
            if (parameterList.SelectedIndices.Count == 1)
            {
                decimal d;
                decimal.TryParse(parameters[parameterList.SelectedItems[0].Text].Value, out d);
                valueBox.Value = d;
                valueBox.Focus();
                valueBox.Select(0, valueBox.Value.ToString().Length);
            }
        }
        #endregion
    }
}
