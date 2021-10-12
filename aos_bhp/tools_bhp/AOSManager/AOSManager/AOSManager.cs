/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*
*  later.
*
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-10-16    akushwah    Created
*******************************************************************************/
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Net;
using System.Net.Sockets;
using System.Collections.ObjectModel;
using System.Windows.Input;
using static System.Windows.Forms.ListViewItem;

namespace AOSManager
{

   public partial class AOSManager : Form
   {
      Controller controllerObj = null;

      /// <summary>
      /// Constructor
      /// </summary>
      public AOSManager()
      {
         InitializeComponent();
         controllerObj = new Controller();
         controllerObj.SetAppStatus += ControllerObj_SetAppStatus;
         listViewTrains.GridLines = true;
         
         for (int index = 0; index < controllerObj.trainCount; index++)
         {
            displayGUI("Train " + (index + 1));
         }
         
      }

      /// <summary>
      /// ControllerObj_SetAppStatus
      /// </summary>
      /// <param name="trainNo"></param>
      /// <param name="trainRunning"></param>
      public void ControllerObj_SetAppStatus(int trainNo, bool trainRunning)
      {
         try
         {
            Form.CheckForIllegalCrossThreadCalls = false;
            if (trainRunning)
            {
               listViewTrains.Items[trainNo-1].SubItems[1].Text = "Running";
            }
            else
            {
               listViewTrains.Items[trainNo-1].SubItems[1].Text = "Not Running";
            }
         }
         catch (Exception ex)
         {
            throw ex;
         }
      }

      /// <summary>
      /// displayGUI
      /// </summary>
      /// <param name="TrainName"></param>
      private void displayGUI(string TrainName)
      {
        var trainValue = new ListViewItem(new [] { TrainName, "Not Running" });
        listViewTrains.Items.Add(trainValue);
      }

      /// <summary>
      /// trainGUIvisibilty_RightClick
      /// </summary>
      /// <param name="sender"></param>
      /// <param name="e"></param>
      private void trainGUIvisibilty_RightClick(object sender, MouseEventArgs e)
      {
         if (e.Button == MouseButtons.Right)
         {
            if(listViewTrains.SelectedItems[0].SubItems[1].Text == "Running")
            SelectWindowOperation.Show(Cursor.Position);
         }
      }

      /// <summary>
      /// Minimize_Click
      /// </summary>
      /// <param name="sender"></param>
      /// <param name="e"></param>
      private void Minimize_Click(object sender, EventArgs e)
      {
         int selectedTrain= listViewTrains.SelectedItems[0].Index ;
         controllerObj.setCommandToSendAOSPC("minimize");
         controllerObj.writeCommandToAOSPC(selectedTrain);
      }

      /// <summary>
      /// Restore_Click
      /// </summary>
      /// <param name="sender"></param>
      /// <param name="e"></param>
      private void Restore_Click(object sender, EventArgs e)
      {
         int selectedTrain = listViewTrains.SelectedItems[0].Index;
         controllerObj.setCommandToSendAOSPC("restore");
         controllerObj.writeCommandToAOSPC(selectedTrain);
      }

   }
}

