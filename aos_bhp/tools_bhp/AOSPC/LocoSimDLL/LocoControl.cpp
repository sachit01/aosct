#pragma once
#include "stdafx.h"
#pragma ident "@(#) Bombardier Transportation %full_filespec:  LocoControl.cpp-4:c++:arn_006#3 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          LocoControl.cpp %
*
*  %version:       4 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-03-20 17:40 %
*
*  DESCRIPTION: 
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2013-04-22    Antbäck     File created
* 2014-03-07    Antbäck     Imported to AOS-PC
* 2016-10-04    Marlundg    Changes in I/O for BHP
* 2017-03-10    Marlundg    Support for new I/O
* 2018-03-02    Marlundg    Handle the TCO Feedback
*
*******************************************************************************/


using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;

#include "LocoControl.h"

/******************************************************************************
* Function:     Tick
* Description:  
******************************************************************************/
void LocoSimDLL::LocoControl::Tick(enum TBSystemStatus  systemStatus,
                       int                      lsThrottle,
                       EnumATOMODE              lsATOSwitch,
                       EnumDriveDir             lsDriveDir,
                       bool                     lsCabin1Click,
                       bool                     lsCabin2Click,
                       bool                     lsLCSReadyClick,
                       bool                     lsEmerStopActive,
                       bool                     lsAOSOffClick,
                       bool                     lsNcuClick,
                       bool                     lsEb1aClick,
                       bool                     lsEb1bClick,
                       bool                     lsEb2aClick,
                       bool                     lsEb2bClick,
                       bool                     lsIsolAClick,
                       bool                     lsIsolBClick,
                       bool                     lsRoadMClick,
                       bool                     lsRailMClick,
                       LCSCom^                  lcsCom,
                       LocoSimDLL::RailDevCom^  rdCom,
                       EnumTcoFb                LocoSimTcoFeedbackChoice,
                       bool                     TcoOrdered,
                       bool                     EbOrdered,
                       UInt16                   tcoFbOffset)
{
    //if (TBSystemRunning == systemStatus)
    {
        // Set active controller and select ATOMode
        if (rdCom->RDConnected &&
            rdCom->RDDriverPanelActive)
        {
            LocoATOSwitch = rdCom->RDATOMode;
            if ((ATOSwitchSupv == LocoATOSwitch) ||
                (ATOSwitchAuto == LocoATOSwitch))
            {
                SelectedController = SelCtrlLCSRailDriver;
            }
            else
            {
                SelectedController = SelCtrlRailDriver;
            }
        }
        // else use LocoSim GUI
        else
        {
            LocoATOSwitch = lsATOSwitch;
            if ((ATOSwitchSupv == LocoATOSwitch) ||
                (ATOSwitchAuto == LocoATOSwitch))
            {
                SelectedController = SelCtrlLCS;
            }
            else
            {
                SelectedController = SelCtrlDriver;
            }
        }

        // Make sure that throttle is reset to zero when switching controllet
        if (oldSelectedController != SelectedController)
        {
            bool lcsCtrlActive    = (SelCtrlLCS == SelectedController) || 
                                    (SelCtrlLCSRailDriver == SelectedController);
            bool oldLcsCtrlActive = (SelCtrlLCS == oldSelectedController) || 
                                    (SelCtrlLCSRailDriver == oldSelectedController);

            if (lcsCtrlActive != oldLcsCtrlActive)
            {
                LocoThrottle = 0;
                LocoDriveDir = DDNeutral;
            }
            oldSelectedController = SelectedController;
        }

        // Use LocoSim GUI if not RailDriver
        if ((SelCtrlDriver == SelectedController) ||
            (SelCtrlLCS    == SelectedController))
        {
            // Handle activation of cabin 1 
            if (lsCabin1Click)
            {
                LocoCabin1 = !LocoCabin1;
            }

            // Handle activation of cabin 2 
            if (lsCabin2Click)
            {
              LocoCabin2 = !LocoCabin2;
            }

            // Handle AOSOff
            if (lsAOSOffClick ||
                rdCom->rdAOSPowerDownReq)
            {
                LocoAOSOff = !LocoAOSOff;
            }

            // Handle NCU
            if (lsNcuClick)
            {
                LocoNcu = !LocoNcu;
            }

            // Handle Eb1a Cut Out
            if (lsEb1aClick)
            {
                LocoEb1a = !LocoEb1a;
            }

            // Handle Eb1b Cut Out
            if (lsEb1bClick)
            {
                LocoEb1b = !LocoEb1b;
            }

            // Handle Eb2a Cut Out
            if (lsEb2aClick)
            {
                LocoEb2a = !LocoEb2a;
            }

            // Handle Eb2b Cut Out
            if (lsEb2bClick)
            {
                LocoEb2b = !LocoEb2b;
            }

            // Handle Isolation Switch
            if (lsIsolAClick)
            {
                LocoIsolA = !LocoIsolA;
            }
            if (lsIsolBClick)
            {
              LocoIsolB = !LocoIsolB;
            }

            // Handle RoadMode
            if (lsRoadMClick)
            {
              LocoRoadM = !LocoRoadM;
            }

            // Handle RailMode
            if (lsRailMClick)
            {
              LocoRailM = !LocoRailM;
            }

            // Handle the TCO Feedback depending on choice.           
            bool newLocoTcoFb;

            if (TcoOnlyFb == LocoSimTcoFeedbackChoice)
            {
              newLocoTcoFb = EbOrdered;
            }
            else if (TcoOrderAndFb == LocoSimTcoFeedbackChoice)
            {
              newLocoTcoFb = TcoOrdered;
            }
            else
            {
              newLocoTcoFb = false;
            }

            // Simulate feedback timing-offset, has order changed?
            if (newLocoTcoFb != LocoTcoFb)
            {
              // Check if time-offset has been reached, otherwise increase cycle counter
              if (tcoFbOffset > tcoFbOffsetCounter)
              {
                tcoFbOffsetCounter++;
              }
              else
              {
                tcoFbOffsetCounter = 0;
                LocoTcoFb = newLocoTcoFb;
              }
            }
  
        }
        else // RailDriver active
        {
            // Handle activation of cabin 
            LocoCabin1 = rdCom->RDDriverPanelActive;

            // Change later ....
            // Handle AOSOff
            if (lsAOSOffClick ||
                rdCom->rdAOSPowerDownReq)
            {
                LocoAOSOff = !LocoAOSOff;
            }

        }
        // Set outputs if cabin is active
        if (LocoCabin1 || LocoCabin2)
        {
            // Handle LCSReady
            if ((SelCtrlDriver == SelectedController) ||
                (SelCtrlLCS    == SelectedController))
            {
                // Use LocoSim GUI if not RailDriver
                if (lsLCSReadyClick)
                {
                    LocoLCSReady = !LocoLCSReady;
                }
            }
            else
            {
                // With RailDriver the LCSReady follows CabinOn
                LocoLCSReady = LocoCabin1 || LocoCabin2;
            }

            // Split handling depending on selected controller
            // LocoSim GUI driver controls activated
            if (SelectedController == SelCtrlDriver)
            {
                // Select driving direction
                LocoDriveDir = lsDriveDir; 

                // Handle throttle selection
                if (LocoLCSReady)
                {
                    LocoThrottle = lsThrottle;
                }
                // Do nothing, let driver play with controls if he wants ;)
            }
            // RailDriver activated
            else if (SelectedController == SelCtrlRailDriver)
            {
                // Select driving direction
                LocoDriveDir = rdCom->RDDriveDir; 

                // Handle throttle selection
                if (LocoLCSReady)
                {
                    LocoThrottle = rdCom->RDThrottle;
                }
                // Do nothing, let driver play with controls if he wants ;)
            }
            // LCS controls activated
            else if ((SelectedController == SelCtrlLCS) ||
                     (SelectedController == SelCtrlLCSRailDriver))
            {
                if (lcsCom->LCSConnected)
                {
                    // Select driving direction
                    if (lcsCom->LCSSelDrivDirCmd->dataValid)
                    {
                        LocoDriveDir = lcsCom->LCSSelDrivDirCmd->value; 
                    }

                    // Handle throttle selection
                    if (LocoLCSReady)
                    {
                        if (lcsCom->LCSSpeedCmd->dataValid)
                        {
                            if (lcsCom->LCSSpeedCmd->IncAcc)
                            {
                                LocoThrottle += 10;
                            }
                            else if (lcsCom->LCSSpeedCmd->IncAcc)
                            {
                                LocoThrottle -= 10;
                            }
                            else if (lcsCom->LCSSpeedCmd->IncAcc)
                            {
                                LocoThrottle = 0;
                            }
                        }
                        else if (lcsCom->LCSAbsAccCmd->dataValid)
                        {
                            LocoThrottle = lcsCom->LCSAbsAccCmd->value;
                        }
                    }
                }
                else
                {
                    LocoDriveDir = DDNeutral;
                    LocoThrottle = -10;
                }
            }
        }
        else
        {
            // Set default inactive values
            LocoLCSReady = false;
            LocoATOSwitch = ATOSwitchMan;
            LocoDriveDir = DDNeutral;
        }

        // Driver EB handling
        LocoEmerStopActive = lsEmerStopActive;
/*
        if (LocoEmerStopActive)
        {
            // Turn off LCSReady
            LocoLCSReady = false;
            // No throttle
            LocoThrottle = 0;
        }
*/
    }
    //else
    //{
    //    SelectedController = SelCtrlDriver;
    //    LocoLCSReady = false;
    //    LocoAOSOff = false;
    //    LocoDriverEB = false;
    //}

    // Always clear any LCSClient commands here !
    lcsCom->LCSSelDrivDirCmd->dataValid = false;
    lcsCom->LCSSpeedCmd->dataValid = false;
    lcsCom->LCSAbsAccCmd->dataValid = false;
}
