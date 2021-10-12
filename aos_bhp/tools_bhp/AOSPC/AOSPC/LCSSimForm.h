#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          LCSSimForm.h %
*
*  %version:       9 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-07-12 18:40 %
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
* 2013-11-19    Antbäck     File created
* 2013-12-03    Antbäck     Added Alarm handling
* 2013-12-05    Antbäck     Force automatic power handling if automatic position handling is active
* 2013-12-07    Antbäck     Reworked form position handling
* 2014-03-07    Antbäck     Used statusStrip, same as in LocoSim
* 2014-03-23    Antbäck     Screen handling
* 2014-03-27    Antbäck     Only update components when content is changed
* 2013-03-27    Antbäck     Added panto simulation from file
* 2014-03-31    Hidaji      Fixed a bug for currAlarmImpactReducedOp2, Remove the alarm and stop sending it if it has been acknowledged
*                           Added "Operability OK" to the list of Alamr impacts, which means no impact at all
* 2013-04-03    Antbäck     Added DoubleBuffered = true
* 2014-07-02    Hidaji      Removed alarm impact from alarm box
* 2014-07-03    Hidaji      Changed Roof => Center to make it uniform with site data representation
* 2014-08-14    Bo H        Corrected position of checkbox LowBattery and form rearranged
* 2014-12-18    Antbäck     Reworked handling of LowBatt, added handling for CurrentStatus byte
* 2014-12-18    Antbäck     Corrected save of setups
* 2015-02-26    Antbäck     Added runtime access to PI-values
* 2017-02-27    Marlundg    Adapted for BHP Project
*
*******************************************************************************/

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Windows::Forms;
using namespace System::Data;
using namespace System::Drawing;
using namespace Microsoft::Win32;
using namespace System::Globalization;
using namespace System::Runtime::InteropServices;

using namespace LCSSimDLL;

#include "LCSSimConsts.h"

#define UPDATE_IF_DIFFERENT(a, b) { if (a != (b)) {a = (b);}}
#define MAX_LINES_DISPLAYED 3000

namespace AOSPC {

  /// <summary>
  /// Summary for LCSSimForm
  ///
  /// WARNING: If you change the name of this class, you will need to change the
  ///          'Resource File Name' property for the managed resource compiler tool
  ///          associated with all .resx files this class depends on.  Otherwise,
  ///          the designers will not be able to interact properly with localized
  ///          resources associated with this form.
  /// </summary>
  public ref class LCSSimForm : public System::Windows::Forms::Form
  {
  public:
    LCSSimDLL::LCSSimulation^   lcsSim;
  private:
    String^                     iniFileName;
    String^                     regRootKey;
    int                         formLeft, formTop;
    CultureInfo^                cultureInfo;
    bool                        showDebugTab;

  private: System::Windows::Forms::TextBox^  textBox1;
  private: System::Windows::Forms::TextBox^  textBox2;
  private: System::Windows::Forms::TextBox^  textBox3;
  private: System::Windows::Forms::TextBox^  textBox4;
  private: System::Windows::Forms::ToolStripButton^   tsbLSGreen;
  private: System::Windows::Forms::ToolStripButton^   tsbLSRed;

  private: System::Windows::Forms::ToolStrip^  toolStrip1;
  private: System::Windows::Forms::ToolStripButton^  tsbATOGreen;
  private: System::Windows::Forms::ToolStripButton^  tsbATORed;
  private: System::Windows::Forms::ToolStripSeparator^  toolStripSeparator2;
  private: System::Windows::Forms::ToolStripButton^  tsbLocoSimGreen;
  private: System::Windows::Forms::ToolStripButton^  tsbLocoSimRed;
  private: System::Windows::Forms::ToolStripButton^  tsbVSIMGreen;
  private: System::Windows::Forms::ToolStripButton^  tsbVSIMYellow;
  private: System::Windows::Forms::ToolStripButton^  tsbVSIMRed;
  private: System::Windows::Forms::ToolStripSeparator^  toolStripSeparator6;

  private: System::Windows::Forms::ColumnHeader^  columnHeader10;
  private: System::Windows::Forms::ColumnHeader^  columnHeader11;
  private: System::Windows::Forms::TabPage^  tpDebug;
  private: System::Windows::Forms::ListView^  lvDebug;
  private: System::Windows::Forms::ColumnHeader^  columnHeader12;
  private: System::Windows::Forms::TabPage^  tpDataToLocoSim;
  private: System::Windows::Forms::Label^  label14;
  private: System::Windows::Forms::ListView^  lvDataToLocoSim;
  private: System::Windows::Forms::ColumnHeader^  columnHeader5;
  private: System::Windows::Forms::ColumnHeader^  columnHeader6;
  private: System::Windows::Forms::TabPage^  tpDataFromLocoSim;
  private: System::Windows::Forms::Label^  label13;
  private: System::Windows::Forms::ListView^  lvDataFromLocoSim;
  private: System::Windows::Forms::ColumnHeader^  columnHeader3;
  private: System::Windows::Forms::ColumnHeader^  columnHeader4;
  private: System::Windows::Forms::TabPage^  tpDataToATO;
  private: System::Windows::Forms::Label^  label12;
  private: System::Windows::Forms::ListView^  lvDataToATO;
  private: System::Windows::Forms::ColumnHeader^  columnHeader1;
  private: System::Windows::Forms::ColumnHeader^  columnHeader2;
  private: System::Windows::Forms::TabPage^  tpDataFromATO;
  private: System::Windows::Forms::Label^  label11;
  private: System::Windows::Forms::ListView^  lvDataFromATO;
  private: System::Windows::Forms::ColumnHeader^  chItem;
  private: System::Windows::Forms::ColumnHeader^  chData;
  private: System::Windows::Forms::TabPage^  tpLCSBrakes;
  private: System::Windows::Forms::Button^  bApplyBrakes;
  private: System::Windows::Forms::GroupBox^  groupBox8;
  private: System::Windows::Forms::Label^  label41;
  private: System::Windows::Forms::Label^  label40;
  private: System::Windows::Forms::TextBox^  tbLastCarBrakePressureLCS;
  private: System::Windows::Forms::Label^  label33;
  private: System::Windows::Forms::TextBox^  tbPercentageOfOpBrakesECPBLCS;
  private: System::Windows::Forms::Label^  label32;
  private: System::Windows::Forms::GroupBox^  groupBox7;
  private: System::Windows::Forms::ComboBox^  cbTrainIntegrityStatusECPBLCS;
  private: System::Windows::Forms::Label^  label31;
  private: System::Windows::Forms::ComboBox^  cbECPBOperatingModeLCS;
  private: System::Windows::Forms::Label^  label30;
  private: System::Windows::Forms::ComboBox^  cbBrakeSystemInUseLCS;
  private: System::Windows::Forms::Label^  label29;
  private: System::Windows::Forms::ComboBox^  cbECPBSequenceStatusLCS;
  private: System::Windows::Forms::Label^  label27;
  private: System::Windows::Forms::TabPage^  tpLCSATO;
  private: System::Windows::Forms::Button^  bApplyAto;
  private: System::Windows::Forms::GroupBox^  groupBox5;
  private: System::Windows::Forms::ComboBox^  cbBlueFlagRequestLCS;
  private: System::Windows::Forms::Label^  label24;
  private: System::Windows::Forms::ComboBox^  cbBlueFlagStatusLCS;
  private: System::Windows::Forms::Label^  label23;
  private: System::Windows::Forms::GroupBox^  groupBox4;
  private: System::Windows::Forms::Label^  label39;
  private: System::Windows::Forms::TextBox^  tbADSEtaLCS;
  private: System::Windows::Forms::Label^  label111;
  private: System::Windows::Forms::ComboBox^  cbADSEtaStatusLCS;
  private: System::Windows::Forms::Label^  label25;
  private: System::Windows::Forms::ComboBox^  cbReadyForPrecisionStopLCS;
  private: System::Windows::Forms::Label^  label28;
  private: System::Windows::Forms::ComboBox^  cbLcsATOReadyLCS;
  private: System::Windows::Forms::Label^  label26;
  private: System::Windows::Forms::Label^  label22;
  private: System::Windows::Forms::ComboBox^  cbTractionModeLCS;
  private: System::Windows::Forms::Label^  label21;
  private: System::Windows::Forms::ComboBox^  cbATODrivingModeLCS;
  private: System::Windows::Forms::Label^  label20;
  private: System::Windows::Forms::ComboBox^  cbATOCabinSelectorStatusLCS;
  private: System::Windows::Forms::TabPage^  tpLCSFaults;
  private: System::Windows::Forms::Button^  bApplyFaults;
  private: System::Windows::Forms::GroupBox^  groupBox6;
  private: System::Windows::Forms::TextBox^  tbVersionADSMapMinorLCS;

  private: System::Windows::Forms::Label^  label38;
  private: System::Windows::Forms::Label^  label37;
  private: System::Windows::Forms::TextBox^  tbVersionADSMapMajorLCS;

  private: System::Windows::Forms::Label^  label36;
  private: System::Windows::Forms::TextBox^  tbTrainWeightTonsLCS;
  private: System::Windows::Forms::Label^  label34;
  private: System::Windows::Forms::Label^  label35;
  private: System::Windows::Forms::GroupBox^  groupBox3;
  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS31;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS30;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS29;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS28;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS27;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS26;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS25;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS24;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS23;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS22;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS21;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS20;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS19;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS18;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS17;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS16;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS15;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS14;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS13;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS12;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS11;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS10;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS9;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS8;

  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS7;
  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS6;
  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS5;
  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS4;
  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS3;
  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS2;
  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS1;
  private: System::Windows::Forms::CheckBox^  cbLocoSystemFaultsLCS0;
  private: System::Windows::Forms::TabPage^  tpTrainCompECPB;
private: System::Windows::Forms::Button^  bApplyTrainComp;
private: System::Windows::Forms::Button^  bSaveTrainComp;





  private: System::Windows::Forms::GroupBox^  groupBox14;
  private: System::Windows::Forms::TextBox^  tbRollingStockVehECP;
  private: System::Windows::Forms::TextBox^  tbVehNotDetected;
  private: System::Windows::Forms::Label^  label57;
  private: System::Windows::Forms::TextBox^  tbVehDetectedPosUnknown;
  private: System::Windows::Forms::Label^  label58;
  private: System::Windows::Forms::Label^  label59;
  private: System::Windows::Forms::DataGridView^  dgTrainCompECPB;



  private: System::Windows::Forms::TabPage^  tpTrainCompAOS;
  private: System::Windows::Forms::TextBox^  tbRollingStockVeh;
  private: System::Windows::Forms::Label^  label53;
  private: System::Windows::Forms::ListView^  lvTrainCompAOS;
  private: System::Windows::Forms::ColumnHeader^  columnHeader7;
  private: System::Windows::Forms::ColumnHeader^  columnHeader8;
  private: System::Windows::Forms::ColumnHeader^  columnHeader9;
  private: System::Windows::Forms::TabPage^  tpAOSStatus;
  private: System::Windows::Forms::CheckBox^  cbEnableSimulation;
  private: System::Windows::Forms::CheckBox^  cbEnableComLinks;
  private: System::Windows::Forms::GroupBox^  groupBox2;
  private: System::Windows::Forms::Label^  label19;


  private: System::Windows::Forms::TextBox^  tbTotalWeight;
  private: System::Windows::Forms::Label^  label15;
  private: System::Windows::Forms::TextBox^  tbHoldingBrake;
  private: System::Windows::Forms::Label^  label10;
  private: System::Windows::Forms::TextBox^  tbECPBTrainCompReq;
  private: System::Windows::Forms::Label^  label7;
  private: System::Windows::Forms::GroupBox^  groupBox1;
  private: System::Windows::Forms::Label^  label18;
  private: System::Windows::Forms::Label^  label17;
  private: System::Windows::Forms::Label^  label6;
  private: System::Windows::Forms::TextBox^  tbTrainOrientationFront;
  private: System::Windows::Forms::Label^  label5;
  private: System::Windows::Forms::TextBox^  tbTrainOrientationRear;
  private: System::Windows::Forms::Label^  label4;
  private: System::Windows::Forms::TextBox^  tbPositionOnTrackFront;
  private: System::Windows::Forms::TextBox^  tbPositionOnTrackRear;
  private: System::Windows::Forms::Label^  label3;
  private: System::Windows::Forms::TextBox^  tbTrackIdFront;
  private: System::Windows::Forms::TextBox^  tbTrackIdRear;
  private: System::Windows::Forms::Label^  label2;
  private: System::Windows::Forms::GroupBox^  groupBoxExample;
  private: System::Windows::Forms::TextBox^  tbBlueFlag;
  private: System::Windows::Forms::Label^  label9;
  private: System::Windows::Forms::TextBox^  tbTravelDirection;
  private: System::Windows::Forms::Label^  label8;
  private: System::Windows::Forms::TextBox^  tbTrainIdling;
  private: System::Windows::Forms::Label^  label1;
  private: System::Windows::Forms::TextBox^  tbATOCabinSelector;
  private: System::Windows::Forms::Label^  labelExample;
  private: System::Windows::Forms::TabControl^  tcMainControl;
  private: System::Windows::Forms::TabPage^  tpMovementAuthority;
  private: System::Windows::Forms::TabPage^  tpWarningCurve;

  private: System::Windows::Forms::GroupBox^  groupBox9;
  private: System::Windows::Forms::Label^  label42;



  private: System::Windows::Forms::TextBox^  tbEndOfMAPos;

  private: System::Windows::Forms::Label^  label44;
  private: System::Windows::Forms::TextBox^  tbDirection;

  private: System::Windows::Forms::Label^  label45;
  private: System::Windows::Forms::TextBox^  tbEndOfMATrkId;

  private: System::Windows::Forms::Label^  label46;
  private: System::Windows::Forms::TabPage^  tpPath;
  private: System::Windows::Forms::TextBox^  tbSpeedBeginPath;


  private: System::Windows::Forms::Label^  label47;
  private: System::Windows::Forms::Label^  label48;
  private: System::Windows::Forms::TextBox^  tbNextTrackId;


  private: System::Windows::Forms::Label^  label52;
  private: System::Windows::Forms::Label^  label51;
  private: System::Windows::Forms::TextBox^  tbReqTOA;

  private: System::Windows::Forms::TextBox^  tbNextTrackPos;

  private: System::Windows::Forms::Label^  label50;
  private: System::Windows::Forms::Label^  label49;


  private:

  internal:



  private: System::Windows::Forms::GroupBox^  groupBox10;
  private: System::Windows::Forms::GroupBox^  groupBox11;
  private: System::Windows::Forms::ListView^  lvPathSpeedChanges;

  private: System::Windows::Forms::ColumnHeader^  columnHeader19;
  private: System::Windows::Forms::ColumnHeader^  columnHeader20;
  private: System::Windows::Forms::ListView^  lvPathTracks;

  private: System::Windows::Forms::ColumnHeader^  columnHeader14;
  private: System::Windows::Forms::ColumnHeader^  columnHeader15;
  private: System::Windows::Forms::Label^  label54;
  private: System::Windows::Forms::Label^  label55;
  private: System::Windows::Forms::TextBox^  tbNumTracksPath;
  private: System::Windows::Forms::TextBox^  tbNumSpeedChanges;


  private: System::Windows::Forms::ColumnHeader^  columnHeader16;
  private: System::Windows::Forms::ColumnHeader^  columnHeader17;
  private: System::Windows::Forms::Label^  label56;
  private: System::Windows::Forms::GroupBox^  groupBox12;
  private: System::Windows::Forms::Label^  label60;
  private: System::Windows::Forms::TextBox^  tbMAMargin;
  private: System::Windows::Forms::Label^  label62;
  private: System::Windows::Forms::TextBox^  tbLimitedSupMode;

  private: System::Windows::Forms::Label^  label61;
  private: System::Windows::Forms::TextBox^  tbAOSVehicleSpeed;
  private: System::Windows::Forms::TextBox^  tbTCCVerADSMap;
  private: System::Windows::Forms::Label^  label16;
  private: System::Windows::Forms::Button^  button1;
  private: System::Windows::Forms::GroupBox^  groupBox13;
  private: System::Windows::Forms::Label^  label63;
  private: System::Windows::Forms::Label^  label64;
  private: System::Windows::Forms::TextBox^  textBox6;
  private: System::Windows::Forms::Label^  label65;
  private: System::Windows::Forms::TextBox^  textBox7;
  private: System::Windows::Forms::Label^  label66;
  private: System::Windows::Forms::GroupBox^  groupBox15;
  private: System::Windows::Forms::ComboBox^  comboBox1;
  private: System::Windows::Forms::Label^  label67;
  private: System::Windows::Forms::ComboBox^  comboBox2;
  private: System::Windows::Forms::Label^  label68;
  private: System::Windows::Forms::ComboBox^  comboBox3;
  private: System::Windows::Forms::Label^  label69;
  private: System::Windows::Forms::ComboBox^  comboBox4;
  private: System::Windows::Forms::Label^  label70;
  private: System::Windows::Forms::ComboBox^  cbPenaltyBrkActive;
  private: System::Windows::Forms::Label^  label83;
  private: System::Windows::Forms::GroupBox^  groupBox16;
  private: System::Windows::Forms::CheckBox^  cbADSBitFieldATPWarningDefined;
  private: System::Windows::Forms::CheckBox^  cbADSBitFieldWeightDefined;
  private: System::Windows::Forms::CheckBox^  cbADSBitFieldMADefined;
  private: System::Windows::Forms::CheckBox^  cbADSBitFieldTrainConfigDefined;
  private: System::Windows::Forms::CheckBox^  cbADSBitFieldPathDefined;


  private: System::Windows::Forms::Label^  label84;
  private: System::Windows::Forms::GroupBox^  groupBox17;
  private: System::Windows::Forms::TextBox^  textBox5;
  private: System::Windows::Forms::Label^  label86;
  private: System::Windows::Forms::TextBox^  textBox8;
  private: System::Windows::Forms::Label^  label87;


  private: System::Windows::Forms::GroupBox^  groupBox18;
  private: System::Windows::Forms::GroupBox^  groupBox19;
  private: System::Windows::Forms::TextBox^  textBox9;
  private: System::Windows::Forms::Label^  label76;
  private: System::Windows::Forms::TextBox^  textBox10;
  private: System::Windows::Forms::Label^  label77;
  private: System::Windows::Forms::Label^  label88;
private: System::Windows::Forms::Label^  label43;
private: System::Windows::Forms::TextBox^  tbNumCurvePoints;
private: System::Windows::Forms::DataGridView^  dgSpeedCurvePoints;
private: System::Windows::Forms::DataGridViewTextBoxColumn^  dgvTbcNumber;
private: System::Windows::Forms::DataGridViewTextBoxColumn^  dgvTbcTrackId;
private: System::Windows::Forms::DataGridViewTextBoxColumn^  dgvTbcPos;
private: System::Windows::Forms::DataGridViewTextBoxColumn^  dgvTbcSpeed;
private: System::Windows::Forms::DataGridViewTextBoxColumn^  No;
private: System::Windows::Forms::DataGridViewTextBoxColumn^  Column1;
private: System::Windows::Forms::DataGridViewComboBoxColumn^  Column3;
private: System::Windows::Forms::Button^  buttonImport;
private: System::Windows::Forms::Button^  buttonAppend;
private: System::Windows::Forms::TabPage^  RclInfo;
private: System::Windows::Forms::GroupBox^  groupBox20;
private: System::Windows::Forms::Label^  label71;
private: System::Windows::Forms::Label^  label72;
private: System::Windows::Forms::TextBox^  tbFwdDtg;

private: System::Windows::Forms::Label^  label73;
private: System::Windows::Forms::TextBox^  tbAllowedTrainMovement;

private: System::Windows::Forms::Label^  label74;
private: System::Windows::Forms::TextBox^  tbAosInterventionApplied;

private: System::Windows::Forms::Label^  label75;
private: System::Windows::Forms::TextBox^  tbAosOperationalMode;

private: System::Windows::Forms::Label^  label78;
private: System::Windows::Forms::Label^  label81;
private: System::Windows::Forms::TextBox^  tbTrainOrientation;

private: System::Windows::Forms::Label^  label80;
private: System::Windows::Forms::TextBox^  tbRevcDtg;


private: System::Windows::Forms::Label^  label79;
private: System::Windows::Forms::Label^  label82;
private: System::Windows::Forms::TextBox^  tbCurCeilingSpeed;
private: System::Windows::Forms::GroupBox^  groupBox21;



private: System::Windows::Forms::Label^  label90;

private: System::Windows::Forms::Button^  bApplyHandingDone;
private: System::Windows::Forms::ComboBox^  cbHandlingDone;

















  private: System::Windows::Forms::ColumnHeader^  columnHeader13;


  public:
    LCSSimForm(String^ regKey, String^ fileName, OBRDSimDLL::OBRDSimulation^ obrdSim)
    {
      InitializeComponent();
      this->DoubleBuffered = true;
      //
      //TODO: Add the constructor code here
      //
      iniFileName = fileName;
      regRootKey = regKey;
      lcsSim = gcnew LCSSimDLL::LCSSimulation(iniFileName, obrdSim);
      cultureInfo = gcnew CultureInfo("en-US");

      // Get ini file settings
      char *tmpIniFile = (char *)Marshal::StringToHGlobalAnsi(iniFileName).ToPointer();
      showDebugTab = GetPrivateProfileIntA("LCSSimForm", "ShowDebugTab", 0, tmpIniFile) != 0 ? true : false;

      // Get window position from registry
      int left = Convert::ToInt16(Registry::GetValue(regRootKey + "\\LCSSim", "Left", "32767"));
      int top = Convert::ToInt16(Registry::GetValue(regRootKey + "\\LCSSim", "Top", "32767"));
      // If registry not available, first run for example, use default position from windows
      if ((left != 32767) &&
        (top != 32767))
      {
        formLeft = left;
        formTop = top;
      }
    }

    /**********************************************************
    * Function:     Init
    * Description:
    **********************************************************/
    void Init(void)
    {
      lcsSim->InitModules();

      // Setup GUI list for communication
      ListViewItem^ itemPtr;

      lvDataFromATO->Items->Clear();
      for (int i = 0; i < lcsSim->atpCom->guiFromATPCnt; i++)
      {
        itemPtr = lvDataFromATO->Items->Add(lcsSim->atpCom->guiFromATPHeader[i]); itemPtr->SubItems->Add("");
      }
      lvDataToATO->Items->Clear();
      for (int i = 0; i < lcsSim->atpCom->guiToATPCnt; i++)
      {
        itemPtr = lvDataToATO->Items->Add(lcsSim->atpCom->guiToATPHeader[i]); itemPtr->SubItems->Add("");
      }
      if (lcsSim->useLocoSim)
      {
        lvDataFromLocoSim->Items->Clear();
        for (int i = 0; i < lcsSim->lsCom->guiFromLSCnt; i++)
        {
          itemPtr = lvDataFromLocoSim->Items->Add(lcsSim->lsCom->guiFromLSHeader[i]); itemPtr->SubItems->Add("");
        }
        lvDataToLocoSim->Items->Clear();
        for (int i = 0; i < lcsSim->lsCom->guiToLSCnt; i++)
        {
          itemPtr = lvDataToLocoSim->Items->Add(lcsSim->lsCom->guiToLSHeader[i]); itemPtr->SubItems->Add("");
        }
      }
      else if (lcsSim->useVSIM)
      {
        lvDataFromLocoSim->Items->Clear();
        for (int i = 0; i < lcsSim->vsimCom->guiFromVSIMCnt; i++)
        {
          itemPtr = lvDataFromLocoSim->Items->Add(lcsSim->vsimCom->guiFromVSIMHeader[i]); itemPtr->SubItems->Add("");
        }
        lvDataToLocoSim->Items->Clear();
        for (int i = 0; i < lcsSim->vsimCom->guiToVSIMCnt; i++)
        {
          itemPtr = lvDataToLocoSim->Items->Add(lcsSim->vsimCom->guiToVSIMHeader[i]); itemPtr->SubItems->Add("");
        }
      }

      // Set default values from simulator
      cbEnableComLinks->Checked = true;

      UPDATE_IF_DIFFERENT(cbATOCabinSelectorStatusLCS->SelectedIndex, (ATOModeCabinSelectorStatusEnum::ATOModeCSSUndefined != lcsSim->currATOModeCabinSelectorStatus) ? lcsSim->currATOModeCabinSelectorStatus : 3);
      UPDATE_IF_DIFFERENT(cbATODrivingModeLCS->SelectedIndex, (ATODrivingModeEnum::ATODrivingModeNotAsserted != lcsSim->currATODrivingMode) ? lcsSim->currATODrivingMode : 6);
      UPDATE_IF_DIFFERENT(cbTractionModeLCS->SelectedIndex, lcsSim->currFreeRollingStatus);
      UPDATE_IF_DIFFERENT(cbBlueFlagStatusLCS->SelectedIndex, lcsSim->currBlueFlagStatus);
      UPDATE_IF_DIFFERENT(cbLcsATOReadyLCS->SelectedIndex, (LCSATOReadyEnum::LCSATONotAsserted != lcsSim->currLCSATOReady) ? lcsSim->currLCSATOReady : 2);
      UPDATE_IF_DIFFERENT(cbECPBSequenceStatusLCS->SelectedIndex, lcsSim->currECPBSequenceStatus);
      UPDATE_IF_DIFFERENT(cbBlueFlagRequestLCS->SelectedIndex, lcsSim->currBlueFlagRequest);
      UPDATE_IF_DIFFERENT(cbADSEtaStatusLCS->SelectedIndex, lcsSim->currADSETAStatus);
      UPDATE_IF_DIFFERENT(tbADSEtaLCS->Text, lcsSim->currADSETA.ToString());
      UPDATE_IF_DIFFERENT(cbReadyForPrecisionStopLCS->SelectedIndex, lcsSim->currReadyForPrecisionStop);
      UPDATE_IF_DIFFERENT(cbBrakeSystemInUseLCS->SelectedIndex, lcsSim->currBrakeSystemInUse);
      UPDATE_IF_DIFFERENT(cbECPBOperatingModeLCS->SelectedIndex, (ECPBOperatingModeEnum::ECPBOperatingModeNotAvailable != lcsSim->currECPBOperatingMode) ? lcsSim->currECPBOperatingMode : 4);
      UPDATE_IF_DIFFERENT(cbTrainIntegrityStatusECPBLCS->SelectedIndex, (TrainIntegrityStatusECPBEnum::TrainIntegrityStatusECPBNotAsserted != lcsSim->currTrainIntegrityStatusECPB) ? lcsSim->currTrainIntegrityStatusECPB : 2);
      UPDATE_IF_DIFFERENT(cbPenaltyBrkActive->SelectedIndex, lcsSim->currLocoPenaltyBrkActive);

      UPDATE_IF_DIFFERENT(tbPercentageOfOpBrakesECPBLCS->Text, lcsSim->currPercentageOfOpBrakesECPB.ToString());
      UPDATE_IF_DIFFERENT(tbLastCarBrakePressureLCS->Text, lcsSim->currLastCarBrakePressure.ToString());

      UPDATE_IF_DIFFERENT(tbVersionADSMapMajorLCS->Text, (lcsSim->currVersionADSMap & 0xFF00 >> 8).ToString());
      UPDATE_IF_DIFFERENT(tbVersionADSMapMinorLCS->Text, (lcsSim->currVersionADSMap & 0x00FF).ToString());
      UPDATE_IF_DIFFERENT(tbTrainWeightTonsLCS->Text, lcsSim->currEstTrainWeightTons.ToString());
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS0->Checked, (lcsSim->currLocoSysFault & (1 << 0)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS1->Checked, (lcsSim->currLocoSysFault & (1 << 1)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS2->Checked, (lcsSim->currLocoSysFault & (1 << 2)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS3->Checked, (lcsSim->currLocoSysFault & (1 << 3)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS4->Checked, (lcsSim->currLocoSysFault & (1 << 4)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS5->Checked, (lcsSim->currLocoSysFault & (1 << 5)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS6->Checked, (lcsSim->currLocoSysFault & (1 << 6)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS7->Checked, (lcsSim->currLocoSysFault & (1 << 7)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS8->Checked, (lcsSim->currLocoSysFault & (1 << 8)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS9->Checked, (lcsSim->currLocoSysFault & (1 << 9)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS10->Checked, (lcsSim->currLocoSysFault & (1 << 10)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS11->Checked, (lcsSim->currLocoSysFault & (1 << 11)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS12->Checked, (lcsSim->currLocoSysFault & (1 << 12)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS13->Checked, (lcsSim->currLocoSysFault & (1 << 13)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS14->Checked, (lcsSim->currLocoSysFault & (1 << 14)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS15->Checked, (lcsSim->currLocoSysFault & (1 << 15)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS16->Checked, (lcsSim->currLocoSysFault & (1 << 16)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS17->Checked, (lcsSim->currLocoSysFault & (1 << 17)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS18->Checked, (lcsSim->currLocoSysFault & (1 << 18)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS19->Checked, (lcsSim->currLocoSysFault & (1 << 19)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS20->Checked, (lcsSim->currLocoSysFault & (1 << 20)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS21->Checked, (lcsSim->currLocoSysFault & (1 << 21)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS22->Checked, (lcsSim->currLocoSysFault & (1 << 22)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS23->Checked, (lcsSim->currLocoSysFault & (1 << 23)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS24->Checked, (lcsSim->currLocoSysFault & (1 << 24)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS25->Checked, (lcsSim->currLocoSysFault & (1 << 25)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS26->Checked, (lcsSim->currLocoSysFault & (1 << 26)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS27->Checked, (lcsSim->currLocoSysFault & (1 << 27)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS28->Checked, (lcsSim->currLocoSysFault & (1 << 28)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS29->Checked, (lcsSim->currLocoSysFault & (1 << 29)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS30->Checked, (lcsSim->currLocoSysFault & (1 << 30)) == 1);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS31->Checked, (lcsSim->currLocoSysFault & (1 << 31)) == 1);

      //ECPB Train Composition
      ProcessECPBTrainComposition(iniFileName, false);
      
      bApplyTrainComp->Enabled = false;
      bSaveTrainComp->Enabled = false;
      
      // Path
      lvPathTracks->Items->Clear();
      lvPathSpeedChanges->Items->Clear();

      UPDATE_IF_DIFFERENT(tbNumTracksPath->Text, lcsSim->currNumTracks.ToString());
      UPDATE_IF_DIFFERENT(tbSpeedBeginPath->Text, lcsSim->currSpeedBeginPath.ToString());
      UPDATE_IF_DIFFERENT(tbNumSpeedChanges->Text, lcsSim->currNumSpeedChanges.ToString());
      UPDATE_IF_DIFFERENT(tbNextTrackId->Text, lcsSim->currNextTragetTrackId.ToString());
      UPDATE_IF_DIFFERENT(tbNextTrackPos->Text, lcsSim->currNextTargetPos.ToString());
      UPDATE_IF_DIFFERENT(tbReqTOA->Text, lcsSim->currReqTOANextTarget.ToString());
      UPDATE_IF_DIFFERENT(tbTCCVerADSMap->Text, lcsSim->atpCom->recvTCCVerADSMap.ToString());

      // ATP Command
      tbECPBTrainCompReq->Clear();
      switch (lcsSim->currECPBTrainCompReq)
      {
      case NotRequested: UPDATE_IF_DIFFERENT(tbECPBTrainCompReq->Text, "Not Requested"); break;
      case Requested: UPDATE_IF_DIFFERENT(tbECPBTrainCompReq->Text, "Requested"); break;
      case NotAsserted: UPDATE_IF_DIFFERENT(tbECPBTrainCompReq->Text, "Not Asserted"); break;
      default: UPDATE_IF_DIFFERENT(tbECPBTrainCompReq->Text, "- Invalid Value -"); break;
      }

      tbHoldingBrake->Clear();
      switch (lcsSim->currHoldingBrake)
      {
      case NotRequested: UPDATE_IF_DIFFERENT(tbHoldingBrake->Text, "Not Requested"); break;
      case Requested: UPDATE_IF_DIFFERENT(tbHoldingBrake->Text, "Requested"); break;
      case NotAsserted: UPDATE_IF_DIFFERENT(tbHoldingBrake->Text, "Not Asserted"); break;
      default: UPDATE_IF_DIFFERENT(tbHoldingBrake->Text, "- Invalid Value -"); break;
      }

      UPDATE_IF_DIFFERENT(tbTotalWeight->Text, lcsSim->currTrainWeight.ToString());

      // Movement Authority
      tbEndOfMATrkId->Clear();
      UPDATE_IF_DIFFERENT(tbEndOfMATrkId->Text, lcsSim->currEndOfMATrkId.ToString());

      tbEndOfMAPos->Clear();
      UPDATE_IF_DIFFERENT(tbEndOfMAPos->Text, lcsSim->currEndOfMAPos.ToString());

      switch (lcsSim->currMADirection)
      {
      case LocoLeading: UPDATE_IF_DIFFERENT(tbDirection->Text, "Loco Leading"); break;
      case LocoTrailing: UPDATE_IF_DIFFERENT(tbDirection->Text, "Loco Trailing"); break;
      case Undefined: UPDATE_IF_DIFFERENT(tbDirection->Text, "Undefined"); break;
      default:UPDATE_IF_DIFFERENT(tbDirection->Text, "- Invalid Value -"); break;
      }

      UPDATE_IF_DIFFERENT(tbMAMargin->Text, lcsSim->currMAMargin.ToString());
      
      // Rcl Status
      UPDATE_IF_DIFFERENT(cbHandlingDone->SelectedIndex, lcsSim->currHandlingDone);
      // Rcl Information
      UPDATE_IF_DIFFERENT(tbAosOperationalMode->Text, "Undefined"); 
      UPDATE_IF_DIFFERENT(tbAosInterventionApplied->Text, "Not Applied");
      UPDATE_IF_DIFFERENT(tbAllowedTrainMovement->Text, "Undefined");
      UPDATE_IF_DIFFERENT(tbFwdDtg->Text, "-1");
      UPDATE_IF_DIFFERENT(tbRevcDtg->Text, "-1");
      UPDATE_IF_DIFFERENT(tbTrainOrientation->Text, "Undefined");
      UPDATE_IF_DIFFERENT(tbCurCeilingSpeed->Text, "0");


    }

    /**********************************************************
    * Function:     SaveWindowSettings
    * Description:
    **********************************************************/
    void SaveWindowSettings(void)
    {
      Registry::SetValue(regRootKey + "\\LCSSim", "Left", String::Format("{0:0}", this->Left));
      Registry::SetValue(regRootKey + "\\LCSSim", "Top", String::Format("{0:0}", this->Top));
      Registry::SetValue(regRootKey + "\\LCSSim", "Visible", this->Visible ? "1" : "0");
    }

    /**********************************************************
    * Function:     ToggleVisibility
    * Description:
    **********************************************************/
    void SetVisibility(bool visible)
    {
      if (visible)
      {
        this->Visible = true;
        this->Left = formLeft;
        this->Top = formTop;
      }
      else
      {
        this->Visible = false;
        formLeft = this->Left;
        formTop = this->Top;
      }
    }

    /**********************************************************
    * Function:     UpdateECPBTrainCompFromGUI
    * Description:  Update simulator values from GUI
    **********************************************************/
    void UpdateECPBTrainCompFromGUI()
    {
      // Number of vehicles must be more than 0.
      if ((dgTrainCompECPB->RowCount - 1) > 0)
      {
        // Get the number of vehicles from row-count
        UPDATE_IF_DIFFERENT(lcsSim->currNumberOfVehicles, (dgTrainCompECPB->RowCount - 1));
        UPDATE_IF_DIFFERENT(tbRollingStockVehECP->Text, lcsSim->currNumberOfVehicles.ToString());

        UPDATE_IF_DIFFERENT(lcsSim->currVehDetectedPosUnknown, Convert::ToUInt16(tbVehDetectedPosUnknown->Text));
        UPDATE_IF_DIFFERENT(lcsSim->currVehNotDetected, Convert::ToUInt16(tbVehNotDetected->Text));

        for (int i = 0; i < dgTrainCompECPB->RowCount - 1; i++)
        {
          UPDATE_IF_DIFFERENT(lcsSim->currRoadNumber[i], Convert::ToUInt16(dgTrainCompECPB->Rows[i]->Cells[1]->Value));
          
          // If no vehicle type is chosen, select VehicleTypeUnknown
          if (dgTrainCompECPB->Rows[i]->Cells[2]->Value == nullptr)
          {
            lcsSim->currVehicleType[i] = VehicleTypeUnknown;
          }
          else
          {
            String^ x = dgTrainCompECPB->Rows[i]->Cells[2]->Value->ToString();
            if (x == "Locomotive")
            {
              lcsSim->currVehicleType[i] = VehicleTypeLocomotive;
            }
            else if (x == "Car")
            {
              lcsSim->currVehicleType[i] = VehicleTypeCar;
            }
            else
            {
              lcsSim->currVehicleType[i] = VehicleTypeUnknown;
            }
          }
        }

        // All data is transfered from View(GUI) to Model(lcssim) ->Disable 'Apply' button and Enable 'Save' button
        bApplyTrainComp->Enabled = false;
        bSaveTrainComp->Enabled = true;
      }
      else
      {
        System::Windows::Forms::MessageBox::Show("The number of vehicles shall be > 0.",
          "Error in params",
          (MessageBoxButtons)0,
          MessageBoxIcon::Error);
      }
    }


    /**********************************************************
    * Function:     ProcessECPBTrainComposition
    * Description:  Update Train Composition GUI from Ini file
    *               Argument 'append' will append the train-composition 
    *               defined in the iniFileName to the existing (and renumber) 
    **********************************************************/
    bool ProcessECPBTrainComposition(String ^iniFileName, bool append)
    {

      char *tmpIniFile = (char *)Marshal::StringToHGlobalAnsi(iniFileName).ToPointer();

      int startIndex = 0;

      if (append)
      {
        // Keep existing vehicles and append to the end
        // Already one row extra for "new" record
        startIndex = dgTrainCompECPB->Rows->Count - 1;
      }
      else
      {
        // replace existing vehicles
        dgTrainCompECPB->Rows->Clear();
      }

      int i = 0;
      bool endOfVehicles = false;
      while (!endOfVehicles)
      {
        String^ roadNumber("RoadNumber_");
        String^ vehicleType("VehicleType_");

        roadNumber += (i + 1);
        vehicleType += (i + 1);

        // Parameter-names
        char *tmpRoadNumberParam = (char *)Marshal::StringToHGlobalAnsi(roadNumber).ToPointer();
        char *tmpVehicleTypeParam = (char *)Marshal::StringToHGlobalAnsi(vehicleType).ToPointer();

        // Values
        unsigned short tmpRoadNumber = GetPrivateProfileIntA("LCSTrainComp", tmpRoadNumberParam, 0, tmpIniFile);
        unsigned short tmpVehicleType = GetPrivateProfileIntA("LCSTrainComp", tmpVehicleTypeParam, VehicleTypeNotDefined, tmpIniFile);

        if (tmpRoadNumber > 0)
        {
          // Add a row for the vehicle
          dgTrainCompECPB->Rows->Add(1);
          dgTrainCompECPB->Rows[i + startIndex]->Cells[0]->Value = i + startIndex + 1;
          dgTrainCompECPB->Rows[i + startIndex]->Cells[1]->Value = tmpRoadNumber;
          switch (tmpVehicleType)
          {
          case VehicleTypeUnknown:     dgTrainCompECPB->Rows[i + startIndex]->Cells[2]->Value = "Unknown"; break;
          case VehicleTypeLocomotive:  dgTrainCompECPB->Rows[i + startIndex]->Cells[2]->Value = "Locomotive"; break;
          case VehicleTypeCar:         dgTrainCompECPB->Rows[i + startIndex]->Cells[2]->Value = "Car"; break;
          default:                  dgTrainCompECPB->Rows[i + startIndex]->Cells[2]->Value = "- Invalid Value -"; break;
          }
          i++;
        }
        else
        {
          endOfVehicles = true;
        }
      }
      lcsSim->currNumberOfVehicles = i + startIndex;
      UPDATE_IF_DIFFERENT(tbRollingStockVehECP->Text, lcsSim->currNumberOfVehicles.ToString());
      UPDATE_IF_DIFFERENT(tbVehDetectedPosUnknown->Text, lcsSim->currVehDetectedPosUnknown.ToString());
      UPDATE_IF_DIFFERENT(tbVehNotDetected->Text, lcsSim->currVehNotDetected.ToString());

      return (i > 0) ? true : false;
    }


    /**********************************************************
    * Function:     UpdateECPBTrainCompFromGUI
    * Description:  Update simulator values from GUI
    **********************************************************/
    void UpdateTrainCompECPBRowNumber()
    {
      for (int i = 0; i < dgTrainCompECPB->RowCount - 1; i++)
      {
        UPDATE_IF_DIFFERENT(dgTrainCompECPB->Rows[i]->Cells[0]->Value, i + 1);
      }
    }

    void SaveTrainCompValues()
    {
      if (!bApplyTrainComp->Enabled)
      {
        // Store only if accepted by Apply
        lcsSim->SaveTrainCompToIniFile();
        
        bSaveTrainComp->Enabled = false;
      }     
    }

    /**********************************************************
    * Function:     UpdateSimValuesFromGUI
    * Description:  Update simulator values from GUI, and check valid range
    **********************************************************/
    void UpdateSimValuesFromGUI()
    {
      String^            ErrorMsg = "";
      System::Object^    ErrorCtrl = nullptr;

      // Use entered GUI values if simulation is not enabled (otherwise the simulation will generate these values)
      if (!cbEnableSimulation->Checked)
      {
        lcsSim->currATODrivingMode = (ATODrivingModeEnum)cbATODrivingModeLCS->SelectedIndex <= ATODrivingModeEnum::ATODrivingModeUnloading ?
          (ATODrivingModeEnum)cbATODrivingModeLCS->SelectedIndex : ATODrivingModeEnum::ATODrivingModeNotAsserted;

        lcsSim->currFreeRollingStatus = (FreeRollingStatusEnum)cbTractionModeLCS->SelectedIndex;
        lcsSim->currBlueFlagStatus = (BlueFlagStatusEnum)cbBlueFlagStatusLCS->SelectedIndex;
        lcsSim->currADSETAStatus = (AdsEtaStatusEnum)cbADSEtaStatusLCS->SelectedIndex;
        lcsSim->currLCSATOReady = (LCSATOReadyEnum)cbLcsATOReadyLCS->SelectedIndex;

        lcsSim->currLCSATOReady = (LCSATOReadyEnum)cbLcsATOReadyLCS->SelectedIndex <= LCSATOReadyEnum::LCSATOReady ?
          (LCSATOReadyEnum)cbLcsATOReadyLCS->SelectedIndex : LCSATOReadyEnum::LCSATONotAsserted;

        lcsSim->currECPBSequenceStatus = (ECPBSequenceStatusEnum)cbECPBSequenceStatusLCS->SelectedIndex;

        lcsSim->currReadyForPrecisionStop = (ReadyForPrecisionStopEnum)cbReadyForPrecisionStopLCS->SelectedIndex;

        lcsSim->currBrakeSystemInUse = (BrakeSystemInUseEnum)cbBrakeSystemInUseLCS->SelectedIndex;

        lcsSim->currECPBOperatingMode = (ECPBOperatingModeEnum)cbECPBOperatingModeLCS->SelectedIndex <= ECPBOperatingModeEnum::ECPBOperatingModeCutOut ?
          (ECPBOperatingModeEnum)cbECPBOperatingModeLCS->SelectedIndex : ECPBOperatingModeEnum::ECPBOperatingModeNotAvailable;

      }
      try {

        // Values below will not be simulated i e only set via GUI.
        // In some cases the GUI value (selected index) must be converted to the simulated value.
        // (The selected index will be 0..X in a sequence. Sometimes the last simulated value shall be '255'.)
        lcsSim->currATOModeCabinSelectorStatus = (ATOModeCabinSelectorStatusEnum)cbATOCabinSelectorStatusLCS->SelectedIndex <= ATOModeCabinSelectorStatusEnum::ATOModeCSSAutomatic ?
          (ATOModeCabinSelectorStatusEnum)cbATOCabinSelectorStatusLCS->SelectedIndex : ATOModeCabinSelectorStatusEnum::ATOModeCSSUndefined;

        lcsSim->currBlueFlagRequest = (BlueFlagRequestEnum)cbBlueFlagRequestLCS->SelectedIndex;

        ErrorMsg = "LeaderEta ";
        ErrorCtrl = (System::Object^)tbADSEtaLCS;
        lcsSim->currADSETA = Convert::ToUInt32(tbADSEtaLCS->Text);

        lcsSim->currTrainIntegrityStatusECPB = (TrainIntegrityStatusECPBEnum)cbTrainIntegrityStatusECPBLCS->SelectedIndex <= TrainIntegrityStatusECPBEnum::TrainIntegrityStatusECPBConfirmed ?
          (TrainIntegrityStatusECPBEnum)cbTrainIntegrityStatusECPBLCS->SelectedIndex : TrainIntegrityStatusECPBEnum::TrainIntegrityStatusECPBNotAsserted;

        ErrorMsg = "Percentage Of Op BrakesECP ";
        ErrorCtrl = (System::Object^)tbPercentageOfOpBrakesECPBLCS;
        lcsSim->currPercentageOfOpBrakesECPB = Convert::ToByte(tbPercentageOfOpBrakesECPBLCS->Text);

        ErrorMsg = "Last Car Brake Pressure ";
        ErrorCtrl = (System::Object^)tbLastCarBrakePressureLCS;
        lcsSim->currLastCarBrakePressure = Convert::ToByte(tbLastCarBrakePressureLCS->Text);

        ErrorMsg = "Version ADSMap Major ";
        ErrorCtrl = (System::Object^)tbVersionADSMapMajorLCS;
        unsigned char tempADSMapMajor = Convert::ToByte(tbVersionADSMapMajorLCS->Text);

        ErrorMsg = "Version ADSMap Minor ";
        ErrorCtrl = (System::Object^)tbVersionADSMapMinorLCS;
        unsigned char tempADSMapMinor = Convert::ToByte(tbVersionADSMapMinorLCS->Text);

        lcsSim->currVersionADSMap = (tempADSMapMajor << 8) + tempADSMapMinor;

        ErrorMsg = "Estimated TrainWeight Tons ";
        ErrorCtrl = (System::Object^)tbTrainWeightTonsLCS;
        lcsSim->currEstTrainWeightTons = Convert::ToUInt32(tbTrainWeightTonsLCS->Text);

        lcsSim->currLocoPenaltyBrkActive = (LocoPenaltyBrkActiveEnum)cbPenaltyBrkActive->SelectedIndex;

        lcsSim->currHandlingDone = (HandlingDoneEnum)cbHandlingDone->SelectedIndex;

        lcsSim->currLocoSysFault =
          (cbLocoSystemFaultsLCS0->Checked ? 1 << 0 : 0) + (cbLocoSystemFaultsLCS1->Checked ? 1 << 1 : 0) + (cbLocoSystemFaultsLCS2->Checked ? 1 << 2 : 0) + (cbLocoSystemFaultsLCS3->Checked ? 1 << 3 : 0) +
          (cbLocoSystemFaultsLCS4->Checked ? 1 << 4 : 0) + (cbLocoSystemFaultsLCS5->Checked ? 1 << 5 : 0) + (cbLocoSystemFaultsLCS6->Checked ? 1 << 6 : 0) + (cbLocoSystemFaultsLCS7->Checked ? 1 << 7 : 0) +
          (cbLocoSystemFaultsLCS8->Checked ? 1 << 8 : 0) + (cbLocoSystemFaultsLCS9->Checked ? 1 << 9 : 0) + (cbLocoSystemFaultsLCS10->Checked ? 1 << 10 : 0) + (cbLocoSystemFaultsLCS11->Checked ? 1 << 11 : 0) +
          (cbLocoSystemFaultsLCS12->Checked ? 1 << 12 : 0) + (cbLocoSystemFaultsLCS13->Checked ? 1 << 13 : 0) + (cbLocoSystemFaultsLCS14->Checked ? 1 << 14 : 0) + (cbLocoSystemFaultsLCS15->Checked ? 1 << 15 : 0) +
          (cbLocoSystemFaultsLCS16->Checked ? 1 << 16 : 0) + (cbLocoSystemFaultsLCS17->Checked ? 1 << 17 : 0) + (cbLocoSystemFaultsLCS18->Checked ? 1 << 18 : 0) + (cbLocoSystemFaultsLCS19->Checked ? 1 << 19 : 0) +
          (cbLocoSystemFaultsLCS20->Checked ? 1 << 20 : 0) + (cbLocoSystemFaultsLCS21->Checked ? 1 << 21 : 0) + (cbLocoSystemFaultsLCS22->Checked ? 1 << 22 : 0) + (cbLocoSystemFaultsLCS23->Checked ? 1 << 23 : 0) +
          (cbLocoSystemFaultsLCS24->Checked ? 1 << 24 : 0) + (cbLocoSystemFaultsLCS25->Checked ? 1 << 25 : 0) + (cbLocoSystemFaultsLCS26->Checked ? 1 << 26 : 0) + (cbLocoSystemFaultsLCS27->Checked ? 1 << 27 : 0) +
          (cbLocoSystemFaultsLCS28->Checked ? 1 << 28 : 0) + (cbLocoSystemFaultsLCS29->Checked ? 1 << 29 : 0) + (cbLocoSystemFaultsLCS30->Checked ? 1 << 30 : 0) + (cbLocoSystemFaultsLCS31->Checked ? 1 << 31 : 0);

        lcsSim->currADSBits =
          (cbADSBitFieldPathDefined->Checked ? 1 << 0 : 0) + (cbADSBitFieldMADefined->Checked ? 1 << 1 : 0) + (cbADSBitFieldATPWarningDefined->Checked ? 1 << 2 : 0) +
          (cbADSBitFieldTrainConfigDefined->Checked ? 1 << 3 : 0) + (cbADSBitFieldWeightDefined->Checked ? 1 << 4 : 0);

      }
      catch (...)
      {
        if (ErrorCtrl != nullptr)
        {
          ((System::Windows::Forms::TextBox^)ErrorCtrl)->Focus();
          ((System::Windows::Forms::TextBox^)ErrorCtrl)->SelectAll();
        }
        System::Windows::Forms::MessageBox::Show(ErrorMsg + "can not be converted to a valid integer.",
          "Error in params",
          (MessageBoxButtons)0,
          MessageBoxIcon::Error);
      }
    }


    /**********************************************************
    * Function:     Tick
    * Description:
    **********************************************************/
    void Tick(void)
    {
      // Enable com links/simulation flag as default
      lcsSim->comLinksEnabled = cbEnableComLinks->Checked;
      lcsSim->simulationEnabled = cbEnableSimulation->Checked;

      // Run simulation (Send/Receive to ATP and Simulate behavior of LCS)
      lcsSim->Tick();

      // Connection status
      UPDATE_IF_DIFFERENT(tsbATOGreen->Visible, lcsSim->atpConnected);
      UPDATE_IF_DIFFERENT(tsbATORed->Visible, !lcsSim->atpConnected);

      // Communication towards LocoSim/VSIM not used (for now).
#if 0

      if (lcsSim->useLocoSim)
      {
        UPDATE_IF_DIFFERENT(tsbLocoSimGreen->Visible, lcsSim->locoSimConnected);
        UPDATE_IF_DIFFERENT(tsbLocoSimRed->Visible, !lcsSim->locoSimConnected);
      }
      else
      {
        UPDATE_IF_DIFFERENT(tsbLocoSimGreen->Visible, false);
        UPDATE_IF_DIFFERENT(tsbLocoSimRed->Visible, false);
      }

      if (lcsSim->useVSIM)
      {
        switch (lcsSim->vsimConnected)
        {
        case VSIMCon_Running:
          UPDATE_IF_DIFFERENT(tsbVSIMGreen->Visible, true);
          UPDATE_IF_DIFFERENT(tsbVSIMYellow->Visible, false);
          UPDATE_IF_DIFFERENT(tsbVSIMRed->Visible, false);
          break;
        case VSIMCon_Connected:
          UPDATE_IF_DIFFERENT(tsbVSIMGreen->Visible, false);
          UPDATE_IF_DIFFERENT(tsbVSIMYellow->Visible, true);
          UPDATE_IF_DIFFERENT(tsbVSIMRed->Visible, false);
          break;
        case VSIMCon_NotConnected:
        case VSIMCon_Undef:
        default:
          UPDATE_IF_DIFFERENT(tsbVSIMGreen->Visible, false);
          UPDATE_IF_DIFFERENT(tsbVSIMYellow->Visible, false);
          UPDATE_IF_DIFFERENT(tsbVSIMRed->Visible, true);
          break;
        }
      }
      else
      {
        tsbVSIMGreen->Visible = false;
        tsbVSIMRed->Visible = false;
      }
#endif 

      // Disable fields if communication is not enabled
      UPDATE_IF_DIFFERENT(cbATOCabinSelectorStatusLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbATODrivingModeLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbTractionModeLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbBlueFlagStatusLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbBlueFlagRequestLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(tbADSEtaLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbADSEtaStatusLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLcsATOReadyLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbECPBSequenceStatusLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbReadyForPrecisionStopLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbBrakeSystemInUseLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbECPBOperatingModeLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbTrainIntegrityStatusECPBLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(tbPercentageOfOpBrakesECPBLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(tbLastCarBrakePressureLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(tbVersionADSMapMajorLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(tbVersionADSMapMinorLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(tbTrainWeightTonsLCS->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS0->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS1->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS2->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS3->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS4->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS5->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS6->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS7->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS8->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS9->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS10->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS11->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS12->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS13->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS14->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS15->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS16->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS17->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS18->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS19->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS20->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS21->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS22->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS23->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS24->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS25->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS26->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS27->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS28->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS29->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS30->Enabled, cbEnableComLinks->Checked);
      UPDATE_IF_DIFFERENT(cbLocoSystemFaultsLCS31->Enabled, cbEnableComLinks->Checked);

      // If communication not enabled, quit here !!
      if (!cbEnableComLinks->Checked)
      {
        return;
      }

      // Present pure AOS-status values received from AOS            
      switch (lcsSim->atpCom->recATOModeCabinSelector)
      {
      case ATOModeCabinSelectorManual:      UPDATE_IF_DIFFERENT(tbATOCabinSelector->Text, "Manual"); break;
      case ATOModeCabinSelectorSupervised:  UPDATE_IF_DIFFERENT(tbATOCabinSelector->Text, "Supervised"); break;
      case ATOModeCabinSelectorAutomatic:   UPDATE_IF_DIFFERENT(tbATOCabinSelector->Text, "Automatic"); break;
      case ATOModeCabinSelectorUndefined:   UPDATE_IF_DIFFERENT(tbATOCabinSelector->Text, "Undefined"); break;
      default:                              UPDATE_IF_DIFFERENT(tbATOCabinSelector->Text, "- Invalid Value -"); break;
      }

      switch (lcsSim->atpCom->recTrainIdling)
      {
      case TrainIdlingMaExists:       UPDATE_IF_DIFFERENT(tbTrainIdling->Text, "MA Exists"); break;
      case TrainIdlingTrainIsIdling:  UPDATE_IF_DIFFERENT(tbTrainIdling->Text, "Train Is Idling"); break;
      case TrainIdlingNOtAsserted:    UPDATE_IF_DIFFERENT(tbTrainIdling->Text, "Not Asserted"); break;
      default:                        UPDATE_IF_DIFFERENT(tbTrainIdling->Text, "- Invalid Value -"); break;
      }

      tbTrackIdFront->Text = lcsSim->atpCom->recTrackIdFrontOfTrain.ToString();
      tbPositionOnTrackFront->Text = lcsSim->atpCom->recPositionOnTrackFrontOfTrain.ToString();

      switch (lcsSim->atpCom->recTrainOrientationOnFrontTrack)
      {
      case TrainOrientationLeadLocoToLeg0:  UPDATE_IF_DIFFERENT(tbTrainOrientationFront->Text, "Loco->Leg 0"); break;
      case TrainOrientationLeadLocoToLeg1:  UPDATE_IF_DIFFERENT(tbTrainOrientationFront->Text, "Loco->Leg 1"); break;
      default:                              UPDATE_IF_DIFFERENT(tbTrainOrientationFront->Text, "- Invalid Value -"); break;
      }

      tbTrackIdRear->Text = lcsSim->atpCom->recTrackIdRearOfTrain.ToString();
      tbPositionOnTrackRear->Text = lcsSim->atpCom->recPositionOnTrackRearOfTrain.ToString();

      switch (lcsSim->atpCom->recTrainOrientationOnRearTrack)
      {
      case TrainOrientationLeadLocoToLeg0:  UPDATE_IF_DIFFERENT(tbTrainOrientationRear->Text, "Loco->Leg 0"); break;
      case TrainOrientationLeadLocoToLeg1:  UPDATE_IF_DIFFERENT(tbTrainOrientationRear->Text, "Loco->Leg 1"); break;
      default:                              UPDATE_IF_DIFFERENT(tbTrainOrientationRear->Text, "- Invalid Value -"); break;
      }

      switch (lcsSim->atpCom->recTravelDirection)
      {
      case TravelDirectionLocoLeading:    UPDATE_IF_DIFFERENT(tbTravelDirection->Text, "Loco Leading"); break;
      case TravelDirectionLocoTrailing:   UPDATE_IF_DIFFERENT(tbTravelDirection->Text, "Loco Trailing"); break;
      default:                            UPDATE_IF_DIFFERENT(tbTravelDirection->Text, "- Invalid Value -"); break;
      }

      tbAOSVehicleSpeed->Text = lcsSim->atpCom->recAOSVehicleSpeed.ToString();

      switch (lcsSim->atpCom->recBlueFlag)
      {
      case BlueFlagNotActive:   UPDATE_IF_DIFFERENT(tbBlueFlag->Text, "Not active"); break;
      case BlueFlagActive:      UPDATE_IF_DIFFERENT(tbBlueFlag->Text, "Active"); break;
      default:                  UPDATE_IF_DIFFERENT(tbBlueFlag->Text, "- Invalid Value -"); break;
      }


      switch (lcsSim->atpCom->recLimSupMode)
      {
      case NotActive:   UPDATE_IF_DIFFERENT(tbLimitedSupMode->Text, "Not active"); break;
      case Active:      UPDATE_IF_DIFFERENT(tbLimitedSupMode->Text, "Active"); break;
      default:          UPDATE_IF_DIFFERENT(tbLimitedSupMode->Text, "- Invalid Value -"); break;
      }

      //Present ATP Command from AOS
      if (lcsSim->atpCom->newATPComdRec)
      {
        switch (lcsSim->atpCom->recvECPBTrainCompReq)
        {
        case NotRequested:   UPDATE_IF_DIFFERENT(tbECPBTrainCompReq->Text, "Not Requested"); break;
        case Requested:      UPDATE_IF_DIFFERENT(tbECPBTrainCompReq->Text, "Requested"); break;
        case NotAsserted:    UPDATE_IF_DIFFERENT(tbECPBTrainCompReq->Text, "Not Asserted"); break;
        default:             UPDATE_IF_DIFFERENT(tbECPBTrainCompReq->Text, "- Invalid Value -"); break;
        }

        switch (lcsSim->atpCom->recvHoldingBrake)
        {
        case NotRequested:   UPDATE_IF_DIFFERENT(tbHoldingBrake->Text, "Not Requested"); break;
        case Requested:      UPDATE_IF_DIFFERENT(tbHoldingBrake->Text, "Requested"); break;
        case NotAsserted:    UPDATE_IF_DIFFERENT(tbHoldingBrake->Text, "Not Asserted"); break;
        default:             UPDATE_IF_DIFFERENT(tbHoldingBrake->Text, "- Invalid Value -"); break;
        }

        UPDATE_IF_DIFFERENT(tbTotalWeight->Text, lcsSim->atpCom->recvTrainWeight.ToString());

        lcsSim->atpCom->newATPComdRec = false;
      }

      //Present MA from AOS
      if (lcsSim->atpCom->newMARec)
      {
        UPDATE_IF_DIFFERENT(tbEndOfMATrkId->Text, lcsSim->atpCom->recvEndOfMATrkId.ToString());
        UPDATE_IF_DIFFERENT(tbEndOfMAPos->Text, lcsSim->atpCom->recvEndOfMAPos.ToString());
        switch (lcsSim->atpCom->recvMADirection)
        {
        case LocoLeading: UPDATE_IF_DIFFERENT(tbDirection->Text, "Loco Leading"); break;
        case LocoTrailing: UPDATE_IF_DIFFERENT(tbDirection->Text, "Loco Trailing"); break;
        default: UPDATE_IF_DIFFERENT(tbDirection->Text, "- Invalid Value -");  break;
        }
        UPDATE_IF_DIFFERENT(tbMAMargin->Text, lcsSim->atpCom->recvMAMargin.ToString());

        lcsSim->atpCom->newMARec = false;
      }

      //Present Warning Curve from AOS
      if (lcsSim->atpCom->newWarningCurveRec)
      {
        UPDATE_IF_DIFFERENT(tbNumCurvePoints->Text, lcsSim->atpCom->recvNumCurvePoints.ToString());
        dgSpeedCurvePoints->Rows->Clear();
        dgSpeedCurvePoints->Rows->Add(lcsSim->atpCom->recvNumCurvePoints);
        for (int i = 0; i < lcsSim->atpCom->recvNumCurvePoints; i++)
        {
          dgSpeedCurvePoints->Rows[i]->Cells[0]->Value = i + 1;
          dgSpeedCurvePoints->Rows[i]->Cells[1]->Value = lcsSim->atpCom->recvTrackIDWarningCurve[i];
          dgSpeedCurvePoints->Rows[i]->Cells[2]->Value = lcsSim->atpCom->recvPosInTrackWarningCurve[i];
          dgSpeedCurvePoints->Rows[i]->Cells[3]->Value = lcsSim->atpCom->recvSpeedWarningCurve[i];

        }

        lcsSim->atpCom->newWarningCurveRec = false;
      }

      // Present Train Composition from AOS
      if (lcsSim->atpCom->newAOSTrainCompRec)
      {
        lcsSim->atpCom->newAOSTrainCompRec = false;
        UPDATE_IF_DIFFERENT(tbRollingStockVeh->Text, lcsSim->atpCom->recNumberOfVehicles.ToString());

        lvTrainCompAOS->Items->Clear();
        ListViewItem^ itemPtr;
        for (int i = 0; i < lcsSim->atpCom->recNumberOfVehicles; i++)
        {
          itemPtr = lvTrainCompAOS->Items->Add((i + 1).ToString());
          itemPtr->SubItems->Add(lcsSim->atpCom->recRoadNumber[i].ToString());
          switch (lcsSim->atpCom->recVehicleType[i])
          {
          case VehicleTypeUnknown:     itemPtr->SubItems->Add("Unknown"); break;
          case VehicleTypeLocomotive:  itemPtr->SubItems->Add("Locomotive"); break;
          case VehicleTypeCar:         itemPtr->SubItems->Add("Car"); break;
          default:                  itemPtr->SubItems->Add("- Invalid Value -"); break;
          }
        }
      }

      //Present Path details
      if (lcsSim->atpCom->newPathRec)
      {
        lcsSim->atpCom->newPathRec = false;

        UPDATE_IF_DIFFERENT(tbSpeedBeginPath->Text, lcsSim->atpCom->recvSpeedBeginPath.ToString());
        UPDATE_IF_DIFFERENT(tbNextTrackId->Text, lcsSim->atpCom->recvNextTragetTrackId.ToString());
        UPDATE_IF_DIFFERENT(tbNextTrackPos->Text, lcsSim->atpCom->recvNextTargetPos.ToString());
        UPDATE_IF_DIFFERENT(tbReqTOA->Text, lcsSim->atpCom->recvReqTOANextTarget.ToString());

        UPDATE_IF_DIFFERENT(tbNumTracksPath->Text, lcsSim->atpCom->recvNumTracks.ToString());
        UPDATE_IF_DIFFERENT(tbNumSpeedChanges->Text, lcsSim->atpCom->recvNumSpeedChanges.ToString());

        UPDATE_IF_DIFFERENT(tbTCCVerADSMap->Text, lcsSim->atpCom->recvTCCVerADSMap.ToString());

        lvPathTracks->Items->Clear();
        lvPathSpeedChanges->Items->Clear();

        ListViewItem^ itemPtr;
        for (int i = 0; i < lcsSim->atpCom->recvNumTracks; i++)
        {
          itemPtr = lvPathTracks->Items->Add((i + 1).ToString());
          itemPtr->SubItems->Add(lcsSim->atpCom->recvTrackIds[i].ToString());
        }
        for (int i = 0; i < lcsSim->atpCom->recvNumSpeedChanges; i++)
        {
          itemPtr = lvPathSpeedChanges->Items->Add((i + 1).ToString());
          itemPtr->SubItems->Add(lcsSim->atpCom->recvTrackIDSpeedChange[i].ToString());
          itemPtr->SubItems->Add(lcsSim->atpCom->recvPosInTrackSpeedChange[i].ToString());
          itemPtr->SubItems->Add(lcsSim->atpCom->recvNewSpeedSpeedChange[i].ToString());
        }
      }

      // Present Rcl Information from AOS
      if (lcsSim->atpCom->newRclInfoRec)
      {

        lcsSim->atpCom->newRclInfoRec = false;
        switch (lcsSim->atpCom->recvAosOperationalMode)
        {
        case AosOperationalModeNormal: 
          UPDATE_IF_DIFFERENT(tbAosOperationalMode->Text, "Normal"); 
          break;
        case AosOperationalModeLocation: 
          UPDATE_IF_DIFFERENT(tbAosOperationalMode->Text, "Location");
          break;
        case AosOperationalModeOther: 
          UPDATE_IF_DIFFERENT(tbAosOperationalMode->Text, "Other"); 
          break;
        default: 
          UPDATE_IF_DIFFERENT(tbAosOperationalMode->Text, "Undefined");  
          break;
        }

        switch (lcsSim->atpCom->recvAosInterventionApplied)
        {
        case AosInterventionNotApplied: UPDATE_IF_DIFFERENT(tbAosInterventionApplied->Text, "Not Applied"); break;
        case AosInterventionApplied: UPDATE_IF_DIFFERENT(tbAosInterventionApplied->Text, "Applied"); break;
        default: UPDATE_IF_DIFFERENT(tbAosInterventionApplied->Text, "Invalid");  break;
        }

        switch (lcsSim->atpCom->recvAllowedTrainMovement)
        {
        case AllowedTrainMovementNone: UPDATE_IF_DIFFERENT(tbAllowedTrainMovement->Text, "None"); break;
        case AllowedTrainMovementBoth: UPDATE_IF_DIFFERENT(tbAllowedTrainMovement->Text, "Both"); break;
        case AllowedTrainMovementReverse: UPDATE_IF_DIFFERENT(tbAllowedTrainMovement->Text, "Reverse"); break;
        case AllowedTrainMovementForward: UPDATE_IF_DIFFERENT(tbAllowedTrainMovement->Text, "Forward"); break;
        default: UPDATE_IF_DIFFERENT(tbAllowedTrainMovement->Text, "Undefined");  break;
        }

        UPDATE_IF_DIFFERENT(tbFwdDtg->Text, lcsSim->atpCom->recvDtgForward.ToString());
        UPDATE_IF_DIFFERENT(tbRevcDtg->Text, lcsSim->atpCom->recvDtgReverse.ToString());

        switch (lcsSim->atpCom->recvRclTrainOrientation)
        {
        case RclTrainOrientationTrainForwardLocomotiveForward: UPDATE_IF_DIFFERENT(tbTrainOrientation->Text, "Train Forward is lead locomotive Forward"); break;
        case RclTrainOrientationTrainForwardLocomotiveReverse: UPDATE_IF_DIFFERENT(tbTrainOrientation->Text, "Train Forward is lead locomotive Reverse"); break;
        default: UPDATE_IF_DIFFERENT(tbTrainOrientation->Text, "Undefined");  break;
        }

        UPDATE_IF_DIFFERENT(tbCurCeilingSpeed->Text, lcsSim->atpCom->recvCurrentCeilingSpeed.ToString());

      }


      // If simulation is enabled -> Disable fields that will be simulated to prevent changes from GUI          
      if (cbEnableSimulation->Checked)
      {
        UPDATE_IF_DIFFERENT(cbATODrivingModeLCS->SelectedIndex, (ATODrivingModeEnum::ATODrivingModeNotAsserted != lcsSim->currATODrivingMode) ? lcsSim->currATODrivingMode : 6);
        UPDATE_IF_DIFFERENT(cbATODrivingModeLCS->Enabled, false);
        UPDATE_IF_DIFFERENT(cbTractionModeLCS->SelectedIndex, lcsSim->currFreeRollingStatus);
        UPDATE_IF_DIFFERENT(cbTractionModeLCS->Enabled, false);
        UPDATE_IF_DIFFERENT(cbBlueFlagStatusLCS->SelectedIndex, lcsSim->currBlueFlagStatus);
        UPDATE_IF_DIFFERENT(cbBlueFlagStatusLCS->Enabled, false);
        UPDATE_IF_DIFFERENT(cbLcsATOReadyLCS->SelectedIndex, (LCSATOReadyEnum::LCSATONotAsserted != lcsSim->currLCSATOReady) ? lcsSim->currLCSATOReady : 2);
        UPDATE_IF_DIFFERENT(cbLcsATOReadyLCS->Enabled, false);
        UPDATE_IF_DIFFERENT(cbECPBSequenceStatusLCS->SelectedIndex, lcsSim->currECPBSequenceStatus);
        UPDATE_IF_DIFFERENT(cbECPBSequenceStatusLCS->Enabled, false);
        UPDATE_IF_DIFFERENT(cbBrakeSystemInUseLCS->SelectedIndex, lcsSim->currBrakeSystemInUse);
        UPDATE_IF_DIFFERENT(cbBrakeSystemInUseLCS->Enabled, false);
        UPDATE_IF_DIFFERENT(cbECPBOperatingModeLCS->SelectedIndex, (ECPBOperatingModeEnum::ECPBOperatingModeNotAvailable != lcsSim->currECPBOperatingMode) ? lcsSim->currECPBOperatingMode : 4);
        UPDATE_IF_DIFFERENT(cbECPBOperatingModeLCS->Enabled, false);
        UPDATE_IF_DIFFERENT(tbTrainWeightTonsLCS->Text, lcsSim->currEstTrainWeightTons.ToString());
        UPDATE_IF_DIFFERENT(tbTrainWeightTonsLCS->Enabled, false);
      }
      else
      {
        UPDATE_IF_DIFFERENT(cbATODrivingModeLCS->Enabled, true);
        UPDATE_IF_DIFFERENT(cbTractionModeLCS->Enabled, true);
        UPDATE_IF_DIFFERENT(cbBlueFlagStatusLCS->Enabled, true);
        UPDATE_IF_DIFFERENT(cbLcsATOReadyLCS->Enabled, true);
        UPDATE_IF_DIFFERENT(cbECPBSequenceStatusLCS->Enabled, true);
        UPDATE_IF_DIFFERENT(cbBrakeSystemInUseLCS->Enabled, true);
        UPDATE_IF_DIFFERENT(cbECPBOperatingModeLCS->Enabled, true);
        UPDATE_IF_DIFFERENT(tbTrainWeightTonsLCS->Enabled, true);
      }

      // Update LocoSim lists
      for (int i = 0; i < lcsSim->atpCom->guiFromATPCnt; i++)
      {
        UPDATE_IF_DIFFERENT(lvDataFromATO->Items[i]->SubItems[1]->Text, lcsSim->atpCom->guiFromATP[i]);
      }
      for (int i = 0; i < lcsSim->atpCom->guiToATPCnt; i++)
      {
        UPDATE_IF_DIFFERENT(lvDataToATO->Items[i]->SubItems[1]->Text, lcsSim->atpCom->guiToATP[i]);
      }

      if (lcsSim->useLocoSim)
      {
        for (int i = 0; i < lcsSim->lsCom->guiFromLSCnt; i++)
        {
          UPDATE_IF_DIFFERENT(lvDataFromLocoSim->Items[i]->SubItems[1]->Text, lcsSim->lsCom->guiFromLS[i]);
        }
        for (int i = 0; i < lcsSim->lsCom->guiToLSCnt; i++)
        {
          UPDATE_IF_DIFFERENT(lvDataToLocoSim->Items[i]->SubItems[1]->Text, lcsSim->lsCom->guiToLS[i]);
        }
      }
      else if (lcsSim->useVSIM)
      {
        for (int i = 0; i < lcsSim->vsimCom->guiFromVSIMCnt; i++)
        {
          UPDATE_IF_DIFFERENT(lvDataFromLocoSim->Items[i]->SubItems[1]->Text, lcsSim->vsimCom->guiFromVSIM[i]);
        }
        for (int i = 0; i < lcsSim->vsimCom->guiToVSIMCnt; i++)
        {
          UPDATE_IF_DIFFERENT(lvDataToLocoSim->Items[i]->SubItems[1]->Text, lcsSim->vsimCom->guiToVSIM[i]);
        }
      }

      // Debug strings
      if (lcsSim->useVSIM &&
        showDebugTab)
      {
        // Add debug strings, if any
        if (lcsSim->vsimCom->dbgStrCnt > 0)
        {
          for (int i = 0; i < lcsSim->vsimCom->dbgStrCnt; i++)
          {
            lvDebug->Items->Insert(lvDebug->Items->Count, lcsSim->vsimCom->dbgStr[i]->Replace("\r\n", ""));
          }

          // Limit no of displayed lines
          int LastIndex = lvDebug->Items->Count - 1;
          while (LastIndex >= MAX_LINES_DISPLAYED)
          {
            lvDebug->Items->RemoveAt(0);
            LastIndex = lvDebug->Items->Count - 1;
          }

          // Select last line as visible
          if (LastIndex >= 0)
          {
            lvDebug->EnsureVisible(LastIndex);
          }
        }
      }
    }

    /**********************************************************
    * Function:     LoadSetup
    * Description:
    **********************************************************/
  public: void LoadSetup(String^ setup) {
    String^ tmpRootKey = regRootKey;
    if (!System::String::IsNullOrEmpty(setup))
    {
      tmpRootKey = regRootKey + "\\" + setup + "\\LCSSim";
    }
    // Check if key exists
    String^ tmp = Convert::ToString(Registry::GetValue(tmpRootKey, "", "empty"));

    // Key exists, i.e. existing setup
    if (!System::String::IsNullOrEmpty(tmp))
    {
      // Get window position from registry
      int    left = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Left", "32767"));
      int    top = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Top", "32767"));
      bool   visible = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Visible", "1")) == 1 ? true : false;
      // If registry not available, first run for example, use default position from windows
      if (left != 32767)     formLeft = left;
      if (top != 32767)      formTop = top;
      SetVisibility(visible);
    }
  }
          /**********************************************************
          * Function:     SaveSetup
          * Description:
          **********************************************************/
  public: void SaveSetup(String^ setup) {
    String^ tmpRootKey = regRootKey;
    if (!System::String::IsNullOrEmpty(setup))
    {
      // Create key
      tmpRootKey = regRootKey + "\\" + setup + "\\LCSSim";

      // Store windows position in registry
      Registry::SetValue(tmpRootKey, "Left", String::Format("{0:0}", this->Left));
      Registry::SetValue(tmpRootKey, "Top", String::Format("{0:0}", this->Top));
      Registry::SetValue(tmpRootKey, "Visible", this->Visible ? "1" : "0");
    }
  }

          /**********************************************************
          * Function:     SetSize
          * Description:
          **********************************************************/
  public: void SetSize(int left, int top, int width, int height) {
    this->Left = left;
    this->Top = top;
    //this->Width = width;
    //this->Height = height;
    formLeft = left;
    formTop = top;
    //formWidth   = width;
    //formHeight  = height;
  }
  protected:
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    ~LCSSimForm()
    {
      if (components)
      {
        delete components;
      }
    }
  protected:


  private:
    /// <summary>
    /// Required designer variable.
    /// </summary>
    System::ComponentModel::Container ^components;

#pragma region Windows Form Designer generated code
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    void InitializeComponent(void)
    {
      System::ComponentModel::ComponentResourceManager^  resources = (gcnew System::ComponentModel::ComponentResourceManager(LCSSimForm::typeid));
      System::Windows::Forms::ListViewItem^  listViewItem1 = (gcnew System::Windows::Forms::ListViewItem(L""));
      System::Windows::Forms::ListViewItem^  listViewItem2 = (gcnew System::Windows::Forms::ListViewItem(L""));
      System::Windows::Forms::ListViewItem^  listViewItem3 = (gcnew System::Windows::Forms::ListViewItem(L""));
      System::Windows::Forms::ListViewItem^  listViewItem4 = (gcnew System::Windows::Forms::ListViewItem(L""));
      System::Windows::Forms::ListViewItem^  listViewItem5 = (gcnew System::Windows::Forms::ListViewItem(L""));
      System::Windows::Forms::ListViewItem^  listViewItem6 = (gcnew System::Windows::Forms::ListViewItem(L""));
      System::Windows::Forms::ListViewItem^  listViewItem7 = (gcnew System::Windows::Forms::ListViewItem(L""));
      this->columnHeader10 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader11 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader13 = (gcnew System::Windows::Forms::ColumnHeader());
      this->toolStrip1 = (gcnew System::Windows::Forms::ToolStrip());
      this->tsbATORed = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbATOGreen = (gcnew System::Windows::Forms::ToolStripButton());
      this->toolStripSeparator2 = (gcnew System::Windows::Forms::ToolStripSeparator());
      this->tsbLocoSimGreen = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbLocoSimRed = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbVSIMGreen = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbVSIMYellow = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbVSIMRed = (gcnew System::Windows::Forms::ToolStripButton());
      this->toolStripSeparator6 = (gcnew System::Windows::Forms::ToolStripSeparator());
      this->tpDebug = (gcnew System::Windows::Forms::TabPage());
      this->lvDebug = (gcnew System::Windows::Forms::ListView());
      this->columnHeader12 = (gcnew System::Windows::Forms::ColumnHeader());
      this->tpDataToLocoSim = (gcnew System::Windows::Forms::TabPage());
      this->label14 = (gcnew System::Windows::Forms::Label());
      this->lvDataToLocoSim = (gcnew System::Windows::Forms::ListView());
      this->columnHeader5 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader6 = (gcnew System::Windows::Forms::ColumnHeader());
      this->tpDataFromLocoSim = (gcnew System::Windows::Forms::TabPage());
      this->label13 = (gcnew System::Windows::Forms::Label());
      this->lvDataFromLocoSim = (gcnew System::Windows::Forms::ListView());
      this->columnHeader3 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader4 = (gcnew System::Windows::Forms::ColumnHeader());
      this->tpDataToATO = (gcnew System::Windows::Forms::TabPage());
      this->label12 = (gcnew System::Windows::Forms::Label());
      this->lvDataToATO = (gcnew System::Windows::Forms::ListView());
      this->columnHeader1 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader2 = (gcnew System::Windows::Forms::ColumnHeader());
      this->tpDataFromATO = (gcnew System::Windows::Forms::TabPage());
      this->label11 = (gcnew System::Windows::Forms::Label());
      this->lvDataFromATO = (gcnew System::Windows::Forms::ListView());
      this->chItem = (gcnew System::Windows::Forms::ColumnHeader());
      this->chData = (gcnew System::Windows::Forms::ColumnHeader());
      this->tpLCSBrakes = (gcnew System::Windows::Forms::TabPage());
      this->bApplyBrakes = (gcnew System::Windows::Forms::Button());
      this->groupBox8 = (gcnew System::Windows::Forms::GroupBox());
      this->label41 = (gcnew System::Windows::Forms::Label());
      this->label40 = (gcnew System::Windows::Forms::Label());
      this->tbLastCarBrakePressureLCS = (gcnew System::Windows::Forms::TextBox());
      this->label33 = (gcnew System::Windows::Forms::Label());
      this->tbPercentageOfOpBrakesECPBLCS = (gcnew System::Windows::Forms::TextBox());
      this->label32 = (gcnew System::Windows::Forms::Label());
      this->groupBox7 = (gcnew System::Windows::Forms::GroupBox());
      this->cbPenaltyBrkActive = (gcnew System::Windows::Forms::ComboBox());
      this->label83 = (gcnew System::Windows::Forms::Label());
      this->cbTrainIntegrityStatusECPBLCS = (gcnew System::Windows::Forms::ComboBox());
      this->label31 = (gcnew System::Windows::Forms::Label());
      this->cbECPBOperatingModeLCS = (gcnew System::Windows::Forms::ComboBox());
      this->label30 = (gcnew System::Windows::Forms::Label());
      this->cbBrakeSystemInUseLCS = (gcnew System::Windows::Forms::ComboBox());
      this->label29 = (gcnew System::Windows::Forms::Label());
      this->cbECPBSequenceStatusLCS = (gcnew System::Windows::Forms::ComboBox());
      this->label27 = (gcnew System::Windows::Forms::Label());
      this->tpLCSATO = (gcnew System::Windows::Forms::TabPage());
      this->groupBox16 = (gcnew System::Windows::Forms::GroupBox());
      this->cbADSBitFieldATPWarningDefined = (gcnew System::Windows::Forms::CheckBox());
      this->cbADSBitFieldWeightDefined = (gcnew System::Windows::Forms::CheckBox());
      this->cbADSBitFieldMADefined = (gcnew System::Windows::Forms::CheckBox());
      this->cbADSBitFieldTrainConfigDefined = (gcnew System::Windows::Forms::CheckBox());
      this->cbADSBitFieldPathDefined = (gcnew System::Windows::Forms::CheckBox());
      this->bApplyAto = (gcnew System::Windows::Forms::Button());
      this->groupBox5 = (gcnew System::Windows::Forms::GroupBox());
      this->cbBlueFlagRequestLCS = (gcnew System::Windows::Forms::ComboBox());
      this->label24 = (gcnew System::Windows::Forms::Label());
      this->cbBlueFlagStatusLCS = (gcnew System::Windows::Forms::ComboBox());
      this->label23 = (gcnew System::Windows::Forms::Label());
      this->groupBox4 = (gcnew System::Windows::Forms::GroupBox());
      this->label39 = (gcnew System::Windows::Forms::Label());
      this->tbADSEtaLCS = (gcnew System::Windows::Forms::TextBox());
      this->label111 = (gcnew System::Windows::Forms::Label());
      this->cbADSEtaStatusLCS = (gcnew System::Windows::Forms::ComboBox());
      this->label25 = (gcnew System::Windows::Forms::Label());
      this->cbReadyForPrecisionStopLCS = (gcnew System::Windows::Forms::ComboBox());
      this->label28 = (gcnew System::Windows::Forms::Label());
      this->cbLcsATOReadyLCS = (gcnew System::Windows::Forms::ComboBox());
      this->label26 = (gcnew System::Windows::Forms::Label());
      this->label22 = (gcnew System::Windows::Forms::Label());
      this->cbTractionModeLCS = (gcnew System::Windows::Forms::ComboBox());
      this->label21 = (gcnew System::Windows::Forms::Label());
      this->cbATODrivingModeLCS = (gcnew System::Windows::Forms::ComboBox());
      this->label20 = (gcnew System::Windows::Forms::Label());
      this->cbATOCabinSelectorStatusLCS = (gcnew System::Windows::Forms::ComboBox());
      this->tpLCSFaults = (gcnew System::Windows::Forms::TabPage());
      this->bApplyFaults = (gcnew System::Windows::Forms::Button());
      this->groupBox6 = (gcnew System::Windows::Forms::GroupBox());
      this->tbVersionADSMapMinorLCS = (gcnew System::Windows::Forms::TextBox());
      this->label38 = (gcnew System::Windows::Forms::Label());
      this->label37 = (gcnew System::Windows::Forms::Label());
      this->tbVersionADSMapMajorLCS = (gcnew System::Windows::Forms::TextBox());
      this->label36 = (gcnew System::Windows::Forms::Label());
      this->tbTrainWeightTonsLCS = (gcnew System::Windows::Forms::TextBox());
      this->label34 = (gcnew System::Windows::Forms::Label());
      this->label35 = (gcnew System::Windows::Forms::Label());
      this->groupBox3 = (gcnew System::Windows::Forms::GroupBox());
      this->cbLocoSystemFaultsLCS31 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS30 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS29 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS28 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS27 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS26 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS25 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS24 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS23 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS22 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS21 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS20 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS19 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS18 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS17 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS16 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS15 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS14 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS13 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS12 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS11 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS10 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS9 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS8 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS7 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS6 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS5 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS4 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS3 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS2 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS1 = (gcnew System::Windows::Forms::CheckBox());
      this->cbLocoSystemFaultsLCS0 = (gcnew System::Windows::Forms::CheckBox());
      this->tpTrainCompECPB = (gcnew System::Windows::Forms::TabPage());
      this->buttonAppend = (gcnew System::Windows::Forms::Button());
      this->buttonImport = (gcnew System::Windows::Forms::Button());
      this->bApplyTrainComp = (gcnew System::Windows::Forms::Button());
      this->bSaveTrainComp = (gcnew System::Windows::Forms::Button());
      this->groupBox14 = (gcnew System::Windows::Forms::GroupBox());
      this->tbRollingStockVehECP = (gcnew System::Windows::Forms::TextBox());
      this->tbVehNotDetected = (gcnew System::Windows::Forms::TextBox());
      this->label57 = (gcnew System::Windows::Forms::Label());
      this->tbVehDetectedPosUnknown = (gcnew System::Windows::Forms::TextBox());
      this->label58 = (gcnew System::Windows::Forms::Label());
      this->label59 = (gcnew System::Windows::Forms::Label());
      this->dgTrainCompECPB = (gcnew System::Windows::Forms::DataGridView());
      this->No = (gcnew System::Windows::Forms::DataGridViewTextBoxColumn());
      this->Column1 = (gcnew System::Windows::Forms::DataGridViewTextBoxColumn());
      this->Column3 = (gcnew System::Windows::Forms::DataGridViewComboBoxColumn());
      this->tpTrainCompAOS = (gcnew System::Windows::Forms::TabPage());
      this->tbRollingStockVeh = (gcnew System::Windows::Forms::TextBox());
      this->label53 = (gcnew System::Windows::Forms::Label());
      this->lvTrainCompAOS = (gcnew System::Windows::Forms::ListView());
      this->columnHeader7 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader8 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader9 = (gcnew System::Windows::Forms::ColumnHeader());
      this->tpAOSStatus = (gcnew System::Windows::Forms::TabPage());
      this->cbEnableSimulation = (gcnew System::Windows::Forms::CheckBox());
      this->cbEnableComLinks = (gcnew System::Windows::Forms::CheckBox());
      this->groupBox2 = (gcnew System::Windows::Forms::GroupBox());
      this->label19 = (gcnew System::Windows::Forms::Label());
      this->tbTotalWeight = (gcnew System::Windows::Forms::TextBox());
      this->label15 = (gcnew System::Windows::Forms::Label());
      this->tbHoldingBrake = (gcnew System::Windows::Forms::TextBox());
      this->label10 = (gcnew System::Windows::Forms::Label());
      this->tbECPBTrainCompReq = (gcnew System::Windows::Forms::TextBox());
      this->label7 = (gcnew System::Windows::Forms::Label());
      this->groupBox1 = (gcnew System::Windows::Forms::GroupBox());
      this->label18 = (gcnew System::Windows::Forms::Label());
      this->label17 = (gcnew System::Windows::Forms::Label());
      this->label6 = (gcnew System::Windows::Forms::Label());
      this->tbTrainOrientationFront = (gcnew System::Windows::Forms::TextBox());
      this->label5 = (gcnew System::Windows::Forms::Label());
      this->tbTrainOrientationRear = (gcnew System::Windows::Forms::TextBox());
      this->label4 = (gcnew System::Windows::Forms::Label());
      this->tbPositionOnTrackFront = (gcnew System::Windows::Forms::TextBox());
      this->tbPositionOnTrackRear = (gcnew System::Windows::Forms::TextBox());
      this->label3 = (gcnew System::Windows::Forms::Label());
      this->tbTrackIdFront = (gcnew System::Windows::Forms::TextBox());
      this->tbTrackIdRear = (gcnew System::Windows::Forms::TextBox());
      this->label2 = (gcnew System::Windows::Forms::Label());
      this->groupBoxExample = (gcnew System::Windows::Forms::GroupBox());
      this->label62 = (gcnew System::Windows::Forms::Label());
      this->tbLimitedSupMode = (gcnew System::Windows::Forms::TextBox());
      this->label61 = (gcnew System::Windows::Forms::Label());
      this->tbAOSVehicleSpeed = (gcnew System::Windows::Forms::TextBox());
      this->tbBlueFlag = (gcnew System::Windows::Forms::TextBox());
      this->label9 = (gcnew System::Windows::Forms::Label());
      this->tbTravelDirection = (gcnew System::Windows::Forms::TextBox());
      this->label8 = (gcnew System::Windows::Forms::Label());
      this->tbTrainIdling = (gcnew System::Windows::Forms::TextBox());
      this->label1 = (gcnew System::Windows::Forms::Label());
      this->tbATOCabinSelector = (gcnew System::Windows::Forms::TextBox());
      this->labelExample = (gcnew System::Windows::Forms::Label());
      this->tcMainControl = (gcnew System::Windows::Forms::TabControl());
      this->tpMovementAuthority = (gcnew System::Windows::Forms::TabPage());
      this->groupBox9 = (gcnew System::Windows::Forms::GroupBox());
      this->label84 = (gcnew System::Windows::Forms::Label());
      this->label60 = (gcnew System::Windows::Forms::Label());
      this->tbMAMargin = (gcnew System::Windows::Forms::TextBox());
      this->label42 = (gcnew System::Windows::Forms::Label());
      this->tbEndOfMAPos = (gcnew System::Windows::Forms::TextBox());
      this->label44 = (gcnew System::Windows::Forms::Label());
      this->tbDirection = (gcnew System::Windows::Forms::TextBox());
      this->label45 = (gcnew System::Windows::Forms::Label());
      this->tbEndOfMATrkId = (gcnew System::Windows::Forms::TextBox());
      this->label46 = (gcnew System::Windows::Forms::Label());
      this->tpWarningCurve = (gcnew System::Windows::Forms::TabPage());
      this->dgSpeedCurvePoints = (gcnew System::Windows::Forms::DataGridView());
      this->dgvTbcNumber = (gcnew System::Windows::Forms::DataGridViewTextBoxColumn());
      this->dgvTbcTrackId = (gcnew System::Windows::Forms::DataGridViewTextBoxColumn());
      this->dgvTbcPos = (gcnew System::Windows::Forms::DataGridViewTextBoxColumn());
      this->dgvTbcSpeed = (gcnew System::Windows::Forms::DataGridViewTextBoxColumn());
      this->tbNumCurvePoints = (gcnew System::Windows::Forms::TextBox());
      this->label43 = (gcnew System::Windows::Forms::Label());
      this->tpPath = (gcnew System::Windows::Forms::TabPage());
      this->tbTCCVerADSMap = (gcnew System::Windows::Forms::TextBox());
      this->label16 = (gcnew System::Windows::Forms::Label());
      this->label56 = (gcnew System::Windows::Forms::Label());
      this->groupBox11 = (gcnew System::Windows::Forms::GroupBox());
      this->tbNumSpeedChanges = (gcnew System::Windows::Forms::TextBox());
      this->label55 = (gcnew System::Windows::Forms::Label());
      this->groupBox12 = (gcnew System::Windows::Forms::GroupBox());
      this->lvPathSpeedChanges = (gcnew System::Windows::Forms::ListView());
      this->columnHeader19 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader20 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader16 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader17 = (gcnew System::Windows::Forms::ColumnHeader());
      this->groupBox10 = (gcnew System::Windows::Forms::GroupBox());
      this->lvPathTracks = (gcnew System::Windows::Forms::ListView());
      this->columnHeader14 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader15 = (gcnew System::Windows::Forms::ColumnHeader());
      this->tbNumTracksPath = (gcnew System::Windows::Forms::TextBox());
      this->label54 = (gcnew System::Windows::Forms::Label());
      this->label52 = (gcnew System::Windows::Forms::Label());
      this->label51 = (gcnew System::Windows::Forms::Label());
      this->tbReqTOA = (gcnew System::Windows::Forms::TextBox());
      this->tbNextTrackPos = (gcnew System::Windows::Forms::TextBox());
      this->label50 = (gcnew System::Windows::Forms::Label());
      this->label49 = (gcnew System::Windows::Forms::Label());
      this->label48 = (gcnew System::Windows::Forms::Label());
      this->tbNextTrackId = (gcnew System::Windows::Forms::TextBox());
      this->tbSpeedBeginPath = (gcnew System::Windows::Forms::TextBox());
      this->label47 = (gcnew System::Windows::Forms::Label());
      this->RclInfo = (gcnew System::Windows::Forms::TabPage());
      this->groupBox21 = (gcnew System::Windows::Forms::GroupBox());
      this->bApplyHandingDone = (gcnew System::Windows::Forms::Button());
      this->cbHandlingDone = (gcnew System::Windows::Forms::ComboBox());
      this->label90 = (gcnew System::Windows::Forms::Label());
      this->groupBox20 = (gcnew System::Windows::Forms::GroupBox());
      this->label82 = (gcnew System::Windows::Forms::Label());
      this->tbCurCeilingSpeed = (gcnew System::Windows::Forms::TextBox());
      this->label81 = (gcnew System::Windows::Forms::Label());
      this->tbTrainOrientation = (gcnew System::Windows::Forms::TextBox());
      this->label80 = (gcnew System::Windows::Forms::Label());
      this->tbRevcDtg = (gcnew System::Windows::Forms::TextBox());
      this->label79 = (gcnew System::Windows::Forms::Label());
      this->label71 = (gcnew System::Windows::Forms::Label());
      this->label72 = (gcnew System::Windows::Forms::Label());
      this->tbFwdDtg = (gcnew System::Windows::Forms::TextBox());
      this->label73 = (gcnew System::Windows::Forms::Label());
      this->tbAllowedTrainMovement = (gcnew System::Windows::Forms::TextBox());
      this->label74 = (gcnew System::Windows::Forms::Label());
      this->tbAosInterventionApplied = (gcnew System::Windows::Forms::TextBox());
      this->label75 = (gcnew System::Windows::Forms::Label());
      this->tbAosOperationalMode = (gcnew System::Windows::Forms::TextBox());
      this->label78 = (gcnew System::Windows::Forms::Label());
      this->groupBox17 = (gcnew System::Windows::Forms::GroupBox());
      this->textBox5 = (gcnew System::Windows::Forms::TextBox());
      this->label86 = (gcnew System::Windows::Forms::Label());
      this->textBox8 = (gcnew System::Windows::Forms::TextBox());
      this->label87 = (gcnew System::Windows::Forms::Label());
      this->groupBox18 = (gcnew System::Windows::Forms::GroupBox());
      this->groupBox19 = (gcnew System::Windows::Forms::GroupBox());
      this->textBox9 = (gcnew System::Windows::Forms::TextBox());
      this->label76 = (gcnew System::Windows::Forms::Label());
      this->textBox10 = (gcnew System::Windows::Forms::TextBox());
      this->label77 = (gcnew System::Windows::Forms::Label());
      this->button1 = (gcnew System::Windows::Forms::Button());
      this->groupBox13 = (gcnew System::Windows::Forms::GroupBox());
      this->label63 = (gcnew System::Windows::Forms::Label());
      this->label64 = (gcnew System::Windows::Forms::Label());
      this->textBox6 = (gcnew System::Windows::Forms::TextBox());
      this->label65 = (gcnew System::Windows::Forms::Label());
      this->textBox7 = (gcnew System::Windows::Forms::TextBox());
      this->label66 = (gcnew System::Windows::Forms::Label());
      this->groupBox15 = (gcnew System::Windows::Forms::GroupBox());
      this->comboBox1 = (gcnew System::Windows::Forms::ComboBox());
      this->label67 = (gcnew System::Windows::Forms::Label());
      this->comboBox2 = (gcnew System::Windows::Forms::ComboBox());
      this->label68 = (gcnew System::Windows::Forms::Label());
      this->comboBox3 = (gcnew System::Windows::Forms::ComboBox());
      this->label69 = (gcnew System::Windows::Forms::Label());
      this->comboBox4 = (gcnew System::Windows::Forms::ComboBox());
      this->label70 = (gcnew System::Windows::Forms::Label());
      this->toolStrip1->SuspendLayout();
      this->tpDebug->SuspendLayout();
      this->tpDataToLocoSim->SuspendLayout();
      this->tpDataFromLocoSim->SuspendLayout();
      this->tpDataToATO->SuspendLayout();
      this->tpDataFromATO->SuspendLayout();
      this->tpLCSBrakes->SuspendLayout();
      this->groupBox8->SuspendLayout();
      this->groupBox7->SuspendLayout();
      this->tpLCSATO->SuspendLayout();
      this->groupBox16->SuspendLayout();
      this->groupBox5->SuspendLayout();
      this->groupBox4->SuspendLayout();
      this->tpLCSFaults->SuspendLayout();
      this->groupBox6->SuspendLayout();
      this->groupBox3->SuspendLayout();
      this->tpTrainCompECPB->SuspendLayout();
      this->groupBox14->SuspendLayout();
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->dgTrainCompECPB))->BeginInit();
      this->tpTrainCompAOS->SuspendLayout();
      this->tpAOSStatus->SuspendLayout();
      this->groupBox2->SuspendLayout();
      this->groupBox1->SuspendLayout();
      this->groupBoxExample->SuspendLayout();
      this->tcMainControl->SuspendLayout();
      this->tpMovementAuthority->SuspendLayout();
      this->groupBox9->SuspendLayout();
      this->tpWarningCurve->SuspendLayout();
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->dgSpeedCurvePoints))->BeginInit();
      this->tpPath->SuspendLayout();
      this->groupBox11->SuspendLayout();
      this->groupBox12->SuspendLayout();
      this->groupBox10->SuspendLayout();
      this->RclInfo->SuspendLayout();
      this->groupBox21->SuspendLayout();
      this->groupBox20->SuspendLayout();
      this->groupBox13->SuspendLayout();
      this->groupBox15->SuspendLayout();
      this->SuspendLayout();
      // 
      // columnHeader10
      // 
      this->columnHeader10->Text = L"No.";
      this->columnHeader10->Width = 32;
      // 
      // columnHeader11
      // 
      this->columnHeader11->Text = L"RoadNumber";
      this->columnHeader11->Width = 160;
      // 
      // toolStrip1
      // 
      this->toolStrip1->Dock = System::Windows::Forms::DockStyle::Bottom;
      this->toolStrip1->GripStyle = System::Windows::Forms::ToolStripGripStyle::Hidden;
      this->toolStrip1->Items->AddRange(gcnew cli::array< System::Windows::Forms::ToolStripItem^  >(9) {
        this->tsbATORed, this->tsbATOGreen,
          this->toolStripSeparator2, this->tsbLocoSimGreen, this->tsbLocoSimRed, this->tsbVSIMGreen, this->tsbVSIMYellow, this->tsbVSIMRed,
          this->toolStripSeparator6
      });
      this->toolStrip1->Location = System::Drawing::Point(0, 505);
      this->toolStrip1->Name = L"toolStrip1";
      this->toolStrip1->RenderMode = System::Windows::Forms::ToolStripRenderMode::System;
      this->toolStrip1->Size = System::Drawing::Size(446, 25);
      this->toolStrip1->TabIndex = 4;
      this->toolStrip1->Text = L"toolStrip1";
      this->toolStrip1->ItemClicked += gcnew System::Windows::Forms::ToolStripItemClickedEventHandler(this, &LCSSimForm::toolStrip1_ItemClicked);
      // 
      // tsbATORed
      // 
      this->tsbATORed->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbATORed->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbATORed.Image")));
      this->tsbATORed->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbATORed->Name = L"tsbATORed";
      this->tsbATORed->Size = System::Drawing::Size(49, 22);
      this->tsbATORed->Text = L"ATP";
      // 
      // tsbATOGreen
      // 
      this->tsbATOGreen->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbATOGreen->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbATOGreen.Image")));
      this->tsbATOGreen->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbATOGreen->Name = L"tsbATOGreen";
      this->tsbATOGreen->Size = System::Drawing::Size(49, 22);
      this->tsbATOGreen->Text = L"ATP";
      this->tsbATOGreen->Visible = false;
      // 
      // toolStripSeparator2
      // 
      this->toolStripSeparator2->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->toolStripSeparator2->Name = L"toolStripSeparator2";
      this->toolStripSeparator2->Size = System::Drawing::Size(6, 25);
      // 
      // tsbLocoSimGreen
      // 
      this->tsbLocoSimGreen->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbLocoSimGreen->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbLocoSimGreen.Image")));
      this->tsbLocoSimGreen->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbLocoSimGreen->Name = L"tsbLocoSimGreen";
      this->tsbLocoSimGreen->Size = System::Drawing::Size(73, 22);
      this->tsbLocoSimGreen->Text = L"LocoSim";
      this->tsbLocoSimGreen->Visible = false;
      // 
      // tsbLocoSimRed
      // 
      this->tsbLocoSimRed->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbLocoSimRed->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbLocoSimRed.Image")));
      this->tsbLocoSimRed->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbLocoSimRed->Name = L"tsbLocoSimRed";
      this->tsbLocoSimRed->Size = System::Drawing::Size(73, 22);
      this->tsbLocoSimRed->Text = L"LocoSim";
      this->tsbLocoSimRed->Visible = false;
      // 
      // tsbVSIMGreen
      // 
      this->tsbVSIMGreen->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbVSIMGreen->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbVSIMGreen.Image")));
      this->tsbVSIMGreen->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbVSIMGreen->Name = L"tsbVSIMGreen";
      this->tsbVSIMGreen->Size = System::Drawing::Size(54, 22);
      this->tsbVSIMGreen->Text = L"VSIM";
      this->tsbVSIMGreen->Visible = false;
      // 
      // tsbVSIMYellow
      // 
      this->tsbVSIMYellow->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbVSIMYellow->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbVSIMYellow.Image")));
      this->tsbVSIMYellow->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbVSIMYellow->Name = L"tsbVSIMYellow";
      this->tsbVSIMYellow->Size = System::Drawing::Size(54, 22);
      this->tsbVSIMYellow->Text = L"VSIM";
      this->tsbVSIMYellow->Visible = false;
      // 
      // tsbVSIMRed
      // 
      this->tsbVSIMRed->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbVSIMRed->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbVSIMRed.Image")));
      this->tsbVSIMRed->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbVSIMRed->Name = L"tsbVSIMRed";
      this->tsbVSIMRed->Size = System::Drawing::Size(54, 22);
      this->tsbVSIMRed->Text = L"VSIM";
      this->tsbVSIMRed->Visible = false;
      // 
      // toolStripSeparator6
      // 
      this->toolStripSeparator6->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->toolStripSeparator6->Name = L"toolStripSeparator6";
      this->toolStripSeparator6->Size = System::Drawing::Size(6, 25);
      // 
      // tpDebug
      // 
      this->tpDebug->Controls->Add(this->lvDebug);
      this->tpDebug->Location = System::Drawing::Point(4, 22);
      this->tpDebug->Name = L"tpDebug";
      this->tpDebug->Padding = System::Windows::Forms::Padding(3);
      this->tpDebug->Size = System::Drawing::Size(425, 464);
      this->tpDebug->TabIndex = 5;
      this->tpDebug->Text = L"Debug";
      this->tpDebug->UseVisualStyleBackColor = true;
      // 
      // lvDebug
      // 
      this->lvDebug->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(1) { this->columnHeader12 });
      this->lvDebug->Location = System::Drawing::Point(20, 19);
      this->lvDebug->Name = L"lvDebug";
      this->lvDebug->Size = System::Drawing::Size(388, 271);
      this->lvDebug->TabIndex = 0;
      this->lvDebug->UseCompatibleStateImageBehavior = false;
      this->lvDebug->View = System::Windows::Forms::View::Details;
      // 
      // columnHeader12
      // 
      this->columnHeader12->Text = L"Text";
      this->columnHeader12->Width = 361;
      // 
      // tpDataToLocoSim
      // 
      this->tpDataToLocoSim->Controls->Add(this->label14);
      this->tpDataToLocoSim->Controls->Add(this->lvDataToLocoSim);
      this->tpDataToLocoSim->Location = System::Drawing::Point(4, 22);
      this->tpDataToLocoSim->Name = L"tpDataToLocoSim";
      this->tpDataToLocoSim->Padding = System::Windows::Forms::Padding(3);
      this->tpDataToLocoSim->Size = System::Drawing::Size(425, 464);
      this->tpDataToLocoSim->TabIndex = 4;
      this->tpDataToLocoSim->Text = L"To xxSim";
      this->tpDataToLocoSim->UseVisualStyleBackColor = true;
      // 
      // label14
      // 
      this->label14->AutoSize = true;
      this->label14->Location = System::Drawing::Point(23, 14);
      this->label14->Name = L"label14";
      this->label14->Size = System::Drawing::Size(167, 13);
      this->label14->TabIndex = 4;
      this->label14->Text = L"Data to simulator (LocoSim/VSIM)";
      this->label14->Click += gcnew System::EventHandler(this, &LCSSimForm::label14_Click);
      // 
      // lvDataToLocoSim
      // 
      this->lvDataToLocoSim->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(2) {
        this->columnHeader5,
          this->columnHeader6
      });
      this->lvDataToLocoSim->GridLines = true;
      this->lvDataToLocoSim->Items->AddRange(gcnew cli::array< System::Windows::Forms::ListViewItem^  >(1) { listViewItem1 });
      this->lvDataToLocoSim->Location = System::Drawing::Point(23, 33);
      this->lvDataToLocoSim->Name = L"lvDataToLocoSim";
      this->lvDataToLocoSim->Size = System::Drawing::Size(384, 400);
      this->lvDataToLocoSim->TabIndex = 3;
      this->lvDataToLocoSim->UseCompatibleStateImageBehavior = false;
      this->lvDataToLocoSim->View = System::Windows::Forms::View::Details;
      // 
      // columnHeader5
      // 
      this->columnHeader5->Text = L"Item";
      this->columnHeader5->Width = 130;
      // 
      // columnHeader6
      // 
      this->columnHeader6->Text = L"Data";
      this->columnHeader6->Width = 250;
      // 
      // tpDataFromLocoSim
      // 
      this->tpDataFromLocoSim->Controls->Add(this->label13);
      this->tpDataFromLocoSim->Controls->Add(this->lvDataFromLocoSim);
      this->tpDataFromLocoSim->Location = System::Drawing::Point(4, 22);
      this->tpDataFromLocoSim->Name = L"tpDataFromLocoSim";
      this->tpDataFromLocoSim->Padding = System::Windows::Forms::Padding(3);
      this->tpDataFromLocoSim->Size = System::Drawing::Size(425, 464);
      this->tpDataFromLocoSim->TabIndex = 3;
      this->tpDataFromLocoSim->Text = L"From xxSim";
      this->tpDataFromLocoSim->UseVisualStyleBackColor = true;
      // 
      // label13
      // 
      this->label13->AutoSize = true;
      this->label13->Location = System::Drawing::Point(23, 14);
      this->label13->Name = L"label13";
      this->label13->Size = System::Drawing::Size(178, 13);
      this->label13->TabIndex = 4;
      this->label13->Text = L"Data from simulator (LocoSim/VSIM)";
      this->label13->Click += gcnew System::EventHandler(this, &LCSSimForm::label13_Click);
      // 
      // lvDataFromLocoSim
      // 
      this->lvDataFromLocoSim->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(2) {
        this->columnHeader3,
          this->columnHeader4
      });
      this->lvDataFromLocoSim->GridLines = true;
      this->lvDataFromLocoSim->Items->AddRange(gcnew cli::array< System::Windows::Forms::ListViewItem^  >(1) { listViewItem2 });
      this->lvDataFromLocoSim->Location = System::Drawing::Point(23, 33);
      this->lvDataFromLocoSim->Name = L"lvDataFromLocoSim";
      this->lvDataFromLocoSim->Size = System::Drawing::Size(384, 400);
      this->lvDataFromLocoSim->TabIndex = 3;
      this->lvDataFromLocoSim->UseCompatibleStateImageBehavior = false;
      this->lvDataFromLocoSim->View = System::Windows::Forms::View::Details;
      this->lvDataFromLocoSim->SelectedIndexChanged += gcnew System::EventHandler(this, &LCSSimForm::listView1_SelectedIndexChanged);
      // 
      // columnHeader3
      // 
      this->columnHeader3->Text = L"Item";
      this->columnHeader3->Width = 130;
      // 
      // columnHeader4
      // 
      this->columnHeader4->Text = L"Data";
      this->columnHeader4->Width = 250;
      // 
      // tpDataToATO
      // 
      this->tpDataToATO->Controls->Add(this->label12);
      this->tpDataToATO->Controls->Add(this->lvDataToATO);
      this->tpDataToATO->Location = System::Drawing::Point(4, 22);
      this->tpDataToATO->Name = L"tpDataToATO";
      this->tpDataToATO->Padding = System::Windows::Forms::Padding(3);
      this->tpDataToATO->Size = System::Drawing::Size(425, 464);
      this->tpDataToATO->TabIndex = 2;
      this->tpDataToATO->Text = L"To ATP";
      this->tpDataToATO->UseVisualStyleBackColor = true;
      // 
      // label12
      // 
      this->label12->AutoSize = true;
      this->label12->Location = System::Drawing::Point(23, 14);
      this->label12->Name = L"label12";
      this->label12->Size = System::Drawing::Size(89, 13);
      this->label12->TabIndex = 4;
      this->label12->Text = L"Data sent to ATP";
      this->label12->Click += gcnew System::EventHandler(this, &LCSSimForm::label12_Click);
      // 
      // lvDataToATO
      // 
      this->lvDataToATO->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(2) {
        this->columnHeader1,
          this->columnHeader2
      });
      this->lvDataToATO->GridLines = true;
      this->lvDataToATO->Items->AddRange(gcnew cli::array< System::Windows::Forms::ListViewItem^  >(1) { listViewItem3 });
      this->lvDataToATO->Location = System::Drawing::Point(23, 33);
      this->lvDataToATO->Name = L"lvDataToATO";
      this->lvDataToATO->Size = System::Drawing::Size(384, 400);
      this->lvDataToATO->TabIndex = 3;
      this->lvDataToATO->UseCompatibleStateImageBehavior = false;
      this->lvDataToATO->View = System::Windows::Forms::View::Details;
      this->lvDataToATO->SelectedIndexChanged += gcnew System::EventHandler(this, &LCSSimForm::lvDataToATO_SelectedIndexChanged);
      // 
      // columnHeader1
      // 
      this->columnHeader1->Text = L"Item";
      this->columnHeader1->Width = 130;
      // 
      // columnHeader2
      // 
      this->columnHeader2->Text = L"Data";
      this->columnHeader2->Width = 250;
      // 
      // tpDataFromATO
      // 
      this->tpDataFromATO->Controls->Add(this->label11);
      this->tpDataFromATO->Controls->Add(this->lvDataFromATO);
      this->tpDataFromATO->Location = System::Drawing::Point(4, 22);
      this->tpDataFromATO->Name = L"tpDataFromATO";
      this->tpDataFromATO->Padding = System::Windows::Forms::Padding(3);
      this->tpDataFromATO->Size = System::Drawing::Size(425, 464);
      this->tpDataFromATO->TabIndex = 1;
      this->tpDataFromATO->Text = L"From ATP";
      this->tpDataFromATO->UseVisualStyleBackColor = true;
      // 
      // label11
      // 
      this->label11->AutoSize = true;
      this->label11->Location = System::Drawing::Point(23, 14);
      this->label11->Name = L"label11";
      this->label11->Size = System::Drawing::Size(77, 13);
      this->label11->TabIndex = 2;
      this->label11->Text = L"Data from ATP";
      // 
      // lvDataFromATO
      // 
      this->lvDataFromATO->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(2) { this->chItem, this->chData });
      this->lvDataFromATO->GridLines = true;
      this->lvDataFromATO->Items->AddRange(gcnew cli::array< System::Windows::Forms::ListViewItem^  >(1) { listViewItem4 });
      this->lvDataFromATO->Location = System::Drawing::Point(23, 33);
      this->lvDataFromATO->Name = L"lvDataFromATO";
      this->lvDataFromATO->Size = System::Drawing::Size(384, 400);
      this->lvDataFromATO->TabIndex = 1;
      this->lvDataFromATO->UseCompatibleStateImageBehavior = false;
      this->lvDataFromATO->View = System::Windows::Forms::View::Details;
      // 
      // chItem
      // 
      this->chItem->Text = L"Item";
      this->chItem->Width = 130;
      // 
      // chData
      // 
      this->chData->Text = L"Data";
      this->chData->Width = 250;
      // 
      // tpLCSBrakes
      // 
      this->tpLCSBrakes->Controls->Add(this->bApplyBrakes);
      this->tpLCSBrakes->Controls->Add(this->groupBox8);
      this->tpLCSBrakes->Controls->Add(this->groupBox7);
      this->tpLCSBrakes->Location = System::Drawing::Point(4, 22);
      this->tpLCSBrakes->Name = L"tpLCSBrakes";
      this->tpLCSBrakes->Size = System::Drawing::Size(425, 464);
      this->tpLCSBrakes->TabIndex = 9;
      this->tpLCSBrakes->Text = L"LCS Brakes";
      this->tpLCSBrakes->UseVisualStyleBackColor = true;
      // 
      // bApplyBrakes
      // 
      this->bApplyBrakes->Location = System::Drawing::Point(174, 327);
      this->bApplyBrakes->Name = L"bApplyBrakes";
      this->bApplyBrakes->Size = System::Drawing::Size(75, 23);
      this->bApplyBrakes->TabIndex = 83;
      this->bApplyBrakes->Text = L"Apply";
      this->bApplyBrakes->UseVisualStyleBackColor = true;
      this->bApplyBrakes->Click += gcnew System::EventHandler(this, &LCSSimForm::bApplyBrakes_Click_1);
      // 
      // groupBox8
      // 
      this->groupBox8->Controls->Add(this->label41);
      this->groupBox8->Controls->Add(this->label40);
      this->groupBox8->Controls->Add(this->tbLastCarBrakePressureLCS);
      this->groupBox8->Controls->Add(this->label33);
      this->groupBox8->Controls->Add(this->tbPercentageOfOpBrakesECPBLCS);
      this->groupBox8->Controls->Add(this->label32);
      this->groupBox8->Location = System::Drawing::Point(15, 212);
      this->groupBox8->Name = L"groupBox8";
      this->groupBox8->Size = System::Drawing::Size(393, 93);
      this->groupBox8->TabIndex = 48;
      this->groupBox8->TabStop = false;
      this->groupBox8->Text = L"Params";
      // 
      // label41
      // 
      this->label41->AutoSize = true;
      this->label41->Location = System::Drawing::Point(305, 32);
      this->label41->Name = L"label41";
      this->label41->Size = System::Drawing::Size(15, 13);
      this->label41->TabIndex = 52;
      this->label41->Text = L"%";
      // 
      // label40
      // 
      this->label40->AutoSize = true;
      this->label40->Location = System::Drawing::Point(306, 61);
      this->label40->Name = L"label40";
      this->label40->Size = System::Drawing::Size(45, 13);
      this->label40->TabIndex = 51;
      this->label40->Text = L"x 0.1bar";
      // 
      // tbLastCarBrakePressureLCS
      // 
      this->tbLastCarBrakePressureLCS->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbLastCarBrakePressureLCS->Location = System::Drawing::Point(157, 55);
      this->tbLastCarBrakePressureLCS->Name = L"tbLastCarBrakePressureLCS";
      this->tbLastCarBrakePressureLCS->Size = System::Drawing::Size(142, 20);
      this->tbLastCarBrakePressureLCS->TabIndex = 50;
      this->tbLastCarBrakePressureLCS->TabStop = false;
      this->tbLastCarBrakePressureLCS->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label33
      // 
      this->label33->AutoSize = true;
      this->label33->Location = System::Drawing::Point(11, 58);
      this->label33->Name = L"label33";
      this->label33->Size = System::Drawing::Size(124, 13);
      this->label33->TabIndex = 49;
      this->label33->Text = L"Last Car Brake Pressure:";
      // 
      // tbPercentageOfOpBrakesECPBLCS
      // 
      this->tbPercentageOfOpBrakesECPBLCS->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbPercentageOfOpBrakesECPBLCS->Location = System::Drawing::Point(157, 29);
      this->tbPercentageOfOpBrakesECPBLCS->Name = L"tbPercentageOfOpBrakesECPBLCS";
      this->tbPercentageOfOpBrakesECPBLCS->Size = System::Drawing::Size(142, 20);
      this->tbPercentageOfOpBrakesECPBLCS->TabIndex = 48;
      this->tbPercentageOfOpBrakesECPBLCS->TabStop = false;
      this->tbPercentageOfOpBrakesECPBLCS->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label32
      // 
      this->label32->AutoSize = true;
      this->label32->Location = System::Drawing::Point(11, 32);
      this->label32->Name = L"label32";
      this->label32->Size = System::Drawing::Size(113, 13);
      this->label32->TabIndex = 47;
      this->label32->Text = L"Percentage of Brakes:";
      // 
      // groupBox7
      // 
      this->groupBox7->Controls->Add(this->cbPenaltyBrkActive);
      this->groupBox7->Controls->Add(this->label83);
      this->groupBox7->Controls->Add(this->cbTrainIntegrityStatusECPBLCS);
      this->groupBox7->Controls->Add(this->label31);
      this->groupBox7->Controls->Add(this->cbECPBOperatingModeLCS);
      this->groupBox7->Controls->Add(this->label30);
      this->groupBox7->Controls->Add(this->cbBrakeSystemInUseLCS);
      this->groupBox7->Controls->Add(this->label29);
      this->groupBox7->Controls->Add(this->cbECPBSequenceStatusLCS);
      this->groupBox7->Controls->Add(this->label27);
      this->groupBox7->Location = System::Drawing::Point(15, 13);
      this->groupBox7->Name = L"groupBox7";
      this->groupBox7->Size = System::Drawing::Size(394, 173);
      this->groupBox7->TabIndex = 47;
      this->groupBox7->TabStop = false;
      this->groupBox7->Text = L"Modes/Status";
      // 
      // cbPenaltyBrkActive
      // 
      this->cbPenaltyBrkActive->DisplayMember = L"1";
      this->cbPenaltyBrkActive->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbPenaltyBrkActive->DropDownWidth = 120;
      this->cbPenaltyBrkActive->FormattingEnabled = true;
      this->cbPenaltyBrkActive->Items->AddRange(gcnew cli::array< System::Object^  >(2) { L"Not Active", L"Active" });
      this->cbPenaltyBrkActive->Location = System::Drawing::Point(157, 139);
      this->cbPenaltyBrkActive->Name = L"cbPenaltyBrkActive";
      this->cbPenaltyBrkActive->Size = System::Drawing::Size(142, 21);
      this->cbPenaltyBrkActive->TabIndex = 50;
      // 
      // label83
      // 
      this->label83->AutoSize = true;
      this->label83->Location = System::Drawing::Point(10, 139);
      this->label83->Name = L"label83";
      this->label83->Size = System::Drawing::Size(136, 13);
      this->label83->TabIndex = 49;
      this->label83->Text = L"Loco Penalty Brake Active:";
      // 
      // cbTrainIntegrityStatusECPBLCS
      // 
      this->cbTrainIntegrityStatusECPBLCS->DisplayMember = L"1";
      this->cbTrainIntegrityStatusECPBLCS->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbTrainIntegrityStatusECPBLCS->DropDownWidth = 120;
      this->cbTrainIntegrityStatusECPBLCS->FormattingEnabled = true;
      this->cbTrainIntegrityStatusECPBLCS->Items->AddRange(gcnew cli::array< System::Object^  >(3) { L"Waiting", L"Confirmed", L"Not Asserted" });
      this->cbTrainIntegrityStatusECPBLCS->Location = System::Drawing::Point(157, 107);
      this->cbTrainIntegrityStatusECPBLCS->Name = L"cbTrainIntegrityStatusECPBLCS";
      this->cbTrainIntegrityStatusECPBLCS->Size = System::Drawing::Size(142, 21);
      this->cbTrainIntegrityStatusECPBLCS->TabIndex = 48;
      this->cbTrainIntegrityStatusECPBLCS->SelectedIndexChanged += gcnew System::EventHandler(this, &LCSSimForm::cbTrainIntegrityStatusECPBLCS_SelectedIndexChanged);
      // 
      // label31
      // 
      this->label31->AutoSize = true;
      this->label31->Location = System::Drawing::Point(11, 110);
      this->label31->Name = L"label31";
      this->label31->Size = System::Drawing::Size(138, 13);
      this->label31->TabIndex = 47;
      this->label31->Text = L"Train Integrity Status ECPB:";
      // 
      // cbECPBOperatingModeLCS
      // 
      this->cbECPBOperatingModeLCS->DisplayMember = L"1";
      this->cbECPBOperatingModeLCS->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbECPBOperatingModeLCS->DropDownWidth = 120;
      this->cbECPBOperatingModeLCS->FormattingEnabled = true;
      this->cbECPBOperatingModeLCS->Items->AddRange(gcnew cli::array< System::Object^  >(5) {
        L"Run", L"Initialization", L"Switch",
          L"CutOut", L"Not Available"
      });
      this->cbECPBOperatingModeLCS->Location = System::Drawing::Point(157, 53);
      this->cbECPBOperatingModeLCS->Name = L"cbECPBOperatingModeLCS";
      this->cbECPBOperatingModeLCS->Size = System::Drawing::Size(142, 21);
      this->cbECPBOperatingModeLCS->TabIndex = 46;
      // 
      // label30
      // 
      this->label30->AutoSize = true;
      this->label30->Location = System::Drawing::Point(11, 56);
      this->label30->Name = L"label30";
      this->label30->Size = System::Drawing::Size(121, 13);
      this->label30->TabIndex = 45;
      this->label30->Text = L"ECPB Operating modes:";
      // 
      // cbBrakeSystemInUseLCS
      // 
      this->cbBrakeSystemInUseLCS->DisplayMember = L"1";
      this->cbBrakeSystemInUseLCS->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbBrakeSystemInUseLCS->DropDownWidth = 120;
      this->cbBrakeSystemInUseLCS->FormattingEnabled = true;
      this->cbBrakeSystemInUseLCS->Items->AddRange(gcnew cli::array< System::Object^  >(2) { L"Pneumatic", L"ECPB" });
      this->cbBrakeSystemInUseLCS->Location = System::Drawing::Point(157, 26);
      this->cbBrakeSystemInUseLCS->Name = L"cbBrakeSystemInUseLCS";
      this->cbBrakeSystemInUseLCS->Size = System::Drawing::Size(142, 21);
      this->cbBrakeSystemInUseLCS->TabIndex = 44;
      // 
      // label29
      // 
      this->label29->AutoSize = true;
      this->label29->Location = System::Drawing::Point(11, 29);
      this->label29->Name = L"label29";
      this->label29->Size = System::Drawing::Size(109, 13);
      this->label29->TabIndex = 43;
      this->label29->Text = L"Brake System In Use:";
      // 
      // cbECPBSequenceStatusLCS
      // 
      this->cbECPBSequenceStatusLCS->DisplayMember = L"1";
      this->cbECPBSequenceStatusLCS->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbECPBSequenceStatusLCS->DropDownWidth = 120;
      this->cbECPBSequenceStatusLCS->FormattingEnabled = true;
      this->cbECPBSequenceStatusLCS->Items->AddRange(gcnew cli::array< System::Object^  >(3) { L"Unknown", L"Scanning", L"ConfigKnown" });
      this->cbECPBSequenceStatusLCS->Location = System::Drawing::Point(157, 80);
      this->cbECPBSequenceStatusLCS->Name = L"cbECPBSequenceStatusLCS";
      this->cbECPBSequenceStatusLCS->Size = System::Drawing::Size(142, 21);
      this->cbECPBSequenceStatusLCS->TabIndex = 42;
      // 
      // label27
      // 
      this->label27->AutoSize = true;
      this->label27->Location = System::Drawing::Point(11, 83);
      this->label27->Name = L"label27";
      this->label27->Size = System::Drawing::Size(123, 13);
      this->label27->TabIndex = 41;
      this->label27->Text = L"ECPB Sequence Status:";
      // 
      // tpLCSATO
      // 
      this->tpLCSATO->Controls->Add(this->groupBox16);
      this->tpLCSATO->Controls->Add(this->bApplyAto);
      this->tpLCSATO->Controls->Add(this->groupBox5);
      this->tpLCSATO->Controls->Add(this->groupBox4);
      this->tpLCSATO->Location = System::Drawing::Point(4, 22);
      this->tpLCSATO->Name = L"tpLCSATO";
      this->tpLCSATO->Size = System::Drawing::Size(425, 464);
      this->tpLCSATO->TabIndex = 8;
      this->tpLCSATO->Text = L"LCS ATO";
      this->tpLCSATO->UseVisualStyleBackColor = true;
      // 
      // groupBox16
      // 
      this->groupBox16->Controls->Add(this->cbADSBitFieldATPWarningDefined);
      this->groupBox16->Controls->Add(this->cbADSBitFieldWeightDefined);
      this->groupBox16->Controls->Add(this->cbADSBitFieldMADefined);
      this->groupBox16->Controls->Add(this->cbADSBitFieldTrainConfigDefined);
      this->groupBox16->Controls->Add(this->cbADSBitFieldPathDefined);
      this->groupBox16->Location = System::Drawing::Point(20, 350);
      this->groupBox16->Name = L"groupBox16";
      this->groupBox16->Size = System::Drawing::Size(391, 71);
      this->groupBox16->TabIndex = 84;
      this->groupBox16->TabStop = false;
      this->groupBox16->Text = L"ADS Bitfield";
      // 
      // cbADSBitFieldATPWarningDefined
      // 
      this->cbADSBitFieldATPWarningDefined->AutoSize = true;
      this->cbADSBitFieldATPWarningDefined->Location = System::Drawing::Point(229, 19);
      this->cbADSBitFieldATPWarningDefined->Name = L"cbADSBitFieldATPWarningDefined";
      this->cbADSBitFieldATPWarningDefined->Size = System::Drawing::Size(168, 17);
      this->cbADSBitFieldATPWarningDefined->TabIndex = 95;
      this->cbADSBitFieldATPWarningDefined->Text = L"2-ATP Warning Curve defined";
      this->cbADSBitFieldATPWarningDefined->UseVisualStyleBackColor = true;
      // 
      // cbADSBitFieldWeightDefined
      // 
      this->cbADSBitFieldWeightDefined->AutoSize = true;
      this->cbADSBitFieldWeightDefined->Location = System::Drawing::Point(213, 42);
      this->cbADSBitFieldWeightDefined->Name = L"cbADSBitFieldWeightDefined";
      this->cbADSBitFieldWeightDefined->Size = System::Drawing::Size(107, 17);
      this->cbADSBitFieldWeightDefined->TabIndex = 88;
      this->cbADSBitFieldWeightDefined->Text = L"4-Weight defined";
      this->cbADSBitFieldWeightDefined->UseVisualStyleBackColor = true;
      // 
      // cbADSBitFieldMADefined
      // 
      this->cbADSBitFieldMADefined->AutoSize = true;
      this->cbADSBitFieldMADefined->Location = System::Drawing::Point(139, 19);
      this->cbADSBitFieldMADefined->Name = L"cbADSBitFieldMADefined";
      this->cbADSBitFieldMADefined->Size = System::Drawing::Size(89, 17);
      this->cbADSBitFieldMADefined->TabIndex = 87;
      this->cbADSBitFieldMADefined->Text = L"1-MA defined";
      this->cbADSBitFieldMADefined->UseVisualStyleBackColor = true;
      // 
      // cbADSBitFieldTrainConfigDefined
      // 
      this->cbADSBitFieldTrainConfigDefined->AutoSize = true;
      this->cbADSBitFieldTrainConfigDefined->Location = System::Drawing::Point(69, 42);
      this->cbADSBitFieldTrainConfigDefined->Name = L"cbADSBitFieldTrainConfigDefined";
      this->cbADSBitFieldTrainConfigDefined->Size = System::Drawing::Size(130, 17);
      this->cbADSBitFieldTrainConfigDefined->TabIndex = 80;
      this->cbADSBitFieldTrainConfigDefined->Text = L"3-Train Config defined";
      this->cbADSBitFieldTrainConfigDefined->UseVisualStyleBackColor = true;
      // 
      // cbADSBitFieldPathDefined
      // 
      this->cbADSBitFieldPathDefined->AutoSize = true;
      this->cbADSBitFieldPathDefined->Location = System::Drawing::Point(26, 19);
      this->cbADSBitFieldPathDefined->Name = L"cbADSBitFieldPathDefined";
      this->cbADSBitFieldPathDefined->Size = System::Drawing::Size(95, 17);
      this->cbADSBitFieldPathDefined->TabIndex = 79;
      this->cbADSBitFieldPathDefined->Text = L"0-Path defined";
      this->cbADSBitFieldPathDefined->UseVisualStyleBackColor = true;
      // 
      // bApplyAto
      // 
      this->bApplyAto->Location = System::Drawing::Point(179, 428);
      this->bApplyAto->Name = L"bApplyAto";
      this->bApplyAto->Size = System::Drawing::Size(75, 23);
      this->bApplyAto->TabIndex = 83;
      this->bApplyAto->Text = L"Apply";
      this->bApplyAto->UseVisualStyleBackColor = true;
      this->bApplyAto->Click += gcnew System::EventHandler(this, &LCSSimForm::bApplyAto_Click_1);
      // 
      // groupBox5
      // 
      this->groupBox5->Controls->Add(this->cbBlueFlagRequestLCS);
      this->groupBox5->Controls->Add(this->label24);
      this->groupBox5->Controls->Add(this->cbBlueFlagStatusLCS);
      this->groupBox5->Controls->Add(this->label23);
      this->groupBox5->Location = System::Drawing::Point(18, 245);
      this->groupBox5->Name = L"groupBox5";
      this->groupBox5->Size = System::Drawing::Size(385, 90);
      this->groupBox5->TabIndex = 44;
      this->groupBox5->TabStop = false;
      this->groupBox5->Text = L"BlueFlag";
      // 
      // cbBlueFlagRequestLCS
      // 
      this->cbBlueFlagRequestLCS->DisplayMember = L"1";
      this->cbBlueFlagRequestLCS->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbBlueFlagRequestLCS->DropDownWidth = 120;
      this->cbBlueFlagRequestLCS->FormattingEnabled = true;
      this->cbBlueFlagRequestLCS->Items->AddRange(gcnew cli::array< System::Object^  >(3) {
        L"Not Requested", L"Activation Request",
          L"Deactivation Request"
      });
      this->cbBlueFlagRequestLCS->Location = System::Drawing::Point(161, 56);
      this->cbBlueFlagRequestLCS->Name = L"cbBlueFlagRequestLCS";
      this->cbBlueFlagRequestLCS->Size = System::Drawing::Size(142, 21);
      this->cbBlueFlagRequestLCS->TabIndex = 44;
      // 
      // label24
      // 
      this->label24->AutoSize = true;
      this->label24->Location = System::Drawing::Point(15, 59);
      this->label24->Name = L"label24";
      this->label24->Size = System::Drawing::Size(97, 13);
      this->label24->TabIndex = 43;
      this->label24->Text = L"Blue Flag Request:";
      // 
      // cbBlueFlagStatusLCS
      // 
      this->cbBlueFlagStatusLCS->DisplayMember = L"1";
      this->cbBlueFlagStatusLCS->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbBlueFlagStatusLCS->DropDownWidth = 120;
      this->cbBlueFlagStatusLCS->FormattingEnabled = true;
      this->cbBlueFlagStatusLCS->Items->AddRange(gcnew cli::array< System::Object^  >(2) { L"Inactive", L"Activated" });
      this->cbBlueFlagStatusLCS->Location = System::Drawing::Point(161, 29);
      this->cbBlueFlagStatusLCS->Name = L"cbBlueFlagStatusLCS";
      this->cbBlueFlagStatusLCS->Size = System::Drawing::Size(142, 21);
      this->cbBlueFlagStatusLCS->TabIndex = 42;
      // 
      // label23
      // 
      this->label23->AutoSize = true;
      this->label23->Location = System::Drawing::Point(15, 32);
      this->label23->Name = L"label23";
      this->label23->Size = System::Drawing::Size(87, 13);
      this->label23->TabIndex = 41;
      this->label23->Text = L"Blue Flag Status:";
      // 
      // groupBox4
      // 
      this->groupBox4->Controls->Add(this->label39);
      this->groupBox4->Controls->Add(this->tbADSEtaLCS);
      this->groupBox4->Controls->Add(this->label111);
      this->groupBox4->Controls->Add(this->cbADSEtaStatusLCS);
      this->groupBox4->Controls->Add(this->label25);
      this->groupBox4->Controls->Add(this->cbReadyForPrecisionStopLCS);
      this->groupBox4->Controls->Add(this->label28);
      this->groupBox4->Controls->Add(this->cbLcsATOReadyLCS);
      this->groupBox4->Controls->Add(this->label26);
      this->groupBox4->Controls->Add(this->label22);
      this->groupBox4->Controls->Add(this->cbTractionModeLCS);
      this->groupBox4->Controls->Add(this->label21);
      this->groupBox4->Controls->Add(this->cbATODrivingModeLCS);
      this->groupBox4->Controls->Add(this->label20);
      this->groupBox4->Controls->Add(this->cbATOCabinSelectorStatusLCS);
      this->groupBox4->Location = System::Drawing::Point(18, 18);
      this->groupBox4->Name = L"groupBox4";
      this->groupBox4->Size = System::Drawing::Size(386, 220);
      this->groupBox4->TabIndex = 43;
      this->groupBox4->TabStop = false;
      this->groupBox4->Text = L"ATO Modes/Params";
      // 
      // label39
      // 
      this->label39->AutoSize = true;
      this->label39->Location = System::Drawing::Point(309, 111);
      this->label39->Name = L"label39";
      this->label39->Size = System::Drawing::Size(49, 13);
      this->label39->TabIndex = 49;
      this->label39->Text = L"UTC sec";
      // 
      // tbADSEtaLCS
      // 
      this->tbADSEtaLCS->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbADSEtaLCS->Location = System::Drawing::Point(161, 108);
      this->tbADSEtaLCS->Name = L"tbADSEtaLCS";
      this->tbADSEtaLCS->Size = System::Drawing::Size(142, 20);
      this->tbADSEtaLCS->TabIndex = 48;
      this->tbADSEtaLCS->TabStop = false;
      this->tbADSEtaLCS->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label111
      // 
      this->label111->AutoSize = true;
      this->label111->Location = System::Drawing::Point(15, 138);
      this->label111->Name = L"label111";
      this->label111->Size = System::Drawing::Size(89, 13);
      this->label111->TabIndex = 45;
      this->label111->Text = L"ADS ETA Status:";
      // 
      // cbADSEtaStatusLCS
      // 
      this->cbADSEtaStatusLCS->DisplayMember = L"1";
      this->cbADSEtaStatusLCS->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbADSEtaStatusLCS->DropDownWidth = 120;
      this->cbADSEtaStatusLCS->FormattingEnabled = true;
      this->cbADSEtaStatusLCS->Items->AddRange(gcnew cli::array< System::Object^  >(4) {
        L"Not Defined", L"Calculating", L"Request Accepted",
          L"Request Rejected"
      });
      this->cbADSEtaStatusLCS->Location = System::Drawing::Point(161, 131);
      this->cbADSEtaStatusLCS->Name = L"cbADSEtaStatusLCS";
      this->cbADSEtaStatusLCS->Size = System::Drawing::Size(142, 21);
      this->cbADSEtaStatusLCS->TabIndex = 46;
      // 
      // label25
      // 
      this->label25->AutoSize = true;
      this->label25->Location = System::Drawing::Point(15, 111);
      this->label25->Name = L"label25";
      this->label25->Size = System::Drawing::Size(56, 13);
      this->label25->TabIndex = 47;
      this->label25->Text = L"ADS ETA:";
      // 
      // cbReadyForPrecisionStopLCS
      // 
      this->cbReadyForPrecisionStopLCS->DisplayMember = L"1";
      this->cbReadyForPrecisionStopLCS->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbReadyForPrecisionStopLCS->DropDownWidth = 120;
      this->cbReadyForPrecisionStopLCS->FormattingEnabled = true;
      this->cbReadyForPrecisionStopLCS->Items->AddRange(gcnew cli::array< System::Object^  >(2) { L"No Action", L"Ready" });
      this->cbReadyForPrecisionStopLCS->Location = System::Drawing::Point(161, 186);
      this->cbReadyForPrecisionStopLCS->Name = L"cbReadyForPrecisionStopLCS";
      this->cbReadyForPrecisionStopLCS->Size = System::Drawing::Size(142, 21);
      this->cbReadyForPrecisionStopLCS->TabIndex = 46;
      // 
      // label28
      // 
      this->label28->AutoSize = true;
      this->label28->Location = System::Drawing::Point(15, 190);
      this->label28->Name = L"label28";
      this->label28->Size = System::Drawing::Size(126, 13);
      this->label28->TabIndex = 45;
      this->label28->Text = L"Ready for precision Stop:";
      // 
      // cbLcsATOReadyLCS
      // 
      this->cbLcsATOReadyLCS->DisplayMember = L"1";
      this->cbLcsATOReadyLCS->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbLcsATOReadyLCS->DropDownWidth = 120;
      this->cbLcsATOReadyLCS->FormattingEnabled = true;
      this->cbLcsATOReadyLCS->Items->AddRange(gcnew cli::array< System::Object^  >(2) { L"Not Ready", L"Ready" });
      this->cbLcsATOReadyLCS->Location = System::Drawing::Point(161, 157);
      this->cbLcsATOReadyLCS->Name = L"cbLcsATOReadyLCS";
      this->cbLcsATOReadyLCS->Size = System::Drawing::Size(142, 21);
      this->cbLcsATOReadyLCS->TabIndex = 44;
      // 
      // label26
      // 
      this->label26->AutoSize = true;
      this->label26->Location = System::Drawing::Point(15, 161);
      this->label26->Name = L"label26";
      this->label26->Size = System::Drawing::Size(89, 13);
      this->label26->TabIndex = 43;
      this->label26->Text = L"LCS ATO Ready:";
      // 
      // label22
      // 
      this->label22->AutoSize = true;
      this->label22->Location = System::Drawing::Point(15, 84);
      this->label22->Name = L"label22";
      this->label22->Size = System::Drawing::Size(99, 13);
      this->label22->TabIndex = 38;
      this->label22->Text = L"Free Rolling Status:";
      // 
      // cbTractionModeLCS
      // 
      this->cbTractionModeLCS->DisplayMember = L"1";
      this->cbTractionModeLCS->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbTractionModeLCS->DropDownWidth = 120;
      this->cbTractionModeLCS->FormattingEnabled = true;
      this->cbTractionModeLCS->Items->AddRange(gcnew cli::array< System::Object^  >(2) { L"Normal", L"Free Roll" });
      this->cbTractionModeLCS->Location = System::Drawing::Point(161, 81);
      this->cbTractionModeLCS->Name = L"cbTractionModeLCS";
      this->cbTractionModeLCS->Size = System::Drawing::Size(142, 21);
      this->cbTractionModeLCS->TabIndex = 37;
      this->cbTractionModeLCS->SelectedIndexChanged += gcnew System::EventHandler(this, &LCSSimForm::cbTractionModeLCS_SelectedIndexChanged);
      // 
      // label21
      // 
      this->label21->AutoSize = true;
      this->label21->Location = System::Drawing::Point(15, 56);
      this->label21->Name = L"label21";
      this->label21->Size = System::Drawing::Size(98, 13);
      this->label21->TabIndex = 36;
      this->label21->Text = L"ATO Driving Mode:";
      // 
      // cbATODrivingModeLCS
      // 
      this->cbATODrivingModeLCS->DisplayMember = L"1";
      this->cbATODrivingModeLCS->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbATODrivingModeLCS->DropDownWidth = 120;
      this->cbATODrivingModeLCS->FormattingEnabled = true;
      this->cbATODrivingModeLCS->Items->AddRange(gcnew cli::array< System::Object^  >(7) {
        L"None", L"ETA Pacing", L"Ceiling Speed",
          L"Loading", L"Precision Stop", L"Unloading", L"Not Asserted"
      });
      this->cbATODrivingModeLCS->Location = System::Drawing::Point(161, 53);
      this->cbATODrivingModeLCS->Name = L"cbATODrivingModeLCS";
      this->cbATODrivingModeLCS->Size = System::Drawing::Size(142, 21);
      this->cbATODrivingModeLCS->TabIndex = 35;
      // 
      // label20
      // 
      this->label20->AutoSize = true;
      this->label20->Location = System::Drawing::Point(15, 29);
      this->label20->Name = L"label20";
      this->label20->Size = System::Drawing::Size(140, 13);
      this->label20->TabIndex = 34;
      this->label20->Text = L"ATOMode Cabin Sel Status:";
      // 
      // cbATOCabinSelectorStatusLCS
      // 
      this->cbATOCabinSelectorStatusLCS->DisplayMember = L"1";
      this->cbATOCabinSelectorStatusLCS->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbATOCabinSelectorStatusLCS->DropDownWidth = 120;
      this->cbATOCabinSelectorStatusLCS->FormattingEnabled = true;
      this->cbATOCabinSelectorStatusLCS->Items->AddRange(gcnew cli::array< System::Object^  >(4) {
        L"Manual\t", L"Supervised", L"Automatic",
          L"Not Asserted"
      });
      this->cbATOCabinSelectorStatusLCS->Location = System::Drawing::Point(161, 26);
      this->cbATOCabinSelectorStatusLCS->Name = L"cbATOCabinSelectorStatusLCS";
      this->cbATOCabinSelectorStatusLCS->Size = System::Drawing::Size(142, 21);
      this->cbATOCabinSelectorStatusLCS->TabIndex = 33;
      // 
      // tpLCSFaults
      // 
      this->tpLCSFaults->Controls->Add(this->bApplyFaults);
      this->tpLCSFaults->Controls->Add(this->groupBox6);
      this->tpLCSFaults->Controls->Add(this->groupBox3);
      this->tpLCSFaults->Location = System::Drawing::Point(4, 22);
      this->tpLCSFaults->Name = L"tpLCSFaults";
      this->tpLCSFaults->Padding = System::Windows::Forms::Padding(3);
      this->tpLCSFaults->Size = System::Drawing::Size(425, 464);
      this->tpLCSFaults->TabIndex = 7;
      this->tpLCSFaults->Text = L"LCS Faults";
      this->tpLCSFaults->UseVisualStyleBackColor = true;
      this->tpLCSFaults->Click += gcnew System::EventHandler(this, &LCSSimForm::tpParams_Click);
      // 
      // bApplyFaults
      // 
      this->bApplyFaults->Location = System::Drawing::Point(174, 337);
      this->bApplyFaults->Name = L"bApplyFaults";
      this->bApplyFaults->Size = System::Drawing::Size(75, 23);
      this->bApplyFaults->TabIndex = 82;
      this->bApplyFaults->Text = L"Apply";
      this->bApplyFaults->UseVisualStyleBackColor = true;
      this->bApplyFaults->Click += gcnew System::EventHandler(this, &LCSSimForm::bApplyFaults_Click_1);
      // 
      // groupBox6
      // 
      this->groupBox6->Controls->Add(this->tbVersionADSMapMinorLCS);
      this->groupBox6->Controls->Add(this->label38);
      this->groupBox6->Controls->Add(this->label37);
      this->groupBox6->Controls->Add(this->tbVersionADSMapMajorLCS);
      this->groupBox6->Controls->Add(this->label36);
      this->groupBox6->Controls->Add(this->tbTrainWeightTonsLCS);
      this->groupBox6->Controls->Add(this->label34);
      this->groupBox6->Controls->Add(this->label35);
      this->groupBox6->Location = System::Drawing::Point(7, 6);
      this->groupBox6->Name = L"groupBox6";
      this->groupBox6->Size = System::Drawing::Size(411, 101);
      this->groupBox6->TabIndex = 81;
      this->groupBox6->TabStop = false;
      this->groupBox6->Text = L"Params";
      // 
      // tbVersionADSMapMinorLCS
      // 
      this->tbVersionADSMapMinorLCS->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbVersionADSMapMinorLCS->Location = System::Drawing::Point(289, 63);
      this->tbVersionADSMapMinorLCS->MaxLength = 3;
      this->tbVersionADSMapMinorLCS->Name = L"tbVersionADSMapMinorLCS";
      this->tbVersionADSMapMinorLCS->Size = System::Drawing::Size(28, 20);
      this->tbVersionADSMapMinorLCS->TabIndex = 89;
      this->tbVersionADSMapMinorLCS->TabStop = false;
      this->tbVersionADSMapMinorLCS->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label38
      // 
      this->label38->AutoSize = true;
      this->label38->Location = System::Drawing::Point(248, 66);
      this->label38->Name = L"label38";
      this->label38->Size = System::Drawing::Size(36, 13);
      this->label38->TabIndex = 88;
      this->label38->Text = L"Minor:";
      // 
      // label37
      // 
      this->label37->AutoSize = true;
      this->label37->Location = System::Drawing::Point(172, 66);
      this->label37->Name = L"label37";
      this->label37->Size = System::Drawing::Size(36, 13);
      this->label37->TabIndex = 86;
      this->label37->Text = L"Major:";
      // 
      // tbVersionADSMapMajorLCS
      // 
      this->tbVersionADSMapMajorLCS->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbVersionADSMapMajorLCS->Location = System::Drawing::Point(214, 63);
      this->tbVersionADSMapMajorLCS->MaxLength = 3;
      this->tbVersionADSMapMajorLCS->Name = L"tbVersionADSMapMajorLCS";
      this->tbVersionADSMapMajorLCS->Size = System::Drawing::Size(28, 20);
      this->tbVersionADSMapMajorLCS->TabIndex = 85;
      this->tbVersionADSMapMajorLCS->TabStop = false;
      this->tbVersionADSMapMajorLCS->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label36
      // 
      this->label36->AutoSize = true;
      this->label36->Location = System::Drawing::Point(323, 37);
      this->label36->Name = L"label36";
      this->label36->Size = System::Drawing::Size(27, 13);
      this->label36->TabIndex = 80;
      this->label36->Text = L"tons";
      // 
      // tbTrainWeightTonsLCS
      // 
      this->tbTrainWeightTonsLCS->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbTrainWeightTonsLCS->Location = System::Drawing::Point(175, 30);
      this->tbTrainWeightTonsLCS->Name = L"tbTrainWeightTonsLCS";
      this->tbTrainWeightTonsLCS->Size = System::Drawing::Size(142, 20);
      this->tbTrainWeightTonsLCS->TabIndex = 84;
      this->tbTrainWeightTonsLCS->TabStop = false;
      this->tbTrainWeightTonsLCS->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label34
      // 
      this->label34->AutoSize = true;
      this->label34->Location = System::Drawing::Point(29, 33);
      this->label34->Name = L"label34";
      this->label34->Size = System::Drawing::Size(147, 13);
      this->label34->TabIndex = 83;
      this->label34->Text = L"Estimated Total Train Weight:";
      // 
      // label35
      // 
      this->label35->AutoSize = true;
      this->label35->Location = System::Drawing::Point(29, 66);
      this->label35->Name = L"label35";
      this->label35->Size = System::Drawing::Size(96, 13);
      this->label35->TabIndex = 82;
      this->label35->Text = L"Version ADS MAP:";
      this->label35->Click += gcnew System::EventHandler(this, &LCSSimForm::label35_Click);
      // 
      // groupBox3
      // 
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS31);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS30);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS29);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS28);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS27);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS26);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS25);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS24);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS23);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS22);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS21);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS20);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS19);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS18);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS17);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS16);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS15);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS14);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS13);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS12);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS11);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS10);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS9);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS8);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS7);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS6);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS5);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS4);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS3);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS2);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS1);
      this->groupBox3->Controls->Add(this->cbLocoSystemFaultsLCS0);
      this->groupBox3->Location = System::Drawing::Point(6, 113);
      this->groupBox3->Name = L"groupBox3";
      this->groupBox3->Size = System::Drawing::Size(412, 208);
      this->groupBox3->TabIndex = 79;
      this->groupBox3->TabStop = false;
      this->groupBox3->Text = L"Loco System Faults";
      // 
      // cbLocoSystemFaultsLCS31
      // 
      this->cbLocoSystemFaultsLCS31->AutoSize = true;
      this->cbLocoSystemFaultsLCS31->Location = System::Drawing::Point(296, 180);
      this->cbLocoSystemFaultsLCS31->Name = L"cbLocoSystemFaultsLCS31";
      this->cbLocoSystemFaultsLCS31->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS31->TabIndex = 110;
      this->cbLocoSystemFaultsLCS31->Text = L"Fault Bit 31";
      this->cbLocoSystemFaultsLCS31->UseVisualStyleBackColor = true;
      this->cbLocoSystemFaultsLCS31->CheckedChanged += gcnew System::EventHandler(this, &LCSSimForm::checkBox25_CheckedChanged);
      // 
      // cbLocoSystemFaultsLCS30
      // 
      this->cbLocoSystemFaultsLCS30->AutoSize = true;
      this->cbLocoSystemFaultsLCS30->Location = System::Drawing::Point(296, 157);
      this->cbLocoSystemFaultsLCS30->Name = L"cbLocoSystemFaultsLCS30";
      this->cbLocoSystemFaultsLCS30->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS30->TabIndex = 109;
      this->cbLocoSystemFaultsLCS30->Text = L"Fault Bit 30";
      this->cbLocoSystemFaultsLCS30->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS29
      // 
      this->cbLocoSystemFaultsLCS29->AutoSize = true;
      this->cbLocoSystemFaultsLCS29->Location = System::Drawing::Point(296, 134);
      this->cbLocoSystemFaultsLCS29->Name = L"cbLocoSystemFaultsLCS29";
      this->cbLocoSystemFaultsLCS29->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS29->TabIndex = 108;
      this->cbLocoSystemFaultsLCS29->Text = L"Fault Bit 29";
      this->cbLocoSystemFaultsLCS29->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS28
      // 
      this->cbLocoSystemFaultsLCS28->AutoSize = true;
      this->cbLocoSystemFaultsLCS28->Location = System::Drawing::Point(296, 111);
      this->cbLocoSystemFaultsLCS28->Name = L"cbLocoSystemFaultsLCS28";
      this->cbLocoSystemFaultsLCS28->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS28->TabIndex = 107;
      this->cbLocoSystemFaultsLCS28->Text = L"Fault Bit 28";
      this->cbLocoSystemFaultsLCS28->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS27
      // 
      this->cbLocoSystemFaultsLCS27->AutoSize = true;
      this->cbLocoSystemFaultsLCS27->Location = System::Drawing::Point(296, 88);
      this->cbLocoSystemFaultsLCS27->Name = L"cbLocoSystemFaultsLCS27";
      this->cbLocoSystemFaultsLCS27->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS27->TabIndex = 106;
      this->cbLocoSystemFaultsLCS27->Text = L"Fault Bit 27";
      this->cbLocoSystemFaultsLCS27->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS26
      // 
      this->cbLocoSystemFaultsLCS26->AutoSize = true;
      this->cbLocoSystemFaultsLCS26->Location = System::Drawing::Point(296, 65);
      this->cbLocoSystemFaultsLCS26->Name = L"cbLocoSystemFaultsLCS26";
      this->cbLocoSystemFaultsLCS26->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS26->TabIndex = 105;
      this->cbLocoSystemFaultsLCS26->Text = L"Fault Bit 26";
      this->cbLocoSystemFaultsLCS26->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS25
      // 
      this->cbLocoSystemFaultsLCS25->AutoSize = true;
      this->cbLocoSystemFaultsLCS25->Location = System::Drawing::Point(296, 42);
      this->cbLocoSystemFaultsLCS25->Name = L"cbLocoSystemFaultsLCS25";
      this->cbLocoSystemFaultsLCS25->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS25->TabIndex = 104;
      this->cbLocoSystemFaultsLCS25->Text = L"Fault Bit 25";
      this->cbLocoSystemFaultsLCS25->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS24
      // 
      this->cbLocoSystemFaultsLCS24->AutoSize = true;
      this->cbLocoSystemFaultsLCS24->Location = System::Drawing::Point(296, 19);
      this->cbLocoSystemFaultsLCS24->Name = L"cbLocoSystemFaultsLCS24";
      this->cbLocoSystemFaultsLCS24->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS24->TabIndex = 103;
      this->cbLocoSystemFaultsLCS24->Text = L"Fault Bit 24";
      this->cbLocoSystemFaultsLCS24->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS23
      // 
      this->cbLocoSystemFaultsLCS23->AutoSize = true;
      this->cbLocoSystemFaultsLCS23->Location = System::Drawing::Point(204, 180);
      this->cbLocoSystemFaultsLCS23->Name = L"cbLocoSystemFaultsLCS23";
      this->cbLocoSystemFaultsLCS23->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS23->TabIndex = 102;
      this->cbLocoSystemFaultsLCS23->Text = L"Fault Bit 23";
      this->cbLocoSystemFaultsLCS23->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS22
      // 
      this->cbLocoSystemFaultsLCS22->AutoSize = true;
      this->cbLocoSystemFaultsLCS22->Location = System::Drawing::Point(204, 157);
      this->cbLocoSystemFaultsLCS22->Name = L"cbLocoSystemFaultsLCS22";
      this->cbLocoSystemFaultsLCS22->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS22->TabIndex = 101;
      this->cbLocoSystemFaultsLCS22->Text = L"Fault Bit 22";
      this->cbLocoSystemFaultsLCS22->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS21
      // 
      this->cbLocoSystemFaultsLCS21->AutoSize = true;
      this->cbLocoSystemFaultsLCS21->Location = System::Drawing::Point(204, 134);
      this->cbLocoSystemFaultsLCS21->Name = L"cbLocoSystemFaultsLCS21";
      this->cbLocoSystemFaultsLCS21->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS21->TabIndex = 100;
      this->cbLocoSystemFaultsLCS21->Text = L"Fault Bit 21";
      this->cbLocoSystemFaultsLCS21->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS20
      // 
      this->cbLocoSystemFaultsLCS20->AutoSize = true;
      this->cbLocoSystemFaultsLCS20->Location = System::Drawing::Point(204, 111);
      this->cbLocoSystemFaultsLCS20->Name = L"cbLocoSystemFaultsLCS20";
      this->cbLocoSystemFaultsLCS20->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS20->TabIndex = 99;
      this->cbLocoSystemFaultsLCS20->Text = L"Fault Bit 20";
      this->cbLocoSystemFaultsLCS20->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS19
      // 
      this->cbLocoSystemFaultsLCS19->AutoSize = true;
      this->cbLocoSystemFaultsLCS19->Location = System::Drawing::Point(204, 88);
      this->cbLocoSystemFaultsLCS19->Name = L"cbLocoSystemFaultsLCS19";
      this->cbLocoSystemFaultsLCS19->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS19->TabIndex = 98;
      this->cbLocoSystemFaultsLCS19->Text = L"Fault Bit 19";
      this->cbLocoSystemFaultsLCS19->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS18
      // 
      this->cbLocoSystemFaultsLCS18->AutoSize = true;
      this->cbLocoSystemFaultsLCS18->Location = System::Drawing::Point(204, 65);
      this->cbLocoSystemFaultsLCS18->Name = L"cbLocoSystemFaultsLCS18";
      this->cbLocoSystemFaultsLCS18->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS18->TabIndex = 97;
      this->cbLocoSystemFaultsLCS18->Text = L"Fault Bit 18";
      this->cbLocoSystemFaultsLCS18->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS17
      // 
      this->cbLocoSystemFaultsLCS17->AutoSize = true;
      this->cbLocoSystemFaultsLCS17->Location = System::Drawing::Point(204, 42);
      this->cbLocoSystemFaultsLCS17->Name = L"cbLocoSystemFaultsLCS17";
      this->cbLocoSystemFaultsLCS17->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS17->TabIndex = 96;
      this->cbLocoSystemFaultsLCS17->Text = L"Fault Bit 17";
      this->cbLocoSystemFaultsLCS17->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS16
      // 
      this->cbLocoSystemFaultsLCS16->AutoSize = true;
      this->cbLocoSystemFaultsLCS16->Location = System::Drawing::Point(204, 19);
      this->cbLocoSystemFaultsLCS16->Name = L"cbLocoSystemFaultsLCS16";
      this->cbLocoSystemFaultsLCS16->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS16->TabIndex = 95;
      this->cbLocoSystemFaultsLCS16->Text = L"Fault Bit 16";
      this->cbLocoSystemFaultsLCS16->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS15
      // 
      this->cbLocoSystemFaultsLCS15->AutoSize = true;
      this->cbLocoSystemFaultsLCS15->Location = System::Drawing::Point(112, 180);
      this->cbLocoSystemFaultsLCS15->Name = L"cbLocoSystemFaultsLCS15";
      this->cbLocoSystemFaultsLCS15->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS15->TabIndex = 94;
      this->cbLocoSystemFaultsLCS15->Text = L"Fault Bit 15";
      this->cbLocoSystemFaultsLCS15->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS14
      // 
      this->cbLocoSystemFaultsLCS14->AutoSize = true;
      this->cbLocoSystemFaultsLCS14->Location = System::Drawing::Point(112, 157);
      this->cbLocoSystemFaultsLCS14->Name = L"cbLocoSystemFaultsLCS14";
      this->cbLocoSystemFaultsLCS14->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS14->TabIndex = 93;
      this->cbLocoSystemFaultsLCS14->Text = L"Fault Bit 14";
      this->cbLocoSystemFaultsLCS14->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS13
      // 
      this->cbLocoSystemFaultsLCS13->AutoSize = true;
      this->cbLocoSystemFaultsLCS13->Location = System::Drawing::Point(112, 134);
      this->cbLocoSystemFaultsLCS13->Name = L"cbLocoSystemFaultsLCS13";
      this->cbLocoSystemFaultsLCS13->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS13->TabIndex = 92;
      this->cbLocoSystemFaultsLCS13->Text = L"Fault Bit 13";
      this->cbLocoSystemFaultsLCS13->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS12
      // 
      this->cbLocoSystemFaultsLCS12->AutoSize = true;
      this->cbLocoSystemFaultsLCS12->Location = System::Drawing::Point(112, 111);
      this->cbLocoSystemFaultsLCS12->Name = L"cbLocoSystemFaultsLCS12";
      this->cbLocoSystemFaultsLCS12->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS12->TabIndex = 91;
      this->cbLocoSystemFaultsLCS12->Text = L"Fault Bit 12";
      this->cbLocoSystemFaultsLCS12->UseVisualStyleBackColor = true;
      this->cbLocoSystemFaultsLCS12->CheckedChanged += gcnew System::EventHandler(this, &LCSSimForm::checkBox12_CheckedChanged);
      // 
      // cbLocoSystemFaultsLCS11
      // 
      this->cbLocoSystemFaultsLCS11->AutoSize = true;
      this->cbLocoSystemFaultsLCS11->Location = System::Drawing::Point(112, 88);
      this->cbLocoSystemFaultsLCS11->Name = L"cbLocoSystemFaultsLCS11";
      this->cbLocoSystemFaultsLCS11->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS11->TabIndex = 90;
      this->cbLocoSystemFaultsLCS11->Text = L"Fault Bit 11";
      this->cbLocoSystemFaultsLCS11->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS10
      // 
      this->cbLocoSystemFaultsLCS10->AutoSize = true;
      this->cbLocoSystemFaultsLCS10->Location = System::Drawing::Point(112, 65);
      this->cbLocoSystemFaultsLCS10->Name = L"cbLocoSystemFaultsLCS10";
      this->cbLocoSystemFaultsLCS10->Size = System::Drawing::Size(79, 17);
      this->cbLocoSystemFaultsLCS10->TabIndex = 89;
      this->cbLocoSystemFaultsLCS10->Text = L"Fault Bit 10";
      this->cbLocoSystemFaultsLCS10->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS9
      // 
      this->cbLocoSystemFaultsLCS9->AutoSize = true;
      this->cbLocoSystemFaultsLCS9->Location = System::Drawing::Point(112, 42);
      this->cbLocoSystemFaultsLCS9->Name = L"cbLocoSystemFaultsLCS9";
      this->cbLocoSystemFaultsLCS9->Size = System::Drawing::Size(73, 17);
      this->cbLocoSystemFaultsLCS9->TabIndex = 88;
      this->cbLocoSystemFaultsLCS9->Text = L"Fault Bit 9";
      this->cbLocoSystemFaultsLCS9->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS8
      // 
      this->cbLocoSystemFaultsLCS8->AutoSize = true;
      this->cbLocoSystemFaultsLCS8->Location = System::Drawing::Point(112, 19);
      this->cbLocoSystemFaultsLCS8->Name = L"cbLocoSystemFaultsLCS8";
      this->cbLocoSystemFaultsLCS8->Size = System::Drawing::Size(73, 17);
      this->cbLocoSystemFaultsLCS8->TabIndex = 87;
      this->cbLocoSystemFaultsLCS8->Text = L"Fault Bit 8";
      this->cbLocoSystemFaultsLCS8->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS7
      // 
      this->cbLocoSystemFaultsLCS7->AutoSize = true;
      this->cbLocoSystemFaultsLCS7->Location = System::Drawing::Point(26, 180);
      this->cbLocoSystemFaultsLCS7->Name = L"cbLocoSystemFaultsLCS7";
      this->cbLocoSystemFaultsLCS7->Size = System::Drawing::Size(73, 17);
      this->cbLocoSystemFaultsLCS7->TabIndex = 86;
      this->cbLocoSystemFaultsLCS7->Text = L"Fault Bit 7";
      this->cbLocoSystemFaultsLCS7->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS6
      // 
      this->cbLocoSystemFaultsLCS6->AutoSize = true;
      this->cbLocoSystemFaultsLCS6->Location = System::Drawing::Point(26, 157);
      this->cbLocoSystemFaultsLCS6->Name = L"cbLocoSystemFaultsLCS6";
      this->cbLocoSystemFaultsLCS6->Size = System::Drawing::Size(73, 17);
      this->cbLocoSystemFaultsLCS6->TabIndex = 85;
      this->cbLocoSystemFaultsLCS6->Text = L"Fault Bit 6";
      this->cbLocoSystemFaultsLCS6->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS5
      // 
      this->cbLocoSystemFaultsLCS5->AutoSize = true;
      this->cbLocoSystemFaultsLCS5->Location = System::Drawing::Point(26, 134);
      this->cbLocoSystemFaultsLCS5->Name = L"cbLocoSystemFaultsLCS5";
      this->cbLocoSystemFaultsLCS5->Size = System::Drawing::Size(73, 17);
      this->cbLocoSystemFaultsLCS5->TabIndex = 84;
      this->cbLocoSystemFaultsLCS5->Text = L"Fault Bit 5";
      this->cbLocoSystemFaultsLCS5->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS4
      // 
      this->cbLocoSystemFaultsLCS4->AutoSize = true;
      this->cbLocoSystemFaultsLCS4->Location = System::Drawing::Point(26, 111);
      this->cbLocoSystemFaultsLCS4->Name = L"cbLocoSystemFaultsLCS4";
      this->cbLocoSystemFaultsLCS4->Size = System::Drawing::Size(73, 17);
      this->cbLocoSystemFaultsLCS4->TabIndex = 83;
      this->cbLocoSystemFaultsLCS4->Text = L"Fault Bit 4";
      this->cbLocoSystemFaultsLCS4->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS3
      // 
      this->cbLocoSystemFaultsLCS3->AutoSize = true;
      this->cbLocoSystemFaultsLCS3->Location = System::Drawing::Point(26, 88);
      this->cbLocoSystemFaultsLCS3->Name = L"cbLocoSystemFaultsLCS3";
      this->cbLocoSystemFaultsLCS3->Size = System::Drawing::Size(73, 17);
      this->cbLocoSystemFaultsLCS3->TabIndex = 82;
      this->cbLocoSystemFaultsLCS3->Text = L"Fault Bit 3";
      this->cbLocoSystemFaultsLCS3->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS2
      // 
      this->cbLocoSystemFaultsLCS2->AutoSize = true;
      this->cbLocoSystemFaultsLCS2->Location = System::Drawing::Point(26, 65);
      this->cbLocoSystemFaultsLCS2->Name = L"cbLocoSystemFaultsLCS2";
      this->cbLocoSystemFaultsLCS2->Size = System::Drawing::Size(73, 17);
      this->cbLocoSystemFaultsLCS2->TabIndex = 81;
      this->cbLocoSystemFaultsLCS2->Text = L"Fault Bit 2";
      this->cbLocoSystemFaultsLCS2->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS1
      // 
      this->cbLocoSystemFaultsLCS1->AutoSize = true;
      this->cbLocoSystemFaultsLCS1->Location = System::Drawing::Point(26, 42);
      this->cbLocoSystemFaultsLCS1->Name = L"cbLocoSystemFaultsLCS1";
      this->cbLocoSystemFaultsLCS1->Size = System::Drawing::Size(73, 17);
      this->cbLocoSystemFaultsLCS1->TabIndex = 80;
      this->cbLocoSystemFaultsLCS1->Text = L"Fault Bit 1";
      this->cbLocoSystemFaultsLCS1->UseVisualStyleBackColor = true;
      // 
      // cbLocoSystemFaultsLCS0
      // 
      this->cbLocoSystemFaultsLCS0->AutoSize = true;
      this->cbLocoSystemFaultsLCS0->Location = System::Drawing::Point(26, 19);
      this->cbLocoSystemFaultsLCS0->Name = L"cbLocoSystemFaultsLCS0";
      this->cbLocoSystemFaultsLCS0->Size = System::Drawing::Size(73, 17);
      this->cbLocoSystemFaultsLCS0->TabIndex = 79;
      this->cbLocoSystemFaultsLCS0->Text = L"Fault Bit 0";
      this->cbLocoSystemFaultsLCS0->UseVisualStyleBackColor = true;
      // 
      // tpTrainCompECPB
      // 
      this->tpTrainCompECPB->BackColor = System::Drawing::SystemColors::ControlLightLight;
      this->tpTrainCompECPB->Controls->Add(this->buttonAppend);
      this->tpTrainCompECPB->Controls->Add(this->buttonImport);
      this->tpTrainCompECPB->Controls->Add(this->bApplyTrainComp);
      this->tpTrainCompECPB->Controls->Add(this->bSaveTrainComp);
      this->tpTrainCompECPB->Controls->Add(this->groupBox14);
      this->tpTrainCompECPB->Controls->Add(this->dgTrainCompECPB);
      this->tpTrainCompECPB->Location = System::Drawing::Point(4, 22);
      this->tpTrainCompECPB->Name = L"tpTrainCompECPB";
      this->tpTrainCompECPB->Size = System::Drawing::Size(425, 464);
      this->tpTrainCompECPB->TabIndex = 3;
      this->tpTrainCompECPB->Text = L"TrainCompECPB";
      // 
      // buttonAppend
      // 
      this->buttonAppend->Location = System::Drawing::Point(84, 433);
      this->buttonAppend->Name = L"buttonAppend";
      this->buttonAppend->Size = System::Drawing::Size(75, 23);
      this->buttonAppend->TabIndex = 93;
      this->buttonAppend->Text = L"Append";
      this->buttonAppend->UseVisualStyleBackColor = true;
      this->buttonAppend->Click += gcnew System::EventHandler(this, &LCSSimForm::buttonAppend_Click);
      // 
      // buttonImport
      // 
      this->buttonImport->Location = System::Drawing::Point(3, 433);
      this->buttonImport->Name = L"buttonImport";
      this->buttonImport->Size = System::Drawing::Size(75, 23);
      this->buttonImport->TabIndex = 92;
      this->buttonImport->Text = L"Import";
      this->buttonImport->UseVisualStyleBackColor = true;
      this->buttonImport->Click += gcnew System::EventHandler(this, &LCSSimForm::buttonImport_Click);
      // 
      // bApplyTrainComp
      // 
      this->bApplyTrainComp->Location = System::Drawing::Point(343, 433);
      this->bApplyTrainComp->Name = L"bApplyTrainComp";
      this->bApplyTrainComp->Size = System::Drawing::Size(75, 23);
      this->bApplyTrainComp->TabIndex = 95;
      this->bApplyTrainComp->Text = L"Apply";
      this->bApplyTrainComp->UseVisualStyleBackColor = true;
      this->bApplyTrainComp->Click += gcnew System::EventHandler(this, &LCSSimForm::trainCompFromGUIUpdate);
      // 
      // bSaveTrainComp
      // 
      this->bSaveTrainComp->Location = System::Drawing::Point(262, 433);
      this->bSaveTrainComp->Name = L"bSaveTrainComp";
      this->bSaveTrainComp->Size = System::Drawing::Size(75, 23);
      this->bSaveTrainComp->TabIndex = 94;
      this->bSaveTrainComp->Text = L"Save";
      this->bSaveTrainComp->UseVisualStyleBackColor = true;
      this->bSaveTrainComp->Click += gcnew System::EventHandler(this, &LCSSimForm::bSaveTrainComp_Click);
      // 
      // groupBox14
      // 
      this->groupBox14->BackColor = System::Drawing::SystemColors::ControlLightLight;
      this->groupBox14->Controls->Add(this->tbRollingStockVehECP);
      this->groupBox14->Controls->Add(this->tbVehNotDetected);
      this->groupBox14->Controls->Add(this->label57);
      this->groupBox14->Controls->Add(this->tbVehDetectedPosUnknown);
      this->groupBox14->Controls->Add(this->label58);
      this->groupBox14->Controls->Add(this->label59);
      this->groupBox14->Location = System::Drawing::Point(3, 3);
      this->groupBox14->Name = L"groupBox14";
      this->groupBox14->Size = System::Drawing::Size(415, 82);
      this->groupBox14->TabIndex = 22;
      this->groupBox14->TabStop = false;
      this->groupBox14->Text = L" Vehicles";
      // 
      // tbRollingStockVehECP
      // 
      this->tbRollingStockVehECP->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbRollingStockVehECP->Enabled = false;
      this->tbRollingStockVehECP->Location = System::Drawing::Point(102, 16);
      this->tbRollingStockVehECP->Name = L"tbRollingStockVehECP";
      this->tbRollingStockVehECP->Size = System::Drawing::Size(63, 20);
      this->tbRollingStockVehECP->TabIndex = 16;
      this->tbRollingStockVehECP->TabStop = false;
      this->tbRollingStockVehECP->Text = L"200";
      this->tbRollingStockVehECP->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // tbVehNotDetected
      // 
      this->tbVehNotDetected->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbVehNotDetected->Location = System::Drawing::Point(105, 55);
      this->tbVehNotDetected->Name = L"tbVehNotDetected";
      this->tbVehNotDetected->Size = System::Drawing::Size(63, 20);
      this->tbVehNotDetected->TabIndex = 14;
      this->tbVehNotDetected->TabStop = false;
      this->tbVehNotDetected->Text = L"0";
      this->tbVehNotDetected->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbVehNotDetected->TextChanged += gcnew System::EventHandler(this, &LCSSimForm::tbVehNotDetected_TextChanged);
      // 
      // label57
      // 
      this->label57->AutoSize = true;
      this->label57->Location = System::Drawing::Point(6, 49);
      this->label57->Name = L"label57";
      this->label57->Size = System::Drawing::Size(93, 26);
      this->label57->TabIndex = 15;
      this->label57->Text = L"Not \r\nDetected By ECP:";
      // 
      // tbVehDetectedPosUnknown
      // 
      this->tbVehDetectedPosUnknown->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbVehDetectedPosUnknown->Location = System::Drawing::Point(289, 19);
      this->tbVehDetectedPosUnknown->Name = L"tbVehDetectedPosUnknown";
      this->tbVehDetectedPosUnknown->Size = System::Drawing::Size(63, 20);
      this->tbVehDetectedPosUnknown->TabIndex = 12;
      this->tbVehDetectedPosUnknown->TabStop = false;
      this->tbVehDetectedPosUnknown->Text = L"0";
      this->tbVehDetectedPosUnknown->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbVehDetectedPosUnknown->TextChanged += gcnew System::EventHandler(this, &LCSSimForm::tbVehDetectedPosUnknown_TextChanged);
      // 
      // label58
      // 
      this->label58->AutoSize = true;
      this->label58->Location = System::Drawing::Point(186, 16);
      this->label58->Name = L"label58";
      this->label58->Size = System::Drawing::Size(96, 26);
      this->label58->TabIndex = 13;
      this->label58->Text = L"Detected by ECP,\r\nPosition Unknown:";
      // 
      // label59
      // 
      this->label59->AutoSize = true;
      this->label59->Location = System::Drawing::Point(4, 19);
      this->label59->Name = L"label59";
      this->label59->Size = System::Drawing::Size(92, 13);
      this->label59->TabIndex = 11;
      this->label59->Text = L"Detected by ECP:";
      // 
      // dgTrainCompECPB
      // 
      this->dgTrainCompECPB->AllowUserToResizeRows = false;
      this->dgTrainCompECPB->BackgroundColor = System::Drawing::SystemColors::ControlLightLight;
      this->dgTrainCompECPB->Columns->AddRange(gcnew cli::array< System::Windows::Forms::DataGridViewColumn^  >(3) {
        this->No, this->Column1,
          this->Column3
      });
      this->dgTrainCompECPB->GridColor = System::Drawing::SystemColors::AppWorkspace;
      this->dgTrainCompECPB->Location = System::Drawing::Point(3, 91);
      this->dgTrainCompECPB->Name = L"dgTrainCompECPB";
      this->dgTrainCompECPB->Size = System::Drawing::Size(415, 336);
      this->dgTrainCompECPB->TabIndex = 91;
      this->dgTrainCompECPB->CellContentClick += gcnew System::Windows::Forms::DataGridViewCellEventHandler(this, &LCSSimForm::dgTrainCompECPB_CellContentClick);
      this->dgTrainCompECPB->CellValueChanged += gcnew System::Windows::Forms::DataGridViewCellEventHandler(this, &LCSSimForm::dgTrainCompECPB_CellContentClick);
      this->dgTrainCompECPB->RowsAdded += gcnew System::Windows::Forms::DataGridViewRowsAddedEventHandler(this, &LCSSimForm::dgTrainCompRowAdd);
      this->dgTrainCompECPB->RowsRemoved += gcnew System::Windows::Forms::DataGridViewRowsRemovedEventHandler(this, &LCSSimForm::dgTrainCompRowDel);
      // 
      // No
      // 
      this->No->HeaderText = L"No.";
      this->No->Name = L"No";
      this->No->ReadOnly = true;
      // 
      // Column1
      // 
      this->Column1->HeaderText = L"RoadNumber";
      this->Column1->Name = L"Column1";
      // 
      // Column3
      // 
      this->Column3->DisplayStyle = System::Windows::Forms::DataGridViewComboBoxDisplayStyle::ComboBox;
      this->Column3->HeaderText = L"VehicleType";
      this->Column3->Items->AddRange(gcnew cli::array< System::Object^  >(3) { L"Unknown", L"Locomotive", L"Car" });
      this->Column3->MaxDropDownItems = 3;
      this->Column3->Name = L"Column3";
      // 
      // tpTrainCompAOS
      // 
      this->tpTrainCompAOS->Controls->Add(this->tbRollingStockVeh);
      this->tpTrainCompAOS->Controls->Add(this->label53);
      this->tpTrainCompAOS->Controls->Add(this->lvTrainCompAOS);
      this->tpTrainCompAOS->Location = System::Drawing::Point(4, 22);
      this->tpTrainCompAOS->Name = L"tpTrainCompAOS";
      this->tpTrainCompAOS->Size = System::Drawing::Size(425, 464);
      this->tpTrainCompAOS->TabIndex = 2;
      this->tpTrainCompAOS->Text = L"TrainCompAOS";
      // 
      // tbRollingStockVeh
      // 
      this->tbRollingStockVeh->BackColor = System::Drawing::SystemColors::HighlightText;
      this->tbRollingStockVeh->Enabled = false;
      this->tbRollingStockVeh->Location = System::Drawing::Point(161, 10);
      this->tbRollingStockVeh->Name = L"tbRollingStockVeh";
      this->tbRollingStockVeh->Size = System::Drawing::Size(100, 20);
      this->tbRollingStockVeh->TabIndex = 19;
      this->tbRollingStockVeh->TabStop = false;
      // 
      // label53
      // 
      this->label53->AutoSize = true;
      this->label53->Location = System::Drawing::Point(3, 13);
      this->label53->Name = L"label53";
      this->label53->Size = System::Drawing::Size(154, 13);
      this->label53->TabIndex = 17;
      this->label53->Text = L"Rolling Stock Vehicles in Train:";
      // 
      // lvTrainCompAOS
      // 
      this->lvTrainCompAOS->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(3) {
        this->columnHeader7,
          this->columnHeader8, this->columnHeader9
      });
      this->lvTrainCompAOS->GridLines = true;
      this->lvTrainCompAOS->Items->AddRange(gcnew cli::array< System::Windows::Forms::ListViewItem^  >(1) { listViewItem5 });
      this->lvTrainCompAOS->Location = System::Drawing::Point(3, 38);
      this->lvTrainCompAOS->Name = L"lvTrainCompAOS";
      this->lvTrainCompAOS->Size = System::Drawing::Size(419, 423);
      this->lvTrainCompAOS->TabIndex = 2;
      this->lvTrainCompAOS->UseCompatibleStateImageBehavior = false;
      this->lvTrainCompAOS->View = System::Windows::Forms::View::Details;
      // 
      // columnHeader7
      // 
      this->columnHeader7->Text = L"No.";
      this->columnHeader7->Width = 32;
      // 
      // columnHeader8
      // 
      this->columnHeader8->Text = L"RoadNumber";
      this->columnHeader8->Width = 160;
      // 
      // columnHeader9
      // 
      this->columnHeader9->Text = L"VehicleType";
      this->columnHeader9->Width = 222;
      // 
      // tpAOSStatus
      // 
      this->tpAOSStatus->Controls->Add(this->cbEnableSimulation);
      this->tpAOSStatus->Controls->Add(this->cbEnableComLinks);
      this->tpAOSStatus->Controls->Add(this->groupBox2);
      this->tpAOSStatus->Controls->Add(this->groupBox1);
      this->tpAOSStatus->Controls->Add(this->groupBoxExample);
      this->tpAOSStatus->Location = System::Drawing::Point(4, 22);
      this->tpAOSStatus->Name = L"tpAOSStatus";
      this->tpAOSStatus->Padding = System::Windows::Forms::Padding(3);
      this->tpAOSStatus->Size = System::Drawing::Size(425, 464);
      this->tpAOSStatus->TabIndex = 0;
      this->tpAOSStatus->Text = L"AOSStatus";
      this->tpAOSStatus->UseVisualStyleBackColor = true;
      this->tpAOSStatus->Click += gcnew System::EventHandler(this, &LCSSimForm::tbOverview_Click);
      // 
      // cbEnableSimulation
      // 
      this->cbEnableSimulation->AutoSize = true;
      this->cbEnableSimulation->Checked = true;
      this->cbEnableSimulation->CheckState = System::Windows::Forms::CheckState::Checked;
      this->cbEnableSimulation->Location = System::Drawing::Point(12, 362);
      this->cbEnableSimulation->Name = L"cbEnableSimulation";
      this->cbEnableSimulation->Size = System::Drawing::Size(157, 17);
      this->cbEnableSimulation->TabIndex = 9;
      this->cbEnableSimulation->Text = L"Enable Simulation of Values";
      this->cbEnableSimulation->UseVisualStyleBackColor = true;
      this->cbEnableSimulation->CheckedChanged += gcnew System::EventHandler(this, &LCSSimForm::checkBox1_CheckedChanged);
      // 
      // cbEnableComLinks
      // 
      this->cbEnableComLinks->AutoSize = true;
      this->cbEnableComLinks->Location = System::Drawing::Point(12, 338);
      this->cbEnableComLinks->Name = L"cbEnableComLinks";
      this->cbEnableComLinks->Size = System::Drawing::Size(111, 17);
      this->cbEnableComLinks->TabIndex = 8;
      this->cbEnableComLinks->Text = L"Enable Com Links";
      this->cbEnableComLinks->UseVisualStyleBackColor = true;
      // 
      // groupBox2
      // 
      this->groupBox2->Controls->Add(this->label19);
      this->groupBox2->Controls->Add(this->tbTotalWeight);
      this->groupBox2->Controls->Add(this->label15);
      this->groupBox2->Controls->Add(this->tbHoldingBrake);
      this->groupBox2->Controls->Add(this->label10);
      this->groupBox2->Controls->Add(this->tbECPBTrainCompReq);
      this->groupBox2->Controls->Add(this->label7);
      this->groupBox2->Location = System::Drawing::Point(6, 238);
      this->groupBox2->Name = L"groupBox2";
      this->groupBox2->Size = System::Drawing::Size(413, 94);
      this->groupBox2->TabIndex = 3;
      this->groupBox2->TabStop = false;
      this->groupBox2->Text = L"ATP Command";
      // 
      // label19
      // 
      this->label19->AutoSize = true;
      this->label19->Location = System::Drawing::Point(385, 30);
      this->label19->Name = L"label19";
      this->label19->Size = System::Drawing::Size(27, 13);
      this->label19->TabIndex = 10;
      this->label19->Text = L"tons";
      // 
      // tbTotalWeight
      // 
      this->tbTotalWeight->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbTotalWeight->Enabled = false;
      this->tbTotalWeight->Location = System::Drawing::Point(299, 27);
      this->tbTotalWeight->Name = L"tbTotalWeight";
      this->tbTotalWeight->Size = System::Drawing::Size(80, 20);
      this->tbTotalWeight->TabIndex = 6;
      this->tbTotalWeight->TabStop = false;
      this->tbTotalWeight->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label15
      // 
      this->label15->AutoSize = true;
      this->label15->Location = System::Drawing::Point(228, 31);
      this->label15->Name = L"label15";
      this->label15->Size = System::Drawing::Size(71, 13);
      this->label15->TabIndex = 7;
      this->label15->Text = L"Total Weight:";
      // 
      // tbHoldingBrake
      // 
      this->tbHoldingBrake->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbHoldingBrake->Enabled = false;
      this->tbHoldingBrake->Location = System::Drawing::Point(125, 55);
      this->tbHoldingBrake->Name = L"tbHoldingBrake";
      this->tbHoldingBrake->Size = System::Drawing::Size(80, 20);
      this->tbHoldingBrake->TabIndex = 4;
      this->tbHoldingBrake->TabStop = false;
      this->tbHoldingBrake->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label10
      // 
      this->label10->AutoSize = true;
      this->label10->Location = System::Drawing::Point(13, 58);
      this->label10->Name = L"label10";
      this->label10->Size = System::Drawing::Size(77, 13);
      this->label10->TabIndex = 5;
      this->label10->Text = L"Holding Brake:";
      // 
      // tbECPBTrainCompReq
      // 
      this->tbECPBTrainCompReq->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbECPBTrainCompReq->Enabled = false;
      this->tbECPBTrainCompReq->Location = System::Drawing::Point(125, 24);
      this->tbECPBTrainCompReq->Name = L"tbECPBTrainCompReq";
      this->tbECPBTrainCompReq->Size = System::Drawing::Size(80, 20);
      this->tbECPBTrainCompReq->TabIndex = 2;
      this->tbECPBTrainCompReq->TabStop = false;
      this->tbECPBTrainCompReq->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbECPBTrainCompReq->TextChanged += gcnew System::EventHandler(this, &LCSSimForm::tbECPBTrainCompReq_TextChanged);
      // 
      // label7
      // 
      this->label7->AutoSize = true;
      this->label7->Location = System::Drawing::Point(13, 27);
      this->label7->Name = L"label7";
      this->label7->Size = System::Drawing::Size(115, 13);
      this->label7->TabIndex = 3;
      this->label7->Text = L"ECPB TrainComp Req:";
      // 
      // groupBox1
      // 
      this->groupBox1->Controls->Add(this->label18);
      this->groupBox1->Controls->Add(this->label17);
      this->groupBox1->Controls->Add(this->label6);
      this->groupBox1->Controls->Add(this->tbTrainOrientationFront);
      this->groupBox1->Controls->Add(this->label5);
      this->groupBox1->Controls->Add(this->tbTrainOrientationRear);
      this->groupBox1->Controls->Add(this->label4);
      this->groupBox1->Controls->Add(this->tbPositionOnTrackFront);
      this->groupBox1->Controls->Add(this->tbPositionOnTrackRear);
      this->groupBox1->Controls->Add(this->label3);
      this->groupBox1->Controls->Add(this->tbTrackIdFront);
      this->groupBox1->Controls->Add(this->tbTrackIdRear);
      this->groupBox1->Controls->Add(this->label2);
      this->groupBox1->Location = System::Drawing::Point(12, 104);
      this->groupBox1->Name = L"groupBox1";
      this->groupBox1->Size = System::Drawing::Size(401, 122);
      this->groupBox1->TabIndex = 2;
      this->groupBox1->TabStop = false;
      this->groupBox1->Text = L"Position Report";
      this->groupBox1->Enter += gcnew System::EventHandler(this, &LCSSimForm::groupBox1_Enter_1);
      // 
      // label18
      // 
      this->label18->AutoSize = true;
      this->label18->Location = System::Drawing::Point(379, 61);
      this->label18->Name = L"label18";
      this->label18->Size = System::Drawing::Size(21, 13);
      this->label18->TabIndex = 16;
      this->label18->Text = L"cm";
      // 
      // label17
      // 
      this->label17->AutoSize = true;
      this->label17->Location = System::Drawing::Point(205, 58);
      this->label17->Name = L"label17";
      this->label17->Size = System::Drawing::Size(21, 13);
      this->label17->TabIndex = 9;
      this->label17->Text = L"cm";
      this->label17->Click += gcnew System::EventHandler(this, &LCSSimForm::label17_Click);
      // 
      // label6
      // 
      this->label6->AutoSize = true;
      this->label6->Location = System::Drawing::Point(316, 12);
      this->label6->Name = L"label6";
      this->label6->Size = System::Drawing::Size(31, 13);
      this->label6->TabIndex = 4;
      this->label6->Text = L"Front";
      this->label6->Click += gcnew System::EventHandler(this, &LCSSimForm::label6_Click);
      // 
      // tbTrainOrientationFront
      // 
      this->tbTrainOrientationFront->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbTrainOrientationFront->Enabled = false;
      this->tbTrainOrientationFront->Location = System::Drawing::Point(293, 85);
      this->tbTrainOrientationFront->Name = L"tbTrainOrientationFront";
      this->tbTrainOrientationFront->Size = System::Drawing::Size(80, 20);
      this->tbTrainOrientationFront->TabIndex = 14;
      this->tbTrainOrientationFront->TabStop = false;
      this->tbTrainOrientationFront->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label5
      // 
      this->label5->AutoSize = true;
      this->label5->Location = System::Drawing::Point(133, 12);
      this->label5->Name = L"label5";
      this->label5->Size = System::Drawing::Size(30, 13);
      this->label5->TabIndex = 3;
      this->label5->Text = L"Rear";
      // 
      // tbTrainOrientationRear
      // 
      this->tbTrainOrientationRear->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbTrainOrientationRear->Enabled = false;
      this->tbTrainOrientationRear->Location = System::Drawing::Point(119, 82);
      this->tbTrainOrientationRear->Name = L"tbTrainOrientationRear";
      this->tbTrainOrientationRear->Size = System::Drawing::Size(80, 20);
      this->tbTrainOrientationRear->TabIndex = 14;
      this->tbTrainOrientationRear->TabStop = false;
      this->tbTrainOrientationRear->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label4
      // 
      this->label4->AutoSize = true;
      this->label4->Location = System::Drawing::Point(7, 85);
      this->label4->Name = L"label4";
      this->label4->Size = System::Drawing::Size(86, 13);
      this->label4->TabIndex = 15;
      this->label4->Text = L"Train orientation:";
      // 
      // tbPositionOnTrackFront
      // 
      this->tbPositionOnTrackFront->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbPositionOnTrackFront->Enabled = false;
      this->tbPositionOnTrackFront->Location = System::Drawing::Point(293, 58);
      this->tbPositionOnTrackFront->Name = L"tbPositionOnTrackFront";
      this->tbPositionOnTrackFront->Size = System::Drawing::Size(80, 20);
      this->tbPositionOnTrackFront->TabIndex = 12;
      this->tbPositionOnTrackFront->TabStop = false;
      this->tbPositionOnTrackFront->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // tbPositionOnTrackRear
      // 
      this->tbPositionOnTrackRear->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbPositionOnTrackRear->Enabled = false;
      this->tbPositionOnTrackRear->Location = System::Drawing::Point(119, 55);
      this->tbPositionOnTrackRear->Name = L"tbPositionOnTrackRear";
      this->tbPositionOnTrackRear->Size = System::Drawing::Size(80, 20);
      this->tbPositionOnTrackRear->TabIndex = 12;
      this->tbPositionOnTrackRear->TabStop = false;
      this->tbPositionOnTrackRear->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbPositionOnTrackRear->TextChanged += gcnew System::EventHandler(this, &LCSSimForm::tbPositionOnTrackRear_TextChanged);
      // 
      // label3
      // 
      this->label3->AutoSize = true;
      this->label3->Location = System::Drawing::Point(7, 58);
      this->label3->Name = L"label3";
      this->label3->Size = System::Drawing::Size(89, 13);
      this->label3->TabIndex = 13;
      this->label3->Text = L"Position on track:";
      // 
      // tbTrackIdFront
      // 
      this->tbTrackIdFront->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbTrackIdFront->Enabled = false;
      this->tbTrackIdFront->Location = System::Drawing::Point(293, 28);
      this->tbTrackIdFront->Name = L"tbTrackIdFront";
      this->tbTrackIdFront->Size = System::Drawing::Size(80, 20);
      this->tbTrackIdFront->TabIndex = 10;
      this->tbTrackIdFront->TabStop = false;
      this->tbTrackIdFront->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // tbTrackIdRear
      // 
      this->tbTrackIdRear->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbTrackIdRear->Enabled = false;
      this->tbTrackIdRear->Location = System::Drawing::Point(119, 28);
      this->tbTrackIdRear->Name = L"tbTrackIdRear";
      this->tbTrackIdRear->Size = System::Drawing::Size(80, 20);
      this->tbTrackIdRear->TabIndex = 10;
      this->tbTrackIdRear->TabStop = false;
      this->tbTrackIdRear->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label2
      // 
      this->label2->AutoSize = true;
      this->label2->Location = System::Drawing::Point(7, 31);
      this->label2->Name = L"label2";
      this->label2->Size = System::Drawing::Size(50, 13);
      this->label2->TabIndex = 11;
      this->label2->Text = L"Track Id:";
      // 
      // groupBoxExample
      // 
      this->groupBoxExample->Controls->Add(this->label62);
      this->groupBoxExample->Controls->Add(this->tbLimitedSupMode);
      this->groupBoxExample->Controls->Add(this->label61);
      this->groupBoxExample->Controls->Add(this->tbAOSVehicleSpeed);
      this->groupBoxExample->Controls->Add(this->tbBlueFlag);
      this->groupBoxExample->Controls->Add(this->label9);
      this->groupBoxExample->Controls->Add(this->tbTravelDirection);
      this->groupBoxExample->Controls->Add(this->label8);
      this->groupBoxExample->Controls->Add(this->tbTrainIdling);
      this->groupBoxExample->Controls->Add(this->label1);
      this->groupBoxExample->Controls->Add(this->tbATOCabinSelector);
      this->groupBoxExample->Controls->Add(this->labelExample);
      this->groupBoxExample->Location = System::Drawing::Point(6, 6);
      this->groupBoxExample->Name = L"groupBoxExample";
      this->groupBoxExample->Size = System::Drawing::Size(413, 226);
      this->groupBoxExample->TabIndex = 1;
      this->groupBoxExample->TabStop = false;
      this->groupBoxExample->Text = L"AOS Status";
      this->groupBoxExample->Enter += gcnew System::EventHandler(this, &LCSSimForm::groupBox1_Enter);
      // 
      // label62
      // 
      this->label62->AutoSize = true;
      this->label62->Location = System::Drawing::Point(211, 71);
      this->label62->Name = L"label62";
      this->label62->Size = System::Drawing::Size(95, 13);
      this->label62->TabIndex = 23;
      this->label62->Text = L"Limited Sup Mode:";
      // 
      // tbLimitedSupMode
      // 
      this->tbLimitedSupMode->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbLimitedSupMode->Enabled = false;
      this->tbLimitedSupMode->Location = System::Drawing::Point(313, 67);
      this->tbLimitedSupMode->Name = L"tbLimitedSupMode";
      this->tbLimitedSupMode->Size = System::Drawing::Size(80, 20);
      this->tbLimitedSupMode->TabIndex = 22;
      this->tbLimitedSupMode->TabStop = false;
      this->tbLimitedSupMode->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label61
      // 
      this->label61->AutoSize = true;
      this->label61->Location = System::Drawing::Point(13, 69);
      this->label61->Name = L"label61";
      this->label61->Size = System::Drawing::Size(104, 13);
      this->label61->TabIndex = 21;
      this->label61->Text = L"AOS Vehicle Speed:";
      // 
      // tbAOSVehicleSpeed
      // 
      this->tbAOSVehicleSpeed->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbAOSVehicleSpeed->Enabled = false;
      this->tbAOSVehicleSpeed->Location = System::Drawing::Point(125, 67);
      this->tbAOSVehicleSpeed->Name = L"tbAOSVehicleSpeed";
      this->tbAOSVehicleSpeed->Size = System::Drawing::Size(80, 20);
      this->tbAOSVehicleSpeed->TabIndex = 20;
      this->tbAOSVehicleSpeed->TabStop = false;
      this->tbAOSVehicleSpeed->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // tbBlueFlag
      // 
      this->tbBlueFlag->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbBlueFlag->Enabled = false;
      this->tbBlueFlag->Location = System::Drawing::Point(313, 42);
      this->tbBlueFlag->Name = L"tbBlueFlag";
      this->tbBlueFlag->Size = System::Drawing::Size(80, 20);
      this->tbBlueFlag->TabIndex = 18;
      this->tbBlueFlag->TabStop = false;
      this->tbBlueFlag->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label9
      // 
      this->label9->AutoSize = true;
      this->label9->Location = System::Drawing::Point(211, 44);
      this->label9->Name = L"label9";
      this->label9->Size = System::Drawing::Size(91, 13);
      this->label9->TabIndex = 19;
      this->label9->Text = L"System Blue Flag:";
      this->label9->Click += gcnew System::EventHandler(this, &LCSSimForm::label9_Click_1);
      // 
      // tbTravelDirection
      // 
      this->tbTravelDirection->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbTravelDirection->Enabled = false;
      this->tbTravelDirection->Location = System::Drawing::Point(313, 16);
      this->tbTravelDirection->Name = L"tbTravelDirection";
      this->tbTravelDirection->Size = System::Drawing::Size(80, 20);
      this->tbTravelDirection->TabIndex = 16;
      this->tbTravelDirection->TabStop = false;
      this->tbTravelDirection->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label8
      // 
      this->label8->AutoSize = true;
      this->label8->Location = System::Drawing::Point(210, 18);
      this->label8->Name = L"label8";
      this->label8->Size = System::Drawing::Size(105, 13);
      this->label8->TabIndex = 17;
      this->label8->Text = L"Curr Travel direction:";
      this->label8->Click += gcnew System::EventHandler(this, &LCSSimForm::label8_Click);
      // 
      // tbTrainIdling
      // 
      this->tbTrainIdling->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbTrainIdling->Enabled = false;
      this->tbTrainIdling->Location = System::Drawing::Point(125, 38);
      this->tbTrainIdling->Name = L"tbTrainIdling";
      this->tbTrainIdling->Size = System::Drawing::Size(80, 20);
      this->tbTrainIdling->TabIndex = 2;
      this->tbTrainIdling->TabStop = false;
      this->tbTrainIdling->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label1
      // 
      this->label1->AutoSize = true;
      this->label1->Location = System::Drawing::Point(13, 42);
      this->label1->Name = L"label1";
      this->label1->Size = System::Drawing::Size(62, 13);
      this->label1->TabIndex = 3;
      this->label1->Text = L"Train Idling:";
      this->label1->Click += gcnew System::EventHandler(this, &LCSSimForm::label1_Click);
      // 
      // tbATOCabinSelector
      // 
      this->tbATOCabinSelector->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbATOCabinSelector->Enabled = false;
      this->tbATOCabinSelector->Location = System::Drawing::Point(125, 11);
      this->tbATOCabinSelector->Name = L"tbATOCabinSelector";
      this->tbATOCabinSelector->Size = System::Drawing::Size(80, 20);
      this->tbATOCabinSelector->TabIndex = 1;
      this->tbATOCabinSelector->TabStop = false;
      this->tbATOCabinSelector->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // labelExample
      // 
      this->labelExample->AutoSize = true;
      this->labelExample->Location = System::Drawing::Point(13, 18);
      this->labelExample->Name = L"labelExample";
      this->labelExample->Size = System::Drawing::Size(104, 13);
      this->labelExample->TabIndex = 1;
      this->labelExample->Text = L"ATO Cabin Selector:";
      this->labelExample->Click += gcnew System::EventHandler(this, &LCSSimForm::label9_Click);
      // 
      // tcMainControl
      // 
      this->tcMainControl->Controls->Add(this->tpAOSStatus);
      this->tcMainControl->Controls->Add(this->tpTrainCompAOS);
      this->tcMainControl->Controls->Add(this->tpTrainCompECPB);
      this->tcMainControl->Controls->Add(this->tpMovementAuthority);
      this->tcMainControl->Controls->Add(this->tpWarningCurve);
      this->tcMainControl->Controls->Add(this->tpPath);
      this->tcMainControl->Controls->Add(this->tpLCSFaults);
      this->tcMainControl->Controls->Add(this->tpLCSATO);
      this->tcMainControl->Controls->Add(this->tpLCSBrakes);
      this->tcMainControl->Controls->Add(this->tpDataFromATO);
      this->tcMainControl->Controls->Add(this->tpDataToATO);
      this->tcMainControl->Controls->Add(this->tpDataFromLocoSim);
      this->tcMainControl->Controls->Add(this->tpDataToLocoSim);
      this->tcMainControl->Controls->Add(this->tpDebug);
      this->tcMainControl->Controls->Add(this->RclInfo);
      this->tcMainControl->Location = System::Drawing::Point(12, 12);
      this->tcMainControl->Name = L"tcMainControl";
      this->tcMainControl->SelectedIndex = 0;
      this->tcMainControl->Size = System::Drawing::Size(433, 490);
      this->tcMainControl->TabIndex = 1;
      // 
      // tpMovementAuthority
      // 
      this->tpMovementAuthority->Controls->Add(this->groupBox9);
      this->tpMovementAuthority->Location = System::Drawing::Point(4, 22);
      this->tpMovementAuthority->Name = L"tpMovementAuthority";
      this->tpMovementAuthority->Padding = System::Windows::Forms::Padding(3);
      this->tpMovementAuthority->Size = System::Drawing::Size(425, 464);
      this->tpMovementAuthority->TabIndex = 10;
      this->tpMovementAuthority->Text = L"M.A.";
      this->tpMovementAuthority->UseVisualStyleBackColor = true;
      // 
      // groupBox9
      // 
      this->groupBox9->Controls->Add(this->label84);
      this->groupBox9->Controls->Add(this->label60);
      this->groupBox9->Controls->Add(this->tbMAMargin);
      this->groupBox9->Controls->Add(this->label42);
      this->groupBox9->Controls->Add(this->tbEndOfMAPos);
      this->groupBox9->Controls->Add(this->label44);
      this->groupBox9->Controls->Add(this->tbDirection);
      this->groupBox9->Controls->Add(this->label45);
      this->groupBox9->Controls->Add(this->tbEndOfMATrkId);
      this->groupBox9->Controls->Add(this->label46);
      this->groupBox9->Location = System::Drawing::Point(3, 6);
      this->groupBox9->Name = L"groupBox9";
      this->groupBox9->Size = System::Drawing::Size(413, 90);
      this->groupBox9->TabIndex = 4;
      this->groupBox9->TabStop = false;
      this->groupBox9->Text = L"Movement Authority";
      // 
      // label84
      // 
      this->label84->AutoSize = true;
      this->label84->Location = System::Drawing::Point(384, 61);
      this->label84->Name = L"label84";
      this->label84->Size = System::Drawing::Size(26, 13);
      this->label84->TabIndex = 13;
      this->label84->Text = L"cms";
      // 
      // label60
      // 
      this->label60->AutoSize = true;
      this->label60->Location = System::Drawing::Point(232, 61);
      this->label60->Name = L"label60";
      this->label60->Size = System::Drawing::Size(61, 13);
      this->label60->TabIndex = 12;
      this->label60->Text = L"MA Margin:";
      // 
      // tbMAMargin
      // 
      this->tbMAMargin->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbMAMargin->Enabled = false;
      this->tbMAMargin->Location = System::Drawing::Point(298, 58);
      this->tbMAMargin->Name = L"tbMAMargin";
      this->tbMAMargin->Size = System::Drawing::Size(80, 20);
      this->tbMAMargin->TabIndex = 11;
      this->tbMAMargin->TabStop = false;
      this->tbMAMargin->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label42
      // 
      this->label42->AutoSize = true;
      this->label42->Location = System::Drawing::Point(385, 30);
      this->label42->Name = L"label42";
      this->label42->Size = System::Drawing::Size(26, 13);
      this->label42->TabIndex = 10;
      this->label42->Text = L"cms";
      // 
      // tbEndOfMAPos
      // 
      this->tbEndOfMAPos->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbEndOfMAPos->Enabled = false;
      this->tbEndOfMAPos->Location = System::Drawing::Point(298, 27);
      this->tbEndOfMAPos->Name = L"tbEndOfMAPos";
      this->tbEndOfMAPos->Size = System::Drawing::Size(80, 20);
      this->tbEndOfMAPos->TabIndex = 6;
      this->tbEndOfMAPos->TabStop = false;
      this->tbEndOfMAPos->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label44
      // 
      this->label44->AutoSize = true;
      this->label44->Location = System::Drawing::Point(195, 29);
      this->label44->Name = L"label44";
      this->label44->Size = System::Drawing::Size(102, 13);
      this->label44->TabIndex = 7;
      this->label44->Text = L"End of MA, position:";
      // 
      // tbDirection
      // 
      this->tbDirection->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbDirection->Enabled = false;
      this->tbDirection->Location = System::Drawing::Point(113, 55);
      this->tbDirection->Name = L"tbDirection";
      this->tbDirection->Size = System::Drawing::Size(80, 20);
      this->tbDirection->TabIndex = 4;
      this->tbDirection->TabStop = false;
      this->tbDirection->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbDirection->TextChanged += gcnew System::EventHandler(this, &LCSSimForm::tbDirection_TextChanged);
      // 
      // label45
      // 
      this->label45->AutoSize = true;
      this->label45->Location = System::Drawing::Point(57, 58);
      this->label45->Name = L"label45";
      this->label45->Size = System::Drawing::Size(52, 13);
      this->label45->TabIndex = 5;
      this->label45->Text = L"Direction:";
      // 
      // tbEndOfMATrkId
      // 
      this->tbEndOfMATrkId->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbEndOfMATrkId->Enabled = false;
      this->tbEndOfMATrkId->Location = System::Drawing::Point(114, 25);
      this->tbEndOfMATrkId->Name = L"tbEndOfMATrkId";
      this->tbEndOfMATrkId->Size = System::Drawing::Size(80, 20);
      this->tbEndOfMATrkId->TabIndex = 2;
      this->tbEndOfMATrkId->TabStop = false;
      this->tbEndOfMATrkId->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label46
      // 
      this->label46->AutoSize = true;
      this->label46->Location = System::Drawing::Point(2, 27);
      this->label46->Name = L"label46";
      this->label46->Size = System::Drawing::Size(106, 13);
      this->label46->TabIndex = 3;
      this->label46->Text = L"End of MA, Track Id:";
      this->label46->Click += gcnew System::EventHandler(this, &LCSSimForm::label46_Click);
      // 
      // tpWarningCurve
      // 
      this->tpWarningCurve->BackColor = System::Drawing::SystemColors::Control;
      this->tpWarningCurve->Controls->Add(this->dgSpeedCurvePoints);
      this->tpWarningCurve->Controls->Add(this->tbNumCurvePoints);
      this->tpWarningCurve->Controls->Add(this->label43);
      this->tpWarningCurve->Location = System::Drawing::Point(4, 22);
      this->tpWarningCurve->Name = L"tpWarningCurve";
      this->tpWarningCurve->Padding = System::Windows::Forms::Padding(3);
      this->tpWarningCurve->Size = System::Drawing::Size(425, 464);
      this->tpWarningCurve->TabIndex = 10;
      this->tpWarningCurve->Text = L"WarningCurve";
      // 
      // dgSpeedCurvePoints
      // 
      this->dgSpeedCurvePoints->AllowUserToAddRows = false;
      this->dgSpeedCurvePoints->AllowUserToDeleteRows = false;
      this->dgSpeedCurvePoints->AllowUserToResizeRows = false;
      this->dgSpeedCurvePoints->BackgroundColor = System::Drawing::SystemColors::ControlLightLight;
      this->dgSpeedCurvePoints->Columns->AddRange(gcnew cli::array< System::Windows::Forms::DataGridViewColumn^  >(4) {
        this->dgvTbcNumber,
          this->dgvTbcTrackId, this->dgvTbcPos, this->dgvTbcSpeed
      });
      this->dgSpeedCurvePoints->GridColor = System::Drawing::SystemColors::AppWorkspace;
      this->dgSpeedCurvePoints->Location = System::Drawing::Point(3, 30);
      this->dgSpeedCurvePoints->Name = L"dgSpeedCurvePoints";
      this->dgSpeedCurvePoints->ShowEditingIcon = false;
      this->dgSpeedCurvePoints->Size = System::Drawing::Size(415, 428);
      this->dgSpeedCurvePoints->TabIndex = 91;
      // 
      // dgvTbcNumber
      // 
      this->dgvTbcNumber->HeaderText = L"No.";
      this->dgvTbcNumber->Name = L"dgvTbcNumber";
      this->dgvTbcNumber->Width = 32;
      // 
      // dgvTbcTrackId
      // 
      this->dgvTbcTrackId->HeaderText = L"Track Id";
      this->dgvTbcTrackId->Name = L"dgvTbcTrackId";
      this->dgvTbcTrackId->Width = 70;
      // 
      // dgvTbcPos
      // 
      this->dgvTbcPos->HeaderText = L"Position [cm]";
      this->dgvTbcPos->Name = L"dgvTbcPos";
      this->dgvTbcPos->Width = 131;
      // 
      // dgvTbcSpeed
      // 
      this->dgvTbcSpeed->HeaderText = L"Speed [0.1 km/h]";
      this->dgvTbcSpeed->Name = L"dgvTbcSpeed";
      this->dgvTbcSpeed->Width = 133;
      // 
      // tbNumCurvePoints
      // 
      this->tbNumCurvePoints->BackColor = System::Drawing::SystemColors::HighlightText;
      this->tbNumCurvePoints->Enabled = false;
      this->tbNumCurvePoints->Location = System::Drawing::Point(134, 4);
      this->tbNumCurvePoints->Name = L"tbNumCurvePoints";
      this->tbNumCurvePoints->Size = System::Drawing::Size(100, 20);
      this->tbNumCurvePoints->TabIndex = 20;
      this->tbNumCurvePoints->TabStop = false;
      // 
      // label43
      // 
      this->label43->AutoSize = true;
      this->label43->Location = System::Drawing::Point(6, 7);
      this->label43->Name = L"label43";
      this->label43->Size = System::Drawing::Size(122, 13);
      this->label43->TabIndex = 18;
      this->label43->Text = L"Number of Curve Points:";
      this->label43->Click += gcnew System::EventHandler(this, &LCSSimForm::label43_Click);
      // 
      // tpPath
      // 
      this->tpPath->Controls->Add(this->tbTCCVerADSMap);
      this->tpPath->Controls->Add(this->label16);
      this->tpPath->Controls->Add(this->label56);
      this->tpPath->Controls->Add(this->groupBox11);
      this->tpPath->Controls->Add(this->groupBox10);
      this->tpPath->Controls->Add(this->label52);
      this->tpPath->Controls->Add(this->label51);
      this->tpPath->Controls->Add(this->tbReqTOA);
      this->tpPath->Controls->Add(this->tbNextTrackPos);
      this->tpPath->Controls->Add(this->label50);
      this->tpPath->Controls->Add(this->label49);
      this->tpPath->Controls->Add(this->label48);
      this->tpPath->Controls->Add(this->tbNextTrackId);
      this->tpPath->Controls->Add(this->tbSpeedBeginPath);
      this->tpPath->Controls->Add(this->label47);
      this->tpPath->Location = System::Drawing::Point(4, 22);
      this->tpPath->Name = L"tpPath";
      this->tpPath->Padding = System::Windows::Forms::Padding(3);
      this->tpPath->Size = System::Drawing::Size(425, 464);
      this->tpPath->TabIndex = 11;
      this->tpPath->Text = L"Path";
      this->tpPath->UseVisualStyleBackColor = true;
      // 
      // tbTCCVerADSMap
      // 
      this->tbTCCVerADSMap->BackColor = System::Drawing::SystemColors::HighlightText;
      this->tbTCCVerADSMap->Enabled = false;
      this->tbTCCVerADSMap->Location = System::Drawing::Point(172, 111);
      this->tbTCCVerADSMap->Name = L"tbTCCVerADSMap";
      this->tbTCCVerADSMap->Size = System::Drawing::Size(100, 20);
      this->tbTCCVerADSMap->TabIndex = 93;
      this->tbTCCVerADSMap->TabStop = false;
      // 
      // label16
      // 
      this->label16->AutoSize = true;
      this->label16->Location = System::Drawing::Point(12, 114);
      this->label16->Name = L"label16";
      this->label16->Size = System::Drawing::Size(97, 13);
      this->label16->TabIndex = 92;
      this->label16->Text = L"TCC ver ADS map:";
      // 
      // label56
      // 
      this->label56->AutoSize = true;
      this->label56->Location = System::Drawing::Point(277, 61);
      this->label56->Name = L"label56";
      this->label56->Size = System::Drawing::Size(21, 13);
      this->label56->TabIndex = 91;
      this->label56->Text = L"cm";
      // 
      // groupBox11
      // 
      this->groupBox11->BackColor = System::Drawing::SystemColors::ControlLightLight;
      this->groupBox11->Controls->Add(this->tbNumSpeedChanges);
      this->groupBox11->Controls->Add(this->label55);
      this->groupBox11->Controls->Add(this->groupBox12);
      this->groupBox11->Location = System::Drawing::Point(139, 137);
      this->groupBox11->Name = L"groupBox11";
      this->groupBox11->Size = System::Drawing::Size(283, 327);
      this->groupBox11->TabIndex = 90;
      this->groupBox11->TabStop = false;
      this->groupBox11->Text = L"Path Speed changes";
      this->groupBox11->Enter += gcnew System::EventHandler(this, &LCSSimForm::groupBox11_Enter);
      // 
      // tbNumSpeedChanges
      // 
      this->tbNumSpeedChanges->BackColor = System::Drawing::SystemColors::HighlightText;
      this->tbNumSpeedChanges->Enabled = false;
      this->tbNumSpeedChanges->Location = System::Drawing::Point(122, 18);
      this->tbNumSpeedChanges->Name = L"tbNumSpeedChanges";
      this->tbNumSpeedChanges->Size = System::Drawing::Size(137, 20);
      this->tbNumSpeedChanges->TabIndex = 91;
      this->tbNumSpeedChanges->TabStop = false;
      // 
      // label55
      // 
      this->label55->AutoSize = true;
      this->label55->Location = System::Drawing::Point(6, 22);
      this->label55->Name = L"label55";
      this->label55->Size = System::Drawing::Size(110, 13);
      this->label55->TabIndex = 90;
      this->label55->Text = L"Num Speed changes:";
      this->label55->Click += gcnew System::EventHandler(this, &LCSSimForm::label55_Click);
      // 
      // groupBox12
      // 
      this->groupBox12->BackColor = System::Drawing::SystemColors::ControlLightLight;
      this->groupBox12->Controls->Add(this->lvPathSpeedChanges);
      this->groupBox12->Location = System::Drawing::Point(0, 44);
      this->groupBox12->Name = L"groupBox12";
      this->groupBox12->Size = System::Drawing::Size(283, 309);
      this->groupBox12->TabIndex = 92;
      this->groupBox12->TabStop = false;
      this->groupBox12->Text = L"Next Target Data";
      // 
      // lvPathSpeedChanges
      // 
      this->lvPathSpeedChanges->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(4) {
        this->columnHeader19,
          this->columnHeader20, this->columnHeader16, this->columnHeader17
      });
      this->lvPathSpeedChanges->GridLines = true;
      this->lvPathSpeedChanges->Items->AddRange(gcnew cli::array< System::Windows::Forms::ListViewItem^  >(1) { listViewItem6 });
      this->lvPathSpeedChanges->Location = System::Drawing::Point(0, 19);
      this->lvPathSpeedChanges->Name = L"lvPathSpeedChanges";
      this->lvPathSpeedChanges->Size = System::Drawing::Size(283, 287);
      this->lvPathSpeedChanges->TabIndex = 88;
      this->lvPathSpeedChanges->TileSize = System::Drawing::Size(168, 30);
      this->lvPathSpeedChanges->UseCompatibleStateImageBehavior = false;
      this->lvPathSpeedChanges->View = System::Windows::Forms::View::Details;
      this->lvPathSpeedChanges->SelectedIndexChanged += gcnew System::EventHandler(this, &LCSSimForm::lvPathSpeedChanges_SelectedIndexChanged);
      // 
      // columnHeader19
      // 
      this->columnHeader19->Text = L"No.";
      this->columnHeader19->Width = 32;
      // 
      // columnHeader20
      // 
      this->columnHeader20->Text = L"Track Id";
      this->columnHeader20->Width = 52;
      // 
      // columnHeader16
      // 
      this->columnHeader16->Text = L"Track pos(cm)";
      this->columnHeader16->Width = 80;
      // 
      // columnHeader17
      // 
      this->columnHeader17->Text = L"New Speed(x0.1km/h)";
      this->columnHeader17->Width = 122;
      // 
      // groupBox10
      // 
      this->groupBox10->BackColor = System::Drawing::SystemColors::ControlLightLight;
      this->groupBox10->Controls->Add(this->lvPathTracks);
      this->groupBox10->Controls->Add(this->tbNumTracksPath);
      this->groupBox10->Controls->Add(this->label54);
      this->groupBox10->Location = System::Drawing::Point(1, 137);
      this->groupBox10->Name = L"groupBox10";
      this->groupBox10->Size = System::Drawing::Size(129, 319);
      this->groupBox10->TabIndex = 89;
      this->groupBox10->TabStop = false;
      this->groupBox10->Text = L"Path Tracks";
      this->groupBox10->Enter += gcnew System::EventHandler(this, &LCSSimForm::groupBox10_Enter_1);
      // 
      // lvPathTracks
      // 
      this->lvPathTracks->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(2) {
        this->columnHeader14,
          this->columnHeader15
      });
      this->lvPathTracks->GridLines = true;
      this->lvPathTracks->Items->AddRange(gcnew cli::array< System::Windows::Forms::ListViewItem^  >(1) { listViewItem7 });
      this->lvPathTracks->Location = System::Drawing::Point(4, 42);
      this->lvPathTracks->Name = L"lvPathTracks";
      this->lvPathTracks->Size = System::Drawing::Size(118, 303);
      this->lvPathTracks->TabIndex = 88;
      this->lvPathTracks->UseCompatibleStateImageBehavior = false;
      this->lvPathTracks->View = System::Windows::Forms::View::Details;
      // 
      // columnHeader14
      // 
      this->columnHeader14->Text = L"No.";
      this->columnHeader14->Width = 39;
      // 
      // columnHeader15
      // 
      this->columnHeader15->Text = L"Track ID";
      this->columnHeader15->Width = 75;
      // 
      // tbNumTracksPath
      // 
      this->tbNumTracksPath->BackColor = System::Drawing::SystemColors::HighlightText;
      this->tbNumTracksPath->Enabled = false;
      this->tbNumTracksPath->Location = System::Drawing::Point(72, 16);
      this->tbNumTracksPath->Name = L"tbNumTracksPath";
      this->tbNumTracksPath->Size = System::Drawing::Size(51, 20);
      this->tbNumTracksPath->TabIndex = 90;
      this->tbNumTracksPath->TabStop = false;
      // 
      // label54
      // 
      this->label54->AutoSize = true;
      this->label54->Location = System::Drawing::Point(4, 18);
      this->label54->Name = L"label54";
      this->label54->Size = System::Drawing::Size(68, 13);
      this->label54->TabIndex = 89;
      this->label54->Text = L"Num Tracks:";
      this->label54->Click += gcnew System::EventHandler(this, &LCSSimForm::label54_Click);
      // 
      // label52
      // 
      this->label52->AutoSize = true;
      this->label52->Location = System::Drawing::Point(277, 91);
      this->label52->Name = L"label52";
      this->label52->Size = System::Drawing::Size(55, 13);
      this->label52->TabIndex = 86;
      this->label52->Text = L"UTC Time";
      // 
      // label51
      // 
      this->label51->AutoSize = true;
      this->label51->Location = System::Drawing::Point(12, 87);
      this->label51->Name = L"label51";
      this->label51->Size = System::Drawing::Size(127, 13);
      this->label51->TabIndex = 85;
      this->label51->Text = L"Req. TOA at next Target:";
      // 
      // tbReqTOA
      // 
      this->tbReqTOA->BackColor = System::Drawing::SystemColors::HighlightText;
      this->tbReqTOA->Enabled = false;
      this->tbReqTOA->Location = System::Drawing::Point(171, 84);
      this->tbReqTOA->Name = L"tbReqTOA";
      this->tbReqTOA->Size = System::Drawing::Size(100, 20);
      this->tbReqTOA->TabIndex = 84;
      this->tbReqTOA->TabStop = false;
      // 
      // tbNextTrackPos
      // 
      this->tbNextTrackPos->BackColor = System::Drawing::SystemColors::HighlightText;
      this->tbNextTrackPos->Enabled = false;
      this->tbNextTrackPos->Location = System::Drawing::Point(171, 58);
      this->tbNextTrackPos->Name = L"tbNextTrackPos";
      this->tbNextTrackPos->Size = System::Drawing::Size(100, 20);
      this->tbNextTrackPos->TabIndex = 83;
      this->tbNextTrackPos->TabStop = false;
      // 
      // label50
      // 
      this->label50->AutoSize = true;
      this->label50->Location = System::Drawing::Point(12, 61);
      this->label50->Name = L"label50";
      this->label50->Size = System::Drawing::Size(84, 13);
      this->label50->TabIndex = 82;
      this->label50->Text = L"Next Track Pos:";
      // 
      // label49
      // 
      this->label49->AutoSize = true;
      this->label49->Location = System::Drawing::Point(277, 13);
      this->label49->Name = L"label49";
      this->label49->Size = System::Drawing::Size(55, 13);
      this->label49->TabIndex = 81;
      this->label49->Text = L"x0.1 km/h";
      // 
      // label48
      // 
      this->label48->AutoSize = true;
      this->label48->Location = System::Drawing::Point(12, 35);
      this->label48->Name = L"label48";
      this->label48->Size = System::Drawing::Size(75, 13);
      this->label48->TabIndex = 22;
      this->label48->Text = L"Next Track Id:";
      this->label48->Click += gcnew System::EventHandler(this, &LCSSimForm::label48_Click);
      // 
      // tbNextTrackId
      // 
      this->tbNextTrackId->BackColor = System::Drawing::SystemColors::HighlightText;
      this->tbNextTrackId->Enabled = false;
      this->tbNextTrackId->Location = System::Drawing::Point(171, 32);
      this->tbNextTrackId->Name = L"tbNextTrackId";
      this->tbNextTrackId->Size = System::Drawing::Size(100, 20);
      this->tbNextTrackId->TabIndex = 21;
      this->tbNextTrackId->TabStop = false;
      // 
      // tbSpeedBeginPath
      // 
      this->tbSpeedBeginPath->BackColor = System::Drawing::SystemColors::HighlightText;
      this->tbSpeedBeginPath->Enabled = false;
      this->tbSpeedBeginPath->Location = System::Drawing::Point(171, 6);
      this->tbSpeedBeginPath->Name = L"tbSpeedBeginPath";
      this->tbSpeedBeginPath->Size = System::Drawing::Size(100, 20);
      this->tbSpeedBeginPath->TabIndex = 20;
      this->tbSpeedBeginPath->TabStop = false;
      // 
      // label47
      // 
      this->label47->AutoSize = true;
      this->label47->Location = System::Drawing::Point(12, 9);
      this->label47->Name = L"label47";
      this->label47->Size = System::Drawing::Size(139, 13);
      this->label47->TabIndex = 18;
      this->label47->Text = L"Speed at beginning of Path:";
      this->label47->Click += gcnew System::EventHandler(this, &LCSSimForm::label47_Click);
      // 
      // RclInfo
      // 
      this->RclInfo->Controls->Add(this->groupBox21);
      this->RclInfo->Controls->Add(this->groupBox20);
      this->RclInfo->Location = System::Drawing::Point(4, 22);
      this->RclInfo->Name = L"RclInfo";
      this->RclInfo->Padding = System::Windows::Forms::Padding(3);
      this->RclInfo->Size = System::Drawing::Size(425, 464);
      this->RclInfo->TabIndex = 12;
      this->RclInfo->Text = L"RCL";
      this->RclInfo->UseVisualStyleBackColor = true;
      // 
      // groupBox21
      // 
      this->groupBox21->Controls->Add(this->bApplyHandingDone);
      this->groupBox21->Controls->Add(this->cbHandlingDone);
      this->groupBox21->Controls->Add(this->label90);
      this->groupBox21->Location = System::Drawing::Point(7, 298);
      this->groupBox21->Name = L"groupBox21";
      this->groupBox21->Size = System::Drawing::Size(411, 92);
      this->groupBox21->TabIndex = 49;
      this->groupBox21->TabStop = false;
      this->groupBox21->Text = L"RCL Status";
      this->groupBox21->Enter += gcnew System::EventHandler(this, &LCSSimForm::groupBox21_Enter);
      // 
      // bApplyHandingDone
      // 
      this->bApplyHandingDone->Location = System::Drawing::Point(194, 57);
      this->bApplyHandingDone->Name = L"bApplyHandingDone";
      this->bApplyHandingDone->Size = System::Drawing::Size(75, 23);
      this->bApplyHandingDone->TabIndex = 84;
      this->bApplyHandingDone->Text = L"Apply";
      this->bApplyHandingDone->UseVisualStyleBackColor = true;
      this->bApplyHandingDone->Click += gcnew System::EventHandler(this, &LCSSimForm::bApplyHandingDone_Click);
      // 
      // cbHandlingDone
      // 
      this->cbHandlingDone->DisplayMember = L"1";
      this->cbHandlingDone->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbHandlingDone->DropDownWidth = 120;
      this->cbHandlingDone->FormattingEnabled = true;
      this->cbHandlingDone->Items->AddRange(gcnew cli::array< System::Object^  >(3) { L"Undefined", L"Requested", L"NotRequested" });
      this->cbHandlingDone->Location = System::Drawing::Point(181, 22);
      this->cbHandlingDone->Name = L"cbHandlingDone";
      this->cbHandlingDone->Size = System::Drawing::Size(109, 21);
      this->cbHandlingDone->TabIndex = 51;
      this->cbHandlingDone->SelectedIndexChanged += gcnew System::EventHandler(this, &LCSSimForm::cbHandlingDone_SelectedIndexChanged);
      // 
      // label90
      // 
      this->label90->AutoSize = true;
      this->label90->Location = System::Drawing::Point(11, 30);
      this->label90->Name = L"label90";
      this->label90->Size = System::Drawing::Size(78, 13);
      this->label90->TabIndex = 49;
      this->label90->Text = L"Handling Done";
      this->label90->Click += gcnew System::EventHandler(this, &LCSSimForm::label90_Click);
      // 
      // groupBox20
      // 
      this->groupBox20->Controls->Add(this->label82);
      this->groupBox20->Controls->Add(this->tbCurCeilingSpeed);
      this->groupBox20->Controls->Add(this->label81);
      this->groupBox20->Controls->Add(this->tbTrainOrientation);
      this->groupBox20->Controls->Add(this->label80);
      this->groupBox20->Controls->Add(this->tbRevcDtg);
      this->groupBox20->Controls->Add(this->label79);
      this->groupBox20->Controls->Add(this->label71);
      this->groupBox20->Controls->Add(this->label72);
      this->groupBox20->Controls->Add(this->tbFwdDtg);
      this->groupBox20->Controls->Add(this->label73);
      this->groupBox20->Controls->Add(this->tbAllowedTrainMovement);
      this->groupBox20->Controls->Add(this->label74);
      this->groupBox20->Controls->Add(this->tbAosInterventionApplied);
      this->groupBox20->Controls->Add(this->label75);
      this->groupBox20->Controls->Add(this->tbAosOperationalMode);
      this->groupBox20->Controls->Add(this->label78);
      this->groupBox20->Location = System::Drawing::Point(6, 6);
      this->groupBox20->Name = L"groupBox20";
      this->groupBox20->Size = System::Drawing::Size(413, 272);
      this->groupBox20->TabIndex = 5;
      this->groupBox20->TabStop = false;
      this->groupBox20->Text = L"RCL Information";
      this->groupBox20->Enter += gcnew System::EventHandler(this, &LCSSimForm::groupBox20_Enter);
      // 
      // label82
      // 
      this->label82->AutoSize = true;
      this->label82->Location = System::Drawing::Point(296, 237);
      this->label82->Name = L"label82";
      this->label82->Size = System::Drawing::Size(32, 13);
      this->label82->TabIndex = 20;
      this->label82->Text = L"km/h";
      // 
      // tbCurCeilingSpeed
      // 
      this->tbCurCeilingSpeed->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbCurCeilingSpeed->Enabled = false;
      this->tbCurCeilingSpeed->Location = System::Drawing::Point(184, 234);
      this->tbCurCeilingSpeed->Name = L"tbCurCeilingSpeed";
      this->tbCurCeilingSpeed->Size = System::Drawing::Size(105, 20);
      this->tbCurCeilingSpeed->TabIndex = 19;
      this->tbCurCeilingSpeed->TabStop = false;
      this->tbCurCeilingSpeed->TextChanged += gcnew System::EventHandler(this, &LCSSimForm::tbCurCeilingSpeed_TextChanged);
      // 
      // label81
      // 
      this->label81->AutoSize = true;
      this->label81->Location = System::Drawing::Point(1, 237);
      this->label81->Name = L"label81";
      this->label81->Size = System::Drawing::Size(109, 13);
      this->label81->TabIndex = 18;
      this->label81->Text = L"Current ceiling speed:";
      this->label81->Click += gcnew System::EventHandler(this, &LCSSimForm::label81_Click);
      // 
      // tbTrainOrientation
      // 
      this->tbTrainOrientation->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbTrainOrientation->Enabled = false;
      this->tbTrainOrientation->Location = System::Drawing::Point(184, 204);
      this->tbTrainOrientation->Name = L"tbTrainOrientation";
      this->tbTrainOrientation->Size = System::Drawing::Size(204, 20);
      this->tbTrainOrientation->TabIndex = 17;
      this->tbTrainOrientation->TabStop = false;
      this->tbTrainOrientation->TextChanged += gcnew System::EventHandler(this, &LCSSimForm::tbTrainOrientation_TextChanged);
      // 
      // label80
      // 
      this->label80->AutoSize = true;
      this->label80->Location = System::Drawing::Point(0, 204);
      this->label80->Name = L"label80";
      this->label80->Size = System::Drawing::Size(86, 13);
      this->label80->TabIndex = 16;
      this->label80->Text = L"Train orientation:";
      // 
      // tbRevcDtg
      // 
      this->tbRevcDtg->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbRevcDtg->Enabled = false;
      this->tbRevcDtg->Location = System::Drawing::Point(184, 169);
      this->tbRevcDtg->Name = L"tbRevcDtg";
      this->tbRevcDtg->Size = System::Drawing::Size(105, 20);
      this->tbRevcDtg->TabIndex = 15;
      this->tbRevcDtg->TabStop = false;
      this->tbRevcDtg->TextChanged += gcnew System::EventHandler(this, &LCSSimForm::tbRevcDtg_TextChanged);
      // 
      // label79
      // 
      this->label79->AutoSize = true;
      this->label79->Location = System::Drawing::Point(1, 169);
      this->label79->Name = L"label79";
      this->label79->Size = System::Drawing::Size(128, 13);
      this->label79->TabIndex = 14;
      this->label79->Text = L"Reverse Distance To Go:";
      // 
      // label71
      // 
      this->label71->AutoSize = true;
      this->label71->Location = System::Drawing::Point(296, 169);
      this->label71->Name = L"label71";
      this->label71->Size = System::Drawing::Size(21, 13);
      this->label71->TabIndex = 13;
      this->label71->Text = L"cm";
      // 
      // label72
      // 
      this->label72->AutoSize = true;
      this->label72->Location = System::Drawing::Point(1, 134);
      this->label72->Name = L"label72";
      this->label72->Size = System::Drawing::Size(126, 13);
      this->label72->TabIndex = 12;
      this->label72->Text = L"Forward Distance To Go:";
      // 
      // tbFwdDtg
      // 
      this->tbFwdDtg->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbFwdDtg->Enabled = false;
      this->tbFwdDtg->Location = System::Drawing::Point(184, 132);
      this->tbFwdDtg->Name = L"tbFwdDtg";
      this->tbFwdDtg->Size = System::Drawing::Size(105, 20);
      this->tbFwdDtg->TabIndex = 11;
      this->tbFwdDtg->TabStop = false;
      this->tbFwdDtg->TextChanged += gcnew System::EventHandler(this, &LCSSimForm::tbFwdDtg_TextChanged);
      // 
      // label73
      // 
      this->label73->AutoSize = true;
      this->label73->Location = System::Drawing::Point(296, 132);
      this->label73->Name = L"label73";
      this->label73->Size = System::Drawing::Size(21, 13);
      this->label73->TabIndex = 10;
      this->label73->Text = L"cm";
      // 
      // tbAllowedTrainMovement
      // 
      this->tbAllowedTrainMovement->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbAllowedTrainMovement->Enabled = false;
      this->tbAllowedTrainMovement->Location = System::Drawing::Point(184, 89);
      this->tbAllowedTrainMovement->Name = L"tbAllowedTrainMovement";
      this->tbAllowedTrainMovement->Size = System::Drawing::Size(105, 20);
      this->tbAllowedTrainMovement->TabIndex = 6;
      this->tbAllowedTrainMovement->TabStop = false;
      this->tbAllowedTrainMovement->TextChanged += gcnew System::EventHandler(this, &LCSSimForm::tbAllowedTrainMovement_TextChanged);
      // 
      // label74
      // 
      this->label74->AutoSize = true;
      this->label74->Location = System::Drawing::Point(0, 96);
      this->label74->Name = L"label74";
      this->label74->Size = System::Drawing::Size(122, 13);
      this->label74->TabIndex = 7;
      this->label74->Text = L"Allowed train movement:";
      this->label74->Click += gcnew System::EventHandler(this, &LCSSimForm::label74_Click);
      // 
      // tbAosInterventionApplied
      // 
      this->tbAosInterventionApplied->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbAosInterventionApplied->Enabled = false;
      this->tbAosInterventionApplied->Location = System::Drawing::Point(184, 55);
      this->tbAosInterventionApplied->Name = L"tbAosInterventionApplied";
      this->tbAosInterventionApplied->Size = System::Drawing::Size(105, 20);
      this->tbAosInterventionApplied->TabIndex = 4;
      this->tbAosInterventionApplied->TabStop = false;
      this->tbAosInterventionApplied->TextChanged += gcnew System::EventHandler(this, &LCSSimForm::tbAosInterventionApplied_TextChanged);
      // 
      // label75
      // 
      this->label75->AutoSize = true;
      this->label75->Location = System::Drawing::Point(1, 58);
      this->label75->Name = L"label75";
      this->label75->Size = System::Drawing::Size(127, 13);
      this->label75->TabIndex = 5;
      this->label75->Text = L"AOS intervention applied:";
      // 
      // tbAosOperationalMode
      // 
      this->tbAosOperationalMode->BackColor = System::Drawing::SystemColors::MenuBar;
      this->tbAosOperationalMode->Enabled = false;
      this->tbAosOperationalMode->Location = System::Drawing::Point(184, 25);
      this->tbAosOperationalMode->Name = L"tbAosOperationalMode";
      this->tbAosOperationalMode->Size = System::Drawing::Size(107, 20);
      this->tbAosOperationalMode->TabIndex = 2;
      this->tbAosOperationalMode->TabStop = false;
      this->tbAosOperationalMode->TextChanged += gcnew System::EventHandler(this, &LCSSimForm::tbAosOperationalMode_TextChanged);
      // 
      // label78
      // 
      this->label78->AutoSize = true;
      this->label78->Location = System::Drawing::Point(2, 27);
      this->label78->Name = L"label78";
      this->label78->Size = System::Drawing::Size(118, 13);
      this->label78->TabIndex = 3;
      this->label78->Text = L"AOS Operational mode:";
      this->label78->Click += gcnew System::EventHandler(this, &LCSSimForm::label78_Click);
      // 
      // groupBox17
      // 
      this->groupBox17->Location = System::Drawing::Point(0, 0);
      this->groupBox17->Name = L"groupBox17";
      this->groupBox17->Size = System::Drawing::Size(200, 100);
      this->groupBox17->TabIndex = 0;
      this->groupBox17->TabStop = false;
      // 
      // textBox5
      // 
      this->textBox5->Location = System::Drawing::Point(0, 0);
      this->textBox5->Name = L"textBox5";
      this->textBox5->Size = System::Drawing::Size(100, 20);
      this->textBox5->TabIndex = 0;
      // 
      // label86
      // 
      this->label86->Location = System::Drawing::Point(0, 0);
      this->label86->Name = L"label86";
      this->label86->Size = System::Drawing::Size(100, 23);
      this->label86->TabIndex = 0;
      // 
      // textBox8
      // 
      this->textBox8->Location = System::Drawing::Point(0, 0);
      this->textBox8->Name = L"textBox8";
      this->textBox8->Size = System::Drawing::Size(100, 20);
      this->textBox8->TabIndex = 0;
      // 
      // label87
      // 
      this->label87->Location = System::Drawing::Point(0, 0);
      this->label87->Name = L"label87";
      this->label87->Size = System::Drawing::Size(100, 23);
      this->label87->TabIndex = 0;
      // 
      // groupBox18
      // 
      this->groupBox18->Location = System::Drawing::Point(0, 0);
      this->groupBox18->Name = L"groupBox18";
      this->groupBox18->Size = System::Drawing::Size(200, 100);
      this->groupBox18->TabIndex = 0;
      this->groupBox18->TabStop = false;
      // 
      // groupBox19
      // 
      this->groupBox19->Location = System::Drawing::Point(0, 0);
      this->groupBox19->Name = L"groupBox19";
      this->groupBox19->Size = System::Drawing::Size(200, 100);
      this->groupBox19->TabIndex = 0;
      this->groupBox19->TabStop = false;
      // 
      // textBox9
      // 
      this->textBox9->Location = System::Drawing::Point(0, 0);
      this->textBox9->Name = L"textBox9";
      this->textBox9->Size = System::Drawing::Size(100, 20);
      this->textBox9->TabIndex = 0;
      // 
      // label76
      // 
      this->label76->Location = System::Drawing::Point(0, 0);
      this->label76->Name = L"label76";
      this->label76->Size = System::Drawing::Size(100, 23);
      this->label76->TabIndex = 0;
      // 
      // textBox10
      // 
      this->textBox10->Location = System::Drawing::Point(0, 0);
      this->textBox10->Name = L"textBox10";
      this->textBox10->Size = System::Drawing::Size(100, 20);
      this->textBox10->TabIndex = 0;
      // 
      // label77
      // 
      this->label77->Location = System::Drawing::Point(0, 0);
      this->label77->Name = L"label77";
      this->label77->Size = System::Drawing::Size(100, 23);
      this->label77->TabIndex = 0;
      // 
      // button1
      // 
      this->button1->Location = System::Drawing::Point(0, 0);
      this->button1->Name = L"button1";
      this->button1->Size = System::Drawing::Size(75, 23);
      this->button1->TabIndex = 0;
      // 
      // groupBox13
      // 
      this->groupBox13->Controls->Add(this->label63);
      this->groupBox13->Controls->Add(this->label64);
      this->groupBox13->Controls->Add(this->textBox6);
      this->groupBox13->Controls->Add(this->label65);
      this->groupBox13->Controls->Add(this->textBox7);
      this->groupBox13->Controls->Add(this->label66);
      this->groupBox13->Location = System::Drawing::Point(15, 168);
      this->groupBox13->Name = L"groupBox13";
      this->groupBox13->Size = System::Drawing::Size(393, 93);
      this->groupBox13->TabIndex = 48;
      this->groupBox13->TabStop = false;
      this->groupBox13->Text = L"Params";
      // 
      // label63
      // 
      this->label63->AutoSize = true;
      this->label63->Location = System::Drawing::Point(305, 32);
      this->label63->Name = L"label63";
      this->label63->Size = System::Drawing::Size(15, 13);
      this->label63->TabIndex = 52;
      this->label63->Text = L"%";
      // 
      // label64
      // 
      this->label64->AutoSize = true;
      this->label64->Location = System::Drawing::Point(306, 61);
      this->label64->Name = L"label64";
      this->label64->Size = System::Drawing::Size(45, 13);
      this->label64->TabIndex = 51;
      this->label64->Text = L"x 0.1bar";
      // 
      // textBox6
      // 
      this->textBox6->BackColor = System::Drawing::SystemColors::MenuBar;
      this->textBox6->Location = System::Drawing::Point(157, 55);
      this->textBox6->Name = L"textBox6";
      this->textBox6->Size = System::Drawing::Size(142, 20);
      this->textBox6->TabIndex = 50;
      this->textBox6->TabStop = false;
      this->textBox6->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label65
      // 
      this->label65->AutoSize = true;
      this->label65->Location = System::Drawing::Point(11, 58);
      this->label65->Name = L"label65";
      this->label65->Size = System::Drawing::Size(124, 13);
      this->label65->TabIndex = 49;
      this->label65->Text = L"Last Car Brake Pressure:";
      // 
      // textBox7
      // 
      this->textBox7->BackColor = System::Drawing::SystemColors::MenuBar;
      this->textBox7->Location = System::Drawing::Point(157, 29);
      this->textBox7->Name = L"textBox7";
      this->textBox7->Size = System::Drawing::Size(142, 20);
      this->textBox7->TabIndex = 48;
      this->textBox7->TabStop = false;
      this->textBox7->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      // 
      // label66
      // 
      this->label66->AutoSize = true;
      this->label66->Location = System::Drawing::Point(11, 32);
      this->label66->Name = L"label66";
      this->label66->Size = System::Drawing::Size(113, 13);
      this->label66->TabIndex = 47;
      this->label66->Text = L"Percentage of Brakes:";
      // 
      // groupBox15
      // 
      this->groupBox15->Controls->Add(this->comboBox1);
      this->groupBox15->Controls->Add(this->label67);
      this->groupBox15->Controls->Add(this->comboBox2);
      this->groupBox15->Controls->Add(this->label68);
      this->groupBox15->Controls->Add(this->comboBox3);
      this->groupBox15->Controls->Add(this->label69);
      this->groupBox15->Controls->Add(this->comboBox4);
      this->groupBox15->Controls->Add(this->label70);
      this->groupBox15->Location = System::Drawing::Point(15, 13);
      this->groupBox15->Name = L"groupBox15";
      this->groupBox15->Size = System::Drawing::Size(394, 149);
      this->groupBox15->TabIndex = 47;
      this->groupBox15->TabStop = false;
      this->groupBox15->Text = L"Modes/Status";
      this->groupBox15->Enter += gcnew System::EventHandler(this, &LCSSimForm::groupBox15_Enter);
      // 
      // comboBox1
      // 
      this->comboBox1->DisplayMember = L"1";
      this->comboBox1->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->comboBox1->DropDownWidth = 120;
      this->comboBox1->FormattingEnabled = true;
      this->comboBox1->Items->AddRange(gcnew cli::array< System::Object^  >(3) { L"Waiting", L"Confirmed", L"Not Asserted" });
      this->comboBox1->Location = System::Drawing::Point(157, 107);
      this->comboBox1->Name = L"comboBox1";
      this->comboBox1->Size = System::Drawing::Size(142, 21);
      this->comboBox1->TabIndex = 48;
      // 
      // label67
      // 
      this->label67->AutoSize = true;
      this->label67->Location = System::Drawing::Point(11, 110);
      this->label67->Name = L"label67";
      this->label67->Size = System::Drawing::Size(138, 13);
      this->label67->TabIndex = 47;
      this->label67->Text = L"Train Integrity Status ECPB:";
      // 
      // comboBox2
      // 
      this->comboBox2->DisplayMember = L"1";
      this->comboBox2->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->comboBox2->DropDownWidth = 120;
      this->comboBox2->FormattingEnabled = true;
      this->comboBox2->Items->AddRange(gcnew cli::array< System::Object^  >(5) {
        L"Run", L"Initialization", L"Switch", L"CutOut",
          L"Not Available"
      });
      this->comboBox2->Location = System::Drawing::Point(157, 53);
      this->comboBox2->Name = L"comboBox2";
      this->comboBox2->Size = System::Drawing::Size(142, 21);
      this->comboBox2->TabIndex = 46;
      // 
      // label68
      // 
      this->label68->AutoSize = true;
      this->label68->Location = System::Drawing::Point(11, 56);
      this->label68->Name = L"label68";
      this->label68->Size = System::Drawing::Size(121, 13);
      this->label68->TabIndex = 45;
      this->label68->Text = L"ECPB Operating modes:";
      // 
      // comboBox3
      // 
      this->comboBox3->DisplayMember = L"1";
      this->comboBox3->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->comboBox3->DropDownWidth = 120;
      this->comboBox3->FormattingEnabled = true;
      this->comboBox3->Items->AddRange(gcnew cli::array< System::Object^  >(2) { L"Pneumatic", L"ECPB" });
      this->comboBox3->Location = System::Drawing::Point(157, 26);
      this->comboBox3->Name = L"comboBox3";
      this->comboBox3->Size = System::Drawing::Size(142, 21);
      this->comboBox3->TabIndex = 44;
      this->comboBox3->SelectedIndexChanged += gcnew System::EventHandler(this, &LCSSimForm::comboBox3_SelectedIndexChanged);
      // 
      // label69
      // 
      this->label69->AutoSize = true;
      this->label69->Location = System::Drawing::Point(11, 29);
      this->label69->Name = L"label69";
      this->label69->Size = System::Drawing::Size(109, 13);
      this->label69->TabIndex = 43;
      this->label69->Text = L"Brake System In Use:";
      // 
      // comboBox4
      // 
      this->comboBox4->DisplayMember = L"1";
      this->comboBox4->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->comboBox4->DropDownWidth = 120;
      this->comboBox4->FormattingEnabled = true;
      this->comboBox4->Items->AddRange(gcnew cli::array< System::Object^  >(3) { L"Unknown", L"Scanning", L"ConfigKnown" });
      this->comboBox4->Location = System::Drawing::Point(157, 80);
      this->comboBox4->Name = L"comboBox4";
      this->comboBox4->Size = System::Drawing::Size(142, 21);
      this->comboBox4->TabIndex = 42;
      // 
      // label70
      // 
      this->label70->AutoSize = true;
      this->label70->Location = System::Drawing::Point(11, 83);
      this->label70->Name = L"label70";
      this->label70->Size = System::Drawing::Size(123, 13);
      this->label70->TabIndex = 41;
      this->label70->Text = L"ECPB Sequence Status:";
      // 
      // LCSSimForm
      // 
      this->AutoScaleDimensions = System::Drawing::SizeF(6, 13);
      this->AutoScaleMode = System::Windows::Forms::AutoScaleMode::Font;
      this->ClientSize = System::Drawing::Size(446, 530);
      this->ControlBox = false;
      this->Controls->Add(this->toolStrip1);
      this->Controls->Add(this->tcMainControl);
      this->FormBorderStyle = System::Windows::Forms::FormBorderStyle::FixedToolWindow;
      this->Icon = (cli::safe_cast<System::Drawing::Icon^>(resources->GetObject(L"$this.Icon")));
      this->MaximizeBox = false;
      this->MaximumSize = System::Drawing::Size(462, 564);
      this->MinimizeBox = false;
      this->MinimumSize = System::Drawing::Size(462, 564);
      this->Name = L"LCSSimForm";
      this->SizeGripStyle = System::Windows::Forms::SizeGripStyle::Hide;
      this->StartPosition = System::Windows::Forms::FormStartPosition::Manual;
      this->Text = L"LCSSim";
      this->FormClosing += gcnew System::Windows::Forms::FormClosingEventHandler(this, &LCSSimForm::LCSSimForm_FormClosing);
      this->Load += gcnew System::EventHandler(this, &LCSSimForm::LCSSimForm_Load);
      this->Shown += gcnew System::EventHandler(this, &LCSSimForm::LCSSimForm_Shown);
      this->LocationChanged += gcnew System::EventHandler(this, &LCSSimForm::LCSSimForm_LocationChanged);
      this->VisibleChanged += gcnew System::EventHandler(this, &LCSSimForm::LCSSimForm_VisibleChanged);
      this->toolStrip1->ResumeLayout(false);
      this->toolStrip1->PerformLayout();
      this->tpDebug->ResumeLayout(false);
      this->tpDataToLocoSim->ResumeLayout(false);
      this->tpDataToLocoSim->PerformLayout();
      this->tpDataFromLocoSim->ResumeLayout(false);
      this->tpDataFromLocoSim->PerformLayout();
      this->tpDataToATO->ResumeLayout(false);
      this->tpDataToATO->PerformLayout();
      this->tpDataFromATO->ResumeLayout(false);
      this->tpDataFromATO->PerformLayout();
      this->tpLCSBrakes->ResumeLayout(false);
      this->groupBox8->ResumeLayout(false);
      this->groupBox8->PerformLayout();
      this->groupBox7->ResumeLayout(false);
      this->groupBox7->PerformLayout();
      this->tpLCSATO->ResumeLayout(false);
      this->groupBox16->ResumeLayout(false);
      this->groupBox16->PerformLayout();
      this->groupBox5->ResumeLayout(false);
      this->groupBox5->PerformLayout();
      this->groupBox4->ResumeLayout(false);
      this->groupBox4->PerformLayout();
      this->tpLCSFaults->ResumeLayout(false);
      this->groupBox6->ResumeLayout(false);
      this->groupBox6->PerformLayout();
      this->groupBox3->ResumeLayout(false);
      this->groupBox3->PerformLayout();
      this->tpTrainCompECPB->ResumeLayout(false);
      this->groupBox14->ResumeLayout(false);
      this->groupBox14->PerformLayout();
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->dgTrainCompECPB))->EndInit();
      this->tpTrainCompAOS->ResumeLayout(false);
      this->tpTrainCompAOS->PerformLayout();
      this->tpAOSStatus->ResumeLayout(false);
      this->tpAOSStatus->PerformLayout();
      this->groupBox2->ResumeLayout(false);
      this->groupBox2->PerformLayout();
      this->groupBox1->ResumeLayout(false);
      this->groupBox1->PerformLayout();
      this->groupBoxExample->ResumeLayout(false);
      this->groupBoxExample->PerformLayout();
      this->tcMainControl->ResumeLayout(false);
      this->tpMovementAuthority->ResumeLayout(false);
      this->groupBox9->ResumeLayout(false);
      this->groupBox9->PerformLayout();
      this->tpWarningCurve->ResumeLayout(false);
      this->tpWarningCurve->PerformLayout();
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->dgSpeedCurvePoints))->EndInit();
      this->tpPath->ResumeLayout(false);
      this->tpPath->PerformLayout();
      this->groupBox11->ResumeLayout(false);
      this->groupBox11->PerformLayout();
      this->groupBox12->ResumeLayout(false);
      this->groupBox10->ResumeLayout(false);
      this->groupBox10->PerformLayout();
      this->RclInfo->ResumeLayout(false);
      this->groupBox21->ResumeLayout(false);
      this->groupBox21->PerformLayout();
      this->groupBox20->ResumeLayout(false);
      this->groupBox20->PerformLayout();
      this->groupBox13->ResumeLayout(false);
      this->groupBox13->PerformLayout();
      this->groupBox15->ResumeLayout(false);
      this->groupBox15->PerformLayout();
      this->ResumeLayout(false);
      this->PerformLayout();

    }
#pragma endregion
  private: System::Void LCSSimForm_Load(System::Object^  sender, System::EventArgs^  e) {
    this->Text += lcsSim->DLLVersion;
  }
  private: System::Void LCSSimForm_Shown(System::Object^  sender, System::EventArgs^  e) {
    this->Left = formLeft;
    this->Top = formTop;
    this->Visible = Convert::ToInt16(Registry::GetValue(regRootKey + "\\LCSSim", "Visible", "1")) != 0 ? true : false;
    if (!showDebugTab ||
      !lcsSim->useVSIM)
    {
      tcMainControl->Controls->Remove(tpDebug);
    }
  }
  private: System::Void label13_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void listView1_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label14_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void checkBox6_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void LCSSimForm_VisibleChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void LCSSimForm_LocationChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void LCSSimForm_FormClosing(System::Object^  sender, System::Windows::Forms::FormClosingEventArgs^  e) {
  }

  private: System::Void cbMoving_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
  }

  private: System::Void bSetAlarm_Click(System::Object^  sender, System::EventArgs^  e) {

  }
  private: System::Void bPSimReleaseError_Click(System::Object^  sender, System::EventArgs^  e) {

  }
  private: System::Void tbPSimFileName_TextChanged(System::Object^  sender, System::EventArgs^  e) {

  }
  private: System::Void rbAcceptAnyPantoPos_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {

  }
  private: System::Void rbUseSimFilePantoPos_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {

  }
  private: System::Void bApplyParams_Click(System::Object^  sender, System::EventArgs^  e) {

  }
  private: System::Void bSaveParams_Click(System::Object^  sender, System::EventArgs^  e) {
    // Activate parameters if not already done

  }
  private: System::Void bPSimSelFile_Click(System::Object^  sender, System::EventArgs^  e) {

  }
  private: System::Void cbAlarmImpact_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {

  }
  private: System::Void ParamsChanged_Click(System::Object^  sender, System::EventArgs^  e) {

  }
  private: System::Void lvDataToATO_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label9_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void groupBox1_Enter(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void cbAlarmClass_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label12_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label1_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void tbOverview_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label9_Click_1(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void groupBox1_Enter_1(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void groupBox2_Enter(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label6_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label17_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void checkBox1_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label22_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void comboBox1_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void cbATOCabinSelectorStatus_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label23_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void tpParams_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label25_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label32_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void textBox6_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  }

  private: System::Void bApplyFaults_Click_1(System::Object^  sender, System::EventArgs^  e) {
    UpdateSimValuesFromGUI();
  }
  private: System::Void bApplyAto_Click_1(System::Object^  sender, System::EventArgs^  e) {

    UpdateSimValuesFromGUI();

  }
  private: System::Void bApplyBrakes_Click_1(System::Object^  sender, System::EventArgs^  e) {

    UpdateSimValuesFromGUI();
  }



  private: System::Void groupBox9_Enter(System::Object^  sender, System::EventArgs^  e) {
  }

  private: System::Void ecpbTrainComp_Change(System::Object^  sender, System::Windows::Forms::MouseEventArgs^  e) {
    UpdateECPBTrainCompFromGUI();
  }

  private: System::Void dgTrainCompRowDel(System::Object^  sender, System::Windows::Forms::DataGridViewRowsRemovedEventArgs^  e) {
    UpdateTrainCompECPBRowNumber();
    bApplyTrainComp->Enabled = true;
  }
  private: System::Void dgTrainCompRowAdd(System::Object^  sender, System::Windows::Forms::DataGridViewRowsAddedEventArgs^  e) {
    UpdateTrainCompECPBRowNumber();
    bApplyTrainComp->Enabled = true;
  }

  private: System::Void trainCompFromGUIUpdate(System::Object^  sender, System::EventArgs^  e) {
    UpdateECPBTrainCompFromGUI();
  }
  private: System::Void label46_Click(System::Object^  sender, System::EventArgs^  e) {
  }

  private: System::Void groupBox10_Enter(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label47_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label48_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void groupBox10_Enter_1(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void groupBox11_Enter(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label54_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label55_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void tbECPBTrainCompReq_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void lvPathSpeedChanges_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void cbTrainIntegrityStatusECPBLCS_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void tbPositionOnTrackRear_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void checkBox12_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void checkBox25_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void tbDirection_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label8_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void toolStrip1_ItemClicked(System::Object^  sender, System::Windows::Forms::ToolStripItemClickedEventArgs^  e) {
  }
  private: System::Void cbTractionModeLCS_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label35_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void comboBox3_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void groupBox15_Enter(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void tpLCSPosition_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void label72_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void comboBox18_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
  }
  private: System::Void bApplyLCSFrontPosition_Click(System::Object^  sender, System::EventArgs^  e) {
    UpdateSimValuesFromGUI();
  }

  private: System::Void bApplyLCSPositionRear_Click(System::Object^  sender, System::EventArgs^  e) {
    UpdateSimValuesFromGUI();
  }

  private: System::Void label43_Click(System::Object^  sender, System::EventArgs^  e) {
  }
  
  private: System::Void bSaveTrainComp_Click(System::Object^  sender, System::EventArgs^  e) {
    SaveTrainCompValues();
  }
  
  private: System::Void dgTrainCompECPB_CellContentClick(System::Object^  sender, System::Windows::Forms::DataGridViewCellEventArgs^  e) {
    bApplyTrainComp->Enabled = true;
  }
  
  private: System::Void tbVehDetectedPosUnknown_TextChanged(System::Object^  sender, System::EventArgs^  e) {
    bApplyTrainComp->Enabled = true;
  }
  private: System::Void tbVehNotDetected_TextChanged(System::Object^  sender, System::EventArgs^  e) {
    bApplyTrainComp->Enabled = true;
  }
  private: System::Void buttonImport_Click(System::Object^  sender, System::EventArgs^  e)
  {
    OpenFileDialog^ dialog = gcnew OpenFileDialog;

    dialog->InitialDirectory = ".\\";
    dialog->Filter = "Ini files (*.ini)|*.ini";
    dialog->FilterIndex = 1;
    dialog->RestoreDirectory = true;
    dialog->Title = "Select ini-file with train-composition to import";

    if (dialog->ShowDialog() == System::Windows::Forms::DialogResult::OK)
    {
      ProcessECPBTrainComposition(dialog->FileName, false);
    }
  }
  private: System::Void buttonAppend_Click(System::Object^  sender, System::EventArgs^  e)
  {
    OpenFileDialog^ dialog = gcnew OpenFileDialog;

    dialog->InitialDirectory = ".\\";
    dialog->Filter = "Ini files (*.ini)|*.ini";
    dialog->FilterIndex = 1;
    dialog->RestoreDirectory = true;
    dialog->Title = "Select ini-file with train-composition to append to existing";

    if (dialog->ShowDialog() == System::Windows::Forms::DialogResult::OK)
    {
      ProcessECPBTrainComposition(dialog->FileName, true);
    }
  }

 /**********************************************************
  * Function:     updateECPBTrainIntegrity
  * Description:
  **********************************************************/
 public: Void updateECPBTrainIntegrity(bool newStatus)
  {
    cbTrainIntegrityStatusECPBLCS->SelectedIndex = newStatus ? TrainIntegrityStatusECPBEnum::TrainIntegrityStatusECPBConfirmed : TrainIntegrityStatusECPBWaiting;
    bApplyBrakes_Click_1(nullptr, nullptr);
  }

/**********************************************************
 * Function:     importTrainComp
 * Description:  Imports train composition from ini-file
 *               Returns true if import successful
 **********************************************************/
 public: bool importTrainComp(String^ fileToImport)
 {
   if (ProcessECPBTrainComposition(fileToImport, false))
   {
     UpdateECPBTrainCompFromGUI();
     return true;
   }
   else
   {
     return false;
   }
 }
 /**********************************************************
  * Function:     appendTrainComp
  * Description:  Appends train composition from ini-file
  *               Returns true if append successful
  **********************************************************/
 public: bool appendTrainComp(String^ fileToImport)
 {
   if (ProcessECPBTrainComposition(fileToImport, true))
   {
     UpdateECPBTrainCompFromGUI();
     return true;
   }
   else
   {
     return false;
   }
 }

  
private: System::Void groupBox20_Enter(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void label78_Click(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void label74_Click(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void label81_Click(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void tbAosOperationalMode_TextChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void tbAosInterventionApplied_TextChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void tbAllowedTrainMovement_TextChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void tbFwdDtg_TextChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void tbTrainOrientation_TextChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void tbCurCeilingSpeed_TextChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void tbRevcDtg_TextChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void label11_Click(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void lvDataFromATO_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void groupBox21_Enter(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void label90_Click(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void bApplyHandingDone_Click(System::Object^  sender, System::EventArgs^  e) {
  UpdateSimValuesFromGUI();
}
private: System::Void cbHandlingDone_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
}
};
}