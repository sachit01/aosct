#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* %name: LocoSimForm.h %
*
* %version: 8 %
*
* %created_by: akushwah %
*
* %date_created: 2017-07-13 15:05 %
*
* DESCRIPTION:
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date Name Changes
* ---------------------------------------------------------------------------
* 2013-04-21 Antbäck File created
* 2013-05-03 Antbävck Handle driver intervention of AutoReg and better start/stop control
* 2013-07-10 Blomqvist Added ATx-parameter boxes in Params tab.
* 2013-11-26 Bo H Added LocoName
* 2014-03-10 Antbäck Added balise id handling, Reg + ReReg
* 2014-03-13 Antbäck Removed TimsSim properties
* 2014-03-23 Antbäck Screen handling
* 2014-03-25 Antbäck Force ATOManual if AOS is not running with ATO enabled
* 2014-03-27 Antbäck Only update components when content is changed.
* 2014-03-28 Antbäck Added UseBalisSimFile/BaliseSimFileName
* 2014-04-03 Antbäck Added DoubleBuffered = true
* 2014-04-14 Antbäck Corrected bug, read "use" balise id when changed not "default"
* Added ReReg direction handling
* 2014-12-18 Antbäck Corrected save of setups
* 2016-10-04 Marlundg Changes in I/O for BHP
* 2016-10-16 Marlundg Turn off start/stopp in HIL
* 2017-03-10 Marlundg Support for new I/O
*
*******************************************************************************/

//#include "LocoSimSetup.h"

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Windows::Forms;
using namespace System::Data;
using namespace System::Drawing;
using namespace System::Diagnostics;
using namespace Microsoft::Win32;
using namespace System::Reflection;
using namespace System::Media;

using namespace LocoSimDLL;

#include "LocoConsts.h"

#define UPDATE_IF_DIFFERENT(a, b) { if (a != (b)) {a = (b);}}

namespace AOSPC {

  /// <summary>
  /// Summary for LocoSimForm
  ///
  /// WARNING: If you change the name of this class, you will need to change the
  /// 'Resource File Name' property for the managed resource compiler tool
  /// associated with all .resx files this class depends on. Otherwise,
  /// the designers will not be able to interact properly with localized
  /// resources associated with this form.
  /// </summary>
  public ref class LocoSimForm : public System::Windows::Forms::Form
  {
  public:
    bool atp1ReqStart;
    String^ atpFileName;
    String^ atpArgs;
    bool atp2ReqStart;
    String^ atp2FileName;
    String^ atp2Args;
    bool atoReqStart;
    String^ atoFileName;
    String^ atoArgs;
    bool aosRunning;
  private:
    // Form variables
    bool internalProcesses;
    String^ iniFileName;
    String^ regRootKey;
    int formLeft, formTop;
    Color DefaultButtonColor;

    //int selIndexBrakePressure;

    // Simulation variables
    LocoSimulation^ LocoSimul;
    bool AcceleratedPressed;
    bool BrakePressed;
    bool Cabin1Pressed;
    bool Cabin2Pressed;
    bool LCSReadyPressed;
    bool AOSOffPressed;
    bool EMSActivated;
    bool NcuPressed;
    bool Eb1aPressed;
    bool Eb1bPressed;
    bool Eb2aPressed;
    bool Eb2bPressed;
    bool IsolAPressed;
    bool IsolBPressed;
    bool RoadMPressed;
    bool RailMPressed;

    bool ATP1ToFatalFailurePressed;

    bool AOSStartRequested;
    bool AOSStopRequested;
    bool ATPStartRequested;
    bool ATP2StartRequested;
    bool ATOStartRequested;
    array<String^>^ outputNames;

    //brake pressure variable
    bool manualSimPressed;
    bool automaticSimPressed;
    //bool ebAppliedPressed;
    //bool noBrakeAppliedPressed;
    //bool sbAppliedPressed;
    int bp1valueEntered;
    int bp2valueEntered;

    // TCO Feedback Offset
    UInt16 tcoFbOffsetEntered;

    bool oldAOSBuzzer;

    // ATPInfo, used for e.g. AutoControl
    UInt16        permittedSpeed;       // From DMI Interface
    EnumDriveDir  permittedDriveDir;    // From DMI Interface
    ATPModeEnum   atpMode;              // From DMI Interface 
    UInt16        distanceToTarget;     // From DMI Interface
    UInt16        distanceToBCA;        // From DMI Interface
    UInt16        targetSpeed;          // From DMI Interface 
    Int16         trackGradient;        // From DMI Interface 
    Int16         effectiveGradient;    // From DMI Interface 
    UInt16        brakeability;         // From DMI Interface 
    UInt16        brakeDelayEB;         // From DMI Interface 
    UInt16        brakeDelaySB;         // From DMI Interface 


  private: System::Windows::Forms::Panel^ panel1;
  private: System::Windows::Forms::TabPage^ brakePressure;
  private: System::Windows::Forms::GroupBox^ groupBox20;
  private: System::Windows::Forms::CheckBox^ manualSim;
  private: System::Windows::Forms::Label^ lbBrakePressureSensor1;
  private: System::Windows::Forms::TextBox^ bp2Entered;
  private: System::Windows::Forms::Label^ label21;
  private: System::Windows::Forms::Label^ lbBrakePressureSensor2;
  private: System::Windows::Forms::TextBox^ bp1Entered;
  private: System::Windows::Forms::Label^ label9;
  private: System::Windows::Forms::GroupBox^ groupBox19;
  private: System::Windows::Forms::Label^ label25;
  private: System::Windows::Forms::Label^ label24;
  private: System::Windows::Forms::TextBox^ autoSimData;
  private: System::Windows::Forms::TextBox^ autoSimValue;
  private: System::Windows::Forms::Label^ label20;
  private: System::Windows::Forms::Label^ label19;
  private: System::Windows::Forms::TabPage^ tabRailDevIF;
  private: System::Windows::Forms::GroupBox^ groupBox11;
  private: System::Windows::Forms::Label^ label26;
  private: System::Windows::Forms::Label^ lRDSndStr;
  private: System::Windows::Forms::Label^ lRDRecStr;
  private: System::Windows::Forms::Label^ label29;
  private: System::Windows::Forms::Label^ label30;
  private: System::Windows::Forms::Label^ lRDSndTime;
  private: System::Windows::Forms::Label^ label32;
  private: System::Windows::Forms::Label^ lRDRecTime;
  private: System::Windows::Forms::Label^ label34;
  private: System::Windows::Forms::Label^ lRDConnStatus;
  private: System::Windows::Forms::TabPage^ tabPage5;
  private: System::Windows::Forms::Panel^ panel3;
  private: System::Windows::Forms::Panel^ panel4;
  private: System::Windows::Forms::GroupBox^ groupBox8;
  private: System::Windows::Forms::Button^ bSelectBaliseSimFile;
  private: System::Windows::Forms::TextBox^ tbBaliseSimFileName;
  private: System::Windows::Forms::Label^ label23;
  private: System::Windows::Forms::CheckBox^ cbUseBaliseSimulationFile;
  private: System::Windows::Forms::GroupBox^ groupBox17;
  private: System::Windows::Forms::Label^ label17;
  private: System::Windows::Forms::TextBox^ tbRegBaliseDefault;
  private: System::Windows::Forms::GroupBox^ gbArguments;
  private: System::Windows::Forms::TextBox^ ATOArgBox;
  private: System::Windows::Forms::Label^ label49;
  private: System::Windows::Forms::TextBox^ ATP2ArgBox;
  private: System::Windows::Forms::Label^ label48;
  private: System::Windows::Forms::TextBox^ ATP1ArgBox;
  private: System::Windows::Forms::Label^ label47;
  private: System::Windows::Forms::GroupBox^ groupBox12;
  private: System::Windows::Forms::TextBox^ tbSBRet;
  private: System::Windows::Forms::Label^ label39;
  private: System::Windows::Forms::TextBox^ tbEBRet;
  private: System::Windows::Forms::Label^ label37;
  private: System::Windows::Forms::TextBox^ tbMaxRet;
  private: System::Windows::Forms::Label^ label35;
  private: System::Windows::Forms::Label^ label38;
  private: System::Windows::Forms::TextBox^ tbMaxAcc;
  private: System::Windows::Forms::Label^ label36;
  private: System::Windows::Forms::Label^ label31;
  private: System::Windows::Forms::Label^ label33;
  private: System::Windows::Forms::TextBox^ tbMaxSpeed;
  private: System::Windows::Forms::Label^ label28;
  private: System::Windows::Forms::Label^ label27;
  private: System::Windows::Forms::Label^ label40;
  private: System::Windows::Forms::GroupBox^ groupBox13;
  private: System::Windows::Forms::CheckBox^ cbEnableSound;
  private: System::Windows::Forms::GroupBox^ gbAutoReg;
  private: System::Windows::Forms::ComboBox^ cbAutoRegATOMode;
  private: System::Windows::Forms::ComboBox^ cbAutoRegDirection;
  private: System::Windows::Forms::CheckBox^ cbAutoRegEnable;
  private: System::Windows::Forms::Label^ label46;
  private: System::Windows::Forms::Label^ label44;
  private: System::Windows::Forms::Label^ label45;
  private: System::Windows::Forms::Label^ label42;
  private: System::Windows::Forms::Label^ label43;
  private: System::Windows::Forms::Label^ label41;
  private: System::Windows::Forms::TextBox^ tbAutoRegSpeed;
  private: System::Windows::Forms::TextBox^ tbAutoRegAcc;
  private: System::Windows::Forms::Button^ bSaveParams;
  private: System::Windows::Forms::Button^ bApplyParams;
  private: System::Windows::Forms::TabPage^ tabPage2;
  private: System::Windows::Forms::GroupBox^ groupBox9;
  private: System::Windows::Forms::PictureBox^ pbATOStart;
  private: System::Windows::Forms::PictureBox^ pbATP2Start;
  private: System::Windows::Forms::PictureBox^ pbATP1Start;
  private: System::Windows::Forms::Label^ lATOState;
  private: System::Windows::Forms::Label^ label12;
  private: System::Windows::Forms::Label^ lATP2State;
  private: System::Windows::Forms::Label^ label11;
  private: System::Windows::Forms::Label^ lATP1State;
  private: System::Windows::Forms::Label^ label10;
  private: System::Windows::Forms::Button^ bATP1ToFF;
  private: System::Windows::Forms::GroupBox^ groupBox14;
  private: System::Windows::Forms::GroupBox^ groupBox16;
  private: System::Windows::Forms::Label^ label15;
  private: System::Windows::Forms::TextBox^ tbRegUseBalise;
  private: System::Windows::Forms::GroupBox^ groupBox15;
  private: System::Windows::Forms::RadioButton^ rbReRegLastBalise;
  private: System::Windows::Forms::TextBox^ tbReRegSpecBalise;
  private: System::Windows::Forms::RadioButton^ rbReRegSpecBalise;
  private: System::Windows::Forms::TextBox^ tbReRegLastBalise;
  private: System::Windows::Forms::TabPage^ tabPage1;
  private: System::Windows::Forms::GroupBox^ groupBox18;
  private: System::Windows::Forms::Button^ bEb1a;
  private: System::Windows::Forms::Button^ bIsolA;
  private: System::Windows::Forms::Button^ bEb2b;
  private: System::Windows::Forms::Button^ bEb2a;
  private: System::Windows::Forms::Button^ bEb1b;
  private: System::Windows::Forms::GroupBox^ groupBox7;
  private: System::Windows::Forms::Button^ bRoadM;
  private: System::Windows::Forms::Button^ bRailM;
  private: System::Windows::Forms::Button^ bNcu;
  private: System::Windows::Forms::Button^ bCabin2;
  private: System::Windows::Forms::Button^ bEMSAlert;
  private: System::Windows::Forms::Button^ bAOSOff;
  private: System::Windows::Forms::Button^ bCabin1;
  private: System::Windows::Forms::Button^ bLCSReady;
  private: System::Windows::Forms::GroupBox^ groupBox6;
  private: System::Windows::Forms::Label^ label8;
  private: System::Windows::Forms::CheckedListBox^ clbOutputs;
  private: System::Windows::Forms::GroupBox^ groupBox5;
  private: System::Windows::Forms::RadioButton^ rbAutomatic;
  private: System::Windows::Forms::RadioButton^ rbSupervised;
  private: System::Windows::Forms::RadioButton^ rbManual;
  private: System::Windows::Forms::GroupBox^ groupBox4;
  private: System::Windows::Forms::RadioButton^ rbReverse;
  private: System::Windows::Forms::RadioButton^ rbNeutral;
  private: System::Windows::Forms::RadioButton^ rbForward;
  private: System::Windows::Forms::GroupBox^ groupBox3;
  private: System::Windows::Forms::Label^ lAcc;
  private: System::Windows::Forms::Label^ label7;
  private: System::Windows::Forms::GroupBox^ groupBox2;
  private: System::Windows::Forms::Label^ lSpeed;
  private: System::Windows::Forms::Label^ label6;
  private: System::Windows::Forms::GroupBox^ groupBox1;
  private: System::Windows::Forms::Button^ bBrake;
  private: System::Windows::Forms::Button^ bAccelerate;
  private: System::Windows::Forms::Button^ bCoast;
  private: System::Windows::Forms::Label^ label3;
  private: System::Windows::Forms::Label^ label2;
  private: System::Windows::Forms::Label^ label1;
  private: System::Windows::Forms::TrackBar^ bThrottle;
  private: System::Windows::Forms::TabControl^ tabMainControl;
  private: System::Windows::Forms::Button^ bIsolB;
private: System::Windows::Forms::GroupBox^  groupBox21;
private: System::Windows::Forms::Label^  label4;
private: System::Windows::Forms::RadioButton^  rbTcoOrderAndFb;
private: System::Windows::Forms::TextBox^  tbTcoFbOffset;




private: System::Windows::Forms::Label^  label5;
private: System::Windows::Forms::RadioButton^  rbTcoOnlyFb;
private: System::Windows::Forms::RadioButton^  rbTcoNoFb;
private: System::Windows::Forms::TabPage^  tabPageAutoControl;
private: System::Windows::Forms::TabPage^  tabPageDMIInterface;
private: System::Windows::Forms::ListView^  listViewDMIFields;
private: System::Windows::Forms::ColumnHeader^  columnHeaderNumber;
private: System::Windows::Forms::ColumnHeader^  columnHeaderDesc;
private: System::Windows::Forms::ColumnHeader^  columnHeaderValue;
private: System::Windows::Forms::CheckBox^  checkBoxAutoControl;
private: System::Windows::Forms::Label^  labelAutoControlState;
private: System::Windows::Forms::TextBox^  textBoxAutoControlState;
private: System::Windows::Forms::GroupBox^  groupBoxAutoControlParams;








private: System::Windows::Forms::TextBox^  textBoxBCAMargin;

private: System::Windows::Forms::Label^  labelBCAMargin;





private: System::Windows::Forms::TextBox^  textBoxSpeedLimit2;






private: System::Windows::Forms::Label^  labelSpeedLimit2;





private: System::Windows::Forms::Label^  labelBCAMarginUnit;
private: System::Windows::Forms::TextBox^  textBoxSpeedLimit1;

private: System::Windows::Forms::Label^  labelSpeedLimit2Unit;
private: System::Windows::Forms::Label^  labelSpeedLimit1;



private: System::Windows::Forms::Label^  labelSpeedLimit1Unit;
private: System::Windows::Forms::Button^  buttonSaveAutoControlParams;







private: System::Windows::Forms::Button^  buttonApplyAutoControlParams;








































           //Brake Pressure























           bool ThrottleChanged;

  public:
    LocoSimForm(String^ regKey, String^ fileName, bool runProcesesInternally, OBRDSimDLL::OBRDSimulation^ obrdSim)
    {
      //
      //TODO: Add the constructor code here
      //
      iniFileName = fileName;
      regRootKey = regKey;
      internalProcesses = runProcesesInternally;

      // Get window position from registry
      int left = Convert::ToInt16(Registry::GetValue(regRootKey + "\\LocoSim", "Left", "32767"));
      int top = Convert::ToInt16(Registry::GetValue(regRootKey + "\\LocoSim", "Top", "32767"));
      // If registry not available, first run for example, use default position from windows
      if ((left != 32767) &&
        (top != 32767))
      {
        formLeft = left;
        formTop = top;
      }

      // Create simulation class with parameters
      LocoSimul = gcnew LocoSimulation(iniFileName, internalProcesses, obrdSim);

      // Initialise simulation
      LocoSimul->Init();

      // Initialize components, to be done when params are read!
      InitializeComponent();
      this->DoubleBuffered = true;

      AcceleratedPressed = false;
      BrakePressed = false;
      Cabin1Pressed = false;
      Cabin2Pressed = false;
      LCSReadyPressed = false;
      AOSOffPressed = false;
      EMSActivated = false;
      ATP1ToFatalFailurePressed = false;
      NcuPressed = false;
      Eb1aPressed = false;
      Eb1bPressed = false;
      Eb2aPressed = false;
      Eb2bPressed = false;
      IsolAPressed = false;
      IsolBPressed = false;
      RailMPressed = false;
      RoadMPressed = false;

      manualSimPressed = false;
      automaticSimPressed = false;
      //ebAppliedPressed = false;
      //noBrakeAppliedPressed = false;
      //sbAppliedPressed = false;
      bp1valueEntered = 0U;
      bp2valueEntered = 0U;

      DefaultButtonColor = bAccelerate->BackColor;

      AOSStartRequested = false;
      AOSStopRequested = false;
      outputNames = gcnew array<String^>(RECEIVED_OUTPUT_SIZE);

      oldAOSBuzzer = false;
      ThrottleChanged = false;

      atp1ReqStart = false;
      atpFileName = "";
      atpArgs = "";
      atp2ReqStart = false;
      atp2FileName = "";
      atp2Args = "";
      atoReqStart = false;
      atoFileName = "";
      atoArgs = "";

      tsbAOSStart->Enabled = SimulationSil == LocoSimul->locoParams->SimMode ? true : false;
    }

    /**********************************************************
    * Function: Init
    * Description:
    **********************************************************/
    void Init(void)
    {
      autoSimData->Text = "No Brake";
      autoSimValue->Text = "800";

      // Not used for this form, kept to be similar to LCSSim
    }

    /**********************************************************
    * Function: SaveWindowSettings
    * Description:
    **********************************************************/
    void SaveWindowSettings(void)
    {
      Registry::SetValue(regRootKey + "\\LocoSim", "Left", String::Format("{0:0}", this->Left));
      Registry::SetValue(regRootKey + "\\LocoSim", "Top", String::Format("{0:0}", this->Top));
      Registry::SetValue(regRootKey + "\\LocoSim", "Visible", this->Visible ? "1" : "0");
    }

    /**********************************************************
    * Function: ToggleVisibility
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
    * Function: clearOutputs
    * Description: When starting the ATP connection, the LocoIO
    * outputs receive buffer should be cleared
    **********************************************************/
  public: void clearOutputBuffer(void)
  {
    if (LocoSimul)
      LocoSimul->clearOutputBuffer();
  
  }

  public: bool getEBReq(void)
  {
 
    if (LocoSimul)
      return LocoSimul->getEBReq();
    else
      return false;
  }

  public: bool getSBReq(void)
  {

    if (LocoSimul)
      return LocoSimul->getSBReq();
    else
      return false;
  }
  public: bool getAutoControl(void)
  {
    if (LocoSimul)
      return LocoSimul->locoParams->autoControlEnabled;
    else
      return false;

  }

          /**********************************************************
          * Function: LoadSetup
          * Description:
          **********************************************************/
  public: void LoadSetup(String^ setup) {
    String^ tmpRootKey = regRootKey;
    if (!System::String::IsNullOrEmpty(setup))
    {
      tmpRootKey = regRootKey + "\\" + setup + "\\LocoSim";
    }
    // Check if key exists
    String^ tmp = Convert::ToString(Registry::GetValue(tmpRootKey, "", "empty"));

    // Key exists, i.e. existing setup
    if (!System::String::IsNullOrEmpty(tmp))
    {
      // Get window position from registry
      int left = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Left", "32767"));
      int top = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Top", "32767"));
      bool visible = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Visible", "1")) == 1 ? true : false;
      // If registry not available, first run for example, use default position from windows
      if (left != 32767) formLeft = left;
      if (top != 32767) formTop = top;
      SetVisibility(visible);
    }
  }
          /**********************************************************
          * Function: SaveSetup
          * Description:
          **********************************************************/
  public: void SaveSetup(String^ setup) {
    String^ tmpRootKey = regRootKey;
    if (!System::String::IsNullOrEmpty(setup))
    {
      // Create key
      tmpRootKey = regRootKey + "\\" + setup + "\\LocoSim";

      // Store windows position in registry
      Registry::SetValue(tmpRootKey, "Left", String::Format("{0:0}", this->Left));
      Registry::SetValue(tmpRootKey, "Top", String::Format("{0:0}", this->Top));
      Registry::SetValue(tmpRootKey, "Visible", this->Visible ? "1" : "0");
    }
  }
          /**********************************************************
          * Function: SetSize
          * Description:
          **********************************************************/
  public: void SetSize(int left, int top, int width, int height) {
    this->Left = left;
    this->Top = top;
    //this->Width = width;
    //this->Height = height;
    formLeft = left;
    formTop = top;
    //formWidth = width;
    //formHeight = height;
  }


  protected:
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    ~LocoSimForm()
    {
      if (components)
      {
        delete components;
      }
    }

  protected:

















































































































  private: System::Windows::Forms::ToolStrip^ toolStrip1;
  private: System::Windows::Forms::ToolStripButton^ tsbAOSStart;
  private: System::Windows::Forms::ToolStripButton^ tsbAOSStop;
  private: System::Windows::Forms::ToolStripSeparator^ toolStripSeparator5;
  private: System::Windows::Forms::ToolStripLabel^ tslSelectedController;
  private: System::Windows::Forms::ToolStripLabel^ toolStripLabel1;
  private: System::Windows::Forms::ToolStrip^ toolStrip2;
  private: System::Windows::Forms::ToolStripButton^ tsbATOGreen;
  private: System::Windows::Forms::ToolStripButton^ tsbATORed;
  private: System::Windows::Forms::ToolStripSeparator^ toolStripSeparator3;
  private: System::Windows::Forms::ToolStripSeparator^ toolStripSeparator2;
  private: System::Windows::Forms::ToolStripButton^ tsbATP1Green;
  private: System::Windows::Forms::ToolStripButton^ tsbATP1Red;
  private: System::Windows::Forms::ToolStripSeparator^ toolStripSeparator4;
  private: System::Windows::Forms::ToolStripButton^ tsbLCSRed;
  private: System::Windows::Forms::ToolStripButton^ tsbLCSGreen;
  private: System::Windows::Forms::ToolStripSeparator^ toolStripSeparator6;
  private: System::Windows::Forms::ToolStripButton^ tsbRDGreen;
  private: System::Windows::Forms::ToolStripSeparator^ tssRD;
  private: System::Windows::Forms::ToolStripLabel^ tslSimState;
  private: System::Windows::Forms::ToolStripSeparator^ tssAutoRegSep;
  private: System::Windows::Forms::ToolStripLabel^ tslAutoRegState;

  private: System::ComponentModel::IContainer^ components;


















  protected:

  private:
    /// <summary>
    /// Required designer variable.
    /// </summary>

#pragma region Windows Form Designer generated code
/// <summary>
/// Required method for Designer support - do not modify
/// the contents of this method with the code editor.
/// </summary>
    void InitializeComponent(void)
    {
      System::ComponentModel::ComponentResourceManager^  resources = (gcnew System::ComponentModel::ComponentResourceManager(LocoSimForm::typeid));
      System::Windows::Forms::ListViewItem^  listViewItem1 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"1",
          L"ATP Mode", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem2 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"2",
          L"Config mode substate", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem3 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"3",
          L"Driver verification state", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem4 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"4",
          L"Interface flags", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem5 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"5",
          L"Core Train status", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem6 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"6",
          L"ATO Mode", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem7 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"7",
          L"Additional status bits", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem8 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"8",
          L"Confirm change to mode needed", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem9 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"9",
          L"Allowed to", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem10 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"10",
          L"Brake Test status", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem11 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"11",
          L"Additional Allowed to", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem12 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"12",
          L"Additional Confirm ", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem13 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"13",
          L"Adaptation Train status", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem14 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"14",
          L"Permitted driving direction", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem15 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"15",
          L"Permitted speed", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem16 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"16",
          L"Target speed", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem17 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"17",
          L"Time to intervention", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem18 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"18",
          L"Remaining distance to target", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem19 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"19",
          L"Remaining distance to BCA", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem20 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"20",
          L"Brake related indications", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem21 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"21",
          L"Supervision related indications", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem22 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"22",
          L"MA Margin", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem23 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"23",
          L"Current speed", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem24 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"24",
          L"Leading track", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem25 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"25",
          L"Leading pos", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem26 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"26",
          L"Trailing track", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem27 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"27",
          L"Trailing pos", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem28 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"28",
          L"Train length", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem29 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"29",
          L"Track gradient", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem30 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"30",
          L"Effective gradient", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem31 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"31",
          L"Brakeability", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem32 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"32",
          L"Brake delay, EB", L""
      }, -1));
      System::Windows::Forms::ListViewItem^  listViewItem33 = (gcnew System::Windows::Forms::ListViewItem(gcnew cli::array< System::String^  >(3) {
        L"33",
          L"Brake delay, SB", L""
      }, -1));
      this->toolStrip1 = (gcnew System::Windows::Forms::ToolStrip());
      this->tsbAOSStart = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbAOSStop = (gcnew System::Windows::Forms::ToolStripButton());
      this->toolStripSeparator5 = (gcnew System::Windows::Forms::ToolStripSeparator());
      this->tslSelectedController = (gcnew System::Windows::Forms::ToolStripLabel());
      this->toolStripLabel1 = (gcnew System::Windows::Forms::ToolStripLabel());
      this->toolStrip2 = (gcnew System::Windows::Forms::ToolStrip());
      this->tsbATOGreen = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbATORed = (gcnew System::Windows::Forms::ToolStripButton());
      this->toolStripSeparator3 = (gcnew System::Windows::Forms::ToolStripSeparator());
      this->toolStripSeparator2 = (gcnew System::Windows::Forms::ToolStripSeparator());
      this->tsbATP1Green = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbATP1Red = (gcnew System::Windows::Forms::ToolStripButton());
      this->toolStripSeparator4 = (gcnew System::Windows::Forms::ToolStripSeparator());
      this->tsbLCSRed = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbLCSGreen = (gcnew System::Windows::Forms::ToolStripButton());
      this->toolStripSeparator6 = (gcnew System::Windows::Forms::ToolStripSeparator());
      this->tsbRDGreen = (gcnew System::Windows::Forms::ToolStripButton());
      this->tssRD = (gcnew System::Windows::Forms::ToolStripSeparator());
      this->tslSimState = (gcnew System::Windows::Forms::ToolStripLabel());
      this->tssAutoRegSep = (gcnew System::Windows::Forms::ToolStripSeparator());
      this->tslAutoRegState = (gcnew System::Windows::Forms::ToolStripLabel());
      this->panel1 = (gcnew System::Windows::Forms::Panel());
      this->brakePressure = (gcnew System::Windows::Forms::TabPage());
      this->groupBox21 = (gcnew System::Windows::Forms::GroupBox());
      this->label4 = (gcnew System::Windows::Forms::Label());
      this->rbTcoOrderAndFb = (gcnew System::Windows::Forms::RadioButton());
      this->tbTcoFbOffset = (gcnew System::Windows::Forms::TextBox());
      this->label5 = (gcnew System::Windows::Forms::Label());
      this->rbTcoOnlyFb = (gcnew System::Windows::Forms::RadioButton());
      this->rbTcoNoFb = (gcnew System::Windows::Forms::RadioButton());
      this->groupBox20 = (gcnew System::Windows::Forms::GroupBox());
      this->manualSim = (gcnew System::Windows::Forms::CheckBox());
      this->lbBrakePressureSensor1 = (gcnew System::Windows::Forms::Label());
      this->bp2Entered = (gcnew System::Windows::Forms::TextBox());
      this->label21 = (gcnew System::Windows::Forms::Label());
      this->lbBrakePressureSensor2 = (gcnew System::Windows::Forms::Label());
      this->bp1Entered = (gcnew System::Windows::Forms::TextBox());
      this->label9 = (gcnew System::Windows::Forms::Label());
      this->groupBox19 = (gcnew System::Windows::Forms::GroupBox());
      this->label25 = (gcnew System::Windows::Forms::Label());
      this->label24 = (gcnew System::Windows::Forms::Label());
      this->autoSimData = (gcnew System::Windows::Forms::TextBox());
      this->autoSimValue = (gcnew System::Windows::Forms::TextBox());
      this->label20 = (gcnew System::Windows::Forms::Label());
      this->label19 = (gcnew System::Windows::Forms::Label());
      this->tabRailDevIF = (gcnew System::Windows::Forms::TabPage());
      this->groupBox11 = (gcnew System::Windows::Forms::GroupBox());
      this->label26 = (gcnew System::Windows::Forms::Label());
      this->lRDSndStr = (gcnew System::Windows::Forms::Label());
      this->lRDRecStr = (gcnew System::Windows::Forms::Label());
      this->label29 = (gcnew System::Windows::Forms::Label());
      this->label30 = (gcnew System::Windows::Forms::Label());
      this->lRDSndTime = (gcnew System::Windows::Forms::Label());
      this->label32 = (gcnew System::Windows::Forms::Label());
      this->lRDRecTime = (gcnew System::Windows::Forms::Label());
      this->label34 = (gcnew System::Windows::Forms::Label());
      this->lRDConnStatus = (gcnew System::Windows::Forms::Label());
      this->tabPage5 = (gcnew System::Windows::Forms::TabPage());
      this->panel3 = (gcnew System::Windows::Forms::Panel());
      this->panel4 = (gcnew System::Windows::Forms::Panel());
      this->groupBox8 = (gcnew System::Windows::Forms::GroupBox());
      this->bSelectBaliseSimFile = (gcnew System::Windows::Forms::Button());
      this->tbBaliseSimFileName = (gcnew System::Windows::Forms::TextBox());
      this->label23 = (gcnew System::Windows::Forms::Label());
      this->cbUseBaliseSimulationFile = (gcnew System::Windows::Forms::CheckBox());
      this->groupBox17 = (gcnew System::Windows::Forms::GroupBox());
      this->label17 = (gcnew System::Windows::Forms::Label());
      this->tbRegBaliseDefault = (gcnew System::Windows::Forms::TextBox());
      this->gbArguments = (gcnew System::Windows::Forms::GroupBox());
      this->ATOArgBox = (gcnew System::Windows::Forms::TextBox());
      this->label49 = (gcnew System::Windows::Forms::Label());
      this->ATP2ArgBox = (gcnew System::Windows::Forms::TextBox());
      this->label48 = (gcnew System::Windows::Forms::Label());
      this->ATP1ArgBox = (gcnew System::Windows::Forms::TextBox());
      this->label47 = (gcnew System::Windows::Forms::Label());
      this->groupBox12 = (gcnew System::Windows::Forms::GroupBox());
      this->tbSBRet = (gcnew System::Windows::Forms::TextBox());
      this->label39 = (gcnew System::Windows::Forms::Label());
      this->tbEBRet = (gcnew System::Windows::Forms::TextBox());
      this->label37 = (gcnew System::Windows::Forms::Label());
      this->tbMaxRet = (gcnew System::Windows::Forms::TextBox());
      this->label35 = (gcnew System::Windows::Forms::Label());
      this->label38 = (gcnew System::Windows::Forms::Label());
      this->tbMaxAcc = (gcnew System::Windows::Forms::TextBox());
      this->label36 = (gcnew System::Windows::Forms::Label());
      this->label31 = (gcnew System::Windows::Forms::Label());
      this->label33 = (gcnew System::Windows::Forms::Label());
      this->tbMaxSpeed = (gcnew System::Windows::Forms::TextBox());
      this->label28 = (gcnew System::Windows::Forms::Label());
      this->label27 = (gcnew System::Windows::Forms::Label());
      this->label40 = (gcnew System::Windows::Forms::Label());
      this->groupBox13 = (gcnew System::Windows::Forms::GroupBox());
      this->cbEnableSound = (gcnew System::Windows::Forms::CheckBox());
      this->gbAutoReg = (gcnew System::Windows::Forms::GroupBox());
      this->cbAutoRegATOMode = (gcnew System::Windows::Forms::ComboBox());
      this->cbAutoRegDirection = (gcnew System::Windows::Forms::ComboBox());
      this->cbAutoRegEnable = (gcnew System::Windows::Forms::CheckBox());
      this->label46 = (gcnew System::Windows::Forms::Label());
      this->label44 = (gcnew System::Windows::Forms::Label());
      this->label45 = (gcnew System::Windows::Forms::Label());
      this->label42 = (gcnew System::Windows::Forms::Label());
      this->label43 = (gcnew System::Windows::Forms::Label());
      this->label41 = (gcnew System::Windows::Forms::Label());
      this->tbAutoRegSpeed = (gcnew System::Windows::Forms::TextBox());
      this->tbAutoRegAcc = (gcnew System::Windows::Forms::TextBox());
      this->bSaveParams = (gcnew System::Windows::Forms::Button());
      this->bApplyParams = (gcnew System::Windows::Forms::Button());
      this->tabPage2 = (gcnew System::Windows::Forms::TabPage());
      this->groupBox9 = (gcnew System::Windows::Forms::GroupBox());
      this->pbATOStart = (gcnew System::Windows::Forms::PictureBox());
      this->pbATP2Start = (gcnew System::Windows::Forms::PictureBox());
      this->pbATP1Start = (gcnew System::Windows::Forms::PictureBox());
      this->lATOState = (gcnew System::Windows::Forms::Label());
      this->label12 = (gcnew System::Windows::Forms::Label());
      this->lATP2State = (gcnew System::Windows::Forms::Label());
      this->label11 = (gcnew System::Windows::Forms::Label());
      this->lATP1State = (gcnew System::Windows::Forms::Label());
      this->label10 = (gcnew System::Windows::Forms::Label());
      this->bATP1ToFF = (gcnew System::Windows::Forms::Button());
      this->groupBox14 = (gcnew System::Windows::Forms::GroupBox());
      this->groupBox16 = (gcnew System::Windows::Forms::GroupBox());
      this->label15 = (gcnew System::Windows::Forms::Label());
      this->tbRegUseBalise = (gcnew System::Windows::Forms::TextBox());
      this->groupBox15 = (gcnew System::Windows::Forms::GroupBox());
      this->rbReRegLastBalise = (gcnew System::Windows::Forms::RadioButton());
      this->tbReRegSpecBalise = (gcnew System::Windows::Forms::TextBox());
      this->rbReRegSpecBalise = (gcnew System::Windows::Forms::RadioButton());
      this->tbReRegLastBalise = (gcnew System::Windows::Forms::TextBox());
      this->tabPage1 = (gcnew System::Windows::Forms::TabPage());
      this->groupBox18 = (gcnew System::Windows::Forms::GroupBox());
      this->bIsolB = (gcnew System::Windows::Forms::Button());
      this->bEb1a = (gcnew System::Windows::Forms::Button());
      this->bIsolA = (gcnew System::Windows::Forms::Button());
      this->bEb2b = (gcnew System::Windows::Forms::Button());
      this->bEb2a = (gcnew System::Windows::Forms::Button());
      this->bEb1b = (gcnew System::Windows::Forms::Button());
      this->groupBox7 = (gcnew System::Windows::Forms::GroupBox());
      this->bRoadM = (gcnew System::Windows::Forms::Button());
      this->bRailM = (gcnew System::Windows::Forms::Button());
      this->bNcu = (gcnew System::Windows::Forms::Button());
      this->bCabin2 = (gcnew System::Windows::Forms::Button());
      this->bEMSAlert = (gcnew System::Windows::Forms::Button());
      this->bAOSOff = (gcnew System::Windows::Forms::Button());
      this->bCabin1 = (gcnew System::Windows::Forms::Button());
      this->bLCSReady = (gcnew System::Windows::Forms::Button());
      this->groupBox6 = (gcnew System::Windows::Forms::GroupBox());
      this->label8 = (gcnew System::Windows::Forms::Label());
      this->clbOutputs = (gcnew System::Windows::Forms::CheckedListBox());
      this->groupBox5 = (gcnew System::Windows::Forms::GroupBox());
      this->rbAutomatic = (gcnew System::Windows::Forms::RadioButton());
      this->rbSupervised = (gcnew System::Windows::Forms::RadioButton());
      this->rbManual = (gcnew System::Windows::Forms::RadioButton());
      this->groupBox4 = (gcnew System::Windows::Forms::GroupBox());
      this->rbReverse = (gcnew System::Windows::Forms::RadioButton());
      this->rbNeutral = (gcnew System::Windows::Forms::RadioButton());
      this->rbForward = (gcnew System::Windows::Forms::RadioButton());
      this->groupBox3 = (gcnew System::Windows::Forms::GroupBox());
      this->lAcc = (gcnew System::Windows::Forms::Label());
      this->label7 = (gcnew System::Windows::Forms::Label());
      this->groupBox2 = (gcnew System::Windows::Forms::GroupBox());
      this->lSpeed = (gcnew System::Windows::Forms::Label());
      this->label6 = (gcnew System::Windows::Forms::Label());
      this->groupBox1 = (gcnew System::Windows::Forms::GroupBox());
      this->bBrake = (gcnew System::Windows::Forms::Button());
      this->bAccelerate = (gcnew System::Windows::Forms::Button());
      this->bCoast = (gcnew System::Windows::Forms::Button());
      this->label3 = (gcnew System::Windows::Forms::Label());
      this->label2 = (gcnew System::Windows::Forms::Label());
      this->label1 = (gcnew System::Windows::Forms::Label());
      this->bThrottle = (gcnew System::Windows::Forms::TrackBar());
      this->tabMainControl = (gcnew System::Windows::Forms::TabControl());
      this->tabPageAutoControl = (gcnew System::Windows::Forms::TabPage());
      this->buttonSaveAutoControlParams = (gcnew System::Windows::Forms::Button());
      this->buttonApplyAutoControlParams = (gcnew System::Windows::Forms::Button());
      this->groupBoxAutoControlParams = (gcnew System::Windows::Forms::GroupBox());
      this->textBoxBCAMargin = (gcnew System::Windows::Forms::TextBox());
      this->labelBCAMargin = (gcnew System::Windows::Forms::Label());
      this->textBoxSpeedLimit2 = (gcnew System::Windows::Forms::TextBox());
      this->labelSpeedLimit2 = (gcnew System::Windows::Forms::Label());
      this->labelBCAMarginUnit = (gcnew System::Windows::Forms::Label());
      this->textBoxSpeedLimit1 = (gcnew System::Windows::Forms::TextBox());
      this->labelSpeedLimit2Unit = (gcnew System::Windows::Forms::Label());
      this->labelSpeedLimit1 = (gcnew System::Windows::Forms::Label());
      this->labelSpeedLimit1Unit = (gcnew System::Windows::Forms::Label());
      this->labelAutoControlState = (gcnew System::Windows::Forms::Label());
      this->textBoxAutoControlState = (gcnew System::Windows::Forms::TextBox());
      this->checkBoxAutoControl = (gcnew System::Windows::Forms::CheckBox());
      this->tabPageDMIInterface = (gcnew System::Windows::Forms::TabPage());
      this->listViewDMIFields = (gcnew System::Windows::Forms::ListView());
      this->columnHeaderNumber = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeaderDesc = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeaderValue = (gcnew System::Windows::Forms::ColumnHeader());
      this->toolStrip1->SuspendLayout();
      this->toolStrip2->SuspendLayout();
      this->brakePressure->SuspendLayout();
      this->groupBox21->SuspendLayout();
      this->groupBox20->SuspendLayout();
      this->groupBox19->SuspendLayout();
      this->tabRailDevIF->SuspendLayout();
      this->groupBox11->SuspendLayout();
      this->tabPage5->SuspendLayout();
      this->panel3->SuspendLayout();
      this->panel4->SuspendLayout();
      this->groupBox8->SuspendLayout();
      this->groupBox17->SuspendLayout();
      this->gbArguments->SuspendLayout();
      this->groupBox12->SuspendLayout();
      this->groupBox13->SuspendLayout();
      this->gbAutoReg->SuspendLayout();
      this->tabPage2->SuspendLayout();
      this->groupBox9->SuspendLayout();
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->pbATOStart))->BeginInit();
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->pbATP2Start))->BeginInit();
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->pbATP1Start))->BeginInit();
      this->groupBox14->SuspendLayout();
      this->groupBox16->SuspendLayout();
      this->groupBox15->SuspendLayout();
      this->tabPage1->SuspendLayout();
      this->groupBox18->SuspendLayout();
      this->groupBox7->SuspendLayout();
      this->groupBox6->SuspendLayout();
      this->groupBox5->SuspendLayout();
      this->groupBox4->SuspendLayout();
      this->groupBox3->SuspendLayout();
      this->groupBox2->SuspendLayout();
      this->groupBox1->SuspendLayout();
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->bThrottle))->BeginInit();
      this->tabMainControl->SuspendLayout();
      this->tabPageAutoControl->SuspendLayout();
      this->groupBoxAutoControlParams->SuspendLayout();
      this->tabPageDMIInterface->SuspendLayout();
      this->SuspendLayout();
      // 
      // toolStrip1
      // 
      this->toolStrip1->GripStyle = System::Windows::Forms::ToolStripGripStyle::Hidden;
      this->toolStrip1->Items->AddRange(gcnew cli::array< System::Windows::Forms::ToolStripItem^  >(5) {
        this->tsbAOSStart, this->tsbAOSStop,
          this->toolStripSeparator5, this->tslSelectedController, this->toolStripLabel1
      });
      this->toolStrip1->Location = System::Drawing::Point(0, 0);
      this->toolStrip1->Name = L"toolStrip1";
      this->toolStrip1->Size = System::Drawing::Size(446, 25);
      this->toolStrip1->TabIndex = 2;
      this->toolStrip1->Text = L"toolStrip1";
      // 
      // tsbAOSStart
      // 
      this->tsbAOSStart->DisplayStyle = System::Windows::Forms::ToolStripItemDisplayStyle::Image;
      this->tsbAOSStart->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbAOSStart.Image")));
      this->tsbAOSStart->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbAOSStart->Name = L"tsbAOSStart";
      this->tsbAOSStart->Size = System::Drawing::Size(23, 22);
      this->tsbAOSStart->Text = L"toolStripButton1";
      this->tsbAOSStart->ToolTipText = L"Start AOS simulation";
      this->tsbAOSStart->Click += gcnew System::EventHandler(this, &LocoSimForm::tsbAOSStart_Click);
      // 
      // tsbAOSStop
      // 
      this->tsbAOSStop->DisplayStyle = System::Windows::Forms::ToolStripItemDisplayStyle::Image;
      this->tsbAOSStop->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbAOSStop.Image")));
      this->tsbAOSStop->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbAOSStop->Name = L"tsbAOSStop";
      this->tsbAOSStop->Size = System::Drawing::Size(23, 22);
      this->tsbAOSStop->Text = L"toolStripButton2";
      this->tsbAOSStop->ToolTipText = L"Stop AOS simulation (Check exe files)";
      this->tsbAOSStop->Visible = false;
      this->tsbAOSStop->Click += gcnew System::EventHandler(this, &LocoSimForm::tsbAOSStop_Click);
      // 
      // toolStripSeparator5
      // 
      this->toolStripSeparator5->Name = L"toolStripSeparator5";
      this->toolStripSeparator5->Size = System::Drawing::Size(6, 25);
      // 
      // tslSelectedController
      // 
      this->tslSelectedController->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tslSelectedController->Name = L"tslSelectedController";
      this->tslSelectedController->Size = System::Drawing::Size(53, 22);
      this->tslSelectedController->Text = L"LocoSim";
      // 
      // toolStripLabel1
      // 
      this->toolStripLabel1->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->toolStripLabel1->Name = L"toolStripLabel1";
      this->toolStripLabel1->Size = System::Drawing::Size(50, 22);
      this->toolStripLabel1->Text = L"Control:";
      // 
      // toolStrip2
      // 
      this->toolStrip2->Dock = System::Windows::Forms::DockStyle::Bottom;
      this->toolStrip2->GripStyle = System::Windows::Forms::ToolStripGripStyle::Hidden;
      this->toolStrip2->Items->AddRange(gcnew cli::array< System::Windows::Forms::ToolStripItem^  >(15) {
        this->tsbATOGreen, this->tsbATORed,
          this->toolStripSeparator3, this->toolStripSeparator2, this->tsbATP1Green, this->tsbATP1Red, this->toolStripSeparator4, this->tsbLCSRed,
          this->tsbLCSGreen, this->toolStripSeparator6, this->tsbRDGreen, this->tssRD, this->tslSimState, this->tssAutoRegSep, this->tslAutoRegState
      });
      this->toolStrip2->Location = System::Drawing::Point(0, 505);
      this->toolStrip2->Name = L"toolStrip2";
      this->toolStrip2->RenderMode = System::Windows::Forms::ToolStripRenderMode::System;
      this->toolStrip2->Size = System::Drawing::Size(446, 25);
      this->toolStrip2->TabIndex = 3;
      this->toolStrip2->Text = L"toolStrip2";
      // 
      // tsbATOGreen
      // 
      this->tsbATOGreen->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbATOGreen->AutoToolTip = false;
      this->tsbATOGreen->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbATOGreen.Image")));
      this->tsbATOGreen->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbATOGreen->Name = L"tsbATOGreen";
      this->tsbATOGreen->Size = System::Drawing::Size(51, 22);
      this->tsbATOGreen->Text = L"ATO";
      this->tsbATOGreen->Visible = false;
      // 
      // tsbATORed
      // 
      this->tsbATORed->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbATORed->AutoToolTip = false;
      this->tsbATORed->BackColor = System::Drawing::SystemColors::Control;
      this->tsbATORed->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbATORed.Image")));
      this->tsbATORed->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbATORed->Name = L"tsbATORed";
      this->tsbATORed->Size = System::Drawing::Size(51, 22);
      this->tsbATORed->Text = L"ATO";
      // 
      // toolStripSeparator3
      // 
      this->toolStripSeparator3->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->toolStripSeparator3->Name = L"toolStripSeparator3";
      this->toolStripSeparator3->Size = System::Drawing::Size(6, 25);
      // 
      // toolStripSeparator2
      // 
      this->toolStripSeparator2->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->toolStripSeparator2->Name = L"toolStripSeparator2";
      this->toolStripSeparator2->Size = System::Drawing::Size(6, 25);
      // 
      // tsbATP1Green
      // 
      this->tsbATP1Green->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbATP1Green->AutoToolTip = false;
      this->tsbATP1Green->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbATP1Green.Image")));
      this->tsbATP1Green->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbATP1Green->Name = L"tsbATP1Green";
      this->tsbATP1Green->Size = System::Drawing::Size(49, 22);
      this->tsbATP1Green->Text = L"ATP";
      this->tsbATP1Green->Visible = false;
      // 
      // tsbATP1Red
      // 
      this->tsbATP1Red->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbATP1Red->AutoToolTip = false;
      this->tsbATP1Red->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbATP1Red.Image")));
      this->tsbATP1Red->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbATP1Red->Name = L"tsbATP1Red";
      this->tsbATP1Red->Size = System::Drawing::Size(49, 22);
      this->tsbATP1Red->Text = L"ATP";
      this->tsbATP1Red->Click += gcnew System::EventHandler(this, &LocoSimForm::tsbATP1Red_Click);
      // 
      // toolStripSeparator4
      // 
      this->toolStripSeparator4->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->toolStripSeparator4->Name = L"toolStripSeparator4";
      this->toolStripSeparator4->Size = System::Drawing::Size(6, 25);
      // 
      // tsbLCSRed
      // 
      this->tsbLCSRed->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbLCSRed->AutoToolTip = false;
      this->tsbLCSRed->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbLCSRed.Image")));
      this->tsbLCSRed->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbLCSRed->Name = L"tsbLCSRed";
      this->tsbLCSRed->Size = System::Drawing::Size(47, 22);
      this->tsbLCSRed->Text = L"LCS";
      this->tsbLCSRed->Visible = false;
      // 
      // tsbLCSGreen
      // 
      this->tsbLCSGreen->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbLCSGreen->AutoToolTip = false;
      this->tsbLCSGreen->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbLCSGreen.Image")));
      this->tsbLCSGreen->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbLCSGreen->Name = L"tsbLCSGreen";
      this->tsbLCSGreen->Size = System::Drawing::Size(47, 22);
      this->tsbLCSGreen->Text = L"LCS";
      this->tsbLCSGreen->Visible = false;
      // 
      // toolStripSeparator6
      // 
      this->toolStripSeparator6->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->toolStripSeparator6->Name = L"toolStripSeparator6";
      this->toolStripSeparator6->Size = System::Drawing::Size(6, 25);
      // 
      // tsbRDGreen
      // 
      this->tsbRDGreen->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbRDGreen->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbRDGreen.Image")));
      this->tsbRDGreen->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbRDGreen->Name = L"tsbRDGreen";
      this->tsbRDGreen->Size = System::Drawing::Size(42, 22);
      this->tsbRDGreen->Text = L"RD";
      this->tsbRDGreen->Visible = false;
      // 
      // tssRD
      // 
      this->tssRD->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tssRD->Name = L"tssRD";
      this->tssRD->Size = System::Drawing::Size(6, 25);
      this->tssRD->Visible = false;
      // 
      // tslSimState
      // 
      this->tslSimState->Name = L"tslSimState";
      this->tslSimState->Size = System::Drawing::Size(90, 22);
      this->tslSimState->Text = L"PendingStartUp";
      // 
      // tssAutoRegSep
      // 
      this->tssAutoRegSep->Name = L"tssAutoRegSep";
      this->tssAutoRegSep->Size = System::Drawing::Size(6, 25);
      this->tssAutoRegSep->Visible = false;
      // 
      // tslAutoRegState
      // 
      this->tslAutoRegState->Name = L"tslAutoRegState";
      this->tslAutoRegState->Size = System::Drawing::Size(86, 22);
      this->tslAutoRegState->Text = L"toolStripLabel2";
      this->tslAutoRegState->Visible = false;
      this->tslAutoRegState->Click += gcnew System::EventHandler(this, &LocoSimForm::tslAutoRegState_Click);
      // 
      // panel1
      // 
      this->panel1->BorderStyle = System::Windows::Forms::BorderStyle::Fixed3D;
      this->panel1->Location = System::Drawing::Point(0, 492);
      this->panel1->Name = L"panel1";
      this->panel1->Size = System::Drawing::Size(454, 10);
      this->panel1->TabIndex = 3;
      // 
      // brakePressure
      // 
      this->brakePressure->Controls->Add(this->groupBox21);
      this->brakePressure->Controls->Add(this->groupBox20);
      this->brakePressure->Controls->Add(this->groupBox19);
      this->brakePressure->Location = System::Drawing::Point(4, 22);
      this->brakePressure->Name = L"brakePressure";
      this->brakePressure->Padding = System::Windows::Forms::Padding(3);
      this->brakePressure->Size = System::Drawing::Size(426, 428);
      this->brakePressure->TabIndex = 5;
      this->brakePressure->Text = L"Brake Pressure";
      this->brakePressure->UseVisualStyleBackColor = true;
      // 
      // groupBox21
      // 
      this->groupBox21->Controls->Add(this->label4);
      this->groupBox21->Controls->Add(this->rbTcoOrderAndFb);
      this->groupBox21->Controls->Add(this->tbTcoFbOffset);
      this->groupBox21->Controls->Add(this->label5);
      this->groupBox21->Controls->Add(this->rbTcoOnlyFb);
      this->groupBox21->Controls->Add(this->rbTcoNoFb);
      this->groupBox21->Location = System::Drawing::Point(6, 264);
      this->groupBox21->Name = L"groupBox21";
      this->groupBox21->Size = System::Drawing::Size(379, 98);
      this->groupBox21->TabIndex = 8;
      this->groupBox21->TabStop = false;
      this->groupBox21->Text = L"TCO Feedback";
      // 
      // label4
      // 
      this->label4->AutoSize = true;
      this->label4->Location = System::Drawing::Point(333, 68);
      this->label4->Name = L"label4";
      this->label4->Size = System::Drawing::Size(43, 13);
      this->label4->TabIndex = 15;
      this->label4->Text = L"x100ms";
      // 
      // rbTcoOrderAndFb
      // 
      this->rbTcoOrderAndFb->AutoSize = true;
      this->rbTcoOrderAndFb->Location = System::Drawing::Point(7, 66);
      this->rbTcoOrderAndFb->Name = L"rbTcoOrderAndFb";
      this->rbTcoOrderAndFb->Size = System::Drawing::Size(109, 17);
      this->rbTcoOrderAndFb->TabIndex = 3;
      this->rbTcoOrderAndFb->TabStop = true;
      this->rbTcoOrderAndFb->Text = L"Follow TCO Order";
      this->rbTcoOrderAndFb->UseVisualStyleBackColor = true;
      // 
      // tbTcoFbOffset
      // 
      this->tbTcoFbOffset->Location = System::Drawing::Point(281, 65);
      this->tbTcoFbOffset->Name = L"tbTcoFbOffset";
      this->tbTcoFbOffset->Size = System::Drawing::Size(50, 20);
      this->tbTcoFbOffset->TabIndex = 14;
      this->tbTcoFbOffset->Text = L"1";
      this->tbTcoFbOffset->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbTcoFbOffset->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::textBox1_TextChanged);
      // 
      // label5
      // 
      this->label5->AutoSize = true;
      this->label5->Location = System::Drawing::Point(171, 68);
      this->label5->Name = L"label5";
      this->label5->Size = System::Drawing::Size(112, 13);
      this->label5->TabIndex = 13;
      this->label5->Text = L"Feedback TimeOffset:";
      // 
      // rbTcoOnlyFb
      // 
      this->rbTcoOnlyFb->AutoSize = true;
      this->rbTcoOnlyFb->Checked = true;
      this->rbTcoOnlyFb->Location = System::Drawing::Point(7, 43);
      this->rbTcoOnlyFb->Name = L"rbTcoOnlyFb";
      this->rbTcoOnlyFb->Size = System::Drawing::Size(142, 17);
      this->rbTcoOnlyFb->TabIndex = 2;
      this->rbTcoOnlyFb->TabStop = true;
      this->rbTcoOnlyFb->Text = L"Follow Emergency Brake";
      this->rbTcoOnlyFb->UseVisualStyleBackColor = true;
      // 
      // rbTcoNoFb
      // 
      this->rbTcoNoFb->AutoSize = true;
      this->rbTcoNoFb->Location = System::Drawing::Point(7, 20);
      this->rbTcoNoFb->Name = L"rbTcoNoFb";
      this->rbTcoNoFb->Size = System::Drawing::Size(90, 17);
      this->rbTcoNoFb->TabIndex = 1;
      this->rbTcoNoFb->Text = L"No Feedback";
      this->rbTcoNoFb->UseVisualStyleBackColor = true;
      this->rbTcoNoFb->CheckedChanged += gcnew System::EventHandler(this, &LocoSimForm::tcoNoFb_CheckedChanged);
      // 
      // groupBox20
      // 
      this->groupBox20->Controls->Add(this->manualSim);
      this->groupBox20->Controls->Add(this->lbBrakePressureSensor1);
      this->groupBox20->Controls->Add(this->bp2Entered);
      this->groupBox20->Controls->Add(this->label21);
      this->groupBox20->Controls->Add(this->lbBrakePressureSensor2);
      this->groupBox20->Controls->Add(this->bp1Entered);
      this->groupBox20->Controls->Add(this->label9);
      this->groupBox20->Location = System::Drawing::Point(6, 143);
      this->groupBox20->Name = L"groupBox20";
      this->groupBox20->Size = System::Drawing::Size(379, 115);
      this->groupBox20->TabIndex = 6;
      this->groupBox20->TabStop = false;
      // 
      // manualSim
      // 
      this->manualSim->AutoSize = true;
      this->manualSim->Location = System::Drawing::Point(9, 0);
      this->manualSim->Name = L"manualSim";
      this->manualSim->Size = System::Drawing::Size(112, 17);
      this->manualSim->TabIndex = 8;
      this->manualSim->Text = L"Manual Simulation";
      this->manualSim->UseVisualStyleBackColor = true;
      // 
      // lbBrakePressureSensor1
      // 
      this->lbBrakePressureSensor1->AutoSize = true;
      this->lbBrakePressureSensor1->Location = System::Drawing::Point(209, 85);
      this->lbBrakePressureSensor1->Name = L"lbBrakePressureSensor1";
      this->lbBrakePressureSensor1->Size = System::Drawing::Size(94, 13);
      this->lbBrakePressureSensor1->TabIndex = 7;
      this->lbBrakePressureSensor1->Text = L"kPa (Range: \? - \?)";
      // 
      // bp2Entered
      // 
      this->bp2Entered->Location = System::Drawing::Point(153, 82);
      this->bp2Entered->Name = L"bp2Entered";
      this->bp2Entered->Size = System::Drawing::Size(50, 20);
      this->bp2Entered->TabIndex = 6;
      this->bp2Entered->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::bp2Entered_TextChanged);
      // 
      // label21
      // 
      this->label21->AutoSize = true;
      this->label21->Location = System::Drawing::Point(6, 85);
      this->label21->Name = L"label21";
      this->label21->Size = System::Drawing::Size(127, 13);
      this->label21->TabIndex = 5;
      this->label21->Text = L"Brake Pressure Sensor 2:";
      // 
      // lbBrakePressureSensor2
      // 
      this->lbBrakePressureSensor2->AutoSize = true;
      this->lbBrakePressureSensor2->Location = System::Drawing::Point(209, 40);
      this->lbBrakePressureSensor2->Name = L"lbBrakePressureSensor2";
      this->lbBrakePressureSensor2->Size = System::Drawing::Size(94, 13);
      this->lbBrakePressureSensor2->TabIndex = 4;
      this->lbBrakePressureSensor2->Text = L"kPa (Range: \? - \?)";
      // 
      // bp1Entered
      // 
      this->bp1Entered->Location = System::Drawing::Point(153, 37);
      this->bp1Entered->Name = L"bp1Entered";
      this->bp1Entered->Size = System::Drawing::Size(50, 20);
      this->bp1Entered->TabIndex = 3;
      this->bp1Entered->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::bp1Entered_TextChanged);
      // 
      // label9
      // 
      this->label9->AutoSize = true;
      this->label9->Location = System::Drawing::Point(6, 40);
      this->label9->Name = L"label9";
      this->label9->Size = System::Drawing::Size(127, 13);
      this->label9->TabIndex = 2;
      this->label9->Text = L"Brake Pressure Sensor 1:";
      this->label9->Click += gcnew System::EventHandler(this, &LocoSimForm::label9_Click);
      // 
      // groupBox19
      // 
      this->groupBox19->Controls->Add(this->label25);
      this->groupBox19->Controls->Add(this->label24);
      this->groupBox19->Controls->Add(this->autoSimData);
      this->groupBox19->Controls->Add(this->autoSimValue);
      this->groupBox19->Controls->Add(this->label20);
      this->groupBox19->Controls->Add(this->label19);
      this->groupBox19->Location = System::Drawing::Point(6, 17);
      this->groupBox19->Name = L"groupBox19";
      this->groupBox19->Size = System::Drawing::Size(379, 120);
      this->groupBox19->TabIndex = 5;
      this->groupBox19->TabStop = false;
      // 
      // label25
      // 
      this->label25->AutoSize = true;
      this->label25->Location = System::Drawing::Point(16, 0);
      this->label25->Name = L"label25";
      this->label25->Size = System::Drawing::Size(105, 13);
      this->label25->TabIndex = 12;
      this->label25->Text = L"Automatic Simulation";
      // 
      // label24
      // 
      this->label24->AutoSize = true;
      this->label24->Location = System::Drawing::Point(261, 80);
      this->label24->Name = L"label24";
      this->label24->Size = System::Drawing::Size(26, 13);
      this->label24->TabIndex = 11;
      this->label24->Text = L"kPa";
      this->label24->Click += gcnew System::EventHandler(this, &LocoSimForm::label24_Click);
      // 
      // autoSimData
      // 
      this->autoSimData->Location = System::Drawing::Point(134, 39);
      this->autoSimData->Name = L"autoSimData";
      this->autoSimData->Size = System::Drawing::Size(121, 20);
      this->autoSimData->TabIndex = 10;
      // 
      // autoSimValue
      // 
      this->autoSimValue->Location = System::Drawing::Point(134, 77);
      this->autoSimValue->Name = L"autoSimValue";
      this->autoSimValue->Size = System::Drawing::Size(121, 20);
      this->autoSimValue->TabIndex = 9;
      // 
      // label20
      // 
      this->label20->AutoSize = true;
      this->label20->Location = System::Drawing::Point(16, 77);
      this->label20->Name = L"label20";
      this->label20->Size = System::Drawing::Size(112, 13);
      this->label20->TabIndex = 8;
      this->label20->Text = L"Brake Pressure Value:";
      // 
      // label19
      // 
      this->label19->AutoSize = true;
      this->label19->Location = System::Drawing::Point(16, 39);
      this->label19->Name = L"label19";
      this->label19->Size = System::Drawing::Size(76, 13);
      this->label19->TabIndex = 6;
      this->label19->Text = L"Brake Applied:";
      // 
      // tabRailDevIF
      // 
      this->tabRailDevIF->Controls->Add(this->groupBox11);
      this->tabRailDevIF->Location = System::Drawing::Point(4, 22);
      this->tabRailDevIF->Name = L"tabRailDevIF";
      this->tabRailDevIF->Padding = System::Windows::Forms::Padding(3);
      this->tabRailDevIF->Size = System::Drawing::Size(426, 428);
      this->tabRailDevIF->TabIndex = 3;
      this->tabRailDevIF->Text = L"RailDriver I/F";
      this->tabRailDevIF->UseVisualStyleBackColor = true;
      // 
      // groupBox11
      // 
      this->groupBox11->Controls->Add(this->label26);
      this->groupBox11->Controls->Add(this->lRDSndStr);
      this->groupBox11->Controls->Add(this->lRDRecStr);
      this->groupBox11->Controls->Add(this->label29);
      this->groupBox11->Controls->Add(this->label30);
      this->groupBox11->Controls->Add(this->lRDSndTime);
      this->groupBox11->Controls->Add(this->label32);
      this->groupBox11->Controls->Add(this->lRDRecTime);
      this->groupBox11->Controls->Add(this->label34);
      this->groupBox11->Controls->Add(this->lRDConnStatus);
      this->groupBox11->Location = System::Drawing::Point(16, 16);
      this->groupBox11->Name = L"groupBox11";
      this->groupBox11->Size = System::Drawing::Size(386, 100);
      this->groupBox11->TabIndex = 3;
      this->groupBox11->TabStop = false;
      this->groupBox11->Text = L"Communication data";
      // 
      // label26
      // 
      this->label26->AutoSize = true;
      this->label26->Location = System::Drawing::Point(15, 16);
      this->label26->Name = L"label26";
      this->label26->Size = System::Drawing::Size(95, 13);
      this->label26->TabIndex = 0;
      this->label26->Text = L"Connection status:";
      // 
      // lRDSndStr
      // 
      this->lRDSndStr->AutoSize = true;
      this->lRDSndStr->Location = System::Drawing::Point(127, 80);
      this->lRDSndStr->Name = L"lRDSndStr";
      this->lRDSndStr->Size = System::Drawing::Size(57, 13);
      this->lRDSndStr->TabIndex = 1;
      this->lRDSndStr->Text = L"lRDSndStr";
      // 
      // lRDRecStr
      // 
      this->lRDRecStr->AutoSize = true;
      this->lRDRecStr->Location = System::Drawing::Point(127, 48);
      this->lRDRecStr->Name = L"lRDRecStr";
      this->lRDRecStr->Size = System::Drawing::Size(58, 13);
      this->lRDRecStr->TabIndex = 1;
      this->lRDRecStr->Text = L"lRDRecStr";
      // 
      // label29
      // 
      this->label29->AutoSize = true;
      this->label29->Location = System::Drawing::Point(15, 64);
      this->label29->Name = L"label29";
      this->label29->Size = System::Drawing::Size(75, 13);
      this->label29->TabIndex = 0;
      this->label29->Text = L"Last sent time:";
      // 
      // label30
      // 
      this->label30->AutoSize = true;
      this->label30->Location = System::Drawing::Point(15, 32);
      this->label30->Name = L"label30";
      this->label30->Size = System::Drawing::Size(90, 13);
      this->label30->TabIndex = 0;
      this->label30->Text = L"Last receive time:";
      // 
      // lRDSndTime
      // 
      this->lRDSndTime->AutoSize = true;
      this->lRDSndTime->Location = System::Drawing::Point(127, 64);
      this->lRDSndTime->Name = L"lRDSndTime";
      this->lRDSndTime->Size = System::Drawing::Size(67, 13);
      this->lRDSndTime->TabIndex = 1;
      this->lRDSndTime->Text = L"lRDSndTime";
      // 
      // label32
      // 
      this->label32->AutoSize = true;
      this->label32->Location = System::Drawing::Point(15, 80);
      this->label32->Name = L"label32";
      this->label32->Size = System::Drawing::Size(81, 13);
      this->label32->TabIndex = 0;
      this->label32->Text = L"Last sent string:";
      // 
      // lRDRecTime
      // 
      this->lRDRecTime->AutoSize = true;
      this->lRDRecTime->Location = System::Drawing::Point(127, 32);
      this->lRDRecTime->Name = L"lRDRecTime";
      this->lRDRecTime->Size = System::Drawing::Size(68, 13);
      this->lRDRecTime->TabIndex = 1;
      this->lRDRecTime->Text = L"lRDRecTime";
      // 
      // label34
      // 
      this->label34->AutoSize = true;
      this->label34->Location = System::Drawing::Point(15, 48);
      this->label34->Name = L"label34";
      this->label34->Size = System::Drawing::Size(102, 13);
      this->label34->TabIndex = 0;
      this->label34->Text = L"Last received string:";
      // 
      // lRDConnStatus
      // 
      this->lRDConnStatus->AutoSize = true;
      this->lRDConnStatus->Location = System::Drawing::Point(127, 16);
      this->lRDConnStatus->Name = L"lRDConnStatus";
      this->lRDConnStatus->Size = System::Drawing::Size(80, 13);
      this->lRDConnStatus->TabIndex = 1;
      this->lRDConnStatus->Text = L"lRDConnStatus";
      // 
      // tabPage5
      // 
      this->tabPage5->Controls->Add(this->panel3);
      this->tabPage5->Controls->Add(this->bSaveParams);
      this->tabPage5->Controls->Add(this->bApplyParams);
      this->tabPage5->Location = System::Drawing::Point(4, 22);
      this->tabPage5->Name = L"tabPage5";
      this->tabPage5->Size = System::Drawing::Size(426, 428);
      this->tabPage5->TabIndex = 4;
      this->tabPage5->Text = L"Params";
      this->tabPage5->UseVisualStyleBackColor = true;
      // 
      // panel3
      // 
      this->panel3->Controls->Add(this->panel4);
      this->panel3->Location = System::Drawing::Point(4, 4);
      this->panel3->Name = L"panel3";
      this->panel3->Size = System::Drawing::Size(408, 380);
      this->panel3->TabIndex = 14;
      // 
      // panel4
      // 
      this->panel4->AutoScroll = true;
      this->panel4->Controls->Add(this->groupBox8);
      this->panel4->Controls->Add(this->groupBox17);
      this->panel4->Controls->Add(this->gbArguments);
      this->panel4->Controls->Add(this->groupBox12);
      this->panel4->Controls->Add(this->groupBox13);
      this->panel4->Controls->Add(this->gbAutoReg);
      this->panel4->Location = System::Drawing::Point(3, 3);
      this->panel4->Name = L"panel4";
      this->panel4->Size = System::Drawing::Size(402, 377);
      this->panel4->TabIndex = 14;
      // 
      // groupBox8
      // 
      this->groupBox8->Controls->Add(this->bSelectBaliseSimFile);
      this->groupBox8->Controls->Add(this->tbBaliseSimFileName);
      this->groupBox8->Controls->Add(this->label23);
      this->groupBox8->Controls->Add(this->cbUseBaliseSimulationFile);
      this->groupBox8->Location = System::Drawing::Point(3, 299);
      this->groupBox8->Name = L"groupBox8";
      this->groupBox8->Size = System::Drawing::Size(379, 60);
      this->groupBox8->TabIndex = 6;
      this->groupBox8->TabStop = false;
      // 
      // bSelectBaliseSimFile
      // 
      this->bSelectBaliseSimFile->Location = System::Drawing::Point(346, 22);
      this->bSelectBaliseSimFile->Name = L"bSelectBaliseSimFile";
      this->bSelectBaliseSimFile->Size = System::Drawing::Size(25, 20);
      this->bSelectBaliseSimFile->TabIndex = 3;
      this->bSelectBaliseSimFile->Text = L"...";
      this->bSelectBaliseSimFile->UseVisualStyleBackColor = true;
      this->bSelectBaliseSimFile->Click += gcnew System::EventHandler(this, &LocoSimForm::bSelectBaliseSimFile_Click);
      // 
      // tbBaliseSimFileName
      // 
      this->tbBaliseSimFileName->Location = System::Drawing::Point(60, 22);
      this->tbBaliseSimFileName->Name = L"tbBaliseSimFileName";
      this->tbBaliseSimFileName->Size = System::Drawing::Size(287, 20);
      this->tbBaliseSimFileName->TabIndex = 2;
      this->tbBaliseSimFileName->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // label23
      // 
      this->label23->AutoSize = true;
      this->label23->Location = System::Drawing::Point(6, 25);
      this->label23->Name = L"label23";
      this->label23->Size = System::Drawing::Size(23, 13);
      this->label23->TabIndex = 4;
      this->label23->Text = L"File";
      // 
      // cbUseBaliseSimulationFile
      // 
      this->cbUseBaliseSimulationFile->AutoSize = true;
      this->cbUseBaliseSimulationFile->Location = System::Drawing::Point(6, 0);
      this->cbUseBaliseSimulationFile->Name = L"cbUseBaliseSimulationFile";
      this->cbUseBaliseSimulationFile->Size = System::Drawing::Size(140, 17);
      this->cbUseBaliseSimulationFile->TabIndex = 1;
      this->cbUseBaliseSimulationFile->Text = L"Use balise simulation file";
      this->cbUseBaliseSimulationFile->UseVisualStyleBackColor = true;
      this->cbUseBaliseSimulationFile->CheckedChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // groupBox17
      // 
      this->groupBox17->Controls->Add(this->label17);
      this->groupBox17->Controls->Add(this->tbRegBaliseDefault);
      this->groupBox17->Location = System::Drawing::Point(268, 3);
      this->groupBox17->Name = L"groupBox17";
      this->groupBox17->Size = System::Drawing::Size(114, 84);
      this->groupBox17->TabIndex = 2;
      this->groupBox17->TabStop = false;
      this->groupBox17->Text = L"Registration";
      // 
      // label17
      // 
      this->label17->AutoSize = true;
      this->label17->Location = System::Drawing::Point(12, 21);
      this->label17->Name = L"label17";
      this->label17->Size = System::Drawing::Size(71, 13);
      this->label17->TabIndex = 5;
      this->label17->Text = L"Default balise";
      // 
      // tbRegBaliseDefault
      // 
      this->tbRegBaliseDefault->Location = System::Drawing::Point(12, 37);
      this->tbRegBaliseDefault->Name = L"tbRegBaliseDefault";
      this->tbRegBaliseDefault->Size = System::Drawing::Size(76, 20);
      this->tbRegBaliseDefault->TabIndex = 1;
      this->tbRegBaliseDefault->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // gbArguments
      // 
      this->gbArguments->AutoSize = true;
      this->gbArguments->Controls->Add(this->ATOArgBox);
      this->gbArguments->Controls->Add(this->label49);
      this->gbArguments->Controls->Add(this->ATP2ArgBox);
      this->gbArguments->Controls->Add(this->label48);
      this->gbArguments->Controls->Add(this->ATP1ArgBox);
      this->gbArguments->Controls->Add(this->label47);
      this->gbArguments->Location = System::Drawing::Point(3, 365);
      this->gbArguments->Name = L"gbArguments";
      this->gbArguments->Size = System::Drawing::Size(379, 108);
      this->gbArguments->TabIndex = 8;
      this->gbArguments->TabStop = false;
      this->gbArguments->Text = L"ATO/ATP Arguments";
      // 
      // ATOArgBox
      // 
      this->ATOArgBox->Anchor = static_cast<System::Windows::Forms::AnchorStyles>(((System::Windows::Forms::AnchorStyles::Top | System::Windows::Forms::AnchorStyles::Left)
        | System::Windows::Forms::AnchorStyles::Right));
      this->ATOArgBox->Location = System::Drawing::Point(60, 69);
      this->ATOArgBox->Name = L"ATOArgBox";
      this->ATOArgBox->Size = System::Drawing::Size(311, 20);
      this->ATOArgBox->TabIndex = 3;
      this->ATOArgBox->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // label49
      // 
      this->label49->AutoSize = true;
      this->label49->Location = System::Drawing::Point(7, 72);
      this->label49->Name = L"label49";
      this->label49->Size = System::Drawing::Size(32, 13);
      this->label49->TabIndex = 0;
      this->label49->Text = L"ATO:";
      // 
      // ATP2ArgBox
      // 
      this->ATP2ArgBox->Anchor = static_cast<System::Windows::Forms::AnchorStyles>(((System::Windows::Forms::AnchorStyles::Top | System::Windows::Forms::AnchorStyles::Left)
        | System::Windows::Forms::AnchorStyles::Right));
      this->ATP2ArgBox->Location = System::Drawing::Point(60, 43);
      this->ATP2ArgBox->Name = L"ATP2ArgBox";
      this->ATP2ArgBox->Size = System::Drawing::Size(311, 20);
      this->ATP2ArgBox->TabIndex = 2;
      this->ATP2ArgBox->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // label48
      // 
      this->label48->AutoSize = true;
      this->label48->Location = System::Drawing::Point(7, 46);
      this->label48->Name = L"label48";
      this->label48->Size = System::Drawing::Size(37, 13);
      this->label48->TabIndex = 0;
      this->label48->Text = L"ATP2:";
      // 
      // ATP1ArgBox
      // 
      this->ATP1ArgBox->Anchor = static_cast<System::Windows::Forms::AnchorStyles>(((System::Windows::Forms::AnchorStyles::Top | System::Windows::Forms::AnchorStyles::Left)
        | System::Windows::Forms::AnchorStyles::Right));
      this->ATP1ArgBox->Location = System::Drawing::Point(60, 17);
      this->ATP1ArgBox->Name = L"ATP1ArgBox";
      this->ATP1ArgBox->Size = System::Drawing::Size(311, 20);
      this->ATP1ArgBox->TabIndex = 1;
      this->ATP1ArgBox->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // label47
      // 
      this->label47->AutoSize = true;
      this->label47->Location = System::Drawing::Point(7, 20);
      this->label47->Name = L"label47";
      this->label47->Size = System::Drawing::Size(37, 13);
      this->label47->TabIndex = 0;
      this->label47->Text = L"ATP1:";
      // 
      // groupBox12
      // 
      this->groupBox12->Controls->Add(this->tbSBRet);
      this->groupBox12->Controls->Add(this->label39);
      this->groupBox12->Controls->Add(this->tbEBRet);
      this->groupBox12->Controls->Add(this->label37);
      this->groupBox12->Controls->Add(this->tbMaxRet);
      this->groupBox12->Controls->Add(this->label35);
      this->groupBox12->Controls->Add(this->label38);
      this->groupBox12->Controls->Add(this->tbMaxAcc);
      this->groupBox12->Controls->Add(this->label36);
      this->groupBox12->Controls->Add(this->label31);
      this->groupBox12->Controls->Add(this->label33);
      this->groupBox12->Controls->Add(this->tbMaxSpeed);
      this->groupBox12->Controls->Add(this->label28);
      this->groupBox12->Controls->Add(this->label27);
      this->groupBox12->Controls->Add(this->label40);
      this->groupBox12->Location = System::Drawing::Point(3, 3);
      this->groupBox12->Name = L"groupBox12";
      this->groupBox12->Size = System::Drawing::Size(259, 154);
      this->groupBox12->TabIndex = 1;
      this->groupBox12->TabStop = false;
      this->groupBox12->Text = L"Acceleration parameters";
      // 
      // tbSBRet
      // 
      this->tbSBRet->Location = System::Drawing::Point(110, 122);
      this->tbSBRet->Name = L"tbSBRet";
      this->tbSBRet->Size = System::Drawing::Size(50, 20);
      this->tbSBRet->TabIndex = 5;
      this->tbSBRet->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // label39
      // 
      this->label39->AutoSize = true;
      this->label39->Location = System::Drawing::Point(15, 125);
      this->label39->Name = L"label39";
      this->label39->Size = System::Drawing::Size(77, 13);
      this->label39->TabIndex = 0;
      this->label39->Text = L"SB retardation:";
      // 
      // tbEBRet
      // 
      this->tbEBRet->Location = System::Drawing::Point(110, 96);
      this->tbEBRet->Name = L"tbEBRet";
      this->tbEBRet->Size = System::Drawing::Size(50, 20);
      this->tbEBRet->TabIndex = 4;
      this->tbEBRet->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // label37
      // 
      this->label37->AutoSize = true;
      this->label37->Location = System::Drawing::Point(15, 99);
      this->label37->Name = L"label37";
      this->label37->Size = System::Drawing::Size(74, 13);
      this->label37->TabIndex = 0;
      this->label37->Text = L"EB retardation";
      // 
      // tbMaxRet
      // 
      this->tbMaxRet->Location = System::Drawing::Point(110, 70);
      this->tbMaxRet->Name = L"tbMaxRet";
      this->tbMaxRet->Size = System::Drawing::Size(50, 20);
      this->tbMaxRet->TabIndex = 3;
      this->tbMaxRet->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // label35
      // 
      this->label35->AutoSize = true;
      this->label35->Location = System::Drawing::Point(15, 73);
      this->label35->Name = L"label35";
      this->label35->Size = System::Drawing::Size(83, 13);
      this->label35->TabIndex = 0;
      this->label35->Text = L"Max retardation:";
      // 
      // label38
      // 
      this->label38->AutoSize = true;
      this->label38->Location = System::Drawing::Point(161, 125);
      this->label38->Name = L"label38";
      this->label38->Size = System::Drawing::Size(37, 13);
      this->label38->TabIndex = 1;
      this->label38->Text = L"cm/s2";
      // 
      // tbMaxAcc
      // 
      this->tbMaxAcc->Location = System::Drawing::Point(110, 44);
      this->tbMaxAcc->Name = L"tbMaxAcc";
      this->tbMaxAcc->Size = System::Drawing::Size(50, 20);
      this->tbMaxAcc->TabIndex = 2;
      this->tbMaxAcc->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // label36
      // 
      this->label36->AutoSize = true;
      this->label36->Location = System::Drawing::Point(161, 99);
      this->label36->Name = L"label36";
      this->label36->Size = System::Drawing::Size(37, 13);
      this->label36->TabIndex = 1;
      this->label36->Text = L"cm/s2";
      // 
      // label31
      // 
      this->label31->AutoSize = true;
      this->label31->Location = System::Drawing::Point(15, 47);
      this->label31->Name = L"label31";
      this->label31->Size = System::Drawing::Size(91, 13);
      this->label31->TabIndex = 0;
      this->label31->Text = L"Max acceleration:";
      // 
      // label33
      // 
      this->label33->AutoSize = true;
      this->label33->Location = System::Drawing::Point(161, 73);
      this->label33->Name = L"label33";
      this->label33->Size = System::Drawing::Size(37, 13);
      this->label33->TabIndex = 1;
      this->label33->Text = L"cm/s2";
      // 
      // tbMaxSpeed
      // 
      this->tbMaxSpeed->Location = System::Drawing::Point(110, 18);
      this->tbMaxSpeed->Name = L"tbMaxSpeed";
      this->tbMaxSpeed->Size = System::Drawing::Size(50, 20);
      this->tbMaxSpeed->TabIndex = 1;
      this->tbMaxSpeed->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // label28
      // 
      this->label28->AutoSize = true;
      this->label28->Location = System::Drawing::Point(161, 47);
      this->label28->Name = L"label28";
      this->label28->Size = System::Drawing::Size(37, 13);
      this->label28->TabIndex = 1;
      this->label28->Text = L"cm/s2";
      // 
      // label27
      // 
      this->label27->AutoSize = true;
      this->label27->Location = System::Drawing::Point(15, 21);
      this->label27->Name = L"label27";
      this->label27->Size = System::Drawing::Size(62, 13);
      this->label27->TabIndex = 0;
      this->label27->Text = L"Max speed:";
      // 
      // label40
      // 
      this->label40->AutoSize = true;
      this->label40->Location = System::Drawing::Point(161, 21);
      this->label40->Name = L"label40";
      this->label40->Size = System::Drawing::Size(31, 13);
      this->label40->TabIndex = 1;
      this->label40->Text = L"cm/s";
      // 
      // groupBox13
      // 
      this->groupBox13->Controls->Add(this->cbEnableSound);
      this->groupBox13->Location = System::Drawing::Point(268, 93);
      this->groupBox13->Name = L"groupBox13";
      this->groupBox13->Size = System::Drawing::Size(114, 64);
      this->groupBox13->TabIndex = 3;
      this->groupBox13->TabStop = false;
      this->groupBox13->Text = L"Misc";
      // 
      // cbEnableSound
      // 
      this->cbEnableSound->AutoSize = true;
      this->cbEnableSound->Location = System::Drawing::Point(15, 21);
      this->cbEnableSound->Name = L"cbEnableSound";
      this->cbEnableSound->Size = System::Drawing::Size(91, 17);
      this->cbEnableSound->TabIndex = 1;
      this->cbEnableSound->Text = L"Enable sound";
      this->cbEnableSound->UseVisualStyleBackColor = true;
      this->cbEnableSound->CheckedChanged += gcnew System::EventHandler(this, &LocoSimForm::cbEnableSound_CheckedChanged);
      // 
      // gbAutoReg
      // 
      this->gbAutoReg->Controls->Add(this->cbAutoRegATOMode);
      this->gbAutoReg->Controls->Add(this->cbAutoRegDirection);
      this->gbAutoReg->Controls->Add(this->cbAutoRegEnable);
      this->gbAutoReg->Controls->Add(this->label46);
      this->gbAutoReg->Controls->Add(this->label44);
      this->gbAutoReg->Controls->Add(this->label45);
      this->gbAutoReg->Controls->Add(this->label42);
      this->gbAutoReg->Controls->Add(this->label43);
      this->gbAutoReg->Controls->Add(this->label41);
      this->gbAutoReg->Controls->Add(this->tbAutoRegSpeed);
      this->gbAutoReg->Controls->Add(this->tbAutoRegAcc);
      this->gbAutoReg->Location = System::Drawing::Point(3, 163);
      this->gbAutoReg->Name = L"gbAutoReg";
      this->gbAutoReg->Size = System::Drawing::Size(379, 130);
      this->gbAutoReg->TabIndex = 4;
      this->gbAutoReg->TabStop = false;
      // 
      // cbAutoRegATOMode
      // 
      this->cbAutoRegATOMode->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbAutoRegATOMode->FormattingEnabled = true;
      this->cbAutoRegATOMode->Items->AddRange(gcnew cli::array< System::Object^  >(3) { L"ATO Manual", L"ATO Supervised", L"ATO Automatic" });
      this->cbAutoRegATOMode->Location = System::Drawing::Point(126, 97);
      this->cbAutoRegATOMode->Name = L"cbAutoRegATOMode";
      this->cbAutoRegATOMode->Size = System::Drawing::Size(121, 21);
      this->cbAutoRegATOMode->TabIndex = 5;
      this->cbAutoRegATOMode->SelectedIndexChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // cbAutoRegDirection
      // 
      this->cbAutoRegDirection->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
      this->cbAutoRegDirection->FormattingEnabled = true;
      this->cbAutoRegDirection->Items->AddRange(gcnew cli::array< System::Object^  >(2) { L"Forward", L"Reverse" });
      this->cbAutoRegDirection->Location = System::Drawing::Point(126, 70);
      this->cbAutoRegDirection->Name = L"cbAutoRegDirection";
      this->cbAutoRegDirection->Size = System::Drawing::Size(121, 21);
      this->cbAutoRegDirection->TabIndex = 4;
      this->cbAutoRegDirection->SelectedIndexChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // cbAutoRegEnable
      // 
      this->cbAutoRegEnable->AutoSize = true;
      this->cbAutoRegEnable->Location = System::Drawing::Point(6, 0);
      this->cbAutoRegEnable->Name = L"cbAutoRegEnable";
      this->cbAutoRegEnable->Size = System::Drawing::Size(140, 17);
      this->cbAutoRegEnable->TabIndex = 1;
      this->cbAutoRegEnable->Text = L"Enable AutoRegistration";
      this->cbAutoRegEnable->UseVisualStyleBackColor = true;
      this->cbAutoRegEnable->CheckedChanged += gcnew System::EventHandler(this, &LocoSimForm::AutoRegEnable_Click);
      // 
      // label46
      // 
      this->label46->AutoSize = true;
      this->label46->Location = System::Drawing::Point(15, 99);
      this->label46->Name = L"label46";
      this->label46->Size = System::Drawing::Size(108, 13);
      this->label46->TabIndex = 0;
      this->label46->Text = L"Resulting ATO mode:";
      // 
      // label44
      // 
      this->label44->AutoSize = true;
      this->label44->Location = System::Drawing::Point(15, 46);
      this->label44->Name = L"label44";
      this->label44->Size = System::Drawing::Size(76, 13);
      this->label44->TabIndex = 0;
      this->label44->Text = L"Search speed:";
      // 
      // label45
      // 
      this->label45->AutoSize = true;
      this->label45->Location = System::Drawing::Point(15, 73);
      this->label45->Name = L"label45";
      this->label45->Size = System::Drawing::Size(52, 13);
      this->label45->TabIndex = 0;
      this->label45->Text = L"Direction:";
      // 
      // label42
      // 
      this->label42->AutoSize = true;
      this->label42->Location = System::Drawing::Point(15, 20);
      this->label42->Name = L"label42";
      this->label42->Size = System::Drawing::Size(69, 13);
      this->label42->TabIndex = 0;
      this->label42->Text = L"Acceleration:";
      // 
      // label43
      // 
      this->label43->AutoSize = true;
      this->label43->Location = System::Drawing::Point(177, 46);
      this->label43->Name = L"label43";
      this->label43->Size = System::Drawing::Size(31, 13);
      this->label43->TabIndex = 1;
      this->label43->Text = L"cm/s";
      // 
      // label41
      // 
      this->label41->AutoSize = true;
      this->label41->Location = System::Drawing::Point(177, 20);
      this->label41->Name = L"label41";
      this->label41->Size = System::Drawing::Size(37, 13);
      this->label41->TabIndex = 1;
      this->label41->Text = L"cm/s2";
      // 
      // tbAutoRegSpeed
      // 
      this->tbAutoRegSpeed->Location = System::Drawing::Point(126, 43);
      this->tbAutoRegSpeed->Name = L"tbAutoRegSpeed";
      this->tbAutoRegSpeed->Size = System::Drawing::Size(50, 20);
      this->tbAutoRegSpeed->TabIndex = 3;
      this->tbAutoRegSpeed->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // tbAutoRegAcc
      // 
      this->tbAutoRegAcc->Location = System::Drawing::Point(126, 17);
      this->tbAutoRegAcc->Name = L"tbAutoRegAcc";
      this->tbAutoRegAcc->Size = System::Drawing::Size(50, 20);
      this->tbAutoRegAcc->TabIndex = 2;
      this->tbAutoRegAcc->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::ParamsChanged_Click);
      // 
      // bSaveParams
      // 
      this->bSaveParams->Enabled = false;
      this->bSaveParams->Location = System::Drawing::Point(256, 397);
      this->bSaveParams->Name = L"bSaveParams";
      this->bSaveParams->Size = System::Drawing::Size(75, 23);
      this->bSaveParams->TabIndex = 1;
      this->bSaveParams->Text = L"Save";
      this->bSaveParams->UseVisualStyleBackColor = true;
      this->bSaveParams->Click += gcnew System::EventHandler(this, &LocoSimForm::bSaveParams_Click);
      // 
      // bApplyParams
      // 
      this->bApplyParams->Enabled = false;
      this->bApplyParams->Location = System::Drawing::Point(337, 397);
      this->bApplyParams->Name = L"bApplyParams";
      this->bApplyParams->Size = System::Drawing::Size(75, 23);
      this->bApplyParams->TabIndex = 2;
      this->bApplyParams->Text = L"Apply";
      this->bApplyParams->UseVisualStyleBackColor = true;
      this->bApplyParams->Click += gcnew System::EventHandler(this, &LocoSimForm::bApplyParams_Click);
      // 
      // tabPage2
      // 
      this->tabPage2->Controls->Add(this->groupBox9);
      this->tabPage2->Controls->Add(this->groupBox14);
      this->tabPage2->Enabled = false;
      this->tabPage2->Location = System::Drawing::Point(4, 22);
      this->tabPage2->Name = L"tabPage2";
      this->tabPage2->Padding = System::Windows::Forms::Padding(3);
      this->tabPage2->Size = System::Drawing::Size(426, 428);
      this->tabPage2->TabIndex = 1;
      this->tabPage2->Text = L"Simulation";
      this->tabPage2->UseVisualStyleBackColor = true;
      // 
      // groupBox9
      // 
      this->groupBox9->Controls->Add(this->pbATOStart);
      this->groupBox9->Controls->Add(this->pbATP2Start);
      this->groupBox9->Controls->Add(this->pbATP1Start);
      this->groupBox9->Controls->Add(this->lATOState);
      this->groupBox9->Controls->Add(this->label12);
      this->groupBox9->Controls->Add(this->lATP2State);
      this->groupBox9->Controls->Add(this->label11);
      this->groupBox9->Controls->Add(this->lATP1State);
      this->groupBox9->Controls->Add(this->label10);
      this->groupBox9->Controls->Add(this->bATP1ToFF);
      this->groupBox9->Location = System::Drawing::Point(16, 16);
      this->groupBox9->Name = L"groupBox9";
      this->groupBox9->Size = System::Drawing::Size(386, 91);
      this->groupBox9->TabIndex = 1;
      this->groupBox9->TabStop = false;
      this->groupBox9->Text = L"System Simulation";
      // 
      // pbATOStart
      // 
      this->pbATOStart->BackgroundImage = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"pbATOStart.BackgroundImage")));
      this->pbATOStart->BackgroundImageLayout = System::Windows::Forms::ImageLayout::Zoom;
      this->pbATOStart->BorderStyle = System::Windows::Forms::BorderStyle::Fixed3D;
      this->pbATOStart->Location = System::Drawing::Point(7, 61);
      this->pbATOStart->Name = L"pbATOStart";
      this->pbATOStart->Size = System::Drawing::Size(16, 16);
      this->pbATOStart->TabIndex = 2;
      this->pbATOStart->TabStop = false;
      this->pbATOStart->Click += gcnew System::EventHandler(this, &LocoSimForm::pbATOStart_Click);
      // 
      // pbATP2Start
      // 
      this->pbATP2Start->BackgroundImage = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"pbATP2Start.BackgroundImage")));
      this->pbATP2Start->BackgroundImageLayout = System::Windows::Forms::ImageLayout::Zoom;
      this->pbATP2Start->BorderStyle = System::Windows::Forms::BorderStyle::Fixed3D;
      this->pbATP2Start->Location = System::Drawing::Point(7, 42);
      this->pbATP2Start->Name = L"pbATP2Start";
      this->pbATP2Start->Size = System::Drawing::Size(16, 16);
      this->pbATP2Start->TabIndex = 2;
      this->pbATP2Start->TabStop = false;
      this->pbATP2Start->Click += gcnew System::EventHandler(this, &LocoSimForm::pbATP2Start_Click);
      // 
      // pbATP1Start
      // 
      this->pbATP1Start->BackgroundImageLayout = System::Windows::Forms::ImageLayout::Zoom;
      this->pbATP1Start->BorderStyle = System::Windows::Forms::BorderStyle::Fixed3D;
      this->pbATP1Start->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"pbATP1Start.Image")));
      this->pbATP1Start->Location = System::Drawing::Point(7, 24);
      this->pbATP1Start->Name = L"pbATP1Start";
      this->pbATP1Start->Size = System::Drawing::Size(16, 16);
      this->pbATP1Start->SizeMode = System::Windows::Forms::PictureBoxSizeMode::StretchImage;
      this->pbATP1Start->TabIndex = 2;
      this->pbATP1Start->TabStop = false;
      this->pbATP1Start->Click += gcnew System::EventHandler(this, &LocoSimForm::pbATP1Start_Click);
      // 
      // lATOState
      // 
      this->lATOState->AutoSize = true;
      this->lATOState->Location = System::Drawing::Point(64, 64);
      this->lATOState->Name = L"lATOState";
      this->lATOState->Size = System::Drawing::Size(78, 13);
      this->lATOState->TabIndex = 1;
      this->lATOState->Text = L"ATxNoProcess";
      // 
      // label12
      // 
      this->label12->AutoSize = true;
      this->label12->Location = System::Drawing::Point(26, 64);
      this->label12->Name = L"label12";
      this->label12->Size = System::Drawing::Size(32, 13);
      this->label12->TabIndex = 1;
      this->label12->Text = L"ATO:";
      // 
      // lATP2State
      // 
      this->lATP2State->AutoSize = true;
      this->lATP2State->Location = System::Drawing::Point(63, 45);
      this->lATP2State->Name = L"lATP2State";
      this->lATP2State->Size = System::Drawing::Size(78, 13);
      this->lATP2State->TabIndex = 1;
      this->lATP2State->Text = L"ATxNoProcess";
      // 
      // label11
      // 
      this->label11->AutoSize = true;
      this->label11->Location = System::Drawing::Point(25, 45);
      this->label11->Name = L"label11";
      this->label11->Size = System::Drawing::Size(37, 13);
      this->label11->TabIndex = 1;
      this->label11->Text = L"ATP2:";
      // 
      // lATP1State
      // 
      this->lATP1State->AutoSize = true;
      this->lATP1State->Location = System::Drawing::Point(64, 27);
      this->lATP1State->Name = L"lATP1State";
      this->lATP1State->Size = System::Drawing::Size(78, 13);
      this->lATP1State->TabIndex = 1;
      this->lATP1State->Text = L"ATxNoProcess";
      // 
      // label10
      // 
      this->label10->AutoSize = true;
      this->label10->Location = System::Drawing::Point(26, 27);
      this->label10->Name = L"label10";
      this->label10->Size = System::Drawing::Size(37, 13);
      this->label10->TabIndex = 1;
      this->label10->Text = L"ATP1:";
      // 
      // bATP1ToFF
      // 
      this->bATP1ToFF->Location = System::Drawing::Point(295, 59);
      this->bATP1ToFF->Name = L"bATP1ToFF";
      this->bATP1ToFF->Size = System::Drawing::Size(75, 23);
      this->bATP1ToFF->TabIndex = 1;
      this->bATP1ToFF->Text = L"ATP1 FF";
      this->bATP1ToFF->UseVisualStyleBackColor = true;
      this->bATP1ToFF->Click += gcnew System::EventHandler(this, &LocoSimForm::bATP1ToFF_Click);
      // 
      // groupBox14
      // 
      this->groupBox14->Controls->Add(this->groupBox16);
      this->groupBox14->Controls->Add(this->groupBox15);
      this->groupBox14->Location = System::Drawing::Point(16, 113);
      this->groupBox14->Name = L"groupBox14";
      this->groupBox14->Size = System::Drawing::Size(386, 103);
      this->groupBox14->TabIndex = 0;
      this->groupBox14->TabStop = false;
      this->groupBox14->Text = L"Balise data";
      // 
      // groupBox16
      // 
      this->groupBox16->Controls->Add(this->label15);
      this->groupBox16->Controls->Add(this->tbRegUseBalise);
      this->groupBox16->Location = System::Drawing::Point(13, 19);
      this->groupBox16->Name = L"groupBox16";
      this->groupBox16->Size = System::Drawing::Size(129, 73);
      this->groupBox16->TabIndex = 6;
      this->groupBox16->TabStop = false;
      this->groupBox16->Text = L"Registration";
      // 
      // label15
      // 
      this->label15->AutoSize = true;
      this->label15->Location = System::Drawing::Point(6, 16);
      this->label15->Name = L"label15";
      this->label15->Size = System::Drawing::Size(61, 13);
      this->label15->TabIndex = 5;
      this->label15->Text = L"Uses balise";
      // 
      // tbRegUseBalise
      // 
      this->tbRegUseBalise->Location = System::Drawing::Point(6, 32);
      this->tbRegUseBalise->Name = L"tbRegUseBalise";
      this->tbRegUseBalise->ReadOnly = true;
      this->tbRegUseBalise->Size = System::Drawing::Size(76, 20);
      this->tbRegUseBalise->TabIndex = 1;
      this->tbRegUseBalise->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::tbRegUseBalise_TextChanged);
      // 
      // groupBox15
      // 
      this->groupBox15->Controls->Add(this->rbReRegLastBalise);
      this->groupBox15->Controls->Add(this->tbReRegSpecBalise);
      this->groupBox15->Controls->Add(this->rbReRegSpecBalise);
      this->groupBox15->Controls->Add(this->tbReRegLastBalise);
      this->groupBox15->Location = System::Drawing::Point(148, 19);
      this->groupBox15->Name = L"groupBox15";
      this->groupBox15->Size = System::Drawing::Size(227, 73);
      this->groupBox15->TabIndex = 4;
      this->groupBox15->TabStop = false;
      this->groupBox15->Text = L"ReRegistration";
      // 
      // rbReRegLastBalise
      // 
      this->rbReRegLastBalise->AutoSize = true;
      this->rbReRegLastBalise->Checked = true;
      this->rbReRegLastBalise->Location = System::Drawing::Point(6, 19);
      this->rbReRegLastBalise->Name = L"rbReRegLastBalise";
      this->rbReRegLastBalise->Size = System::Drawing::Size(131, 17);
      this->rbReRegLastBalise->TabIndex = 1;
      this->rbReRegLastBalise->TabStop = true;
      this->rbReRegLastBalise->Text = L"Search from last balise";
      this->rbReRegLastBalise->UseVisualStyleBackColor = true;
      // 
      // tbReRegSpecBalise
      // 
      this->tbReRegSpecBalise->Location = System::Drawing::Point(139, 41);
      this->tbReRegSpecBalise->Name = L"tbReRegSpecBalise";
      this->tbReRegSpecBalise->Size = System::Drawing::Size(76, 20);
      this->tbReRegSpecBalise->TabIndex = 4;
      // 
      // rbReRegSpecBalise
      // 
      this->rbReRegSpecBalise->AutoSize = true;
      this->rbReRegSpecBalise->Location = System::Drawing::Point(6, 42);
      this->rbReRegSpecBalise->Name = L"rbReRegSpecBalise";
      this->rbReRegSpecBalise->Size = System::Drawing::Size(93, 17);
      this->rbReRegSpecBalise->TabIndex = 3;
      this->rbReRegSpecBalise->Text = L"Use this balise";
      this->rbReRegSpecBalise->UseVisualStyleBackColor = true;
      // 
      // tbReRegLastBalise
      // 
      this->tbReRegLastBalise->BackColor = System::Drawing::SystemColors::InactiveBorder;
      this->tbReRegLastBalise->Location = System::Drawing::Point(139, 18);
      this->tbReRegLastBalise->Name = L"tbReRegLastBalise";
      this->tbReRegLastBalise->ReadOnly = true;
      this->tbReRegLastBalise->Size = System::Drawing::Size(76, 20);
      this->tbReRegLastBalise->TabIndex = 2;
      // 
      // tabPage1
      // 
      this->tabPage1->Controls->Add(this->groupBox18);
      this->tabPage1->Controls->Add(this->groupBox7);
      this->tabPage1->Controls->Add(this->groupBox6);
      this->tabPage1->Controls->Add(this->groupBox5);
      this->tabPage1->Controls->Add(this->groupBox4);
      this->tabPage1->Controls->Add(this->groupBox3);
      this->tabPage1->Controls->Add(this->groupBox2);
      this->tabPage1->Controls->Add(this->groupBox1);
      this->tabPage1->Location = System::Drawing::Point(4, 22);
      this->tabPage1->Name = L"tabPage1";
      this->tabPage1->Padding = System::Windows::Forms::Padding(3);
      this->tabPage1->Size = System::Drawing::Size(426, 428);
      this->tabPage1->TabIndex = 0;
      this->tabPage1->Text = L"LocoSim";
      this->tabPage1->UseVisualStyleBackColor = true;
      // 
      // groupBox18
      // 
      this->groupBox18->Controls->Add(this->bIsolB);
      this->groupBox18->Controls->Add(this->bEb1a);
      this->groupBox18->Controls->Add(this->bIsolA);
      this->groupBox18->Controls->Add(this->bEb2b);
      this->groupBox18->Controls->Add(this->bEb2a);
      this->groupBox18->Controls->Add(this->bEb1b);
      this->groupBox18->Location = System::Drawing::Point(16, 222);
      this->groupBox18->Name = L"groupBox18";
      this->groupBox18->Size = System::Drawing::Size(390, 52);
      this->groupBox18->TabIndex = 8;
      this->groupBox18->TabStop = false;
      this->groupBox18->Text = L"Cut Out";
      // 
      // bIsolB
      // 
      this->bIsolB->Location = System::Drawing::Point(324, 19);
      this->bIsolB->Name = L"bIsolB";
      this->bIsolB->Size = System::Drawing::Size(57, 23);
      this->bIsolB->TabIndex = 15;
      this->bIsolB->Text = L"IsolB";
      this->bIsolB->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bIsolB->UseVisualStyleBackColor = true;
      this->bIsolB->Click += gcnew System::EventHandler(this, &LocoSimForm::bIsolB_Click);
      // 
      // bEb1a
      // 
      this->bEb1a->Location = System::Drawing::Point(10, 19);
      this->bEb1a->Name = L"bEb1a";
      this->bEb1a->Size = System::Drawing::Size(57, 23);
      this->bEb1a->TabIndex = 14;
      this->bEb1a->Text = L"EB #1A";
      this->bEb1a->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bEb1a->UseVisualStyleBackColor = true;
      this->bEb1a->Click += gcnew System::EventHandler(this, &LocoSimForm::bEb1a_Click);
      // 
      // bIsolA
      // 
      this->bIsolA->Location = System::Drawing::Point(262, 19);
      this->bIsolA->Name = L"bIsolA";
      this->bIsolA->Size = System::Drawing::Size(57, 23);
      this->bIsolA->TabIndex = 13;
      this->bIsolA->Text = L"IsolA";
      this->bIsolA->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bIsolA->UseVisualStyleBackColor = true;
      this->bIsolA->Click += gcnew System::EventHandler(this, &LocoSimForm::bIsolA_Click);
      // 
      // bEb2b
      // 
      this->bEb2b->Location = System::Drawing::Point(199, 19);
      this->bEb2b->Name = L"bEb2b";
      this->bEb2b->Size = System::Drawing::Size(57, 23);
      this->bEb2b->TabIndex = 12;
      this->bEb2b->Text = L"EB #2B";
      this->bEb2b->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bEb2b->UseVisualStyleBackColor = true;
      this->bEb2b->Click += gcnew System::EventHandler(this, &LocoSimForm::bEb2b_Click);
      // 
      // bEb2a
      // 
      this->bEb2a->Location = System::Drawing::Point(136, 19);
      this->bEb2a->Name = L"bEb2a";
      this->bEb2a->Size = System::Drawing::Size(57, 23);
      this->bEb2a->TabIndex = 11;
      this->bEb2a->Text = L"EB #2A";
      this->bEb2a->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bEb2a->UseVisualStyleBackColor = true;
      this->bEb2a->Click += gcnew System::EventHandler(this, &LocoSimForm::bEb2a_Click);
      // 
      // bEb1b
      // 
      this->bEb1b->Location = System::Drawing::Point(73, 19);
      this->bEb1b->Name = L"bEb1b";
      this->bEb1b->Size = System::Drawing::Size(57, 23);
      this->bEb1b->TabIndex = 10;
      this->bEb1b->Text = L"EB #1B";
      this->bEb1b->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bEb1b->UseVisualStyleBackColor = true;
      this->bEb1b->Click += gcnew System::EventHandler(this, &LocoSimForm::bEb1b_Click);
      // 
      // groupBox7
      // 
      this->groupBox7->BackgroundImageLayout = System::Windows::Forms::ImageLayout::Center;
      this->groupBox7->Controls->Add(this->bRoadM);
      this->groupBox7->Controls->Add(this->bRailM);
      this->groupBox7->Controls->Add(this->bNcu);
      this->groupBox7->Controls->Add(this->bCabin2);
      this->groupBox7->Controls->Add(this->bEMSAlert);
      this->groupBox7->Controls->Add(this->bAOSOff);
      this->groupBox7->Controls->Add(this->bCabin1);
      this->groupBox7->Controls->Add(this->bLCSReady);
      this->groupBox7->Location = System::Drawing::Point(128, 130);
      this->groupBox7->Name = L"groupBox7";
      this->groupBox7->Size = System::Drawing::Size(278, 86);
      this->groupBox7->TabIndex = 6;
      this->groupBox7->TabStop = false;
      this->groupBox7->Text = L"Loco I/O";
      // 
      // bRoadM
      // 
      this->bRoadM->Location = System::Drawing::Point(117, 48);
      this->bRoadM->Name = L"bRoadM";
      this->bRoadM->Size = System::Drawing::Size(71, 23);
      this->bRoadM->TabIndex = 8;
      this->bRoadM->Text = L"RoadMode";
      this->bRoadM->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bRoadM->UseVisualStyleBackColor = true;
      this->bRoadM->Click += gcnew System::EventHandler(this, &LocoSimForm::bRoadM_Click);
      // 
      // bRailM
      // 
      this->bRailM->Location = System::Drawing::Point(195, 48);
      this->bRailM->Name = L"bRailM";
      this->bRailM->Size = System::Drawing::Size(71, 23);
      this->bRailM->TabIndex = 7;
      this->bRailM->Text = L"RailMode";
      this->bRailM->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bRailM->UseVisualStyleBackColor = true;
      this->bRailM->Click += gcnew System::EventHandler(this, &LocoSimForm::bRailM_Click);
      // 
      // bNcu
      // 
      this->bNcu->Location = System::Drawing::Point(183, 19);
      this->bNcu->Name = L"bNcu";
      this->bNcu->Size = System::Drawing::Size(57, 23);
      this->bNcu->TabIndex = 6;
      this->bNcu->Text = L"NCU";
      this->bNcu->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bNcu->UseVisualStyleBackColor = true;
      this->bNcu->Click += gcnew System::EventHandler(this, &LocoSimForm::bNcu_Click);
      // 
      // bCabin2
      // 
      this->bCabin2->Location = System::Drawing::Point(57, 19);
      this->bCabin2->Name = L"bCabin2";
      this->bCabin2->Size = System::Drawing::Size(45, 23);
      this->bCabin2->TabIndex = 5;
      this->bCabin2->Text = L"Cab2";
      this->bCabin2->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bCabin2->UseVisualStyleBackColor = true;
      this->bCabin2->Click += gcnew System::EventHandler(this, &LocoSimForm::bCabin2_Click);
      // 
      // bEMSAlert
      // 
      this->bEMSAlert->Location = System::Drawing::Point(69, 48);
      this->bEMSAlert->Name = L"bEMSAlert";
      this->bEMSAlert->Size = System::Drawing::Size(40, 23);
      this->bEMSAlert->TabIndex = 4;
      this->bEMSAlert->Text = L"EMS";
      this->bEMSAlert->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bEMSAlert->UseVisualStyleBackColor = true;
      // 
      // bAOSOff
      // 
      this->bAOSOff->Location = System::Drawing::Point(6, 48);
      this->bAOSOff->Name = L"bAOSOff";
      this->bAOSOff->Size = System::Drawing::Size(57, 23);
      this->bAOSOff->TabIndex = 3;
      this->bAOSOff->Text = L"AOS Off";
      this->bAOSOff->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bAOSOff->UseVisualStyleBackColor = true;
      this->bAOSOff->Click += gcnew System::EventHandler(this, &LocoSimForm::bAOSOff_Click);
      // 
      // bCabin1
      // 
      this->bCabin1->Location = System::Drawing::Point(6, 19);
      this->bCabin1->Name = L"bCabin1";
      this->bCabin1->Size = System::Drawing::Size(45, 23);
      this->bCabin1->TabIndex = 1;
      this->bCabin1->Text = L"Cab1";
      this->bCabin1->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bCabin1->UseVisualStyleBackColor = true;
      this->bCabin1->Click += gcnew System::EventHandler(this, &LocoSimForm::bCabin1_Click);
      // 
      // bLCSReady
      // 
      this->bLCSReady->Location = System::Drawing::Point(108, 19);
      this->bLCSReady->Name = L"bLCSReady";
      this->bLCSReady->Size = System::Drawing::Size(69, 23);
      this->bLCSReady->TabIndex = 2;
      this->bLCSReady->Text = L"LCS Ready";
      this->bLCSReady->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bLCSReady->UseVisualStyleBackColor = true;
      this->bLCSReady->Click += gcnew System::EventHandler(this, &LocoSimForm::bLCSReady_Click);
      // 
      // groupBox6
      // 
      this->groupBox6->Controls->Add(this->label8);
      this->groupBox6->Controls->Add(this->clbOutputs);
      this->groupBox6->Location = System::Drawing::Point(13, 280);
      this->groupBox6->Name = L"groupBox6";
      this->groupBox6->Size = System::Drawing::Size(393, 141);
      this->groupBox6->TabIndex = 7;
      this->groupBox6->TabStop = false;
      this->groupBox6->Text = L"Loco I/O";
      // 
      // label8
      // 
      this->label8->AutoSize = true;
      this->label8->Location = System::Drawing::Point(10, 20);
      this->label8->Name = L"label8";
      this->label8->Size = System::Drawing::Size(44, 13);
      this->label8->TabIndex = 1;
      this->label8->Text = L"Outputs";
      this->label8->Click += gcnew System::EventHandler(this, &LocoSimForm::label8_Click);
      // 
      // clbOutputs
      // 
      this->clbOutputs->BackColor = System::Drawing::SystemColors::ControlLight;
      this->clbOutputs->ColumnWidth = 180;
      this->clbOutputs->Enabled = false;
      this->clbOutputs->FormattingEnabled = true;
      this->clbOutputs->Items->AddRange(gcnew cli::array< System::Object^  >(12) {
        L"1: Emergency Brake 1", L"2: Emergency Brake 2",
          L"3: Emergency Brake Active", L"4: Service Break", L"5: Traction Cut Off", L"6: - Spare -", L"7: ATP OK", L"8: Buzzer", L"9: Lamp",
          L"10: Power Off", L"11: - Spare -", L"12: - Spare -"
      });
      this->clbOutputs->Location = System::Drawing::Point(10, 36);
      this->clbOutputs->MultiColumn = true;
      this->clbOutputs->Name = L"clbOutputs";
      this->clbOutputs->Size = System::Drawing::Size(374, 94);
      this->clbOutputs->TabIndex = 0;
      this->clbOutputs->SelectedIndexChanged += gcnew System::EventHandler(this, &LocoSimForm::clbOutput_SelectedIndexChanged);
      // 
      // groupBox5
      // 
      this->groupBox5->Controls->Add(this->rbAutomatic);
      this->groupBox5->Controls->Add(this->rbSupervised);
      this->groupBox5->Controls->Add(this->rbManual);
      this->groupBox5->Location = System::Drawing::Point(318, 16);
      this->groupBox5->Name = L"groupBox5";
      this->groupBox5->Size = System::Drawing::Size(88, 108);
      this->groupBox5->TabIndex = 5;
      this->groupBox5->TabStop = false;
      this->groupBox5->Text = L"ATO Mode";
      // 
      // rbAutomatic
      // 
      this->rbAutomatic->AutoSize = true;
      this->rbAutomatic->Location = System::Drawing::Point(7, 66);
      this->rbAutomatic->Name = L"rbAutomatic";
      this->rbAutomatic->Size = System::Drawing::Size(72, 17);
      this->rbAutomatic->TabIndex = 3;
      this->rbAutomatic->Text = L"Automatic";
      this->rbAutomatic->UseVisualStyleBackColor = true;
      this->rbAutomatic->MouseUp += gcnew System::Windows::Forms::MouseEventHandler(this, &LocoSimForm::SetFocusToThrottle);
      // 
      // rbSupervised
      // 
      this->rbSupervised->AutoSize = true;
      this->rbSupervised->Location = System::Drawing::Point(7, 43);
      this->rbSupervised->Name = L"rbSupervised";
      this->rbSupervised->Size = System::Drawing::Size(78, 17);
      this->rbSupervised->TabIndex = 2;
      this->rbSupervised->Text = L"Supervised";
      this->rbSupervised->UseVisualStyleBackColor = true;
      this->rbSupervised->MouseUp += gcnew System::Windows::Forms::MouseEventHandler(this, &LocoSimForm::SetFocusToThrottle);
      // 
      // rbManual
      // 
      this->rbManual->AutoSize = true;
      this->rbManual->Checked = true;
      this->rbManual->Location = System::Drawing::Point(7, 20);
      this->rbManual->Name = L"rbManual";
      this->rbManual->Size = System::Drawing::Size(60, 17);
      this->rbManual->TabIndex = 1;
      this->rbManual->TabStop = true;
      this->rbManual->Text = L"Manual";
      this->rbManual->UseVisualStyleBackColor = true;
      this->rbManual->MouseUp += gcnew System::Windows::Forms::MouseEventHandler(this, &LocoSimForm::SetFocusToThrottle);
      // 
      // groupBox4
      // 
      this->groupBox4->Controls->Add(this->rbReverse);
      this->groupBox4->Controls->Add(this->rbNeutral);
      this->groupBox4->Controls->Add(this->rbForward);
      this->groupBox4->Location = System::Drawing::Point(229, 16);
      this->groupBox4->Name = L"groupBox4";
      this->groupBox4->Size = System::Drawing::Size(83, 108);
      this->groupBox4->TabIndex = 4;
      this->groupBox4->TabStop = false;
      this->groupBox4->Text = L"Direction";
      // 
      // rbReverse
      // 
      this->rbReverse->AutoSize = true;
      this->rbReverse->Location = System::Drawing::Point(7, 66);
      this->rbReverse->Name = L"rbReverse";
      this->rbReverse->Size = System::Drawing::Size(65, 17);
      this->rbReverse->TabIndex = 3;
      this->rbReverse->Text = L"Reverse";
      this->rbReverse->UseVisualStyleBackColor = true;
      this->rbReverse->MouseUp += gcnew System::Windows::Forms::MouseEventHandler(this, &LocoSimForm::SetFocusToThrottle);
      // 
      // rbNeutral
      // 
      this->rbNeutral->AutoSize = true;
      this->rbNeutral->Checked = true;
      this->rbNeutral->Location = System::Drawing::Point(7, 43);
      this->rbNeutral->Name = L"rbNeutral";
      this->rbNeutral->Size = System::Drawing::Size(59, 17);
      this->rbNeutral->TabIndex = 2;
      this->rbNeutral->TabStop = true;
      this->rbNeutral->Text = L"Neutral";
      this->rbNeutral->UseVisualStyleBackColor = true;
      this->rbNeutral->MouseUp += gcnew System::Windows::Forms::MouseEventHandler(this, &LocoSimForm::SetFocusToThrottle);
      // 
      // rbForward
      // 
      this->rbForward->AutoSize = true;
      this->rbForward->Location = System::Drawing::Point(7, 20);
      this->rbForward->Name = L"rbForward";
      this->rbForward->Size = System::Drawing::Size(63, 17);
      this->rbForward->TabIndex = 1;
      this->rbForward->Text = L"Forward";
      this->rbForward->UseVisualStyleBackColor = true;
      this->rbForward->MouseUp += gcnew System::Windows::Forms::MouseEventHandler(this, &LocoSimForm::SetFocusToThrottle);
      // 
      // groupBox3
      // 
      this->groupBox3->Controls->Add(this->lAcc);
      this->groupBox3->Controls->Add(this->label7);
      this->groupBox3->Location = System::Drawing::Point(128, 73);
      this->groupBox3->Name = L"groupBox3";
      this->groupBox3->Size = System::Drawing::Size(94, 51);
      this->groupBox3->TabIndex = 3;
      this->groupBox3->TabStop = false;
      this->groupBox3->Text = L"Acceleration";
      // 
      // lAcc
      // 
      this->lAcc->AutoSize = true;
      this->lAcc->Font = (gcnew System::Drawing::Font(L"Microsoft Sans Serif", 14, System::Drawing::FontStyle::Regular, System::Drawing::GraphicsUnit::Point,
        static_cast<System::Byte>(0)));
      this->lAcc->Location = System::Drawing::Point(10, 20);
      this->lAcc->Name = L"lAcc";
      this->lAcc->Size = System::Drawing::Size(45, 24);
      this->lAcc->TabIndex = 1;
      this->lAcc->Text = L"0,12";
      // 
      // label7
      // 
      this->label7->AutoSize = true;
      this->label7->Location = System::Drawing::Point(59, 28);
      this->label7->Name = L"label7";
      this->label7->Size = System::Drawing::Size(31, 13);
      this->label7->TabIndex = 1;
      this->label7->Text = L"m/s2";
      // 
      // groupBox2
      // 
      this->groupBox2->Controls->Add(this->lSpeed);
      this->groupBox2->Controls->Add(this->label6);
      this->groupBox2->Location = System::Drawing::Point(128, 16);
      this->groupBox2->Name = L"groupBox2";
      this->groupBox2->Size = System::Drawing::Size(94, 51);
      this->groupBox2->TabIndex = 2;
      this->groupBox2->TabStop = false;
      this->groupBox2->Text = L"Speed";
      // 
      // lSpeed
      // 
      this->lSpeed->AutoSize = true;
      this->lSpeed->Font = (gcnew System::Drawing::Font(L"Microsoft Sans Serif", 14, System::Drawing::FontStyle::Regular, System::Drawing::GraphicsUnit::Point,
        static_cast<System::Byte>(0)));
      this->lSpeed->Location = System::Drawing::Point(10, 20);
      this->lSpeed->Name = L"lSpeed";
      this->lSpeed->Size = System::Drawing::Size(45, 24);
      this->lSpeed->TabIndex = 1;
      this->lSpeed->Text = L"25,0";
      // 
      // label6
      // 
      this->label6->AutoSize = true;
      this->label6->Location = System::Drawing::Point(59, 28);
      this->label6->Name = L"label6";
      this->label6->Size = System::Drawing::Size(32, 13);
      this->label6->TabIndex = 1;
      this->label6->Text = L"km/h";
      // 
      // groupBox1
      // 
      this->groupBox1->Controls->Add(this->bBrake);
      this->groupBox1->Controls->Add(this->bAccelerate);
      this->groupBox1->Controls->Add(this->bCoast);
      this->groupBox1->Controls->Add(this->label3);
      this->groupBox1->Controls->Add(this->label2);
      this->groupBox1->Controls->Add(this->label1);
      this->groupBox1->Controls->Add(this->bThrottle);
      this->groupBox1->Location = System::Drawing::Point(16, 16);
      this->groupBox1->Name = L"groupBox1";
      this->groupBox1->Size = System::Drawing::Size(106, 200);
      this->groupBox1->TabIndex = 1;
      this->groupBox1->TabStop = false;
      this->groupBox1->Text = L"Throttle";
      // 
      // bBrake
      // 
      this->bBrake->Location = System::Drawing::Point(7, 144);
      this->bBrake->Name = L"bBrake";
      this->bBrake->Size = System::Drawing::Size(25, 23);
      this->bBrake->TabIndex = 2;
      this->bBrake->Text = L"<";
      this->bBrake->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bBrake->UseVisualStyleBackColor = true;
      this->bBrake->MouseDown += gcnew System::Windows::Forms::MouseEventHandler(this, &LocoSimForm::bBrake_MouseDown);
      this->bBrake->MouseUp += gcnew System::Windows::Forms::MouseEventHandler(this, &LocoSimForm::bBrake_MouseUp);
      // 
      // bAccelerate
      // 
      this->bAccelerate->Location = System::Drawing::Point(69, 144);
      this->bAccelerate->Name = L"bAccelerate";
      this->bAccelerate->Size = System::Drawing::Size(25, 23);
      this->bAccelerate->TabIndex = 4;
      this->bAccelerate->Text = L">";
      this->bAccelerate->TextImageRelation = System::Windows::Forms::TextImageRelation::ImageBeforeText;
      this->bAccelerate->UseVisualStyleBackColor = true;
      this->bAccelerate->MouseDown += gcnew System::Windows::Forms::MouseEventHandler(this, &LocoSimForm::bAccelerate_MouseDown);
      this->bAccelerate->MouseUp += gcnew System::Windows::Forms::MouseEventHandler(this, &LocoSimForm::bAccelerate_MouseUp);
      // 
      // bCoast
      // 
      this->bCoast->Location = System::Drawing::Point(38, 144);
      this->bCoast->Name = L"bCoast";
      this->bCoast->Size = System::Drawing::Size(25, 23);
      this->bCoast->TabIndex = 3;
      this->bCoast->Text = L"=";
      this->bCoast->UseVisualStyleBackColor = true;
      this->bCoast->Click += gcnew System::EventHandler(this, &LocoSimForm::bCoast_Click);
      // 
      // label3
      // 
      this->label3->AutoSize = true;
      this->label3->Location = System::Drawing::Point(16, 72);
      this->label3->Name = L"label3";
      this->label3->Size = System::Drawing::Size(13, 13);
      this->label3->TabIndex = 1;
      this->label3->Text = L"0";
      // 
      // label2
      // 
      this->label2->AutoSize = true;
      this->label2->Location = System::Drawing::Point(16, 26);
      this->label2->Name = L"label2";
      this->label2->Size = System::Drawing::Size(25, 13);
      this->label2->TabIndex = 1;
      this->label2->Text = L"100";
      // 
      // label1
      // 
      this->label1->AutoSize = true;
      this->label1->Location = System::Drawing::Point(12, 116);
      this->label1->Name = L"label1";
      this->label1->Size = System::Drawing::Size(28, 13);
      this->label1->TabIndex = 1;
      this->label1->Text = L"-100";
      // 
      // bThrottle
      // 
      this->bThrottle->LargeChange = 20;
      this->bThrottle->Location = System::Drawing::Point(38, 26);
      this->bThrottle->Maximum = 100;
      this->bThrottle->Minimum = -100;
      this->bThrottle->Name = L"bThrottle";
      this->bThrottle->Orientation = System::Windows::Forms::Orientation::Vertical;
      this->bThrottle->Size = System::Drawing::Size(45, 104);
      this->bThrottle->SmallChange = 10;
      this->bThrottle->TabIndex = 1;
      this->bThrottle->TickFrequency = 25;
      this->bThrottle->MouseDown += gcnew System::Windows::Forms::MouseEventHandler(this, &LocoSimForm::bThrottle_MouseDown);
      // 
      // tabMainControl
      // 
      this->tabMainControl->Controls->Add(this->tabPage1);
      this->tabMainControl->Controls->Add(this->tabPage2);
      this->tabMainControl->Controls->Add(this->tabPage5);
      this->tabMainControl->Controls->Add(this->tabRailDevIF);
      this->tabMainControl->Controls->Add(this->brakePressure);
      this->tabMainControl->Controls->Add(this->tabPageAutoControl);
      this->tabMainControl->Controls->Add(this->tabPageDMIInterface);
      this->tabMainControl->Location = System::Drawing::Point(12, 32);
      this->tabMainControl->Name = L"tabMainControl";
      this->tabMainControl->SelectedIndex = 0;
      this->tabMainControl->Size = System::Drawing::Size(434, 454);
      this->tabMainControl->TabIndex = 1;
      // 
      // tabPageAutoControl
      // 
      this->tabPageAutoControl->Controls->Add(this->buttonSaveAutoControlParams);
      this->tabPageAutoControl->Controls->Add(this->buttonApplyAutoControlParams);
      this->tabPageAutoControl->Controls->Add(this->groupBoxAutoControlParams);
      this->tabPageAutoControl->Controls->Add(this->labelAutoControlState);
      this->tabPageAutoControl->Controls->Add(this->textBoxAutoControlState);
      this->tabPageAutoControl->Controls->Add(this->checkBoxAutoControl);
      this->tabPageAutoControl->Location = System::Drawing::Point(4, 22);
      this->tabPageAutoControl->Name = L"tabPageAutoControl";
      this->tabPageAutoControl->Size = System::Drawing::Size(426, 428);
      this->tabPageAutoControl->TabIndex = 6;
      this->tabPageAutoControl->Text = L"Auto Control";
      this->tabPageAutoControl->UseVisualStyleBackColor = true;
      // 
      // buttonSaveAutoControlParams
      // 
      this->buttonSaveAutoControlParams->Enabled = false;
      this->buttonSaveAutoControlParams->Location = System::Drawing::Point(261, 393);
      this->buttonSaveAutoControlParams->Name = L"buttonSaveAutoControlParams";
      this->buttonSaveAutoControlParams->Size = System::Drawing::Size(75, 23);
      this->buttonSaveAutoControlParams->TabIndex = 4;
      this->buttonSaveAutoControlParams->Text = L"Save";
      this->buttonSaveAutoControlParams->UseVisualStyleBackColor = true;
      this->buttonSaveAutoControlParams->Click += gcnew System::EventHandler(this, &LocoSimForm::buttonSaveAutoControlParams_Click);
      // 
      // buttonApplyAutoControlParams
      // 
      this->buttonApplyAutoControlParams->Enabled = false;
      this->buttonApplyAutoControlParams->Location = System::Drawing::Point(342, 393);
      this->buttonApplyAutoControlParams->Name = L"buttonApplyAutoControlParams";
      this->buttonApplyAutoControlParams->Size = System::Drawing::Size(75, 23);
      this->buttonApplyAutoControlParams->TabIndex = 5;
      this->buttonApplyAutoControlParams->Text = L"Apply";
      this->buttonApplyAutoControlParams->UseVisualStyleBackColor = true;
      this->buttonApplyAutoControlParams->Click += gcnew System::EventHandler(this, &LocoSimForm::buttonApplyAutoControlParams_Click);
      // 
      // groupBoxAutoControlParams
      // 
      this->groupBoxAutoControlParams->Controls->Add(this->textBoxBCAMargin);
      this->groupBoxAutoControlParams->Controls->Add(this->labelBCAMargin);
      this->groupBoxAutoControlParams->Controls->Add(this->textBoxSpeedLimit2);
      this->groupBoxAutoControlParams->Controls->Add(this->labelSpeedLimit2);
      this->groupBoxAutoControlParams->Controls->Add(this->labelBCAMarginUnit);
      this->groupBoxAutoControlParams->Controls->Add(this->textBoxSpeedLimit1);
      this->groupBoxAutoControlParams->Controls->Add(this->labelSpeedLimit2Unit);
      this->groupBoxAutoControlParams->Controls->Add(this->labelSpeedLimit1);
      this->groupBoxAutoControlParams->Controls->Add(this->labelSpeedLimit1Unit);
      this->groupBoxAutoControlParams->Location = System::Drawing::Point(13, 94);
      this->groupBoxAutoControlParams->Name = L"groupBoxAutoControlParams";
      this->groupBoxAutoControlParams->Size = System::Drawing::Size(277, 104);
      this->groupBoxAutoControlParams->TabIndex = 3;
      this->groupBoxAutoControlParams->TabStop = false;
      this->groupBoxAutoControlParams->Text = L"AutoControl parameters";
      // 
      // textBoxBCAMargin
      // 
      this->textBoxBCAMargin->Location = System::Drawing::Point(147, 70);
      this->textBoxBCAMargin->Name = L"textBoxBCAMargin";
      this->textBoxBCAMargin->Size = System::Drawing::Size(50, 20);
      this->textBoxBCAMargin->TabIndex = 3;
      this->textBoxBCAMargin->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::autoControlParam_Changed);
      // 
      // labelBCAMargin
      // 
      this->labelBCAMargin->AutoSize = true;
      this->labelBCAMargin->Location = System::Drawing::Point(15, 73);
      this->labelBCAMargin->Name = L"labelBCAMargin";
      this->labelBCAMargin->Size = System::Drawing::Size(65, 13);
      this->labelBCAMargin->TabIndex = 0;
      this->labelBCAMargin->Text = L"BCA margin:";
      // 
      // textBoxSpeedLimit2
      // 
      this->textBoxSpeedLimit2->Location = System::Drawing::Point(147, 44);
      this->textBoxSpeedLimit2->Name = L"textBoxSpeedLimit2";
      this->textBoxSpeedLimit2->Size = System::Drawing::Size(50, 20);
      this->textBoxSpeedLimit2->TabIndex = 2;
      this->textBoxSpeedLimit2->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::autoControlParam_Changed);
      // 
      // labelSpeedLimit2
      // 
      this->labelSpeedLimit2->AutoSize = true;
      this->labelSpeedLimit2->Location = System::Drawing::Point(15, 47);
      this->labelSpeedLimit2->Name = L"labelSpeedLimit2";
      this->labelSpeedLimit2->Size = System::Drawing::Size(70, 13);
      this->labelSpeedLimit2->TabIndex = 0;
      this->labelSpeedLimit2->Text = L"Speed limit 2:";
      // 
      // labelBCAMarginUnit
      // 
      this->labelBCAMarginUnit->AutoSize = true;
      this->labelBCAMarginUnit->Location = System::Drawing::Point(198, 73);
      this->labelBCAMarginUnit->Name = L"labelBCAMarginUnit";
      this->labelBCAMarginUnit->Size = System::Drawing::Size(12, 13);
      this->labelBCAMarginUnit->TabIndex = 1;
      this->labelBCAMarginUnit->Text = L"s";
      // 
      // textBoxSpeedLimit1
      // 
      this->textBoxSpeedLimit1->Location = System::Drawing::Point(147, 18);
      this->textBoxSpeedLimit1->Name = L"textBoxSpeedLimit1";
      this->textBoxSpeedLimit1->Size = System::Drawing::Size(50, 20);
      this->textBoxSpeedLimit1->TabIndex = 1;
      this->textBoxSpeedLimit1->TextChanged += gcnew System::EventHandler(this, &LocoSimForm::autoControlParam_Changed);
      // 
      // labelSpeedLimit2Unit
      // 
      this->labelSpeedLimit2Unit->AutoSize = true;
      this->labelSpeedLimit2Unit->Location = System::Drawing::Point(198, 47);
      this->labelSpeedLimit2Unit->Name = L"labelSpeedLimit2Unit";
      this->labelSpeedLimit2Unit->Size = System::Drawing::Size(15, 13);
      this->labelSpeedLimit2Unit->TabIndex = 1;
      this->labelSpeedLimit2Unit->Text = L"%";
      // 
      // labelSpeedLimit1
      // 
      this->labelSpeedLimit1->AutoSize = true;
      this->labelSpeedLimit1->Location = System::Drawing::Point(15, 21);
      this->labelSpeedLimit1->Name = L"labelSpeedLimit1";
      this->labelSpeedLimit1->Size = System::Drawing::Size(70, 13);
      this->labelSpeedLimit1->TabIndex = 0;
      this->labelSpeedLimit1->Text = L"Speed limit 1:";
      // 
      // labelSpeedLimit1Unit
      // 
      this->labelSpeedLimit1Unit->AutoSize = true;
      this->labelSpeedLimit1Unit->Location = System::Drawing::Point(198, 21);
      this->labelSpeedLimit1Unit->Name = L"labelSpeedLimit1Unit";
      this->labelSpeedLimit1Unit->Size = System::Drawing::Size(15, 13);
      this->labelSpeedLimit1Unit->TabIndex = 1;
      this->labelSpeedLimit1Unit->Text = L"%";
      // 
      // labelAutoControlState
      // 
      this->labelAutoControlState->AutoSize = true;
      this->labelAutoControlState->Location = System::Drawing::Point(9, 42);
      this->labelAutoControlState->Name = L"labelAutoControlState";
      this->labelAutoControlState->Size = System::Drawing::Size(32, 13);
      this->labelAutoControlState->TabIndex = 2;
      this->labelAutoControlState->Text = L"State";
      // 
      // textBoxAutoControlState
      // 
      this->textBoxAutoControlState->Location = System::Drawing::Point(12, 58);
      this->textBoxAutoControlState->Name = L"textBoxAutoControlState";
      this->textBoxAutoControlState->Size = System::Drawing::Size(100, 20);
      this->textBoxAutoControlState->TabIndex = 1;
      // 
      // checkBoxAutoControl
      // 
      this->checkBoxAutoControl->AutoSize = true;
      this->checkBoxAutoControl->Location = System::Drawing::Point(13, 17);
      this->checkBoxAutoControl->Name = L"checkBoxAutoControl";
      this->checkBoxAutoControl->Size = System::Drawing::Size(117, 17);
      this->checkBoxAutoControl->TabIndex = 0;
      this->checkBoxAutoControl->Text = L"Enable AutoControl";
      this->checkBoxAutoControl->UseVisualStyleBackColor = true;
      this->checkBoxAutoControl->Click += gcnew System::EventHandler(this, &LocoSimForm::checkBoxAutoControl_Click);
      // 
      // tabPageDMIInterface
      // 
      this->tabPageDMIInterface->Controls->Add(this->listViewDMIFields);
      this->tabPageDMIInterface->Location = System::Drawing::Point(4, 22);
      this->tabPageDMIInterface->Name = L"tabPageDMIInterface";
      this->tabPageDMIInterface->Size = System::Drawing::Size(426, 428);
      this->tabPageDMIInterface->TabIndex = 7;
      this->tabPageDMIInterface->Text = L"DMI Interface";
      this->tabPageDMIInterface->UseVisualStyleBackColor = true;
      // 
      // listViewDMIFields
      // 
      this->listViewDMIFields->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(3) {
        this->columnHeaderNumber,
          this->columnHeaderDesc, this->columnHeaderValue
      });
      this->listViewDMIFields->GridLines = true;
      this->listViewDMIFields->Items->AddRange(gcnew cli::array< System::Windows::Forms::ListViewItem^  >(33) {
        listViewItem1, listViewItem2,
          listViewItem3, listViewItem4, listViewItem5, listViewItem6, listViewItem7, listViewItem8, listViewItem9, listViewItem10, listViewItem11,
          listViewItem12, listViewItem13, listViewItem14, listViewItem15, listViewItem16, listViewItem17, listViewItem18, listViewItem19,
          listViewItem20, listViewItem21, listViewItem22, listViewItem23, listViewItem24, listViewItem25, listViewItem26, listViewItem27,
          listViewItem28, listViewItem29, listViewItem30, listViewItem31, listViewItem32, listViewItem33
      });
      this->listViewDMIFields->Location = System::Drawing::Point(3, 3);
      this->listViewDMIFields->Name = L"listViewDMIFields";
      this->listViewDMIFields->Size = System::Drawing::Size(300, 422);
      this->listViewDMIFields->TabIndex = 0;
      this->listViewDMIFields->UseCompatibleStateImageBehavior = false;
      this->listViewDMIFields->View = System::Windows::Forms::View::Details;
      // 
      // columnHeaderNumber
      // 
      this->columnHeaderNumber->Text = L"Field nr";
      this->columnHeaderNumber->Width = 48;
      // 
      // columnHeaderDesc
      // 
      this->columnHeaderDesc->Text = L"Description";
      this->columnHeaderDesc->Width = 169;
      // 
      // columnHeaderValue
      // 
      this->columnHeaderValue->Text = L"Value";
      this->columnHeaderValue->Width = 74;
      // 
      // LocoSimForm
      // 
      this->AutoScaleDimensions = System::Drawing::SizeF(6, 13);
      this->AutoScaleMode = System::Windows::Forms::AutoScaleMode::Font;
      this->ClientSize = System::Drawing::Size(446, 530);
      this->ControlBox = false;
      this->Controls->Add(this->panel1);
      this->Controls->Add(this->toolStrip2);
      this->Controls->Add(this->toolStrip1);
      this->Controls->Add(this->tabMainControl);
      this->FormBorderStyle = System::Windows::Forms::FormBorderStyle::FixedToolWindow;
      this->MaximizeBox = false;
      this->MaximumSize = System::Drawing::Size(462, 564);
      this->MinimizeBox = false;
      this->MinimumSize = System::Drawing::Size(462, 464);
      this->Name = L"LocoSimForm";
      this->Text = L"LocoSim";
      this->FormClosing += gcnew System::Windows::Forms::FormClosingEventHandler(this, &LocoSimForm::LocoSimForm_FormClosing);
      this->Load += gcnew System::EventHandler(this, &LocoSimForm::LocoSimForm_Load);
      this->Shown += gcnew System::EventHandler(this, &LocoSimForm::LocoSimForm_Shown);
      this->toolStrip1->ResumeLayout(false);
      this->toolStrip1->PerformLayout();
      this->toolStrip2->ResumeLayout(false);
      this->toolStrip2->PerformLayout();
      this->brakePressure->ResumeLayout(false);
      this->groupBox21->ResumeLayout(false);
      this->groupBox21->PerformLayout();
      this->groupBox20->ResumeLayout(false);
      this->groupBox20->PerformLayout();
      this->groupBox19->ResumeLayout(false);
      this->groupBox19->PerformLayout();
      this->tabRailDevIF->ResumeLayout(false);
      this->groupBox11->ResumeLayout(false);
      this->groupBox11->PerformLayout();
      this->tabPage5->ResumeLayout(false);
      this->panel3->ResumeLayout(false);
      this->panel4->ResumeLayout(false);
      this->panel4->PerformLayout();
      this->groupBox8->ResumeLayout(false);
      this->groupBox8->PerformLayout();
      this->groupBox17->ResumeLayout(false);
      this->groupBox17->PerformLayout();
      this->gbArguments->ResumeLayout(false);
      this->gbArguments->PerformLayout();
      this->groupBox12->ResumeLayout(false);
      this->groupBox12->PerformLayout();
      this->groupBox13->ResumeLayout(false);
      this->groupBox13->PerformLayout();
      this->gbAutoReg->ResumeLayout(false);
      this->gbAutoReg->PerformLayout();
      this->tabPage2->ResumeLayout(false);
      this->groupBox9->ResumeLayout(false);
      this->groupBox9->PerformLayout();
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->pbATOStart))->EndInit();
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->pbATP2Start))->EndInit();
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->pbATP1Start))->EndInit();
      this->groupBox14->ResumeLayout(false);
      this->groupBox16->ResumeLayout(false);
      this->groupBox16->PerformLayout();
      this->groupBox15->ResumeLayout(false);
      this->groupBox15->PerformLayout();
      this->tabPage1->ResumeLayout(false);
      this->groupBox18->ResumeLayout(false);
      this->groupBox7->ResumeLayout(false);
      this->groupBox6->ResumeLayout(false);
      this->groupBox6->PerformLayout();
      this->groupBox5->ResumeLayout(false);
      this->groupBox5->PerformLayout();
      this->groupBox4->ResumeLayout(false);
      this->groupBox4->PerformLayout();
      this->groupBox3->ResumeLayout(false);
      this->groupBox3->PerformLayout();
      this->groupBox2->ResumeLayout(false);
      this->groupBox2->PerformLayout();
      this->groupBox1->ResumeLayout(false);
      this->groupBox1->PerformLayout();
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->bThrottle))->EndInit();
      this->tabMainControl->ResumeLayout(false);
      this->tabPageAutoControl->ResumeLayout(false);
      this->tabPageAutoControl->PerformLayout();
      this->groupBoxAutoControlParams->ResumeLayout(false);
      this->groupBoxAutoControlParams->PerformLayout();
      this->tabPageDMIInterface->ResumeLayout(false);
      this->ResumeLayout(false);
      this->PerformLayout();

    }
#pragma endregion
    /**********************************************************
    * Function: startAOS
    * Description: 
    **********************************************************/
  public: void startAOS(void)
  {
      // Activate cabin 1
    bCabin1_Click(nullptr, nullptr);
      // Activate LCS Ready
    bLCSReady_Click(nullptr, nullptr);
      // Start ATP
    tsbAOSStart_Click(nullptr, nullptr);
  }
          /**********************************************************
          * Function: stopAOS
          * Description:
          **********************************************************/
  public: void stopAOS(void)
  {
    // Stop ATP
    LocoSimul->ShutDown() ;
  }


          /**********************************************************
          * Function: getBrakePressure 
          * Description: Get Simulated brake pressure in kPa
          **********************************************************/
 public: UInt16 getBrakePressure(void)
  {
    // Simulated brake pressure value
    return LocoSimul->brakePressureValue;
  }
          /**********************************************************
          * Function: stopAOS
          * Description:
          **********************************************************/
  public: void updateDMIFieldsView(array<String^>^ fields)
  {
    bool endOfFieldList = false;
    int index = 2; // Skip ok;atpinfo
    while (!endOfFieldList)
    {
      try
      {
        listViewDMIFields->Items[index - 2]->SubItems[2]->Text = fields[index];
        index++;
      }
      catch (...)
      {
        // End of fields
        endOfFieldList = true;
      }
    }
  }
          /**********************************************************
          * Function: updateAutoControl
          * Description:
          **********************************************************/
  public: void updateAutoControl(bool newStatus)
  {
    checkBoxAutoControl->Checked = newStatus;
    checkBoxAutoControl_Click(nullptr, nullptr);
  }
          /**********************************************************
          * Function: updateNCU
          * Description:
          **********************************************************/
  public: void updateNCU(bool newStatus)
  {
    if (LocoSimul->LocoNCU != newStatus)
      bNcu_Click(nullptr, nullptr);
  }

          /**********************************************************
          * Function: updateBaliseId for registration balise
          * Description:
          **********************************************************/
  public: void updateBaliseId(String^ baliseId)
  {
    tbRegBaliseDefault->Text = baliseId;
    bApplyParams_Click(nullptr, nullptr);
  }
          /**********************************************************
          * Function: updateATPInfo
          * Description:
          **********************************************************/
  public: void updateATPInfo(UInt16        newPermittedSpeed, 
                            EnumDriveDir  newPermittedDriveDir,
                            ATPModeEnum   newAtpMode,
                            UInt16        newDistanceToTarget,                   
                            UInt16        newDistanceToBCA,                     
                            UInt16        newTargetSpeed,
                            UInt16        newMAMargin,
                            Int16         newTrackGradient,
                            Int16         newEffectiveGradient,
                            UInt16        newBrakeability,
                            UInt16        newBrakeDelayEB,
                            UInt16        newBrakeDelaySB)
  {
    permittedSpeed    = newPermittedSpeed;
    permittedDriveDir = newPermittedDriveDir;  
    atpMode           = newAtpMode;
    distanceToTarget  = newDistanceToTarget;
    distanceToBCA     = newDistanceToBCA;
    targetSpeed       = newTargetSpeed;
    trackGradient     = newTrackGradient;
    effectiveGradient = newEffectiveGradient;
    brakeability      = newBrakeability;
    brakeDelayEB      = newBrakeDelayEB;
    brakeDelaySB      = newBrakeDelaySB;

    LocoSimul->autoControl->setMAMargin(newMAMargin);
  }


    /**********************************************************
    * Function: Tick
    * Description: Run one tick of simulation
    **********************************************************/
  public: void Tick(void)
  {

    LocoSimul->LocoIo->SetAOSConnectionActive(this->aosRunning);

    char *ATxStatus_str[] =
    {
    {"Stopped"},
    {"ReadyToRun"},
    {"Running"},
    {"ATxNoProcess"}
    };
    char *TBSystemStatus_str[] =
    {
    {"PendingStartUp"},
    {"StartUp"},
    {"Running"},
    {"Stopped"},
    {"QuitPrograms"}
    };
    char *EnumSelectedController_str[] =
    {
    {"Driver"},
    {"LCS"},
    {"RailDriver"},
    {"LCS (RD)"},
    };
    char *AutoReg_str[] =
    {
    {"AutoReg: Init"},
    {"AutoReg: Startup delay"},
    {"AutoReg: (1) Brk release"},
    {"AutoReg: (1) Acceleration"},
    {"AutoReg: (1) Coasting"},
    {"AutoReg: (2) Brk release"},
    {"AutoReg: (2) Acceleration"},
    {"AutoReg: (2) Coasting"},
    {"AutoReg: Stop delay"},
    {"AutoReg: Pending startup"},
    };

    char *AutoControl_str[] =
    {
      { "Inactive" },
      { "Idle" },
      { "Accelerate" },
      { "Coasting" },
      { "Decelerate" },
      { "Approach" }
    };

    int i;

    // Collect data to sent to LocoSimul
    EnumATOMODE tmpATOModeSwitch;
    if (rbSupervised->Checked)
      tmpATOModeSwitch = ATOSwitchSupv;
    else if (rbAutomatic->Checked)
      tmpATOModeSwitch = ATOSwitchAuto;
    else
      tmpATOModeSwitch = ATOSwitchMan;

    EnumDriveDir tmpDriveDir;
    if (rbForward->Checked)
      tmpDriveDir = DDForward;
    else if (rbReverse->Checked)
      tmpDriveDir = DDReverse;
    else
      tmpDriveDir = DDNeutral;

    if (SelCtrlDriver == LocoSimul->SelectedController)
    {
      if (AcceleratedPressed)
      {
        UPDATE_IF_DIFFERENT(bThrottle->Value, min(bThrottle->Value + 5, bThrottle->Maximum));
      }
      else if (BrakePressed)
      {
        UPDATE_IF_DIFFERENT(bThrottle->Value, max(bThrottle->Value - 5, bThrottle->Minimum));
      }
    }
    // Grey out Brake Pressure System
    autoSimData->Enabled = false;
    autoSimValue->Enabled = false;

    if ((manualSim->Checked))
    {
      bp1Entered->Enabled = true;
      bp2Entered->Enabled = true;
    }
    else
    {
      manualSim->Checked = false;
      bp1Entered->Enabled = true;
      bp2Entered->Enabled = true;
    }

    // 'Emergency Stop Active' Alert Value
    int EMSAlertValue = 700;

    if (manualSim->Checked)
    {
      manualSimPressed = true;
      try
      {
        bp1valueEntered = Convert::ToUInt16(bp1Entered->Text);
      }
      catch (...)
      {
        bp1valueEntered = 0; // Exception handling
      }

      try
      {
        bp2valueEntered = Convert::ToUInt16(bp2Entered->Text);
       
      }
      catch (...)
      {
        bp2valueEntered = 0; // Exception handling
      }

      // Emergency Stop Active Alert gets activated if any of the Brake Pressure Sensor comes down from the 'EMS Alert Value' i.e. '700'
      EMSActivated = ((bp1valueEntered < EMSAlertValue) || (bp2valueEntered < EMSAlertValue))  ? true : false;
    }
    else
    {
      automaticSimPressed = true;
      EMSActivated = false;
      if (!(LocoSimul->LocoOutputs[0]))
      {
        autoSimData->Text = "Emergency Brake";

        // Emergency Stop Active Alert gets activated in Automatic Simulation when 'Emergency Brakes' are applied
        EMSActivated = true;
      }
      else if (!(LocoSimul->LocoOutputs[3]))
      {
        autoSimData->Text = "Service Brake";
      }
      else
      {
        autoSimData->Text = "No Brake";
      }

      autoSimValue->Text = LocoSimul->brakePressureValue.ToString();
    }

    // TCO Offset handling
    EnumTcoFb tmpTcoFb;
    
    if (rbTcoNoFb->Checked)
      tmpTcoFb = TcoNoFb;
    else if (rbTcoOnlyFb->Checked)
      tmpTcoFb = TcoOnlyFb;
    else
      tmpTcoFb = TcoOrderAndFb;

    try
    {
      tcoFbOffsetEntered = Convert::ToUInt16(tbTcoFbOffset->Text);
    }
    catch (...)
    {
      tcoFbOffsetEntered = 0; // Exception handling
    }


    // Update LocoSimul
    LocoSimul->Tick(bThrottle->Value,
      tmpATOModeSwitch,
      tmpDriveDir,
      Cabin1Pressed,
      Cabin2Pressed,
      LCSReadyPressed,
      EMSActivated,
      AOSOffPressed,
      AOSStartRequested,
      AOSStopRequested,
      ATPStartRequested,
      ATOStartRequested,
      ATP1ToFatalFailurePressed,
      ThrottleChanged,
      NcuPressed,
      Eb1aPressed,
      Eb1bPressed,
      Eb2aPressed,
      Eb2bPressed,
      IsolAPressed,
      IsolBPressed,
      RoadMPressed,
      RailMPressed,
      automaticSimPressed,
      manualSimPressed,
      bp1valueEntered,
      bp2valueEntered,
      tmpTcoFb,
      tcoFbOffsetEntered,
      permittedSpeed,                     // From DMI Interface
      permittedDriveDir,                  // From DMI Interface
      atpMode,                            // From DMI Interface 
      distanceToTarget,                   // From DMI Interface
      distanceToBCA,                      // From DMI Interface
      targetSpeed,                        // From DMI Interface 
      trackGradient,                      // From DMI Interface 
      effectiveGradient,                  // From DMI Interface 
      brakeability,                       // From DMI Interface 
      brakeDelayEB,                       // From DMI Interface 
      brakeDelaySB                        // From DMI Interface 
    );

    // Clear all "one sample" items
    Cabin1Pressed = false;
    Cabin2Pressed = false;
    LCSReadyPressed = false;
    AOSOffPressed = false;
    AOSStartRequested = false;
    AOSStopRequested = false;
    ATPStartRequested = false;
    ATP2StartRequested = false;
    ATOStartRequested = false;
    ATP1ToFatalFailurePressed = false;
    ThrottleChanged = false;
    NcuPressed = false;
    Eb1aPressed = false;
    Eb1bPressed = false;
    Eb2aPressed = false;
    Eb2bPressed = false;
    IsolAPressed = false;
    IsolBPressed = false;
    RoadMPressed = false;
    RailMPressed = false;
    automaticSimPressed = false;
    //noBrakeAppliedPressed = false;
    //sbAppliedPressed = false;
    //ebAppliedPressed = false;
    manualSimPressed = false;
    bp1valueEntered = 0U;
    bp2valueEntered = 0U;

    // Update GUI from LocoSimul
    UPDATE_IF_DIFFERENT(lSpeed->Text, String::Format("{0:0.0}", (float)(LocoSimul->LocoSpeed) / 100 * 3.6));
    UPDATE_IF_DIFFERENT(lAcc->Text, String::Format("{0:0.00}", (float)(LocoSimul->LocoAcc) / 100));

    UPDATE_IF_DIFFERENT(bCabin1->BackColor, LocoSimul->LocoCabin1 ? Color::YellowGreen : DefaultButtonColor);
    UPDATE_IF_DIFFERENT(bCabin2->BackColor, LocoSimul->LocoCabin2 ? Color::YellowGreen : DefaultButtonColor);
    UPDATE_IF_DIFFERENT(bLCSReady->BackColor, LocoSimul->LocoLCSReady ? Color::YellowGreen : DefaultButtonColor);
    UPDATE_IF_DIFFERENT(bAOSOff->BackColor, LocoSimul->LocoAOSOff ? Color::OrangeRed : DefaultButtonColor);
    UPDATE_IF_DIFFERENT(bEMSAlert->BackColor, EMSActivated ? Color::OrangeRed : DefaultButtonColor);

    UPDATE_IF_DIFFERENT(bNcu->BackColor, LocoSimul->LocoNCU ? Color::YellowGreen : DefaultButtonColor);
    UPDATE_IF_DIFFERENT(bEb1a->BackColor, LocoSimul->LocoEb1a ? Color::YellowGreen : DefaultButtonColor);
    UPDATE_IF_DIFFERENT(bEb1b->BackColor, LocoSimul->LocoEb1b ? Color::OrangeRed : DefaultButtonColor);
    UPDATE_IF_DIFFERENT(bEb2a->BackColor, LocoSimul->LocoEb2a ? Color::YellowGreen : DefaultButtonColor);
    UPDATE_IF_DIFFERENT(bEb2b->BackColor, LocoSimul->LocoEb2b ? Color::OrangeRed : DefaultButtonColor);
    UPDATE_IF_DIFFERENT(bIsolA->BackColor, LocoSimul->LocoIsolA ? Color::YellowGreen : DefaultButtonColor);
    UPDATE_IF_DIFFERENT(bIsolB->BackColor, LocoSimul->LocoIsolB ? Color::OrangeRed : DefaultButtonColor);

    UPDATE_IF_DIFFERENT(bRoadM->BackColor, LocoSimul->LocoRoadM ? Color::YellowGreen : DefaultButtonColor);
    UPDATE_IF_DIFFERENT(bRailM->BackColor, LocoSimul->LocoRailM ? Color::YellowGreen : DefaultButtonColor);

    tslSimState->Text = gcnew String(TBSystemStatus_str[LocoSimul->SystemStatus]);
    lATP1State->Text = gcnew String(ATxStatus_str[LocoSimul->ATPStatus]);
    lATP2State->Text = gcnew String(ATxStatus_str[LocoSimul->ATPBStatus]);
    lATOState->Text = gcnew String(ATxStatus_str[LocoSimul->ATOStatus]);
    tslSelectedController->Text = gcnew String(EnumSelectedController_str[LocoSimul->SelectedController]);

    UPDATE_IF_DIFFERENT(tsbATP1Green->Visible, LocoSimul->ATPStatus == ATxRunning);
    UPDATE_IF_DIFFERENT(tsbATP1Red->Visible, !tsbATP1Green->Visible);
    UPDATE_IF_DIFFERENT(tsbATOGreen->Visible, LocoSimul->ATOStatus == ATxRunning);
    UPDATE_IF_DIFFERENT(tsbATORed->Visible, !tsbATOGreen->Visible);
    UPDATE_IF_DIFFERENT(tsbAOSStart->Visible, LocoSimul->AOSReadyToRun);
    UPDATE_IF_DIFFERENT(tsbAOSStop->Visible, !LocoSimul->AOSReadyToRun);
    UPDATE_IF_DIFFERENT(pbATP1Start->Visible, LocoSimul->ATPReadyToRun);
    UPDATE_IF_DIFFERENT(pbATOStart->Visible, LocoSimul->ATOReadyToRun);

    if (outputNames != LocoSimul->LocoOutputNames)
    {
      outputNames = LocoSimul->LocoOutputNames;

      clbOutputs->Items->Clear();
      for (i = 0; i < RECEIVED_OUTPUT_SIZE; i++)
        clbOutputs->Items->Add(outputNames[i]);
    }

    for (i = 0; i < clbOutputs->Items->Count; i++)
    {
      clbOutputs->SetItemChecked(i, LocoSimul->LocoOutputs[i]);
    }


    // AutoReg info
    if (LocoSimul->locoParams->AutoRegEnabled)
    {
      UPDATE_IF_DIFFERENT(tslAutoRegState->Visible, true);
      UPDATE_IF_DIFFERENT(tssAutoRegSep->Visible, true);
      if (LocoSimul->AOSAutomaticRegistrationInProgress)
      {
        tslAutoRegState->Text = gcnew String(AutoReg_str[LocoSimul->AutoRegState]);
      }
      else
      {
        UPDATE_IF_DIFFERENT(tslAutoRegState->Text, "AutoReg: Enabled");
      }
    }
    else
    {
      UPDATE_IF_DIFFERENT(tslAutoRegState->Visible, false);
      UPDATE_IF_DIFFERENT(tssAutoRegSep->Visible, false);
    }

    // AutoControl info
    if (LocoSimul->locoParams->autoControlEnabled)
    {
      UPDATE_IF_DIFFERENT(textBoxAutoControlState->Visible, true);
      UPDATE_IF_DIFFERENT(labelAutoControlState->Visible, true);
      textBoxAutoControlState->Text = gcnew String(AutoControl_str[LocoSimul->autoControl->getState()]);

    }
    else
    {
      UPDATE_IF_DIFFERENT(textBoxAutoControlState->Visible, false);
      UPDATE_IF_DIFFERENT(labelAutoControlState->Visible, false);

    }


    // Handle active driver controls
    // LocoSim GUI (driver)
    if ((SelCtrlDriver == LocoSimul->SelectedController) &&
      !LocoSimul->AOSAutomaticRegistrationInProgress && 
      !LocoSimul->AutoControlInProgress)
    {
      UPDATE_IF_DIFFERENT(bAccelerate->Enabled, true);
      UPDATE_IF_DIFFERENT(bCoast->Enabled, true);
      UPDATE_IF_DIFFERENT(bBrake->Enabled, true);
      UPDATE_IF_DIFFERENT(bThrottle->Enabled, true);
      UPDATE_IF_DIFFERENT(rbNeutral->Enabled, true);
      UPDATE_IF_DIFFERENT(rbForward->Enabled, true);
      UPDATE_IF_DIFFERENT(rbReverse->Enabled, true);
      // Only use if ATO enabled by AOS
      if (LocoSimul->ATOEnable ||
        ((LocoSimul->ATPStatus != ATxRunning)
          /* && (LocoSimul->ATOStatus != ATxRunning)*/))
      {
        UPDATE_IF_DIFFERENT(rbManual->Enabled, true);
        UPDATE_IF_DIFFERENT(rbSupervised->Enabled, true);
        UPDATE_IF_DIFFERENT(rbAutomatic->Enabled, true);
      }
      else
      {
        UPDATE_IF_DIFFERENT(rbManual->Checked, true);
        UPDATE_IF_DIFFERENT(rbManual->Enabled, false);
        UPDATE_IF_DIFFERENT(rbSupervised->Enabled, false);
        UPDATE_IF_DIFFERENT(rbAutomatic->Enabled, false);
      }
    }
    // RailDriver
    else if ((SelCtrlRailDriver == LocoSimul->SelectedController) ||
      (SelCtrlLCSRailDriver == LocoSimul->SelectedController) ||
      (LocoSimul->AOSAutomaticRegistrationInProgress))
    {
      if (!LocoSimul->AOSAutomaticRegistrationInProgress)
      {
        UPDATE_IF_DIFFERENT(bAccelerate->Enabled, false);
        UPDATE_IF_DIFFERENT(bCoast->Enabled, false);
        UPDATE_IF_DIFFERENT(bBrake->Enabled, false);
        UPDATE_IF_DIFFERENT(bThrottle->Enabled, false);
      }
      UPDATE_IF_DIFFERENT(bThrottle->Value, LocoSimul->LocoThrottle);
      UPDATE_IF_DIFFERENT(rbNeutral->Enabled, false);
      UPDATE_IF_DIFFERENT(rbForward->Enabled, false);
      UPDATE_IF_DIFFERENT(rbReverse->Enabled, false);
      UPDATE_IF_DIFFERENT(rbManual->Enabled, false);
      UPDATE_IF_DIFFERENT(rbSupervised->Enabled, false);
      UPDATE_IF_DIFFERENT(rbAutomatic->Enabled, false);
      if (LocoSimul->LocoDriveDir == DDNeutral)
      {
        UPDATE_IF_DIFFERENT(rbNeutral->Checked, true);
      }
      else if (LocoSimul->LocoDriveDir == DDForward)
      {
        UPDATE_IF_DIFFERENT(rbForward->Checked, true);
      }
      else if (LocoSimul->LocoDriveDir == DDReverse)
      {
        UPDATE_IF_DIFFERENT(rbReverse->Checked, true);
      }
      if (LocoSimul->LocoATOSwitch == ATOSwitchMan)
      {
        UPDATE_IF_DIFFERENT(rbManual->Checked, true);
      }
      else if (LocoSimul->LocoATOSwitch == ATOSwitchSupv)
      {
        UPDATE_IF_DIFFERENT(rbSupervised->Checked, true);
      }
      else if (LocoSimul->LocoATOSwitch == ATOSwitchAuto)
      {
        UPDATE_IF_DIFFERENT(rbAutomatic->Checked, true);
      }
    }
    // LCS
    else
    {
      UPDATE_IF_DIFFERENT(bAccelerate->Enabled, false);
      UPDATE_IF_DIFFERENT(bCoast->Enabled, false);
      UPDATE_IF_DIFFERENT(bBrake->Enabled, false);
      UPDATE_IF_DIFFERENT(bThrottle->Enabled, false);
      UPDATE_IF_DIFFERENT(bThrottle->Value, LocoSimul->LocoThrottle);
      UPDATE_IF_DIFFERENT(rbNeutral->Enabled, false);
      UPDATE_IF_DIFFERENT(rbForward->Enabled, false);
      UPDATE_IF_DIFFERENT(rbReverse->Enabled, false);
      UPDATE_IF_DIFFERENT(rbManual->Enabled, true);
      UPDATE_IF_DIFFERENT(rbSupervised->Enabled, true);
      UPDATE_IF_DIFFERENT(rbAutomatic->Enabled, true);
      if (LocoSimul->LocoDriveDir == DDNeutral)
      {
        UPDATE_IF_DIFFERENT(rbNeutral->Checked, true);
      }
      else if (LocoSimul->LocoDriveDir == DDForward)
      {
        UPDATE_IF_DIFFERENT(rbForward->Checked, true);
      }
      else if (LocoSimul->LocoDriveDir == DDReverse)
      {
        UPDATE_IF_DIFFERENT(rbReverse->Checked, true);
      }
    }

    // Disable buttons during AutoReg
    if (LocoSimul->AOSAutomaticRegistrationInProgress)
    {
      UPDATE_IF_DIFFERENT(bCabin1->Enabled, false);
      UPDATE_IF_DIFFERENT(bCabin2->Enabled, false);
      UPDATE_IF_DIFFERENT(bLCSReady->Enabled, false);
      UPDATE_IF_DIFFERENT(bAOSOff->Enabled, false);
      UPDATE_IF_DIFFERENT(bEMSAlert->Enabled, false);
    }
    else
    {
      // Capture when AutoReg is completed and "fix" current ATOModeSwitch/DriveDir
      // based on settings in LocoSimul
      if (!bCabin1->Enabled && !bCabin2->Enabled)
      {
        if (LocoSimul->LocoDriveDir == DDNeutral)
        {
          UPDATE_IF_DIFFERENT(rbNeutral->Checked, true);
        }
        else if (LocoSimul->LocoDriveDir == DDForward)
        {
          UPDATE_IF_DIFFERENT(rbForward->Checked, true);
        }
        else if (LocoSimul->LocoDriveDir == DDReverse)
        {
          UPDATE_IF_DIFFERENT(rbReverse->Checked, true);
        }

        // Only use if ATO enabled by AOS
        if (LocoSimul->ATOEnable)
        {
          if (LocoSimul->LocoATOSwitch == ATOSwitchMan)
          {
            UPDATE_IF_DIFFERENT(rbManual->Checked, true);
          }
          else if (LocoSimul->LocoATOSwitch == ATOSwitchSupv)
          {
            UPDATE_IF_DIFFERENT(rbSupervised->Checked, true);
          }
          else if (LocoSimul->LocoATOSwitch == ATOSwitchAuto)
          {
            UPDATE_IF_DIFFERENT(rbAutomatic->Checked, true);
          }
          UPDATE_IF_DIFFERENT(rbManual->Enabled, true);
          UPDATE_IF_DIFFERENT(rbSupervised->Enabled, true);
          UPDATE_IF_DIFFERENT(rbAutomatic->Enabled, true);
        }
        else
        {
          UPDATE_IF_DIFFERENT(rbManual->Checked, true);
          UPDATE_IF_DIFFERENT(rbManual->Enabled, false);
          UPDATE_IF_DIFFERENT(rbSupervised->Enabled, false);
          UPDATE_IF_DIFFERENT(rbAutomatic->Enabled, false);
        }
      }
      UPDATE_IF_DIFFERENT(bCabin1->Enabled, true);
      UPDATE_IF_DIFFERENT(bCabin2->Enabled, true);
      UPDATE_IF_DIFFERENT(bLCSReady->Enabled, true);
      UPDATE_IF_DIFFERENT(bAOSOff->Enabled, true);
      UPDATE_IF_DIFFERENT(bEMSAlert->Enabled, true);
    }


    // Update RailDriver interface tab
    if (LocoSimul->locoParams->LocoSimUseRailDev)
    {
      UPDATE_IF_DIFFERENT(tsbRDGreen->Visible, LocoSimul->rdCom->RDConnected);
      UPDATE_IF_DIFFERENT(tssRD->Visible, tsbRDGreen->Visible);
      UPDATE_IF_DIFFERENT(lRDConnStatus->Text, LocoSimul->rdCom->RDConnected ? "Connected" : "Not connected");
      UPDATE_IF_DIFFERENT(lRDRecTime->Text, LocoSimul->rdCom->RDLastRecMsgTime);
      UPDATE_IF_DIFFERENT(lRDRecStr->Text, LocoSimul->rdCom->RDLastRecString);
      UPDATE_IF_DIFFERENT(lRDSndTime->Text, LocoSimul->rdCom->RDLastSndMsgTime);
      UPDATE_IF_DIFFERENT(lRDSndStr->Text, LocoSimul->rdCom->RDLastSndString);
    }
    else
    {
      // If not enabled it shall be removed immediately
      tabMainControl->Controls->Remove(tabRailDevIF);
    }

    // Handle sound
    if ((LocoSimul->locoParams->UseBuzzerSound) &&
      (false == oldAOSBuzzer) &&
      (true == LocoSimul->AOSBuzzer))
    {
      SystemSounds::Beep->Play();
    }
    oldAOSBuzzer = LocoSimul->AOSBuzzer;

    // ReRegistration balise handling
    String ^tmpStr = LocoSimul->LastReadBaliseId.ToString() + (LocoSimul->LastReadBaliseForward ? "/Forw" : "/Rev");
    UPDATE_IF_DIFFERENT(tbReRegLastBalise->Text, tmpStr);
    UPDATE_IF_DIFFERENT(tbReRegSpecBalise->Enabled, rbReRegSpecBalise->Checked);
    if (rbReRegLastBalise->Checked)
    {
      LocoSimul->ReRegBaliseId = LocoSimul->LastReadBaliseId;
      LocoSimul->ReRegForward = LocoSimul->LastReadBaliseForward;
      LocoSimul->ReRegUsingSpecBaliseId = false;
    }
    else
    {
      try
      {
        LocoSimul->ReRegBaliseId = (int)(Convert::ToUInt16(tbReRegSpecBalise->Text));
        LocoSimul->ReRegUsingSpecBaliseId = true;
      }
      catch (...)
      {
        LocoSimul->ReRegBaliseId = 0; // Forces ATP1 to use default balise id (above)
        LocoSimul->ReRegUsingSpecBaliseId = true;
      }
    }

    // Export any process start requests
    if (LocoSimul->reqStartATP)
    {
      atp1ReqStart = true;
      atpFileName = LocoSimul->locoParams->ATPFile;
      atpArgs = "";
      if (LocoSimul->locoParams->UseBaliseSimFile)
      {
        atpArgs = "-sitedata=" + LocoSimul->locoParams->BaliseSimFileName + " ";
      }
      atpArgs += LocoSimul->locoParams->ATPArgs;
      LocoSimul->reqStartATP = false;
    }
    else
    {
      atp1ReqStart = false;
    }

    if (LocoSimul->reqStartATO)
    {
      atoReqStart = true;
      atoFileName = LocoSimul->locoParams->ATOFile;
      atoArgs = LocoSimul->locoParams->ATOArgs;
      LocoSimul->reqStartATO = false;
    }
    else
    {
      atoReqStart = false;
    }
  }
  private: System::Void bAOSOff_Click(System::Object^ sender, System::EventArgs^ e) {
    AOSOffPressed = true;
    bThrottle->Focus();
  }
  private: System::Void bCabin1_Click(System::Object^ sender, System::EventArgs^ e) {
    Cabin1Pressed = true;
    bThrottle->Focus();
  }
  private: System::Void bCabin2_Click(System::Object^ sender, System::EventArgs^ e) {
    Cabin2Pressed = true;
    bThrottle->Focus();
  }
  private: System::Void bLCSReady_Click(System::Object^ sender, System::EventArgs^ e) {
    LCSReadyPressed = true;
    bThrottle->Focus();
  }
  private: System::Void SetFocusToThrottle(System::Object^ sender, System::Windows::Forms::MouseEventArgs^ e) {
    bThrottle->Focus();
  }
  private: System::Void bBrake_MouseDown(System::Object^ sender, System::Windows::Forms::MouseEventArgs^ e) {
    BrakePressed = true;
    ThrottleChanged = true;
  }
  private: System::Void bBrake_MouseUp(System::Object^ sender, System::Windows::Forms::MouseEventArgs^ e) {
    BrakePressed = false;
    ThrottleChanged = true;
    bThrottle->Focus();
  }
  private: System::Void bAccelerate_KeyUp(System::Object^ sender, System::Windows::Forms::KeyEventArgs^ e) {
    bThrottle->Focus();
  }
  private: System::Void bAccelerate_MouseDown(System::Object^ sender, System::Windows::Forms::MouseEventArgs^ e) {
    AcceleratedPressed = true;
    ThrottleChanged = true;
  }
  private: System::Void bAccelerate_MouseUp(System::Object^ sender, System::Windows::Forms::MouseEventArgs^ e) {
    AcceleratedPressed = false;
    ThrottleChanged = true;
    bThrottle->Focus();
  }
  private: System::Void bCoast_Click(System::Object^ sender, System::EventArgs^ e) {
    bThrottle->Value = 0;
    ThrottleChanged = true;
    bThrottle->Focus();
  }
  private: System::Void bThrottle_MouseDown(System::Object^ sender, System::Windows::Forms::MouseEventArgs^ e) {
    ThrottleChanged = true;
  }
  private: System::Void pbATOStart_Click(System::Object^ sender, System::EventArgs^ e) {
    ATOStartRequested = true;
  }
  private: System::Void pbATP2Start_Click(System::Object^ sender, System::EventArgs^ e) {
    ATP2StartRequested = true;
  }
  private: System::Void pbATP1Start_Click(System::Object^ sender, System::EventArgs^ e) {
    ATPStartRequested = true;
  }
  private: System::Void bATP1ToFF_Click(System::Object^ sender, System::EventArgs^ e) {
    ATP1ToFatalFailurePressed = true;
  }
  private: System::Void ParamsChanged_Click(System::Object^ sender, System::EventArgs^ e) {
    bSaveParams->Enabled = true;
    bApplyParams->Enabled = true;
  }
  private: System::Void cbEnableSound_CheckedChanged(System::Object^ sender, System::EventArgs^ e) {
    bSaveParams->Enabled = true;
    bApplyParams->Enabled = true;
  }
  private: System::Void AutoRegEnable_Click(System::Object^ sender, System::EventArgs^ e) {
    bSaveParams->Enabled = true;
    bApplyParams->Enabled = true;

    if (cbAutoRegEnable->Checked)
    {
      tbAutoRegAcc->Enabled = true;
      tbAutoRegSpeed->Enabled = true;
      cbAutoRegDirection->Enabled = true;
      cbAutoRegATOMode->Enabled = true;
    }
    else
    {
      tbAutoRegAcc->Enabled = false;
      tbAutoRegSpeed->Enabled = false;
      cbAutoRegDirection->Enabled = false;
      cbAutoRegATOMode->Enabled = false;
    }
  }
  private: System::Void bSaveParams_Click(System::Object^ sender, System::EventArgs^ e) {
    bApplyParams_Click(sender, e);

    if (!bApplyParams->Enabled)
    {
      // Store only if accepted by Apply
      LocoSimul->locoParams->SaveToIniFile();
      bSaveParams->Enabled = false;
    }
  }
  private: System::Void bApplyParams_Click(System::Object^ sender, System::EventArgs^ e) {
    String^ ErrorMsg = "";
    System::Object^ ErrorCtrl = nullptr;

    try
    {
      ErrorMsg = "Max speed ";
      ErrorCtrl = (System::Object^)tbMaxSpeed;
      double tmpMaxSpeed = (double)(Convert::ToUInt16(tbMaxSpeed->Text));
      ErrorMsg = "Max acceleration ";
      ErrorCtrl = (System::Object^)tbMaxAcc;
      double tmpMaxAcc = (double)(Convert::ToUInt16(tbMaxAcc->Text));
      ErrorMsg = "Max retardation ";
      ErrorCtrl = (System::Object^)tbMaxRet;
      double tmpMaxRet = (double)(Convert::ToUInt16(tbMaxRet->Text));
      ErrorMsg = "EB retardation ";
      ErrorCtrl = (System::Object^)tbEBRet;
      double tmpEBRet = (double)(Convert::ToUInt16(tbEBRet->Text));
      ErrorMsg = "SB retardation ";
      ErrorCtrl = (System::Object^)tbSBRet;
      double tmpSBRet = (double)(Convert::ToUInt16(tbSBRet->Text));

      ErrorMsg = "AutoReg acceleration ";
      ErrorCtrl = (System::Object^)tbAutoRegAcc;
      int tmpAutoRegAcc = (int)(Convert::ToUInt16(tbAutoRegAcc->Text));
      ErrorMsg = "AutoReg speed ";
      ErrorCtrl = (System::Object^)tbAutoRegSpeed;
      int tmpAutoRegSpeed = (int)(Convert::ToUInt16(tbAutoRegSpeed->Text));

      ErrorMsg = "Registration balise ";
      ErrorCtrl = (System::Object^)tbRegBaliseDefault;
      int tmpRegBaliseDefault = (int)(Convert::ToUInt16(tbRegBaliseDefault->Text));

      // Set values as active in LocoParams
      LocoSimul->locoParams->MaxSpeed = tmpMaxSpeed;
      LocoSimul->locoParams->MaxAcc = tmpMaxAcc;
      LocoSimul->locoParams->MaxRet = tmpMaxRet;
      LocoSimul->locoParams->EBRet = tmpEBRet;
      LocoSimul->locoParams->SBRet = tmpSBRet;

      LocoSimul->locoParams->AutoRegEnabled = cbAutoRegEnable->Checked;
      LocoSimul->locoParams->AutoRegAcceleration = tmpAutoRegAcc;
      LocoSimul->locoParams->AutoRegSpeed = tmpAutoRegSpeed;
      LocoSimul->locoParams->AutoRegDirection = (EnumDriveDir)((int)(cbAutoRegDirection->SelectedIndex) + 2);
      LocoSimul->locoParams->AutoRegATOMode = (EnumATOMODE)((int)(cbAutoRegATOMode->SelectedIndex) + 1);

      LocoSimul->locoParams->UseBuzzerSound = cbEnableSound->Checked;

      LocoSimul->locoParams->ATPArgs = ATP1ArgBox->Text;
      LocoSimul->locoParams->ATOArgs = ATOArgBox->Text;


      LocoSimul->locoParams->UseBaliseSimFile = cbUseBaliseSimulationFile->Checked;
      LocoSimul->locoParams->BaliseSimFileName = tbBaliseSimFileName->Text;

      LocoSimul->locoParams->RegDefaultBaliseId = tmpRegBaliseDefault;
      tbRegUseBalise->Text = tbRegBaliseDefault->Text; // Copy updated value to "use" TB

      // Trigger actions within the LocoIO
      LocoSimul->LocoIo->locoSimApplyButtonPressed();

      bApplyParams->Enabled = false;
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
  private: System::Void tsbAOSStart_Click(System::Object^ sender, System::EventArgs^ e) {

    // Force Coast and Neutral when starting to avoid (hopefully) moving during startup
    rbNeutral->Checked = true;
    rbManual->Checked = true;
    AOSStartRequested = true;

    // Emergency Brakes NOT cut out at Start up
    LocoSimul->LocoCtrl->LocoEb1a = true;
    LocoSimul->LocoCtrl->LocoEb1b = false;
    LocoSimul->LocoCtrl->LocoEb2a = true;
    LocoSimul->LocoCtrl->LocoEb2b = false;

    //Isolation Switch in "Run" mode
    LocoSimul->LocoCtrl->LocoIsolA = true;
    LocoSimul->LocoCtrl->LocoIsolB = false;

  }
  private: System::Void tsbAOSStop_Click(System::Object^ sender, System::EventArgs^ e) {
    AOSStopRequested = true;

    //Restore EB-Cut out values
    LocoSimul->LocoCtrl->LocoEb1a = false;
    LocoSimul->LocoCtrl->LocoEb1b = false;
    LocoSimul->LocoCtrl->LocoEb2a = false;
    LocoSimul->LocoCtrl->LocoEb2b = false;

    //Restore Isolation Switch Inputs
    LocoSimul->LocoCtrl->LocoIsolA = false;
    LocoSimul->LocoCtrl->LocoIsolB = false;

  }
  private: System::Void LocoSimForm_Load(System::Object^ sender, System::EventArgs^ e) {

    // Setup parameter tab
    tbMaxSpeed->Text = LocoSimul->locoParams->MaxSpeed.ToString();
    tbMaxAcc->Text = LocoSimul->locoParams->MaxAcc.ToString();
    tbMaxRet->Text = LocoSimul->locoParams->MaxRet.ToString();
    tbEBRet->Text = LocoSimul->locoParams->EBRet.ToString();
    tbSBRet->Text = LocoSimul->locoParams->SBRet.ToString();

    cbAutoRegEnable->Checked = LocoSimul->locoParams->AutoRegEnabled;
    tbAutoRegAcc->Text = LocoSimul->locoParams->AutoRegAcceleration.ToString();
    tbAutoRegSpeed->Text = LocoSimul->locoParams->AutoRegSpeed.ToString();
    cbAutoRegDirection->SelectedIndex = LocoSimul->locoParams->AutoRegDirection - 2;
    cbAutoRegATOMode->SelectedIndex = LocoSimul->locoParams->AutoRegATOMode - 1;

    cbEnableSound->Checked = LocoSimul->locoParams->UseBuzzerSound;

    ATP1ArgBox->Text = LocoSimul->locoParams->ATPArgs;
    ATOArgBox->Text = LocoSimul->locoParams->ATOArgs;

    cbUseBaliseSimulationFile->Checked = LocoSimul->locoParams->UseBaliseSimFile;
    tbBaliseSimFileName->Text = LocoSimul->locoParams->BaliseSimFileName;

    tbRegBaliseDefault->Text = LocoSimul->locoParams->RegDefaultBaliseId.ToString();
    tbRegUseBalise->Text = tbRegBaliseDefault->Text;

    //Brake pressure
    bp1Entered->Text = LocoSimul->locoParams->noBrakeAppliedPressure.ToString();
    bp2Entered->Text = LocoSimul->locoParams->noBrakeAppliedPressure.ToString();
    lbBrakePressureSensor1->Text = L"kPa (Range: " + LocoSimul->locoParams->minimumBrakePressureRange.ToString() + " - " + LocoSimul->locoParams->MaxBrakePressure.ToString() + ")";
    lbBrakePressureSensor2->Text = L"kPa (Range: " + LocoSimul->locoParams->minimumBrakePressureRange.ToString() + " - " + LocoSimul->locoParams->MaxBrakePressure.ToString() + ")";

    bSaveParams->Enabled = false;
    bApplyParams->Enabled = false;

    // TCO
    rbTcoNoFb->Checked = false;
    rbTcoOnlyFb->Checked = false;
    rbTcoOrderAndFb->Checked = true;
    tbTcoFbOffset->Text = "0";

    // AutoControl
    checkBoxAutoControl->Checked = LocoSimul->locoParams->autoControlEnabled;
    textBoxSpeedLimit1->Text = LocoSimul->locoParams->autoControlSpeedLimit1Perc.ToString();
    textBoxSpeedLimit2->Text = LocoSimul->locoParams->autoControlSpeedLimit2Perc.ToString();
    textBoxBCAMargin->Text = LocoSimul->locoParams->autoControlBCAMarginSecs.ToString();

    buttonSaveAutoControlParams->Enabled = false;
    buttonApplyAutoControlParams->Enabled = false;

    // Set application title
    this->Text += LocoSimul->DLLVersion;
  }
  private: System::Void LocoSimForm_FormClosing(System::Object^ sender, System::Windows::Forms::FormClosingEventArgs^ e) {
    // Store windows position in registry
    //Registry::SetValue(registryRootKeyName, "Left",
    // String::Format("{0:0}", this->Left));
    //Registry::SetValue(registryRootKeyName, "Top",
    // String::Format("{0:0}", this->Top));

    // Shutdown simulation
    LocoSimul->ShutDown();
  }
  private: System::Void LocoSimForm_Shown(System::Object^ sender, System::EventArgs^ e) {
    this->Left = formLeft;
    this->Top = formTop;
    this->Visible = Convert::ToInt16(Registry::GetValue(regRootKey + "\\LocoSim", "Visible", "1")) != 0 ? true : false;
  }
  private: System::Void tbRegUseBalise_TextChanged(System::Object^ sender, System::EventArgs^ e) {
    try
    {
      LocoSimul->RegUseBaliseId = (int)(Convert::ToUInt16(tbRegUseBalise->Text));
    }
    catch (...)
    {
      LocoSimul->RegUseBaliseId = 4513; // Just to set something that does not cause FF ...
    }
  }
  private: System::Void bSelectBaliseSimFile_Click(System::Object^ sender, System::EventArgs^ e) {
    OpenFileDialog^ dialog = gcnew OpenFileDialog;

    dialog->InitialDirectory = ".\\";
    dialog->Filter = "All files (*.*)|*.*";
    dialog->FilterIndex = 1;
    dialog->RestoreDirectory = true;
    dialog->Title = "Select balise simulation file";

    if (dialog->ShowDialog() == System::Windows::Forms::DialogResult::OK)
    {
      // Update textbox with selected file name
      tbBaliseSimFileName->Text = dialog->FileName;
    }

  }
  private: System::Void label8_Click(System::Object^ sender, System::EventArgs^ e) {
  }
  private: System::Void clbDX_SelectedIndexChanged(System::Object^ sender, System::EventArgs^ e) {
  }
  private: System::Void button1_Click(System::Object^ sender, System::EventArgs^ e) {
  }
  private: System::Void clbOutput_SelectedIndexChanged(System::Object^ sender, System::EventArgs^ e) {
  }
  private: System::Void tsbATP1Red_Click(System::Object^ sender, System::EventArgs^ e) {
  }
  private: System::Void manualSim_CheckedChanged(System::Object^ sender, System::EventArgs^ e)
  {

  }

  private: System::Void bp1Entered_TextChanged(System::Object^ sender, System::EventArgs^ e) {
    try
    {
      bp1valueEntered = (int)(Convert::ToUInt16(bp1Entered->Text));
    }
    catch (...)
    {
      bp1valueEntered = 1000; // Just to set something that does not cause FF ...
    }
  }

  private: System::Void bp2Entered_TextChanged(System::Object^ sender, System::EventArgs^ e) {
    try
    {
      bp2valueEntered = (int)(Convert::ToUInt16(bp2Entered->Text));
    }
    catch (...)
    {
      bp2valueEntered = 1000; // Just to set something that does not cause FF ...
    }
  }

  private: System::Void button2_Click(System::Object^ sender, System::EventArgs^ e) {
  }
  private: System::Void bNcu_Click(System::Object^ sender, System::EventArgs^ e) {
    NcuPressed = true;
    bThrottle->Focus();
  }
  private: System::Void bEb1a_Click(System::Object^ sender, System::EventArgs^ e) {
    Eb1aPressed = true;
    bThrottle->Focus();
  }
  private: System::Void bEb1b_Click(System::Object^ sender, System::EventArgs^ e) {
    Eb1bPressed = true;
    bThrottle->Focus();
  }
  private: System::Void bEb2a_Click(System::Object^ sender, System::EventArgs^ e) {
    Eb2aPressed = true;
    bThrottle->Focus();
  }
  private: System::Void bEb2b_Click(System::Object^ sender, System::EventArgs^ e) {
    Eb2bPressed = true;
    bThrottle->Focus();
  }
  private: System::Void bIsolA_Click(System::Object^ sender, System::EventArgs^ e) {
    IsolAPressed = true;
    bThrottle->Focus();
  }
  private: System::Void bIsolB_Click(System::Object^ sender, System::EventArgs^ e) {
    IsolBPressed = true;
    bThrottle->Focus();
  }


  private: System::Void bRoadM_Click(System::Object^ sender, System::EventArgs^ e) {
    RoadMPressed = true;
    bThrottle->Focus();
  }
  private: System::Void bRailM_Click(System::Object^ sender, System::EventArgs^ e) {

    RailMPressed = true;
    bThrottle->Focus();
  }
  private: System::Void automaticSim_CheckedChanged(System::Object^ sender, System::EventArgs^ e) {

  }
  private: System::Void label9_Click(System::Object^ sender, System::EventArgs^ e) {
  }
  private: System::Void autoSimBrakeApplied_SelectedIndexChanged(System::Object^ sender, System::EventArgs^ e) {
    // selIndexBrakePressure = autoSimBrakeApplied->SelectedIndex;
  }
  private: System::Void label24_Click(System::Object^ sender, System::EventArgs^ e) {
  }
  private: System::Void groupBox10_Enter(System::Object^  sender, System::EventArgs^  e) {
  }
private: System::Void radioButton1_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void radioButton2_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void textBox1_TextChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void tcoNoFb_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void tslAutoRegState_Click(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void checkBoxAutoControl_Click(System::Object^  sender, System::EventArgs^  e) {
  LocoSimul->locoParams->autoControlEnabled = checkBoxAutoControl->Checked;
  LocoSimul->autoControl->setEnabled(LocoSimul->locoParams->autoControlEnabled);
}

private: System::Void buttonApplyAutoControlParams_Click(System::Object^  sender, System::EventArgs^  e) {
  String^ ErrorMsg = "";
  System::Object^ ErrorCtrl = nullptr;

  try
  {
    ErrorMsg = labelSpeedLimit1->Text;
    ErrorCtrl = (System::Object^)textBoxSpeedLimit1;
    UInt16 speedLimit1 = Convert::ToUInt16(textBoxSpeedLimit1->Text);

    ErrorMsg = labelSpeedLimit2->Text;
    ErrorCtrl = (System::Object^)textBoxSpeedLimit2;
    UInt16 speedLimit2 = Convert::ToUInt16(textBoxSpeedLimit2->Text);

    ErrorMsg = labelBCAMargin->Text;
    ErrorCtrl = (System::Object^)textBoxBCAMargin;
    UInt16 BCAMargin = Convert::ToUInt16(textBoxBCAMargin->Text);

    // Set values as active in LocoParams
    LocoSimul->locoParams->autoControlSpeedLimit1Perc = speedLimit1;
    LocoSimul->locoParams->autoControlSpeedLimit2Perc = speedLimit2;
    LocoSimul->locoParams->autoControlBCAMarginSecs = BCAMargin;

    buttonApplyAutoControlParams->Enabled = false;
    buttonSaveAutoControlParams->Enabled = true;

    LocoSimul->autoControl->setParam(BCAMargin, speedLimit1, speedLimit2);
  }
  catch (...)
  {
    if (ErrorCtrl != nullptr)
    {
      ((System::Windows::Forms::TextBox^)ErrorCtrl)->Focus();
      ((System::Windows::Forms::TextBox^)ErrorCtrl)->SelectAll();
    }
    System::Windows::Forms::MessageBox::Show(ErrorMsg + "Can not be converted to a valid integer.",
      "Error in AutoControl params",
      (MessageBoxButtons)0,
      MessageBoxIcon::Error);
  }

}

private: System::Void autoControlParam_Changed(System::Object^  sender, System::EventArgs^  e) {
  buttonApplyAutoControlParams->Enabled = true;
  buttonSaveAutoControlParams->Enabled = false;
}
  
private: System::Void buttonSaveAutoControlParams_Click(System::Object^  sender, System::EventArgs^  e) {
  if (!buttonApplyAutoControlParams->Enabled)
  {
    // Store only if accepted by Apply
    LocoSimul->locoParams->SaveToIniFile();
    buttonSaveAutoControlParams->Enabled = false;
  }

}
};
}
