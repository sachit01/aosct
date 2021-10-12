#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          Form1.h %
*
*  %version:       4 %
*
*  %created_by:    nsyed %
*
*  %date_created:  2017-05-03 17:03 %
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
* 2013-11-26    Antbäck     LocoName
* 2013-12-07    Antbäck     Reworked form position handling for LCSSim
* 2014-03-07    Antbäck     Added LocoSim
* 2014-03-23    Antbäck     Added ATx processes
* 2014-03-25    Antbäck     Corrected window size "Normal"
* 2013-04-03    Antbäck     Added DoubleBuffered = true
* 2014-06-13    Hidaji      Added SiteDataExt
* 2014-07-02    Hidaji      Changed sitedataext icon
* 2014-12-16    Antbäck     Corrected form and button size
* 2014-12-18    Antbäck     Rearranged Normal screen
*                           Sorted problem with save/load of Setup1/2 screens
* 2015-02-26    Antbäck     Changed save of setup1/2, again
*                           Partially implemented zOrder handling, does not interfere
*                           but is equally not yet working
* 2016-10-16    Marlundg    Support for new console connections and parameters for BHP
* 2017-03-10    Marlundg    Support for Dispatcher Console
*
*******************************************************************************/
#include <string>
#include "LocoSimForm.h"
#include "LCSSimForm.h"
#include "OBRDSimForm.h"
#include "SiteDataExtForm.h"
#include "ATxConsoleForm.h"
#include "RunAOSClass.h"
#include "RemoteConnection.h"
#include "DMIInterface.h"

namespace AOSPC {

   using namespace System;
   using namespace System::ComponentModel;
   using namespace System::Collections;
   using namespace System::Windows::Forms;
   using namespace System::Data;
   using namespace System::Drawing;
   using namespace Microsoft::Win32;
   using namespace System::Reflection;
   using namespace System::Diagnostics;
   using namespace System::Runtime::InteropServices;
   using namespace System::Text;

   /// <summary>
   /// Summary for Form1
   ///
   /// WARNING: If you change the name of this class, you will need to change the
   ///          'Resource File Name' property for the managed resource compiler tool
   ///          associated with all .resx files this class depends on.  Otherwise,
   ///          the designers will not be able to interact properly with localized
   ///          resources associated with this form.
   /// </summary>
   public ref class Form1 : public System::Windows::Forms::Form
   {

      LocoSimForm^    locoSimForm;
      LCSSimForm^     lcsSimForm;
      OBRDSimForm^    obrdSimForm;
      SiteDataExtForm^siteDataExtForm;
      ATxConsoleForm^ atpAForm;
      ATxConsoleForm^ atpBForm;
      ATxConsoleForm^ dispForm;
      ATxConsoleForm^ atoForm;
      String^         registryRootKeyName;
      String^         iniFileName;
      String^         obrdIniFileName;
      RunAOSClass^    runAOS;
      RemoteConnection^ remConnection;
      RemoteConnection^ remTestbedConnection;
      DMIInterface^ dmiInterface;
      // Simulation Mode
      EnumSimulationMode SimMode;
      //Window State
      EnumWindowState winState;

      //StartTrain Step
      StartTrainCommands regStep;
      bool isStartTrainInProgress;
      int maxTimeForStartTrainCommandProcessing;
      int counterForStartTrainCommandProcessing = 0;

      //Max time for verifying the ATP to go Configuration mode after power-up in re-registration(in secs)
      int         maxTimeforRereg = 15;
      int         counterformaxTimeforRereg = 0;
      bool        isLoginOK = false;
      bool        isReregStarted = false;

      String^          responseString;
      array<String^>^  remoteStringForStartTrain;
      int              counterForExecutingStartTrain = 0;
      //AOSPC is running at 100mesc
      int              noOfCycleExecutedinOneSec = 10;

      // Remote Port
      int  trainNumber;


      // Components used
      bool            LocoSimEnabled;
      bool            LocoSimVisible;
      bool            UseProcessForms;
      bool            LCSSimEnabled;
      bool            LCSSimVisible;
      bool            OBRDSimEnabled;
      bool            OBRDSimVisible;
      bool            SiteDataExtEnabled;
      bool            SiteDataExtmVisible;
      bool            isRemoteInterfaceEnabled;
      bool            isRemoteTestbedInterfaceEnabled;
      bool            isDMIInterfaceEnabled;

      String^         LocoName;
   private: System::Windows::Forms::ToolStripButton^  tsbATPB;
   private: System::Windows::Forms::ToolStripButton^  tsbATO;
   private: System::Windows::Forms::ToolStripSeparator^  toolStripSeparator1;
   private: System::Windows::Forms::ToolStripComboBox^  tscbSetup;
   private: System::Windows::Forms::ToolStripSeparator^  toolStripSeparator2;
   private: System::Windows::Forms::ToolStripSeparator^  toolStripSeparator3;
   private: System::Windows::Forms::ToolStripButton^  tsbOBRDSim;

   private: System::Windows::Forms::ToolStripButton^  tsbSiteDataExt;
   private: System::Windows::Forms::ToolStripButton^  tsbATPA;
   private: System::Windows::Forms::ToolStrip^  toolStrip1;
   private: System::Windows::Forms::ToolStripButton^  tsbLCSSim;
   private: System::Windows::Forms::ToolStripButton^  tsbLocoSim;
   private: System::Windows::Forms::Timer^  timer1;
   private: System::Windows::Forms::ToolStripSplitButton^  tssbSave;
   private: System::Windows::Forms::ToolStripMenuItem^  setup1ToolStripMenuItem;
   private: System::Windows::Forms::ToolStripMenuItem^  setup2ToolStripMenuItem;
   private: System::Windows::Forms::ToolStripButton^  tsbDISP;
   private: System::ComponentModel::IContainer^  components;

   public:
      Form1(array<System::String ^> ^args)
      {
         InitializeComponent();
         this->DoubleBuffered = true;

         // Get the ini file from function argument
         try
         {
            iniFileName = args[0];
         }
         catch (...)
         {
            iniFileName = Process::GetCurrentProcess()->MainModule->FileName;
            iniFileName = iniFileName->ToLower();
            obrdIniFileName = iniFileName->Replace(".exe", "_OBRDSimTracks.ini");
            iniFileName = iniFileName->Replace(".exe", ".ini");
         }

         //
         //TODO: Add the constructor code here
         //

         // Do not store zOrder until initialization done
         //storeZOrder = false;

         // Determine which components that shall be used
         // Create temporary file name
         char *tmpIniFile = (char *)Marshal::StringToHGlobalAnsi(iniFileName).ToPointer();
         char *tmpObrdIniFileName = (char *)Marshal::StringToHGlobalAnsi(obrdIniFileName).ToPointer();

         char tmpStr[200];
         GetPrivateProfileStringA("AOSPC", "LocoName", "Loco 1", tmpStr, sizeof(tmpStr), tmpIniFile);
         LocoName = Marshal::PtrToStringAnsi((IntPtr)tmpStr);

         SimMode = (EnumSimulationMode)GetPrivateProfileIntA("AOSPC", "SimulationMode", SimulationSil, tmpIniFile);
         winState = (EnumWindowState)GetPrivateProfileIntA("AOSPC", "WindowStateGUI", WindowStateNormal, tmpIniFile);

         LocoSimEnabled = GetPrivateProfileIntA("AOSPC", "LocoSimEnabled", 1, tmpIniFile) == 1 ? true : false;
         UseProcessForms = GetPrivateProfileIntA("AOSPC", "UseProcessForms", 1, tmpIniFile) == 1 ? true : false;
         isRemoteInterfaceEnabled = GetPrivateProfileIntA("RemoteInterface", "Enabled", 0, tmpIniFile) == 1 ? true : false;
         isRemoteTestbedInterfaceEnabled = GetPrivateProfileIntA("RemoteTestbedInterface", "Enabled", 0, tmpIniFile) == 1 ? true : false;
         isDMIInterfaceEnabled = GetPrivateProfileIntA("DMIInterface", "Enabled", 0, tmpIniFile) == 1 ? true : false;
         LCSSimEnabled = GetPrivateProfileIntA("AOSPC", "LCSSimEnabled", 1, tmpIniFile) == 1 ? true : false;
         OBRDSimEnabled = GetPrivateProfileIntA("AOSPC", "OBRDSimEnabled", 1, tmpIniFile) == 1 ? true : false;
         SiteDataExtEnabled = GetPrivateProfileIntA("AOSPC", "SiteDataExtEnabled", 1, tmpIniFile) == 1 ? true : false;

         // Set window position from Registry
         registryRootKeyName = "HKEY_CURRENT_USER\\Software\\Bombardier\\AOSPC\\" + LocoName;
         // Handle Z order
         //zOrder = gcnew array<String^>(10);
         //int i;
         //for (i = 0; i < zOrder->Length; i++)
         //{
         //    zOrder[i] = Convert::ToString(Registry::GetValue(registryRootKeyName, "zOrder" + i, ""));
         //}

         switch (winState)
         {
         case WindowStateMin:
            WindowState = FormWindowState::Minimized;
            break;
         case WindowStateMax:
            WindowState = FormWindowState::Maximized;
            break;
         case WindowStateNormal:
         default:
            WindowState = FormWindowState::Normal;
            break;
         }


         // OBRDSim 
         if (OBRDSimEnabled)
         {
            // Create form
            obrdSimForm = gcnew OBRDSimForm(registryRootKeyName, iniFileName, obrdIniFileName);
            // Set the parent form of the child window.
            obrdSimForm->MdiParent = this;
            obrdSimForm->Init();
            obrdSimForm->Show();
         }
         else
         {
            obrdSimForm = nullptr;
            tsbOBRDSim->Visible = false;
         }

         // LCSSim
         if (LCSSimEnabled)
         {
            // Create form
            lcsSimForm = gcnew LCSSimForm(registryRootKeyName, iniFileName, OBRDSimEnabled ? obrdSimForm->getSimulator() : nullptr);
            // Set the parent form of the child window.
            lcsSimForm->MdiParent = this;
            lcsSimForm->Init();
            lcsSimForm->Show();
         }
         else
         {
            lcsSimForm = nullptr;
            tsbLCSSim->Visible = false;
         }

         // SiteDataExt 
         if (SiteDataExtEnabled)
         {
            // Create form
            siteDataExtForm = gcnew SiteDataExtForm(registryRootKeyName, iniFileName);
            // Set the parent form of the child window.
            siteDataExtForm->MdiParent = this;
            siteDataExtForm->Init();
            siteDataExtForm->Show();
         }
         else
         {
            siteDataExtForm = nullptr;
            tsbSiteDataExt->Visible = false;
         }

         // LocoSim (placed last to appear "on top")
         if (LocoSimEnabled)
         {
            // Create form
            locoSimForm = gcnew LocoSimForm(registryRootKeyName, iniFileName, UseProcessForms, OBRDSimEnabled ? obrdSimForm->getSimulator() : nullptr);
            // Set the parent form of the child window.
            locoSimForm->MdiParent = this;
            if ((SimMode == SimulationHil) || (SimMode == SimulationSil))
            {
               locoSimForm->Init();
               locoSimForm->Show();
            }
            else if (SimMode == SimulationEmd)
            {
               locoSimForm->Init();
            }
            // RunAOSClass shall only be used if LocoSim is enabled
            if (UseProcessForms)
            {
               runAOS = gcnew RunAOSClass();

               runAOS->SimMode = SimMode;

               // Get the IPs and ports for console ports
               runAOS->ATPConsolePortToConnect = GetPrivateProfileIntA("AOSPC", "ATPConsolePortToConnect", 30165, tmpIniFile);

               GetPrivateProfileStringA("AOSPC", "ATPAIP", "10.11.12.13", tmpStr, sizeof(tmpStr), tmpIniFile);
               runAOS->ATPAIP = Marshal::PtrToStringAnsi((IntPtr)tmpStr);

               runAOS->ATPAConsolePortToConnect = GetPrivateProfileIntA("AOSPC", "ATPAConsolePortToConnect", 30165, tmpIniFile);

               GetPrivateProfileStringA("AOSPC", "ATPBIP", "10.11.12.14", tmpStr, sizeof(tmpStr), tmpIniFile);
               runAOS->ATPBIP = Marshal::PtrToStringAnsi((IntPtr)tmpStr);

               runAOS->ATPBConsolePortToConnect = GetPrivateProfileIntA("AOSPC", "ATPBConsolePortToConnect", 30165, tmpIniFile);

               GetPrivateProfileStringA("AOSPC", "DISPIP", "10.11.12.15", tmpStr, sizeof(tmpStr), tmpIniFile);
               runAOS->DISPIP = Marshal::PtrToStringAnsi((IntPtr)tmpStr);

               runAOS->DispConsolePortToConnect = GetPrivateProfileIntA("AOSPC", "DispConsolePortToConnect", 30167, tmpIniFile);


               // Create ATP/ATPA
               atpAForm = gcnew ATxConsoleForm(SimulationSil == SimMode ? "ATP" : "ATPA", registryRootKeyName, iniFileName);
               // Set the parent form of the child window.
               atpAForm->MdiParent = this;
               atpAForm->Init();

               // ATP/ATPA console is used in SIL/HIL/EMD/VSIM mode
               atpAForm->Show();

               // Create ATPB
               atpBForm = gcnew ATxConsoleForm("ATPB", registryRootKeyName, iniFileName);
               // Set the parent form of the child window.
               atpBForm->MdiParent = this;
               atpBForm->Init();

               // Create Dispatcher
               dispForm = gcnew ATxConsoleForm("Dispatcher", registryRootKeyName, iniFileName);
               // Set the parent form of the child window.
               dispForm->MdiParent = this;
               dispForm->Init();

               // ATPB and DISP console are only used in HIL, EMD and VSIM mode
               if (SimMode != SimulationSil)
               {
                  atpBForm->Show();
                  dispForm->Show();
               }

               // Create ATO
               atoForm = gcnew ATxConsoleForm("ATO", registryRootKeyName, iniFileName);
               // Set the parent form of the child window.
               atoForm->MdiParent = this;
               atoForm->Init();

               // TODO: Not in ATP-Limited
               //atoForm->Show();

               // Modify ATP Console buttons according to simulation mode
               tsbATPA->Text = SimulationSil == SimMode ? L"ATP" : L"ATPA";
               if ((SimulationHil == SimMode) || (SimulationEmd == SimMode) || (SimulationVSim == SimMode))
               {
                  tsbATPB->Visible = true;
                  tsbDISP->Visible = true;
               }

               // TODO: Not in ATP-Limited
               this->tsbATO->Visible = false;

            }
            else
            {
               runAOS = nullptr;
               tsbATPA->Visible = false;
               tsbATPB->Visible = false;
               tsbDISP->Visible = false;
               tsbATO->Visible = false;
            }
         }
         else
         {
            locoSimForm = nullptr;
            tsbLocoSim->Visible = false;
            runAOS = nullptr;
            atpAForm = nullptr;
            atpBForm = nullptr;
            dispForm = nullptr;
            atoForm = nullptr;
            tsbATPA->Visible = false;
            tsbATPB->Visible = false;
            tsbDISP->Visible = false;
            tsbATO->Visible = false;
         }

         if (isRemoteInterfaceEnabled)
         {
            //Get Remote Port from ini file
            int listenPortRemoteInterface = GetPrivateProfileIntA("RemoteInterface", "Port", 30198, tmpIniFile);
            remConnection = gcnew RemoteConnection(listenPortRemoteInterface);
         }
         if (isRemoteTestbedInterfaceEnabled)
         {
            //Get Remote Port from ini file
            int listenPortRemoteTestbedInterface = GetPrivateProfileIntA("RemoteTestbedInterface", "Port", 30195, tmpIniFile);
            remTestbedConnection = gcnew RemoteConnection(listenPortRemoteTestbedInterface);
         }
         if (isDMIInterfaceEnabled)
         {
            //Get DMI Remote Port from ini file
            int dmiPortToConnect = GetPrivateProfileIntA("DMIInterface", "Port", 30197, tmpIniFile);
            int dmiInternalPortToConnect = GetPrivateProfileIntA("DMIInterface", "InternalPort", 30196, tmpIniFile);
            char dmiIPToConnect[20];
            GetPrivateProfileStringA("DMIInterface", "IP", "127.0.0.1", dmiIPToConnect, sizeof(dmiIPToConnect), tmpIniFile);
            dmiInterface = gcnew DMIInterface(IPAddress::Parse(gcnew String(dmiIPToConnect)), dmiPortToConnect, dmiInternalPortToConnect);
         }

      }

   protected:
      /// <summary>
      /// Clean up any resources being used.
      /// </summary>
      ~Form1()
      {
         if (components)
         {
            delete components;
         }
      }
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
         this->components = (gcnew System::ComponentModel::Container());
         System::ComponentModel::ComponentResourceManager^  resources = (gcnew System::ComponentModel::ComponentResourceManager(Form1::typeid));
         this->toolStrip1 = (gcnew System::Windows::Forms::ToolStrip());
         this->tsbLocoSim = (gcnew System::Windows::Forms::ToolStripButton());
         this->tsbLCSSim = (gcnew System::Windows::Forms::ToolStripButton());
         this->tsbOBRDSim = (gcnew System::Windows::Forms::ToolStripButton());
         this->tsbSiteDataExt = (gcnew System::Windows::Forms::ToolStripButton());
         this->tsbATPA = (gcnew System::Windows::Forms::ToolStripButton());
         this->tsbATPB = (gcnew System::Windows::Forms::ToolStripButton());
         this->tsbDISP = (gcnew System::Windows::Forms::ToolStripButton());
         this->tsbATO = (gcnew System::Windows::Forms::ToolStripButton());
         this->toolStripSeparator3 = (gcnew System::Windows::Forms::ToolStripSeparator());
         this->toolStripSeparator1 = (gcnew System::Windows::Forms::ToolStripSeparator());
         this->tssbSave = (gcnew System::Windows::Forms::ToolStripSplitButton());
         this->setup1ToolStripMenuItem = (gcnew System::Windows::Forms::ToolStripMenuItem());
         this->setup2ToolStripMenuItem = (gcnew System::Windows::Forms::ToolStripMenuItem());
         this->tscbSetup = (gcnew System::Windows::Forms::ToolStripComboBox());
         this->toolStripSeparator2 = (gcnew System::Windows::Forms::ToolStripSeparator());
         this->timer1 = (gcnew System::Windows::Forms::Timer(this->components));
         this->toolStrip1->SuspendLayout();
         this->SuspendLayout();
         // 
         // toolStrip1
         // 
         this->toolStrip1->GripStyle = System::Windows::Forms::ToolStripGripStyle::Hidden;
         this->toolStrip1->Items->AddRange(gcnew cli::array< System::Windows::Forms::ToolStripItem^  >(13) {
            this->tsbLocoSim, this->tsbLCSSim,
               this->tsbOBRDSim, this->tsbSiteDataExt, this->tsbATPA, this->tsbATPB, this->tsbDISP, this->tsbATO, this->toolStripSeparator3,
               this->toolStripSeparator1, this->tssbSave, this->tscbSetup, this->toolStripSeparator2
         });
         this->toolStrip1->Location = System::Drawing::Point(0, 0);
         this->toolStrip1->Name = L"toolStrip1";
         this->toolStrip1->Size = System::Drawing::Size(674, 25);
         this->toolStrip1->TabIndex = 1;
         this->toolStrip1->Text = L"toolStrip1";
         // 
         // tsbLocoSim
         // 
         this->tsbLocoSim->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbLocoSim.Image")));
         this->tsbLocoSim->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->tsbLocoSim->Name = L"tsbLocoSim";
         this->tsbLocoSim->Size = System::Drawing::Size(73, 22);
         this->tsbLocoSim->Text = L"LocoSim";
         this->tsbLocoSim->Click += gcnew System::EventHandler(this, &Form1::tsbLocoSim_Click);
         // 
         // tsbLCSSim
         // 
         this->tsbLCSSim->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbLCSSim.Image")));
         this->tsbLCSSim->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->tsbLCSSim->Name = L"tsbLCSSim";
         this->tsbLCSSim->Size = System::Drawing::Size(67, 22);
         this->tsbLCSSim->Text = L"LCSSim";
         this->tsbLCSSim->Click += gcnew System::EventHandler(this, &Form1::tsbLCSSim_Click);
         // 
         // tsbOBRDSim
         // 
         this->tsbOBRDSim->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbOBRDSim.Image")));
         this->tsbOBRDSim->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->tsbOBRDSim->Name = L"tsbOBRDSim";
         this->tsbOBRDSim->Size = System::Drawing::Size(78, 22);
         this->tsbOBRDSim->Text = L"OBRDSim";
         this->tsbOBRDSim->Click += gcnew System::EventHandler(this, &Form1::tsbOBRDSim_Click);
         // 
         // tsbSiteDataExt
         // 
         this->tsbSiteDataExt->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbSiteDataExt.Image")));
         this->tsbSiteDataExt->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->tsbSiteDataExt->Name = L"tsbSiteDataExt";
         this->tsbSiteDataExt->Size = System::Drawing::Size(85, 22);
         this->tsbSiteDataExt->Text = L"SiteDataExt";
         this->tsbSiteDataExt->Click += gcnew System::EventHandler(this, &Form1::tsbSiteDataExt_Click);
         // 
         // tsbATPA
         // 
         this->tsbATPA->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbATPA.Image")));
         this->tsbATPA->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->tsbATPA->Name = L"tsbATPA";
         this->tsbATPA->Size = System::Drawing::Size(57, 22);
         this->tsbATPA->Text = L"ATPA";
         this->tsbATPA->Click += gcnew System::EventHandler(this, &Form1::tsbATP1_Click);
         // 
         // tsbATPB
         // 
         this->tsbATPB->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbATPB.Image")));
         this->tsbATPB->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->tsbATPB->Name = L"tsbATPB";
         this->tsbATPB->Size = System::Drawing::Size(56, 22);
         this->tsbATPB->Text = L"ATPB";
         this->tsbATPB->Click += gcnew System::EventHandler(this, &Form1::tsbATP2_Click);
         // 
         // tsbDISP
         // 
         this->tsbDISP->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbDISP.Image")));
         this->tsbDISP->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->tsbDISP->Name = L"tsbDISP";
         this->tsbDISP->Size = System::Drawing::Size(51, 22);
         this->tsbDISP->Text = L"DISP";
         this->tsbDISP->Click += gcnew System::EventHandler(this, &Form1::tsbDISP_Click);
         // 
         // tsbATO
         // 
         this->tsbATO->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbATO.Image")));
         this->tsbATO->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->tsbATO->Name = L"tsbATO";
         this->tsbATO->Size = System::Drawing::Size(51, 22);
         this->tsbATO->Text = L"ATO";
         this->tsbATO->Click += gcnew System::EventHandler(this, &Form1::tsbATO_Click);
         // 
         // toolStripSeparator3
         // 
         this->toolStripSeparator3->Name = L"toolStripSeparator3";
         this->toolStripSeparator3->Size = System::Drawing::Size(6, 25);
         // 
         // toolStripSeparator1
         // 
         this->toolStripSeparator1->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
         this->toolStripSeparator1->Name = L"toolStripSeparator1";
         this->toolStripSeparator1->Size = System::Drawing::Size(6, 25);
         // 
         // tssbSave
         // 
         this->tssbSave->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
         this->tssbSave->DisplayStyle = System::Windows::Forms::ToolStripItemDisplayStyle::Text;
         this->tssbSave->DropDownItems->AddRange(gcnew cli::array< System::Windows::Forms::ToolStripItem^  >(2) {
            this->setup1ToolStripMenuItem,
               this->setup2ToolStripMenuItem
         });
         this->tssbSave->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tssbSave.Image")));
         this->tssbSave->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->tssbSave->Name = L"tssbSave";
         this->tssbSave->Size = System::Drawing::Size(47, 22);
         this->tssbSave->Text = L"Save";
         // 
         // setup1ToolStripMenuItem
         // 
         this->setup1ToolStripMenuItem->Name = L"setup1ToolStripMenuItem";
         this->setup1ToolStripMenuItem->Size = System::Drawing::Size(110, 22);
         this->setup1ToolStripMenuItem->Text = L"Setup1";
         this->setup1ToolStripMenuItem->Click += gcnew System::EventHandler(this, &Form1::setup1ToolStripMenuItem_Click);
         // 
         // setup2ToolStripMenuItem
         // 
         this->setup2ToolStripMenuItem->Name = L"setup2ToolStripMenuItem";
         this->setup2ToolStripMenuItem->Size = System::Drawing::Size(110, 22);
         this->setup2ToolStripMenuItem->Text = L"Setup2";
         this->setup2ToolStripMenuItem->Click += gcnew System::EventHandler(this, &Form1::setup2ToolStripMenuItem_Click);
         // 
         // tscbSetup
         // 
         this->tscbSetup->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
         this->tscbSetup->DropDownStyle = System::Windows::Forms::ComboBoxStyle::DropDownList;
         this->tscbSetup->Items->AddRange(gcnew cli::array< System::Object^  >(5) { L"Small", L"Normal", L"Developer", L"Setup1", L"Setup2" });
         this->tscbSetup->Name = L"tscbSetup";
         this->tscbSetup->Size = System::Drawing::Size(80, 25);
         this->tscbSetup->SelectedIndexChanged += gcnew System::EventHandler(this, &Form1::tscbSetup_SelectedIndexChanged);
         // 
         // toolStripSeparator2
         // 
         this->toolStripSeparator2->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
         this->toolStripSeparator2->Name = L"toolStripSeparator2";
         this->toolStripSeparator2->Size = System::Drawing::Size(6, 25);
         // 
         // timer1
         // 
         this->timer1->Enabled = true;
         this->timer1->Tick += gcnew System::EventHandler(this, &Form1::timer1_Tick);
         // 
         // Form1
         // 
         this->AutoScaleDimensions = System::Drawing::SizeF(6, 13);
         this->AutoScaleMode = System::Windows::Forms::AutoScaleMode::Font;
         this->ClientSize = System::Drawing::Size(674, 608);
         this->Controls->Add(this->toolStrip1);
         this->Icon = (cli::safe_cast<System::Drawing::Icon^>(resources->GetObject(L"$this.Icon")));
         this->IsMdiContainer = true;
         this->Name = L"Form1";
         this->Text = L"AOS-PC";
         this->FormClosing += gcnew System::Windows::Forms::FormClosingEventHandler(this, &Form1::Form1_FormClosing);
         this->Load += gcnew System::EventHandler(this, &Form1::Form1_Load);
         this->MdiChildActivate += gcnew System::EventHandler(this, &Form1::Form1_MdiChildActivate);
         this->Shown += gcnew System::EventHandler(this, &Form1::Form1_Shown);
         this->Resize += gcnew System::EventHandler(this, &Form1::Form1_Resize);
         this->toolStrip1->ResumeLayout(false);
         this->toolStrip1->PerformLayout();
         this->ResumeLayout(false);
         this->PerformLayout();

      }
#pragma endregion
   private: System::Void tsbLocoSim_Click(System::Object^  sender, System::EventArgs^  e) {
      if (locoSimForm != this->ActiveMdiChild &&
         locoSimForm->Visible)
      {
         locoSimForm->Focus();
      }
      else
      {
         if ((LocoSimEnabled) &&
            (locoSimForm != nullptr))
         {
            // Toggle visibility
            locoSimForm->SetVisibility(!locoSimForm->Visible);
         }
      }
   }
   private: System::Void tsbLCSSim_Click(System::Object^  sender, System::EventArgs^  e) {
      if (lcsSimForm != this->ActiveMdiChild &&
         lcsSimForm->Visible)
      {
         lcsSimForm->Focus();
      }
      else
      {
         if ((LCSSimEnabled) &&
            (lcsSimForm != nullptr))
         {
            // Toggle visibility
            lcsSimForm->SetVisibility(!lcsSimForm->Visible);
         }
      }
   }
   private: System::Void tsbOBRDSim_Click(System::Object^  sender, System::EventArgs^  e) {
      if (obrdSimForm != this->ActiveMdiChild &&
         obrdSimForm->Visible)
      {
         obrdSimForm->Focus();
      }
      else
      {
         if ((OBRDSimEnabled) &&
            (obrdSimForm != nullptr))
         {
            // Toggle visibility
            obrdSimForm->SetVisibility(!obrdSimForm->Visible);
         }
      }
   }

   private: System::Void tsbSiteDataExt_Click(System::Object^  sender, System::EventArgs^  e) {
      if (siteDataExtForm != this->ActiveMdiChild &&
         siteDataExtForm->Visible)
      {
         siteDataExtForm->Focus();
      }
      else
      {
         if ((SiteDataExtEnabled) &&
            (siteDataExtForm != nullptr))
         {
            // Toggle visibility
            siteDataExtForm->SetVisibility(!siteDataExtForm->Visible);
         }
      }
   }
   private: System::Void tsbATP1_Click(System::Object^  sender, System::EventArgs^  e) {
      if (atpAForm != this->ActiveMdiChild &&
         atpAForm->Visible)
      {
         atpAForm->Focus();
      }
      else
      {
         atpAForm->SetVisibility(!atpAForm->Visible);
      }
   }
   private: System::Void tsbATP2_Click(System::Object^  sender, System::EventArgs^  e) {
      if (atpBForm != this->ActiveMdiChild &&
         atpBForm->Visible)
      {
         atpBForm->Focus();
      }
      else
      {
         atpBForm->SetVisibility(!atpBForm->Visible);
      }
   }
   private: System::Void tsbATO_Click(System::Object^  sender, System::EventArgs^  e) {
      if (atoForm != this->ActiveMdiChild &&
         atoForm->Visible)
      {
         atoForm->Focus();
      }
      else
      {
         atoForm->SetVisibility(!atoForm->Visible);
      }
   }


  /**********************************************************
   * Function:     handleRemoteMinimizeRestoreCommand
   * Description:  Returns true if command recognized
   *
   **********************************************************/   
  private: bool handleRemoteMinimizeRestoreCommand(String^ commandString)
  {
    if ((commandString == "minimize") || (commandString == "restore"))
    {
      //Process the minimise or restore GUI operation
      if (commandString == "minimize")
      {
        WindowState = FormWindowState::Minimized;
      }
      else //commandString == "restore"
      {
        WindowState = FormWindowState::Normal;
      }
       //Send the same command to DMI for processing hide/restore functionality.
      String^ sendString = commandString + gcnew String("\r\n");
      dmiInterface->sendCommandToDMI(sendString->Length, Encoding::UTF8->GetBytes(sendString));

      return true;
    }
    else
    {
      return false;
    }
  }

  /**********************************************************
   * Function:     handleRemoteStopCommand
   * Description:  Returns true if command recognized
   *
   **********************************************************/
  private: bool handleRemoteStopCommand(String^ commandString)
  {
    if (commandString == "stop")
    {
      locoSimForm->stopAOS();

      //Send  DMIClose command to DMI for close itself
      String^ sendDMIString = gcnew String("DMIClose") + gcnew String("\r\n");
      dmiInterface->sendCommandToDMI(sendDMIString->Length, Encoding::UTF8->GetBytes(sendDMIString));

      // Write the acknowledgment before closing the application
      String^ sendString = gcnew String("Ok");
      remConnection->sendResponseToRemote(sendString->Length, Encoding::UTF8->GetBytes(sendString));

      //Wait for some time 
      static const int waitTimeBeforeClosingAOSPC = 500;
      Sleep(waitTimeBeforeClosingAOSPC);

      //Kill the process for remote connection
      static const int exitCodeForClosingAOSPC = 1;
      remConnection->terminateAOSPCProcess(GetCurrentProcessId(), exitCodeForClosingAOSPC);

      return true;

    }
    else
    {
      return false;
    }

 }

  /**********************************************************
   * Function:     handleRemoteStartCommand
   * Description:  Returns true if command recognized
   *
   **********************************************************/
  private: bool handleRemoteStartCommand(String^ commandString)
  {
    if (commandString == "start")
    {
      //Start the AOSPC processing
      locoSimForm->startAOS();
      String^ sendString = gcnew String("Ok");
      remConnection->sendResponseToRemote(sendString->Length, Encoding::UTF8->GetBytes(sendString));

      return true;
    }
    else
    {
      return false;
    }
  }
  /**********************************************************
   * Function:     handleRemoteStartTrainCommand
   * Description:  Returns true if command recognized
   *
   **********************************************************/
  private: bool handleRemoteStartTrainCommand(array<String^>^ fieldsFromRemote)
  {
    if (fieldsFromRemote[0] == "starttrain")
    {
      //Start the AOSPC processing
      locoSimForm->startAOS();
//      remoteStringForStartTrain = commandString->Split(' ');
      remoteStringForStartTrain = fieldsFromRemote;

      regStep = StartTrainCommandRegArea;
      maxTimeForStartTrainCommandProcessing = Convert::ToUInt16(remoteStringForStartTrain[StartTrainArgTimeout]) * noOfCycleExecutedinOneSec;
      counterForStartTrainCommandProcessing = 0;
      counterForExecutingStartTrain = 0;
      isStartTrainInProgress = true;

      //No of cars processing.
      int noOfCars = Convert::ToInt16(remoteStringForStartTrain[StartTrainArgAutoConfigCars]);

      int undefinedNoOfCars = -1;

      if (noOfCars > undefinedNoOfCars)
      {
        char *tmpIniFile = (char *)Marshal::StringToHGlobalAnsi(iniFileName).ToPointer();
        //get the Offset for No of cars.
        UInt16 CarsOffSet = GetPrivateProfileIntA("LCSTrainComp", "RoadNumber_1", maxVehicles, tmpIniFile);

        //Clear the content for the LCSTrainComp section from ini file.
        WritePrivateProfileStringA("LCSTrainComp", NULL, NULL, tmpIniFile);

        WritePrivateProfileStringA("LCSTrainComp", "DetectedByECPPosUnknown", "0", tmpIniFile);
        WritePrivateProfileStringA("LCSTrainComp", "NotDetectedByECP", "0", tmpIniFile);

        //Add the Locomotive at the beginning of train consist.
        WritePrivateProfileStringA("LCSTrainComp", "RoadNumber_1", (char *)Marshal::StringToHGlobalAnsi(CarsOffSet.ToString()).ToPointer(), tmpIniFile);
        WritePrivateProfileStringA("LCSTrainComp", "VehicleType_1", "1", tmpIniFile);

        // Save as many vehicles that are given in the parameter
        for (int i = 1; i <= noOfCars; i++)
        {
          String^ roadNumberParam("RoadNumber_");
          String^ vehicleTypeParam("VehicleType_");

          roadNumberParam += (i + 1);
          vehicleTypeParam += (i + 1);

          // Parameter-names
          char *tmpRoadNumberParam = (char *)Marshal::StringToHGlobalAnsi(roadNumberParam).ToPointer();
          char *tmpVehicleTypeParam = (char *)Marshal::StringToHGlobalAnsi(vehicleTypeParam).ToPointer();

          // Values
          char *tmpCurrRoadNumber = (char *)Marshal::StringToHGlobalAnsi((CarsOffSet + i).ToString()).ToPointer();
          char *tmpCurrVehicleType = (char *)Marshal::StringToHGlobalAnsi("2").ToPointer();

          WritePrivateProfileStringA("LCSTrainComp", tmpRoadNumberParam, tmpCurrRoadNumber, tmpIniFile);
          WritePrivateProfileStringA("LCSTrainComp", tmpVehicleTypeParam, tmpCurrVehicleType, tmpIniFile);
        }

        //Update the ECPBTrainComp with No of cars and locomotive
        int noOfLocomotive = 1;
        lcsSimForm->lcsSim->currNumberOfVehicles = noOfCars + noOfLocomotive;
        lcsSimForm->ProcessECPBTrainComposition(iniFileName, false);
        lcsSimForm->UpdateECPBTrainCompFromGUI();
      }

      return true;
    }
    else
    {
      return false;
    }
  }

  /**********************************************************
   * Function:     handleRemoteAutoControlCommand
   * Description:  Returns true if command recognized
   *
   **********************************************************/
   private: bool handleRemoteAutoControlCommand(array<String^>^ fieldsFromRemote)
   {
    if (fieldsFromRemote[0] == "autocontrol")
    {
      bool newStatus;
      bool legalArg;
      try
      {
        if (fieldsFromRemote[1]->ToLower() == "on")  // arg is On/Off
        {
          newStatus = true;
          legalArg = true;
        }
        else if (fieldsFromRemote[1]->ToLower() == "off")
        {
          newStatus = false;
          legalArg = true;
        }
        else
        {
          newStatus = false;
          legalArg = false;
        }
      }
      catch (...)
      {
        newStatus = false;
        legalArg = false;
      }

      String^ sendString;
      if (legalArg)
      {
        if (legalArg)
        {
          locoSimForm->updateAutoControl(newStatus);
          sendString = gcnew String("Ok");
        }
        else
        {
          sendString = gcnew String("Fail");
        }
      }
      else
      {
        sendString = gcnew String("Fail");
      }
      remConnection->sendResponseToRemote(sendString->Length, Encoding::UTF8->GetBytes(sendString));

      return true;
    }
    else
    {
      return false;
    }
  }

 /**********************************************************
  * Function:     handleRemoteSetRegBaliseCommand
  * Description:  Returns true if command recognized
  *
  **********************************************************/
  private: bool handleRemoteSetRegBaliseCommand(array<String^>^ fieldsFromRemote)
  {
    if (fieldsFromRemote[0] == "setregbaliseid")
    {
      UInt16 baliseId;
      try
      {
        baliseId = Convert::ToUInt16(fieldsFromRemote[1]);  // arg is baliseId
      }
      catch (...)
      {
        baliseId = 0;
      }

      String^ sendString;
      if (baliseId > 0)
      {                       // update the field with the string corresponding baliseId
        locoSimForm->updateBaliseId(fieldsFromRemote[1]);
        sendString = gcnew String("Ok");
      }
      else
      {
        sendString = gcnew String("Fail");
      }
      remConnection->sendResponseToRemote(sendString->Length, Encoding::UTF8->GetBytes(sendString));

      return true;
    }
    else
    {
      return false;
    }

  }

  /**********************************************************
   * Function:     handleRemoteNCUCommand
   * Description:  NCU == Non-Controlling Unit 
   *               Also called Non-Leading or Sleeping
   * Returns true if command recognized
   *
   **********************************************************/
  private: bool handleRemoteNCUCommand(array<String^>^ fieldsFromRemote)
  {
    if (fieldsFromRemote[0] == "ncu")
    {
      bool newStatus;
      bool legalArg;
      try
      {
        if (fieldsFromRemote[1]->ToLower() == "on")  // arg is On/Off
        {
          newStatus = true;
          legalArg = true;
        }
        else if (fieldsFromRemote[1]->ToLower() == "off")
        {
          newStatus = false;
          legalArg = true;
        }
        else
        {
          newStatus = false;
          legalArg = false;
        }
      }
      catch (...)
      {
        newStatus = false;
        legalArg = false;
      }

      String^ sendString;
      if (legalArg)
      {
        if (legalArg)
        {
          locoSimForm->updateNCU(newStatus);
          sendString = gcnew String("Ok");
        }
        else
        {
          sendString = gcnew String("Fail");
        }
      }
      else
      {
        sendString = gcnew String("Fail");
      }
      remConnection->sendResponseToRemote(sendString->Length, Encoding::UTF8->GetBytes(sendString));

      return true;
    }
    else
    {
      return false;
    }
  }

  /**********************************************************
   * Function:     handleRemoteTrainIntegrityCommand
   * Description:  Set the TrainIntegrity Status of ECPB
   * Returns true if command recognized
   *
   **********************************************************/
  private: bool handleRemoteTrainIntegrityCommand(array<String^>^ fieldsFromRemote)
  {
    if (fieldsFromRemote[0] == "trainintegrity")
    {
      bool newStatus;
      bool legalArg;
      try
      {
        if (fieldsFromRemote[1]->ToLower() == "on")  // arg is On/Off
        {
          newStatus = true;
          legalArg = true;
        }
        else if (fieldsFromRemote[1]->ToLower() == "off")
        {
          newStatus = false;
          legalArg = true;
        }
        else
        {
          newStatus = false;
          legalArg = false;
        }
      }
      catch (...)
      {
        newStatus = false;
        legalArg = false;
      }

      String^ sendString;
      if (legalArg)
      {
        if (legalArg)
        {
          lcsSimForm->updateECPBTrainIntegrity(newStatus);
            
          sendString = gcnew String("Ok");
        }
        else
        {
          sendString = gcnew String("Fail");
        }
      }
      else
      {
        sendString = gcnew String("Fail");
      }
      remConnection->sendResponseToRemote(sendString->Length, Encoding::UTF8->GetBytes(sendString));

      return true;
    }
    else
    {
      return false;
    }
  }

 /**********************************************************
  * Function:     handleRemoteImportTrainCompCommand
  * Description:  Returns true if command recognized
  *
  **********************************************************/
  private: bool handleRemoteImportTrainCompCommand(array<String^>^ fieldsFromRemote)
  {
    if (fieldsFromRemote[0] == "importtraincomp")
    { 
      String^ fileToImport;
      try
      {
        fileToImport = fieldsFromRemote[1];  // arg is file to import
      }
      catch (...)
      {
        fileToImport ="";
      }

      String^ sendString;
      if (fileToImport->Length > strlen("x.ini")) // Shortest possible filename
      {                       // update the field with the string corresponding baliseId
        if (lcsSimForm->importTrainComp(fileToImport))
          sendString = gcnew String("Ok");
        else
          sendString = gcnew String("Faile");
      }
      else
      {
        sendString = gcnew String("Fail");
      }
      remConnection->sendResponseToRemote(sendString->Length, Encoding::UTF8->GetBytes(sendString));

      return true;
    }
    else
    {
      return false;
    }

  }
 /**********************************************************
  * Function:     handleRemoteAppendTrainCompCommand
  * Description:  Returns true if command recognized
  *
  **********************************************************/
  private: bool handleRemoteAppendTrainCompCommand(array<String^>^ fieldsFromRemote)
  {
    if (fieldsFromRemote[0] == "appendtraincomp")
    {
      String^ fileToImport;
      try
      {
        fileToImport = fieldsFromRemote[1];  // arg is file to import
      }
      catch (...)
      {
        fileToImport = "";
      }

      String^ sendString;
      if (fileToImport->Length > strlen("x.ini")) // Shortest possible filename
      {                       // update the field with the string corresponding baliseId
        if (lcsSimForm->appendTrainComp(fileToImport))
          sendString = gcnew String("Ok");
        else
          sendString = gcnew String("Faile");
      }
      else
      {
        sendString = gcnew String("Fail");
      }
      remConnection->sendResponseToRemote(sendString->Length, Encoding::UTF8->GetBytes(sendString));

      return true;
    }
    else
    {
      return false;
    }

  }

  /**********************************************************
  * Function:     handleRemoteCommand
  * Description:  Handles all remote command arriving on the 
  * AOSPC Remote Interface
  *
  **********************************************************/
  private: void handleRemoteCommand(String^ commandString)
  {
    if (handleRemoteMinimizeRestoreCommand(commandString))
      ;
    else if ((commandString != "") && (isStartTrainInProgress))
    {
      String^ sendString = gcnew String("Fail");
      remConnection->sendResponseToRemote(sendString->Length, Encoding::UTF8->GetBytes(sendString));
    }
    else if (handleRemoteStopCommand(commandString))
      ;
    else if (handleRemoteStartCommand(commandString))
      ;
    else 
    { // Commands with argument(s) or to DMI ?
      array<String^>^ fieldsFromRemote = commandString->Split(' ');
      if (handleRemoteStartTrainCommand(fieldsFromRemote))
        ;
      else if (handleRemoteAutoControlCommand(fieldsFromRemote))
        ;
      else if (handleRemoteSetRegBaliseCommand(fieldsFromRemote))
        ;
      else if (handleRemoteNCUCommand(fieldsFromRemote))
        ;
      else if (handleRemoteTrainIntegrityCommand(fieldsFromRemote))
        ;
      else if (handleRemoteImportTrainCompCommand(fieldsFromRemote))
        ;
      else if (handleRemoteAppendTrainCompCommand(fieldsFromRemote))
        ;
      else
      {
        // Unknown command, forward to dmi
        String^ sendString = commandString + gcnew String("\r\n");
        dmiInterface->sendCommandToDMI(sendString->Length, Encoding::UTF8->GetBytes(sendString));
      }
    }
  }

  private: void handleRemoteTestbedCommand(String^ commandString)
  {
    if (commandString == "geteotinfo")
    {

      UInt16 brakePressure = locoSimForm->getBrakePressure();
      UInt16 trailingTrack = dmiInterface->getTrailingTrack();
      UInt32 trailingPos = dmiInterface->getTrailingPos();

      String^ sendString = gcnew String("Ok;EOTInfo;") + trailingTrack.ToString() + gcnew String(";") + trailingPos.ToString() + gcnew String(";") + brakePressure.ToString() + gcnew String("\r\n");
      remTestbedConnection->sendResponseToRemote(sendString->Length, Encoding::UTF8->GetBytes(sendString));
    }
    else
    { // Unknown command
      String^ sendString = gcnew String("Fail\r\n");
      remTestbedConnection->sendResponseToRemote(sendString->Length, Encoding::UTF8->GetBytes(sendString));
    }
 }

            //Start Train Procedure
   private: void handleStartTrainProcessing()
   {
      if (isStartTrainInProgress)
      {
         switch (regStep)
         {
         case StartTrainCommandRegArea:
         {
            String^ sendStringForStartTrain = gcnew String("DMIRegArea ") + gcnew String(remoteStringForStartTrain[StartTrainArgRegArea]) + gcnew String("\r\n");

            dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
            counterForStartTrainCommandProcessing = 0;
            regStep = StartTrainCommandLogin;
            break;
         }

         case StartTrainCommandLogin:
         {
            if (dmiInterface->responseReceived())
            {
               if (dmiInterface->recString == "ok")
               {
                  //Reset the counter
                  counterForStartTrainCommandProcessing = 0;
                  String^ sendStringForStartTrain = gcnew String("DMILogin ") + gcnew String(remoteStringForStartTrain[StartTrainArgUser]) + gcnew String(" ") + gcnew String(remoteStringForStartTrain[StartTrainArgPwd]) + gcnew String("\r\n");
                  dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                  regStep = StartTrainCommandRegistrationTypeSelection;
               }
               else if (dmiInterface->recString == "retry")
               {
                  if (dmiInterface->getIsAllowedToLogin())
                  {
                     String^ sendStringForStartTrain = gcnew String("DMILogin ") + gcnew String(remoteStringForStartTrain[StartTrainArgUser]) + gcnew String(" ") + gcnew String(remoteStringForStartTrain[StartTrainArgPwd]) + gcnew String("\r\n");
                     dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                     counterForStartTrainCommandProcessing = 0;
                     regStep = StartTrainCommandRegistrationTypeSelection;
                  }
                  else
                  {
                     String^ sendStringForStartTrain = gcnew String("DMIRegArea ") + gcnew String(remoteStringForStartTrain[StartTrainArgRegArea]) + gcnew String("\r\n");
                     dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                     counterForStartTrainCommandProcessing++;
                  }
               }
               else //dmiInterface->recString == "fail"
               {
                  //Exit from the processing of Start Train
                  responseString = dmiInterface->recString;
                  regStep = StartTrainCommandFailReceivedFromDMI;
               }
            }
            else //NO response received
            {
               counterForStartTrainCommandProcessing++;
            }
            break;
         }

         case StartTrainCommandRegistrationTypeSelection:
         {

            if (dmiInterface->responseReceived())
            {
               if (dmiInterface->recString == "ok")
               {
                  isLoginOK = true;
               }
               else if (dmiInterface->recString == "retry")
               {
                  String^ sendStringForStartTrain = gcnew String("DMILogin ") + gcnew String(remoteStringForStartTrain[StartTrainArgUser]) + gcnew String(" ") + gcnew String(remoteStringForStartTrain[StartTrainArgPwd]) + gcnew String("\r\n");
                  dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                  counterForStartTrainCommandProcessing++;
               }
               else //dmiInterface->recString == "fail"
               {
                  //Exit from the processing of Start Train
                  responseString = dmiInterface->recString;
                  regStep = StartTrainCommandFailReceivedFromDMI;
               }
            }
            else if (!isLoginOK)//NO response received
            {
               counterForStartTrainCommandProcessing++;
            }

            //check for the type of registration or re-registration
            //wait for some secs to check for the ATP mode as Configuration or Powerup for re-registration
            if (isLoginOK && (counterformaxTimeforRereg > maxTimeforRereg*noOfCycleExecutedinOneSec))
            {
               //wait for some secs to check for the ATP mode as Configuration or Powerup for re-registration
               ATPModeEnum currentATPMode = dmiInterface->getAtpMode();
               if (currentATPMode == ATPModeConfiguration)
               {
                  regStep = StartTrainCommandAcceptReReg;
               }
               else if (currentATPMode == ATPModePowerUp)
               {
                  regStep = StartTrainCommandConfigSelection;
               }

               //Reset the isLoginOK Variable 
               isLoginOK = false;
               counterformaxTimeforRereg = 0;
            }
            else
            {
               counterformaxTimeforRereg++;
            }

            break;
         }

         case StartTrainCommandConfigSelection:
         {
            //Reset the counter
            String^ sendStringForStartTrain = gcnew String("DMIselect config") + gcnew String("\r\n");
            dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
            regStep = StartTrainCommandSetBaliseID;
            break;
         }

         case StartTrainCommandAcceptReReg:
         {
            if (dmiInterface->getConfigModeSubState() == ConfigModeSubStateReRegistration)
            {
               //Reset the counter
               counterForStartTrainCommandProcessing = 0;
               String^ sendStringForStartTrain = gcnew String("DMIselect acceptrereg") + gcnew String("\r\n");
               dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
               isReregStarted = true;
               regStep = StartTrainCommandConfirmDeparture;
            }
            else
            {
               counterForStartTrainCommandProcessing++;
            }
            break;
         }

         case StartTrainCommandSetBaliseID:
         {
            if (dmiInterface->responseReceived())
            {
               if ((dmiInterface->recString == "ok"))
               {
                  //Reset the counter
                  counterForStartTrainCommandProcessing = 0;
                  locoSimForm->updateBaliseId(remoteStringForStartTrain[StartTrainArgBaliseId]);
                  regStep = StartTrainCommandAcceptAutoConfig;
               }
               else if (dmiInterface->recString == "retry")
               {
                  String^ sendStringForStartTrain = gcnew String("DMIselect config") + gcnew String("\r\n");
                  dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                  counterForStartTrainCommandProcessing++;
               }
               else //dmiInterface->recString == "fail"
               {
                  //Exit from the processing of Start Train
                  responseString = dmiInterface->recString;
                  regStep = StartTrainCommandFailReceivedFromDMI;
               }
            }
            else //NO response received
            {
               counterForStartTrainCommandProcessing++;
            }
            break;
         }

         case StartTrainCommandAcceptAutoConfig:
         {
            if (dmiInterface->getConfirmAcceptAutoConfig())
            {
               //Reset the counter
               counterForStartTrainCommandProcessing = 0;
               String^ sendStringForStartTrain = gcnew String("DMIselect acceptautoconfig") + gcnew String("\r\n");
               dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
               regStep = StartTrainCommandConfirmDeparture;
            }
            else
            {
               counterForStartTrainCommandProcessing++;
            }
            break;
         }

         case StartTrainCommandConfirmDeparture:
         {
            if (dmiInterface->responseReceived())
            {
               if (dmiInterface->recString == "ok")
               {
                  //Reset the counter
                  counterForStartTrainCommandProcessing = 0;
                  String^ sendStringForStartTrain = gcnew String("DMIselect confirmdeparture") + gcnew String("\r\n");
                  dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));

                  if (isReregStarted)
                  {
                     regStep = StartTrainCommandBrakeTest;
                  }
                  else
                  {
                     regStep = StartTrainCommandOrientation;
                  }
               }
               else if (dmiInterface->recString == "retry")
               {
                  String^ sendStringForStartTrain;
                  if (isReregStarted)
                  {
                     sendStringForStartTrain = gcnew String("DMIselect acceptrereg") + gcnew String("\r\n");
                  }
                  else
                  {
                     sendStringForStartTrain = gcnew String("DMIselect acceptautoconfig") + gcnew String("\r\n");
                  }

                  dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                  counterForStartTrainCommandProcessing++;
               }
               else //dmiInterface->recString == "fail"
               {
                  //Exit from the processing of Start Train
                  responseString = dmiInterface->recString;
                  regStep = StartTrainCommandFailReceivedFromDMI;
               }
            }
            else //NO response received
            {
               counterForStartTrainCommandProcessing++;
            }
            break;
         }

         case StartTrainCommandOrientation:
         {
            if (dmiInterface->responseReceived())
            {
               if ((dmiInterface->recString == "ok"))
               {
                  //Reset the counter
                  counterForStartTrainCommandProcessing = 0;
                  String^ sendStringForStartTrain = gcnew String("DMIselect ") + gcnew String(remoteStringForStartTrain[StartTrainArgOrientation]) + gcnew String("\r\n");
                  dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                  regStep = StartTrainCommandBrakeTest;
               }
               else if (dmiInterface->recString == "retry")
               {
                  String^ sendStringForStartTrain = gcnew String("DMIselect confirmdeparture") + gcnew String("\r\n");
                  dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                  counterForStartTrainCommandProcessing++;
               }
               else //dmiInterface->recString == "fail"
               {
                  //Exit from the processing of Start Train
                  responseString = dmiInterface->recString;
                  regStep = StartTrainCommandFailReceivedFromDMI;
               }
            }
            else //NO response received
            {
               counterForStartTrainCommandProcessing++;
            }
            break;
         }

         case StartTrainCommandBrakeTest:
         {
            if (dmiInterface->responseReceived())
            {
               if ((dmiInterface->recString == "ok"))
               {
                  //Reset the counter
                  counterForStartTrainCommandProcessing = 0;
                  String^ sendStringForStartTrain = gcnew String("DMIselect braketest") + gcnew String("\r\n");
                  dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                  regStep = StartTrainCommandAutoControl;
               }
               else if (dmiInterface->recString == "retry")
               {
                  if (isReregStarted)
                  {
                     String^ sendStringForStartTrain = gcnew String("DMIselect confirmdeparture") + gcnew String("\r\n");
                     dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                     counterForStartTrainCommandProcessing++;
                  }
                  else
                  {
                     String^ sendStringForStartTrain = gcnew String("DMIselect ") + gcnew String(remoteStringForStartTrain[StartTrainArgOrientation]) + gcnew String("\r\n");
                     dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                     counterForStartTrainCommandProcessing++;
                  }
               }
               else //dmiInterface->recString == "fail"
               {
                  //Exit from the processing of Start Train
                  //Workaround
                  String^ sendStringForStartTrain = gcnew String("DMIselect ") + gcnew String(remoteStringForStartTrain[StartTrainArgOrientation]) + gcnew String("\r\n");
                  dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                  /*responseString = dmiInterface->recString;
                  regStep = CommandFailReceivedFromDMI;*/
               }
            }
            else //NO response received
            {
               counterForStartTrainCommandProcessing++;
            }
            break;
         }

         case StartTrainCommandAutoControl:
         {
            if (dmiInterface->responseReceived())
            {
               if (dmiInterface->recString == "ok")
               {
                  //Reset the counter
                  counterForStartTrainCommandProcessing = 0;
                  locoSimForm->updateAutoControl(true);
                  regStep = StartTrainCommandReleaseSB;
               }
               else if (dmiInterface->recString == "retry")
               {
                  String^ sendStringForStartTrain = gcnew String("DMIselect ") + gcnew String(remoteStringForStartTrain[StartTrainArgOrientation]) + gcnew String("\r\n");
                  dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                  counterForStartTrainCommandProcessing++;
               }
               else //dmiInterface->recString == "fail"
               {
                  //Exit from the processing of Start Train
                  responseString = dmiInterface->recString;
                  regStep = StartTrainCommandFailReceivedFromDMI;
               }
            }
            else //NO response received
            {
               counterForStartTrainCommandProcessing++;
            }
            break;
         }

         case StartTrainCommandReleaseSB:
         {
            String^ sendStringForStartTrain = gcnew String("DMISelect releasesb") + gcnew String("\r\n");
            dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
            regStep = StartTrainCommandSBFlashes;
            break;
         }

         case StartTrainCommandSBFlashes:
         {
            if (dmiInterface->responseReceived())
            {
               if ((dmiInterface->recString == "ok"))
               {
                  //Reset the counter
                  counterForStartTrainCommandProcessing = 0;
               }
               else if (dmiInterface->recString == "retry")
               {
                  String^ sendStringForStartTrain = gcnew String("DMISelect releasesb") + gcnew String("\r\n");
                  dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
                  counterForStartTrainCommandProcessing++;
               }
               else //dmiInterface->recString == "fail"
               {
                  //Exit from the processing of Start Train
                  responseString = dmiInterface->recString;
                  regStep = StartTrainCommandFailReceivedFromDMI;
               }
            }
            else //NO response received
            {
               counterForStartTrainCommandProcessing++;
            }

            if (dmiInterface->getReleaseServiceBrakeFlashes())
            {
               String^ sendStringForStartTrain = gcnew String("DMISelect releasesb") + gcnew String("\r\n");
               dmiInterface->sendCommandToDMI(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
               counterForStartTrainCommandProcessing = 0;
            }

            if (dmiInterface->getAtpMode() == ATPModeNormal)
            {
               //Reset the counter
               counterForStartTrainCommandProcessing = 0;
               regStep = StartTrainCommandComplete;
            }
            break;
         }

         case StartTrainCommandFailReceivedFromDMI:
         {
            remConnection->sendResponseToRemote(responseString->Length, Encoding::UTF8->GetBytes(responseString));
            isStartTrainInProgress = false;
            break;
         }

         case StartTrainCommandComplete:
         {
            String^ sendStringForStartTrain = gcnew String("ok");
            remConnection->sendResponseToRemote(sendStringForStartTrain->Length, Encoding::UTF8->GetBytes(sendStringForStartTrain));
            isStartTrainInProgress = false;
            break;
         }

         default:
            break;
         }

         //Check for the timeout timer for command execution of Start Train
         if (counterForStartTrainCommandProcessing > maxTimeForStartTrainCommandProcessing)
         {
            isStartTrainInProgress = false;
         }
      }
   }

   private: void handleRemoteInterface()
   {
     if (isRemoteInterfaceEnabled)
     {
       remConnection->Tick();

       if (remConnection->commandReceived())
       {
         handleRemoteCommand(remConnection->recString);
       }

       //Handling of Start Train Command from AOS Manager
       //Start the handling after some time, time require for DMI and ATP Connection to be up and running
       if (counterForExecutingStartTrain > 300)
       {
         handleStartTrainProcessing();
       }
       counterForExecutingStartTrain++;
     }
   }

   private: void handleRemoteTestbedInterface()
   {
     if (isRemoteTestbedInterfaceEnabled && isDMIInterfaceEnabled) // DMI interface must be enabled as well
     {
       remTestbedConnection->Tick();

       if (remTestbedConnection->commandReceived())
       {
         handleRemoteTestbedCommand(remTestbedConnection->recString);
       }
     }
   }

   private: void handleDMIInterface()
   {

     if (isDMIInterfaceEnabled)
     {
       dmiInterface->Tick();
       if (dmiInterface->responseReceived())
       {
         if (dmiInterface->recString->Length > 0)
         {
           if ((isRemoteInterfaceEnabled) && (!isStartTrainInProgress))
           {
             remConnection->sendResponseToRemote(dmiInterface->recString->Length, Encoding::UTF8->GetBytes(dmiInterface->recString));
           }
         }
       }
       if (dmiInterface->internalResponseReceived())
       {
         try
         {
           if ((dmiInterface->fields[0] == "ok") && (dmiInterface->fields[1] == "atpinfo"))
           {
             locoSimForm->updateDMIFieldsView(dmiInterface->fields);
             locoSimForm->updateATPInfo(dmiInterface->getPermittedSpeed(), dmiInterface->getPermittedDriveDir(), dmiInterface->getAtpMode(),
               dmiInterface->getDistanceToTarget(), dmiInterface->getDistanceToBCA(), dmiInterface->getTargetSpeed(),
               (UInt16)(dmiInterface->getMAMargin() / 100), // Convert MA Margin from cm to m
               dmiInterface->getTrackGradient(), dmiInterface->getEffectiveGradient(), 
               dmiInterface->getBrakeability(), dmiInterface->getBrakeDelayEB(), dmiInterface->getBrakeDelaySB()); 
           }
         }
         catch (...)
         {
           // Do Nothing
           ;
         }
       }

       // If AutoControl && permittedSpeed > 0
       if ((dmiInterface->getPermittedSpeed() > 0) && locoSimForm->getAutoControl())
       {
         if (locoSimForm->getEBReq())
         {
           dmiInterface->releaseEB();
         }
         else if (locoSimForm->getSBReq())
         {
           dmiInterface->releaseSB();
         }
       }
     }
   }

   private: System::Void timer1_Tick(System::Object^  sender, System::EventArgs^  e)
   {

      locoSimForm->aosRunning = runAOS->aosRunning;

      handleRemoteInterface();

      handleRemoteTestbedInterface();

      handleDMIInterface();

      // Run "Tick"
      // LocoSim
      if ((LocoSimEnabled) &&
         (locoSimForm != nullptr))
      {
         if (SimMode != SimulationVSim)
         {
            locoSimForm->Tick();
         }
         // RunAOS
         if (UseProcessForms)
         {
            // Handle request to start processes

            // Call tick function to check processes
            runAOS->Tick();

            // Handle process I/O
            if (SimMode != SimulationVSim)
            {
               if (locoSimForm->atp1ReqStart)
               {
                  runAOS->StartATP(locoSimForm->atpFileName, locoSimForm->atpArgs);
                  locoSimForm->atp1ReqStart = false;
                  locoSimForm->clearOutputBuffer();
               }
            }

            // TODO: Handle StartATO similar to StartATP

            // Send Command ATP/ATPA
            if (nullptr != atpAForm->cmdString && atpAForm->cmdValid)
            {
               array<unsigned char>^ sendArray = Encoding::UTF8->GetBytes(atpAForm->cmdString);
               runAOS->ATPAConsoleConnection->SendData(atpAForm->cmdString->Length, sendArray);
            }

            // Send Command ATB
            if (nullptr != atpBForm->cmdString && atpBForm->cmdValid)
            {
               array<unsigned char>^ sendArray = Encoding::UTF8->GetBytes(atpBForm->cmdString);
               runAOS->ATPBConsoleConnection->SendData(atpBForm->cmdString->Length, sendArray);
            }

            // Send Command Dispatcher
            if (nullptr != dispForm->cmdString && dispForm->cmdValid)
            {
               array<unsigned char>^ sendArray = Encoding::UTF8->GetBytes(dispForm->cmdString);
               runAOS->DispConsoleConnection->SendData(dispForm->cmdString->Length, sendArray);
            }

            atpAForm->cmdValid = false;
            atpBForm->cmdValid = false;
            dispForm->cmdValid = false;
            atoForm->cmdValid = false;

            // Process to forms
            atpAForm->procRunning = runAOS->atpARunning;
            atpBForm->procRunning = runAOS->atpBRunning;
            dispForm->procRunning = runAOS->dispRunning;
            atoForm->procRunning = runAOS->atoRunning;

            atpAForm->Tick(runAOS->atpARecStrings, runAOS->atpARecStringCnt);
            atpBForm->Tick(runAOS->atpBRecStrings, runAOS->atpBRecStringCnt);
            dispForm->Tick(runAOS->dispRecStrings, runAOS->dispRecStringCnt);
            atoForm->Tick(runAOS->atoRecStrings, runAOS->atoRecStringCnt);

            runAOS->atpARecStringCnt = 0;
            runAOS->atpBRecStringCnt = 0;
            runAOS->dispRecStringCnt = 0;
            runAOS->atoRecStringCnt = 0;
         }
      }

      // LCSSimulation
      if ((LCSSimEnabled) &&
         (lcsSimForm != nullptr))
      {
         lcsSimForm->Tick();
      }

      // OBRDSimulation
      if ((OBRDSimEnabled) &&
         (obrdSimForm != nullptr))
      {
         obrdSimForm->Tick();
      }
   }
   private: System::Void Form1_Load(System::Object^  sender, System::EventArgs^  e) {
      int left = Convert::ToInt16(Registry::GetValue(registryRootKeyName, "Left", "0"));
      int top = Convert::ToInt16(Registry::GetValue(registryRootKeyName, "Top", "0"));
      int width = Convert::ToInt16(Registry::GetValue(registryRootKeyName, "Width", "32767"));
      int height = Convert::ToInt16(Registry::GetValue(registryRootKeyName, "Height", "32767"));

      // If registry not available, first run for example, use default position from windows
      if ((left != 32767) &&
         (top != 32767))
      {
         this->Left = left;
         this->Top = top;
      }
      this->Width = width < 400 ? 400 : width;
      this->Height = height < 200 ? 200 : height;

      // Check if form is visible on any screen
      bool visible = false;
      for each (Screen^ screen in Screen::AllScreens)
      {
         if (screen->Bounds.Contains(Drawing::Rectangle(this->Left, this->Top, 20, 20)))
         {
            visible = true;
         }
      }

      if (!visible)
      {
         this->Left = 1;
         this->Top = 1;
      }


      // Set application title
      this->Text += " v" +
         Assembly::GetExecutingAssembly()->GetName()->Version->Major.ToString() + "." +
         Assembly::GetExecutingAssembly()->GetName()->Version->Minor.ToString() + "." +
         Assembly::GetExecutingAssembly()->GetName()->Version->Build.ToString() + " - " + LocoName;

      switch (SimMode)
      {
      case SimulationSil:
         this->Text += " (SIL)";
         break;
      case SimulationHil:
         this->Text += " (HIL)";
         break;
      case SimulationEmd:
         this->Text += " (EMD)";
         break;
      case SimulationVSim:
         this->Text += " (VSim)";
         break;
      default:
         break;
      }


   }
   private: System::Void Form1_FormClosing(System::Object^  sender, System::Windows::Forms::FormClosingEventArgs^  e) {

      // Store windows position in registry
      if (this->WindowState == FormWindowState::Normal)
      {
         Registry::SetValue(registryRootKeyName, "Left", String::Format("{0:0}", this->Left));
         Registry::SetValue(registryRootKeyName, "Top", String::Format("{0:0}", this->Top));
         Registry::SetValue(registryRootKeyName, "Width", String::Format("{0:0}", this->Width));
         Registry::SetValue(registryRootKeyName, "Height", String::Format("{0:0}", this->Height));
      }

      // Save settings
      // LocoSim
      if ((LocoSimEnabled) &&
         (locoSimForm != nullptr))
      {
         if ((SimMode == SimulationHil) || (SimMode == SimulationSil))
         {
            locoSimForm->SaveWindowSettings();
         }

         if (UseProcessForms)
         {
            atpAForm->SaveWindowSettings();
            atpBForm->SaveWindowSettings();
            dispForm->SaveWindowSettings();
            atoForm->SaveWindowSettings();
         }
      }

      // LCSSimulation
      if ((LCSSimEnabled) &&
         (lcsSimForm != nullptr))
      {
         lcsSimForm->SaveWindowSettings();
      }

      // OBRDSSim
      if ((OBRDSimEnabled) &&
         (obrdSimForm != nullptr))
      {
         obrdSimForm->SaveWindowSettings();
      }
      // SiteDataExt
      if ((SiteDataExtEnabled) &&
         (siteDataExtForm != nullptr))
      {
         siteDataExtForm->SaveWindowSettings();
      }

      // zOrder
      //int i;
      //for (i = 0; i < zOrder->Length; i++)
      //{
      //    Registry::SetValue(registryRootKeyName, "zOrder" + i, zOrder[i]);
      //}
   }
   private: System::Void tscbSetup_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e) {
      int locoSimLeft = 0;
      int locoSimTop = 0;
      int locoSimWidth = 0;
      int locoSimHeight = 0;
      int lcsSimLeft = 0;
      int lcsSimTop = 0;
      int lcsSimWidth = 0;
      int lcsSimHeight = 0;

      int obrdSimLeft = 0;
      int obrdSimTop = 0;
      int obrdSimWidth = 0;
      int obrdSimHeight = 0;
      int siteDataExtLeft = 0;
      int siteDataExtTop = 0;
      int siteDataExtWidth = 0;
      int siteDataExtHeight = 0;

      int wBorder = this->Width - this->ClientSize.Width + 8;
      int hBorder = this->Height - this->ClientSize.Height + toolStrip1->Height + 8;
      if (tscbSetup->Text == "Small")
      {
         // Get max enabled width/height
         int maxWidth = 400;
         int maxHeight = 50;

         if (LocoSimEnabled)
         {
            maxWidth = max(maxWidth, locoSimForm->Width);
            maxHeight = max(maxHeight, locoSimForm->Height);
         }
         if (LCSSimEnabled)
         {
            maxWidth = max(maxWidth, lcsSimForm->Width);
            maxHeight = max(maxHeight, lcsSimForm->Height);
         }
         if (OBRDSimEnabled)
         {
            maxWidth = max(maxWidth, obrdSimForm->Width);
            maxHeight = max(maxHeight, obrdSimForm->Height);
         }
         if (SiteDataExtEnabled)
         {
            maxWidth = max(maxWidth, siteDataExtForm->Width);
            maxHeight = max(maxHeight, siteDataExtForm->Height);
         }

         // Set window size
         this->Width = maxWidth + wBorder;
         this->Height = maxHeight + hBorder;

         // LocoSim
         if (LocoSimEnabled)
         {
            locoSimLeft = 1;
            locoSimTop = 1;
            locoSimWidth = locoSimForm->Width;
            locoSimHeight = locoSimForm->Height;
            locoSimForm->SetSize(locoSimLeft, locoSimTop, locoSimWidth, locoSimHeight);
            if (UseProcessForms)
            {
               atpAForm->SetSize(1, 1, locoSimWidth, locoSimHeight);
               atpBForm->SetSize(1, 1, locoSimWidth, locoSimHeight);
               dispForm->SetSize(1, 1, locoSimWidth, locoSimHeight);
               atoForm->SetSize(1, 1, locoSimWidth, locoSimHeight);
            }
         }

         // LCSSim
         if (LCSSimEnabled)
         {
            lcsSimLeft = 1;
            lcsSimTop = 1;
            lcsSimWidth = lcsSimForm->Width;
            lcsSimHeight = lcsSimForm->Height;
            lcsSimForm->SetSize(lcsSimLeft, lcsSimTop, lcsSimWidth, lcsSimHeight);
         }

         // OBRDSim
         if (OBRDSimEnabled)
         {
            obrdSimLeft = 1;
            obrdSimTop = 1;
            obrdSimWidth = obrdSimForm->Width;
            obrdSimHeight = obrdSimForm->Height;
            obrdSimForm->SetSize(obrdSimLeft, obrdSimTop, obrdSimWidth, obrdSimHeight);
         }

         // SiteDataExt
         if (SiteDataExtEnabled)
         {
            siteDataExtLeft = 1;
            siteDataExtTop = 1;
            siteDataExtWidth = siteDataExtForm->Width;
            siteDataExtHeight = siteDataExtForm->Height;
            siteDataExtForm->SetSize(siteDataExtLeft, siteDataExtTop, siteDataExtWidth, siteDataExtHeight);
         }

         // Show first enabled form
         if (LocoSimEnabled)
         {
            locoSimForm->SetVisibility(true);
            atpAForm->SetVisibility(false);
            atpBForm->SetVisibility(false);
            dispForm->SetVisibility(false);
            atoForm->SetVisibility(false);
            if (LCSSimEnabled)
            {
               lcsSimForm->SetVisibility(false);
            }
            if (OBRDSimEnabled)
            {
               obrdSimForm->SetVisibility(false);
            }
         }
         if (LCSSimEnabled)
         {
            lcsSimForm->SetVisibility(false);
            if (OBRDSimEnabled)
            {
               obrdSimForm->SetVisibility(false);
            }
         }
         if (!LocoSimEnabled &&
            !LCSSimEnabled &&
            OBRDSimEnabled)
         {
            obrdSimForm->SetVisibility(false);
         }

         if (SiteDataExtEnabled)
         {
            siteDataExtForm->SetVisibility(false);
         }
      }
      // Normal, show LocoSim, LCSSim and OBRDSim
      else if (tscbSetup->Text == "Normal")
      {
         // Accumulate width/height
         int maxWidth = 0;
         int maxHeight = 0;

         maxWidth += LocoSimEnabled ? locoSimForm->Width : 0;
         maxWidth += LCSSimEnabled ? lcsSimForm->Width : 0;
         maxWidth += OBRDSimEnabled ? obrdSimForm->Width : 0;

         maxHeight = LocoSimEnabled ? max(maxHeight, locoSimForm->Height) : maxHeight;
         maxHeight = LCSSimEnabled ? max(maxHeight, lcsSimForm->Height) : maxHeight;
         maxHeight = OBRDSimEnabled ? max(maxHeight, obrdSimForm->Height) : maxHeight;
         //maxHeight = SiteDataExtEnabled ? max(maxHeight, siteDataExtForm->Height) : maxHeight;
         maxHeight = (OBRDSimEnabled && LCSSimEnabled) ? max(maxHeight, (obrdSimForm->Height + lcsSimForm->Height)) : maxHeight;
         //maxHeight = (SiteDataExtEnabled && LocoSimEnabled) ? max(maxHeight, (siteDataExtForm->Height + locoSimForm->Height)) : maxHeight;

         // Set window size
         this->Width = maxWidth + wBorder;
         this->Height = maxHeight + hBorder;

         // LocoSim
         if (LocoSimEnabled)
         {
            locoSimLeft = 1;
            locoSimTop = 1;
            locoSimWidth = locoSimForm->Width;
            locoSimHeight = locoSimForm->Height;
            locoSimForm->SetSize(locoSimLeft, locoSimTop, locoSimWidth, locoSimHeight);
            if (UseProcessForms)
            {
               atpAForm->SetSize(1, 1, maxWidth, maxHeight);
               atpBForm->SetSize(1, 1, maxWidth, maxHeight);
               dispForm->SetSize(1, 1, maxWidth, maxHeight);
               atoForm->SetSize(1, 1, maxWidth, maxHeight);
            }
         }
         // LCSSim
         if (LCSSimEnabled)
         {
            if (LocoSimEnabled)
            {
               lcsSimLeft = locoSimLeft + locoSimWidth;
            }
            else
            {
               lcsSimLeft = 1;
            }
            lcsSimTop = 1;
            lcsSimWidth = lcsSimForm->Width;
            lcsSimHeight = lcsSimForm->Height;
            lcsSimForm->SetSize(lcsSimLeft, lcsSimTop, lcsSimWidth, lcsSimHeight);
         }
         // OBRDSim
         if (OBRDSimEnabled)
         {
            if (LCSSimEnabled)
            {
               obrdSimLeft = lcsSimLeft + lcsSimWidth;
            }
            else
            {
               obrdSimLeft = locoSimLeft + locoSimWidth;
            }

            obrdSimTop = 1;
            obrdSimWidth = obrdSimForm->Width;
            obrdSimHeight = obrdSimForm->Height;
            obrdSimForm->SetSize(obrdSimLeft, obrdSimTop, obrdSimWidth, obrdSimHeight);
         }

         // SiteDataExt
         //if (SiteDataExtEnabled)
         //{
         //    if (LocoSimEnabled)
         //    {
         //        siteDataExtLeft   = locoSimLeft;
         //    }
         //    else
         //    {
         //        siteDataExtLeft   = 1;
         //    }

         //    if (LocoSimEnabled)
         //    {
         //        siteDataExtTop   = locoSimTop + locoSimHeight;
         //    }
         //    else if (LCSSimEnabled)
         //    {
         //        siteDataExtTop   = locoSimTop + lcsSimHeight;
         //    }
         //    else
         //    {
         //        siteDataExtTop   = 1;
         //    }
         //    siteDataExtWidth = siteDataExtForm->Width;
         //    siteDataExtHeight = siteDataExtForm->Height;
         //    siteDataExtForm->SetSize(siteDataExtLeft, siteDataExtTop, siteDataExtWidth, siteDataExtHeight);
         //}

         // Show first enabled form
         if (LocoSimEnabled)
         {
            locoSimForm->SetVisibility(true);
            atpAForm->SetVisibility(false);
            atpBForm->SetVisibility(false);
            dispForm->SetVisibility(false);
            atoForm->SetVisibility(false);
         }
         if (LCSSimEnabled)
         {
            lcsSimForm->SetVisibility(true);
         }
         if (OBRDSimEnabled)
         {
            obrdSimForm->SetVisibility(true);
         }

         //if (SiteDataExtEnabled)
         //{
         //    siteDataExtForm->SetVisibility(true);
         //}
         //siteDataExtForm->SetVisibility(false);
      }
      else if (tscbSetup->Text == "Developer")
      {
         // Define ATxConsole size
         int conWidth = 500;
         int conHeight = 240;

         // Accumulate width/height
         int maxWidth = 0;
         int maxHeight = 0;

         maxWidth += LocoSimEnabled ? locoSimForm->Width : 0;
         maxWidth += (LocoSimEnabled && UseProcessForms) ? conWidth : 0;
         maxWidth += LCSSimEnabled ? lcsSimForm->Width : 0;
         maxWidth += (maxWidth == 0) ? obrdSimForm->Width : 0;

         maxHeight = LocoSimEnabled ? max(maxHeight, locoSimForm->Height) : maxHeight;
         maxHeight = LCSSimEnabled ? max(maxHeight, lcsSimForm->Height) : maxHeight;
         maxHeight = OBRDSimEnabled ? max(maxHeight, obrdSimForm->Height) : maxHeight;
         maxHeight = SiteDataExtEnabled ? max(maxHeight, siteDataExtForm->Height) : maxHeight;
         maxHeight = (OBRDSimEnabled && LCSSimEnabled) ? max(maxHeight, (obrdSimForm->Height + lcsSimForm->Height)) : maxHeight;
         maxHeight = (SiteDataExtEnabled && LocoSimEnabled) ? max(maxHeight, (siteDataExtForm->Height + locoSimForm->Height)) : maxHeight;
         maxHeight = max(maxHeight, (LocoSimEnabled && UseProcessForms) ? 4 * conHeight : 0);

         // Set window size
         this->Width = maxWidth + wBorder;
         this->Height = maxHeight + hBorder;

         // LocoSim
         if (LocoSimEnabled)
         {
            locoSimLeft = 1;
            locoSimTop = 1;
            locoSimWidth = locoSimForm->Width;
            locoSimHeight = locoSimForm->Height;
            locoSimForm->SetSize(locoSimLeft, locoSimTop, locoSimWidth, locoSimHeight);
         }

         // LCSSim
         if (LCSSimEnabled)
         {
            if (LocoSimEnabled)
            {
               lcsSimLeft = locoSimLeft + locoSimWidth;
            }
            else
            {
               lcsSimLeft = 1;
            }
            lcsSimTop = 1;
            lcsSimWidth = lcsSimForm->Width;
            lcsSimHeight = lcsSimForm->Height;
            lcsSimForm->SetSize(lcsSimLeft, lcsSimTop, lcsSimWidth, lcsSimHeight);
         }

         // OBRDSim
         if (OBRDSimEnabled)
         {
            if (LCSSimEnabled)
            {
               obrdSimLeft = lcsSimLeft + lcsSimWidth;
            }
            else
            {
               obrdSimLeft = locoSimLeft + locoSimWidth;
            }

            obrdSimTop = 1;
            obrdSimWidth = obrdSimForm->Width;
            obrdSimHeight = obrdSimForm->Height;
            obrdSimForm->SetSize(obrdSimLeft, obrdSimTop, obrdSimWidth, obrdSimHeight);
         }

         // SiteDataExt
         if (SiteDataExtEnabled)
         {
            if (LocoSimEnabled)
            {
               siteDataExtLeft = locoSimLeft;
            }
            else
            {
               siteDataExtLeft = 1;
            }

            if (LocoSimEnabled)
            {
               siteDataExtTop = locoSimTop + locoSimHeight;
            }
            else if (LCSSimEnabled)
            {
               siteDataExtTop = locoSimTop + lcsSimHeight;
            }
            else
            {
               siteDataExtTop = 1;
            }
            siteDataExtWidth = siteDataExtForm->Width;
            siteDataExtHeight = siteDataExtForm->Height;
            siteDataExtForm->SetSize(siteDataExtLeft, siteDataExtTop, siteDataExtWidth, siteDataExtHeight);
         }

         if (LocoSimEnabled)
         {
            if (UseProcessForms)
            {
               int left = LCSSimEnabled ? (lcsSimLeft + lcsSimWidth) : (locoSimLeft + locoSimWidth);

               left = OBRDSimEnabled ? left + obrdSimWidth : left;
               atpAForm->SetSize(left, 1, conWidth, conHeight);
               atpBForm->SetSize(left, 1 + conHeight, conWidth, conHeight);
               dispForm->SetSize(left, 1 + 2 * conHeight, conWidth, conHeight);
               atoForm->SetSize(left, 1 + 3 * conHeight, conWidth, conHeight);
            }
         }
         // Show first enabled form
         if (LocoSimEnabled)
         {
            locoSimForm->SetVisibility(true);
            atpAForm->SetVisibility(true);
            atpBForm->SetVisibility(true);
            dispForm->SetVisibility(true);
            atoForm->SetVisibility(true);
         }
         if (LCSSimEnabled)
         {
            lcsSimForm->SetVisibility(true);
         }
         if (OBRDSimEnabled)
         {
            obrdSimForm->SetVisibility(true);
         }

         if (SiteDataExtEnabled)
         {
            siteDataExtForm->SetVisibility(true);
         }
      }
      else if (tscbSetup->Text == "Setup1")
      {
         LoadSetup("Setup1");
      }
      else if (tscbSetup->Text == "Setup2")
      {
         LoadSetup("Setup2");
      }
   }

   private: void LoadSetup(String^ setup) {
      String^ tmpRootKey = registryRootKeyName;
      if (!System::String::IsNullOrEmpty(setup))
      {
         tmpRootKey += "\\" + setup;
      }
      // Check if key exists
      String^ tmp = Convert::ToString(Registry::GetValue(tmpRootKey, "", "empty"));

      // Key does not exist, i.e. existing setup
      if (!System::String::IsNullOrEmpty(tmp))
      {
         int width = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Width", "32767"));
         int height = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Height", "32767"));

         // If registry not available, first run for example, use default position from windows
         if ((width != 32767) &&
            (height != 32767))
         {
            this->Width = width < 400 ? 400 : width;
            this->Height = height < 200 ? 200 : height;
         }
      }

      if (LocoSimEnabled)
      {
         locoSimForm->LoadSetup(tscbSetup->Text);
         if (UseProcessForms)
         {
            atpAForm->LoadSetup(tscbSetup->Text);
            atpBForm->LoadSetup(tscbSetup->Text);
            dispForm->LoadSetup(tscbSetup->Text);
            atoForm->LoadSetup(tscbSetup->Text);
         }
      }
      if (LCSSimEnabled)
      {
         lcsSimForm->LoadSetup(tscbSetup->Text);
      }
      if (OBRDSimEnabled)
      {
         obrdSimForm->LoadSetup(tscbSetup->Text);
      }
      if (SiteDataExtEnabled)
      {
         siteDataExtForm->LoadSetup(tscbSetup->Text);
      }

   }
   private: void SaveSetup(String^ setup) {
      String^ tmpRootKey = registryRootKeyName;
      if (!System::String::IsNullOrEmpty(setup))
      {
         tmpRootKey += "\\" + setup;
      }

      // Store windows position in registry
      Registry::SetValue(tmpRootKey, "Left", String::Format("{0:0}", this->Left));
      Registry::SetValue(tmpRootKey, "Top", String::Format("{0:0}", this->Top));
      Registry::SetValue(tmpRootKey, "Width", String::Format("{0:0}", this->Width));
      Registry::SetValue(tmpRootKey, "Height", String::Format("{0:0}", this->Height));


      if (LocoSimEnabled)
      {
         locoSimForm->SaveSetup(tscbSetup->Text);
         if (UseProcessForms)
         {
            atpAForm->SaveSetup(tscbSetup->Text);
            atpBForm->SaveSetup(tscbSetup->Text);
            dispForm->SaveSetup(tscbSetup->Text);
            atoForm->SaveSetup(tscbSetup->Text);
         }
      }
      if (LCSSimEnabled)
      {
         lcsSimForm->SaveSetup(tscbSetup->Text);
      }
      if (OBRDSimEnabled)
      {
         obrdSimForm->SaveSetup(tscbSetup->Text);
      }
      if (SiteDataExtEnabled)
      {
         siteDataExtForm->SaveSetup(tscbSetup->Text);
      }
   }
   private: System::Void Form1_Resize(System::Object^  sender, System::EventArgs^  e) {
      if (this->Width < 620)
      {
         tsbLocoSim->DisplayStyle = ToolStripItemDisplayStyle::Image;
         tsbLCSSim->DisplayStyle = ToolStripItemDisplayStyle::Image;
         tsbOBRDSim->DisplayStyle = ToolStripItemDisplayStyle::Image;
         tsbSiteDataExt->DisplayStyle = ToolStripItemDisplayStyle::Image;
         tsbATPA->DisplayStyle = ToolStripItemDisplayStyle::Image;
         tsbATPB->DisplayStyle = ToolStripItemDisplayStyle::Image;
         tsbDISP->DisplayStyle = ToolStripItemDisplayStyle::Image;
         tsbATO->DisplayStyle = ToolStripItemDisplayStyle::Image;
      }
      else
      {
         tsbLocoSim->DisplayStyle = ToolStripItemDisplayStyle::ImageAndText;
         tsbLCSSim->DisplayStyle = ToolStripItemDisplayStyle::ImageAndText;
         tsbOBRDSim->DisplayStyle = ToolStripItemDisplayStyle::ImageAndText;
         tsbSiteDataExt->DisplayStyle = ToolStripItemDisplayStyle::ImageAndText;
         tsbATPA->DisplayStyle = ToolStripItemDisplayStyle::ImageAndText;
         tsbATPB->DisplayStyle = ToolStripItemDisplayStyle::ImageAndText;
         tsbDISP->DisplayStyle = ToolStripItemDisplayStyle::ImageAndText;
         tsbATO->DisplayStyle = ToolStripItemDisplayStyle::ImageAndText;
      }
   }
   private: System::Void Form1_MdiChildActivate(System::Object^  sender, System::EventArgs^  e) {


      //if (storeZOrder == false)
      //{
      //    return;
      //}

      //array<String^>^ tmpZOrder = gcnew array<String^>(10);

      //if (this->ActiveMdiChild == locoSimForm)
      //{
      //    tmpZOrder[0] = "locoSimForm";
      //}
      //else if (this->ActiveMdiChild == lcsSimForm)
      //{
      //    tmpZOrder[0] = "lcsSimForm";
      //}
      //else if (this->ActiveMdiChild == obrdSimForm)
      //{
      //    tmpZOrder[0] = "obrdSimForm";
      //}
      //else if (this->ActiveMdiChild == atp1Form)
      //{
      //    tmpZOrder[0] = "atp1Form";
      //}
      //else if (this->ActiveMdiChild == atp2Form)
      //{
      //    tmpZOrder[0] = "atp2Form";
      //}
      //else if (this->ActiveMdiChild == atoForm)
      //{
      //    tmpZOrder[0] = "atoForm";
      //}
      //else if (this->ActiveMdiChild == siteDataExtForm)
      //{
      //    tmpZOrder[0] = "siteDataExtForm";
      //}

      //int i;
      //int j = 1;
      //for (i = 0; (i < zOrder->Length) && (j < tmpZOrder->Length); i++)
      //{
      //    if (zOrder[i] != tmpZOrder[0])
      //    {
      //        tmpZOrder[j++] = zOrder[i];
      //    }
      //}

      //zOrder = tmpZOrder;
   }
   private: System::Void setup1ToolStripMenuItem_Click(System::Object^  sender, System::EventArgs^  e) {
      SaveSetup("Setup1");
   }
   private: System::Void setup2ToolStripMenuItem_Click(System::Object^  sender, System::EventArgs^  e) {
      SaveSetup("Setup2");
   }
   private: System::Void Form1_Shown(System::Object^  sender, System::EventArgs^  e) {

      // Handle zOrder
     // int i;
     // for (i = (zOrder->Length - 1); i > 0; i--)
     // {
     //     Form^ f = nullptr;

     //     if (zOrder[i] = "locoSimForm")           { f = locoSimForm; }
     //     else if (zOrder[i] = "lcsSimForm")       { f = lcsSimForm; }
     //     else if (zOrder[i] = "obrdSimForm")      { f = obrdSimForm; }
     //     else if (zOrder[i] = "atp1Form")         { f = atp1Form; }
     //     else if (zOrder[i] = "atp2Form")         { f = atp2Form; }
     //     else if (zOrder[i] = "atoForm")          { f = atoForm; }
     //     else if (zOrder[i] = "siteDataExtForm")  { f = siteDataExtForm; }

     //     if (f != nullptr)
     //     {
     //         //if (f->Visible == true)
     //         {
     //             f->Focus();
     //         }
     //     }

     // }
     //storeZOrder = true;
   }
   private: System::Void tsbDISP_Click(System::Object^  sender, System::EventArgs^  e) {

      if (dispForm != this->ActiveMdiChild &&
         dispForm->Visible)
      {
         dispForm->Focus();
      }
      else
      {
         dispForm->SetVisibility(!dispForm->Visible);
      }


   }
   };
}

