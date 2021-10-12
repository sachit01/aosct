#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  %name:          OBRDSimForm.h %
*
*  %version:       1 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2018-10-11 12:40 %
*
*  DESCRIPTION: GUI for the OBRD-simulation
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-11    marlundg    File created
*
*******************************************************************************/

#include <cmath>

#include "OBRDSimTypes.h"
#include "../LocoSimDLL/LocoConsts.h"

namespace AOSPC {

  using namespace System;
  using namespace System::ComponentModel;
  using namespace System::Collections;
  using namespace System::Windows::Forms;
  using namespace System::Data;
  using namespace System::Drawing;
  using namespace Microsoft::Win32;
  using namespace System::Globalization;
  using namespace System::Runtime::InteropServices;
  using namespace System::Xml;
  using namespace System::IO;
  
  using namespace OBRDSimDLL;

	/// <summary>
	/// Summary for OBRDSimForm
	/// </summary>
	public ref class OBRDSimForm : public System::Windows::Forms::Form
	{
	public:

    OBRDSimDLL::OBRDSimulation^   obrdSim;
   
    /**********************************************************
    * Function:     OBRDSimForm
    * Description:  Constructor
    **********************************************************/
    OBRDSimForm(String^ regKey, String^ fileName, String^ obrdSimfileName)
    {
      InitializeComponent();
      this->DoubleBuffered = true;

      regRootKey = regKey;
      
      obrdSim = gcnew OBRDSimulation(fileName, obrdSimfileName);

      // Get window position from registry
      int left = Convert::ToInt16(Registry::GetValue(regRootKey + "\\OBRDSim", "Left", "32767"));
      int top = Convert::ToInt16(Registry::GetValue(regRootKey + "\\OBRDSim", "Top", "32767"));
      
      // If registry not available, first run for example, use default position from windows
      if ((left != 32767) &&
        (top != 32767))
      {
        formLeft = left;
        formTop = top;
      }
    }

    /*********************************************************************************************
    * Function:     Tick
    * Description:  Periodically called to run simulation and update values between GUI and model
    **********************************************************************************************/
    void Tick(void)
    {
      // Run OBRD simulation
      obrdSim->Tick();

      if (obrdSim->GetMissingTrackFault())
      {

        System::Windows::Forms::MessageBox::Show("Track defined in Path is not in track-list","",
          (MessageBoxButtons)0,
          MessageBoxIcon::Error);
      }
      
      // Connection status
      UPDATE_IF_DIFFERENT(tsbATOGreen->Visible, obrdSim->atpConnected);
      UPDATE_IF_DIFFERENT(tsbATORed->Visible, !obrdSim->atpConnected);
      
      // Update Send packets lists
      for (int i = 0; i < obrdSim->guiStatusToATPCnt; i++)
      {
        UPDATE_IF_DIFFERENT(lvStatusToATP->Items[i]->SubItems[1]->Text, obrdSim->guiStatusToATP[i]);
      }

      for (int i = 0; i < obrdSim->guiProtocolToATPCnt; i++)
      {
        UPDATE_IF_DIFFERENT(lvProtocolToATP->Items[i]->SubItems[1]->Text, obrdSim->guiProtocolToATP[i]);
      }

      // Update Received packets lists
      for (int i = 0; i < obrdSim->guiRejectFromATPCnt; i++)
      {
        UPDATE_IF_DIFFERENT(lvRejectFromATP->Items[i]->SubItems[1]->Text, obrdSim->guiRejectFromATP[i]);
      }

      for (int i = 0; i < obrdSim->guiProtocolFromATPCnt; i++)
      {
        UPDATE_IF_DIFFERENT(lvProtocolFromATP->Items[i]->SubItems[1]->Text, obrdSim->guiProtocolFromATP[i]);
      }
    }

    /**********************************************************
    * Function:     getSimulator
    * Description:
    **********************************************************/
    OBRDSimulation^ getSimulator()
    {
      return obrdSim;
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

    void SetSize(int left, int top, int width, int height) 
    {
      
      this->Left = left;
      this->Top = top;
      
      formLeft = left;
      formTop = top;
    }

    /**********************************************************
    * Function:     SaveSetup
    * Description:
    **********************************************************/
    void SaveSetup(String^ setup)
    {
      String^ tmpRootKey = regRootKey;
      if (!System::String::IsNullOrEmpty(setup))
      {
        // Create key
        tmpRootKey = regRootKey + "\\" + setup + "\\OBRDSim";

        // Store windows position in registry
        Registry::SetValue(tmpRootKey, "Left", String::Format("{0:0}", this->Left));
        Registry::SetValue(tmpRootKey, "Top", String::Format("{0:0}", this->Top));
        Registry::SetValue(tmpRootKey, "Visible", this->Visible ? "1" : "0");
      }
    }
    
    /**********************************************************
    * Function: SaveWindowSettings
    * Description:
    **********************************************************/
    void SaveWindowSettings(void)
    {
      Registry::SetValue(regRootKey + "\\OBRDSim", "Left", String::Format("{0:0}", this->Left));
      Registry::SetValue(regRootKey + "\\OBRDSim", "Top", String::Format("{0:0}", this->Top));
      Registry::SetValue(regRootKey + "\\OBRDSim", "Visible", this->Visible ? "1" : "0");
    }

    /**********************************************************
    * Function:     LoadSetup
    * Description:
    **********************************************************/
    void LoadSetup(String^ setup)
    {
      String^ tmpRootKey = regRootKey;
      if (!System::String::IsNullOrEmpty(setup))
      {
        tmpRootKey = regRootKey + "\\" + setup + "\\OBRDSim";
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
    * Function:     Init
    * Description:
    **********************************************************/
    void Init(void)
    {
      // Setup GUI list for communication
      ListViewItem^ itemPtr;

      // Update all tabs in GUI from model

      // ----- OBRDSim Tab 1 -------

      UPDATE_IF_DIFFERENT(tbTrainLength->Text, obrdSim->mdlTrainLength.ToString());
      UPDATE_IF_DIFFERENT(tbTimestampDelay->Text, obrdSim->mdlTimestampDelay.ToString());
      UPDATE_IF_DIFFERENT(tbStatusReportPeriodicity->Text, obrdSim->mdlStatusReportPeriodicity.ToString());
      UPDATE_IF_DIFFERENT(tbVSIMLastCarBP->Text, obrdSim->mdlVSIMLastCarBP.ToString());

      if ((obrdSim->mdlSimMode != SimulationVSim) && (obrdSim->mdlSimMode != SimulationEmd))
      {
        tbVSIMLastCarBP->Enabled = false;
        label3->Enabled = false;
        label20->Enabled = false;
      }

      switch (obrdSim->mdlFaultInjection)
      {
      case FINoError:
        rbFINoError->Checked = true;
        break;
      case FIFreezePOS:
        rbFIFreezePOS->Checked = true;
        break;
      case FIFailedToReadGPS:
        rbFIFailedToReadGPS->Checked = true;
        break;
      case FICRCFailure:
        rbFICRCFailure->Checked = true;
        break;
      case FIIgnoreRejection:
        rbIgnoreRejection->Checked = true;
        break;
      case FIPosErrorOffset:
        rbFIPosErrorOffset->Checked = true;
        break;
      case FIOverrideBrakePressure:
        rbOverrideBrakePressure->Checked = true;
        break;
      default:
        break;
      }

      UPDATE_IF_DIFFERENT(tbPosErrorOffset->Text, obrdSim->mdlPosErrorOffset.ToString());
      UPDATE_IF_DIFFERENT(tbLastCarBP->Text, obrdSim->mdlOverrideLastCarBP.ToString());
      UPDATE_IF_DIFFERENT(cbEnableComWithATP->Checked, obrdSim->mdlEnableComWithATP);

      bApplyOBRDSim->Enabled = false;
      bSaveOBRDSim->Enabled = false;

      // ----- OBRDSim Tab 2 -------

      dgTrackParams->Rows->Clear();
      
      if (obrdSim->mdlNumberOfTracks > 0)
      {
        dgTrackParams->Rows->Add(obrdSim->mdlNumberOfTracks);

        for (int i = 0; i < obrdSim->mdlNumberOfTracks; i++)
        {
          dgTrackParams->Rows[i]->Cells[0]->Value = obrdSim->mdlTrackId[i];
          dgTrackParams->Rows[i]->Cells[1]->Value = obrdSim->mdlTrackLength[i];
        }
      }

      bApplyTrackParams->Enabled = false;
      bSaveTrackParams->Enabled = false;

      // ----- OBRDSim Tab 3 -------

      UPDATE_IF_DIFFERENT(tbSiteId->Text, obrdSim->mdlSiteId.ToString());
      UPDATE_IF_DIFFERENT(tbReceiverId->Text, obrdSim->mdlReceiverId);
      UPDATE_IF_DIFFERENT(tbSenderId->Text, obrdSim->mdlSenderId);
      UPDATE_IF_DIFFERENT(tbLocoId->Text, obrdSim->mdlLocoId.ToString());
      UPDATE_IF_DIFFERENT(tbProtocolMajorVersion->Text, obrdSim->mdlMajorVersion.ToString());
      UPDATE_IF_DIFFERENT(tbProtocolMinorVersion->Text, obrdSim->mdlMinorVersion.ToString());

      bApplyOtherParams->Enabled = false;
      bSaveOtherParams->Enabled = false;

      // ----- OBRDSim Tab 4 -------

      lvStatusToATP->Items->Clear();

      for (int i = 0; i < obrdSim->guiStatusToATPCnt; i++)
      {
        itemPtr = lvStatusToATP->Items->Add(obrdSim->guiStatusToATPHeader[i]); itemPtr->SubItems->Add("");
      }
            
      // ----- OBRDSim Tab 5 -------

      lvProtocolToATP->Items->Clear();

      for (int i = 0; i < obrdSim->guiProtocolToATPCnt; i++)
      {
        itemPtr = lvProtocolToATP->Items->Add(obrdSim->guiProtocolToATPHeader[i]); itemPtr->SubItems->Add("");
      }

      // ----- OBRDSim Tab 6 -------

      lvRejectFromATP->Items->Clear();

      for (int i = 0; i < obrdSim->guiRejectFromATPCnt; i++)
      {
        itemPtr = lvRejectFromATP->Items->Add(obrdSim->guiRejectFromATPHeader[i]); itemPtr->SubItems->Add("");
      }

      // ----- OBRDSim Tab 7 -------

      lvProtocolFromATP->Items->Clear();

      for (int i = 0; i < obrdSim->guiProtocolFromATPCnt; i++)
      {
        itemPtr = lvProtocolFromATP->Items->Add(obrdSim->guiProtocolFromATPHeader[i]); itemPtr->SubItems->Add("");
      }

    
    }

  protected:
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    ~OBRDSimForm()
    {
      if (components)
      {
        delete components;
      }
    }
  

	private:

    String^                     iniFileName;
    String^                     regRootKey;
    int                         formLeft, formTop;
private: System::Windows::Forms::DataGridViewTextBoxColumn^  Track;
private: System::Windows::Forms::TabPage^  tabPage6;
private: System::Windows::Forms::TabPage^  tabPage7;
private: System::Windows::Forms::Label^  label18;
private: System::Windows::Forms::ListView^  lvProtocolToATP;

private: System::Windows::Forms::ColumnHeader^  columnHeader5;
private: System::Windows::Forms::ColumnHeader^  columnHeader6;
private: System::Windows::Forms::Label^  label19;
private: System::Windows::Forms::ListView^  lvProtocolFromATP;

private: System::Windows::Forms::ColumnHeader^  columnHeader7;
private: System::Windows::Forms::ColumnHeader^  columnHeader8;
private: System::Windows::Forms::RadioButton^  rbOverrideBrakePressure;
private: System::Windows::Forms::Label^  label3;
private: System::Windows::Forms::Label^  label20;
private: System::Windows::Forms::TextBox^  tbVSIMLastCarBP;


private: System::Windows::Forms::DataGridViewTextBoxColumn^  Length;






    /**********************************************************
    * Function:     UpdateOBRDSimFromGUI
    * Description:  Update Simulator parameters from GUI
    **********************************************************/
    void UpdateOBRDSimFromGUI()
    {
      String^ ErrorMsg = "";
      System::Object^ ErrorCtrl = nullptr;
      
      bool paramsAreValid = true;

      // Fetch values from GUI and write to model.
      try
      {
        ErrorMsg = "Train Length ";
        ErrorCtrl = (System::Object^)tbTrainLength;
        unsigned short tmpTrainLength = Convert::ToUInt16(tbTrainLength->Text);

        ErrorMsg = "Time Stamp Delay ";
        ErrorCtrl = (System::Object^)tbTimestampDelay;
        unsigned short tmpTimestampDelay = Convert::ToUInt16(tbTimestampDelay->Text);

        if ((9 < tmpTimestampDelay) || ( 0 > tmpTimestampDelay))
        {
          System::Windows::Forms::MessageBox::Show("Delay shall be 0-9 (x500ms) i.e. min 0 and max 4.5 seconds.",
            "",
            (MessageBoxButtons)0,
            MessageBoxIcon::Error);

          paramsAreValid = false;
        }

        ErrorMsg = "VSIM Last Car BP ";
        ErrorCtrl = (System::Object^)tbVSIMLastCarBP;
        Byte tmpVSIMLastCarBP = Convert::ToByte(tbVSIMLastCarBP->Text);

        ErrorMsg = "Override Last Car BP ";
        ErrorCtrl = (System::Object^)tbLastCarBP;
        Byte tmpLastCarBP = Convert::ToByte(tbLastCarBP->Text);

        ErrorMsg = "Status Report Periodicity ";
        ErrorCtrl = (System::Object^)tbStatusReportPeriodicity;
        unsigned short tmpStatusReportPeriodicity = Convert::ToUInt16(tbStatusReportPeriodicity->Text);

        ErrorMsg = "Pos Error Offset ";
        ErrorCtrl = (System::Object^)tbPosErrorOffset;
        short tmpPosErrorOffset = Convert::ToInt16(tbPosErrorOffset->Text);
        
        if (paramsAreValid)
        {
          // Validation ok, update all values.
          UPDATE_IF_DIFFERENT(obrdSim->mdlTrainLength, tmpTrainLength);
          UPDATE_IF_DIFFERENT(obrdSim->mdlTimestampDelay, tmpTimestampDelay);
          UPDATE_IF_DIFFERENT(obrdSim->mdlStatusReportPeriodicity, tmpStatusReportPeriodicity);
          UPDATE_IF_DIFFERENT(obrdSim->mdlVSIMLastCarBP, tmpVSIMLastCarBP);

          if (rbFINoError->Checked)
            obrdSim->mdlFaultInjection = FINoError;
          else if (rbFIFreezePOS->Checked)
            obrdSim->mdlFaultInjection = FIFreezePOS;
          else if (rbFIFailedToReadGPS->Checked)
            obrdSim->mdlFaultInjection = FIFailedToReadGPS;
          else if (rbFICRCFailure->Checked)
            obrdSim->mdlFaultInjection = FICRCFailure;
          else if (rbIgnoreRejection->Checked)
            obrdSim->mdlFaultInjection = FIIgnoreRejection;
          else if (rbFIPosErrorOffset->Checked)
            obrdSim->mdlFaultInjection = FIPosErrorOffset;
          else
            obrdSim->mdlFaultInjection = FIOverrideBrakePressure;

          UPDATE_IF_DIFFERENT(obrdSim->mdlPosErrorOffset, tmpPosErrorOffset);
          UPDATE_IF_DIFFERENT(obrdSim->mdlOverrideLastCarBP, tmpLastCarBP);
          UPDATE_IF_DIFFERENT(obrdSim->mdlEnableComWithATP, cbEnableComWithATP->Checked ? 1 : 0);

          // All data is transfered from GUI to model -> Disable 'Apply' button and Enable 'Save' button
          bApplyOBRDSim->Enabled = false;
          bSaveOBRDSim->Enabled = true;
        }
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
    * Function:     UpdateTrackListFromGUI
    * Description:  Update TrackList values from GUI
    **********************************************************/
    void UpdateTrackListFromGUI()
    {
      String^ ErrorMsg = "";
      int i = 0;

      bool trackListIsValid = true;

      // Fetch values from GUI and write to model.
      try
      {
        // Number of tracks must be more than 0.
        if ((dgTrackParams->RowCount - 1) > 0)
        {
          array<unsigned short>^  tmpTrackId;
          array<unsigned long>^   tmpTrackLength;

          tmpTrackId = gcnew array<unsigned short>(maxTracks);
          tmpTrackLength = gcnew array<unsigned long>(maxTracks);

          for (i = 0; (i < dgTrackParams->RowCount - 1) && trackListIsValid; i++)
          {
            ErrorMsg = "TrackId ";
            tmpTrackId[i] = Convert::ToUInt16(dgTrackParams->Rows[i]->Cells[0]->Value);

            ErrorMsg = "Length ";
            tmpTrackLength[i] = Convert::ToUInt32(dgTrackParams->Rows[i]->Cells[1]->Value);

            if (0 == tmpTrackId[i])
            {
              dgTrackParams->CurrentCell = dgTrackParams->Rows[i]->Cells[0];
              
              System::Windows::Forms::MessageBox::Show("TrackId cannot be 0.",
                "Error in params",
                (MessageBoxButtons)0,
                MessageBoxIcon::Error);
              
              trackListIsValid = false;
            }

            for (int x = 0; x < i; ++x)
            {
              if (tmpTrackId[i] == tmpTrackId[x])
              {
                dgTrackParams->CurrentCell = dgTrackParams->Rows[x]->Cells[0];

                System::Windows::Forms::MessageBox::Show("TrackId must be unique.",
                  "Error in params",
                  (MessageBoxButtons)0,
                  MessageBoxIcon::Error);

                trackListIsValid = false;
              }
            }
          }

          if (trackListIsValid)
          {
            // Get the number of vehicles from row-count
            UPDATE_IF_DIFFERENT(obrdSim->mdlNumberOfTracks, (dgTrackParams->RowCount - 1));

            for (int i = 0; i < dgTrackParams->RowCount - 1; i++)
            {
              UPDATE_IF_DIFFERENT(obrdSim->mdlTrackId[i], tmpTrackId[i]);
              UPDATE_IF_DIFFERENT(obrdSim->mdlTrackLength[i], tmpTrackLength[i]);
            }

            // All data is transfered from GUI to model -> Disable 'Apply' button and Enable 'Save' button
            bApplyTrackParams->Enabled = false;
            bSaveTrackParams->Enabled = true;
          }
        }
        else
        {
          System::Windows::Forms::MessageBox::Show("The number of vehicles shall be > 0.",
            "Error in params",
            (MessageBoxButtons)0,
            MessageBoxIcon::Error);
        }
      }
      catch (...)
      {
        dgTrackParams->CurrentCell = dgTrackParams->Rows[i]->Cells[0];

        System::Windows::Forms::MessageBox::Show(ErrorMsg + "can not be converted to a valid integer.",
          "Error in params",
          (MessageBoxButtons)0,
          MessageBoxIcon::Error);
      }
    }

    /**********************************************************
    * Function:     UpdateOtherFromGUI
    * Description:  Update other values from GUI
    **********************************************************/
    void UpdateOtherFromGUI()
    {
      String^ ErrorMsg = "";
      System::Object^ ErrorCtrl = nullptr;

      // Fetch values from GUI and write to model.
      try
      {
        ErrorMsg = "Site ID ";
        ErrorCtrl = (System::Object^)tbSiteId;
        unsigned short tmpSiteId = Convert::ToUInt16(tbSiteId->Text);

        ErrorMsg = "Receiver ID ";
        ErrorCtrl = (System::Object^)tbReceiverId;
        String^ tmpReceiverId = Convert::ToString(tbReceiverId->Text);

        ErrorMsg = "Sender ID ";
        ErrorCtrl = (System::Object^)tbSenderId;
        String^ tmpSenderId = Convert::ToString(tbSenderId->Text);

        ErrorMsg = "Loco ID ";
        ErrorCtrl = (System::Object^)tbLocoId;
        unsigned short tmpLocoId = Convert::ToUInt16(tbLocoId->Text);

        ErrorMsg = "Major Version ";
        ErrorCtrl = (System::Object^)tbProtocolMajorVersion;
        Byte tmpMajorVersion = Convert::ToByte(tbProtocolMajorVersion->Text);

        ErrorMsg = "Minor Version ";
        ErrorCtrl = (System::Object^)tbProtocolMinorVersion;
        Byte tmpMinorVersion = Convert::ToByte(tbProtocolMinorVersion->Text);

        // Validation ok, update all values.
        UPDATE_IF_DIFFERENT(obrdSim->mdlSiteId, tmpSiteId);
        UPDATE_IF_DIFFERENT(obrdSim->mdlReceiverId, tmpReceiverId);
        UPDATE_IF_DIFFERENT(obrdSim->mdlSenderId, tmpSenderId);
        UPDATE_IF_DIFFERENT(obrdSim->mdlLocoId, tmpLocoId);
        UPDATE_IF_DIFFERENT(obrdSim->mdlMajorVersion, tmpMajorVersion);
        UPDATE_IF_DIFFERENT(obrdSim->mdlMinorVersion, tmpMinorVersion);

        // Notify OBRD Simulator that the safety parameters shall be updated in protocol-layer.
        obrdSim->UpdateSafetyParams();

        // All data is transfered from GUI to model -> Disable 'Apply' button and Enable 'Save' button
        bApplyOtherParams->Enabled = false;
        bSaveOtherParams->Enabled = true;
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

    /**************************************************************************************************
    * Function:     SaveOBRDSimValues
    * Description:  Save all parameter values to ini-file, update save-buttons for OBRDSim-tab
    **************************************************************************************************/
    void SaveOBRDSimValues()
    {
      obrdSim->SaveParametersToIniFile();

      bSaveOBRDSim->Enabled = false;
    }

    /**************************************************************************************************
    * Function:     SaveTrackParamsValues
    * Description:  Save all parameter values to tracks-ini-file, update save-buttons for TrackParams-tab
    **************************************************************************************************/
    void SaveTrackParamsValues()
    {
      obrdSim->SaveTrackParamsToIniFile();

      bSaveTrackParams->Enabled = false;
    }

    /**************************************************************************************************
    * Function:     SaveOtherValues
    * Description:  Save all parameter values to ini-file, update save-buttons for Other-tab
    **************************************************************************************************/
    void SaveOtherValues()
    {
      obrdSim->SaveParametersToIniFile();

      bSaveOtherParams->Enabled = false;
    }

    /**************************************************************************************************
    * Function:     UpdateTrackDataFromFile
    * Description:  Parse SiteData XML-file to update GUI with trackId and length.
    **************************************************************************************************/
    bool UpdateTrackDataFromFile(String^ inputFile, String^% loadResult)
    {
      XmlDocument ^ docSiteData = gcnew XmlDocument;
      bool retVal = true;
      unsigned short trkCnt = 0;

      // Process XML-file
      try
      {
        docSiteData->Load(inputFile);

        dgTrackParams->Rows->Clear();

        for each (XmlNode ^ node in docSiteData->DocumentElement->SelectNodes("Object"))
        {
          // Find tracks
          if (node->SelectSingleNode("ObjectType")->FirstChild->Value->Equals("BHPBTrack"))
          {
            for each (XmlNode ^ node1 in node->SelectNodes("Parameter"))
            {
              if (node1->SelectSingleNode("Name")->FirstChild->Value->Equals("trackId"))
              {
                dgTrackParams->Rows->Add();
                dgTrackParams->Rows[trkCnt]->Cells[0]->Value = Convert::ToUInt16(node1->SelectSingleNode("Value/UnsignedShortValue")->FirstChild->Value);
              }

              if (node1->SelectSingleNode("Name")->FirstChild->Value->Equals("length"))
              {
                unsigned long decimal, value;
                
                decimal = Convert::ToInt32(node1->SelectSingleNode("Value/NumberValue/Decimals")->FirstChild->Value);
                value = Convert::ToUInt32(node1->SelectSingleNode("Value/NumberValue/Value")->FirstChild->Value);
                
                dgTrackParams->Rows[trkCnt]->Cells[1]->Value = Convert::ToUInt32(value * pow(10, decimal));
              }
            }

            ++trkCnt;
          }
        }
      }
      catch (...)
      {
        loadResult = "Error when trying to load xml-file";
        retVal = false;
      }
      
      return retVal;
    }

private: System::Windows::Forms::Button^  bSaveTrackParams;
private: System::Windows::Forms::TextBox^  tbSiteDataFileName;


private: System::Windows::Forms::DataGridView^  dgTrackParams;



private: System::Windows::Forms::TabPage^  tabPage3;
private: System::Windows::Forms::GroupBox^  groupBox4;
private: System::Windows::Forms::TextBox^  tbProtocolMinorVersion;

private: System::Windows::Forms::TextBox^  tbProtocolMajorVersion;

private: System::Windows::Forms::Label^  label15;
private: System::Windows::Forms::Label^  label14;
private: System::Windows::Forms::Button^  bApplyOtherParams;

private: System::Windows::Forms::Button^  bSaveOtherParams;

private: System::Windows::Forms::GroupBox^  groupBox3;
private: System::Windows::Forms::Label^  label4;
private: System::Windows::Forms::TextBox^  tbLocoId;

private: System::Windows::Forms::TextBox^  tbSenderId;

private: System::Windows::Forms::TextBox^  tbReceiverId;

private: System::Windows::Forms::Label^  label5;
private: System::Windows::Forms::Label^  label6;
private: System::Windows::Forms::Label^  label7;
private: System::Windows::Forms::TextBox^  tbSiteId;

private: System::Windows::Forms::Button^  bSelectSiteDataFile;

private: System::Windows::Forms::TabPage^  tabPage4;
private: System::Windows::Forms::Label^  label12;
private: System::Windows::Forms::ListView^  lvStatusToATP;


private: System::Windows::Forms::ColumnHeader^  columnHeader1;
private: System::Windows::Forms::ColumnHeader^  columnHeader2;
private: System::Windows::Forms::TabPage^  tabPage5;
private: System::Windows::Forms::Label^  label16;
private: System::Windows::Forms::ListView^  lvRejectFromATP;


private: System::Windows::Forms::ColumnHeader^  columnHeader3;
private: System::Windows::Forms::ColumnHeader^  columnHeader4;

private: System::Windows::Forms::Label^  label23;


private: System::Windows::Forms::TabControl^  OBRDSim;

























private: System::Windows::Forms::TabPage^  tabPage2;
private: System::Windows::Forms::GroupBox^  groupBox5;
private: System::Windows::Forms::Button^  bApplyTrackParams;

private: System::Windows::Forms::TabPage^  tabPage1;
private: System::Windows::Forms::Button^  bApplyOBRDSim;

private: System::Windows::Forms::Button^  bSaveOBRDSim;


private: System::Windows::Forms::GroupBox^  groupBox2;
private: System::Windows::Forms::Label^  label13;
private: System::Windows::Forms::Label^  label11;
private: System::Windows::Forms::Label^  label10;
private: System::Windows::Forms::Label^  label9;
private: System::Windows::Forms::TextBox^  tbStatusReportPeriodicity;


private: System::Windows::Forms::Label^  label8;
private: System::Windows::Forms::TextBox^  tbLastCarBP;

private: System::Windows::Forms::TextBox^  tbTimestampDelay;

private: System::Windows::Forms::TextBox^  tbTrainLength;


private: System::Windows::Forms::Label^  label2;
private: System::Windows::Forms::Label^  label1;
private: System::Windows::Forms::GroupBox^  groupBox1;

private: System::Windows::Forms::Label^  label17;
private: System::Windows::Forms::RadioButton^  rbIgnoreRejection;


private: System::Windows::Forms::TextBox^  tbPosErrorOffset;
private: System::Windows::Forms::RadioButton^  rbFICRCFailure;



private: System::Windows::Forms::RadioButton^  rbFIFailedToReadGPS;



private: System::Windows::Forms::RadioButton^  rbFINoError;
private: System::Windows::Forms::RadioButton^  rbFIFreezePOS;




private: System::Windows::Forms::CheckBox^  cbEnableComWithATP;
private: System::Windows::Forms::RadioButton^  rbFIPosErrorOffset;





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
      System::Windows::Forms::ListViewItem^  listViewItem1 = (gcnew System::Windows::Forms::ListViewItem(L""));
      System::Windows::Forms::ListViewItem^  listViewItem2 = (gcnew System::Windows::Forms::ListViewItem(L""));
      System::Windows::Forms::ListViewItem^  listViewItem3 = (gcnew System::Windows::Forms::ListViewItem(L""));
      System::Windows::Forms::ListViewItem^  listViewItem4 = (gcnew System::Windows::Forms::ListViewItem(L""));
      System::ComponentModel::ComponentResourceManager^  resources = (gcnew System::ComponentModel::ComponentResourceManager(OBRDSimForm::typeid));
      this->bSaveTrackParams = (gcnew System::Windows::Forms::Button());
      this->tbSiteDataFileName = (gcnew System::Windows::Forms::TextBox());
      this->dgTrackParams = (gcnew System::Windows::Forms::DataGridView());
      this->Track = (gcnew System::Windows::Forms::DataGridViewTextBoxColumn());
      this->Length = (gcnew System::Windows::Forms::DataGridViewTextBoxColumn());
      this->tabPage3 = (gcnew System::Windows::Forms::TabPage());
      this->groupBox4 = (gcnew System::Windows::Forms::GroupBox());
      this->tbProtocolMinorVersion = (gcnew System::Windows::Forms::TextBox());
      this->tbProtocolMajorVersion = (gcnew System::Windows::Forms::TextBox());
      this->label15 = (gcnew System::Windows::Forms::Label());
      this->label14 = (gcnew System::Windows::Forms::Label());
      this->bApplyOtherParams = (gcnew System::Windows::Forms::Button());
      this->bSaveOtherParams = (gcnew System::Windows::Forms::Button());
      this->groupBox3 = (gcnew System::Windows::Forms::GroupBox());
      this->label4 = (gcnew System::Windows::Forms::Label());
      this->tbLocoId = (gcnew System::Windows::Forms::TextBox());
      this->tbSenderId = (gcnew System::Windows::Forms::TextBox());
      this->tbReceiverId = (gcnew System::Windows::Forms::TextBox());
      this->label5 = (gcnew System::Windows::Forms::Label());
      this->label6 = (gcnew System::Windows::Forms::Label());
      this->label7 = (gcnew System::Windows::Forms::Label());
      this->tbSiteId = (gcnew System::Windows::Forms::TextBox());
      this->bSelectSiteDataFile = (gcnew System::Windows::Forms::Button());
      this->tabPage4 = (gcnew System::Windows::Forms::TabPage());
      this->label12 = (gcnew System::Windows::Forms::Label());
      this->lvStatusToATP = (gcnew System::Windows::Forms::ListView());
      this->columnHeader1 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader2 = (gcnew System::Windows::Forms::ColumnHeader());
      this->tabPage5 = (gcnew System::Windows::Forms::TabPage());
      this->label16 = (gcnew System::Windows::Forms::Label());
      this->lvRejectFromATP = (gcnew System::Windows::Forms::ListView());
      this->columnHeader3 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader4 = (gcnew System::Windows::Forms::ColumnHeader());
      this->label23 = (gcnew System::Windows::Forms::Label());
      this->OBRDSim = (gcnew System::Windows::Forms::TabControl());
      this->tabPage1 = (gcnew System::Windows::Forms::TabPage());
      this->bApplyOBRDSim = (gcnew System::Windows::Forms::Button());
      this->bSaveOBRDSim = (gcnew System::Windows::Forms::Button());
      this->groupBox2 = (gcnew System::Windows::Forms::GroupBox());
      this->label20 = (gcnew System::Windows::Forms::Label());
      this->tbVSIMLastCarBP = (gcnew System::Windows::Forms::TextBox());
      this->label3 = (gcnew System::Windows::Forms::Label());
      this->label13 = (gcnew System::Windows::Forms::Label());
      this->label10 = (gcnew System::Windows::Forms::Label());
      this->label9 = (gcnew System::Windows::Forms::Label());
      this->tbStatusReportPeriodicity = (gcnew System::Windows::Forms::TextBox());
      this->label8 = (gcnew System::Windows::Forms::Label());
      this->tbTimestampDelay = (gcnew System::Windows::Forms::TextBox());
      this->tbTrainLength = (gcnew System::Windows::Forms::TextBox());
      this->label2 = (gcnew System::Windows::Forms::Label());
      this->label1 = (gcnew System::Windows::Forms::Label());
      this->groupBox1 = (gcnew System::Windows::Forms::GroupBox());
      this->rbOverrideBrakePressure = (gcnew System::Windows::Forms::RadioButton());
      this->label11 = (gcnew System::Windows::Forms::Label());
      this->rbFIPosErrorOffset = (gcnew System::Windows::Forms::RadioButton());
      this->label17 = (gcnew System::Windows::Forms::Label());
      this->rbIgnoreRejection = (gcnew System::Windows::Forms::RadioButton());
      this->tbPosErrorOffset = (gcnew System::Windows::Forms::TextBox());
      this->rbFICRCFailure = (gcnew System::Windows::Forms::RadioButton());
      this->tbLastCarBP = (gcnew System::Windows::Forms::TextBox());
      this->rbFIFailedToReadGPS = (gcnew System::Windows::Forms::RadioButton());
      this->rbFINoError = (gcnew System::Windows::Forms::RadioButton());
      this->rbFIFreezePOS = (gcnew System::Windows::Forms::RadioButton());
      this->cbEnableComWithATP = (gcnew System::Windows::Forms::CheckBox());
      this->tabPage2 = (gcnew System::Windows::Forms::TabPage());
      this->groupBox5 = (gcnew System::Windows::Forms::GroupBox());
      this->bApplyTrackParams = (gcnew System::Windows::Forms::Button());
      this->tabPage6 = (gcnew System::Windows::Forms::TabPage());
      this->label18 = (gcnew System::Windows::Forms::Label());
      this->lvProtocolToATP = (gcnew System::Windows::Forms::ListView());
      this->columnHeader5 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader6 = (gcnew System::Windows::Forms::ColumnHeader());
      this->tabPage7 = (gcnew System::Windows::Forms::TabPage());
      this->label19 = (gcnew System::Windows::Forms::Label());
      this->lvProtocolFromATP = (gcnew System::Windows::Forms::ListView());
      this->columnHeader7 = (gcnew System::Windows::Forms::ColumnHeader());
      this->columnHeader8 = (gcnew System::Windows::Forms::ColumnHeader());
      this->toolStrip1 = (gcnew System::Windows::Forms::ToolStrip());
      this->tsbATOGreen = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbATORed = (gcnew System::Windows::Forms::ToolStripButton());
      this->toolStripSeparator2 = (gcnew System::Windows::Forms::ToolStripSeparator());
      this->tsbLocoSimGreen = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbLocoSimRed = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbVSIMGreen = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbVSIMYellow = (gcnew System::Windows::Forms::ToolStripButton());
      this->tsbVSIMRed = (gcnew System::Windows::Forms::ToolStripButton());
      this->toolStripSeparator6 = (gcnew System::Windows::Forms::ToolStripSeparator());
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->dgTrackParams))->BeginInit();
      this->tabPage3->SuspendLayout();
      this->groupBox4->SuspendLayout();
      this->groupBox3->SuspendLayout();
      this->tabPage4->SuspendLayout();
      this->tabPage5->SuspendLayout();
      this->OBRDSim->SuspendLayout();
      this->tabPage1->SuspendLayout();
      this->groupBox2->SuspendLayout();
      this->groupBox1->SuspendLayout();
      this->tabPage2->SuspendLayout();
      this->groupBox5->SuspendLayout();
      this->tabPage6->SuspendLayout();
      this->tabPage7->SuspendLayout();
      this->toolStrip1->SuspendLayout();
      this->SuspendLayout();
      // 
      // bSaveTrackParams
      // 
      this->bSaveTrackParams->Location = System::Drawing::Point(250, 430);
      this->bSaveTrackParams->Name = L"bSaveTrackParams";
      this->bSaveTrackParams->Size = System::Drawing::Size(75, 23);
      this->bSaveTrackParams->TabIndex = 1;
      this->bSaveTrackParams->Text = L"Save";
      this->bSaveTrackParams->UseVisualStyleBackColor = true;
      this->bSaveTrackParams->Click += gcnew System::EventHandler(this, &OBRDSimForm::bSaveTrackParams_Click);
      // 
      // tbSiteDataFileName
      // 
      this->tbSiteDataFileName->Enabled = false;
      this->tbSiteDataFileName->Location = System::Drawing::Point(71, 29);
      this->tbSiteDataFileName->Name = L"tbSiteDataFileName";
      this->tbSiteDataFileName->Size = System::Drawing::Size(265, 20);
      this->tbSiteDataFileName->TabIndex = 5;
      this->tbSiteDataFileName->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbSiteDataFileName_TextChanged);
      // 
      // dgTrackParams
      // 
      this->dgTrackParams->AllowUserToResizeRows = false;
      this->dgTrackParams->ColumnHeadersHeightSizeMode = System::Windows::Forms::DataGridViewColumnHeadersHeightSizeMode::AutoSize;
      this->dgTrackParams->Columns->AddRange(gcnew cli::array< System::Windows::Forms::DataGridViewColumn^  >(2) {
        this->Track,
          this->Length
      });
      this->dgTrackParams->GridColor = System::Drawing::SystemColors::AppWorkspace;
      this->dgTrackParams->Location = System::Drawing::Point(12, 16);
      this->dgTrackParams->Name = L"dgTrackParams";
      this->dgTrackParams->Size = System::Drawing::Size(389, 312);
      this->dgTrackParams->TabIndex = 0;
      this->dgTrackParams->CellContentClick += gcnew System::Windows::Forms::DataGridViewCellEventHandler(this, &OBRDSimForm::dgTrackParams_CellContentClick);
      this->dgTrackParams->CellValueChanged += gcnew System::Windows::Forms::DataGridViewCellEventHandler(this, &OBRDSimForm::dgTrackParams_CellContentClick);
      this->dgTrackParams->RowsAdded += gcnew System::Windows::Forms::DataGridViewRowsAddedEventHandler(this, &OBRDSimForm::dgTrackParams_RowsAdded);
      this->dgTrackParams->RowsRemoved += gcnew System::Windows::Forms::DataGridViewRowsRemovedEventHandler(this, &OBRDSimForm::dgTrackParams_RowsRemoved);
      // 
      // Track
      // 
      this->Track->HeaderText = L"TrackId";
      this->Track->Name = L"Track";
      this->Track->Width = 173;
      // 
      // Length
      // 
      this->Length->HeaderText = L"Length(cm)";
      this->Length->Name = L"Length";
      this->Length->Width = 173;
      // 
      // tabPage3
      // 
      this->tabPage3->Controls->Add(this->groupBox4);
      this->tabPage3->Controls->Add(this->bApplyOtherParams);
      this->tabPage3->Controls->Add(this->bSaveOtherParams);
      this->tabPage3->Controls->Add(this->groupBox3);
      this->tabPage3->Location = System::Drawing::Point(4, 22);
      this->tabPage3->Name = L"tabPage3";
      this->tabPage3->Size = System::Drawing::Size(414, 463);
      this->tabPage3->TabIndex = 2;
      this->tabPage3->Text = L"Safety";
      this->tabPage3->UseVisualStyleBackColor = true;
      // 
      // groupBox4
      // 
      this->groupBox4->Controls->Add(this->tbProtocolMinorVersion);
      this->groupBox4->Controls->Add(this->tbProtocolMajorVersion);
      this->groupBox4->Controls->Add(this->label15);
      this->groupBox4->Controls->Add(this->label14);
      this->groupBox4->Location = System::Drawing::Point(19, 185);
      this->groupBox4->Name = L"groupBox4";
      this->groupBox4->Size = System::Drawing::Size(375, 87);
      this->groupBox4->TabIndex = 11;
      this->groupBox4->TabStop = false;
      this->groupBox4->Text = L"Protocol Version";
      // 
      // tbProtocolMinorVersion
      // 
      this->tbProtocolMinorVersion->Location = System::Drawing::Point(100, 48);
      this->tbProtocolMinorVersion->Name = L"tbProtocolMinorVersion";
      this->tbProtocolMinorVersion->Size = System::Drawing::Size(100, 20);
      this->tbProtocolMinorVersion->TabIndex = 10;
      this->tbProtocolMinorVersion->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbProtocolMinorVersion->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbProtocolMinorVersion_TextChanged);
      // 
      // tbProtocolMajorVersion
      // 
      this->tbProtocolMajorVersion->Location = System::Drawing::Point(100, 25);
      this->tbProtocolMajorVersion->Name = L"tbProtocolMajorVersion";
      this->tbProtocolMajorVersion->Size = System::Drawing::Size(100, 20);
      this->tbProtocolMajorVersion->TabIndex = 11;
      this->tbProtocolMajorVersion->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbProtocolMajorVersion->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbProtocolMajorVersion_TextChanged);
      // 
      // label15
      // 
      this->label15->AutoSize = true;
      this->label15->Location = System::Drawing::Point(17, 51);
      this->label15->Name = L"label15";
      this->label15->Size = System::Drawing::Size(74, 13);
      this->label15->TabIndex = 9;
      this->label15->Text = L"Minor Version:";
      // 
      // label14
      // 
      this->label14->AutoSize = true;
      this->label14->Location = System::Drawing::Point(17, 28);
      this->label14->Name = L"label14";
      this->label14->Size = System::Drawing::Size(74, 13);
      this->label14->TabIndex = 8;
      this->label14->Text = L"Major Version:";
      // 
      // bApplyOtherParams
      // 
      this->bApplyOtherParams->Location = System::Drawing::Point(331, 430);
      this->bApplyOtherParams->Name = L"bApplyOtherParams";
      this->bApplyOtherParams->Size = System::Drawing::Size(75, 23);
      this->bApplyOtherParams->TabIndex = 10;
      this->bApplyOtherParams->Text = L"Apply";
      this->bApplyOtherParams->UseVisualStyleBackColor = true;
      this->bApplyOtherParams->Click += gcnew System::EventHandler(this, &OBRDSimForm::bApplyOtherParams_Click);
      // 
      // bSaveOtherParams
      // 
      this->bSaveOtherParams->Location = System::Drawing::Point(250, 430);
      this->bSaveOtherParams->Name = L"bSaveOtherParams";
      this->bSaveOtherParams->Size = System::Drawing::Size(75, 23);
      this->bSaveOtherParams->TabIndex = 9;
      this->bSaveOtherParams->Text = L"Save";
      this->bSaveOtherParams->UseVisualStyleBackColor = true;
      this->bSaveOtherParams->Click += gcnew System::EventHandler(this, &OBRDSimForm::bSaveOtherParams_Click);
      // 
      // groupBox3
      // 
      this->groupBox3->Controls->Add(this->label4);
      this->groupBox3->Controls->Add(this->tbLocoId);
      this->groupBox3->Controls->Add(this->tbSenderId);
      this->groupBox3->Controls->Add(this->tbReceiverId);
      this->groupBox3->Controls->Add(this->label5);
      this->groupBox3->Controls->Add(this->label6);
      this->groupBox3->Controls->Add(this->label7);
      this->groupBox3->Controls->Add(this->tbSiteId);
      this->groupBox3->Location = System::Drawing::Point(19, 23);
      this->groupBox3->Name = L"groupBox3";
      this->groupBox3->Size = System::Drawing::Size(375, 140);
      this->groupBox3->TabIndex = 8;
      this->groupBox3->TabStop = false;
      this->groupBox3->Text = L"Safety Layer Params";
      // 
      // label4
      // 
      this->label4->AutoSize = true;
      this->label4->Location = System::Drawing::Point(17, 33);
      this->label4->Name = L"label4";
      this->label4->Size = System::Drawing::Size(42, 13);
      this->label4->TabIndex = 0;
      this->label4->Text = L"Site ID:";
      // 
      // tbLocoId
      // 
      this->tbLocoId->Location = System::Drawing::Point(100, 100);
      this->tbLocoId->Name = L"tbLocoId";
      this->tbLocoId->Size = System::Drawing::Size(206, 20);
      this->tbLocoId->TabIndex = 5;
      this->tbLocoId->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbLocoId->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbLocoId_TextChanged);
      // 
      // tbSenderId
      // 
      this->tbSenderId->Location = System::Drawing::Point(100, 77);
      this->tbSenderId->Name = L"tbSenderId";
      this->tbSenderId->Size = System::Drawing::Size(206, 20);
      this->tbSenderId->TabIndex = 6;
      this->tbSenderId->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbSenderId_TextChanged);
      // 
      // tbReceiverId
      // 
      this->tbReceiverId->Location = System::Drawing::Point(100, 53);
      this->tbReceiverId->Name = L"tbReceiverId";
      this->tbReceiverId->Size = System::Drawing::Size(206, 20);
      this->tbReceiverId->TabIndex = 7;
      this->tbReceiverId->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbReceiverId_TextChanged);
      // 
      // label5
      // 
      this->label5->AutoSize = true;
      this->label5->Location = System::Drawing::Point(17, 56);
      this->label5->Name = L"label5";
      this->label5->Size = System::Drawing::Size(67, 13);
      this->label5->TabIndex = 2;
      this->label5->Text = L"Receiver ID:";
      // 
      // label6
      // 
      this->label6->AutoSize = true;
      this->label6->Location = System::Drawing::Point(17, 80);
      this->label6->Name = L"label6";
      this->label6->Size = System::Drawing::Size(58, 13);
      this->label6->TabIndex = 3;
      this->label6->Text = L"Sender ID:";
      // 
      // label7
      // 
      this->label7->AutoSize = true;
      this->label7->Location = System::Drawing::Point(17, 103);
      this->label7->Name = L"label7";
      this->label7->Size = System::Drawing::Size(48, 13);
      this->label7->TabIndex = 4;
      this->label7->Text = L"Loco ID:";
      // 
      // tbSiteId
      // 
      this->tbSiteId->Location = System::Drawing::Point(100, 30);
      this->tbSiteId->Name = L"tbSiteId";
      this->tbSiteId->Size = System::Drawing::Size(206, 20);
      this->tbSiteId->TabIndex = 1;
      this->tbSiteId->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbSiteId->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbSiteId_TextChanged);
      // 
      // bSelectSiteDataFile
      // 
      this->bSelectSiteDataFile->Location = System::Drawing::Point(341, 29);
      this->bSelectSiteDataFile->Name = L"bSelectSiteDataFile";
      this->bSelectSiteDataFile->Size = System::Drawing::Size(25, 20);
      this->bSelectSiteDataFile->TabIndex = 6;
      this->bSelectSiteDataFile->Text = L"...";
      this->bSelectSiteDataFile->UseVisualStyleBackColor = true;
      this->bSelectSiteDataFile->Click += gcnew System::EventHandler(this, &OBRDSimForm::bSelectSiteDataFile_Click);
      // 
      // tabPage4
      // 
      this->tabPage4->Controls->Add(this->label12);
      this->tabPage4->Controls->Add(this->lvStatusToATP);
      this->tabPage4->Location = System::Drawing::Point(4, 22);
      this->tabPage4->Name = L"tabPage4";
      this->tabPage4->Padding = System::Windows::Forms::Padding(3);
      this->tabPage4->Size = System::Drawing::Size(414, 463);
      this->tabPage4->TabIndex = 3;
      this->tabPage4->Text = L"Status -> AOS";
      this->tabPage4->UseVisualStyleBackColor = true;
      // 
      // label12
      // 
      this->label12->AutoSize = true;
      this->label12->Location = System::Drawing::Point(21, 14);
      this->label12->Name = L"label12";
      this->label12->Size = System::Drawing::Size(156, 13);
      this->label12->TabIndex = 6;
      this->label12->Text = L"Last Status Packet sent to ATP";
      // 
      // lvStatusToATP
      // 
      this->lvStatusToATP->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(2) {
        this->columnHeader1,
          this->columnHeader2
      });
      this->lvStatusToATP->GridLines = true;
      this->lvStatusToATP->Items->AddRange(gcnew cli::array< System::Windows::Forms::ListViewItem^  >(1) { listViewItem1 });
      this->lvStatusToATP->Location = System::Drawing::Point(22, 34);
      this->lvStatusToATP->Name = L"lvStatusToATP";
      this->lvStatusToATP->Size = System::Drawing::Size(372, 410);
      this->lvStatusToATP->TabIndex = 5;
      this->lvStatusToATP->UseCompatibleStateImageBehavior = false;
      this->lvStatusToATP->View = System::Windows::Forms::View::Details;
      // 
      // columnHeader1
      // 
      this->columnHeader1->Text = L"Variable";
      this->columnHeader1->Width = 130;
      // 
      // columnHeader2
      // 
      this->columnHeader2->Text = L"Data";
      this->columnHeader2->Width = 250;
      // 
      // tabPage5
      // 
      this->tabPage5->Controls->Add(this->label16);
      this->tabPage5->Controls->Add(this->lvRejectFromATP);
      this->tabPage5->Location = System::Drawing::Point(4, 22);
      this->tabPage5->Name = L"tabPage5";
      this->tabPage5->Padding = System::Windows::Forms::Padding(3);
      this->tabPage5->Size = System::Drawing::Size(414, 463);
      this->tabPage5->TabIndex = 4;
      this->tabPage5->Text = L"Rej <- AOS";
      this->tabPage5->UseVisualStyleBackColor = true;
      // 
      // label16
      // 
      this->label16->AutoSize = true;
      this->label16->Location = System::Drawing::Point(21, 14);
      this->label16->Name = L"label16";
      this->label16->Size = System::Drawing::Size(203, 13);
      this->label16->TabIndex = 8;
      this->label16->Text = L"Last Rejection Packet received from ATP";
      // 
      // lvRejectFromATP
      // 
      this->lvRejectFromATP->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(2) {
        this->columnHeader3,
          this->columnHeader4
      });
      this->lvRejectFromATP->GridLines = true;
      this->lvRejectFromATP->Items->AddRange(gcnew cli::array< System::Windows::Forms::ListViewItem^  >(1) { listViewItem2 });
      this->lvRejectFromATP->Location = System::Drawing::Point(22, 34);
      this->lvRejectFromATP->Name = L"lvRejectFromATP";
      this->lvRejectFromATP->Size = System::Drawing::Size(372, 410);
      this->lvRejectFromATP->TabIndex = 7;
      this->lvRejectFromATP->UseCompatibleStateImageBehavior = false;
      this->lvRejectFromATP->View = System::Windows::Forms::View::Details;
      // 
      // columnHeader3
      // 
      this->columnHeader3->Text = L"Variable";
      this->columnHeader3->Width = 130;
      // 
      // columnHeader4
      // 
      this->columnHeader4->Text = L"Data";
      this->columnHeader4->Width = 250;
      // 
      // label23
      // 
      this->label23->AutoSize = true;
      this->label23->Location = System::Drawing::Point(17, 32);
      this->label23->Name = L"label23";
      this->label23->Size = System::Drawing::Size(23, 13);
      this->label23->TabIndex = 7;
      this->label23->Text = L"File";
      // 
      // OBRDSim
      // 
      this->OBRDSim->Controls->Add(this->tabPage1);
      this->OBRDSim->Controls->Add(this->tabPage2);
      this->OBRDSim->Controls->Add(this->tabPage3);
      this->OBRDSim->Controls->Add(this->tabPage4);
      this->OBRDSim->Controls->Add(this->tabPage6);
      this->OBRDSim->Controls->Add(this->tabPage5);
      this->OBRDSim->Controls->Add(this->tabPage7);
      this->OBRDSim->Location = System::Drawing::Point(12, 12);
      this->OBRDSim->Multiline = true;
      this->OBRDSim->Name = L"OBRDSim";
      this->OBRDSim->SelectedIndex = 0;
      this->OBRDSim->Size = System::Drawing::Size(422, 489);
      this->OBRDSim->TabIndex = 3;
      // 
      // tabPage1
      // 
      this->tabPage1->Controls->Add(this->bApplyOBRDSim);
      this->tabPage1->Controls->Add(this->bSaveOBRDSim);
      this->tabPage1->Controls->Add(this->groupBox2);
      this->tabPage1->Controls->Add(this->groupBox1);
      this->tabPage1->Controls->Add(this->cbEnableComWithATP);
      this->tabPage1->Location = System::Drawing::Point(4, 22);
      this->tabPage1->Name = L"tabPage1";
      this->tabPage1->Padding = System::Windows::Forms::Padding(3);
      this->tabPage1->Size = System::Drawing::Size(414, 463);
      this->tabPage1->TabIndex = 0;
      this->tabPage1->Text = L"Main";
      this->tabPage1->UseVisualStyleBackColor = true;
      // 
      // bApplyOBRDSim
      // 
      this->bApplyOBRDSim->Enabled = false;
      this->bApplyOBRDSim->Location = System::Drawing::Point(331, 430);
      this->bApplyOBRDSim->Name = L"bApplyOBRDSim";
      this->bApplyOBRDSim->Size = System::Drawing::Size(75, 23);
      this->bApplyOBRDSim->TabIndex = 4;
      this->bApplyOBRDSim->Text = L"Apply";
      this->bApplyOBRDSim->UseVisualStyleBackColor = true;
      this->bApplyOBRDSim->Click += gcnew System::EventHandler(this, &OBRDSimForm::bApplyOBRDSim_Click);
      // 
      // bSaveOBRDSim
      // 
      this->bSaveOBRDSim->Enabled = false;
      this->bSaveOBRDSim->Location = System::Drawing::Point(250, 430);
      this->bSaveOBRDSim->Name = L"bSaveOBRDSim";
      this->bSaveOBRDSim->Size = System::Drawing::Size(75, 23);
      this->bSaveOBRDSim->TabIndex = 3;
      this->bSaveOBRDSim->Text = L"Save";
      this->bSaveOBRDSim->UseVisualStyleBackColor = true;
      this->bSaveOBRDSim->Click += gcnew System::EventHandler(this, &OBRDSimForm::bSaveOBRDSim_Click);
      // 
      // groupBox2
      // 
      this->groupBox2->Controls->Add(this->label20);
      this->groupBox2->Controls->Add(this->tbVSIMLastCarBP);
      this->groupBox2->Controls->Add(this->label3);
      this->groupBox2->Controls->Add(this->label13);
      this->groupBox2->Controls->Add(this->label10);
      this->groupBox2->Controls->Add(this->label9);
      this->groupBox2->Controls->Add(this->tbStatusReportPeriodicity);
      this->groupBox2->Controls->Add(this->label8);
      this->groupBox2->Controls->Add(this->tbTimestampDelay);
      this->groupBox2->Controls->Add(this->tbTrainLength);
      this->groupBox2->Controls->Add(this->label2);
      this->groupBox2->Controls->Add(this->label1);
      this->groupBox2->Location = System::Drawing::Point(22, 21);
      this->groupBox2->Name = L"groupBox2";
      this->groupBox2->Size = System::Drawing::Size(373, 121);
      this->groupBox2->TabIndex = 2;
      this->groupBox2->TabStop = false;
      this->groupBox2->Text = L"Parameters";
      // 
      // label20
      // 
      this->label20->AutoSize = true;
      this->label20->Location = System::Drawing::Point(291, 92);
      this->label20->Name = L"label20";
      this->label20->Size = System::Drawing::Size(32, 13);
      this->label20->TabIndex = 14;
      this->label20->Text = L"PSIG";
      // 
      // tbVSIMLastCarBP
      // 
      this->tbVSIMLastCarBP->Location = System::Drawing::Point(187, 89);
      this->tbVSIMLastCarBP->Name = L"tbVSIMLastCarBP";
      this->tbVSIMLastCarBP->Size = System::Drawing::Size(100, 20);
      this->tbVSIMLastCarBP->TabIndex = 13;
      this->tbVSIMLastCarBP->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbVSIMLastCarBP->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbVSIMLastCarBP_TextChanged);
      // 
      // label3
      // 
      this->label3->AutoSize = true;
      this->label3->Location = System::Drawing::Point(16, 92);
      this->label3->Name = L"label3";
      this->label3->Size = System::Drawing::Size(165, 13);
      this->label3->TabIndex = 12;
      this->label3->Text = L"Last car brake pressure for VSIM:";
      this->label3->Click += gcnew System::EventHandler(this, &OBRDSimForm::label3_Click);
      // 
      // label13
      // 
      this->label13->AutoSize = true;
      this->label13->Location = System::Drawing::Point(292, 68);
      this->label13->Name = L"label13";
      this->label13->Size = System::Drawing::Size(24, 13);
      this->label13->TabIndex = 11;
      this->label13->Text = L"sec";
      // 
      // label10
      // 
      this->label10->AutoSize = true;
      this->label10->Location = System::Drawing::Point(292, 44);
      this->label10->Name = L"label10";
      this->label10->Size = System::Drawing::Size(58, 13);
      this->label10->TabIndex = 9;
      this->label10->Text = L"( x 500ms )";
      // 
      // label9
      // 
      this->label9->AutoSize = true;
      this->label9->Location = System::Drawing::Point(292, 20);
      this->label9->Name = L"label9";
      this->label9->Size = System::Drawing::Size(15, 13);
      this->label9->TabIndex = 8;
      this->label9->Text = L"m";
      // 
      // tbStatusReportPeriodicity
      // 
      this->tbStatusReportPeriodicity->Location = System::Drawing::Point(187, 65);
      this->tbStatusReportPeriodicity->Name = L"tbStatusReportPeriodicity";
      this->tbStatusReportPeriodicity->Size = System::Drawing::Size(100, 20);
      this->tbStatusReportPeriodicity->TabIndex = 7;
      this->tbStatusReportPeriodicity->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbStatusReportPeriodicity->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbStatusReportPeriodicity_TextChanged);
      // 
      // label8
      // 
      this->label8->AutoSize = true;
      this->label8->Location = System::Drawing::Point(16, 68);
      this->label8->Name = L"label8";
      this->label8->Size = System::Drawing::Size(123, 13);
      this->label8->TabIndex = 6;
      this->label8->Text = L"StatusReport Periodicity:";
      // 
      // tbTimestampDelay
      // 
      this->tbTimestampDelay->Location = System::Drawing::Point(187, 41);
      this->tbTimestampDelay->Name = L"tbTimestampDelay";
      this->tbTimestampDelay->Size = System::Drawing::Size(100, 20);
      this->tbTimestampDelay->TabIndex = 4;
      this->tbTimestampDelay->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbTimestampDelay->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbTimestampDelay_TextChanged);
      // 
      // tbTrainLength
      // 
      this->tbTrainLength->Location = System::Drawing::Point(187, 17);
      this->tbTrainLength->Name = L"tbTrainLength";
      this->tbTrainLength->Size = System::Drawing::Size(100, 20);
      this->tbTrainLength->TabIndex = 3;
      this->tbTrainLength->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbTrainLength->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbTrainLength_TextChanged);
      // 
      // label2
      // 
      this->label2->AutoSize = true;
      this->label2->Location = System::Drawing::Point(16, 44);
      this->label2->Name = L"label2";
      this->label2->Size = System::Drawing::Size(149, 13);
      this->label2->TabIndex = 1;
      this->label2->Text = L"OBRD Timestamp Delay (0-9):";
      // 
      // label1
      // 
      this->label1->AutoSize = true;
      this->label1->Location = System::Drawing::Point(16, 20);
      this->label1->Name = L"label1";
      this->label1->Size = System::Drawing::Size(70, 13);
      this->label1->TabIndex = 0;
      this->label1->Text = L"Train Length:";
      // 
      // groupBox1
      // 
      this->groupBox1->Controls->Add(this->rbOverrideBrakePressure);
      this->groupBox1->Controls->Add(this->label11);
      this->groupBox1->Controls->Add(this->rbFIPosErrorOffset);
      this->groupBox1->Controls->Add(this->label17);
      this->groupBox1->Controls->Add(this->rbIgnoreRejection);
      this->groupBox1->Controls->Add(this->tbPosErrorOffset);
      this->groupBox1->Controls->Add(this->rbFICRCFailure);
      this->groupBox1->Controls->Add(this->tbLastCarBP);
      this->groupBox1->Controls->Add(this->rbFIFailedToReadGPS);
      this->groupBox1->Controls->Add(this->rbFINoError);
      this->groupBox1->Controls->Add(this->rbFIFreezePOS);
      this->groupBox1->Location = System::Drawing::Point(22, 159);
      this->groupBox1->Name = L"groupBox1";
      this->groupBox1->Size = System::Drawing::Size(373, 191);
      this->groupBox1->TabIndex = 1;
      this->groupBox1->TabStop = false;
      this->groupBox1->Text = L"FaultInjection";
      // 
      // rbOverrideBrakePressure
      // 
      this->rbOverrideBrakePressure->AutoSize = true;
      this->rbOverrideBrakePressure->Location = System::Drawing::Point(16, 163);
      this->rbOverrideBrakePressure->Name = L"rbOverrideBrakePressure";
      this->rbOverrideBrakePressure->Size = System::Drawing::Size(178, 17);
      this->rbOverrideBrakePressure->TabIndex = 15;
      this->rbOverrideBrakePressure->TabStop = true;
      this->rbOverrideBrakePressure->Text = L"Override last car brake pressure:";
      this->rbOverrideBrakePressure->UseVisualStyleBackColor = true;
      // 
      // label11
      // 
      this->label11->AutoSize = true;
      this->label11->Location = System::Drawing::Point(305, 163);
      this->label11->Name = L"label11";
      this->label11->Size = System::Drawing::Size(32, 13);
      this->label11->TabIndex = 10;
      this->label11->Text = L"PSIG";
      // 
      // rbFIPosErrorOffset
      // 
      this->rbFIPosErrorOffset->AutoSize = true;
      this->rbFIPosErrorOffset->Location = System::Drawing::Point(16, 139);
      this->rbFIPosErrorOffset->Name = L"rbFIPosErrorOffset";
      this->rbFIPosErrorOffset->Size = System::Drawing::Size(215, 17);
      this->rbFIPosErrorOffset->TabIndex = 6;
      this->rbFIPosErrorOffset->Text = L"OBRD pos offset (+ = add , - = subtract):";
      this->rbFIPosErrorOffset->UseVisualStyleBackColor = true;
      this->rbFIPosErrorOffset->CheckedChanged += gcnew System::EventHandler(this, &OBRDSimForm::rbFIPosErrorOffset_CheckedChanged);
      // 
      // label17
      // 
      this->label17->AutoSize = true;
      this->label17->Location = System::Drawing::Point(306, 139);
      this->label17->Name = L"label17";
      this->label17->Size = System::Drawing::Size(15, 13);
      this->label17->TabIndex = 14;
      this->label17->Text = L"m";
      // 
      // rbIgnoreRejection
      // 
      this->rbIgnoreRejection->AutoSize = true;
      this->rbIgnoreRejection->Location = System::Drawing::Point(16, 116);
      this->rbIgnoreRejection->Name = L"rbIgnoreRejection";
      this->rbIgnoreRejection->Size = System::Drawing::Size(289, 17);
      this->rbIgnoreRejection->TabIndex = 5;
      this->rbIgnoreRejection->Text = L"Ignore Rejection from AOS (send OBRD Status anyway)";
      this->rbIgnoreRejection->UseVisualStyleBackColor = true;
      this->rbIgnoreRejection->CheckedChanged += gcnew System::EventHandler(this, &OBRDSimForm::rbFIIgnoreRejection_CheckedChanged);
      // 
      // tbPosErrorOffset
      // 
      this->tbPosErrorOffset->Location = System::Drawing::Point(236, 136);
      this->tbPosErrorOffset->Name = L"tbPosErrorOffset";
      this->tbPosErrorOffset->Size = System::Drawing::Size(66, 20);
      this->tbPosErrorOffset->TabIndex = 13;
      this->tbPosErrorOffset->Text = L"0";
      this->tbPosErrorOffset->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbPosErrorOffset->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbPosErrorOffset_TextChanged);
      // 
      // rbFICRCFailure
      // 
      this->rbFICRCFailure->AutoSize = true;
      this->rbFICRCFailure->Location = System::Drawing::Point(16, 93);
      this->rbFICRCFailure->Name = L"rbFICRCFailure";
      this->rbFICRCFailure->Size = System::Drawing::Size(201, 17);
      this->rbFICRCFailure->TabIndex = 4;
      this->rbFICRCFailure->Text = L"Insert CRC Failure for status message";
      this->rbFICRCFailure->UseVisualStyleBackColor = true;
      this->rbFICRCFailure->CheckedChanged += gcnew System::EventHandler(this, &OBRDSimForm::rbFICRCFailure_CheckedChanged);
      // 
      // tbLastCarBP
      // 
      this->tbLastCarBP->Location = System::Drawing::Point(236, 160);
      this->tbLastCarBP->Name = L"tbLastCarBP";
      this->tbLastCarBP->Size = System::Drawing::Size(66, 20);
      this->tbLastCarBP->TabIndex = 5;
      this->tbLastCarBP->TextAlign = System::Windows::Forms::HorizontalAlignment::Right;
      this->tbLastCarBP->TextChanged += gcnew System::EventHandler(this, &OBRDSimForm::tbLastCarBP_TextChanged);
      // 
      // rbFIFailedToReadGPS
      // 
      this->rbFIFailedToReadGPS->AutoSize = true;
      this->rbFIFailedToReadGPS->Location = System::Drawing::Point(16, 70);
      this->rbFIFailedToReadGPS->Name = L"rbFIFailedToReadGPS";
      this->rbFIFailedToReadGPS->Size = System::Drawing::Size(291, 17);
      this->rbFIFailedToReadGPS->TabIndex = 3;
      this->rbFIFailedToReadGPS->Text = L"Simulate failure to read GPS / Conversion to track Failed";
      this->rbFIFailedToReadGPS->UseVisualStyleBackColor = true;
      this->rbFIFailedToReadGPS->CheckedChanged += gcnew System::EventHandler(this, &OBRDSimForm::rbFIFailedToReadGPS_CheckedChanged);
      // 
      // rbFINoError
      // 
      this->rbFINoError->AutoSize = true;
      this->rbFINoError->Location = System::Drawing::Point(16, 24);
      this->rbFINoError->Name = L"rbFINoError";
      this->rbFINoError->Size = System::Drawing::Size(64, 17);
      this->rbFINoError->TabIndex = 1;
      this->rbFINoError->TabStop = true;
      this->rbFINoError->Text = L"No Error";
      this->rbFINoError->UseVisualStyleBackColor = true;
      this->rbFINoError->CheckedChanged += gcnew System::EventHandler(this, &OBRDSimForm::rbFINoError_CheckedChanged);
      // 
      // rbFIFreezePOS
      // 
      this->rbFIFreezePOS->AutoSize = true;
      this->rbFIFreezePOS->Location = System::Drawing::Point(16, 47);
      this->rbFIFreezePOS->Name = L"rbFIFreezePOS";
      this->rbFIFreezePOS->Size = System::Drawing::Size(235, 17);
      this->rbFIFreezePOS->TabIndex = 2;
      this->rbFIFreezePOS->Text = L"Freeze OBRD unit position (last car dropped)";
      this->rbFIFreezePOS->UseVisualStyleBackColor = true;
      this->rbFIFreezePOS->CheckedChanged += gcnew System::EventHandler(this, &OBRDSimForm::rbFIFreezePOS_CheckedChanged);
      // 
      // cbEnableComWithATP
      // 
      this->cbEnableComWithATP->AutoSize = true;
      this->cbEnableComWithATP->Location = System::Drawing::Point(38, 367);
      this->cbEnableComWithATP->Name = L"cbEnableComWithATP";
      this->cbEnableComWithATP->Size = System::Drawing::Size(181, 17);
      this->cbEnableComWithATP->TabIndex = 0;
      this->cbEnableComWithATP->Text = L"Enable Communication with AOS";
      this->cbEnableComWithATP->UseVisualStyleBackColor = true;
      this->cbEnableComWithATP->CheckedChanged += gcnew System::EventHandler(this, &OBRDSimForm::cbEnableComWithATP_CheckedChanged);
      // 
      // tabPage2
      // 
      this->tabPage2->Controls->Add(this->groupBox5);
      this->tabPage2->Controls->Add(this->bApplyTrackParams);
      this->tabPage2->Controls->Add(this->bSaveTrackParams);
      this->tabPage2->Controls->Add(this->dgTrackParams);
      this->tabPage2->Location = System::Drawing::Point(4, 22);
      this->tabPage2->Name = L"tabPage2";
      this->tabPage2->Padding = System::Windows::Forms::Padding(3);
      this->tabPage2->Size = System::Drawing::Size(414, 463);
      this->tabPage2->TabIndex = 1;
      this->tabPage2->Text = L"Tracks";
      this->tabPage2->UseVisualStyleBackColor = true;
      // 
      // groupBox5
      // 
      this->groupBox5->Controls->Add(this->tbSiteDataFileName);
      this->groupBox5->Controls->Add(this->bSelectSiteDataFile);
      this->groupBox5->Controls->Add(this->label23);
      this->groupBox5->Location = System::Drawing::Point(12, 345);
      this->groupBox5->Name = L"groupBox5";
      this->groupBox5->Size = System::Drawing::Size(389, 72);
      this->groupBox5->TabIndex = 8;
      this->groupBox5->TabStop = false;
      this->groupBox5->Text = L"Import Site Data";
      // 
      // bApplyTrackParams
      // 
      this->bApplyTrackParams->Location = System::Drawing::Point(331, 430);
      this->bApplyTrackParams->Name = L"bApplyTrackParams";
      this->bApplyTrackParams->Size = System::Drawing::Size(75, 23);
      this->bApplyTrackParams->TabIndex = 2;
      this->bApplyTrackParams->Text = L"Apply";
      this->bApplyTrackParams->UseVisualStyleBackColor = true;
      this->bApplyTrackParams->Click += gcnew System::EventHandler(this, &OBRDSimForm::bApplyTrackParams_Click);
      // 
      // tabPage6
      // 
      this->tabPage6->Controls->Add(this->label18);
      this->tabPage6->Controls->Add(this->lvProtocolToATP);
      this->tabPage6->Location = System::Drawing::Point(4, 22);
      this->tabPage6->Name = L"tabPage6";
      this->tabPage6->Padding = System::Windows::Forms::Padding(3);
      this->tabPage6->Size = System::Drawing::Size(414, 463);
      this->tabPage6->TabIndex = 5;
      this->tabPage6->Text = L"Prot -> AOS";
      this->tabPage6->UseVisualStyleBackColor = true;
      // 
      // label18
      // 
      this->label18->AutoSize = true;
      this->label18->Location = System::Drawing::Point(21, 14);
      this->label18->Name = L"label18";
      this->label18->Size = System::Drawing::Size(165, 13);
      this->label18->TabIndex = 8;
      this->label18->Text = L"Last Protocol Packet sent to ATP";
      this->label18->Click += gcnew System::EventHandler(this, &OBRDSimForm::label18_Click);
      // 
      // lvProtocolToATP
      // 
      this->lvProtocolToATP->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(2) {
        this->columnHeader5,
          this->columnHeader6
      });
      this->lvProtocolToATP->GridLines = true;
      this->lvProtocolToATP->Items->AddRange(gcnew cli::array< System::Windows::Forms::ListViewItem^  >(1) { listViewItem3 });
      this->lvProtocolToATP->Location = System::Drawing::Point(22, 34);
      this->lvProtocolToATP->Name = L"lvProtocolToATP";
      this->lvProtocolToATP->Size = System::Drawing::Size(372, 410);
      this->lvProtocolToATP->TabIndex = 7;
      this->lvProtocolToATP->UseCompatibleStateImageBehavior = false;
      this->lvProtocolToATP->View = System::Windows::Forms::View::Details;
      // 
      // columnHeader5
      // 
      this->columnHeader5->Text = L"Variable";
      this->columnHeader5->Width = 130;
      // 
      // columnHeader6
      // 
      this->columnHeader6->Text = L"Data";
      this->columnHeader6->Width = 250;
      // 
      // tabPage7
      // 
      this->tabPage7->Controls->Add(this->label19);
      this->tabPage7->Controls->Add(this->lvProtocolFromATP);
      this->tabPage7->Location = System::Drawing::Point(4, 22);
      this->tabPage7->Name = L"tabPage7";
      this->tabPage7->Padding = System::Windows::Forms::Padding(3);
      this->tabPage7->Size = System::Drawing::Size(414, 463);
      this->tabPage7->TabIndex = 6;
      this->tabPage7->Text = L"Prot <- AOS";
      this->tabPage7->UseVisualStyleBackColor = true;
      // 
      // label19
      // 
      this->label19->AutoSize = true;
      this->label19->Location = System::Drawing::Point(21, 14);
      this->label19->Name = L"label19";
      this->label19->Size = System::Drawing::Size(197, 13);
      this->label19->TabIndex = 8;
      this->label19->Text = L"Last Protocol Packet received from ATP";
      // 
      // lvProtocolFromATP
      // 
      this->lvProtocolFromATP->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(2) {
        this->columnHeader7,
          this->columnHeader8
      });
      this->lvProtocolFromATP->GridLines = true;
      this->lvProtocolFromATP->Items->AddRange(gcnew cli::array< System::Windows::Forms::ListViewItem^  >(1) { listViewItem4 });
      this->lvProtocolFromATP->Location = System::Drawing::Point(22, 34);
      this->lvProtocolFromATP->Name = L"lvProtocolFromATP";
      this->lvProtocolFromATP->Size = System::Drawing::Size(372, 410);
      this->lvProtocolFromATP->TabIndex = 7;
      this->lvProtocolFromATP->UseCompatibleStateImageBehavior = false;
      this->lvProtocolFromATP->View = System::Windows::Forms::View::Details;
      // 
      // columnHeader7
      // 
      this->columnHeader7->Text = L"Variable";
      this->columnHeader7->Width = 130;
      // 
      // columnHeader8
      // 
      this->columnHeader8->Text = L"Data";
      this->columnHeader8->Width = 250;
      // 
      // toolStrip1
      // 
      this->toolStrip1->Dock = System::Windows::Forms::DockStyle::Bottom;
      this->toolStrip1->GripStyle = System::Windows::Forms::ToolStripGripStyle::Hidden;
      this->toolStrip1->Items->AddRange(gcnew cli::array< System::Windows::Forms::ToolStripItem^  >(9) {
        this->tsbATOGreen, this->tsbATORed,
          this->toolStripSeparator2, this->tsbLocoSimGreen, this->tsbLocoSimRed, this->tsbVSIMGreen, this->tsbVSIMYellow, this->tsbVSIMRed,
          this->toolStripSeparator6
      });
      this->toolStrip1->Location = System::Drawing::Point(0, 500);
      this->toolStrip1->Name = L"toolStrip1";
      this->toolStrip1->RenderMode = System::Windows::Forms::ToolStripRenderMode::System;
      this->toolStrip1->Size = System::Drawing::Size(446, 25);
      this->toolStrip1->TabIndex = 5;
      this->toolStrip1->Text = L"toolStrip1";
      // 
      // tsbATOGreen
      // 
      this->tsbATOGreen->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbATOGreen->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbATOGreen.Image")));
      this->tsbATOGreen->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbATOGreen->Name = L"tsbATOGreen";
      this->tsbATOGreen->Size = System::Drawing::Size(48, 22);
      this->tsbATOGreen->Text = L"ATP";
      this->tsbATOGreen->Visible = false;
      // 
      // tsbATORed
      // 
      this->tsbATORed->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
      this->tsbATORed->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbATORed.Image")));
      this->tsbATORed->ImageTransparentColor = System::Drawing::Color::Magenta;
      this->tsbATORed->Name = L"tsbATORed";
      this->tsbATORed->Size = System::Drawing::Size(48, 22);
      this->tsbATORed->Text = L"ATP";
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
      // OBRDSimForm
      // 
      this->AutoScaleDimensions = System::Drawing::SizeF(6, 13);
      this->AutoScaleMode = System::Windows::Forms::AutoScaleMode::Font;
      this->ClientSize = System::Drawing::Size(446, 525);
      this->ControlBox = false;
      this->Controls->Add(this->toolStrip1);
      this->Controls->Add(this->OBRDSim);
      this->FormBorderStyle = System::Windows::Forms::FormBorderStyle::FixedToolWindow;
      this->Name = L"OBRDSimForm";
      this->Text = L"OBRDSim";
      this->Load += gcnew System::EventHandler(this, &OBRDSimForm::OBRDSimForm_Load);
      this->Shown += gcnew System::EventHandler(this, &OBRDSimForm::OBRDSimForm_Shown);
      (cli::safe_cast<System::ComponentModel::ISupportInitialize^>(this->dgTrackParams))->EndInit();
      this->tabPage3->ResumeLayout(false);
      this->groupBox4->ResumeLayout(false);
      this->groupBox4->PerformLayout();
      this->groupBox3->ResumeLayout(false);
      this->groupBox3->PerformLayout();
      this->tabPage4->ResumeLayout(false);
      this->tabPage4->PerformLayout();
      this->tabPage5->ResumeLayout(false);
      this->tabPage5->PerformLayout();
      this->OBRDSim->ResumeLayout(false);
      this->tabPage1->ResumeLayout(false);
      this->tabPage1->PerformLayout();
      this->groupBox2->ResumeLayout(false);
      this->groupBox2->PerformLayout();
      this->groupBox1->ResumeLayout(false);
      this->groupBox1->PerformLayout();
      this->tabPage2->ResumeLayout(false);
      this->groupBox5->ResumeLayout(false);
      this->groupBox5->PerformLayout();
      this->tabPage6->ResumeLayout(false);
      this->tabPage6->PerformLayout();
      this->tabPage7->ResumeLayout(false);
      this->tabPage7->PerformLayout();
      this->toolStrip1->ResumeLayout(false);
      this->toolStrip1->PerformLayout();
      this->ResumeLayout(false);
      this->PerformLayout();

    }
#pragma endregion

private: System::Void tbTrainLength_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void tbVSIMLastCarBP_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void tbLastCarBP_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void tbTimestampDelay_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void tbStatusReportPeriodicity_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void rbFIFreezePOS_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void rbFIFailedToReadGPS_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void rbFICRCFailure_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void rbFIPosErrorOffset_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void rbFIIgnoreRejection_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void rbFINoError_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void tbPosErrorOffset_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void cbEnableComWithATP_CheckedChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOBRDSim->Enabled = true;
}
private: System::Void bApplyOBRDSim_Click(System::Object^  sender, System::EventArgs^  e) {
  UpdateOBRDSimFromGUI();
}
private: System::Void bSaveOBRDSim_Click(System::Object^  sender, System::EventArgs^  e) {
  SaveOBRDSimValues();
}

private: System::Void dgTrackParams_CellContentClick(System::Object^  sender, System::Windows::Forms::DataGridViewCellEventArgs^  e) {
  bApplyTrackParams->Enabled = true;
}
private: System::Void bApplyTrackParams_Click(System::Object^  sender, System::EventArgs^  e) {
  UpdateTrackListFromGUI();
}
private: System::Void bSaveTrackParams_Click(System::Object^  sender, System::EventArgs^  e) {
  SaveTrackParamsValues();
}
private: System::Void dgTrackParams_RowsAdded(System::Object^  sender, System::Windows::Forms::DataGridViewRowsAddedEventArgs^  e) {
  bApplyTrackParams->Enabled = true;
}
private: System::Void dgTrackParams_RowsRemoved(System::Object^  sender, System::Windows::Forms::DataGridViewRowsRemovedEventArgs^  e) {
  bApplyTrackParams->Enabled = true;
}
private: System::Void bSelectSiteDataFile_Click(System::Object^  sender, System::EventArgs^  e) {
  OpenFileDialog^ dialog = gcnew OpenFileDialog;

  dialog->InitialDirectory = ".\\";
  dialog->Filter = "All files (*.*)|*.*";
  dialog->FilterIndex = 1;
  dialog->RestoreDirectory = true;
  dialog->Title = "Select Site-Data file";

  if (dialog->ShowDialog() == System::Windows::Forms::DialogResult::OK)
  {
    String^ error;

    // Update textbox with selected file name
    tbSiteDataFileName->Text = dialog->FileName;

    if (!UpdateTrackDataFromFile(tbSiteDataFileName->Text, error))
    {
      System::Windows::Forms::MessageBox::Show(error,
        "",
        (MessageBoxButtons)0,
        MessageBoxIcon::Error);
    }
  }
}
private: System::Void bApplyOtherParams_Click(System::Object^  sender, System::EventArgs^  e) {
  UpdateOtherFromGUI();
}
private: System::Void bSaveOtherParams_Click(System::Object^  sender, System::EventArgs^  e) {
  SaveOtherValues();
}
private: System::Void tbSiteId_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOtherParams->Enabled = true;
}
private: System::Void tbReceiverId_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOtherParams->Enabled = true;
}
private: System::Void tbSenderId_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOtherParams->Enabled = true;
}
private: System::Void tbLocoId_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOtherParams->Enabled = true;
}
private: System::Void tbProtocolMajorVersion_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOtherParams->Enabled = true;
}
private: System::Void tbProtocolMinorVersion_TextChanged(System::Object^  sender, System::EventArgs^  e) {
  bApplyOtherParams->Enabled = true;
}
private: System::Void OBRDSimForm_Shown(System::Object^  sender, System::EventArgs^  e) {
  this->Left = formLeft;
  this->Top = formTop;
  this->Visible = Convert::ToInt16(Registry::GetValue(regRootKey + "\\LocoSim", "Visible", "1")) != 0 ? true : false;
}
private: System::Void OBRDSimForm_Load(System::Object^  sender, System::EventArgs^  e) {
  this->Text += obrdSim->DLLVersion;
}
private: System::Void tbSiteDataFileName_TextChanged(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void label18_Click(System::Object^  sender, System::EventArgs^  e) {
}
private: System::Void label3_Click(System::Object^  sender, System::EventArgs^  e) {
}
};
}