#pragma once
#pragma ident "@(#) Bombardier Transportation %full_filespec:  SiteDataExtForm.h-1:incl:arn_006#2 %"
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2014
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          SiteDataExtForm.h %
*
*  %version:       1 %
*
*  %created_by:    bhermans %
*
*  %date_created:  2016-09-16 16:17 %
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
* 2014-12-18    Antbäck     Corrected save of setups
*
*******************************************************************************/

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
using namespace System::Runtime::InteropServices;

using namespace SiteDataExtDLL;

namespace AOSPC {

    /// <summary>
    /// Summary for SiteDataExtForm
    ///
    /// WARNING: If you change the name of this class, you will need to change the
    ///          'Resource File Name' property for the managed resource compiler tool
    ///          associated with all .resx files this class depends on.  Otherwise,
    ///          the designers will not be able to interact properly with localized
    ///          resources associated with this form.
    /// </summary>
    public ref class SiteDataExtForm : public System::Windows::Forms::Form
    {
    private:
        // Form variables
        String ^            iniFileName;
        String ^            regRootKey;
        String ^            SiteDataExtractFileName;
        String ^            outputFileName;
        int                 formLeft, formTop;

    private: System::Windows::Forms::TabControl^  tabControl1;
    private: System::Windows::Forms::Button^  button1;
    private: System::Windows::Forms::GroupBox^  groupBox3;
    private: System::Windows::Forms::Button^  button2;
    private: System::Windows::Forms::TextBox^  textBox1;
    private: System::Windows::Forms::Label^  label20;

    private: System::Windows::Forms::ColumnHeader^  columnHeader8;
    private: System::Windows::Forms::ColumnHeader^  columnHeader9;
    private: System::Windows::Forms::ColumnHeader^  columnHeader10;
    private: System::Windows::Forms::TabPage^  tbOverview;

    private: System::Windows::Forms::GroupBox^  groupBox1;

    private: System::Windows::Forms::Label^  lGAVersion;
    private: System::Windows::Forms::Label^  lGAName;



    private: System::Windows::Forms::Label^  lGAproductID;
    private: System::Windows::Forms::Label^  label17;
    private: System::Windows::Forms::Label^  lSAVersion;
    private: System::Windows::Forms::Label^  lSAName;
    private: System::Windows::Forms::Label^  lable46;
    private: System::Windows::Forms::Label^  lable47;
    private: System::Windows::Forms::Label^  lable45;
    private: System::Windows::Forms::Label^  lSAproductID;
    private: System::Windows::Forms::Label^  label52;
    private: System::Windows::Forms::Label^  lExtractedSiteDataFileName;
    private: System::Windows::Forms::Label^  lable44;
    private: System::Windows::Forms::TabPage^  tbTrackList;
    private: System::Windows::Forms::Label^  label56;
    private: System::Windows::Forms::ListView^  lvTrackData;
    private: System::Windows::Forms::ColumnHeader^  columnHeader11;
    private: System::Windows::Forms::ColumnHeader^  columnHeader12;
    private: System::Windows::Forms::ColumnHeader^  columnHeader13;
    private: System::Windows::Forms::TabPage^  tbBaliseList;
    private: System::Windows::Forms::Label^  label57;
    private: System::Windows::Forms::ListView^  lvBaliseData;
    private: System::Windows::Forms::ColumnHeader^  columnHeader14;
    private: System::Windows::Forms::ColumnHeader^  columnHeader15;
    private: System::Windows::Forms::ColumnHeader^  columnHeader16;
    private: System::Windows::Forms::ColumnHeader^  columnHeader17;
    private: System::Windows::Forms::ColumnHeader^  columnHeader18;
    private: System::Windows::Forms::TabPage^  tbPantoShift;

    private: System::Windows::Forms::Label^  label58;
    private: System::Windows::Forms::ListView^  lvPantoShift;

    private: System::Windows::Forms::ColumnHeader^  columnHeader19;
    private: System::Windows::Forms::ColumnHeader^  columnHeader20;
    private: System::Windows::Forms::ColumnHeader^  columnHeader21;
    private: System::Windows::Forms::ColumnHeader^  columnHeader22;
    private: System::Windows::Forms::ColumnHeader^  columnHeader23;
    private: System::Windows::Forms::ColumnHeader^  columnHeader24;
    private: System::Windows::Forms::GroupBox^  groupBox2;
    private: System::Windows::Forms::Button^  bSiteDataExtFile;
    private: System::Windows::Forms::TextBox^  tbSiteDataExtFileName;


    private: System::Windows::Forms::Label^  label1;
    private: System::Windows::Forms::Button^  bExtract;
    private: System::Windows::Forms::Button^  bOutpuFile;
    private: System::Windows::Forms::TextBox^  tbOutputFile;



    private: System::Windows::Forms::Label^  label2;
    private: System::Windows::Forms::TabPage^  tbPantoZone;
    private: System::Windows::Forms::Label^  label3;
    private: System::Windows::Forms::ListView^  lvPantoZone;
    private: System::Windows::Forms::ColumnHeader^  chNo;


    private: System::Windows::Forms::ColumnHeader^  columnHeader25;
    private: System::Windows::Forms::ColumnHeader^  columnHeader26;
    private: System::Windows::Forms::ColumnHeader^  columnHeader27;
    private: System::Windows::Forms::ColumnHeader^  columnHeader28;

    private: System::Windows::Forms::ColumnHeader^  columnHeader1;
    private: System::Windows::Forms::ColumnHeader^  columnHeader2;
    private: System::Windows::Forms::ColumnHeader^  columnHeader3;
    private: System::Windows::Forms::ColumnHeader^  columnHeader4;
    private: System::Windows::Forms::ColumnHeader^  columnHeader5;
    private: System::Windows::Forms::ColumnHeader^  Pantoshiftzonesize;
    private: System::Windows::Forms::ColumnHeader^  columnHeader6;
    private: System::Windows::Forms::ColumnHeader^  columnHeader7;
    private: System::Windows::Forms::ColumnHeader^  columnHeader29;


             //Color               DefaultButtonColor;
             //int                 selIndexES2Id;
             //int                 selIndexTrainIntegrity;
             //int                 selIndexEAButton;
             //int                 selIndexLamp1;
             //int                 selIndexLamp2;
             SiteDataExt^          siteDataExt;
             //int                 startupES2Id;

    public:
        SiteDataExtForm(String^ regKey, String^ fileName)
        {
            iniFileName = fileName;
            regRootKey  = regKey;

            // Get window position from registry
            int left   = Convert::ToInt16(Registry::GetValue(regRootKey + "\\SiteDataExt", "Left", "32767"));
            int top    = Convert::ToInt16(Registry::GetValue(regRootKey + "\\SiteDataExt", "Top", "32767"));
            // If registry not available, first run for example, use default position from windows
            if ((left != 32767) &&
                (top != 32767))
            {
                formLeft    = left;
                formTop     = top;
            }

            // Create simulation
            siteDataExt = gcnew SiteDataExt(iniFileName);

            InitializeComponent();
            this->DoubleBuffered = true;
        }

        /**********************************************************
        * Function:     Init
        * Description:  
        **********************************************************/
    public:
        void Init(void)
        {
        }
        /**********************************************************
        * Function:     SaveWindowSettings
        * Description:  
        **********************************************************/
    public:
        void SaveWindowSettings(void)
        {
            Registry::SetValue(regRootKey + "\\SiteDataExt", "Left", String::Format("{0:0}", this->Left));
            Registry::SetValue(regRootKey + "\\SiteDataExt", "Top", String::Format("{0:0}", this->Top));
            Registry::SetValue(regRootKey + "\\SiteDataExt", "Visible", this->Visible ? "1" : "0");
        }

        /**********************************************************
        * Function:     ToggleVisibility
        * Description:  
        **********************************************************/
    public:
        void SetVisibility(bool visible)
        {
            if (visible)
            {
                this->Visible = true;
                this->Left = formLeft;
                this->Top  = formTop;
            }
            else
            {
                this->Visible = false;
                formLeft = this->Left;
                formTop = this->Top;
            }
        }
    public:
        
        /**********************************************************
        * Function:     LoadSetup
        * Description:  
        **********************************************************/
    public: void LoadSetup(String^ setup) {
                String^ tmpRootKey = regRootKey;
                if (!System::String::IsNullOrEmpty(setup)) 
                {
                    tmpRootKey = regRootKey + "\\" + setup + "\\SiteDataExt";
                }
                // Check if key exists
                String^ tmp = Convert::ToString(Registry::GetValue(tmpRootKey, "", "empty"));

                // Key exists, i.e. existing setup
                if (!System::String::IsNullOrEmpty(tmp)) 
                {
                    // Get window position from registry
                    int    left   = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Left", "32767"));
                    int    top    = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Top", "32767"));
                    bool   visible = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Visible", "1")) == 1 ? true : false;
                    // If registry not available, first run for example, use default position from windows
                    if (left != 32767)     formLeft    = left;
                    if (top != 32767)      formTop     = top;
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
                    tmpRootKey = regRootKey + "\\" + setup + "\\SiteDataExt";

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
                this->Left  = left;
                this->Top   = top;
                //this->Width = width;
                //this->Height = height;
                formLeft    = left;
                formTop     = top;
                //formWidth   = width;
                //formHeight  = height;
            }

    protected:
        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        ~SiteDataExtForm()
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
        System::ComponentModel::Container ^components;

#pragma region Windows Form Designer generated code
        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        void InitializeComponent(void)
        {
            this->tabControl1 = (gcnew System::Windows::Forms::TabControl());
            this->tbOverview = (gcnew System::Windows::Forms::TabPage());
            this->bExtract = (gcnew System::Windows::Forms::Button());
            this->groupBox2 = (gcnew System::Windows::Forms::GroupBox());
            this->bOutpuFile = (gcnew System::Windows::Forms::Button());
            this->tbOutputFile = (gcnew System::Windows::Forms::TextBox());
            this->label2 = (gcnew System::Windows::Forms::Label());
            this->bSiteDataExtFile = (gcnew System::Windows::Forms::Button());
            this->tbSiteDataExtFileName = (gcnew System::Windows::Forms::TextBox());
            this->label1 = (gcnew System::Windows::Forms::Label());
            this->groupBox1 = (gcnew System::Windows::Forms::GroupBox());
            this->lGAVersion = (gcnew System::Windows::Forms::Label());
            this->lGAName = (gcnew System::Windows::Forms::Label());
            this->lGAproductID = (gcnew System::Windows::Forms::Label());
            this->label17 = (gcnew System::Windows::Forms::Label());
            this->lSAVersion = (gcnew System::Windows::Forms::Label());
            this->lSAName = (gcnew System::Windows::Forms::Label());
            this->lable46 = (gcnew System::Windows::Forms::Label());
            this->lable47 = (gcnew System::Windows::Forms::Label());
            this->lable45 = (gcnew System::Windows::Forms::Label());
            this->lSAproductID = (gcnew System::Windows::Forms::Label());
            this->label52 = (gcnew System::Windows::Forms::Label());
            this->lExtractedSiteDataFileName = (gcnew System::Windows::Forms::Label());
            this->lable44 = (gcnew System::Windows::Forms::Label());
            this->tbTrackList = (gcnew System::Windows::Forms::TabPage());
            this->label56 = (gcnew System::Windows::Forms::Label());
            this->lvTrackData = (gcnew System::Windows::Forms::ListView());
            this->columnHeader11 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader12 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader13 = (gcnew System::Windows::Forms::ColumnHeader());
            this->tbBaliseList = (gcnew System::Windows::Forms::TabPage());
            this->label57 = (gcnew System::Windows::Forms::Label());
            this->lvBaliseData = (gcnew System::Windows::Forms::ListView());
            this->columnHeader14 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader15 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader16 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader17 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader18 = (gcnew System::Windows::Forms::ColumnHeader());
            this->tbPantoZone = (gcnew System::Windows::Forms::TabPage());
            this->label3 = (gcnew System::Windows::Forms::Label());
            this->lvPantoZone = (gcnew System::Windows::Forms::ListView());
            this->chNo = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader25 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader26 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader27 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader28 = (gcnew System::Windows::Forms::ColumnHeader());
            this->tbPantoShift = (gcnew System::Windows::Forms::TabPage());
            this->label58 = (gcnew System::Windows::Forms::Label());
            this->lvPantoShift = (gcnew System::Windows::Forms::ListView());
            this->columnHeader19 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader21 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader22 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader23 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader24 = (gcnew System::Windows::Forms::ColumnHeader());
            this->Pantoshiftzonesize = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader29 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader20 = (gcnew System::Windows::Forms::ColumnHeader());
            this->button1 = (gcnew System::Windows::Forms::Button());
            this->groupBox3 = (gcnew System::Windows::Forms::GroupBox());
            this->button2 = (gcnew System::Windows::Forms::Button());
            this->textBox1 = (gcnew System::Windows::Forms::TextBox());
            this->label20 = (gcnew System::Windows::Forms::Label());
            this->columnHeader8 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader9 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader10 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader1 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader2 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader3 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader4 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader5 = (gcnew System::Windows::Forms::ColumnHeader());
            this->columnHeader6 = (gcnew System::Windows::Forms::ColumnHeader());
            this->tabControl1->SuspendLayout();
            this->tbOverview->SuspendLayout();
            this->groupBox2->SuspendLayout();
            this->groupBox1->SuspendLayout();
            this->tbTrackList->SuspendLayout();
            this->tbBaliseList->SuspendLayout();
            this->tbPantoZone->SuspendLayout();
            this->tbPantoShift->SuspendLayout();
            this->groupBox3->SuspendLayout();
            this->SuspendLayout();
            // 
            // tabControl1
            // 
            this->tabControl1->Controls->Add(this->tbOverview);
            this->tabControl1->Controls->Add(this->tbTrackList);
            this->tabControl1->Controls->Add(this->tbBaliseList);
            this->tabControl1->Controls->Add(this->tbPantoZone);
            this->tabControl1->Controls->Add(this->tbPantoShift);
            this->tabControl1->Location = System::Drawing::Point(3, 5);
            this->tabControl1->Name = L"tabControl1";
            this->tabControl1->SelectedIndex = 0;
            this->tabControl1->Size = System::Drawing::Size(450, 315);
            this->tabControl1->TabIndex = 2;
            // 
            // tbOverview
            // 
            this->tbOverview->Controls->Add(this->bExtract);
            this->tbOverview->Controls->Add(this->groupBox2);
            this->tbOverview->Controls->Add(this->groupBox1);
            this->tbOverview->Location = System::Drawing::Point(4, 22);
            this->tbOverview->Name = L"tbOverview";
            this->tbOverview->Padding = System::Windows::Forms::Padding(3);
            this->tbOverview->Size = System::Drawing::Size(442, 289);
            this->tbOverview->TabIndex = 0;
            this->tbOverview->Text = L"Overview";
            this->tbOverview->UseVisualStyleBackColor = true;
            // 
            // bExtract
            // 
            this->bExtract->Location = System::Drawing::Point(12, 255);
            this->bExtract->Name = L"bExtract";
            this->bExtract->Size = System::Drawing::Size(75, 23);
            this->bExtract->TabIndex = 3;
            this->bExtract->Text = L"Extract";
            this->bExtract->UseVisualStyleBackColor = true;
            this->bExtract->Click += gcnew System::EventHandler(this, &SiteDataExtForm::bExtract_Click);
            // 
            // groupBox2
            // 
            this->groupBox2->Controls->Add(this->bOutpuFile);
            this->groupBox2->Controls->Add(this->tbOutputFile);
            this->groupBox2->Controls->Add(this->label2);
            this->groupBox2->Controls->Add(this->bSiteDataExtFile);
            this->groupBox2->Controls->Add(this->tbSiteDataExtFileName);
            this->groupBox2->Controls->Add(this->label1);
            this->groupBox2->Location = System::Drawing::Point(7, 149);
            this->groupBox2->Name = L"groupBox2";
            this->groupBox2->Size = System::Drawing::Size(412, 97);
            this->groupBox2->TabIndex = 2;
            this->groupBox2->TabStop = false;
            this->groupBox2->Text = L"Extract file";
            // 
            // bOutpuFile
            // 
            this->bOutpuFile->Location = System::Drawing::Point(357, 56);
            this->bOutpuFile->Name = L"bOutpuFile";
            this->bOutpuFile->Size = System::Drawing::Size(25, 20);
            this->bOutpuFile->TabIndex = 4;
            this->bOutpuFile->Text = L"...";
            this->bOutpuFile->UseVisualStyleBackColor = true;
            this->bOutpuFile->Click += gcnew System::EventHandler(this, &SiteDataExtForm::bOutpuFile_Click);
            // 
            // tbOutputFile
            // 
            this->tbOutputFile->Location = System::Drawing::Point(93, 56);
            this->tbOutputFile->Name = L"tbOutputFile";
            this->tbOutputFile->Size = System::Drawing::Size(264, 20);
            this->tbOutputFile->TabIndex = 3;
            this->tbOutputFile->Text = L"C:\\Documents and Settings\\bhidaji\\Desktop\\TestTrack.txt";
            // 
            // label2
            // 
            this->label2->AutoSize = true;
            this->label2->Location = System::Drawing::Point(16, 58);
            this->label2->Name = L"label2";
            this->label2->Size = System::Drawing::Size(55, 13);
            this->label2->TabIndex = 4;
            this->label2->Text = L"Output file";
            // 
            // bSiteDataExtFile
            // 
            this->bSiteDataExtFile->Location = System::Drawing::Point(356, 21);
            this->bSiteDataExtFile->Name = L"bSiteDataExtFile";
            this->bSiteDataExtFile->Size = System::Drawing::Size(25, 20);
            this->bSiteDataExtFile->TabIndex = 2;
            this->bSiteDataExtFile->Text = L"...";
            this->bSiteDataExtFile->UseVisualStyleBackColor = true;
            this->bSiteDataExtFile->Click += gcnew System::EventHandler(this, &SiteDataExtForm::bSiteDataExtFile_Click);
            // 
            // tbSiteDataExtFileName
            // 
            this->tbSiteDataExtFileName->Location = System::Drawing::Point(92, 21);
            this->tbSiteDataExtFileName->Name = L"tbSiteDataExtFileName";
            this->tbSiteDataExtFileName->Size = System::Drawing::Size(264, 20);
            this->tbSiteDataExtFileName->TabIndex = 1;
            // 
            // label1
            // 
            this->label1->AutoSize = true;
            this->label1->Location = System::Drawing::Point(15, 23);
            this->label1->Name = L"label1";
            this->label1->Size = System::Drawing::Size(65, 13);
            this->label1->TabIndex = 0;
            this->label1->Text = L"Site data file";
            // 
            // groupBox1
            // 
            this->groupBox1->Controls->Add(this->lGAVersion);
            this->groupBox1->Controls->Add(this->lGAName);
            this->groupBox1->Controls->Add(this->lGAproductID);
            this->groupBox1->Controls->Add(this->label17);
            this->groupBox1->Controls->Add(this->lSAVersion);
            this->groupBox1->Controls->Add(this->lSAName);
            this->groupBox1->Controls->Add(this->lable46);
            this->groupBox1->Controls->Add(this->lable47);
            this->groupBox1->Controls->Add(this->lable45);
            this->groupBox1->Controls->Add(this->lSAproductID);
            this->groupBox1->Controls->Add(this->label52);
            this->groupBox1->Controls->Add(this->lExtractedSiteDataFileName);
            this->groupBox1->Controls->Add(this->lable44);
            this->groupBox1->Location = System::Drawing::Point(7, 11);
            this->groupBox1->Name = L"groupBox1";
            this->groupBox1->Size = System::Drawing::Size(412, 135);
            this->groupBox1->TabIndex = 1;
            this->groupBox1->TabStop = false;
            this->groupBox1->Text = L"Extracted Site Data info";
            // 
            // lGAVersion
            // 
            this->lGAVersion->Font = (gcnew System::Drawing::Font(L"Microsoft Sans Serif", 8.25F, System::Drawing::FontStyle::Bold, System::Drawing::GraphicsUnit::Point, 
                static_cast<System::Byte>(0)));
            this->lGAVersion->Location = System::Drawing::Point(341, 73);
            this->lGAVersion->Name = L"lGAVersion";
            this->lGAVersion->Size = System::Drawing::Size(57, 13);
            this->lGAVersion->TabIndex = 13;
            this->lGAVersion->Text = L"-";
            // 
            // lGAName
            // 
            this->lGAName->Font = (gcnew System::Drawing::Font(L"Microsoft Sans Serif", 8.25F, System::Drawing::FontStyle::Bold, System::Drawing::GraphicsUnit::Point, 
                static_cast<System::Byte>(0)));
            this->lGAName->Location = System::Drawing::Point(71, 73);
            this->lGAName->Name = L"lGAName";
            this->lGAName->Size = System::Drawing::Size(90, 13);
            this->lGAName->TabIndex = 12;
            this->lGAName->Text = L"-";
            // 
            // lGAproductID
            // 
            this->lGAproductID->Font = (gcnew System::Drawing::Font(L"Microsoft Sans Serif", 8.25F, System::Drawing::FontStyle::Bold, System::Drawing::GraphicsUnit::Point, 
                static_cast<System::Byte>(0)));
            this->lGAproductID->Location = System::Drawing::Point(181, 73);
            this->lGAproductID->Name = L"lGAproductID";
            this->lGAproductID->Size = System::Drawing::Size(138, 13);
            this->lGAproductID->TabIndex = 7;
            this->lGAproductID->Text = L"-";
            // 
            // label17
            // 
            this->label17->AutoSize = true;
            this->label17->Location = System::Drawing::Point(12, 73);
            this->label17->Name = L"label17";
            this->label17->Size = System::Drawing::Size(22, 13);
            this->label17->TabIndex = 8;
            this->label17->Text = L"GA";
            // 
            // lSAVersion
            // 
            this->lSAVersion->Font = (gcnew System::Drawing::Font(L"Microsoft Sans Serif", 8.25F, System::Drawing::FontStyle::Bold, System::Drawing::GraphicsUnit::Point, 
                static_cast<System::Byte>(0)));
            this->lSAVersion->Location = System::Drawing::Point(341, 47);
            this->lSAVersion->Name = L"lSAVersion";
            this->lSAVersion->Size = System::Drawing::Size(57, 13);
            this->lSAVersion->TabIndex = 6;
            this->lSAVersion->Text = L"-";
            // 
            // lSAName
            // 
            this->lSAName->Font = (gcnew System::Drawing::Font(L"Microsoft Sans Serif", 8.25F, System::Drawing::FontStyle::Bold, System::Drawing::GraphicsUnit::Point, 
                static_cast<System::Byte>(0)));
            this->lSAName->Location = System::Drawing::Point(71, 47);
            this->lSAName->Name = L"lSAName";
            this->lSAName->Size = System::Drawing::Size(90, 13);
            this->lSAName->TabIndex = 5;
            this->lSAName->Text = L"-";
            // 
            // lable46
            // 
            this->lable46->AutoSize = true;
            this->lable46->Location = System::Drawing::Point(181, 21);
            this->lable46->Name = L"lable46";
            this->lable46->Size = System::Drawing::Size(58, 13);
            this->lable46->TabIndex = 4;
            this->lable46->Text = L"Product ID";
            // 
            // lable47
            // 
            this->lable47->AutoSize = true;
            this->lable47->Location = System::Drawing::Point(341, 21);
            this->lable47->Name = L"lable47";
            this->lable47->Size = System::Drawing::Size(42, 13);
            this->lable47->TabIndex = 3;
            this->lable47->Text = L"Version";
            // 
            // lable45
            // 
            this->lable45->AutoSize = true;
            this->lable45->Location = System::Drawing::Point(71, 21);
            this->lable45->Name = L"lable45";
            this->lable45->Size = System::Drawing::Size(35, 13);
            this->lable45->TabIndex = 2;
            this->lable45->Text = L"Name";
            // 
            // lSAproductID
            // 
            this->lSAproductID->Font = (gcnew System::Drawing::Font(L"Microsoft Sans Serif", 8.25F, System::Drawing::FontStyle::Bold, System::Drawing::GraphicsUnit::Point, 
                static_cast<System::Byte>(0)));
            this->lSAproductID->Location = System::Drawing::Point(181, 47);
            this->lSAproductID->Name = L"lSAproductID";
            this->lSAproductID->Size = System::Drawing::Size(138, 13);
            this->lSAproductID->TabIndex = 1;
            this->lSAproductID->Text = L"-";
            // 
            // label52
            // 
            this->label52->AutoSize = true;
            this->label52->Location = System::Drawing::Point(11, 49);
            this->label52->Name = L"label52";
            this->label52->Size = System::Drawing::Size(21, 13);
            this->label52->TabIndex = 1;
            this->label52->Text = L"SA";
            // 
            // lExtractedSiteDataFileName
            // 
            this->lExtractedSiteDataFileName->Font = (gcnew System::Drawing::Font(L"Microsoft Sans Serif", 8.25F, System::Drawing::FontStyle::Bold, 
                System::Drawing::GraphicsUnit::Point, static_cast<System::Byte>(0)));
            this->lExtractedSiteDataFileName->Location = System::Drawing::Point(182, 103);
            this->lExtractedSiteDataFileName->Name = L"lExtractedSiteDataFileName";
            this->lExtractedSiteDataFileName->Size = System::Drawing::Size(138, 13);
            this->lExtractedSiteDataFileName->TabIndex = 1;
            this->lExtractedSiteDataFileName->Text = L"-";
            // 
            // lable44
            // 
            this->lable44->AutoSize = true;
            this->lable44->Location = System::Drawing::Point(12, 103);
            this->lable44->Name = L"lable44";
            this->lable44->Size = System::Drawing::Size(50, 13);
            this->lable44->TabIndex = 1;
            this->lable44->Text = L"Input File";
            // 
            // tbTrackList
            // 
            this->tbTrackList->Controls->Add(this->label56);
            this->tbTrackList->Controls->Add(this->lvTrackData);
            this->tbTrackList->Location = System::Drawing::Point(4, 22);
            this->tbTrackList->Name = L"tbTrackList";
            this->tbTrackList->Padding = System::Windows::Forms::Padding(3);
            this->tbTrackList->Size = System::Drawing::Size(442, 289);
            this->tbTrackList->TabIndex = 6;
            this->tbTrackList->Text = L"Track List";
            this->tbTrackList->UseVisualStyleBackColor = true;
            // 
            // label56
            // 
            this->label56->AutoSize = true;
            this->label56->Location = System::Drawing::Point(6, 6);
            this->label56->Name = L"label56";
            this->label56->Size = System::Drawing::Size(124, 13);
            this->label56->TabIndex = 1;
            this->label56->Text = L"Loaded track information";
            // 
            // lvTrackData
            // 
            this->lvTrackData->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(3) {this->columnHeader11, 
                this->columnHeader12, this->columnHeader13});
            this->lvTrackData->HeaderStyle = System::Windows::Forms::ColumnHeaderStyle::Nonclickable;
            this->lvTrackData->Location = System::Drawing::Point(6, 22);
            this->lvTrackData->Name = L"lvTrackData";
            this->lvTrackData->Size = System::Drawing::Size(433, 264);
            this->lvTrackData->TabIndex = 0;
            this->lvTrackData->UseCompatibleStateImageBehavior = false;
            this->lvTrackData->View = System::Windows::Forms::View::Details;
            // 
            // columnHeader11
            // 
            this->columnHeader11->Text = L"No";
            this->columnHeader11->Width = 35;
            // 
            // columnHeader12
            // 
            this->columnHeader12->Text = L"Track name";
            this->columnHeader12->Width = 105;
            // 
            // columnHeader13
            // 
            this->columnHeader13->Text = L"Track Length";
            this->columnHeader13->Width = 106;
            // 
            // tbBaliseList
            // 
            this->tbBaliseList->Controls->Add(this->label57);
            this->tbBaliseList->Controls->Add(this->lvBaliseData);
            this->tbBaliseList->Location = System::Drawing::Point(4, 22);
            this->tbBaliseList->Name = L"tbBaliseList";
            this->tbBaliseList->Size = System::Drawing::Size(442, 289);
            this->tbBaliseList->TabIndex = 9;
            this->tbBaliseList->Text = L"Balise List";
            this->tbBaliseList->UseVisualStyleBackColor = true;
            // 
            // label57
            // 
            this->label57->AutoSize = true;
            this->label57->Location = System::Drawing::Point(6, 6);
            this->label57->Name = L"label57";
            this->label57->Size = System::Drawing::Size(127, 13);
            this->label57->TabIndex = 3;
            this->label57->Text = L"Loaded balise information";
            // 
            // lvBaliseData
            // 
            this->lvBaliseData->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(5) {this->columnHeader14, 
                this->columnHeader15, this->columnHeader16, this->columnHeader17, this->columnHeader18});
            this->lvBaliseData->HeaderStyle = System::Windows::Forms::ColumnHeaderStyle::Nonclickable;
            this->lvBaliseData->Location = System::Drawing::Point(6, 22);
            this->lvBaliseData->Name = L"lvBaliseData";
            this->lvBaliseData->Size = System::Drawing::Size(431, 264);
            this->lvBaliseData->TabIndex = 2;
            this->lvBaliseData->UseCompatibleStateImageBehavior = false;
            this->lvBaliseData->View = System::Windows::Forms::View::Details;
            // 
            // columnHeader14
            // 
            this->columnHeader14->Text = L"No";
            this->columnHeader14->Width = 35;
            // 
            // columnHeader15
            // 
            this->columnHeader15->Text = L"Balise Name";
            this->columnHeader15->Width = 105;
            // 
            // columnHeader16
            // 
            this->columnHeader16->Text = L"Balise ID";
            // 
            // columnHeader17
            // 
            this->columnHeader17->Text = L"TrackName";
            this->columnHeader17->Width = 105;
            // 
            // columnHeader18
            // 
            this->columnHeader18->Text = L"Balise Position";
            this->columnHeader18->Width = 100;
            // 
            // tbPantoZone
            // 
            this->tbPantoZone->Controls->Add(this->label3);
            this->tbPantoZone->Controls->Add(this->lvPantoZone);
            this->tbPantoZone->Location = System::Drawing::Point(4, 22);
            this->tbPantoZone->Name = L"tbPantoZone";
            this->tbPantoZone->Size = System::Drawing::Size(442, 289);
            this->tbPantoZone->TabIndex = 11;
            this->tbPantoZone->Text = L"Panto Zone";
            this->tbPantoZone->UseVisualStyleBackColor = true;
            // 
            // label3
            // 
            this->label3->AutoSize = true;
            this->label3->Location = System::Drawing::Point(5, 5);
            this->label3->Name = L"label3";
            this->label3->Size = System::Drawing::Size(106, 13);
            this->label3->TabIndex = 7;
            this->label3->Text = L"Loaded panto Zones";
            // 
            // lvPantoZone
            // 
            this->lvPantoZone->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(5) {this->chNo, this->columnHeader25, 
                this->columnHeader26, this->columnHeader27, this->columnHeader28});
            this->lvPantoZone->HeaderStyle = System::Windows::Forms::ColumnHeaderStyle::Nonclickable;
            this->lvPantoZone->Location = System::Drawing::Point(5, 21);
            this->lvPantoZone->Name = L"lvPantoZone";
            this->lvPantoZone->Size = System::Drawing::Size(432, 265);
            this->lvPantoZone->TabIndex = 6;
            this->lvPantoZone->UseCompatibleStateImageBehavior = false;
            this->lvPantoZone->View = System::Windows::Forms::View::Details;
            // 
            // chNo
            // 
            this->chNo->Text = L"No";
            this->chNo->Width = 30;
            // 
            // columnHeader25
            // 
            this->columnHeader25->Text = L"Track Name";
            this->columnHeader25->Width = 90;
            // 
            // columnHeader26
            // 
            this->columnHeader26->Text = L"Start";
            this->columnHeader26->Width = 55;
            // 
            // columnHeader27
            // 
            this->columnHeader27->Text = L"End";
            this->columnHeader27->Width = 55;
            // 
            // columnHeader28
            // 
            this->columnHeader28->Text = L"Position";
            this->columnHeader28->Width = 90;
            // 
            // tbPantoShift
            // 
            this->tbPantoShift->Controls->Add(this->label58);
            this->tbPantoShift->Controls->Add(this->lvPantoShift);
            this->tbPantoShift->Location = System::Drawing::Point(4, 22);
            this->tbPantoShift->Name = L"tbPantoShift";
            this->tbPantoShift->Padding = System::Windows::Forms::Padding(3);
            this->tbPantoShift->Size = System::Drawing::Size(442, 289);
            this->tbPantoShift->TabIndex = 10;
            this->tbPantoShift->Text = L"Panto Shift List";
            this->tbPantoShift->UseVisualStyleBackColor = true;
            // 
            // label58
            // 
            this->label58->AutoSize = true;
            this->label58->Location = System::Drawing::Point(6, 6);
            this->label58->Name = L"label58";
            this->label58->Size = System::Drawing::Size(149, 13);
            this->label58->TabIndex = 5;
            this->label58->Text = L"Loaded panto shift information";
            // 
            // lvPantoShift
            // 
            this->lvPantoShift->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(8) {this->columnHeader19, 
                this->columnHeader21, this->columnHeader22, this->columnHeader23, this->columnHeader24, this->Pantoshiftzonesize, this->columnHeader29, 
                this->columnHeader20});
            this->lvPantoShift->HeaderStyle = System::Windows::Forms::ColumnHeaderStyle::Nonclickable;
            this->lvPantoShift->Location = System::Drawing::Point(6, 22);
            this->lvPantoShift->Name = L"lvPantoShift";
            this->lvPantoShift->Size = System::Drawing::Size(430, 264);
            this->lvPantoShift->TabIndex = 4;
            this->lvPantoShift->UseCompatibleStateImageBehavior = false;
            this->lvPantoShift->View = System::Windows::Forms::View::Details;
            // 
            // columnHeader19
            // 
            this->columnHeader19->Text = L"No";
            this->columnHeader19->Width = 30;
            // 
            // columnHeader21
            // 
            this->columnHeader21->Text = L"Nominal";
            this->columnHeader21->Width = 55;
            // 
            // columnHeader22
            // 
            this->columnHeader22->Text = L"Reverse";
            this->columnHeader22->Width = 55;
            // 
            // columnHeader23
            // 
            this->columnHeader23->Text = L"Track Name";
            this->columnHeader23->Width = 90;
            // 
            // columnHeader24
            // 
            this->columnHeader24->Text = L"Position";
            this->columnHeader24->Width = 55;
            // 
            // Pantoshiftzonesize
            // 
            this->Pantoshiftzonesize->Text = L"panto shift size";
            // 
            // columnHeader29
            // 
            this->columnHeader29->Text = L"Direction";
            // 
            // columnHeader20
            // 
            this->columnHeader20->Text = L"Panto Name";
            this->columnHeader20->Width = 90;
            // 
            // button1
            // 
            this->button1->Location = System::Drawing::Point(21, 286);
            this->button1->Name = L"button1";
            this->button1->Size = System::Drawing::Size(75, 23);
            this->button1->TabIndex = 5;
            this->button1->Text = L"Extract";
            this->button1->UseVisualStyleBackColor = true;
            // 
            // groupBox3
            // 
            this->groupBox3->Controls->Add(this->button2);
            this->groupBox3->Controls->Add(this->textBox1);
            this->groupBox3->Controls->Add(this->label20);
            this->groupBox3->Location = System::Drawing::Point(6, 210);
            this->groupBox3->Name = L"groupBox3";
            this->groupBox3->Size = System::Drawing::Size(412, 74);
            this->groupBox3->TabIndex = 4;
            this->groupBox3->TabStop = false;
            this->groupBox3->Text = L"Extract file";
            // 
            // button2
            // 
            this->button2->Location = System::Drawing::Point(356, 30);
            this->button2->Name = L"button2";
            this->button2->Size = System::Drawing::Size(25, 20);
            this->button2->TabIndex = 3;
            this->button2->Text = L"...";
            this->button2->UseVisualStyleBackColor = true;
            // 
            // textBox1
            // 
            this->textBox1->Location = System::Drawing::Point(92, 28);
            this->textBox1->Name = L"textBox1";
            this->textBox1->Size = System::Drawing::Size(264, 20);
            this->textBox1->TabIndex = 1;
            // 
            // label20
            // 
            this->label20->AutoSize = true;
            this->label20->Location = System::Drawing::Point(15, 30);
            this->label20->Name = L"label20";
            this->label20->Size = System::Drawing::Size(65, 13);
            this->label20->TabIndex = 0;
            this->label20->Text = L"Site data file";
            // 
            // columnHeader8
            // 
            this->columnHeader8->Text = L"No";
            this->columnHeader8->Width = 35;
            // 
            // columnHeader9
            // 
            this->columnHeader9->Text = L"Track name";
            this->columnHeader9->Width = 105;
            // 
            // columnHeader10
            // 
            this->columnHeader10->Text = L"Track Length";
            this->columnHeader10->Width = 106;
            // 
            // columnHeader1
            // 
            this->columnHeader1->Text = L"No";
            this->columnHeader1->Width = 30;
            // 
            // columnHeader2
            // 
            this->columnHeader2->Text = L"Name";
            this->columnHeader2->Width = 90;
            // 
            // columnHeader3
            // 
            this->columnHeader3->Text = L"Nominal";
            this->columnHeader3->Width = 55;
            // 
            // columnHeader4
            // 
            this->columnHeader4->Text = L"Reverse";
            this->columnHeader4->Width = 55;
            // 
            // columnHeader5
            // 
            this->columnHeader5->Text = L"Track Name";
            this->columnHeader5->Width = 90;
            // 
            // columnHeader6
            // 
            this->columnHeader6->Text = L"Position";
            this->columnHeader6->Width = 55;
            // 
            // SiteDataExtForm
            // 
            this->AutoScaleDimensions = System::Drawing::SizeF(6, 13);
            this->AutoScaleMode = System::Windows::Forms::AutoScaleMode::Font;
            this->ClientSize = System::Drawing::Size(456, 321);
            this->ControlBox = false;
            this->Controls->Add(this->tabControl1);
            this->FormBorderStyle = System::Windows::Forms::FormBorderStyle::FixedToolWindow;
            this->MaximizeBox = false;
            this->MaximumSize = System::Drawing::Size(462, 345);
            this->MinimumSize = System::Drawing::Size(462, 345);
            this->Name = L"SiteDataExtForm";
            this->Text = L"Site Data Extractor";
            this->Load += gcnew System::EventHandler(this, &SiteDataExtForm::SiteDataExtForm_Load);
            this->Shown += gcnew System::EventHandler(this, &SiteDataExtForm::SiteDataExtForm_Shown);
            this->tabControl1->ResumeLayout(false);
            this->tbOverview->ResumeLayout(false);
            this->groupBox2->ResumeLayout(false);
            this->groupBox2->PerformLayout();
            this->groupBox1->ResumeLayout(false);
            this->groupBox1->PerformLayout();
            this->tbTrackList->ResumeLayout(false);
            this->tbTrackList->PerformLayout();
            this->tbBaliseList->ResumeLayout(false);
            this->tbBaliseList->PerformLayout();
            this->tbPantoZone->ResumeLayout(false);
            this->tbPantoZone->PerformLayout();
            this->tbPantoShift->ResumeLayout(false);
            this->tbPantoShift->PerformLayout();
            this->groupBox3->ResumeLayout(false);
            this->groupBox3->PerformLayout();
            this->ResumeLayout(false);

        }
#pragma endregion
    private: System::Void SiteDataExtForm_Shown(System::Object^  sender, System::EventArgs^  e) {
                 this->Left = formLeft;
                 this->Top  = formTop;
                 this->Visible = Convert::ToInt16(Registry::GetValue(regRootKey + "\\SiteDataExt", "Visible", "1")) != 0 ? true : false;
             }
    private: System::Void SiteDataExtForm_Load(System::Object^  sender, System::EventArgs^  e) {
                 this->Text += siteDataExt->DLLVersion;
                 this->tbSiteDataExtFileName->Text =  siteDataExt->siteDataFileFromIni;
                 this->tbOutputFile->Text = siteDataExt->outputFileFromIni;
             }

    private: System::Void bSiteDataExtFile_Click(System::Object^  sender, System::EventArgs^  e) {
                 OpenFileDialog^ dialog = gcnew OpenFileDialog;

                 dialog->InitialDirectory = ".\\";
                 dialog->Filter = "All files (*.*)|*.*";
                 dialog->FilterIndex = 1;
                 dialog->RestoreDirectory = true;
                 dialog->Title = "Select site data file";

                 if ( dialog->ShowDialog() == System::Windows::Forms::DialogResult::OK )
                 {
                     // Update textbox with selected file name
                     tbSiteDataExtFileName->Text = dialog->FileName;
                 }             
             }
    private: System::Void bExtract_Click(System::Object^  sender, System::EventArgs^  e) {

                 // Copy parameters to "running"
                 SiteDataExtractFileName = tbSiteDataExtFileName->Text;
                 outputFileName =  tbOutputFile->Text; 

                 // Open file if needed
                 // Warn if file needed but name missing
                 if (String::IsNullOrEmpty(SiteDataExtractFileName))
                 {
                     System::Windows::Forms::MessageBox::Show("Missing file name to use for site data file.",
                         "Params error",
                         (MessageBoxButtons)0,
                         MessageBoxIcon::Error);
                 }
                 // Open file
                 else if (!siteDataExt->LoadSiteDataFile(SiteDataExtractFileName, outputFileName))
                 {
                     System::Windows::Forms::MessageBox::Show("Load of site data file failed.\n" + 
                         "File: " + SiteDataExtractFileName + "\n" + 
                         "Error: " + siteDataExt->LoadResult,
                         "Load error",
                         (MessageBoxButtons)0,
                         MessageBoxIcon::Error);
                     // Select "any panto pos" simulation if load failed
                     //rbAcceptAnyPantoPos->Checked = true;
                 }
                 else
                 {
                     lExtractedSiteDataFileName->Text = siteDataExt->extractedFileName;
                     lSAName->Text = siteDataExt->siteDataNameSA; 
                     lSAproductID->Text = siteDataExt->siteDataProductIdSA;
                     lSAVersion->Text = siteDataExt->siteDataVersionSA;
                     lGAName->Text = siteDataExt->siteDataNameGA; 
                     lGAproductID->Text = siteDataExt->siteDataProductIdGA;
                     lGAVersion->Text = siteDataExt->siteDataVersionGA;

                     // Add track
                     lvTrackData->Items->Clear();
                     for (int i = 0; i < siteDataExt->TrackCount; i++)
                     {
                         ListViewItem^ item = lvTrackData->Items->Insert(i,  (i+1).ToString());
                         item->SubItems->Add(siteDataExt->TrackList[i]->Name);
                         item->SubItems->Add(siteDataExt->TrackList[i]->Length.ToString());
                     }

                     // Add Balise
                     lvBaliseData->Items->Clear();
                     for (int i = 0; i < siteDataExt->BaliseCount; i++)
                     {
                         ListViewItem^ item = lvBaliseData->Items->Insert(i,  (i+1).ToString());
                         item->SubItems->Add(siteDataExt->BaliseList[i]->Name);
                         item->SubItems->Add(siteDataExt->BaliseList[i]->identity);
                         item->SubItems->Add(siteDataExt->BaliseList[i]->TrackName);
                         item->SubItems->Add(siteDataExt->BaliseList[i]->position.ToString());
                     }


                     // Add Panto Shift
                     lvPantoShift->Items->Clear();
                     for (int i = 0; i < siteDataExt->PantoShiftCount; i++)
                     {
                         ListViewItem^ item = lvPantoShift->Items->Insert(i,  (i+1).ToString());
                         item->SubItems->Add(siteDataExt->PantoShiftList[i]->PantoPosNom);
                         item->SubItems->Add(siteDataExt->PantoShiftList[i]->PantoPosRev);
                         item->SubItems->Add(siteDataExt->PantoShiftList[i]->TrackName);
                         item->SubItems->Add(siteDataExt->PantoShiftList[i]->position.ToString());
                         item->SubItems->Add(siteDataExt->PantoShiftList[i]->ITrackCond.ToString());
                         item->SubItems->Add(siteDataExt->PantoShiftList[i]->direction);
                         item->SubItems->Add(siteDataExt->PantoShiftList[i]->PantoName);
                     }

                     // Add Panto Zone
                     lvPantoZone->Items->Clear();
                     for (int i = 0; i < siteDataExt->PantoZoneCount; i++)
                     {
                         ListViewItem^ item = lvPantoZone->Items->Insert(i,  (i+1).ToString());
                         item->SubItems->Add(siteDataExt->PantoZoneList[i]->TrackName);
                         item->SubItems->Add(siteDataExt->PantoZoneList[i]->start);
                         item->SubItems->Add(siteDataExt->PantoZoneList[i]->end);
                         item->SubItems->Add(siteDataExt->PantoZoneList[i]->PantoPos);
                     }
                 
                 try
                 {
                     // Create "old time" strings
                     char *tmpIniFile    = (char *) Marshal::StringToHGlobalAnsi(iniFileName).ToPointer();
                     char *tmpSiteDataFile  = (char *) Marshal::StringToHGlobalAnsi(SiteDataExtractFileName).ToPointer();
                     char *tmpOutPutFile    = (char *) Marshal::StringToHGlobalAnsi(outputFileName).ToPointer();

                     //// Write to ini file
                     WritePrivateProfileStringA("SiteDataExt", "SiteDataFile", tmpSiteDataFile, tmpIniFile);
                     WritePrivateProfileStringA("SiteDataExt", "OutputFile", tmpOutPutFile, tmpIniFile);

                     //// Free temporary buffer again
                     Marshal::FreeHGlobal(IntPtr(tmpIniFile));
                     Marshal::FreeHGlobal(IntPtr(tmpSiteDataFile));
                     Marshal::FreeHGlobal(IntPtr(tmpOutPutFile));
                 }
                 catch (...)
                 {
                     //System::Windows::Forms::MessageBox::Show("Save failed.\n" + 
                     //    "File: " + iniFileName + "\n",
                     //    "Save error",
                     //    (MessageBoxButtons)0,
                     //    MessageBoxIcon::Error);
                     //return;
                 }
                 
                 }
             }
    private: System::Void bOutpuFile_Click(System::Object^  sender, System::EventArgs^  e) {
                 SaveFileDialog^ dialog = gcnew SaveFileDialog;

                 dialog->InitialDirectory = ".\\";
                 dialog->Filter = "All files (*.*)|*.*";
                 dialog->FilterIndex = 1;
                 dialog->RestoreDirectory = true;
                 dialog->Title = "Save output file as";
                 dialog->AddExtension = true;
                 dialog->DefaultExt = "txt";

                 if ( dialog->ShowDialog() == System::Windows::Forms::DialogResult::OK )
                 {
                     // Update textbox with selected file name
                     tbOutputFile->Text = dialog->FileName;
                 }
             }
};
}
