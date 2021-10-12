#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          ATxConsoleForm.h %
*
*  %version:       2 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2016-10-18 15:09 %
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
* 2014-03-13    Antbäck     File created
* 2014-03-25    Antbäck     Show last line when set visible
* 2014-03-25    Antbäck     Show last line when command sent to process
* 2013-04-03    Antbäck     Added DoubleBuffered = true
*
*******************************************************************************/

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Windows::Forms;
using namespace System::Data;
using namespace System::Drawing;
using namespace Microsoft::Win32;
using namespace System::IO;
using namespace System::Diagnostics;
using namespace System::Text;

#define MAX_LINES_DISPLAYED 3000

namespace AOSPC {

    /// <summary>
    /// Summary for ATxConsoleForm
    ///
    /// WARNING: If you change the name of this class, you will need to change the
    ///          'Resource File Name' property for the managed resource compiler tool
    ///          associated with all .resx files this class depends on.  Otherwise,
    ///          the designers will not be able to interact properly with localized
    ///          resources associated with this form.
    /// </summary>
    public ref class ATxConsoleForm : public System::Windows::Forms::Form
    {
    public:
        // Commands sent from form to process
        String^                 cmdString;
        bool                    cmdValid;

        // States from Process
        bool                    procRunning;

    private: 
        String^                 unitName;
        String^                 iniFileName;
        String^                 regRootKey;
        int                     formLeft, formTop, formWidth, formHeight;

        StreamWriter^           procInStream;

        // Required to be static to be used in event handling!
        // Thus no DLL here since then everything will be mixed anyway :(
        static int              recStringCnt;
        static array<String^>^  recStrings;
        static Process^         myProcess;

             /**********************************************************
             * Function:     Constructor
             * Description:  
             **********************************************************/
    public:
        ATxConsoleForm(String^ uName, String^ regKey, String^ fileName)
        {
            InitializeComponent();
            this->DoubleBuffered = true;
            unitName        = uName;
            iniFileName     = fileName;
            regRootKey      = regKey;
            procRunning     = false;

            // Get window position from registry
            int left   = Convert::ToInt16(Registry::GetValue(regRootKey + "\\" + unitName, "Left", "32767"));
            int top    = Convert::ToInt16(Registry::GetValue(regRootKey + "\\" + unitName, "Top", "32767"));
            int width  = Convert::ToInt16(Registry::GetValue(regRootKey + "\\" + unitName, "Width", "100"));
            int height = Convert::ToInt16(Registry::GetValue(regRootKey + "\\" + unitName, "Height", "100"));
            // If registry not available, first run for example, use default position from windows
            if ((left != 32767) &&
                (top != 32767))
            {
                formLeft    = left;
                formTop     = top;
            }
            formWidth = width;
            formHeight = height;

            this->Text = "Cmd - " + unitName;
            
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
            Registry::SetValue(regRootKey + "\\" + unitName, "Left", String::Format("{0:0}", this->Left));
            Registry::SetValue(regRootKey + "\\" + unitName, "Top", String::Format("{0:0}", this->Top));
            Registry::SetValue(regRootKey + "\\" + unitName, "Width", String::Format("{0:0}", this->Width));
            Registry::SetValue(regRootKey + "\\" + unitName, "Height", String::Format("{0:0}", this->Height));
            Registry::SetValue(regRootKey + "\\" + unitName, "Visible", this->Visible ? "1" : "0");
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
                this->Left      = formLeft;
                this->Top       = formTop;
                this->Width     = formWidth;
                this->Height    = formHeight;
                this->Visible   = true;
                // Select last line as visible
                if (lvOutput->Items->Count > 0)
                {
                    lvOutput->EnsureVisible(lvOutput->Items->Count - 1);
                }
            }
            else
            {
                formLeft        = this->Left;
                formTop         = this->Top;
                formWidth       = this->Width;
                formHeight      = this->Height;
                this->Visible   = false;
            }
        }

        /**********************************************************
        * Function:     Tick
        * Description:  
        **********************************************************/
    public:
        void Tick(array<String^>^ strings, int count)
        {
            int i;

            // Update GUI
            if (count > 0)
            {
                for (i = 0; i < count; i++)
                {
                    if (strings[i]->IndexOf("[RED]") == 0)
                    {
                        ListViewItem^ item = lvOutput->Items->Insert(lvOutput->Items->Count, strings[i]->Substring(5));
                        item->ForeColor = Color::Red;
                    }
                    else
                    {
                        lvOutput->Items->Insert(lvOutput->Items->Count, strings[i]);
                    }
                }

                // Limit no of displayed lines
                int LastIndex = lvOutput->Items->Count - 1;
                while (LastIndex >= MAX_LINES_DISPLAYED)
                {
                    lvOutput->Items->RemoveAt(0);
                    LastIndex = lvOutput->Items->Count - 1;
                }

                // Select last line as visible
                lvOutput->EnsureVisible(LastIndex);
            }

            // Enable/disable controls
            if (procRunning)
            {
                tsbGreen->Visible       = true;
                tsbRed->Visible         = false;
                tstbCommand->Enabled    = true;
                tsbSend->Enabled        = true;
            }
            else
            {
                tsbGreen->Visible       = false;
                tsbRed->Visible         = true;
                tstbCommand->Enabled    = false;
                tsbSend->Enabled        = false;
            }
        }
        /**********************************************************
        * Function:     LoadSetup
        * Description:  
        **********************************************************/
    public: void LoadSetup(String^ setup) {
                 String^ tmpRootKey = regRootKey + "\\" + unitName;
                 if (!System::String::IsNullOrEmpty(setup)) 
                 {
                     tmpRootKey = regRootKey + "\\" + setup + "\\" + unitName;
                 }
                 // Check if key exists
                 String^ tmp = Convert::ToString(Registry::GetValue(tmpRootKey, "", "empty"));

                 // Key exists, i.e. existing setup
                 if (!System::String::IsNullOrEmpty(tmp)) 
                 {
                     // Get window position from registry
                     int    left   = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Left", "32767"));
                     int    top    = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Top", "32767"));
                     int    width  = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Width", "100"));
                     int    height = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Height", "100"));
                     bool   visible = Convert::ToInt16(Registry::GetValue(tmpRootKey, "Visible", "1")) == 1 ? true : false;
                     // If registry not available, first run for example, use default position from windows
                     if (left != 32767)     formLeft    = left;
                     if (top != 32767)      formTop     = top;
                     if (width != 32767)    formWidth   = width;
                     if (height != 32767)   formHeight  = height;
                     SetVisibility(visible);
                 }
             }
        /**********************************************************
        * Function:     SaveSetup
        * Description:  
        **********************************************************/
    public: void SaveSetup(String^ setup) {
                 String^ tmpRootKey = regRootKey + "\\" + unitName;
                 if (!System::String::IsNullOrEmpty(setup)) 
                 {
                     tmpRootKey = regRootKey + "\\" + setup + "\\" + unitName;
                 }

                 // Store windows position in registry
                 Registry::SetValue(tmpRootKey, "Left", String::Format("{0:0}", this->Left));
                 Registry::SetValue(tmpRootKey, "Top", String::Format("{0:0}", this->Top));
                 Registry::SetValue(tmpRootKey, "Width", String::Format("{0:0}", this->Width));
                 Registry::SetValue(tmpRootKey, "Height", String::Format("{0:0}", this->Height));
                 Registry::SetValue(tmpRootKey, "Visible", this->Visible ? "1" : "0");
             }
        /**********************************************************
        * Function:     SetSize
        * Description:  
        **********************************************************/
    public: void SetSize(int left, int top, int width, int height) {
                this->Left  = left;
                this->Top   = top;
                this->Width = width;
                this->Height = height;
                formLeft    = left;
                formTop     = top;
                formWidth   = width;
                formHeight  = height;
             }

    protected:
        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        ~ATxConsoleForm()
        {
            if (components)
            {
                delete components;
            }
        }


    private: System::Windows::Forms::ListView^  lvOutput;
    private: System::Windows::Forms::ToolStrip^  tsStatusRow;
    private: System::Windows::Forms::ToolStripLabel^  tslCmdLabel;
    private: System::Windows::Forms::ToolStripTextBox^  tstbCommand;
    private: System::Windows::Forms::Panel^  pStatusRowSep;
    private: System::Windows::Forms::ColumnHeader^  chText;
    private: System::Windows::Forms::ToolStripButton^  tsbSend;
    private: System::Windows::Forms::ToolStripButton^  tsbRed;
    private: System::Windows::Forms::ToolStripButton^  tsbGreen;
    private: System::Windows::Forms::ToolStripSeparator^  toolStripSeparator1;
    private: System::Windows::Forms::ToolStripButton^  tsbCopyAllToClipboard;
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
            System::ComponentModel::ComponentResourceManager^  resources = (gcnew System::ComponentModel::ComponentResourceManager(ATxConsoleForm::typeid));
            this->lvOutput = (gcnew System::Windows::Forms::ListView());
            this->chText = (gcnew System::Windows::Forms::ColumnHeader());
            this->tsStatusRow = (gcnew System::Windows::Forms::ToolStrip());
            this->tslCmdLabel = (gcnew System::Windows::Forms::ToolStripLabel());
            this->tstbCommand = (gcnew System::Windows::Forms::ToolStripTextBox());
            this->tsbSend = (gcnew System::Windows::Forms::ToolStripButton());
            this->tsbRed = (gcnew System::Windows::Forms::ToolStripButton());
            this->tsbGreen = (gcnew System::Windows::Forms::ToolStripButton());
            this->toolStripSeparator1 = (gcnew System::Windows::Forms::ToolStripSeparator());
            this->tsbCopyAllToClipboard = (gcnew System::Windows::Forms::ToolStripButton());
            this->pStatusRowSep = (gcnew System::Windows::Forms::Panel());
            this->tsStatusRow->SuspendLayout();
            this->SuspendLayout();
            // 
            // lvOutput
            // 
            this->lvOutput->BackColor = System::Drawing::SystemColors::Window;
            this->lvOutput->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(1) { this->chText });
            this->lvOutput->Font = (gcnew System::Drawing::Font(L"Courier New", 8.25F, System::Drawing::FontStyle::Regular, System::Drawing::GraphicsUnit::Point,
                static_cast<System::Byte>(0)));
            this->lvOutput->ForeColor = System::Drawing::SystemColors::WindowText;
            this->lvOutput->FullRowSelect = true;
            this->lvOutput->HeaderStyle = System::Windows::Forms::ColumnHeaderStyle::Nonclickable;
            this->lvOutput->Location = System::Drawing::Point(9, 12);
            this->lvOutput->Name = L"lvOutput";
            this->lvOutput->Size = System::Drawing::Size(307, 216);
            this->lvOutput->TabIndex = 0;
            this->lvOutput->UseCompatibleStateImageBehavior = false;
            this->lvOutput->View = System::Windows::Forms::View::Details;
            this->lvOutput->Enter += gcnew System::EventHandler(this, &ATxConsoleForm::lvOutput_Enter);
            // 
            // chText
            // 
            this->chText->Text = L"Text";
            this->chText->Width = 194;
            // 
            // tsStatusRow
            // 
            this->tsStatusRow->Dock = System::Windows::Forms::DockStyle::Bottom;
            this->tsStatusRow->GripStyle = System::Windows::Forms::ToolStripGripStyle::Hidden;
            this->tsStatusRow->Items->AddRange(gcnew cli::array< System::Windows::Forms::ToolStripItem^  >(7) {
                this->tslCmdLabel, this->tstbCommand,
                    this->tsbSend, this->tsbRed, this->tsbGreen, this->toolStripSeparator1, this->tsbCopyAllToClipboard
            });
            this->tsStatusRow->Location = System::Drawing::Point(0, 241);
            this->tsStatusRow->Name = L"tsStatusRow";
            this->tsStatusRow->RenderMode = System::Windows::Forms::ToolStripRenderMode::System;
            this->tsStatusRow->Size = System::Drawing::Size(325, 25);
            this->tsStatusRow->TabIndex = 1;
            this->tsStatusRow->Text = L"toolStrip1";
            // 
            // tslCmdLabel
            // 
            this->tslCmdLabel->Name = L"tslCmdLabel";
            this->tslCmdLabel->Size = System::Drawing::Size(33, 22);
            this->tslCmdLabel->Text = L"Cmd";
            // 
            // tstbCommand
            // 
            this->tstbCommand->AutoSize = false;
            this->tstbCommand->Name = L"tstbCommand";
            this->tstbCommand->Size = System::Drawing::Size(100, 25);
            this->tstbCommand->KeyPress += gcnew System::Windows::Forms::KeyPressEventHandler(this, &ATxConsoleForm::tstbCommand_KeyPress);
            // 
            // tsbSend
            // 
            this->tsbSend->DisplayStyle = System::Windows::Forms::ToolStripItemDisplayStyle::Text;
            this->tsbSend->ImageTransparentColor = System::Drawing::Color::Magenta;
            this->tsbSend->Name = L"tsbSend";
            this->tsbSend->Size = System::Drawing::Size(37, 22);
            this->tsbSend->Text = L"Send";
            this->tsbSend->Click += gcnew System::EventHandler(this, &ATxConsoleForm::tsbSend_Click);
            // 
            // tsbRed
            // 
            this->tsbRed->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
            this->tsbRed->AutoToolTip = false;
            this->tsbRed->DisplayStyle = System::Windows::Forms::ToolStripItemDisplayStyle::Image;
            this->tsbRed->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbRed.Image")));
            this->tsbRed->ImageTransparentColor = System::Drawing::Color::Magenta;
            this->tsbRed->Name = L"tsbRed";
            this->tsbRed->Size = System::Drawing::Size(23, 22);
            this->tsbRed->Text = L"Stop";
            // 
            // tsbGreen
            // 
            this->tsbGreen->Alignment = System::Windows::Forms::ToolStripItemAlignment::Right;
            this->tsbGreen->AutoToolTip = false;
            this->tsbGreen->DisplayStyle = System::Windows::Forms::ToolStripItemDisplayStyle::Image;
            this->tsbGreen->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbGreen.Image")));
            this->tsbGreen->ImageTransparentColor = System::Drawing::Color::Magenta;
            this->tsbGreen->Name = L"tsbGreen";
            this->tsbGreen->Size = System::Drawing::Size(23, 22);
            this->tsbGreen->Text = L"Run";
            this->tsbGreen->Visible = false;
            // 
            // toolStripSeparator1
            // 
            this->toolStripSeparator1->Name = L"toolStripSeparator1";
            this->toolStripSeparator1->Size = System::Drawing::Size(6, 25);
            // 
            // tsbCopyAllToClipboard
            // 
            this->tsbCopyAllToClipboard->DisplayStyle = System::Windows::Forms::ToolStripItemDisplayStyle::Image;
            this->tsbCopyAllToClipboard->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"tsbCopyAllToClipboard.Image")));
            this->tsbCopyAllToClipboard->ImageTransparentColor = System::Drawing::Color::Magenta;
            this->tsbCopyAllToClipboard->Name = L"tsbCopyAllToClipboard";
            this->tsbCopyAllToClipboard->Size = System::Drawing::Size(23, 22);
            this->tsbCopyAllToClipboard->Text = L"toolStripButton1";
            this->tsbCopyAllToClipboard->ToolTipText = L"Copy all to clipboard";
            this->tsbCopyAllToClipboard->Click += gcnew System::EventHandler(this, &ATxConsoleForm::tsbCopyAllToClipboard_Click);
            // 
            // pStatusRowSep
            // 
            this->pStatusRowSep->BorderStyle = System::Windows::Forms::BorderStyle::Fixed3D;
            this->pStatusRowSep->Location = System::Drawing::Point(0, 234);
            this->pStatusRowSep->Name = L"pStatusRowSep";
            this->pStatusRowSep->Size = System::Drawing::Size(328, 4);
            this->pStatusRowSep->TabIndex = 6;
            // 
            // ATxConsoleForm
            // 
            this->AutoScaleDimensions = System::Drawing::SizeF(6, 13);
            this->AutoScaleMode = System::Windows::Forms::AutoScaleMode::Font;
            this->ClientSize = System::Drawing::Size(325, 266);
            this->ControlBox = false;
            this->Controls->Add(this->pStatusRowSep);
            this->Controls->Add(this->tsStatusRow);
            this->Controls->Add(this->lvOutput);
            this->FormBorderStyle = System::Windows::Forms::FormBorderStyle::SizableToolWindow;
            this->MaximizeBox = false;
            this->MinimizeBox = false;
            this->MinimumSize = System::Drawing::Size(333, 200);
            this->Name = L"ATxConsoleForm";
            this->StartPosition = System::Windows::Forms::FormStartPosition::Manual;
            this->Text = L"ATxConsoleForm";
            this->Shown += gcnew System::EventHandler(this, &ATxConsoleForm::ATxConsoleForm_Shown);
            this->Resize += gcnew System::EventHandler(this, &ATxConsoleForm::ATxConsoleForm_Resize);
            this->tsStatusRow->ResumeLayout(false);
            this->tsStatusRow->PerformLayout();
            this->ResumeLayout(false);
            this->PerformLayout();

        }
#pragma endregion
    private: System::Void ATxConsoleForm_Shown(System::Object^  sender, System::EventArgs^  e) {
                 this->Left = formLeft;
                 this->Top  = formTop;
                 this->Width = formWidth;
                 this->Height  = formHeight;
                 this->Visible = Convert::ToInt16(Registry::GetValue(regRootKey + "\\ATP1", "Visible", "1")) != 0 ? true : false;
                 ATxConsoleForm_Resize(sender, e);
             }
    private: System::Void ATxConsoleForm_Resize(System::Object^  sender, System::EventArgs^  e) {
                 int borderWidth    = 5;
                 int borderMargin   = 6;

                 lvOutput->Left     = borderMargin;
                 lvOutput->Width    = this->Width - 2*borderWidth - 2*borderMargin;
                 lvOutput->Top      = borderMargin;
                 lvOutput->Height   = tsStatusRow->Top - 3*borderMargin;

                 pStatusRowSep->Top     = tsStatusRow->Top - pStatusRowSep->Height;
                 pStatusRowSep->Left    = 0;
                 pStatusRowSep->Width   = this->Width;

                 chText->Width = lvOutput->Width - lvOutput->Margin.Left - lvOutput->Margin.Right - 15;            
                 tstbCommand->Width = min(tsStatusRow->Width - tslCmdLabel->Width - tsbSend->Width - tsbGreen->Width - 100, 300);
             }
    private: System::Void tsbSend_Click(System::Object^  sender, System::EventArgs^  e) {

                 if (!String::IsNullOrEmpty(tstbCommand->Text))
                 {
                     // Set output string to send
                     cmdString  = tstbCommand->Text;
                     cmdValid   = true;
                     lvOutput->Items->Insert(lvOutput->Items->Count, DateTime::Now.ToString("HH:mm:ss.fff") + "Cmd> " + tstbCommand->Text);
                     // Select last line as visible
                     lvOutput->EnsureVisible(lvOutput->Items->Count - 1);
                 }
                 tstbCommand->SelectAll();
                 tstbCommand->Focus();
             }
    private: System::Void tstbCommand_KeyPress(System::Object^  sender, System::Windows::Forms::KeyPressEventArgs^  e) {
                 // Capture enter
                 if (e->KeyChar == 13)
                 {
                     e->Handled = true;
                     tsbSend_Click(sender, nullptr);
                     tstbCommand->Focus();
                 }
             }
    private: System::Void lvOutput_Enter(System::Object^  sender, System::EventArgs^  e) {
                 tstbCommand->Focus();
             }
    private: System::Void tsbCopyAllToClipboard_Click(System::Object^  sender, System::EventArgs^  e) {
                 int i;
                 String^ tmpStr = "";

                 // Copy lvOutput items as text to clipboard
                 for (i = 0; i < lvOutput->Items->Count; i++)
                 {
                     tmpStr += lvOutput->Items[i]->Text + "\r\n";
                 }
                 if(!System::String::IsNullOrEmpty(tmpStr)) 
                 {
                     try 
                     {
                         System::Windows::Forms::Clipboard::SetText(tmpStr);
                     }
                     catch( ... ) 
                     { 
                     }
                 }
             }
    };
}
