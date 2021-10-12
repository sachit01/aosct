#pragma once

#include "tcpip.h"
#include "RU.h"
#include "iostream"
#include <list>


namespace RU {

  using namespace System;
  using namespace System::ComponentModel;
  using namespace System::Collections;
  using namespace System::Collections::Generic;
  using namespace System::Windows::Forms;
  using namespace System::Data;
  using namespace System::Drawing;

  struct ItemToLog
  {
    ClientConnection_t* ccb;
    const ConfigRU_t* config;
    std::string line;
  };

  typedef std::list<ItemToLog> ListOfItemsToLog;

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
  public:
    Form1(void)
    {
      InitializeComponent();

      g_njru = new RU_t;
      g_ru = new RU_t;
      g_commonConfig = new ConfigCommon_t;
      g_listOfItemsToLog = new ListOfItemsToLog();

      g_commonConfig->ShowSource = false;
      g_commonConfig->ShowTimestamp = false;

      // Initialize N-JRU
      Form1::InitRU(g_njru, e_NJRU, "N-JRU");

      // Initialize RU
      Form1::InitRU(g_ru, e_RU, "RU");

      g_visibleListView = e_DEFAULT;
      g_showInLogTip = (gcnew ToolTip());
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
      delete(g_njru);
      delete(g_ru);
      delete(g_commonConfig);
    }
  private: System::Windows::Forms::Timer^  timerNJRUIP;
  protected:


  protected:


  private:

  internal:


  private: System::Windows::Forms::ContextMenuStrip^  contextMenuStrip;
  private: System::Windows::Forms::TabControl^  tabControl;
  private: System::Windows::Forms::TabPage^  tabPageLog;
  private: System::Windows::Forms::ListView^  listViewLog;
  private: System::Windows::Forms::ColumnHeader^  columnHeaderTimestamp;
  private: System::Windows::Forms::ColumnHeader^  columnHeaderSource;
  private: System::Windows::Forms::ColumnHeader^  columnHeaderLine;
  private: System::Windows::Forms::TabPage^  tabPageClientsNJRU;
  private: System::Windows::Forms::ListView^  listViewClientsNJRU;
  private: System::Windows::Forms::ColumnHeader^  columnHeaderNJRUIndex;
  private: System::Windows::Forms::ColumnHeader^  columnHeaderNJRUIPAddress;
  private: System::Windows::Forms::ColumnHeader^  columnHeaderNJRUCreated;
  private: System::Windows::Forms::ColumnHeader^  columnHeaderNJRULastLog;
  private: System::Windows::Forms::ColumnHeader^  columnHeaderNJRULines;





  private: System::Windows::Forms::ToolStrip^  toolStrip1;
  private: System::Windows::Forms::ToolStripButton^  toolStripButtonClear;
  private: System::Windows::Forms::ToolStripSeparator^  toolStripSeparator1;
  private: System::Windows::Forms::ToolStripButton^  toolStripButtonExit;
  private: System::Windows::Forms::Timer^  timerPurge;
  private: System::Windows::Forms::ToolStripMenuItem^  toolStripMenuItemCopyToClipboard;
  private: System::Windows::Forms::ToolStripSeparator^  toolStripSeparator3;
  private: System::Windows::Forms::ToolStripMenuItem^  toolStripMenuItemSelectAll;
  private: System::Windows::Forms::ToolStripSeparator^  toolStripSeparator2;
  private: System::Windows::Forms::ToolStripButton^  toolStripButtonCopyToClipboard;
  private: System::Windows::Forms::ToolStripButton^  toolStripButtonSelectAll;
  private: System::Windows::Forms::TabPage^  tabPage1;
  private: System::Windows::Forms::TabPage^  tabPageClientsRU;
  private: System::Windows::Forms::ListView^  listViewClientsRU;


  private: System::Windows::Forms::ContextMenuStrip^  contextMenuStrip1;

  private: System::Windows::Forms::ColumnHeader^  columnHeaderRUIndex;
  private: System::Windows::Forms::ColumnHeader^  columnHeaderRUIPAddress;
  private: System::Windows::Forms::ColumnHeader^  columnHeaderRUCreated;
  private: System::Windows::Forms::ColumnHeader^  columnHeaderRULastLog;
  private: System::Windows::Forms::ColumnHeader^  columnHeaderRULines;
private: System::Windows::Forms::Timer^  timerRUIP;



  private: System::Windows::Forms::ToolStripMenuItem^  toolStripMenuItemClear;

  private:

    WCHAR* ConvertFromManagedString(String ^ManagedStr);
    char* ConvertFromManagedStringToAnsi(String ^ManagedStr);

    void LoadDefaultSettings(void);
    void LoadSettings(ConfigRU_t *config);
    BOOL WritePrivateProfileIntA(LPCSTR lpAppName, LPCSTR lpKeyName,
      int Value, LPCSTR lpFileName);
    void LoadDesktop(void);
    void SaveDesktop(void);

    std::string ChangeFileExt(const char *filepath, const char *fileext);
    void InitRU(RU_t *ru, enum e_ru, char *prefix);
    void InitIP(ClientConnection_t * ccb);
    void PollNewConnections(RU_t * ru);
    void PollIP(RU_t *ru);
    void ListenIP(RU_t *ru);
    void CloseSockets(SOCKET socket);
    void DisplaySocket(ListView ^listview, ClientConnection_t *ccb, int Index);
    void DisplaySockets(ListView ^view, RU_t *R);
    void WriteToDiskFile(const ConfigRU_t *config, const char *logFileName, const char *timestamp, const char * source, const char *text);
    void LogSavedItems();
    void LogItem(ClientConnection_t* ccb, const ConfigRU_t* config, const char* line);
    void LogItem(const ItemToLog& itemToLog);
    void LogRUItem(const e_ru type, const char* line);
    bool FileExists(const char *PathAndFileName);
    void WriteInfo(const char *info, const char *timestamp, const char *logFilePathAndName);
    int FindFstEmptyCCBSlot(RU_t *ru);
    std::string getLogFileTime(const e_ru type);
    std::string getLogFileName(const ConfigRU_t* config);

    bool AddSocket(ClientConnection_t *ccb, SOCKET socketId, const char *IPAddr, enum e_ru ru_type);
    void ClearView(ListView ^view);
    void PurgeLogFiles(RU_t *ru, char* extension);
    void CompressLogFiles(RU_t* ru);
    bool WindowsSystemCall(const char * cmd);

    RU_t *g_njru;
    RU_t *g_ru;
    ConfigCommon_t *g_commonConfig;
    enum e_ru g_visibleListView;
    ToolTip^ g_showInLogTip;
    HANDLE g_listViewLogMutex;
    ListOfItemsToLog* g_listOfItemsToLog;


  private: System::ComponentModel::IContainer^  components;
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
         this->contextMenuStrip = (gcnew System::Windows::Forms::ContextMenuStrip(this->components));
         this->toolStripMenuItemClear = (gcnew System::Windows::Forms::ToolStripMenuItem());
         this->toolStripSeparator3 = (gcnew System::Windows::Forms::ToolStripSeparator());
         this->toolStripMenuItemSelectAll = (gcnew System::Windows::Forms::ToolStripMenuItem());
         this->toolStripSeparator2 = (gcnew System::Windows::Forms::ToolStripSeparator());
         this->toolStripMenuItemCopyToClipboard = (gcnew System::Windows::Forms::ToolStripMenuItem());
         this->timerNJRUIP = (gcnew System::Windows::Forms::Timer(this->components));
         this->tabControl = (gcnew System::Windows::Forms::TabControl());
         this->tabPageLog = (gcnew System::Windows::Forms::TabPage());
         this->listViewLog = (gcnew System::Windows::Forms::ListView());
         this->columnHeaderTimestamp = (gcnew System::Windows::Forms::ColumnHeader());
         this->columnHeaderSource = (gcnew System::Windows::Forms::ColumnHeader());
         this->columnHeaderLine = (gcnew System::Windows::Forms::ColumnHeader());
         this->tabPageClientsNJRU = (gcnew System::Windows::Forms::TabPage());
         this->listViewClientsNJRU = (gcnew System::Windows::Forms::ListView());
         this->columnHeaderNJRUIndex = (gcnew System::Windows::Forms::ColumnHeader());
         this->columnHeaderNJRUIPAddress = (gcnew System::Windows::Forms::ColumnHeader());
         this->columnHeaderNJRUCreated = (gcnew System::Windows::Forms::ColumnHeader());
         this->columnHeaderNJRULastLog = (gcnew System::Windows::Forms::ColumnHeader());
         this->columnHeaderNJRULines = (gcnew System::Windows::Forms::ColumnHeader());
         this->tabPageClientsRU = (gcnew System::Windows::Forms::TabPage());
         this->listViewClientsRU = (gcnew System::Windows::Forms::ListView());
         this->columnHeaderRUIndex = (gcnew System::Windows::Forms::ColumnHeader());
         this->columnHeaderRUIPAddress = (gcnew System::Windows::Forms::ColumnHeader());
         this->columnHeaderRUCreated = (gcnew System::Windows::Forms::ColumnHeader());
         this->columnHeaderRULastLog = (gcnew System::Windows::Forms::ColumnHeader());
         this->columnHeaderRULines = (gcnew System::Windows::Forms::ColumnHeader());
         this->toolStrip1 = (gcnew System::Windows::Forms::ToolStrip());
         this->toolStripButtonClear = (gcnew System::Windows::Forms::ToolStripButton());
         this->toolStripButtonSelectAll = (gcnew System::Windows::Forms::ToolStripButton());
         this->toolStripButtonCopyToClipboard = (gcnew System::Windows::Forms::ToolStripButton());
         this->toolStripSeparator1 = (gcnew System::Windows::Forms::ToolStripSeparator());
         this->toolStripButtonExit = (gcnew System::Windows::Forms::ToolStripButton());
         this->timerPurge = (gcnew System::Windows::Forms::Timer(this->components));
         this->tabPage1 = (gcnew System::Windows::Forms::TabPage());
         this->contextMenuStrip1 = (gcnew System::Windows::Forms::ContextMenuStrip(this->components));
         this->timerRUIP = (gcnew System::Windows::Forms::Timer(this->components));
         this->contextMenuStrip->SuspendLayout();
         this->tabControl->SuspendLayout();
         this->tabPageLog->SuspendLayout();
         this->tabPageClientsNJRU->SuspendLayout();
         this->tabPageClientsRU->SuspendLayout();
         this->toolStrip1->SuspendLayout();
         this->SuspendLayout();
         // 
         // contextMenuStrip
         // 
         this->contextMenuStrip->Items->AddRange(gcnew cli::array< System::Windows::Forms::ToolStripItem^  >(5) {
           this->toolStripMenuItemClear,
             this->toolStripSeparator3, this->toolStripMenuItemSelectAll, this->toolStripSeparator2, this->toolStripMenuItemCopyToClipboard
         });
         this->contextMenuStrip->Name = L"contextMenuStrip";
         this->contextMenuStrip->Size = System::Drawing::Size(212, 82);
         this->contextMenuStrip->Opening += gcnew System::ComponentModel::CancelEventHandler(this, &Form1::contextMenuStrip_Opening);
         // 
         // toolStripMenuItemClear
         // 
         this->toolStripMenuItemClear->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"toolStripMenuItemClear.Image")));
         this->toolStripMenuItemClear->Name = L"toolStripMenuItemClear";
         this->toolStripMenuItemClear->Size = System::Drawing::Size(211, 22);
         this->toolStripMenuItemClear->Text = L"Clear displayed log";
         this->toolStripMenuItemClear->Click += gcnew System::EventHandler(this, &Form1::toolStripMenuItemClear_Click);
         // 
         // toolStripSeparator3
         // 
         this->toolStripSeparator3->Name = L"toolStripSeparator3";
         this->toolStripSeparator3->Size = System::Drawing::Size(208, 6);
         // 
         // toolStripMenuItemSelectAll
         // 
         this->toolStripMenuItemSelectAll->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"toolStripMenuItemSelectAll.Image")));
         this->toolStripMenuItemSelectAll->Name = L"toolStripMenuItemSelectAll";
         this->toolStripMenuItemSelectAll->ShortcutKeys = static_cast<System::Windows::Forms::Keys>((System::Windows::Forms::Keys::Control | System::Windows::Forms::Keys::A));
         this->toolStripMenuItemSelectAll->Size = System::Drawing::Size(211, 22);
         this->toolStripMenuItemSelectAll->Text = L"Select all";
         this->toolStripMenuItemSelectAll->Click += gcnew System::EventHandler(this, &Form1::toolStripMenuItemSelectAll_Click);
         // 
         // toolStripSeparator2
         // 
         this->toolStripSeparator2->Name = L"toolStripSeparator2";
         this->toolStripSeparator2->Size = System::Drawing::Size(208, 6);
         // 
         // toolStripMenuItemCopyToClipboard
         // 
         this->toolStripMenuItemCopyToClipboard->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"toolStripMenuItemCopyToClipboard.Image")));
         this->toolStripMenuItemCopyToClipboard->Name = L"toolStripMenuItemCopyToClipboard";
         this->toolStripMenuItemCopyToClipboard->ShortcutKeys = static_cast<System::Windows::Forms::Keys>((System::Windows::Forms::Keys::Control | System::Windows::Forms::Keys::C));
         this->toolStripMenuItemCopyToClipboard->Size = System::Drawing::Size(211, 22);
         this->toolStripMenuItemCopyToClipboard->Text = L"Copy to clipboard";
         this->toolStripMenuItemCopyToClipboard->Click += gcnew System::EventHandler(this, &Form1::toolStripMenuItemCopyToClipboard_Click);
         // 
         // timerNJRUIP
         // 
         this->timerNJRUIP->Interval = 50;
         this->timerNJRUIP->Tick += gcnew System::EventHandler(this, &Form1::timerNJRUIP_Tick);
         // 
         // tabControl
         // 
         this->tabControl->Controls->Add(this->tabPageLog);
         this->tabControl->Controls->Add(this->tabPageClientsNJRU);
         this->tabControl->Controls->Add(this->tabPageClientsRU);
         this->tabControl->Dock = System::Windows::Forms::DockStyle::Bottom;
         this->tabControl->Font = (gcnew System::Drawing::Font(L"Tahoma", 8.25F, System::Drawing::FontStyle::Regular, System::Drawing::GraphicsUnit::Point,
           static_cast<System::Byte>(0)));
         this->tabControl->Location = System::Drawing::Point(0, 28);
         this->tabControl->Multiline = true;
         this->tabControl->Name = L"tabControl";
         this->tabControl->SelectedIndex = 0;
         this->tabControl->Size = System::Drawing::Size(792, 338);
         this->tabControl->TabIndex = 1;
         this->tabControl->SelectedIndexChanged += gcnew System::EventHandler(this, &Form1::tabControl_SelectedIndexChanged);
         // 
         // tabPageLog
         // 
         this->tabPageLog->Controls->Add(this->listViewLog);
         this->tabPageLog->Location = System::Drawing::Point(4, 22);
         this->tabPageLog->Name = L"tabPageLog";
         this->tabPageLog->Padding = System::Windows::Forms::Padding(3);
         this->tabPageLog->Size = System::Drawing::Size(784, 312);
         this->tabPageLog->TabIndex = 0;
         this->tabPageLog->Text = L"Log";
         this->tabPageLog->UseVisualStyleBackColor = true;
         // 
         // listViewLog
         // 
         this->listViewLog->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(3) {
           this->columnHeaderTimestamp,
             this->columnHeaderSource, this->columnHeaderLine
         });
         this->listViewLog->ContextMenuStrip = this->contextMenuStrip;
         this->listViewLog->Dock = System::Windows::Forms::DockStyle::Fill;
         this->listViewLog->Font = (gcnew System::Drawing::Font(L"Courier New", 9, System::Drawing::FontStyle::Regular, System::Drawing::GraphicsUnit::Point,
           static_cast<System::Byte>(0)));
         this->listViewLog->FullRowSelect = true;
         this->listViewLog->GridLines = true;
         this->listViewLog->HideSelection = false;
         this->listViewLog->Location = System::Drawing::Point(3, 3);
         this->listViewLog->Name = L"listViewLog";
         this->listViewLog->Size = System::Drawing::Size(778, 306);
         this->listViewLog->TabIndex = 1;
         this->listViewLog->UseCompatibleStateImageBehavior = false;
         this->listViewLog->View = System::Windows::Forms::View::Details;
         this->listViewLog->ItemSelectionChanged += gcnew System::Windows::Forms::ListViewItemSelectionChangedEventHandler(this, &Form1::listViewLog_ItemSelectionChanged);
         // 
         // columnHeaderTimestamp
         // 
         this->columnHeaderTimestamp->Text = L"Timestamp";
         this->columnHeaderTimestamp->Width = 150;
         // 
         // columnHeaderSource
         // 
         this->columnHeaderSource->Text = L"Source";
         this->columnHeaderSource->Width = 112;
         // 
         // columnHeaderLine
         // 
         this->columnHeaderLine->Text = L"Line";
         this->columnHeaderLine->Width = 850;
         // 
         // tabPageClientsNJRU
         // 
         this->tabPageClientsNJRU->Controls->Add(this->listViewClientsNJRU);
         this->tabPageClientsNJRU->Location = System::Drawing::Point(4, 22);
         this->tabPageClientsNJRU->Name = L"tabPageClientsNJRU";
         this->tabPageClientsNJRU->Padding = System::Windows::Forms::Padding(3);
         this->tabPageClientsNJRU->Size = System::Drawing::Size(784, 312);
         this->tabPageClientsNJRU->TabIndex = 1;
         this->tabPageClientsNJRU->Text = L"Clients N-JRU";
         this->tabPageClientsNJRU->UseVisualStyleBackColor = true;
         // 
         // listViewClientsNJRU
         // 
         this->listViewClientsNJRU->Alignment = System::Windows::Forms::ListViewAlignment::Default;
         this->listViewClientsNJRU->CheckBoxes = true;
         this->listViewClientsNJRU->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(5) {
           this->columnHeaderNJRUIndex,
             this->columnHeaderNJRUIPAddress, this->columnHeaderNJRUCreated, this->columnHeaderNJRULastLog, this->columnHeaderNJRULines
         });
         this->listViewClientsNJRU->Dock = System::Windows::Forms::DockStyle::Fill;
         this->listViewClientsNJRU->Font = (gcnew System::Drawing::Font(L"Courier New", 9, System::Drawing::FontStyle::Regular, System::Drawing::GraphicsUnit::Point,
           static_cast<System::Byte>(0)));
         this->listViewClientsNJRU->Location = System::Drawing::Point(3, 3);
         this->listViewClientsNJRU->Name = L"listViewClientsNJRU";
         this->listViewClientsNJRU->ShowItemToolTips = true;
         this->listViewClientsNJRU->Size = System::Drawing::Size(778, 306);
         this->listViewClientsNJRU->TabIndex = 37;
         this->listViewClientsNJRU->UseCompatibleStateImageBehavior = false;
         this->listViewClientsNJRU->View = System::Windows::Forms::View::Details;
         this->listViewClientsNJRU->ItemChecked += gcnew System::Windows::Forms::ItemCheckedEventHandler(this, &Form1::logConnectionCheckBox_Checked);
         this->listViewClientsNJRU->ItemMouseHover += gcnew System::Windows::Forms::ListViewItemMouseHoverEventHandler(this, &Form1::listView_ItemMouseHover);
         this->listViewClientsNJRU->VisibleChanged += gcnew System::EventHandler(this, &Form1::listViewClientsNJRU_Visible);
         // 
         // columnHeaderNJRUIndex
         // 
         this->columnHeaderNJRUIndex->Text = L"Index";
         // 
         // columnHeaderNJRUIPAddress
         // 
         this->columnHeaderNJRUIPAddress->Text = L"IP Address";
         this->columnHeaderNJRUIPAddress->Width = 160;
         // 
         // columnHeaderNJRUCreated
         // 
         this->columnHeaderNJRUCreated->Text = L"Created";
         this->columnHeaderNJRUCreated->Width = 180;
         // 
         // columnHeaderNJRULastLog
         // 
         this->columnHeaderNJRULastLog->Text = L"Last log";
         this->columnHeaderNJRULastLog->Width = 180;
         // 
         // columnHeaderNJRULines
         // 
         this->columnHeaderNJRULines->Text = L"Lines";
         // 
         // tabPageClientsRU
         // 
         this->tabPageClientsRU->Controls->Add(this->listViewClientsRU);
         this->tabPageClientsRU->Location = System::Drawing::Point(4, 22);
         this->tabPageClientsRU->Name = L"tabPageClientsRU";
         this->tabPageClientsRU->Padding = System::Windows::Forms::Padding(3);
         this->tabPageClientsRU->Size = System::Drawing::Size(784, 312);
         this->tabPageClientsRU->TabIndex = 2;
         this->tabPageClientsRU->Text = L"Clients RU";
         this->tabPageClientsRU->UseVisualStyleBackColor = true;
         this->tabPageClientsRU->Click += gcnew System::EventHandler(this, &Form1::tabPage2_Click);
         // 
         // listViewClientsRU
         // 
         this->listViewClientsRU->Alignment = System::Windows::Forms::ListViewAlignment::Default;
         this->listViewClientsRU->CheckBoxes = true;
         this->listViewClientsRU->Columns->AddRange(gcnew cli::array< System::Windows::Forms::ColumnHeader^  >(5) {
           this->columnHeaderRUIndex,
             this->columnHeaderRUIPAddress, this->columnHeaderRUCreated, this->columnHeaderRULastLog, this->columnHeaderRULines
         });
         this->listViewClientsRU->Dock = System::Windows::Forms::DockStyle::Fill;
         this->listViewClientsRU->Font = (gcnew System::Drawing::Font(L"Courier New", 9, System::Drawing::FontStyle::Regular, System::Drawing::GraphicsUnit::Point,
           static_cast<System::Byte>(0)));
         this->listViewClientsRU->Location = System::Drawing::Point(3, 3);
         this->listViewClientsRU->Name = L"listViewClientsRU";
         this->listViewClientsRU->Size = System::Drawing::Size(778, 306);
         this->listViewClientsRU->TabIndex = 0;
         this->listViewClientsRU->UseCompatibleStateImageBehavior = false;
         this->listViewClientsRU->View = System::Windows::Forms::View::Details;
         this->listViewClientsRU->ItemChecked += gcnew System::Windows::Forms::ItemCheckedEventHandler(this, &Form1::logConnectionCheckBox_Checked);
         this->listViewClientsRU->ItemMouseHover += gcnew System::Windows::Forms::ListViewItemMouseHoverEventHandler(this, &Form1::listView_ItemMouseHover);
         this->listViewClientsRU->VisibleChanged += gcnew System::EventHandler(this, &Form1::listViewClientsRU_Visible);
         this->listViewClientsRU->MouseLeave += gcnew System::EventHandler(this, &Form1::listViewClientsRU_MouseLeave);
         // 
         // columnHeaderRUIndex
         // 
         this->columnHeaderRUIndex->Text = L"Index";
         // 
         // columnHeaderRUIPAddress
         // 
         this->columnHeaderRUIPAddress->Text = L"IP Address";
         this->columnHeaderRUIPAddress->Width = 160;
         // 
         // columnHeaderRUCreated
         // 
         this->columnHeaderRUCreated->Text = L"Created";
         this->columnHeaderRUCreated->Width = 180;
         // 
         // columnHeaderRULastLog
         // 
         this->columnHeaderRULastLog->Text = L"Last log";
         this->columnHeaderRULastLog->Width = 180;
         // 
         // columnHeaderRULines
         // 
         this->columnHeaderRULines->Text = L"Lines";
         // 
         // toolStrip1
         // 
         this->toolStrip1->Items->AddRange(gcnew cli::array< System::Windows::Forms::ToolStripItem^  >(5) {
           this->toolStripButtonClear,
             this->toolStripButtonSelectAll, this->toolStripButtonCopyToClipboard, this->toolStripSeparator1, this->toolStripButtonExit
         });
         this->toolStrip1->Location = System::Drawing::Point(0, 0);
         this->toolStrip1->Name = L"toolStrip1";
         this->toolStrip1->Size = System::Drawing::Size(792, 25);
         this->toolStrip1->TabIndex = 2;
         this->toolStrip1->Text = L"toolStrip1";
         // 
         // toolStripButtonClear
         // 
         this->toolStripButtonClear->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"toolStripButtonClear.Image")));
         this->toolStripButtonClear->ImageScaling = System::Windows::Forms::ToolStripItemImageScaling::None;
         this->toolStripButtonClear->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->toolStripButtonClear->Name = L"toolStripButtonClear";
         this->toolStripButtonClear->Size = System::Drawing::Size(54, 22);
         this->toolStripButtonClear->Text = L"Clear";
         this->toolStripButtonClear->ToolTipText = L"Clear view";
         this->toolStripButtonClear->Click += gcnew System::EventHandler(this, &Form1::toolStripButtonClear_Click);
         // 
         // toolStripButtonSelectAll
         // 
         this->toolStripButtonSelectAll->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"toolStripButtonSelectAll.Image")));
         this->toolStripButtonSelectAll->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->toolStripButtonSelectAll->Name = L"toolStripButtonSelectAll";
         this->toolStripButtonSelectAll->Size = System::Drawing::Size(73, 22);
         this->toolStripButtonSelectAll->Text = L"Select all";
         this->toolStripButtonSelectAll->Click += gcnew System::EventHandler(this, &Form1::toolStripMenuItemSelectAll_Click);
         // 
         // toolStripButtonCopyToClipboard
         // 
         this->toolStripButtonCopyToClipboard->Enabled = false;
         this->toolStripButtonCopyToClipboard->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"toolStripButtonCopyToClipboard.Image")));
         this->toolStripButtonCopyToClipboard->ImageScaling = System::Windows::Forms::ToolStripItemImageScaling::None;
         this->toolStripButtonCopyToClipboard->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->toolStripButtonCopyToClipboard->Name = L"toolStripButtonCopyToClipboard";
         this->toolStripButtonCopyToClipboard->Size = System::Drawing::Size(122, 22);
         this->toolStripButtonCopyToClipboard->Text = L"Copy to clipboard";
         this->toolStripButtonCopyToClipboard->Click += gcnew System::EventHandler(this, &Form1::toolStripMenuItemCopyToClipboard_Click);
         // 
         // toolStripSeparator1
         // 
         this->toolStripSeparator1->Name = L"toolStripSeparator1";
         this->toolStripSeparator1->Size = System::Drawing::Size(6, 25);
         // 
         // toolStripButtonExit
         // 
         this->toolStripButtonExit->Image = (cli::safe_cast<System::Drawing::Image^>(resources->GetObject(L"toolStripButtonExit.Image")));
         this->toolStripButtonExit->ImageTransparentColor = System::Drawing::Color::Magenta;
         this->toolStripButtonExit->Name = L"toolStripButtonExit";
         this->toolStripButtonExit->Size = System::Drawing::Size(45, 22);
         this->toolStripButtonExit->Text = L"Exit";
         this->toolStripButtonExit->ToolTipText = L"Exit application";
         this->toolStripButtonExit->Click += gcnew System::EventHandler(this, &Form1::toolStripButtonExit_Click);
         // 
         // timerPurge
         // 
         this->timerPurge->Interval = 10000;
         this->timerPurge->Tick += gcnew System::EventHandler(this, &Form1::timerPurge_Tick);
         // 
         // tabPage1
         // 
         this->tabPage1->Location = System::Drawing::Point(0, 0);
         this->tabPage1->Name = L"tabPage1";
         this->tabPage1->Size = System::Drawing::Size(200, 100);
         this->tabPage1->TabIndex = 0;
         // 
         // contextMenuStrip1
         // 
         this->contextMenuStrip1->Name = L"contextMenuStrip1";
         this->contextMenuStrip1->Size = System::Drawing::Size(61, 4);
         // 
         // timerRUIP
         // 
         this->timerRUIP->Interval = 50;
         this->timerRUIP->Tick += gcnew System::EventHandler(this, &Form1::timerRUIP_Tick);
         // 
         // Form1
         // 
         this->AutoScaleDimensions = System::Drawing::SizeF(6, 13);
         this->AutoScaleMode = System::Windows::Forms::AutoScaleMode::Font;
         this->ClientSize = System::Drawing::Size(792, 366);
         this->Controls->Add(this->toolStrip1);
         this->Controls->Add(this->tabControl);
         this->Icon = (cli::safe_cast<System::Drawing::Icon^>(resources->GetObject(L"$this.Icon")));
         this->Name = L"Form1";
         this->Text = L"N-JRU - ver \?\?\?";
         this->FormClosing += gcnew System::Windows::Forms::FormClosingEventHandler(this, &Form1::Form1_FormClosing);
         this->Load += gcnew System::EventHandler(this, &Form1::Form1_Load);
         this->Shown += gcnew System::EventHandler(this, &Form1::Form1_Shown);
         this->Resize += gcnew System::EventHandler(this, &Form1::Form1_Resize);
         this->contextMenuStrip->ResumeLayout(false);
         this->tabControl->ResumeLayout(false);
         this->tabPageLog->ResumeLayout(false);
         this->tabPageClientsNJRU->ResumeLayout(false);
         this->tabPageClientsRU->ResumeLayout(false);
         this->toolStrip1->ResumeLayout(false);
         this->toolStrip1->PerformLayout();
         this->ResumeLayout(false);
         this->PerformLayout();

       }
#pragma endregion
  private: System::Void Form1_Load(System::Object^  sender, System::EventArgs^  e) {
    LoadDefaultSettings();
    LogSavedItems();

    // Create a mutex for accessing the Log listview tab with no initial owner
    this->g_listViewLogMutex = CreateMutex(
      NULL,              // default security attributes
      FALSE,             // initially not owned
      NULL);             // unnamed mutex
    if (g_listViewLogMutex == NULL)
    {
      std::cout << "CreateMutex error: " << GetLastError() << "\n"; //shows only in the debug window in debug mode
    }
  }
  private: System::Void timerNJRUIP_Tick(System::Object^  sender, System::EventArgs^  e) {

    timerNJRUIP->Enabled = false;
    PollNewConnections(this->g_njru);
    PollIP(this->g_njru);
    timerNJRUIP->Enabled = true;	
  }
  private: System::Void timerRUIP_Tick(System::Object^  sender, System::EventArgs^  e) {

    timerRUIP->Enabled = false;
    PollNewConnections(this->g_ru);
    PollIP(this->g_ru);
    timerRUIP->Enabled = true;
  }
  private: System::Void Form1_Shown(System::Object^  sender, System::EventArgs^  e) {

    LoadDesktop();
    initSocketEnvironment();
    ListenIP(g_njru);
    ListenIP(g_ru);
    this->timerNJRUIP->Enabled = true;
    this->timerRUIP->Enabled = true;
  }
  private: System::Void Form1_FormClosing(System::Object^  sender, System::Windows::Forms::FormClosingEventArgs^  e) {
    CloseSockets(g_njru->socketListenId);
    CloseSockets(g_ru->socketListenId);
    exitSocketEnvironment();
    SaveDesktop();
    CloseHandle(g_listViewLogMutex);
  }
  private: System::Void contextMenuStrip_Opening(System::Object^  sender, System::ComponentModel::CancelEventArgs^  e) {
  }

  private: System::Void Form1_Resize(System::Object^  sender, System::EventArgs^  e)
  {
    tabControl->Height = Height - toolStrip1->Height - 34;

  }
  private: System::Void toolStripButtonClear_Click(System::Object^  sender, System::EventArgs^  e)
  {
    toolStripButtonCopyToClipboard->Enabled = false;
    if (tabControl->SelectedIndex == 0)
    {
        listViewLog->Items->Clear();
    }
    else
    {
      if (this->g_visibleListView == e_NJRU)
      {
        ClearView(listViewClientsNJRU);
      }
      else if (this->g_visibleListView == e_RU)
      {
        ClearView(listViewClientsRU);
      }
    }
  }
  private: System::Void toolStripMenuItemClear_Click(System::Object^  sender, System::EventArgs^  e)
  {
    listViewLog->Items->Clear();
  }
  private: System::Void toolStripButtonExit_Click(System::Object^  sender, System::EventArgs^  e)
  {
    Close();
  }
  private: System::Void timerPurge_Tick(System::Object^  sender, System::EventArgs^  e)
  {
    timerPurge->Enabled = false;
    PurgeLogFiles(g_njru, "log");

    CompressLogFiles(g_ru);
    PurgeLogFiles(g_ru, "log");
    PurgeLogFiles(g_ru, "gz");
    timerPurge->Enabled = true;
  }
  private: System::Void toolStripMenuItemCopyToClipboard_Click(System::Object^  sender, System::EventArgs^  e)
  {
    // Copy listViewLog items as text to clipboard
    int itemsCount = listViewLog->SelectedIndices->Count;
    System::String^ outputClipboard = System::String::Empty;

    for (int i = 0; i < itemsCount; i++)
    {
      int selectedIndex = listViewLog->SelectedIndices[i];
      for (int j = 0; j < listViewLog->Items[selectedIndex]->SubItems->Count; j++)
      {
        outputClipboard += listViewLog->Items[selectedIndex]->SubItems[j]->Text + "\t";
      }
      outputClipboard += "\r\n";
    }

    if (!System::String::IsNullOrEmpty(outputClipboard))
    {
      try
      {
        System::Windows::Forms::Clipboard::SetText(outputClipboard);
      }
      catch (...)
      {
      }
    }
  }
  private: System::Void toolStripMenuItemSelectAll_Click(System::Object^  sender, System::EventArgs^  e)
  {
    for (int i = 0; i < listViewLog->Items->Count; i++)
    {
      //Checks all items
      listViewLog->Items[i]->Selected = true;
    }

  }
  private: System::Void tabControl_SelectedIndexChanged(System::Object^  sender, System::EventArgs^  e)
  {
    if (tabControl->SelectedIndex == 0)
    {
      toolStripButtonSelectAll->Enabled = true;
      if (listViewLog->SelectedIndices->Count)
      {
        toolStripButtonCopyToClipboard->Enabled = true;
      }
      else
      {
        toolStripButtonCopyToClipboard->Enabled = false;
      }
    }
    else
    {
      toolStripButtonSelectAll->Enabled = false;
      toolStripButtonCopyToClipboard->Enabled = false;
    }

  }
  private: System::Void listViewLog_ItemSelectionChanged(System::Object^  sender, System::Windows::Forms::ListViewItemSelectionChangedEventArgs^  e)
  {
    toolStripButtonCopyToClipboard->Enabled = true;
  }
  private: System::Void tabPage2_Click(System::Object^  sender, System::EventArgs^  e) {
  }

  private: System::Void logConnectionCheckBox_Checked(System::Object^  sender, System::Windows::Forms::ItemCheckedEventArgs^  e)
  {
    int index = e->Item->Index;
    if (this->g_visibleListView == e_NJRU && ((ListView^)sender)->Focused == true)
    {
      this->g_njru->CCB[index].showInLogTab = e->Item->Checked;
    }
    else if (this->g_visibleListView == e_RU && ((ListView^)sender)->Focused == true) // Focused because there seems to be a bug
    {
      this->g_ru->CCB[index].showInLogTab = e->Item->Checked;
    }
  }
  private: System::Void listViewClientsRU_Visible(System::Object^  sender, System::EventArgs^  e)
  {
    this->g_visibleListView = e_RU;
  }
  private: System::Void listViewClientsNJRU_Visible(System::Object^  sender, System::EventArgs^  e)
  {
    this->g_visibleListView = e_NJRU;
  }

  private: System::Void listView_ItemMouseHover(System::Object^  sender, System::Windows::Forms::ListViewItemMouseHoverEventArgs^  e)
  {
    this->g_showInLogTip->Show("Check to log", e->Item->ListView);

  }
  private: System::Void listViewClientsRU_MouseLeave(System::Object^  sender, System::EventArgs^  e)
  {
    this->g_showInLogTip->Active = false;
    this->g_showInLogTip->Active = true;
  }

  private: System::Void column_handler(System::Object^  sender, System::EventArgs^  e)
  {
    this->g_showInLogTip->Active = false;
    this->g_showInLogTip->Active = true;
  }
};
}


