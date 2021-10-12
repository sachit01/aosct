// RU.cpp : main project file.
/****************************************************************************
*
*    REVISION HISTORY :
*
*      Rev   Date        Name      Measures
*      --------------------------------------------------------------------
*      1.0.2 2011-08-16   BoH      MAX_CHARS_TO_READ 1024 -> 2048
*      1.0.3 2011-11-18   BoH      Rebuilt for .Net 2.0 (instead of 3.5) in order to support Xp Embedded
*      2.0.0 2011-12-08   BoH      Redesigned with TabControl etc
*      2.0.1 2011-12-09   BoH      Rebuilt for .Net 3.5 (Both 2.0 and 3.5 works with new MMI Xp Embedded image)
*      2.0.4 2012-03-21   BoH      Increased MAX_CLIENT_CONNECTIONS 10->20
*      2.1.0 2013-05-30   BoH      CopyToClipboard, Select all , ini-file option to exclude Source IP from logged line.
*      2.1.1 2013-12-17   BoH      Write N-JRU version to log-file at each startup of N-JRU and each new log-file
*      3.0.0 2018-05-04   MoM      Version 3.0, extended with support for RU. Name change from N-JRU to RU
****************************************************************************/


#include "stdafx.h"
#include "RU.h"
#include "Form1.h"
#include "tcpip.h"
#include "iostream"

namespace RU
{

  [STAThreadAttribute]
  int main(array<System::String ^> ^args)
  {
    // Enabling Windows XP visual effects before any controls are created
    Application::EnableVisualStyles();
    Application::SetCompatibleTextRenderingDefault(false);

    // Create the main window and run it
    Application::Run(gcnew Form1());
    return 0;
  }

  /**********************************************************
  * Function:     ConvertFromManagedString
  * Description:  Convert a managed string to a wide string
  **********************************************************/
  WCHAR* Form1::ConvertFromManagedString(String ^ManagedStr)
  {
    return (WCHAR *)System::Runtime::InteropServices::Marshal::StringToHGlobalUni(ManagedStr).ToPointer();

  }

  /**********************************************************
  * Function:     ConvertFromManagedString
  * Description:  Convert a managed string to a string
  **********************************************************/
  char* Form1::ConvertFromManagedStringToAnsi(String ^ManagedStr)
  {
    return (char *)System::Runtime::InteropServices::Marshal::StringToHGlobalAnsi(ManagedStr).ToPointer();

  }

  /**********************************************************
  * Function:     LoadDefaultSettings
  * Description:  Load settings from INI-file
  **********************************************************/
  void Form1::LoadDefaultSettings(void)
  {
    char exepath[MAX_PATH];
    GetModuleFileNameA(0, exepath, MAX_PATH);
    SettingsProfile = ChangeFileExt(exepath, ".Ini");

    /*
     * Load common settings.
     */

     // View timestamp?
    this->g_commonConfig->ShowTimestamp = GetPrivateProfileIntA("View", "Timestamp", 0, SettingsProfile.c_str()) != 0;
    // View source IP?
    this->g_commonConfig->ShowSource = GetPrivateProfileIntA("View", "Source", 0, SettingsProfile.c_str()) != 0;

    /*
     * Load RU specific settings.
     */
    LoadSettings(&this->g_njru->config);
    LoadSettings(&this->g_ru->config);

    /*
     * Update the GUI according to settings.
     */
     // Set window title
    sprintf_s(ApplicationCaption, sizeof(ApplicationCaption),
      "RU v%s - NJRU-Port:%ld, RU-Port:%ld", RUVersion, this->g_njru->config.IPListenToPort, this->g_ru->config.IPListenToPort);
    Text = gcnew String(ApplicationCaption);

    LogRUItem(e_RU, ApplicationCaption);

    // Update the logview to show only colums that are chosen in the settings
    // The order is importante since the indexing of the column changes as they are removed
    if (!this->g_commonConfig->ShowSource)
    {
      // Remove column for source (2nd column)
      listViewLog->Columns->RemoveAt(1);
    }
    if (!this->g_commonConfig->ShowTimestamp)
    {
      // Remove column for Timestamp (1st column)
      listViewLog->Columns->RemoveAt(0);
    }

    timerPurge->Enabled = true;
  }

  /**********************************************************
  * Function:     LoadSettings
  * Description:  Load RU specific settings for the given RU from INI-file
  **********************************************************/
  void Form1::LoadSettings(ConfigRU_t *config)
  {
    std::string prefix = (config->type == e_RU) ? "RU-" : "";

    // Read the Listening port
    char listenIPPort[10];
    char *defaultListenIPPort = (config->type == e_RU) ? "30180" : "30131";
    std::string name = prefix + "ListenPort";
    GetPrivateProfileStringA("IP", name.c_str(), defaultListenIPPort, listenIPPort, sizeof(listenIPPort), SettingsProfile.c_str());
    config->IPListenToPort = atoi(listenIPPort);

    // Read the "log file path"
    char moduleFileName[MAX_PATH];
    GetModuleFileNameA(0, moduleFileName, MAX_PATH);
    char backSlash = '\\';
    char *backSlashPos = strrchr(moduleFileName, backSlash);

    if (backSlashPos)
    {  // Cut file name and extension
      *backSlashPos = '\0';
    }

    std::string defaultLogFilePath(moduleFileName);

    if (config->type == e_NJRU)
    {
      defaultLogFilePath += "\\NJRU-log";
    }
    else
    {
      defaultLogFilePath += "\\RU-log";
    }

    name = prefix + "Path";
    char stringValue[MAX_PATH];
    GetPrivateProfileStringA("Log", name.c_str(), defaultLogFilePath.c_str(), stringValue, sizeof(stringValue), SettingsProfile.c_str());
    config->LogFilePath = std::string(stringValue);

    // To handle the case when the given path is in full path format.
    String^ dir;
    if (strchr(config->LogFilePath.c_str(), ':'))
    {
      dir = gcnew String(config->LogFilePath.c_str());
    }
    else
    {
      dir = System::IO::Directory::GetCurrentDirectory() + (gcnew String(config->LogFilePath.c_str()));
    }

    // Check it the "log file path" directory exist. If not we create it.
    if (!System::IO::Directory::Exists(dir))
    {
      System::IO::Directory::CreateDirectory(dir);
    }

    // Read the PurgeDays setting
    name = prefix + "PurgeDays";
    config->LogPurgeDays = GetPrivateProfileIntA("Log", name.c_str(), defaultLogPurgeDays, SettingsProfile.c_str());
    if ((config->LogPurgeDays < minLogPurgeDays) ||
      (config->LogPurgeDays > maxLogPurgeDays))
    {
      // if the configured value is outside the allowed range
      config->LogPurgeDays = defaultLogPurgeDays;

      char errorMessage[128];
      sprintf_s(errorMessage, sizeof(errorMessage),
        "Configured LogPurgeDays is out of the range %d-%d. Using default value %d days",
        minLogPurgeDays, maxLogPurgeDays, defaultLogPurgeDays);
      LogRUItem(config->type, errorMessage);
    }

    // Save source IP to logfile?
    name = prefix + "Source";
    config->LogSource = GetPrivateProfileIntA("Log", name.c_str(), 0, SettingsProfile.c_str());
  }

  /**********************************************************
  * Function:     LoadDesktop
  * Description:  Load desktop (window pos,size) from INI-file
  **********************************************************/
  void Form1::LoadDesktop(void)
  {
    char exepath[MAX_PATH];
    GetModuleFileNameA(0, exepath, MAX_PATH);
    DesktopProfile = ChangeFileExt(exepath, ".Ini");

    System::Windows::Forms::FormWindowState LoadWindowState =
      (System::Windows::Forms::FormWindowState) GetPrivateProfileIntA("Pos", "WindowsState", (int)WindowState, DesktopProfile.c_str());

    WindowState = LoadWindowState;

    Left = GetPrivateProfileIntA("Pos", "Left", Left, DesktopProfile.c_str());
    Top = GetPrivateProfileIntA("Pos", "Top", Top, DesktopProfile.c_str());
    Width = GetPrivateProfileIntA("Size", "Width", Width, DesktopProfile.c_str());
    Height = GetPrivateProfileIntA("Size", "Height", Height, DesktopProfile.c_str());
  }

  /**********************************************************
  * Function:     WritePrivateProfileIntA
  * Description:  Writes an integer to a private profile (INI-file)
  **********************************************************/
  BOOL Form1::WritePrivateProfileIntA(LPCSTR lpAppName, LPCSTR lpKeyName,
    int Value, LPCSTR lpFileName)
  {
#define MaxNumChars 40
    char numStr[MaxNumChars];
    snprintf(numStr, sizeof(numStr), "%d", Value);
    return WritePrivateProfileStringA(lpAppName, lpKeyName,
      numStr, lpFileName);

  }

  /**********************************************************
  * Function:     SaveDesktop
  * Description:  Save desktop (window pos,size) to INI-file
  **********************************************************/
  void Form1::SaveDesktop(void)
  {
    if (WindowState == System::Windows::Forms::FormWindowState::Normal)
    {
      WritePrivateProfileIntA("Pos", "Left", Left, DesktopProfile.c_str());
      WritePrivateProfileIntA("Pos", "Top", Top, DesktopProfile.c_str());
      WritePrivateProfileIntA("Size", "Width", Width, DesktopProfile.c_str());
      WritePrivateProfileIntA("Size", "Height", Height, DesktopProfile.c_str());
    }

    WritePrivateProfileIntA("Pos", "WindowsState", (int)WindowState, DesktopProfile.c_str());
  }

  /**********************************************************
  * Function:     ChangeFileExt
  * Description:  Change the extension part of a filename.
  *               The new extension is fileext (including the '.')
  **********************************************************/
  std::string Form1::ChangeFileExt(const char *filepath, const char *fileext)
  {
    std::string newName = std::string(filepath);

    char dot = '.';
    const char *extPos = strrchr(filepath, dot);
    if (extPos != NULL)
    {
      newName = std::string(filepath, extPos - filepath) + std::string(fileext);
    }
    else
    {
      newName = std::string(filepath);
    }

    return newName;
  }

  /**********************************************************
  * Function:     InitRU
  * Description:  Init the recording unit.
  *
  **********************************************************/
  void Form1::InitRU(RU_t *ru, enum e_ru type, char *prefix)
  {
    ru->noActiveClientConnections = 0;
    ru->config.type = type;
    ru->config.IPListenToPort = 0;
    ru->config.LogPurgeDays = 0;
    ru->config.LogSource = 0;
    ru->config.LogFilePath = std::string();
    ru->config.prefix = std::string(prefix);

    Form1::InitIP(ru->CCB);

    if (type == e_NJRU)
    {
      // Set up a "dummy" control block for logging from here
      AddSocket(&ru->CCB[0], INVALID_SOCKET, "-", type);

      ru->noActiveClientConnections = 1;
    }
  }

  /**********************************************************
  * Function:     InitIP
  * Description:  Init IP environment.
  *
  **********************************************************/
  void Form1::InitIP(ClientConnection_t *ccb)
  {
    for (int i = 0; i < MAX_CLIENT_CONNECTIONS; i++)
    {
      ccb[i].socketId = INVALID_SOCKET;
      ccb[i].IPAddr = std::string();
      ccb[i].CreatedTimestamp = std::string();
      ccb[i].ReceivedTimestamp = std::string();
      ccb[i].LinesReceived = 0;
      ccb[i].showInLogTab = false;
      ccb[i].isActive = false;
      ccb[i].buffer.isBuffering = false;
      ccb[i].buffer.logItemBuffer[0] = '\0';
    }
  }

  /**********************************************************
  * Function:     getLogFileTime
  * Description:  Returns the time dependent part of the log file name
  **********************************************************/
  std::string Form1::getLogFileTime(const e_ru type)
  {
    System::DateTime dt = System::DateTime::Now;
    char timeStamp[64];

    if (type == e_RU)
    {
      const uint8_t startHour = (dt.Hour / logfileIntervalInHours) * logfileIntervalInHours;

      snprintf(timeStamp, sizeof(timeStamp), "%04d-%02d-%02d_%02d.00", dt.Year, dt.Month, dt.Day, startHour);
    }
    else
    {
      snprintf(timeStamp, sizeof(timeStamp), "%04d-%02d-%02d", dt.Year, dt.Month, dt.Day);
    }

    return std::string(timeStamp);
  }

  /**********************************************************
  * Function:     getLogFileName
  * Description:  Returns the current log file name
  **********************************************************/
  std::string Form1::getLogFileName(const ConfigRU_t* config)
  {
    return config->prefix + "_" + getLogFileTime(config->type) + ".log";
  }

  /**********************************************************
  * Function:     FileExists
  * Description:  Returns true if file exists, else false
  *
  **********************************************************/
  bool Form1::FileExists(const char *PathAndFileName)
  {
    WIN32_FIND_DATAA findFileData;
    HANDLE hFind;
    hFind = FindFirstFileA(PathAndFileName, &findFileData);
    if (hFind == INVALID_HANDLE_VALUE)
    {
      return false;
    }
    else
    {
      return true;
    }
  }

  /**********************************************************
  * Function:     WriteToDiskFile
  * Description:  Writes given log item to file
  *
  **********************************************************/
  void Form1::WriteToDiskFile(const ConfigRU_t *config, const char *logFileName, const char *timestamp, const char *source, const char *text)
  {
    std::string logFilePathAndName = config->LogFilePath + "\\" + logFileName;

    if (config->type == e_NJRU)
    {
      if (!FileExists(logFilePathAndName.c_str()))
      {
        WriteInfo(ApplicationCaption, timestamp, logFilePathAndName.c_str());
      }
    }

    FILE *logf = fopen(logFilePathAndName.c_str(), "a+");
    if (logf)
    {
      // Include source IP in logged line
      if (config->LogSource)
      {
        fprintf(logf, "%s %s%c%s\n", timestamp, source, TAB, text);
      }
      else
      {
        fprintf(logf, "%s %s\n", timestamp, text);
      }
      fclose(logf);
    }
  }

  /**********************************************************
  * Function:     WriteInfo
  * Description:  Writes the given text including the given timestamp in to the given file 
  *
  **********************************************************/
  void Form1::WriteInfo(const char *text, const char *timestamp, const char *logFilePathAndName)
  {
    FILE *logf = fopen(logFilePathAndName, "a+");
    if (logf)
    {
      // Include source IP in logged line
      fprintf(logf, "%s%c%s\n", timestamp, TAB, text);
      fclose(logf);
    }
  }

  /**********************************************************
  * Function:     LogSavedItems
  * Description:  Logs the items in g_listOfItemsToLog to file and display and clears g_listOfItemsToLog.
  **********************************************************/
  void Form1::LogSavedItems()
  {
    while (g_listOfItemsToLog->size() > 0)
    {
      LogItem(g_listOfItemsToLog->front());
      g_listOfItemsToLog->pop_front();
    }
  }

  /**********************************************************
  * Function:     LogItem
  * Description:  Log item to file and display
  *
  **********************************************************/
  void Form1::LogItem(ClientConnection_t* ccb, const ConfigRU_t* config, const char* line)
  {
    DWORD dwWaitResult = WaitForSingleObject(g_listViewLogMutex,    // handle to mutex
                                             INFINITE);             // no time-out interval

    ItemToLog itemToLog;
    itemToLog.ccb = ccb;
    itemToLog.config = config;
    itemToLog.line = line;

    if ((dwWaitResult == WAIT_OBJECT_0) || (dwWaitResult == WAIT_ABANDONED))
    {
      LogSavedItems();

      LogItem(itemToLog);
    }
    else
    {
      g_listOfItemsToLog->push_back(itemToLog);
    }
  }

  /**********************************************************
  * Function:     LogItem
  * Description:  Log item to file and display
  *
  **********************************************************/
  void Form1::LogItem(const ItemToLog& itemToLog)
  {
    char timestampShow[] = "YYYY-MM-DD HH:MM:SS";
    char timestampLog[] = "YYYY-MM-DDTHH:MM:SS+00:00 ";

    System::DateTime dt = System::DateTime::Now;
    System::TimeZone^ localZone = TimeZone::CurrentTimeZone;
    System::TimeSpan ts = localZone->GetUtcOffset(dt);

    snprintf(timestampShow, sizeof(timestampShow), "%04d-%02d-%02d %02d:%02d:%02d", dt.Year, dt.Month, dt.Day, dt.Hour, dt.Minute, dt.Second);
    snprintf(timestampLog, sizeof(timestampLog), "%04d-%02d-%02dT%02d:%02d:%02d+%02d:%02d", dt.Year, dt.Month, dt.Day, dt.Hour, dt.Minute, dt.Second, ts.Hours, ts.Minutes);

    // Show in the Log tab only of the checkbox in the client tab is set.
    if (itemToLog.ccb->showInLogTab)
    {
      ListViewItem^ item;
      // Add the timestamp if configured
      if (this->g_commonConfig->ShowTimestamp)
      {
        item = listViewLog->Items->Insert(0, dt.ToString());
      }

      // Add the information about the origins of the message if configured to
      if (this->g_commonConfig->ShowSource)
      {
        if (!item)
        {
          item = listViewLog->Items->Insert(0, gcnew String(itemToLog.ccb->IPAddr.c_str()));
        }
        else
        {
          item->SubItems->Add(gcnew String(itemToLog.ccb->IPAddr.c_str()));
        }
      }

      //Finally add the message content
      String^ message = gcnew String(itemToLog.line.c_str());

      // If RU is of type RU we show only 100 characters in the logview tab
      if ((itemToLog.config->type == e_RU) && (message->Length >= 100))
      {
        message = message->Substring(0, 100);
        message = String::Concat(message, "...");
      }

      if (!item)
      {
        item = listViewLog->Items->Insert(0, message);
      }
      else
      {
        item->SubItems->Add(message);
      }

      // Limit no of displayed lines
      int lastIndex = listViewLog->Items->Count - 1;
      while (lastIndex >= MAX_LOG_LINES_DISPLAYED)
      {
        listViewLog->Items->RemoveAt(lastIndex);
        lastIndex = listViewLog->Items->Count - 1;
      }
    }

    itemToLog.ccb->ReceivedTimestamp = std::string(timestampShow);
    itemToLog.ccb->LinesReceived++;

    std::string logFileName = getLogFileName(itemToLog.config);

    WriteToDiskFile(itemToLog.config, logFileName.c_str(), timestampLog, itemToLog.ccb->IPAddr.c_str(), itemToLog.line.c_str());
  }

  /**********************************************************
  * Function:     LogRUItem
  * Description:  Log a message from this application to file and display
  **********************************************************/
  void Form1::LogRUItem(const e_ru type, const char* line)
  {
    const char* typeString = (type == e_RU) ? "RU  " : "NJRU";
    System::DateTime dt = System::DateTime::Now;

    char message[256];
    snprintf(message, sizeof(message),
      "%02d:%02d:%02d.%03d                      %s           : %s",
      dt.Hour, dt.Minute, dt.Second, dt.Millisecond, typeString, line);

    LogItem(&g_njru->CCB[0], &g_njru->config, message);
  }

  /**********************************************************
  * Function:     FindFstEmptyCCBSlot
  * Description:  Find the first unused slot in the CCB container
  *               Maybe a new slot or one previously "used"
  **********************************************************/
  int RU::Form1::FindFstEmptyCCBSlot(RU_t *ru)
  {
    for (int i = 0; i <= ru->noActiveClientConnections; i++)
    {
      if (ru->CCB[i].isActive == false)
      {
        ru->CCB[i].isActive = true;
        return i;
      }
    }
    return 0;
  }

  /**********************************************************
  * Function:     PollNewConnections
  * Description:  Handle connections.
  *               Run at 20 Hz
  **********************************************************/
  void Form1::PollNewConnections(RU_t *ru)
  {
    ClientConnection_t *ccb = ru->CCB;

    // To use the correct listview depending on the RU type
    ListView ^view = (ru->config.type == e_NJRU) ? this->listViewClientsNJRU : this->listViewClientsRU;

    // Server mode and need to accept new connections
    struct sockaddr Addr;
    SOCKET socketId = acceptSocket(ru->socketListenId, &Addr);
    if (socketId != INVALID_SOCKET)
    {
      if (SetSocketKeepAlive(ru->socketListenId) != TB_OK)
      {
        MessageBox::Show(this, "Set ListenSocket socket-option (keep-alive) failed!", "RU");
      }

      // Add socket to list of connections
      char IPAddr[20];
      DWORD IPAddrLength = sizeof(IPAddr);
      IPAddressToString(&Addr, IPAddr, &IPAddrLength);

      int emptySlotIndex = FindFstEmptyCCBSlot(ru);
      if (ru->noActiveClientConnections <= emptySlotIndex)
      {
        ru->noActiveClientConnections++; // Increase "number of connections" before calling DisplaySockets()
      }

      if (AddSocket(&ccb[emptySlotIndex], socketId, IPAddr, ru->config.type))
      {
        if (ru->noActiveClientConnections >= MAX_CLIENT_CONNECTIONS)
        {
          MessageBox::Show(this, "Max number of connections reached!", "RU");
        }

        DisplaySockets(view, ru);
      }
      else
      {
        MessageBox::Show(this, "AddSocket failed!", "RU");
      }
    }
  }

  /**********************************************************
  * Function:     PollIP
  * Description:  Handle reception of data.
  *               Run at 20 Hz
  * Parameters:   logItemBuffer - RU dedicated container for buffering received data between the polls
  *               isBuffering - Ru dedicated state to indicate previous message is buffering or not
  **********************************************************/
  void Form1::PollIP(RU_t *ru)
  {
    ClientConnection_t *ccb = ru->CCB;

    // To use the correct listview depending on the RU type
    ListView ^view = (ru->config.type == e_RU) ? this->listViewClientsRU : this->listViewClientsNJRU;

    // Check for any incoming data
    for (int i = 0; i < ru->noActiveClientConnections; i++)
    {
      if (ccb[i].socketId != INVALID_SOCKET)
      {
        char readBuffer[MAX_CHARS_TO_READ + 1] = "";
        bool anyCharRead = false;
        bool finishedSocket = false;
        int noBytesRead = 0;
        bool *isBuffering = &ccb[i].buffer.isBuffering;
        char *logItemBuffer = ccb[i].buffer.logItemBuffer;

        // Get data from socket as long as there is data to read.
        // The received data is Line-feed separated, so at each iteration
        // log/display the first "message" and then move on to the next in the buffer.
        do 
        {
          noBytesRead = 0;
          readSocket(ccb[i].socketId, readBuffer, MAX_CHARS_TO_READ, &noBytesRead);

          // If new data was read or data still remains to handle from previous read.
          if (noBytesRead > 0)
          {
            anyCharRead = true;

            // Clean any garbage that may have followed the frame.
            int c = noBytesRead - 1;
            while (readBuffer[c--] == '\0')
            {
              noBytesRead--;
            }

            readBuffer[noBytesRead] = '\0';
            char *readPtr = readBuffer;

            char *LFptr = strchr(readBuffer, '\n');
            if (LFptr)
            {
              // Message Length based on the start of the Readbuffer memory address and the line-feed pointer
              int msgLength = LFptr - readBuffer + 1;
              int readLength = 0;

              if (*isBuffering) // If started buffering contents from previous frame
              {
                readBuffer[msgLength - 1] = '\0';
                // The end of the message is received indicated by the line-feed so append the last part to the buffered content */
                strncat(logItemBuffer, readBuffer, msgLength);

                // Now log and display the message
                LogItem(&ccb[i], &ru->config, logItemBuffer);
                DisplaySocket(view, ccb, i);
                logItemBuffer[0] = '\0'; // reset
                *isBuffering = false;

                // If the frame contains even more data
                if (msgLength < noBytesRead)
                {
                  readPtr += msgLength;

                  // Look for a new line-feed within the rest of the received frame
                  LFptr = strchr(readPtr, '\n');
                  if (LFptr)
                  {
                    // Prepare for "iteration process"
                    msgLength = (LFptr - readPtr) + 1;  // next message lenth
                    readLength = msgLength;             // already read lenth
                  }
                  else
                  {
                    // If a new line-feed is not found it is safe to assume that the message is incomplete
                    // and we should start buffering again.
                    strncat(logItemBuffer, readPtr, noBytesRead - msgLength);
                    *isBuffering = true;
                    finishedSocket = true;
                  }
                }
                else
                {
                  finishedSocket = true;
                }
              }
              if (msgLength <= noBytesRead && !finishedSocket) // "iteration process"
              {
                // The frame has more contents than the first line-feed separated message, so iterate through
                // the frame and log and display any other message alternativly buffer an incomplete message
                while (readLength <= noBytesRead)
                {
                  readPtr[msgLength - 1] = '\0'; // To separate the first message from the rest
                  LogItem(&ccb[i], &ru->config, readPtr);
                  DisplaySocket(view, ccb, i);
                  readLength += msgLength;
                  readPtr += msgLength;

                  char *tLFptr = strchr(readPtr, '\n');
                  if (tLFptr)
                  {
                    // A complete message is found
                    msgLength = (tLFptr - readPtr + 1);
                    tLFptr++; // to jump the line-feed
                  }
                  else if (readLength == noBytesRead)
                  {
                    // Reach the end of the frame so break
                    noBytesRead = 0;
                    break;
                  }
                  else
                  {
                    // A line-feed is not found so it is safe to assume that the message is incomplete
                    // and we should start buffering it.
                    strncpy(logItemBuffer, readPtr, (noBytesRead - msgLength));
                    *isBuffering = true;
                    break;
                  }
                }
              }
            }
            else
            {
              // Buffer the received data since it does not contain a comlete message.
              strncat(logItemBuffer, readBuffer, noBytesRead);
              *isBuffering = true;
            }
          }

        } while (noBytesRead > 0);

        // If nothing was read check if the connection is still alive
        if (!anyCharRead)
        {
          if (isConnectedSocket(ccb[i].socketId) != TB_OK)
          {
            // Socket disconnected by the other end ?
            closeSocket(ccb[i].socketId);
            ccb[i].socketId = INVALID_SOCKET;
            ccb[i].isActive = false;
            DisplaySockets(view, ru);
          }
        }
      }
    }
  }

  /**********************************************************
  * Function:     AddSocket
  * Description:  Add socket to list of clients
  **********************************************************/
  bool Form1::AddSocket(ClientConnection_t *ccb, SOCKET socketId, const char *IPAddr, enum e_ru ru_type)
  {
    char timestamp[] = "YYYY-MM-DD HH:MM:SS";
    System::DateTime dt = System::DateTime::Now;
    sprintf(timestamp, "%04d-%02d-%02d %02d:%02d:%02d", dt.Year, dt.Month, dt.Day, dt.Hour, dt.Minute, dt.Second);

    bool finished = false;
    // Default setting for the different types of RU
    bool showInLog = ru_type == e_NJRU ? true : false;

    if (ccb->socketId == INVALID_SOCKET)
    {
      ccb->socketId = socketId;
      ccb->IPAddr = std::string(IPAddr);
      ccb->CreatedTimestamp = std::string(timestamp);
      ccb->ReceivedTimestamp = std::string();
      ccb->LinesReceived = 0;
      ccb->showInLogTab = showInLog;
      ccb->isActive = true;

      finished = true;
    }

    return finished;
  }

  /**********************************************************
  * Function:     ListenIP
  * Description:  Listen to the listen IP/port defined on form.
  *               Invoked once when the Form is "shown" the first time
  **********************************************************/
  void Form1::ListenIP(RU_t *ru)
  {
    ru->socketListenId = openSocket(SOCK_STREAM);

    if (ru->socketListenId != INVALID_SOCKET)
    {
      if (bindSocket(ru->socketListenId, ru->config.IPListenToPort) == TB_OK)
      {
        if (SetSocketKeepAlive(ru->socketListenId) != TB_OK)
        {
          MessageBox::Show(this, "Set socket-option (keep-alive) failed!", "RU");
        }

        if (listenSocket(ru->socketListenId) == TB_OK)
        {
          // Set non-blocking in order to avoid acceptSocket to wait forever
          SetNonBlockingSocket(ru->socketListenId);
        }
        else
        {
          MessageBox::Show(this, "Listen on socket failed!", "RU");
        }
      }
      else
      {
        MessageBox::Show(this, "Bind socket failed!", "RU");
      }
    }
  }

  /**********************************************************
  * Function:     CloseSockets
  * Description:  Close all sockets
  **********************************************************/
  void Form1::CloseSockets(SOCKET socket)
  {
    if (socket != INVALID_SOCKET)
    {
      closeSocket(socket);
    }

    // Close any open client sockets
  }

  /**********************************************************
  * Function:     DisplaySocket
  * Description:  Display dynamic data for a single socket
  **********************************************************/
  void Form1::DisplaySocket(ListView ^listview, ClientConnection_t *ccb, int Index)
  {
    ListViewItem^ item = listview->Items[Index];
    item->SubItems[3]->Text = gcnew String(ccb[Index].ReceivedTimestamp.c_str());
    item->SubItems[4]->Text = ccb[Index].LinesReceived.ToString();
  }

  /**********************************************************
  * Function:     DisplaySockets
  * Description:  Display all sockets for a given RU
  **********************************************************/
  void Form1::DisplaySockets(ListView ^view, RU_t *ru)
  {
    ClientConnection_t *ccb = ru->CCB;

    view->Items->Clear();
    for (int i = 0; i < ru->noActiveClientConnections; i++)
    {
      ListViewItem^ item = gcnew ListViewItem(i.ToString());
      if (ccb[i].isActive)
      {
        item->SubItems->Add(gcnew String(ccb[i].IPAddr.c_str()));
        item->SubItems->Add(gcnew String(ccb[i].CreatedTimestamp.c_str()));
        item->SubItems->Add(gcnew String(ccb[i].ReceivedTimestamp.c_str()));
        item->SubItems->Add(ccb[i].LinesReceived.ToString());
        item->Checked = ccb[i].showInLogTab;
      }
      view->Items->Add(item);
    }
  }

  /**********************************************************
  * Function:     ClearView
  * Description:  Clear the active tab-sheet
  **********************************************************/
  void Form1::ClearView(ListView ^view)
  {
    for (int i = 0; i < view->Items->Count; i++)
    {
      if (view->Items[i]->SubItems->Count >= 3)
      {
        view->Items[i]->SubItems[3]->Text = "";
        view->Items[i]->SubItems[4]->Text = "0";
      }
    }
  }

  /**********************************************************
  * Function:     PurgeLogFiles
  * Description:  Remove files older than the purge limit
  *               for the given RU
  **********************************************************/
  void Form1::PurgeLogFiles(RU_t *ru, char *extension)
  {
    std::string logFilePathAndName = ru->config.LogFilePath + "\\*." + extension;

    // The filetime limit
    __int64 purgeLimit64 = System::DateTime::Today.AddDays(-ru->config.LogPurgeDays).ToFileTime();
    // Convert to FILETIME
    ::FILETIME purgeLimit;
    purgeLimit.dwLowDateTime = purgeLimit64 & 0xFFFFFFFF;
    purgeLimit.dwHighDateTime = purgeLimit64 >> 32;

    WIN32_FIND_DATAA findFileData;
    HANDLE hFind = FindFirstFileA(logFilePathAndName.c_str(), &findFileData);
    if (hFind != INVALID_HANDLE_VALUE)
    {
      bool endOfFiles = false;
      while (!endOfFiles)
      {
        if (CompareFileTime(&findFileData.ftLastWriteTime, &purgeLimit) == -1)
        {
          std::string logFileToPurgePathAndName = ru->config.LogFilePath + std::string("\\") + std::string(findFileData.cFileName);

          DeleteFileA(logFileToPurgePathAndName.c_str());
        }

        if (FindNextFileA(hFind, &findFileData) == 0)
        {
          endOfFiles = true;
        }
      }

      FindClose(hFind);
    }

  }

  /**********************************************************
  * Function:     CompressLogFiles
  * Description:  Compress log files that are not new (that
  *               is created today) for the give RU
  **********************************************************/
  void Form1::CompressLogFiles(RU_t* ru)
  {
    std::string logFileTime = getLogFileTime(ru->config.type);

    // Check each file within the log file path to find if any should be compressed
    array <System::String^>^ files = System::IO::Directory::GetFiles(gcnew String(ru->config.LogFilePath.c_str()));
    Collections::IEnumerator^ e = files->GetEnumerator();
    while (e->MoveNext())
    {
      const char* name = (const char*) (void*) System::Runtime::InteropServices::Marshal::StringToHGlobalAnsi(e->Current->ToString());

      if ((!strstr(name, ".gz")) &&    // If the file is NOT already compressed AND
          (!strstr(name, logFileTime.c_str()))) // if the file is NOT created today
      {
        std::string compressCommand = std::string("gzip -f \"") + name + "\"";

        bool success = WindowsSystemCall(compressCommand.c_str());

        if (!success)
        {
          LogRUItem(e_RU, "Error: gzip failed or is not installed, cannot compress RU log files");
          break;
        }
      }
    }
  }

  /**********************************************************
  * Function:     WindowsSystemCall
  * Description:  Just like system() but with CreateProcess
  *               so the cmd window can be hiden
  **********************************************************/
  bool Form1::WindowsSystemCall(const char *cmd)
  {
    bool success = false;
    PROCESS_INFORMATION p_info;
    STARTUPINFO s_info;
    wchar_t cmdline[512];

    memset(&s_info, 0, sizeof(s_info));
    memset(&p_info, 0, sizeof(p_info));
    s_info.cb = sizeof(s_info);

    mbstowcs(cmdline, cmd, 512);

    if (CreateProcess(0, cmdline, NULL, NULL, 0, CREATE_NO_WINDOW, NULL, NULL, &s_info, &p_info))
    {
      WaitForSingleObject(p_info.hProcess, INFINITE);
      CloseHandle(p_info.hProcess);
      CloseHandle(p_info.hThread);
      success = true;
    }

    return success;
  }

} // namespace RU
