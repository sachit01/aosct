#pragma once

#include <string>

#define MAX_CHARS_TO_READ 2048
#define MAX_CLIENT_CONNECTIONS 20
#define MAX_LOG_ITEM_SIZE 2048

#define MAX_LOG_LINES_DISPLAYED 1000

#define TAB 9

std::string SettingsProfile;
std::string DesktopProfile;

char RUVersion[] = "3.2";
char ApplicationCaption[50];

const int defaultLogPurgeDays = 7;
const int minLogPurgeDays = 1;
const int maxLogPurgeDays = 180;
const int logfileIntervalInHours = 6;

enum e_ru
{
  e_NJRU = 0,
  e_RU,
  e_DEFAULT
};

typedef struct SocketBuffer_s
{
  char logItemBuffer[MAX_LOG_ITEM_SIZE];
  bool isBuffering;

} SocketBuffer_t;

typedef struct ClientConnection_s
{
  SOCKET socketId;
  std::string IPAddr;
  std::string CreatedTimestamp;
  std::string ReceivedTimestamp;
  unsigned long int LinesReceived;
  bool showInLogTab;
  bool isActive;
  SocketBuffer_t buffer;
} ClientConnection_t;

typedef struct ConfigRU_s
{
  enum e_ru type;
  int  IPListenToPort;
  int  LogPurgeDays;
  int  LogSource;
  std::string LogFilePath;
  std::string prefix;
} ConfigRU_t;

typedef struct ConfigCommon_s
{
  bool  ShowTimestamp;
  bool  ShowSource;
} ConfigCommon_t;

typedef struct RU_s
{
  ClientConnection_t CCB[MAX_CLIENT_CONNECTIONS];
  ConfigRU_t config;
  SOCKET socketListenId;
  int noActiveClientConnections; // Should not exceed MAX_CLIENT_CONNECTIONS
} RU_t;


