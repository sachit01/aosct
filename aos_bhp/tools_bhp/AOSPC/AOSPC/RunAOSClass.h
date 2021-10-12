#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          RunAOSClass.h %
*
*  %version:       6 %
*
*  %created_by:    marlundg %
*
*  %date_created:  2017-07-12 18:40 %
*
*  DESCRIPTION:    Class to run AOS processes
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
* 2016-10-16    Marlundg    Support for new console handling via TCP, ATP2 removed
* 2016-10-18    Marlundg    Trimmed received array from \0 to be able to copy to clipboard
* 2017-03-10    Marlundg    Support for Dispatcher Console
*
*******************************************************************************/
using namespace System;
using namespace Microsoft::Win32;
using namespace System::IO;
using namespace System::Diagnostics;
using namespace System::Text;

#include <timeapi.h>
#include "ConsoleConnection.h"
#include "LocoConsts.h"

#define MAX_REC_STRINGS     100
#define MAX_LINES_DISPLAYED 3000

namespace AOSPC {

    public ref class RunAOSClass 
    {
    public:
        // Output data (from ATx processes)
        bool                    atpARunning;
        bool                    atpBRunning;
        bool                    dispRunning;
        bool                    atoRunning;
        bool                    aosRunning;
        
        static int              atpARecStringCnt;
        static array<String^>^  atpARecStrings;
        
        static int              atpBRecStringCnt;
        static array<String^>^  atpBRecStrings;

        static int              dispRecStringCnt;
        static array<String^>^  dispRecStrings;
        
        static int              atoRecStringCnt;
        static array<String^>^  atoRecStrings;

        // Simulation Mode
        EnumSimulationMode SimMode;

        ConsoleConnection^ ATPAConsoleConnection;
        ConsoleConnection^ ATPBConsoleConnection;
        ConsoleConnection^ DispConsoleConnection;

        // SIL SimulationMode
        int             ATPConsolePortToConnect;

        // HIL SimulationMode
        String^     ATPAIP;
        int         ATPAConsolePortToConnect;
        String^     ATPBIP;
        int         ATPBConsolePortToConnect;
        String^     DISPIP;
        int         DispConsolePortToConnect;

        // TODO: Add console for ATO as well


    private: 

        // Required to be static to be used in event handling!
        static Process^         atpProcess;
        static Process^         atpBProcess;
        static Process^         atoProcess;

        /**********************************************************
        * Function:     Constructor
        * Description:  
        **********************************************************/
    public:
        RunAOSClass()
        {
            atpARunning = false;
            atpBRunning = false;
            dispRunning = false;
            atoRunning  = false;
            aosRunning  = false;
            atpProcess      = nullptr;
            atpBProcess     = nullptr;
            atoProcess      = nullptr;
            atpARecStrings  = gcnew array<String^>(MAX_REC_STRINGS);
            atpBRecStrings  = gcnew array<String^>(MAX_REC_STRINGS);
            dispRecStrings  = gcnew array<String^>(MAX_REC_STRINGS);
            atoRecStrings   = gcnew array<String^>(MAX_REC_STRINGS);
            atpARecStringCnt= 0;
            atpBRecStringCnt= 0;
            dispRecStringCnt = 0;
            atoRecStringCnt = 0;

            int i;
            for (i = 0; i < MAX_REC_STRINGS; i++)
            {
                atpARecStrings[i]   = "";
                atpBRecStrings[i]   = "";
                dispRecStrings[i]   = "";
                atoRecStrings[i]    = "";
            }

            ATPAConsoleConnection = gcnew ConsoleConnection();
            ATPBConsoleConnection = gcnew ConsoleConnection();
            DispConsoleConnection = gcnew ConsoleConnection();

        }

        /**********************************************************
        * Function:     Tick
        * Description:  
        **********************************************************/
    public:
        void Tick(void)
        {
            static DWORD lastTime = 0;

            // Time to check console data
            if ((timeGetTime() - lastTime) >= 100)
            {
                lastTime = timeGetTime();

                atpAOutputHandler();

                // Not used in SIL
                if (SimulationSil != SimMode)
                {        
                    atpBOutputHandler();
                    dispOutputHandler();
                }
            }

            // In HIL/VSIM-mode check if console connection is up -> then assume ATP/Dispatcher is up.
            if (SimulationSil == SimMode)
            {

                // Check ATP process
                if (atpProcess != nullptr)
                {
                    if (atpProcess->HasExited)
                    {
                        atpARecStrings[atpARecStringCnt++] = "[RED]" + DateTime::Now.ToString("HH:mm:ss.fff") + " PROCESS EXITED";
                        atpProcess = nullptr;
                    }
                }
                atpARunning = atpProcess != nullptr;
                aosRunning = atpARunning;

                // Check ATP2 process
                if (atpBProcess != nullptr)
                {
                    if (atpBProcess->HasExited)
                    {
                        atpBRecStrings[atpBRecStringCnt++] = "[RED]" + DateTime::Now.ToString("HH:mm:ss.fff") + " PROCESS EXITED";
                        atpBProcess = nullptr;
                    }
                }
                atpBRunning = atpBProcess != nullptr;

                // Check ATO process
                if (atoProcess != nullptr)
                {
                    if (atoProcess->HasExited)
                    {
                        atoRecStrings[atoRecStringCnt++] = "[RED]" + DateTime::Now.ToString("HH:mm:ss.fff") + " PROCESS EXITED";
                        atoProcess = nullptr;
                    }
                }
                atoRunning = atoProcess != nullptr;
            }
            else
            {
                atpARunning = ATPAConsoleConnection->isConnected();
                atpBRunning = ATPBConsoleConnection->isConnected();
                dispRunning = DispConsoleConnection->isConnected();
                atoRunning = true;

                aosRunning = (atpARunning && atpBRunning && dispRunning);
            }

        }

        /******************************************************************************
        * Function:     StartAT__
        * Description:  
        ******************************************************************************/
    public:
        bool StartATP(String^ atp1File, String^ atp1Args)
        {
            // ATP
            if (atpProcess == nullptr)
            {
                try
                {
                    // Setup process
                    ProcessStartInfo^ startInfo         = gcnew ProcessStartInfo();
                    startInfo->FileName                 = atp1File;
                    startInfo->Arguments                = atp1Args;
                    startInfo->WorkingDirectory         = atp1File->Substring(0, atp1File->LastIndexOf("\\"));
                    startInfo->CreateNoWindow           = true;
                    startInfo->RedirectStandardOutput   = true;
                    startInfo->RedirectStandardError    = true;
                    startInfo->RedirectStandardInput    = true;
                    startInfo->UseShellExecute          = false;
                    startInfo->ErrorDialog              = false;

                    atpProcess                       = gcnew Process;
                    atpProcess->StartInfo            = startInfo;
                    atpProcess->Exited              += gcnew EventHandler(atpAExitHandler);

                    // Start process
                    atpProcess->Start();

                    // Initialise capture of output strings
                    atpProcess->BeginOutputReadLine();

                    atpARecStrings[atpARecStringCnt++] = "[RED]" + DateTime::Now.ToString("HH:mm:ss.fff") + " PROCESS STARTED";
                }
                catch (...)
                {
                    atpProcess = nullptr;
                }
            }
            return (atpProcess != nullptr);
        }
 
    public:

        // TODO: StartATO method similar to StartATP
      

        /******************************************************************************
        * Function:     Stop
        * Description:  
        ******************************************************************************/
    public:
        void Stop(void)
        {
            // ATP
            if (atpProcess != nullptr)
            {
                try
                {
                    atpProcess->Kill();
                    atpARecStrings[atpARecStringCnt++] = "[RED]" + DateTime::Now.ToString("HH:mm:ss.fff") + " PROCESS KILLED";
                }
                catch(...)
                {
                    atpARecStrings[atpARecStringCnt++] = "[RED]" + DateTime::Now.ToString("HH:mm:ss.fff") + " PROCESS KILL FAILED";
                }
                atpProcess = nullptr;
            }
            
            // TODO: ATO handling
        }
        private:
            void atpAOutputHandler()
            {

                ATPAConsoleConnection->Connect(
                    SimulationSil == SimMode ? IPAddress::Parse("127.0.0.1") : IPAddress::Parse(ATPAIP),
                    SimulationSil == SimMode ? ATPConsolePortToConnect : ATPAConsolePortToConnect);

                // Reads any potential data
                array<unsigned char>^ tmpRecArray = gcnew array<unsigned char>(32768);
                int cnt = ATPAConsoleConnection->ReadData(tmpRecArray);

                // Add the text to the collected output.
                if (cnt > 0)
                {
                    // Convert the received array to a String and skip the \0:s at the end.
                    String^ tmpRecString(System::Text::UTF8Encoding::UTF8->GetString(tmpRecArray));
                    tmpRecString = tmpRecString->TrimEnd('\0');

                    atpARecStringCnt++;
                    atpARecStrings = tmpRecString->Split('\n');

                    // Count added rows
                    int i = 0;
                    for (i = 0; i < cnt; i++)
                    {
                        if ('\n' == tmpRecArray[i])
                        {
                            atpARecStringCnt++;
                        }
                    }

                    // Insert Time and Date
                    for (i = 0; i < atpARecStringCnt; i++)
                    {
                        atpARecStrings[i] = String::Concat(DateTime::Now.ToString("HH:mm:ss.fff" + " "), atpARecStrings[i]);
                    }
                }
            }
        private:
            void atpBOutputHandler()
            {
                ATPBConsoleConnection->Connect(IPAddress::Parse(ATPBIP), ATPBConsolePortToConnect);

                // Reads any potential data
                array<unsigned char>^ tmpRecArray = gcnew array<unsigned char>(32768);
                int cnt = ATPBConsoleConnection->ReadData(tmpRecArray);

                // Add the text to the collected output.
                if (cnt > 0)
                {
                    // Convert the received array to a String and skip the \0:s at the end.
                    String^ tmpRecString(System::Text::UTF8Encoding::UTF8->GetString(tmpRecArray));
                    tmpRecString = tmpRecString->TrimEnd('\0');
                    
                    atpBRecStringCnt++;
                    atpBRecStrings = tmpRecString->Split('\n');

                    // Count added rows
                    int i = 0;
                    for (i = 0; i < cnt; i++)
                    {
                        if ('\n' == tmpRecArray[i])
                        {
                            atpBRecStringCnt++;
                        }
                    }

                    // Insert Time and Date
                    for (i = 0; i < atpBRecStringCnt; i++)
                    {
                        atpBRecStrings[i] = String::Concat(DateTime::Now.ToString("HH:mm:ss.fff" + " "), atpBRecStrings[i]);
                    }
                }
            }

            private:
                void dispOutputHandler()
                {
                    DispConsoleConnection->Connect(IPAddress::Parse(DISPIP), DispConsolePortToConnect);

                    // Reads any potential data
                    array<unsigned char>^ tmpRecArray = gcnew array<unsigned char>(32768);
                    int cnt = DispConsoleConnection->ReadData(tmpRecArray);

                    // Add the text to the collected output.
                    if (cnt > 0)
                    {
                        // Convert the received array to a String and skip the \0:s at the end.
                        String^ tmpRecString(System::Text::UTF8Encoding::UTF8->GetString(tmpRecArray));
                        tmpRecString = tmpRecString->TrimEnd('\0');

                        dispRecStringCnt++;
                        dispRecStrings = tmpRecString->Split('\n');

                        // Count added rows
                        int i = 0;
                        for (i = 0; i < cnt; i++)
                        {
                            if ('\n' == tmpRecArray[i])
                            {
                                dispRecStringCnt++;
                            }
                        }

                        // Insert Time and Date
                        for (i = 0; i < dispRecStringCnt; i++)
                        {
                            dispRecStrings[i] = String::Concat(DateTime::Now.ToString("HH:mm:ss.fff" + " "), dispRecStrings[i]);
                        }
                    }
                }

            // TODO: atoOutputHandler


        /******************************************************************************
        * Function:     at__ExitHandler
        * Description:  Handle if process exits
        ******************************************************************************/
    private: 
        static void atpAExitHandler(Object^ sender, EventArgs^ e)
        {
            atpProcess = nullptr;
        }
    private: 
        static void atpBExitHandler(Object^ sender, EventArgs^ e)
        {
            atpBProcess = nullptr;
        }
    private: 
        static void atoExitHandler(Object^ sender, EventArgs^ e)
        {
            atoProcess = nullptr;
        }


        /******************************************************************************
        * Function:     Destructor
        * Description:  
        ******************************************************************************/
    protected:
        // Clean up any resources being used.
        ~RunAOSClass()
        {
        }
    };
}
