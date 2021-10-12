#pragma once
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          SiteDataExtDLL.h %
*
*  %version:       1 %
*
*  %created_by:    bhermans %
*
*  %date_created:  2016-09-16 16:19 %
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
* 2013-06-13    Hidaji      File created
*
*******************************************************************************/

#include <windows.h> 
#include <tchar.h>
#include <stdio.h> 

using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Data;
using namespace System::Net;
using namespace System::Net::Sockets;
using namespace System::Text;
using namespace System::IO;
using namespace System::Runtime::InteropServices;
using namespace System::Reflection;
using namespace System::Xml;

namespace SiteDataExtDLL {

    public ref class PantoShiftListItem
    {
    public:
        String^             PantoName;
        String^             TrackName;
        int                 position;
        String^             PantoPosNom;
        String^             PantoPosRev;
        int                 ITrackCond; 
        String^             direction;
        int                 positionITrackCond;
    };

    public ref class PantoZoneListItem
    {
    public:
        String^             TrackName;
        String^             start;
        String^             end;
        String^             PantoPos;
    };

    public ref class TrackListItem
    {
    public:
        String^             Name;
        int                 Length; 

        String^             int0;   // intersection0
        String^             int1;   // intersection1
        String^             int0PantoPos;
        String^             int1PantoPos;
    };

    public ref class BaliseListItem
    {
    public:
        String^             Name;
        String^             identity;
        String^             TrackName;
        int                 position;     
    };

    public ref class SiteDataExt
    {
    public:
        String ^ DLLVersion;
        String ^ LoadResult;
        String ^ extractedFileName;
        String ^ siteDataNameSA;
        String ^ siteDataProductIdSA;
        String ^ siteDataVersionSA;
        String ^ siteDataNameGA;
        String ^ siteDataProductIdGA;
        String ^ siteDataVersionGA;
        String ^ siteDataFileFromIni;
        String ^ outputFileFromIni;

        int                         MaxBalItems;
        int                         MaxTrkItems;
        int                         MaxPantoShiftItems;
        int                         MaxPantoZoneItems;
        int                         TrackCount;
        int                         BaliseCount;
        int                         PantoShiftCount;
        int                         PantoZoneCount;

        array<TrackListItem^>^      TrackList;
        array<BaliseListItem^>^     BaliseList;
        array<PantoShiftListItem^>^ PantoShiftList;
        array<PantoZoneListItem^>^  PantoZoneList;

        // Constructor
        // ===========
    public:
        SiteDataExt(String^ IniFile)
        {
            iniFile = String::Copy(IniFile);

            MaxBalItems = 1000;
            MaxTrkItems = 1000;
            MaxPantoShiftItems = 1000;
            MaxPantoZoneItems = 1000;

            TrackCount = 0;
            BaliseCount = 0;
            TrackList = gcnew array<TrackListItem^>(MaxTrkItems);
            BaliseList = gcnew array<BaliseListItem^>(MaxPantoShiftItems);
            PantoShiftList = gcnew array<PantoShiftListItem^>(MaxPantoShiftItems);
            PantoZoneList = gcnew array<PantoZoneListItem^>(MaxPantoZoneItems);



            for (int i = 0; i < MaxTrkItems; i++)
            {
                TrackList[i] = gcnew TrackListItem;
                TrackList[i]->Name          = "";
                TrackList[i]->Length        = 0;

                TrackList[i]->int0          = "";
                TrackList[i]->int1          = "";
                TrackList[i]->int0PantoPos  = ""; 
                TrackList[i]->int1PantoPos  = "";  
            }

            for (int i = 0; i < MaxBalItems; i++)
            {
                BaliseList[i] = gcnew BaliseListItem;
                BaliseList[i]->identity      = "";
                BaliseList[i]->Name          = "";
                BaliseList[i]->TrackName     = "";
                BaliseList[i]->position      = 0;
            }

            for (int i = 0; i < MaxPantoShiftItems; i++)
            {
                PantoShiftList[i] = gcnew PantoShiftListItem;
                PantoShiftList[i]->PantoName       = "";
                PantoShiftList[i]->TrackName       = "";
                PantoShiftList[i]->position        = 0;
                PantoShiftList[i]->PantoPosNom     = "";
                PantoShiftList[i]->PantoPosRev     = "";
            }

            for (int i = 0; i < MaxPantoZoneItems; i++)
            {
                PantoZoneList[i] = gcnew PantoZoneListItem;
                PantoZoneList[i]->TrackName       = "";
                PantoZoneList[i]->start           = "";
                PantoZoneList[i]->end             = "";
                PantoZoneList[i]->PantoPos        = "";
            }


            // Read data from IniFile
            // Create temporary file name
            char tmpStr[200];
            char *tmpIniFile = (char *) Marshal::StringToHGlobalAnsi(iniFile).ToPointer();
            GetPrivateProfileStringA("SiteDataExt", "SiteDataFile", "", tmpStr, sizeof(tmpStr), tmpIniFile);
            siteDataFileFromIni = gcnew String(tmpStr);

            GetPrivateProfileStringA("SiteDataExt", "OutputFile", "", tmpStr, sizeof(tmpStr), tmpIniFile);
            outputFileFromIni = gcnew String(tmpStr);

            // Free temporary buffer again
            Marshal::FreeHGlobal(IntPtr(tmpIniFile));

            // Get DLL version
            DLLVersion = " - v" +
                Assembly::GetExecutingAssembly()->GetName()->Version->Major.ToString() + "." + 
                Assembly::GetExecutingAssembly()->GetName()->Version->Minor.ToString() + "." + 
                Assembly::GetExecutingAssembly()->GetName()->Version->Build.ToString();

        };

        bool LoadSiteDataFile(String^ inputFile, String ^outputFile);
        int  compPantoShiftList(const PantoShiftListItem^ lhs, const PantoShiftListItem^ rhs);
        int  compPantoZoneList(const PantoZoneListItem^ lhs, const PantoZoneListItem^ rhs);
       

        // Destructor
        // ==========
    public:
        ~SiteDataExt()
        {
        }

    private:
        String^     iniFile;
    };
}
