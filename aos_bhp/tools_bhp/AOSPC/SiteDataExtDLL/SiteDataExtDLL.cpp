#pragma once
#pragma ident "@(#) Bombardier Transportation %full_filespec:  SiteDataExtDLL.cpp-1:c++:arn_006#1 %"

#include "stdafx.h"
#include <math.h>
#include "SiteDataExtDLL.h"
#include <functional>
#include <algorithm>
#include <iterator>

/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2011
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          SiteDataExtDLL.cpp %
*
*  %version:       1 %
*
*  %created_by:    bhidaji %
*
*  %date_created:  2014-06-26 10:09 %
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
* 2014-06-13    Hidaji      File created
* 2014-12-16    Antbäck     Added use of trackId from XML file
*
*******************************************************************************/


/******************************************************************************
* Function:     compPantoShiftList
* Description:  compares two PantoShiftListItem and returns -1 if lhs is smaller 
*               than rhs, 0 if they are equal, and 1 if it is greater 
******************************************************************************/
int SiteDataExtDLL::SiteDataExt::compPantoShiftList(const PantoShiftListItem^ lhs, const PantoShiftListItem^ rhs)
{
    int m = String::Compare(lhs->TrackName, rhs->TrackName);
    if (0 == m)
    {
        if(lhs->position < rhs->position)
            m = -1;
        else if(lhs->position == rhs->position)
            m = 0;
        else
            m = 1;
    }

    return m;
}

/******************************************************************************
* Function:     compPantoShiftList
* Description:  compares two PantoZoneListItem and returns -1 if lhs is smaller 
*               than rhs, 0 if they are equal, and 1 if it is greater 
*                
******************************************************************************/
int SiteDataExtDLL::SiteDataExt::compPantoZoneList(const PantoZoneListItem^ lhs, const PantoZoneListItem^ rhs)
{
    int m = String::Compare(lhs->TrackName, rhs->TrackName);
    if (0 == m)
    {
        if(lhs->start->Equals("*"))
            m = -1;
        else
            m = String::Compare(lhs->start, rhs->start);
    }
    return m;
}

/******************************************************************************
* Function:     LoadSiteDataFile
* Description:  This function extract balise data, panto shifts, and track lists
*               from the input site data and writes a list of them in a proper 
*               format to be used by AOS PC 
******************************************************************************/
bool SiteDataExtDLL::SiteDataExt::LoadSiteDataFile(String^ inputFile, String ^outputFile)
{
    int trkCnt = 0;
    int balCnt = 0;
    int psCnt = 0;
    int pzCnt = 0;
    String ^ StartPoint = "*";

    XmlDocument ^ docSiteData = gcnew XmlDocument;
    StreamWriter^ outFile = gcnew StreamWriter(outputFile);

    // Check if file exists
    if (!File::Exists(inputFile))
    {
        LoadResult = "File \"" + inputFile + "\" does not exist";
        return false;
    }

    // Open file
    try
    {
        docSiteData->Load(inputFile);
        extractedFileName = IO::Path::GetFileName(inputFile);
    }
    catch (...)
    {
        LoadResult = "Error when trying to load xml file";
        return false;
    }

    outFile->WriteLine("filever 1");
    outFile->WriteLine("comment This file is created on " + DateTime::Now + " based on site data file: " + extractedFileName);

    try
    {
        siteDataNameSA = docSiteData->DocumentElement->SelectSingleNode("SpecificApplication/Name")->FirstChild->Value;
        siteDataProductIdSA = docSiteData->DocumentElement->SelectSingleNode("SpecificApplication/ProductId")->FirstChild->Value;
        siteDataVersionSA = docSiteData->DocumentElement->SelectSingleNode("SpecificApplication/Version")->FirstChild->Value;
        siteDataNameGA = docSiteData->DocumentElement->SelectSingleNode("GenericApplication/Name")->FirstChild->Value;
        siteDataProductIdGA = docSiteData->DocumentElement->SelectSingleNode("GenericApplication/ProductId")->FirstChild->Value;
        siteDataVersionGA = docSiteData->DocumentElement->SelectSingleNode("GenericApplication/Version")->FirstChild->Value;
    }
    catch (...)
    {
        LoadResult = "Error when trying to load SA, GA product information";
        return false;
    }

    outFile->WriteLine("comment Specific Application; Name:" + siteDataNameSA +  " Product ID:" + siteDataProductIdSA + " Version:" + siteDataVersionSA);    
    outFile->WriteLine("comment Generic Application; Name:" + siteDataNameGA +  " Product ID:" + siteDataProductIdGA + " Version:" + siteDataVersionGA);

    for each (XmlNode ^ node in docSiteData->DocumentElement->SelectNodes("Object"))
    {
        //Find tracks
        if(node->SelectSingleNode("ObjectType")->FirstChild->Value->Equals("NormalTrack"))
        {
            int sdTrackId = 0;

            TrackList[trkCnt]->Name = node->SelectSingleNode("ObjectName")->FirstChild->Value;

            for each (XmlNode ^ node1 in node->SelectNodes("Parameter"))
            {
                if(node1->SelectSingleNode("Name")->FirstChild->Value->Equals("intersection0"))
                {
                    TrackList[trkCnt]->int0 = node1->SelectSingleNode("Value/ReferenceValue/ObjectName")->FirstChild->Value;
                }
                if(node1->SelectSingleNode("Name")->FirstChild->Value->Equals("intersection1"))
                {
                    TrackList[trkCnt]->int1 = node1->SelectSingleNode("Value/ReferenceValue/ObjectName")->FirstChild->Value;
                }
                if(node1->SelectSingleNode("Name")->FirstChild->Value->Equals("length"))
                {
                    double decimal, value;
                    decimal = Convert::ToInt32(node1->SelectSingleNode("Value/NumberValue/Decimals")->FirstChild->Value);
                    value = Convert::ToInt32(node1->SelectSingleNode("Value/NumberValue/Value")->FirstChild->Value);
                    TrackList[trkCnt]->Length = (int)(value * pow(10,decimal));
                }
                if(node1->SelectSingleNode("Name")->FirstChild->Value->Equals("trackId"))
                {
                    sdTrackId = Convert::ToInt16(BaliseList[balCnt]->identity = node1->SelectSingleNode("Value/UnsignedShortValue")->FirstChild->Value);
                }
            }

            // Choose SiteData trackId before accumulating number of tracks as identity
            TrackCount = trkCnt + 1; 
            if (sdTrackId > 0)
            {
                outFile->WriteLine("track " + TrackList[trkCnt]->Name + " "  +  sdTrackId + " " + TrackList[trkCnt]->Length);
            }
            else
            {
                outFile->WriteLine("track " + TrackList[trkCnt]->Name + " "  +  TrackCount + " " + TrackList[trkCnt]->Length);
            }
            trkCnt++;
        }

        //Find Balises
        if(node->SelectSingleNode("ObjectType")->FirstChild->Value->Equals("EuroBaliseGroup"))
        {
            BaliseList[balCnt]->Name = node->SelectSingleNode("ObjectName")->FirstChild->Value;
            for each (XmlNode ^ node1 in node->SelectNodes("Parameter"))
            {
                if(node1->SelectSingleNode("Name")->FirstChild->Value->Equals("identity"))
                {
                    BaliseList[balCnt]->identity = node1->SelectSingleNode("Value/UnsignedShortValue")->FirstChild->Value;
                }
                if(node1->SelectSingleNode("Name")->FirstChild->Value->Equals("position"))
                {
                    double decimal, value;
                    decimal = Convert::ToInt32(node1->SelectSingleNode("Value/ObjectValue/Parameter/Value/NumberValue/Decimals")->FirstChild->Value);
                    value = Convert::ToInt32(node1->SelectSingleNode("Value/ObjectValue/Parameter/Value/NumberValue/Value")->FirstChild->Value);
                    BaliseList[balCnt]->position = (int)(value * pow(10,decimal));
                    BaliseList[balCnt]->TrackName = node1->SelectSingleNode("Value/ObjectValue/Parameter/Value/ReferenceValue/ObjectName")->FirstChild->Value;
                }
            }
            outFile->WriteLine("balise " + BaliseList[balCnt]->Name + " " + BaliseList[balCnt]->identity + " " + BaliseList[balCnt]->TrackName + " " + BaliseList[balCnt]->position);
            balCnt++;
            BaliseCount = balCnt;
        }

        //Find panto shift
        if((node->SelectSingleNode("ObjectType")->FirstChild->Value->Equals("PantographShift")) 
            // temporary pantograph shifts are not used
            /*|| ((node->SelectSingleNode("ObjectType")->FirstChild->Value->Equals("TemporaryPantographShift")))*/)
        {
            PantoShiftList[psCnt]->PantoName = node->SelectSingleNode("ObjectName")->FirstChild->Value;

            for each (XmlNode ^ node1 in node->SelectNodes("Parameter"))
            {
                if(node1->SelectSingleNode("Name")->FirstChild->Value->Equals("pantographPositionNominal"))
                {
                    PantoShiftList[psCnt]->PantoPosNom = node1->SelectSingleNode("Value/EnumValue")->FirstChild->Value;
                }
                else if(node1->SelectSingleNode("Name")->FirstChild->Value->Equals("lTrackCond"))
                {
                    PantoShiftList[psCnt]->ITrackCond = 100 * Convert::ToInt32(node1->SelectSingleNode("Value/UnsignedShortValue")->FirstChild->Value);
                }
                else if(node1->SelectSingleNode("Name")->FirstChild->Value->Equals("direction"))
                {
                    PantoShiftList[psCnt]->direction = node1->SelectSingleNode("Value/EnumValue")->FirstChild->Value;
                }
                else if(node1->SelectSingleNode("Name")->FirstChild->Value->Equals("pantographPositionReverse"))
                {
                    PantoShiftList[psCnt]->PantoPosRev = node1->SelectSingleNode("Value/EnumValue")->FirstChild->Value;
                }
                else if(node1->SelectSingleNode("Name")->FirstChild->Value->Equals("position"))
                {
                    double decimal, value;
                    decimal = Convert::ToInt32(node1->SelectSingleNode("Value/ObjectValue/Parameter/Value/NumberValue/Decimals")->FirstChild->Value);
                    value = Convert::ToInt32(node1->SelectSingleNode("Value/ObjectValue/Parameter/Value/NumberValue/Value")->FirstChild->Value);
                    PantoShiftList[psCnt]->position = (int)(value * pow(10,decimal));
                    PantoShiftList[psCnt]->TrackName = node1->SelectSingleNode("Value/ObjectValue/Parameter/Value/ReferenceValue/ObjectName")->FirstChild->Value; 
                }
            }
            //outFile->WriteLine("panto " + PantoShiftList[psCnt]->PantoName + " " + pantoPosNominal + " " + pantoPosReverse + " " + PantoShiftList[psCnt]->TrackName 
            //    + " " + PantoShiftList[psCnt]->position);

            // Convert negative panto shifts
            if(PantoShiftList[psCnt]->direction->Equals("negative"))
            {
                String ^tmp = PantoShiftList[psCnt]->PantoPosRev;
                PantoShiftList[psCnt]->PantoPosRev = PantoShiftList[psCnt]->PantoPosNom;
                PantoShiftList[psCnt]->PantoPosNom = tmp;
                PantoShiftList[psCnt]->positionITrackCond = PantoShiftList[psCnt]->position;
                PantoShiftList[psCnt]->position = PantoShiftList[psCnt]->position - PantoShiftList[psCnt]->ITrackCond;
                if( 0 > PantoShiftList[psCnt]->positionITrackCond) {PantoShiftList[psCnt]->positionITrackCond = 0;}  
            }

            psCnt++;
            PantoShiftCount = psCnt;
        }

    }

    // Sort the pantoshift array
    for(int i = 0; i< PantoShiftCount; i++)
    {
        bool changed = false;      
        for (int j = 0; j< (PantoShiftCount - 1) ; j++)
        {
            if(1 == compPantoShiftList(PantoShiftList[j],PantoShiftList[j+1]))
            {
                PantoShiftListItem ^tmp = gcnew PantoShiftListItem; 
                tmp = PantoShiftList[j+1];
                PantoShiftList[j+1] = PantoShiftList[j];
                PantoShiftList[j] = tmp;
                changed = true;     
            }
        }
        if(false == changed)
        {
            break;
        }
    }

    // The list of pantograph shifts is sorted by the track name (panto shifts with the same track has to be adjutant for this code to work) 
    // and also sorted by the position of the panto shift
    // Update the track list with pantograph shift positions and also output the panto zones to the output file
    for (int i = 0; i < PantoShiftCount; i++)
    {
        if(!PantoShiftList[i]->direction->Equals("negative"))
        {
            PantoShiftList[i]->positionITrackCond = PantoShiftList[i]->position + PantoShiftList[i]->ITrackCond;
            for(int j = 0; j < TrackCount; j++)
            {
                if(TrackList[j]->Name->Equals(PantoShiftList[i]->TrackName))
                {
                    if(PantoShiftList[i]->positionITrackCond > TrackList[j]->Length)
                    {
                        PantoShiftList[i]->positionITrackCond = TrackList[j]->Length;
                    }
                    break;
                }
            }
        }
    }    

    for (int i = 0; i < PantoShiftCount; i++)
    {
        // If the next pantograph shift belongs to a different track
        if(!PantoShiftList[i]->TrackName->Equals(PantoShiftList[i+1]->TrackName))
        {
            // The track will be devided to two zones 
            // 1: From the start point to the panto shift location with reverse panto pos, Note that if the previous panto shift belonged the same track
            //    the starting point must be from the previous panto shift location
            // 2: From the panto shift location all the way to the end of the track because next panto shift belongs to another track 
                PantoZoneList[pzCnt]->TrackName = PantoShiftList[i]->TrackName;
                PantoZoneList[pzCnt]->start = StartPoint;
                PantoZoneList[pzCnt]->end = PantoShiftList[i]->positionITrackCond.ToString();
                PantoZoneList[pzCnt]->PantoPos = PantoShiftList[i]->PantoPosRev;
                pzCnt++;

                PantoZoneList[pzCnt]->TrackName = PantoShiftList[i]->TrackName;
                PantoZoneList[pzCnt]->start = PantoShiftList[i]->position.ToString();
                PantoZoneList[pzCnt]->end = "*";
                PantoZoneList[pzCnt]->PantoPos = PantoShiftList[i]->PantoPosNom;
                pzCnt++;
     
            // Now the track list shall be udated based on panto shifts, Intersection 1 will be updated because this is the last panto shift in this track
            // if the startpoint is zero, the intersection 0 is updated as well.
            for (int j = 0; j < TrackCount; j++)
            {
                if(TrackList[j]->Name->Equals(PantoShiftList[i]->TrackName))
                {
                    // Update intersection 0 
                    if(StartPoint->Equals("*"))  
                    {
                        TrackList[j]->int0PantoPos = PantoShiftList[i]->PantoPosRev;
                    }

                    // Update intersection 1 
                    TrackList[j]->int1PantoPos = PantoShiftList[i]->PantoPosNom;
                    break;
                }
            }

            // Set Start point to zero since next panto shift will belong to a different track
            StartPoint = "*"; 
        }
        else
        {
            // Next panto shift belongs to the same track
            // From the start point to the panto shift location with reverse panto pos, Note that if the previous panto shift belonged the same track
            // the starting point must be from the previous panto shift location
            PantoZoneList[pzCnt]->TrackName = PantoShiftList[i]->TrackName;
            PantoZoneList[pzCnt]->start = StartPoint;
            PantoZoneList[pzCnt]->end = PantoShiftList[i]->positionITrackCond.ToString();
            PantoZoneList[pzCnt]->PantoPos = PantoShiftList[i]->PantoPosRev;    
            //outFile->WriteLine("panto " + PantoShiftList[i]->TrackName + StartPoint + PantoZoneList[pzCnt]->end + "  " + PantoShiftList[i]->PantoPosRev);
            pzCnt++;
            
            // Update the track if the start point is zero
            if(StartPoint->Equals("*"))
            {
                for (int j = 0; j < TrackCount; j++)
                {
                    if(TrackList[j]->Name->Equals(PantoShiftList[i]->TrackName))
                    {
                        TrackList[j]->int0PantoPos = PantoShiftList[i]->PantoPosRev;
                        break; 
                    }
                }

                // Update start position for the next panto shift zone
                StartPoint = PantoShiftList[i]->position.ToString(); 
            }
        }
    }

    // In this loop we iterate trough the track list and update ant track that was not mentioned in the pantograph shift list
    // if a track's panto position is not defined yet, this code examines the adjustant track (tracks that share the same point and if
    // they have a defined panto position the panto position for this track is updated as well, 
    // NB that a track that is not in the panto shift list has the same panto position in intersection 0 and 1
    int loopCnt = 0;
    while(true)
    {
        bool changed = false;
        for (int j = 0; j < TrackCount; j++)
        {
            //If not updated yet
            if(TrackList[j]->int0PantoPos->Equals(""))
            {
                for (int k = 0; k < TrackCount; k++)
                {
                    if((TrackList[j]->int0->Equals(TrackList[k]->int1)) && (!TrackList[k]->int1PantoPos->Equals("")))
                    {
                        TrackList[j]->int0PantoPos = TrackList[k]->int1PantoPos;
                        TrackList[j]->int1PantoPos = TrackList[k]->int1PantoPos;
                        PantoZoneList[pzCnt]->TrackName = TrackList[j]->Name;
                        PantoZoneList[pzCnt]->start = "*";
                        PantoZoneList[pzCnt]->end = "*";
                        PantoZoneList[pzCnt]->PantoPos = TrackList[j]->int0PantoPos;
                        pzCnt++;
                        changed = true;
                        break;
                    }
                    if((TrackList[j]->int1->Equals(TrackList[k]->int0)) && (!TrackList[k]->int0PantoPos->Equals("")))
                    {
                        TrackList[j]->int1PantoPos = TrackList[k]->int0PantoPos;
                        TrackList[j]->int0PantoPos = TrackList[k]->int0PantoPos;
                        PantoZoneList[pzCnt]->TrackName = TrackList[j]->Name;
                        PantoZoneList[pzCnt]->start = "*";
                        PantoZoneList[pzCnt]->end = "*";
                        PantoZoneList[pzCnt]->PantoPos = TrackList[j]->int0PantoPos;
                        pzCnt++;
                        changed = true;
                        break;
                    }
                }
            }
        }

        loopCnt++;

        // Nothing has changed in this loop
        if(false == changed)  
        { 
            break; 
        }

        if(TrackCount < loopCnt)
        {
            LoadResult = String::Concat("Too many itterations of the loop");
            return false;
        }
    }

    PantoZoneCount = pzCnt;

    //Sort panto zone array and put it in the file
    // Sort the panto zone array
    for(int i = 0; i< PantoZoneCount; i++)
    {
        bool changed = false;      
        for (int j = 0; j< (PantoZoneCount - 1) ; j++)
        {
            if(1 == compPantoZoneList(PantoZoneList[j],PantoZoneList[j+1]))
            {
                PantoZoneListItem ^tmp = gcnew PantoZoneListItem; 
                tmp = PantoZoneList[j+1];
                PantoZoneList[j+1] = PantoZoneList[j];
                PantoZoneList[j] = tmp;
                changed = true;     
            }
        }
        if(false == changed)
        {
            break;
        }
    }

    // write the panto zone list to the output file
    for(int i = 0; i< PantoZoneCount; i++)
    {
        if(!PantoZoneList[i]->PantoPos->Equals("None"))
            outFile->WriteLine("panto " + PantoZoneList[i]->TrackName + " " + PantoZoneList[i]->start + " " + PantoZoneList[i]->end + " " + PantoZoneList[i]->PantoPos);
    }
    
    outFile->Close();
    return true;
}
