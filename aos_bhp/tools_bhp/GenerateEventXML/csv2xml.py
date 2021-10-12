#*******************************************************************************
#
# (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
# 
# We reserve all rights in this file and in the information 
# contained therein. Reproduction, use or disclosure to third 
# parties without written authority is strictly forbidden.
#
#  DESCRIPTION:
#  Command that reads a CSV-file and generates an XML-file according to template.py.
#  Filenames are specified as parameters to command.
#
#******************************************************************************

#******************************************************************************
#
# REVISION HISTORY :
#
# Date          Name        Changes
# ---------------------------------------------------------------------------
# 2018-08-02    marlundg    Created
#
#******************************************************************************

#******************************************************************************
# INCLUDE FILES
#******************************************************************************
import re
import csv
import sys
from template import *
from enum import Enum

#******************************************************************************
# DECLARATIONS
#******************************************************************************
class EventType(Enum):
    EventTypeLog = '0'
    EventTypeSBReq = '1'
    EventTypeEBReq = '2'
    EventTypeStandstill = '3'
    EventTypeSafeBrakeSB = '4'
    EventTypeSafeBrakeEB = '5'
    EventTypeSafetyHalt = '6'

class LogLevel(Enum):
    debug = "debug"
    info = "info"
    warning = "warning"
    error = "error"
    systemFailure = "sytemFailure"

#******************************************************************************
# Maps the event-type to a log-level string
#
# @param[in]    eventTypeIndex - Event-type to be mapped to log-level
# @return       Log-level string
#
#******************************************************************************
def mapLogLevel(eventTypeIndex):
    et = EventType(eventTypeIndex)
    if et == EventType.EventTypeLog:
        return LogLevel.info
    elif et == EventType.EventTypeSBReq:
        return LogLevel.warning
    elif et == EventType.EventTypeEBReq:
        return LogLevel.warning
    elif et == EventType.EventTypeStandstill:
        return LogLevel.info
    elif et == EventType.EventTypeSafeBrakeSB:
        return LogLevel.error
    elif et == EventType.EventTypeSafeBrakeEB:
        return LogLevel.error
    elif et == EventType.EventTypeSafetyHalt:
        return LogLevel.systemFailure

#******************************************************************************
# Reads a csv-file and return specific columns as a list of tuples.
#
# @param[in]    filename - Filename of CSV-file (relative path from command execution)
# @return       Events as a list of tuples.
#
#******************************************************************************
def readCSV(filename):
    with open(filename,'rt') as csvf:
        creader = csv.reader(csvf, delimiter=';')
        rows = []
        for row in creader:
            rows.append((row[0],row[4],row[5],row[6]))
    
        return rows
    
#******************************************************************************
# Check for duplicated IDs
#
# @param[in]    rows - List of event tuples.
# @return       A set of duplicated IDs
#
#******************************************************************************
def propUniqueId(rows):
    seen = []
    err = []
    succ = True
    for row in rows:
        if row[0] in seen:
            succ &= False 
            err.append(str(row[0]))
        else:
            seen.append(row[0])
    return (succ,set(err))

#******************************************************************************
# Convert an event to XML format
#
# @param[in]    tup - An event tuple to convert one event into XML.
# @return       An XML-string according to template in template.py
#
#******************************************************************************
def toXML(tup):
    id,severity,message,isDynamicText = tup
    dynamicXML = ""
    dynamicVar = ""
    dynamicXMLFormated = ""
    if isDynamicText == '1' :
        dynamicXML = templateDynamicXML
        dynamicVar = "%dynamic_text%"
        
        dynamicXMLFormated = dynamicXML.format(description = message, dynamicVar = dynamicVar)
        
    return templateXML.format(id=id,
                            isDynamic=isDynamicText,
                            description = message,
                            severity = severity,
                            ifDynamicText = dynamicXMLFormated)

#******************************************************************************
# main function
#
#******************************************************************************
def main():
    if len(sys.argv) != 3:
        print("Usage: python " + sys.argv[0] + " infile outfile")
        sys.exit(1)

    inputf  = sys.argv[1]
    outputf = sys.argv[2]

    if not(outputf.endswith('.xml')):
        outputf += '.xml'

    rows = readCSV(inputf)

    # If any duplicate rows exists -> Remove
    rows = list(set(rows))
 
    unqID, dupIdList = propUniqueId(rows)

    # Duplicate ID:s still exists -> Issue a warning
    with open(outputf + '.ErrorLog', "w") as text_file:
        print("Error Report:\n-------------\n", file=text_file)

    if not(unqID):
        with open(outputf + '.ErrorLog', "w") as text_file:
            print("Warning, duplicate ids detected:", file=text_file)
            for r in dupIdList:
                print("{}".format([item for item in rows if item[0] == r]), file=text_file)
    else:
        with open(outputf + '.ErrorLog', "a") as text_file:
            print("No errors found for event-generation.\n", file=text_file)

        rows= list(map(
            lambda t : (t[0],mapLogLevel(t[1]).name,t[2],t[3]),
                rows))
        rows_sorted = sorted(rows, key = lambda t: int(t[0]))

        body =""

        for row in rows_sorted:
            body += toXML(row)

        # Remove all empty lines
        strippedBody = ""
        strippedBody = re.sub('\n\s+\n', '\n', body)

        with open(outputf,'wt') as xf:
            xf.write(templateBodyXML.format(body=strippedBody))

    if (unqID):
        sys.exit(0)
    else: 
        sys.exit(2)

main()


