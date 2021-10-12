#*******************************************************************************
#
# (C) COPYRIGHT Bombardier Transportation Sweden AB, 2019
# 
# We reserve all rights in this file and in the information
# contained therein. Reproduction, use or disclosure to third 
# parties without written authority is strictly forbidden.
#
#  DESCRIPTION:
#  Command that reads a CSV-file and generates AOSEvents.xlsx which contains infromation about all the events
#  reported by AOS.
#
#******************************************************************************

#******************************************************************************
#
# REVISION HISTORY :
#
# Date          Name        Changes
# ---------------------------------------------------------------------------
# 2019-05-07    nsyed    Created
#
#******************************************************************************

#******************************************************************************
# INCLUDE FILES
#******************************************************************************
import sys
import pandas as pd
from openpyxl import load_workbook

#******************************************************************************
# Read the input csv file and save the formatted results in AOSEvents.xlsx
# Pre-condition: AOSEvent.csv has already exists
#******************************************************************************
if len(sys.argv) != 3:
    print("Usage: python " + sys.argv[0] + " infile outfile")
    sys.exit(1)

csvFile  = sys.argv[1]
xlsxFile = sys.argv[2]

#Headers for the event list
eventListHeadersAll = ['Event ID', 'Component ID', 'Component Container', 'Event Number', 'Event Type', 'Event Text', 'DynText']

#Read csv
eventList = pd.read_csv(csvFile, names = eventListHeadersAll, index_col=False, delimiter=';')
eventList = eventList.drop_duplicates()
eventList = eventList.sort_values(by=['Event ID'])

#required Headers in AOSEvents.xlsx
eventListHeadersToWrite = ['Event ID', 'Component ID', 'Component Container','Event Type', 'Event Text']

#Load AOSEvents.xlsx and Write the Events List
book = load_workbook(xlsxFile)
del book['Events']
writer = pd.ExcelWriter(xlsxFile, engine = 'openpyxl')
writer.book = book

eventList.to_excel(writer, sheet_name='Events', columns= eventListHeadersToWrite, index= False)

#Format Output
worksheet = writer.sheets['Events']
worksheet.column_dimensions['A'].width = 8
worksheet.column_dimensions['B'].width = 14
worksheet.column_dimensions['C'].width = 21
worksheet.column_dimensions['D'].width = 10
worksheet.column_dimensions['E'].width = 70

#Set the Front page as the default selected sheet
writer.book.active = 0

#All done, save and close
writer.save()
writer.close()
