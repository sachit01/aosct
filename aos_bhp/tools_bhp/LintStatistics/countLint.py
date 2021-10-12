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
# 2018-08-03    marlundg    Created
#
#******************************************************************************

#******************************************************************************
# INCLUDE FILES
#******************************************************************************
import re
import csv
import sys
from collections import defaultdict

#******************************************************************************
# Fetces the lint errors from a given file and returns a sorted list with
# errors and number of occurences.
#
# @param[in]    lint_error_file - Filename for lint-errors
# @return       Sorted list of tuples (lint-error, set of src-lines)
#
#******************************************************************************
def fetch_lint_errors(lint_error_file):

    errors_dict = defaultdict(set)

    f = open(lint_error_file, "r")

    while True:
        line = f.readline()
        if not line: break

        # Search for the error tag
        match = re.search(r'(.*)?: error (\d+):', line)

        # Clear error
        error = ""

        if match:

            lint_src_line = match.group(1)
            lint_error = match.group(2)

            # Lint Error 1960, 1963 --> Violates a MISRA C++ Rule
            if(lint_error == "1960" or lint_error == "1963"):

                match = re.search(r' Rule (\d+\-\d+\-\d+)', line)

                if match:
                    # Add Misra rule to Lint error
                    error = lint_error + ' Rule:' + match.group(1)

            # Other 'normal' Lint Error
            else:
                error = lint_error
                
            # Any valid Lint/Misra error found? -> Store src-line in dictionary as a set.
            # This is to be able to ignore if the same src-line is reported as faulty several times.
            # Number of occurences will be number of objects in the set.
            errors_dict[error].add(lint_src_line)
        
    # Return a sorted list (Lint-errors with highest occurences first)
    return(sorted(errors_dict.items(), key=lambda t: len(t[1]), reverse=True))


#******************************************************************************
# Writes a list of tuples (lint-error, occurences) to a csv-file
# Lint-error description is added from  the gimpel.html file for each error.
#
# @param[in]    lint_error_list - List of lint-errors
# @param[in]    lint_error_file - Output filename for the lint errors, 
#                                 occurences and descriptions.
#
#******************************************************************************
def write_csv_file(lint_error_list, output_filename):

    # Open File
    csv_file = open(output_filename,'w')

    # Create Writer Object
    wr = csv.writer(csv_file, dialect="excel", delimiter=",")

    # Open Gimpel.html which has the descriptions embedded
    gimpel_file = open("gimpel.html", "r")

    # Put whole file in string
    gimple_html_file = gimpel_file.read()

    # Write Headings
    wr.writerow(('Lint Error', 'Count', 'Description'))

    # Write Data to File
    # Go through list and fetch error description (either Lint Error or MISRA rule) from Gimpels HTML file.
    for item in lint_error_list:

        # Search for description of MISRA rule
        match = re.search(r"(\d+) Rule:(\d+\-\d+\-\d+)", item[0])
        
        description_match = None
        
        if(match):

            lint_error = match.group(1)        
            misra_rule = match.group(2)

            description_match = re.search(r'id=\"' + lint_error + '\".*?<pre>.*?' + misra_rule + '.*?\)\s(.*?)\n', gimple_html_file, flags=re.S)
           
        else:

            # Search for description of Lint error
            match = re.search(r"\d+", item[0])

            if(match):
                description_match = re.search(r'id=\"' + item[0] + '\".*?<pre>(.*?)<', gimple_html_file, flags=re.S)
        
        # Write one tuple-line to CSV-file. (LintError, Count (length of set), Description (if available))
        complete_item = (item[0],str(len(item[1])))
        
        if description_match:
            description = description_match.group(1)
            description = re.sub(r'\r', r' ', description)
            description = re.sub(r'\n', r' ', description)
            description = re.sub(r' +', r' ', description)
            complete_item += (description,)

        wr.writerow(complete_item)
         

    csv_file.close()
    gimpel_file.close()
 
 
#******************************************************************************
# main function
#
#******************************************************************************
def main():
    if len(sys.argv) != 3:
        print ("usage: ./countLint.py <lintlogFile> <outputCsvFile>")
        sys.exit(1)

    src_file = sys.argv[1]
    dst_file = sys.argv[2]

    # Fetch all lint-errors as a list of tuples (Error,Count)
    lint_error_list = fetch_lint_errors(src_file)
 
    # Generate a CSV-file, and also include error-description
    write_csv_file(lint_error_list, dst_file)
              

if __name__ == "__main__":
  main()

        
