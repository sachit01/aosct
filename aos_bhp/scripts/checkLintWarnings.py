#*******************************************************************************
#
# (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
#
# We reserve all rights in this file and in the information
# contained therein. Reproduction, use or disclosure to third
# parties without written authority is strictly forbidden.
#
# DESCRIPTION:
# Command that reads the output from pclint/flexelint and exits with an
# error status if one or more serious lint errors were found.
#
#******************************************************************************

#******************************************************************************
#
# REVISION HISTORY :
#
# Date          Name        Changes
# ---------------------------------------------------------------------------
# 2018-10-11    csundin     Created
#
#******************************************************************************

#******************************************************************************
# INCLUDE FILES
#******************************************************************************
from __future__ import print_function
import re
import sys

#******************************************************************************
# Returns the set of lint warning numbers that should be ignored.
#
# @return       set of error numbers
#******************************************************************************
def get_ignored_lint_errors():
    lint_errors = set()
    lint_errors.add("586 (printf)")
    lint_errors.add("768")
    lint_errors.add("830") # "Location cited" - not an error
    lint_errors.add("831") # "Reference cited" - not an error
    lint_errors.add("900") # "N messages produced" - not an error
    lint_errors.add("1714") # "Member function not referenced"

    return lint_errors


#******************************************************************************
# Reads the lint messages from a given file and returns a list with the errors.
#
# @param[in]    lint_output_file - filename for lint messages
# @param[in]    ignored_errors - set of ignored error numbers
#
# @return       list of error messages
#******************************************************************************
def check_lint_errors(file, ignored_errors, only_fatal):
    found_errors = []
    saved_lines = ""
    num_errors = 0

    while True:
        line = file.readline()
        if not line:
            break

        if line.find(" Module: ") >= 0:
            if not only_fatal:
                print(saved_lines, end='')
                print(line, end='')
            saved_lines = ""
        elif line.find(" error 900: ") >= 0:
            if not only_fatal:
                print(saved_lines, end='')
                print(" error 900: (Note -- Successful completion, " + str(num_errors) + " messages produced)")
                num_errors = 0
            saved_lines = ""
        else:
            # Search for the error tag
            match = re.search(r'(.*)?: error (\d+):', line)

            # Clear error
            error = ""

            if match:
                lint_src_line = match.group(1)
                lint_error = match.group(2)
                lint_name = ""

                # Lint Error 1960, 1963 --> Violates a MISRA C++ Rule
                if (lint_error == "1960" or lint_error == "1963"):
                    match = re.search(r' Rule (\d+\-\d+\-\d+)', line)
                    if match:
                        lint_name = match.group(1)
                        error = lint_error + ' (' + lint_name + ')'

                # Error 586: 'name' is deprecated
                elif lint_error == "586":
                    match = re.search(r' \'(\w+)\' is deprecated', line)
                    if match:
                        lint_name = match.group(1)
                        error = lint_error + ' (' + lint_name + ')'

                # Other 'normal' Lint Error
                else:
                    error = lint_error

                if only_fatal:
                    if error not in ignored_errors:
                        found_errors.append(saved_lines + line)
                else:
                    print(saved_lines + line, end='')
                    num_errors += 1

                saved_lines = ""
            else:
                saved_lines += line

    if not only_fatal:
        print(saved_lines, end='')

    return found_errors


#******************************************************************************
# main function
#******************************************************************************
def main():
    if len(sys.argv) < 1 or len(sys.argv) > 3:
        print("Usage: ./checkLintWarnings.py [--only-fatal] <lintLogFile>")
        sys.exit(1)

    index = 1
    only_fatal = False

    if index < len(sys.argv) and sys.argv[index] == "--only-fatal":
        only_fatal = True
        index += 1

    if index < len(sys.argv):
        src_file = open(sys.argv[index], "r")
    else:
        src_file = sys.stdin

    ignored_errors = get_ignored_lint_errors()

    found_errors = check_lint_errors(src_file, ignored_errors, only_fatal)

    if len(found_errors) > 0:
        print(len(found_errors), "error(s) found:")
        print("")

        for error in found_errors:
            print(error)


if __name__ == "__main__":
    main()
