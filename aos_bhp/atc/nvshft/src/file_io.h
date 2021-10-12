/******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Implementation of AOS NVSHFT
*
******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <stdint.h>

/******************************************************************************
* EXPORTED DECLARATIONS
******************************************************************************/
#define NVSHFT_MAX_PARAMS 2000

typedef __uint64_t uint64_t;

/******************************************************************************
* EXPORTED FUNCTIONS
******************************************************************************/

/**
* Trims the given text line by removing CR and LF from the end of it.
*
* @param[inout] line  the line to trim
*/
extern void trimCRLF(char* line);

/**
* Skips the word that the given pointer points to.
*
* @param[in] ptr  pointer to a string
*
* @return a pointer to the first whitespace character found in the string
* (or to the end of the string if no whitespace character was found)
*/
extern const char* skipWord(const char* ptr);

/**
* Skips the whitespace(s) that the given pointer points to.
*
* @param[in] ptr  pointer to a string
*
* @return a pointer to the first non-whitespace character found in the string
* (or to the end of the string if no non-whitespace character was found)
*/
extern const char* skipSpace(const char* ptr);

/**
* Reads one line of text from the given file and removes CR and LF from the end of the line.
*
* @param[out]   line      the buffer to read into
* @param[in]    lineSize  the size of the buffer
* @param[inout] file      the file to read from
*
* @return returns line on success or NULL if a line could not be read
*/
extern char* readLine(char* line, const int lineSize, FILE* file);

/**
* Reads a parameter definition file.
*
* @param[in]  fileName         The name of the file from which parameter definitions will be read
* @param[out] majorDefVersion  Will contain the major version number of the definition file
* @param[out] minorDefVersion  Will contain the minor version number of the definition file
* @param[out] indexToSize      Will contain a mapping from parameter index to parameter size
* @param[out] numParams        Will contain the number of parameter sizes in indexToSize
*/
extern void readDefinitionFile(const char* fileName, int* const majorDefVersion, int* const minorDefVersion, int* const indexToSize, int* const numParams);

/**
* Reads a parameter value file and retrieves its parameter values.
*
* @param[in]  fileName         The name of the file from which parameter values will be read
* @param[out] majorParVersion  Will contain the major version number of the parameter file
* @param[out] minorParVersion  Will contain the minor version number of the parameter file
* @param[out] indexToValue     Will contain a mapping from parameter index to parameter value
* @param[out] numParams        Will contain the number of parameter values in indexToValue
* @param[out] storedCrc        Will contain the CRC stored in the parameter file
* @param[out] calculatedCrc    Will contain the CRC calculated from the contents of the parameter file
*/
extern void readParameterFile(const char* const fileName, int* const majorParVersion, int* const minorParVersion, uint64_t* const indexToValue, int* const numParams, uint32_t* const storedCrc, uint32_t* const calculatedCrc);
