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
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "crc32.h"
#include "file_io.h"

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* EXPORTED FUNCTIONS
******************************************************************************/

void trimCRLF(char* line)
{
  char* ptr = line + strlen(line);

  while (ptr > line)
  {
    if ((ptr[-1] == '\n') || (ptr[-1] == '\r'))
    {
      ptr[-1] = '\0';
      --ptr;
    }
    else
    {
      break;
    }
  }
}

const char* skipWord(const char* ptr)
{
  while ((*ptr != '\0') && (*ptr != ' ') && (*ptr != '\t'))
  {
    ++ptr;
  }

  return ptr;
}

const char* skipSpace(const char* ptr)
{
  while ((*ptr != '\0') && ((*ptr == ' ') || (*ptr == '\t')))
  {
    ++ptr;
  }

  return ptr;
}

char* readLine(char* line, const int lineSize, FILE* file)
{
  char* readResult = fgets(line, lineSize, file);

  if (readResult != NULL)
  {
    trimCRLF(line);
  }

  return readResult;
}

void readDefinitionFile(const char* fileName, int* const majorDefVersion, int* const minorDefVersion, int* const indexToSize, int* const numParams)
{
  char line[512];
  FILE* defFile;

  *majorDefVersion = -1;
  *minorDefVersion = -1;
  *numParams = 0;

  defFile = fopen(fileName, "r");

  if (defFile == NULL)
  {
    fprintf(stderr, "Error: could not open definition file '%s'\n", fileName);
    exit(1);
  }

  while (1)
  {
    char* readResult = readLine(line, sizeof(line), defFile);

    if (readResult != NULL)
    {
      if ((line[0] != '\0') && (line[0] != '#'))
      {
        const char* ptr = line;

        ptr = skipWord(ptr);

        if (strncmp(line, "Version", (size_t) (ptr - line)) == 0)
        {
          ptr = skipSpace(ptr);

          {
            int scanResult = sscanf(ptr, "%d.%d", majorDefVersion, minorDefVersion);

            if (scanResult == 2)
            {
              break;
            }
            else
            {
              fprintf(stderr, "Error: could not parse version from definition file\n");
              exit(1);
            }
          }
        }
      }
    }
    else
    {
      break;
    }
  }

  if ((*majorDefVersion < 0) || (*minorDefVersion < 0))
  {
    fprintf(stderr, "Error: could not read version from definition file\n");
    exit(1);
  }

  while (1)
  {
    char* readResult = readLine(line, sizeof(line), defFile);

    if (readResult != NULL)
    {
      if ((line[0] != '\0') && (line[0] != '#'))
      {
        int size;
        const char* ptr = line;

        ptr = skipWord(ptr);
        ptr = skipSpace(ptr);

        if (*numParams < NVSHFT_MAX_PARAMS)
        {
          const char* ptr2 = skipWord(ptr);
          size_t len = (size_t) (ptr2 - ptr);
          int size;

          if (strncmp(ptr, "BOOL_TYPE", len) == 0)
          {
            size = 4;
          }
          else if (strncmp(ptr, "BYTE_TYPE", len) == 0)
          {
            size = 1;
          }
          else if (strncmp(ptr, "WORD_TYPE", len) == 0)
          {
            size = 2;
          }
          else if (strncmp(ptr, "DWORD_TYPE", len) == 0)
          {
            size = 4;
          }
          else
          {
            fprintf(stderr, "Error: unknown type in definition file: '%s'\n", line);
            exit(1);
          }

          indexToSize[*numParams] = size;
          *numParams += 1;
        }
        else
        {
          fprintf(stderr, "Error: too many parameters in definition file: '%s'\n", line);
          exit(1);
        }
      }
    }
    else
    {
      break;
    }
  }

  fclose(defFile);
}

void readParameterFile(const char* const fileName, int* const majorParVersion, int* const minorParVersion, uint64_t* const indexToValue, int* const numParams, uint32_t* const storedCrc, uint32_t* const calculatedCrc)
{
  char line[512];
  FILE* paramFile;
  int paramIndex = 0;

  *majorParVersion = -1;
  *minorParVersion = -1;
  *storedCrc = 0U;
  *calculatedCrc = 0U;

  initCrc32();

  paramFile = fopen(fileName, "r");

  if (paramFile == NULL)
  {
    fprintf(stderr, "Error: could not open parameter file '%s'\n", fileName);
    exit(1);
  }

  /* Read the version */

  while (1)
  {
    char* readResult = readLine(line, sizeof(line), paramFile);

    if (readResult != NULL)
    {
      addToCrc32(line, calculatedCrc);

      if ((line[0] != '\0') && (line[0] != '#'))
      {
        const char* ptr = line;

        ptr = skipWord(ptr);

        if (strncmp(line, "Version", (size_t) (ptr - line)) == 0)
        {
          ptr = skipSpace(ptr);

          {
            int scanResult = sscanf(ptr, "%d.%d", majorParVersion, minorParVersion);

            if (scanResult == 2)
            {
              break;
            }
            else
            {
              fprintf(stderr, "Error: could not parse version from parameter file '%s'\n", fileName);
              exit(1);
            }
          }
        }
      }
    }
    else
    {
      break;
    }
  }

  if ((*majorParVersion < 0) || (*minorParVersion < 0))
  {
    fprintf(stderr, "Error: could not read version from parameter file '%s'\n", fileName);
    exit(1);
  }

  /* Read the parameters and CRC */

  while (1)
  {
    char* readResult = readLine(line, sizeof(line), paramFile);

    if (readResult != NULL)
    {
      if ((line[0] != '\0') && (line[0] != '#'))
      {
        const char* ptr = line;

        ptr = skipWord(ptr);

        if (strncmp(line, "CRC", (size_t) (ptr - line)) == 0)
        {
          ptr = skipSpace(ptr);

          {
            int scanResult = sscanf(ptr, "%x", storedCrc);

            if (scanResult != 1)
            {
              *storedCrc = 0U;
            }
          }
        }
        else
        {
          addToCrc32(line, calculatedCrc);

          ptr = skipSpace(ptr);

          if (*numParams < NVSHFT_MAX_PARAMS)
          {
            uint32_t value;

            int scanResult = sscanf(ptr, "%u", &value);

            if (scanResult == 1)
            {
              indexToValue[*numParams] = value;
              *numParams += 1;
            }
            else
            {
              fprintf(stderr, "Error: error in parameter file '%s':\n  '%s'\n", fileName, line);
              exit(1);
            }
          }
          else
          {
            fprintf(stderr, "Error: too many parameters in parameter file '%s':\n  '%s'\n", fileName, line);
            exit(1);
          }
        }
      }
    }
    else
    {
      break;
    }
  }

  fclose(paramFile);
}
