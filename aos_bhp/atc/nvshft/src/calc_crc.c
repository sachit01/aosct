/******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* Implementation of CRC calculation tool for NVSHFT
*
******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "crc32.h"
#include "file_io.h"

/* For some reason, snprintf isn't declared in the PPC include files */
extern int snprintf(char *str, size_t size, const char *format, ...);

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/
#define USAGE "Usage: calc_crc IN_FILE OUT_FILE\n"

/** The mapping from parameter index to parameter value */
static uint64_t indexToValue[NVSHFT_MAX_PARAMS];

/******************************************************************************
* EXPORTED FUNCTIONS
******************************************************************************/

int main(int argc, const char* const* argv)
{
  FILE* paramFile;
  int i;
  int majorParVersion = 0;
  int minorParVersion = 0;
  int numParamValues = 0;
  uint32_t storedCrc = 0U;
  uint32_t calculatedCrc = 0U;
  uint32_t newCrc = 0U;
  char line[80];

  if (argc != 3)
  {
    fprintf(stderr, USAGE);
    exit(1);
  }

  /* Read the existing parameter file */

  readParameterFile(argv[1], &majorParVersion, &minorParVersion, indexToValue, &numParamValues, &storedCrc, &calculatedCrc);

  /* Write a new parameter file and calculate the CRC for it */

  paramFile = fopen(argv[2], "w");

  if (paramFile == NULL)
  {
    fprintf(stderr, "calc_crc: could not open parameter file '%s' for writing\n", argv[2]);
    exit(1);
  }

  initCrc32();

  for (i = -1; i < numParamValues; ++i)
  {
    if (i < 0)
    {
      snprintf(line, sizeof(line), "Version %d.%d", majorParVersion, minorParVersion);
    }
    else
    {
      snprintf(line, sizeof(line), "%d %u", i, (uint32_t) indexToValue[i]);
    }

    addToCrc32(line, &newCrc);
    fprintf(paramFile, "%s\n", line);
  }

  fprintf(paramFile, "\n");
  fprintf(paramFile, "# CRC:\n");
  fprintf(paramFile, "CRC %8X\n", newCrc);

  fclose(paramFile);

  fprintf(stderr, "calc_crc: saved parameter file '%s' with CRC\n", argv[2]);

  return 0;
}
