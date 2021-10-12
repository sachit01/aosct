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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vfw_buffer.h>
#include <vfw_crc.h>
#include "file_io.h"

/* For some reason, snprintf isn't declared in the PPC include files */
extern int snprintf(char *str, size_t size, const char *format, ...);

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

#define RUNTIME_OPTION "--runtime"
#define DISPATCHER_OPTION "--dispatcher"
#define NVSHFT_USAGE "Usage: nvshft_generic [" RUNTIME_OPTION "] [" DISPATCHER_OPTION "] DEFINITION_FILE PARAMETER_FILE BINARY_FILE\n"

#define NVSHFT_MAX_FILE_SIZE 10000

#define INTERFACE_VERSION 1
#define SEQUENCE_NUMBER 0

/******************************************************************************
* LOCAL FUNCTIONS
******************************************************************************/

/**
* Appends an unsigned integer value to the given buffer.
*
* @param[in]    value   the value to write
* @param[in]    size    the size (in bytes) of the written value
* @param[inout] buffer  the buffer to write to
*/
static void writeUnsignedInt(uint64_t value, int size, VFW_Buffer* buffer)
{
  unsigned char byte;

  switch (size)
  {
  case 1:
    vfwPutU8(buffer, value);
    break;
  case 2:
    vfwPutU16(buffer, value);
    break;
  case 4:
    vfwPutU32(buffer, value);
    break;
  case 8:
    vfwPutU64(buffer, value);
    break;
  default:
    fprintf(stderr, "NVSHFT: unsupported parameter size %d\n", size);
    exit(1);
    break;
  }
}

/**
* Appends a byte sequence to the given buffer
*
* @param[in]    bytes   the bytes to write
* @param[in]    length  the number of bytes to write
* @param[inout] buffer  the buffer to write to
*/
static void writeBytes(const void* bytes, const uint32_t length, VFW_Buffer* buffer)
{
  vfwCpyFromRawBuffer(buffer, bytes, length);
}

/**
* Writes version and parameter values to the given buffer in binary form
*
* @param[in]  majorParVersion  The major version of the parameter file
* @param[in]  minorParVersion  The minor version of the parameter file
* @param[in]  indexToSize      A mapping from parameter index to parameter size
* @param[in]  indexToValue     The array of parameter values to be stored
* @param[in]  numParams        The number of parameter sizes in indexToSize and parameter values in indexToValue
* @param[out] paramBuffer      A buffer to which the version and parameters will be written
*/
static void writeParameters(const int majorParVersion, const int minorParVersion, const int* const indexToSize, const uint64_t* const indexToValue, const int numParams, VFW_Buffer* const paramBuffer)
{
  int i;

  writeUnsignedInt((unsigned int) majorParVersion, 1U, paramBuffer);
  writeUnsignedInt((unsigned int) minorParVersion, 1U, paramBuffer);

  for (i = 0; i < numParams; ++i)
  {
    writeUnsignedInt(indexToValue[i], indexToSize[i], paramBuffer);
  }
}

/**
* Generates a parameter file in the binary format.
*
* @param[in]  fileName         The name of the binary file
* @param[in]  majorParVersion  The major version of the parameter file
* @param[in]  minorParVersion  The minor version of the parameter file
* @param[in]  readOnly         If non-zero, the binary file will be marked as read-only
* @param[in]  paramBuffer      A buffer containing parameters in binary format
* @param[out] fileBuffer       A buffer that will contain the whole binary file
*/
static void writeBinaryData(const char* fileName, const int majorParVersion, const int minorParVersion, const int readOnly, VFW_Buffer* const paramBuffer, VFW_Buffer* const fileBuffer)
{
  char fileId[64];
  char fileIdWithVersion[64];
  uint64_t crc = 0;
  size_t baseNameLength;

  /* Find the file name */

  const char* baseName = fileName + strlen(fileName);
  while (baseName > fileName)
  {
    if (baseName[-1] == '/')
    {
      break;
    }
    else
    {
      --baseName;
    }
  }

  baseNameLength = strlen(baseName);

  /* Build the file id */

  {
    const char* suffix = "_data.bin";
    const size_t suffixLength = strlen(suffix);
    size_t fileIdLength;

    if ((baseNameLength > suffixLength) && (strncmp(baseName + (baseNameLength - suffixLength), suffix, suffixLength) == 0))
    {
      fileIdLength = baseNameLength - suffixLength;
    }
    else
    {
      fileIdLength = baseNameLength;
    }

    /* TODO check fileIdLength or use vfw to copy */

    strncpy(fileId, baseName, fileIdLength);
    fileId[fileIdLength] = '\0';

    (void) snprintf(fileIdWithVersion, sizeof(fileIdWithVersion), "%s_%u.%u", fileId, majorParVersion, minorParVersion);
  }

  /* Write the data */

  writeUnsignedInt(0U, 4U, fileBuffer); /* placeholder for length, written last */

  writeUnsignedInt(INTERFACE_VERSION, 4U, fileBuffer);

  writeUnsignedInt((uint32_t) readOnly, 4U, fileBuffer);

  writeUnsignedInt(baseNameLength, 4U, fileBuffer);

  writeBytes(baseName, baseNameLength, fileBuffer);

  writeUnsignedInt(strlen(fileIdWithVersion), 4U, fileBuffer);

  writeBytes(fileIdWithVersion, strlen(fileIdWithVersion), fileBuffer);

  writeUnsignedInt(SEQUENCE_NUMBER, 4U, fileBuffer);

  writeUnsignedInt(vfwGetValidSize(paramBuffer), 4U, fileBuffer); /* payload length */

  writeBytes(paramBuffer->b, vfwGetValidSize(paramBuffer), fileBuffer); /* payload */

  crc = vfwCalcCrc64(vfwGetStart(fileBuffer) + 4U, vfwGetValidSize(fileBuffer) - 4U);

  writeUnsignedInt(crc, sizeof(crc), fileBuffer);

  /* Use a second buffer object to write the length */

  {
    uint32_t bytesWritten = vfwGetValidSize(fileBuffer);

    VFW_Buffer fileBuffer2;
    vfwInitBuffer(&fileBuffer2, vfwGetStart(fileBuffer), 4U);

    writeUnsignedInt(bytesWritten - 4U, 4U, &fileBuffer2);
  }
}

/**
* Writes a binary parameter file to disk.
*
* @param[in] fileName    The name of the binary file
* @param[in] fileBuffer  A buffer containing the whole binary file
* @param[in] duplicate   If non-zero, writes two copies of the binary data to the file
*/
static void writeBinaryFile(const char* fileName, VFW_Buffer* const fileBuffer, const int duplicate)
{
  FILE* binFile;

  binFile = fopen(fileName, "wb");

  if (binFile == NULL)
  {
    fprintf(stderr, "NVSHFT: could not open binary file '%s'\n", fileName);
    exit(1);
  }

  fwrite(vfwGetStart(fileBuffer), 1, vfwGetValidSize(fileBuffer), binFile);

  if (duplicate)
  {
    fwrite(vfwGetStart(fileBuffer), 1, vfwGetValidSize(fileBuffer), binFile);
  }

  fclose(binFile);

  printf("NVSHFT: wrote binary file '%s'\n", fileName);
}

/** The mapping from parameter index to parameter size (in bytes) */
static int indexToSize[NVSHFT_MAX_PARAMS];

/** The mapping from parameter index to parameter value */
static uint64_t indexToValue[NVSHFT_MAX_PARAMS];

/** A buffer to which parameters are written in binary format */
static unsigned char paramMemory[NVSHFT_MAX_FILE_SIZE];

/** A buffer to which the complete binary file will be written */
static unsigned char fileMemory[NVSHFT_MAX_FILE_SIZE];

int main(int argc, const char* const* argv)
{
  int majorDefVersion = 0;
  int minorDefVersion = 0;
  int majorParVersion = 0;
  int minorParVersion = 0;
  int numParamDefs = 0;
  int numParamValues = 0;
  int readOnly = 1; /* 0 for run time parameters */
  int duplicate = 1; /* 0 for dispatcher parameters */
  uint32_t storedCrc = 0U;
  uint32_t calculatedCrc = 0U;

  VFW_Buffer paramBuffer;
  VFW_Buffer fileBuffer;

  /* Parse the command line */

  while ((argc > 1) && (strncmp(argv[1], "--", 2) == 0))
  {
    if (strncmp(argv[1], RUNTIME_OPTION, strlen(RUNTIME_OPTION)) == 0)
    {
      readOnly = 0;
      --argc;
      ++argv;
    }
    else if (strncmp(argv[1], DISPATCHER_OPTION, strlen(DISPATCHER_OPTION)) == 0)
    {
      duplicate = 0;
      --argc;
      ++argv;
    }
    else
    {
      fprintf(stderr, NVSHFT_USAGE);
      exit(1);
    }
  }

  if (argc != 4)
  {
    fprintf(stderr, "NVSHFT: too few parameters\n");
    fprintf(stderr, "\n");
    fprintf(stderr, NVSHFT_USAGE);
    exit(1);
  }

  /* Initialize data structures */

  memset(indexToSize, 0, sizeof(indexToSize));
  memset(indexToValue, 0, sizeof(indexToValue));
  vfwInitBuffer(&paramBuffer, paramMemory, sizeof(paramMemory));
  vfwInitBuffer(&fileBuffer, fileMemory, sizeof(fileMemory));

  /* Process input and output */

  readDefinitionFile(argv[1], &majorDefVersion, &minorDefVersion, indexToSize, &numParamDefs);

  readParameterFile(argv[2], &majorParVersion, &minorParVersion, indexToValue, &numParamValues, &storedCrc, &calculatedCrc);

  if ((majorParVersion != majorDefVersion) || (minorParVersion != minorDefVersion))
  {
    fprintf(stderr, "NVSHFT: version in parameter file '%s' does not match definition file version\n", argv[2]);
    exit(1);
  }

  if (numParamDefs != numParamValues)
  {
    fprintf(stderr, "NVSHFT: the definition and parameter files don't have the same number of parameters\n");
    exit(1);
  }

  if (calculatedCrc != storedCrc)
  {
    fprintf(stderr, "NVSHFT: CRC error in parameter file '%s'\n", argv[2]);
    exit(1);
  }

  writeParameters(majorParVersion, minorParVersion, indexToSize, indexToValue, numParamValues, &paramBuffer);

  writeBinaryData(argv[3], majorParVersion, minorParVersion, readOnly, &paramBuffer, &fileBuffer);

  writeBinaryFile(argv[3], &fileBuffer, duplicate);

  return 0;
}
