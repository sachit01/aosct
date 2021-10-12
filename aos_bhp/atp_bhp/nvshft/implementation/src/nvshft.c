/*******************************************************************************
 *
 * (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2002
 *
 * We reserve all rights in this file and in the information
 * contained therein. Reproduction, use or disclosure to third
 * parties without express authority is strictly forbidden.
 *
 * Module name:
 *
 * %name: nvshFT.c %
 * %version: 1 %
 * %created_by: nsyed %
 * %date_created: 2017-01-31 16:11 %
 * %Creation date of original object: Fri Nov 17 13:55:22 2000 %
 *
 * Description:
 *
 *******************************************************************************/
 /*******************************************************************************
  * Revision History
  *
  * Version  Date    Sign    Change description
  * 1.0     130222  wahmad   Initially created
  * 1.1     130225  wahmad   Struct is used now instead of global arrays for NVSHFT parameters
  * 1.2     130227  wahmad   Requirement # 45 to 48 is added regarding file writing/ No of parameters/
  *                          Size of parameters. Moreover fprintf is changes to printf.
  * 1.3     130314  wahmad   NVSHFT version1.1 according to parameters updated 1.2 of CFPARAM
  *                          and 1.4 of IFSATPCU document
  * 1.4     130603  wahmad   Changes due to new requirements added for NVSHFT such as
  *                          interface_version and read_only_data.
  * 1.5     130604  wahmad   Lint corrections have been made
  * 1.6     140516  gerglun  Modified path to configuration binary files on target.
  * 1.7     140519  anlindel Modified erronous error trace.
  * 1.8     140626  wahmad   Compilable on B-board
  * 1.10    140909  wahmad   One extra slash (/) removed
  * 1.11    170127  nsyed    Adapted to BHP Project
  *******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "nvshft.h"
#include "crc.h"

#ifndef WIN32
void strreverse(S_CHAR* begin, S_CHAR* end) {

    S_CHAR aux;

    while (end > begin)

        aux = *end, *end-- = *begin, *begin++ = aux;

}

void itoa(S_INT value, S_CHAR* str, S_INT base) {

    static S_CHAR num[] = "0123456789abcdefghijklmnopqrstuvwxyz";

    S_CHAR* wstr = str;

    S_INT sign;

    /* Validate base*/

    if (base < 2 || base>35) { *wstr = '\0'; return; }

    /* Take care of sign*/

    if ((sign = value) < 0) value = -value;

    /* Conversion. Number is reversed.*/

    do *wstr++ = num[value%base]; while (value /= base);

    if (sign < 0) *wstr++ = '-';

    *wstr = '\0';

    /* Reverse string*/

    strreverse(str, wstr - 1);

}
#endif

void showVersion() {
    /*printf("\n------> NOT A RELEASED VERSION <-----\n");*/
    printf(
        "NVSHFT version %d.%d\n\n\
This version of NVSHFT is compatible with the following data files:\n\n\
Version %d.%d of Common Parameter files cfg_data.txt, mnt_data.txt, rt_data.txt for BHP (Params_ATP.xls)\n\
Version %d.%d of Dispatcher Configuration Parameter file dispatcher_data.txt for BHP (Params_Disp.xls) \n\n",
NVSHFT_MAJOR, NVSHFT_MINOR, CFPARAM_MAJOR,
CFPARAM_MINOR, CFPARAM_MAJOR, CFPARAM_MINOR);
}

S_INT showHelp() {
    printf(
        "Usage: nvshft [options]\nNote::Please note that at least one switch is necessary\n\n"
        "-h\tDisplays Usage of nvshft\n"
        "-v\tDisplays Version of nvshft\n"
        "-n\tConvert Common, Maintenance and Runtime parameter files in ASCII format into corresponding NVSH parameter files.\n"
        "-n -c\tConvert Common parameter file in ASCII format into an NVSH Configuration parameter file.\n"
        "-n -m\tConvert Maintenance parameter file in ASCII format into an NVSH Maintenance parameter file.\n"
        "-n -r\tConvert Runtime parameter file in ASCII format into an NVSH Runtime parameter file.\n"
        "-n -d\tConvert Dispatcher Configuration parameter file in ASCII format into an NVSH Runtime parameter file.\n"
        "-n -i\tConvert Instance parameter file in ASCII format into an NVSH Runtime parameter file.\n"
        "-n -t\tConvert Type parameter file in ASCII format into an NVSH Runtime parameter file.\n"

        "-a\tConvert NVSH Common, Maintenance and Runtime parameter files into corresponding parameter files in ASCII format.\n"
        "-a -c\tConvert NVSH Configuration parameter file into a Configuration parameter file in ASCII format.\n"
        "-a -m\tConvert NVSH Maintenance parameter file into a Maintenance parameter file in ASCII format.\n"
        "-a -r\tConvert NVSH Runtime parameter file into a Runtime parameter file in ASCII format.\n"
        "-a -d\tConvert NVSH Dispatcher Configuration parameter file into a Runtime parameter file in ASCII format.\n"

    );
    return 0;
}

const US_CHAR * getFilename(S_INT option)
{
    switch (option) {
    case COMMON:
        return COMMON_NAME"_data.bin";
        break;
    case DISPATCHER:
        return DISPATCHER_NAME"_data.bin";
        break;
    case INSTANCE:
        return INSTANCE_NAME"_data.bin";
        break;
    case MAINTENANCE:
        return MAINTENANCE_NAME"_data.bin";
        break;
    case TYPE:
        return TYPE_NAME"_data.bin";
        break;
    case RUNTIME:
        return RUNTIME_NAME"_data.bin";
        break;
    default:
        exit(1);
        break;
    }
}

S_INT getFileId(S_INT option, US_CHAR *buffer, US_INT buffersize)
{
    const char* name;
    S_INT result;
    switch (option)
    {
    case COMMON:
        name = COMMON_NAME;
        break;
    case INSTANCE:
        name = INSTANCE_NAME;
        break;
    case MAINTENANCE:
        name = MAINTENANCE_NAME;
        break;
    case TYPE:
        name = TYPE_NAME;
        break;
    case RUNTIME:
        name = RUNTIME_NAME;
        break;
    case DISPATCHER:
        name = DISPATCHER_NAME;
        break;
    default:
        exit(1);
        break;
    }

    result = snprintf(buffer, buffersize, "%s_%d.%d", name, CFPARAM_MAJOR, CFPARAM_MINOR);
    return result;
}

US_INT calcSizeOfPayload(S_INT option) {
    US_INT i, payloadSize = 0;

    switch (option) {
    case 1:
        for (i = 0; i < NOP_COMMON; i++)
        {
            payloadSize = payloadSize + parameterSize_COMMON[i];
        }
        break;
    case 2:
        for (i = 0; i < NOP_MNT; i++)
        {
            payloadSize = payloadSize + parameterSize_MNT[i];
        }
        break;
    case 3:
        for (i = 0; i < NOP_RT; i++)
        {
            payloadSize = payloadSize + parameterSize_RT[i];
        }
        break;
    case 4:
        for (i = 0; i < NOP_CFG_DISP; i++)
        {
            payloadSize = payloadSize + parameterSize_CFG_DISP[i];
        }
        break;
    case 5:
        for (i = 0; i < NOP_INSTANCE; i++)
        {
            payloadSize = payloadSize + parameterSize_INSTANCE[i];
        }
        break;
    case 6:
        for (i = 0; i < NOP_TYPE; i++)
        {
            payloadSize = payloadSize + parameterSize_TYPE[i];
        }
        break;
    default:
        printf("\nNVSHFT: Error in calcSizeOfPayload - This should not happen\n");
        exit(1);
        break;
    }

    return payloadSize;
}

S_INT writeNVSHFile(US_CHAR * dataFileBuffer, US_INT fileLength, S_INT option) {
    US_CHAR *afterCommentsRemoved;
    US_CHAR *writeBuffer;
    US_CHAR *test_filename = NULL;
    US_CHAR nvshFilePath[40] = "../data_files/";
    US_INT application_data_length;
    S_INT writeBuffer_location = 0, locTemp = 0;
    S_INT bytesWritten;
    S_INT Major, Minor;
    S_INT noOfParameters;
    S_INT *parameterSize;
    S_INT i, j = 11;
    S_CHAR formattedString[3640] = "%*s %*s %*s %lu";
    FILE *bfptr = NULL;

    US_CHAR fileIdBuffer[50];
    test_filename = (US_CHAR*)getFilename(option);//cfg_data.bin
    strcat(nvshFilePath, test_filename);

    uint64_t calculatedCRCs;
    US_LONG value;
    US_CHAR strMajor[5], strMinor[5];
    US_INT writeBufferLength = 0;
    US_CHAR read_only_data = 1;

    S_INT fileIdLength = getFileId(option, fileIdBuffer, sizeof(fileIdBuffer) );
    application_data_length = calcSizeOfPayload(option) + 2; /* parameters + 2 bytes of version */

    if (option == COMMON)
    {
        Major = CFPARAM_MAJOR;
        Minor = CFPARAM_MINOR;
        noOfParameters = NOP_COMMON;
        parameterSize = parameterSize_COMMON;

        /* 28 = size of bytes defined in SMDS, fileIdLength-COMMON_1.0 , 8(interface_version+read_only_data) */
        writeBufferLength = 2 * (28 + fileIdLength + 8 + nvshft_strlen(test_filename)
            + application_data_length); /* Multiplied by 2 because of DATA B */
    }

    else if (option == TYPE)
    {
        Major = CFPARAM_MAJOR;
        Minor = CFPARAM_MINOR;
        noOfParameters = NOP_TYPE;
        parameterSize = parameterSize_TYPE;

        /* 28 = size of bytes defined in SMDS, fileIdLength-TYPE_1.0 , 8(interface_version+read_only_data) */
        writeBufferLength = 2 * (28 + fileIdLength + 8 + nvshft_strlen(test_filename)
            + application_data_length); /* Multiplied by 2 because of DATA B */
    }
    else if (option == INSTANCE)
    {
        Major = CFPARAM_MAJOR;
        Minor = CFPARAM_MINOR;
        noOfParameters = NOP_INSTANCE;
        parameterSize = parameterSize_INSTANCE;

        /* 28 = size of bytes defined in SMDS, fileIdLength-INSTANCE_1.0 , 8(interface_version+read_only_data) */
        writeBufferLength = 2 * (28 + fileIdLength + 8 + nvshft_strlen(test_filename)
            + application_data_length); /* Multiplied by 2 because of DATA B */
    }
    else if (option == MAINTENANCE)
    {
        Major = CFPARAM_MAJOR;
        Minor = CFPARAM_MINOR;
        noOfParameters = NOP_MNT;
        parameterSize = parameterSize_MNT;

        /* 28 = size of bytes defined in SMDS, fileIdLength-MNT_1.0 , 8(interface_version+read_only_data) */
        writeBufferLength = 2 * (28 + fileIdLength + 8 + nvshft_strlen(test_filename)
            + application_data_length); /* Multiplied by 2 because of DATA B */
    }
    else if (option == RUNTIME)
    {
        Major = CFPARAM_MAJOR;
        Minor = CFPARAM_MINOR;
        noOfParameters = NOP_RT;
        parameterSize = parameterSize_RT;
        read_only_data = 0;

        /* 28 = size of bytes defined in SMDS, fileIdLength-RT_1.0 , 8(interface_version+read_only_data) */
        writeBufferLength = 2 * (28 + fileIdLength + 8 + nvshft_strlen(test_filename)
            + application_data_length); /* Multiplied by 2 because of DATA B */
    }
    else if (option == DISPATCHER)
    {
        Major = CFPARAM_MAJOR;
        Minor = CFPARAM_MINOR;
        noOfParameters = NOP_CFG_DISP;
        parameterSize = parameterSize_CFG_DISP;

        /* 28 = size of bytes defined in SMDS, fileIdLength-CFG_DISP_1.0 , 8(interface_version+read_only_data) */
        writeBufferLength = (28 + fileIdLength + 8 + nvshft_strlen(test_filename)
            + application_data_length);
    }
    else {
        printf("\nNVSHFT: Invalid Choice of ASCII file - Execution is aborting...\n");
        exit(1);
    }

    /*printf("\n-----------\nAfter:\n%s\n",dataFileBuffer);
     writeNVSH(dataFileBuffer, fileLength, 1);*/

    afterCommentsRemoved = (US_CHAR*)malloc(
        sizeof(US_CHAR) * (fileLength + 1));
    if (afterCommentsRemoved == NULL) {
        fputs("Could not allocate buffer for data file content\n", stderr);
        exit(2);
    }
    memset(afterCommentsRemoved, 0, sizeof(US_CHAR) * (fileLength + 1));

    writeBuffer = (US_CHAR*)malloc(sizeof(US_CHAR) * (writeBufferLength + 1));
    if (writeBuffer == NULL) {
        fputs("Could not allocate buffer for data file content\n", stderr);
        exit(2);
    }

    memset(writeBuffer, 0, sizeof(US_CHAR) * (writeBufferLength + 1));

    afterCommentsRemoved = removeComments(dataFileBuffer, fileLength);
    /*printf("\n-------\n0x%x\n",nvshft_strlen(test_filename));*/

    writeBuffer_location = 4;

    /* Interface version: */
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = INTERFACE_VERSION;

    /* Read-only flag */
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = read_only_data;

    /* Size of filename */
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = nvshft_strlen(test_filename);

    /* Filename */
    strncpy((S_CHAR *)&writeBuffer[writeBuffer_location],
        (const S_CHAR*)test_filename, nvshft_strlen(test_filename));
    writeBuffer_location = writeBuffer_location + nvshft_strlen(test_filename);

    /* Size of Version */
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = 0;

    /* unique file id */
#if 0
    writeBuffer[writeBuffer_location++] = 4;
    sscanf(afterCommentsRemoved, "%*s %s", &writeBuffer[writeBuffer_location]);
    writeBuffer_location += 4;
#else
    locTemp = writeBuffer_location;
    writeBuffer_location++;
    S_INT result = getFileId(option, &writeBuffer[writeBuffer_location], writeBufferLength - writeBuffer_location);
    if (result > 0)
    {
        writeBuffer_location += result;
    }
    writeBuffer[locTemp] = writeBuffer_location - locTemp - 1;
#endif

    /* Data Sequence Number */
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = 0;
    writeBuffer[writeBuffer_location++] = 0;

    /* Application data length */
    /*application_data_length = 19;*/
    writeBuffer[writeBuffer_location++] = (US_CHAR)(application_data_length
        >> 24);
    writeBuffer[writeBuffer_location++] = (US_CHAR)(application_data_length
        >> 16);
    writeBuffer[writeBuffer_location++] = (US_CHAR)(application_data_length
        >> 8);
    writeBuffer[writeBuffer_location++] = (US_CHAR)(application_data_length);

    /* Application Data - Starts*/
    /* File Version in Hex */
    writeBuffer[writeBuffer_location++] = Major;
    writeBuffer[writeBuffer_location++] = Minor;

    /* Parameter values */
    for (i = 0; i < noOfParameters; i++) {
        if (sscanf((S_CHAR*)afterCommentsRemoved, formattedString, &value) != 1) {
            printf("\nNVSHFT: Failed to parse parameter %d in input ASCII file.\n", i);
            exit(1);
        }

        if (parameterSize[i] == 4) {
            writeBuffer[writeBuffer_location++] = (US_CHAR)(value >> 24);
            writeBuffer[writeBuffer_location++] = (US_CHAR)(value >> 16);
            writeBuffer[writeBuffer_location++] = (US_CHAR)(value >> 8);
            writeBuffer[writeBuffer_location++] = (US_CHAR)value;
        }
        else if (parameterSize[i] == 2) {
            if (value > USHRT_MAX) {
                printf("\nNVSHFT: Parameter %d in input ASCII file has value %lu - incompatible with the parameter's size %d.\n",
                    i, value, parameterSize[i]);
                exit(1);
            }
            writeBuffer[writeBuffer_location++] = (US_CHAR)(value >> 8);
            writeBuffer[writeBuffer_location++] = (US_CHAR)value;
        }
        else {
            if (value > UCHAR_MAX) {
                printf("\nNVSHFT: Parameter %d in input ASCII file has value %lu - incompatible with the parameter's size %d.\n",
                    i, value, parameterSize[i]);
                exit(1);
            }
            writeBuffer[writeBuffer_location++] = (US_CHAR)value;
        }

        strncpy((S_CHAR*)&formattedString[j], (const S_CHAR*) " %*s %*s %lu", sizeof(formattedString) - j);

        j = j + 8;
    }
    /* Application Data - Ends*/

    /* NVSH data block length */
    writeBuffer[0] = (writeBuffer_location + 4) >> 24;
    writeBuffer[1] = (writeBuffer_location + 4) >> 16;
    writeBuffer[2] = (writeBuffer_location + 4) >> 8;
    writeBuffer[3] = (writeBuffer_location + 4);

    /* Calculating CRC 64 */
    calculatedCRCs = calcCrc64(writeBuffer + 4, writeBuffer_location - 4);

    /* NVSH data tail */
    writeBuffer[writeBuffer_location++] = (US_CHAR)(((uint64_t)calculatedCRCs)
        >> 56);
    writeBuffer[writeBuffer_location++] = (US_CHAR)(((uint64_t)calculatedCRCs)
        >> 48);
    writeBuffer[writeBuffer_location++] = (US_CHAR)(((uint64_t)calculatedCRCs)
        >> 40);
    writeBuffer[writeBuffer_location++] = (US_CHAR)(((uint64_t)calculatedCRCs)
        >> 32);
    writeBuffer[writeBuffer_location++] = (US_CHAR)(((uint64_t)calculatedCRCs)
        >> 24);
    writeBuffer[writeBuffer_location++] = (US_CHAR)(((uint64_t)calculatedCRCs)
        >> 16);
    writeBuffer[writeBuffer_location++] = (US_CHAR)(((uint64_t)calculatedCRCs)
        >> 8);
    writeBuffer[writeBuffer_location++] = (US_CHAR)((uint64_t)calculatedCRCs);

    /* Copying data for data block B */
    if (option != 4) {
        memcpy(&writeBuffer[writeBuffer_location], writeBuffer,
            writeBuffer_location);
        writeBuffer_location = writeBuffer_location * 2;
    }
    /* Opening & Writing NVSH file */

    bfptr = fopen(nvshFilePath, "wb");
    if (NULL == bfptr) {
        printf("NVSHFT: could not write to file '%s'\n", nvshFilePath);
        exit(1);
    }

    bytesWritten = fwrite(writeBuffer, sizeof(US_CHAR), writeBuffer_location, bfptr);
    fclose(bfptr);

    
    free(afterCommentsRemoved);
    free(writeBuffer);

    if (bytesWritten == writeBuffer_location) {
        printf("NVSHFT: .bin successfully generated and stored in '%s'\n", nvshFilePath);
    }
    else {
        printf("NVSHFT: could not write to file '%s'\n", nvshFilePath);
        exit(1);
    }

    return 0;
}

S_INT writeASCIIFile(US_CHAR * dataFileBuffer, US_INT fileLength, S_INT option) {
    US_CHAR *writeBuffer = 0;
    S_INT writeBuffer_location = 0;
    S_INT noOfParameters = 0;
    S_INT *parameterSize;
    US_CHAR** parameterName;
    S_INT i;
    FILE *afptr;
    S_CHAR *asciiFilePath = NULL;
    S_INT sizeOfWriteBuffer = 0;
    US_CHAR strMajor[5], strMinor[5];
    US_INT dataFileBufferLoc = 0;
    US_CHAR strParamVal[10] = { 0 };
    US_INT paramVal_uint;
    US_INT calculatedCRC;
    US_CHAR *strForCRC;
    /*  US_CHAR string_CRC[13];*/

    if (option == COMMON) {
        noOfParameters = NOP_COMMON;
        sizeOfWriteBuffer = 12 + (40 * noOfParameters) + 20;

        asciiFilePath = "../data_files/cfg_data_readout.txt";

        writeBuffer = (US_CHAR*)malloc(sizeof(US_CHAR) * sizeOfWriteBuffer);
        if (writeBuffer == NULL) {
            fputs("Could not allocate buffer for data file content\n", stderr);
            exit(2);
        }
        memset(writeBuffer, 0, sizeof(US_CHAR) * sizeOfWriteBuffer);

        strncpy((S_CHAR*)writeBuffer, (const S_CHAR*) "Version ", 8);
        writeBuffer_location = writeBuffer_location + 8;

        itoa(CFPARAM_MAJOR, (S_CHAR*)strMajor, 10);
        itoa(CFPARAM_MINOR, (S_CHAR*)strMinor, 10);
        /*printf("\n size:: %d  %d", nvshft_strlen(strMajor), nvshft_strlen(strMinor));*/

        strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
            (const S_CHAR*)strMajor, nvshft_strlen(strMajor));
        writeBuffer_location = writeBuffer_location + nvshft_strlen(strMajor);
        strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
            (const S_CHAR*) ".", 1);
        writeBuffer_location = writeBuffer_location + 1;
        strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
            (const S_CHAR*)strMinor, nvshft_strlen(strMinor));
        writeBuffer_location = writeBuffer_location + nvshft_strlen(strMinor);
        writeBuffer[writeBuffer_location++] = '\r';
        writeBuffer[writeBuffer_location++] = '\n';

        dataFileBufferLoc = 41 + 8; /* 8 due to interface_version and read_only_data */
        parameterSize = parameterSize_COMMON;
        parameterName = parameterName_COMMON;
    }
    else if (option == MAINTENANCE) {
        noOfParameters = NOP_MNT;
        sizeOfWriteBuffer = 12 + (40 * noOfParameters) + 20;

        asciiFilePath = "../data_files/mnt_data_readout.txt";

        writeBuffer = (US_CHAR*)malloc(sizeof(US_CHAR) * sizeOfWriteBuffer);
        if (writeBuffer == NULL) {
            fputs("Could not allocate buffer for data file content\n",
                stderr);
            exit(2);
        }
        memset(writeBuffer, 0, sizeof(US_CHAR) * sizeOfWriteBuffer);

        strncpy((S_CHAR*)writeBuffer, (const S_CHAR*) "Version ", 8);
        writeBuffer_location = writeBuffer_location + 8;

        itoa(CFPARAM_MAJOR, (S_CHAR*)strMajor, 10);
        itoa(CFPARAM_MINOR, (S_CHAR*)strMinor, 10);
        /*printf("\n size:: %d  %d", nvshft_strlen(strMajor), nvshft_strlen(strMinor));*/

        strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
            (const S_CHAR*)strMajor, nvshft_strlen(strMajor));
        writeBuffer_location = writeBuffer_location + nvshft_strlen(strMajor);
        strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
            (const S_CHAR*) ".", 1);
        writeBuffer_location = writeBuffer_location + 1;
        strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
            (const S_CHAR*)strMinor, nvshft_strlen(strMinor));
        writeBuffer_location = writeBuffer_location + nvshft_strlen(strMinor);
        writeBuffer[writeBuffer_location++] = '\r';
        writeBuffer[writeBuffer_location++] = '\n';

        dataFileBufferLoc = 41 + 8; /* 8 due to interface_version and read_only_data */
        parameterSize = parameterSize_MNT;
        parameterName = parameterName_MNT;
    }
    else if (option == RUNTIME) {
        noOfParameters = NOP_RT;
        sizeOfWriteBuffer = 12 + (40 * noOfParameters) + 20;

        asciiFilePath = "../data_files/rt_data_readout.txt";

        writeBuffer = (US_CHAR*)malloc(sizeof(US_CHAR) * sizeOfWriteBuffer);
        if (writeBuffer == NULL) {
            fputs("Could not allocate buffer for data file content\n",
                stderr);
            exit(2);
        }
        memset(writeBuffer, 0, sizeof(US_CHAR) * sizeOfWriteBuffer);

        strncpy((S_CHAR*)writeBuffer, (const S_CHAR*) "Version ", 8);
        writeBuffer_location = writeBuffer_location + 8;

        itoa(CFPARAM_MAJOR, (S_CHAR*)strMajor, 10);
        itoa(CFPARAM_MINOR, (S_CHAR*)strMinor, 10);
        /*printf("\n size:: %d  %d", nvshft_strlen(strMajor), nvshft_strlen(strMinor));*/

        strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
            (const S_CHAR*)strMajor, nvshft_strlen(strMajor));
        writeBuffer_location = writeBuffer_location + nvshft_strlen(strMajor);
        strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
            (const S_CHAR*) ".", 1);
        writeBuffer_location = writeBuffer_location + 1;
        strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
            (const S_CHAR*)strMinor, nvshft_strlen(strMinor));
        writeBuffer_location = writeBuffer_location + nvshft_strlen(strMinor);
        writeBuffer[writeBuffer_location++] = '\r';
        writeBuffer[writeBuffer_location++] = '\n';

        dataFileBufferLoc = 39 + 8; /* 8 due to interface_version and read_only_data */
        parameterSize = parameterSize_RT;
        parameterName = parameterName_RT;
    }
    else if (option == DISPATCHER) {
        noOfParameters = NOP_CFG_DISP;
        sizeOfWriteBuffer = 12 + (40 * noOfParameters) + 20;

        asciiFilePath = "../data_files/dispatcher_data_readout.txt";

        writeBuffer = (US_CHAR*)malloc(sizeof(US_CHAR) * sizeOfWriteBuffer);
        if (writeBuffer == NULL) {
            fputs("Could not allocate buffer for data file content\n",
                stderr);
            exit(2);
        }
        memset(writeBuffer, 0, sizeof(US_CHAR) * sizeOfWriteBuffer);

        strncpy((S_CHAR*)writeBuffer, (const S_CHAR*) "Version ", 8);
        writeBuffer_location = writeBuffer_location + 8;

        itoa(CFPARAM_MAJOR, (S_CHAR*)strMajor, 10);
        itoa(CFPARAM_MINOR, (S_CHAR*)strMinor, 10);
        /*printf("\n size:: %d  %d", nvshft_strlen(strMajor), nvshft_strlen(strMinor));*/

        strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
            (const S_CHAR*)strMajor, nvshft_strlen(strMajor));
        writeBuffer_location = writeBuffer_location + nvshft_strlen(strMajor);
        strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
            (const S_CHAR*) ".", 1);
        writeBuffer_location = writeBuffer_location + 1;
        strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
            (const S_CHAR*)strMinor, nvshft_strlen(strMinor));
        writeBuffer_location = writeBuffer_location + nvshft_strlen(strMinor);
        writeBuffer[writeBuffer_location++] = '\r';
        writeBuffer[writeBuffer_location++] = '\n';

        dataFileBufferLoc = 51 + 8; /* 8 due to interface_version and read_only_data */
        parameterSize = parameterSize_CFG_DISP;
        parameterName = parameterName_CFG_DISP;
    }
    else {
        printf("NVSHFT: illegal option %d.\n", option);
        exit(1);
    }

    for (i = 0; i < (noOfParameters)/*No of Params*/; i++) {
        if (parameterSize[i] == 2) {
            strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
                (const S_CHAR*)parameterName[i],
                nvshft_strlen(parameterName[i]));
            writeBuffer_location = writeBuffer_location
                + nvshft_strlen(parameterName[i]);

            writeBuffer[writeBuffer_location++] = ' ';

            itoa(
                (dataFileBuffer[dataFileBufferLoc + 1]
                    | (dataFileBuffer[dataFileBufferLoc] << 8)),
                    (S_CHAR*)strParamVal, 10);
            strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
                (const S_CHAR*)strParamVal, nvshft_strlen(strParamVal));
            writeBuffer_location = writeBuffer_location
                + nvshft_strlen(strParamVal);
            dataFileBufferLoc = dataFileBufferLoc + 2;
        }
        else if (parameterSize[i] == 4) {
            strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
                (const S_CHAR*)parameterName[i],
                nvshft_strlen(parameterName[i]));
            writeBuffer_location = writeBuffer_location
                + nvshft_strlen(parameterName[i]);

            writeBuffer[writeBuffer_location++] = ' ';

            /*itoa(((dataFileBuffer[dataFileBufferLoc+3])|(dataFileBuffer[dataFileBufferLoc+2]<<8)|(dataFileBuffer[dataFileBufferLoc+1]<<16)|(dataFileBuffer[dataFileBufferLoc]<<24)), &writeBuffer[writeBuffer_location], 10);*/
            paramVal_uint = (US_INT)((dataFileBuffer[dataFileBufferLoc + 3])
                | (dataFileBuffer[dataFileBufferLoc + 2] << 8)
                | (dataFileBuffer[dataFileBufferLoc + 1] << 16)
                | ((dataFileBuffer[dataFileBufferLoc] << 24) & 0x7FFFFFFF));
            if (dataFileBuffer[dataFileBufferLoc] & 0x80) {
                paramVal_uint = paramVal_uint + 2147483648U; /* 2^31*/
                sprintf((S_CHAR*)&writeBuffer[writeBuffer_location],
                    (S_CHAR*) "%u", paramVal_uint);
                writeBuffer_location = writeBuffer_location + 10;
            }
            else {
                /*itoa((unsigned long)((dataFileBuffer[dataFileBufferLoc+3])|(dataFileBuffer[dataFileBufferLoc+2]<<8)|(dataFileBuffer[dataFileBufferLoc+1]<<16)|(dataFileBuffer[dataFileBufferLoc]<<24)), strParamVal, 10);*/
                itoa(paramVal_uint, (S_CHAR*)strParamVal, 10);
                /*        if(i==62){
                 printf("\n\n%ld\n",paramVal_uint);
                 exit(0);
                 }*/
                strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
                    (const S_CHAR*)strParamVal,
                    nvshft_strlen(strParamVal));
                /*printf("\n\n %ld\n", ((dataFileBuffer[dataFileBufferLoc+3])|(dataFileBuffer[dataFileBufferLoc+2]<<24)|(dataFileBuffer[dataFileBufferLoc+1]<<16)|(dataFileBuffer[dataFileBufferLoc]<<8)));*/

                writeBuffer_location = writeBuffer_location
                    + nvshft_strlen(strParamVal);
            }
            dataFileBufferLoc = dataFileBufferLoc + 4;
        }
        else {
            strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
                (const S_CHAR*)parameterName[i],
                nvshft_strlen(parameterName[i]));
            writeBuffer_location = writeBuffer_location
                + nvshft_strlen(parameterName[i]);

            writeBuffer[writeBuffer_location++] = ' ';

            itoa((dataFileBuffer[dataFileBufferLoc++]), (S_CHAR*)strParamVal,
                10);
            strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
                (const S_CHAR*)strParamVal, nvshft_strlen(strParamVal));
            writeBuffer_location = writeBuffer_location
                + nvshft_strlen(strParamVal);
        }
        writeBuffer[writeBuffer_location++] = '\r';
        writeBuffer[writeBuffer_location++] = '\n';

        /*printf("\n\n %ld\n", (dataFileBuffer[42]|(dataFileBuffer[41]<<8)));*/
    }

    crcInit();
    strForCRC = removeSpacesCarriages1(writeBuffer, writeBuffer_location);
    calculatedCRC = crcFast(strForCRC, nvshft_strlen(strForCRC));

    strncpy((S_CHAR*)&writeBuffer[writeBuffer_location],
        (const S_CHAR*) "CRC ", 4);
    writeBuffer_location = writeBuffer_location + 4;
#if 0
    /*itoa(calculatedCRC, &writeBuffer[writeBuffer_location], 16);*/
    itoa(calculatedCRC, string_CRC, 16);
    /*strcpy(&writeBuffer[writeBuffer_location], string_CRC);*/
    if (nvshft_strlen(string_CRC) < 8)
    {
        i = 8 - nvshft_strlen(string_CRC);
        switch (i)
        {
        case 1:
            sprintf(&string_CRC[0], "%X", 0);
            sprintf(&string_CRC[1], "%X", calculatedCRC);
            break;
        case 2:
            sprintf(&string_CRC[0], "%X", 0);
            sprintf(&string_CRC[1], "%X", 0);
            sprintf(&string_CRC[2], "%X", calculatedCRC);
            break;
        case 3:
            sprintf(&string_CRC[0], "%X", 0);
            sprintf(&string_CRC[1], "%X", 0);
            sprintf(&string_CRC[2], "%X", 0);
            sprintf(&string_CRC[3], "%X", calculatedCRC);
            break;
        case 4:
            sprintf(&string_CRC[0], "%X", 0);
            sprintf(&string_CRC[1], "%X", 0);
            sprintf(&string_CRC[2], "%X", 0);
            sprintf(&string_CRC[3], "%X", 0);
            sprintf(&string_CRC[4], "%X", calculatedCRC);
            break;
        case 5:
            sprintf(&string_CRC[0], "%X", 0);
            sprintf(&string_CRC[1], "%X", 0);
            sprintf(&string_CRC[2], "%X", 0);
            sprintf(&string_CRC[3], "%X", 0);
            sprintf(&string_CRC[4], "%X", 0);
            sprintf(&string_CRC[5], "%X", calculatedCRC);
            break;
        case 6:
            sprintf(&string_CRC[0], "%X", 0);
            sprintf(&string_CRC[1], "%X", 0);
            sprintf(&string_CRC[2], "%X", 0);
            sprintf(&string_CRC[3], "%X", 0);
            sprintf(&string_CRC[4], "%X", 0);
            sprintf(&string_CRC[5], "%X", 0);
            sprintf(&string_CRC[6], "%X", calculatedCRC);
            break;
        default: break;
        }
    }
#endif
    sprintf((S_CHAR*)&writeBuffer[writeBuffer_location], (S_CHAR*) "%08X",
        calculatedCRC);
    /*  strncpy(&writeBuffer[writeBuffer_location], string_CRC, 8);*/

    writeBuffer_location = writeBuffer_location + 8;

    switch (option) {
    case 1:
        afptr = fopen(asciiFilePath, "w+b");
        if (NULL == afptr) {
            printf(
                "NVSHFT: writing to file cfg_data_readout.txt was not successful.\n");
            exit(1);
        }
        printf(
            "NVSHFT:cfg_data_readout.txt successfully generated and stored in : %s\n",
            asciiFilePath);
        break;
    case 5:
        afptr = fopen(asciiFilePath, "w+b");
        if (NULL == afptr) {
            printf(
                "NVSHFT: writing to file instance_spec_readout.txt was not successful.\n");
            exit(1);
        }
        printf(
            "NVSHFT:instance_spec_data.txt successfully generated and stored in : %s\n",
            asciiFilePath);
        break;
    case 2:
        afptr = fopen(asciiFilePath, "w+b");
        if (NULL == afptr) {
            printf(
                "NVSHFT: writing to file mnt_data_readout.txt was not successful.\n");
            exit(1);
        }
        printf(
            "NVSHFT:mnt_data_readout.txt successfully generated and stored in : %s\n",
            asciiFilePath);
        break;
    case 3:
        afptr = fopen(asciiFilePath, "w+b");
        if (NULL == afptr) {
            printf(
                "NVSHFT: writing to file rt_data_readout.txt was not successful.\n");
            exit(1);
        }
        printf(
            "NVSHFT:rt_data_readout.txt successfully generated and stored in : %s\n",
            asciiFilePath);
        break;
    case 4:
        afptr = fopen(asciiFilePath, "w+b");
        if (NULL == afptr) {
            printf(
                "NVSHFT: writing to file dispatcher_data_readout.txt was not successful.\n");
            exit(1);
        }
        printf(
            "NVSHFT:dispatcher_data_readout.txt successfully generated and stored in : %s\n",
            asciiFilePath);
        break;
    default:
        printf("\nNVSHFT: Invalid Choice of NVSH binary file - Execution is aborting...\n");
        exit(1);
        break;
    }

    fwrite(writeBuffer, sizeof(US_CHAR), writeBuffer_location, afptr);
    fclose(afptr);
    free(writeBuffer);
    return 0;
}

US_CHAR* readASCIIFile(S_INT option, US_INT *length) {
    FILE *fptr;
    S_CHAR *dataFilePath;
    US_CHAR *dataFileBuffer;

    S_CHAR *dataFilePathCFG = "../data_files/cfg_data.txt";
    S_CHAR *dataFilePathMNT = "../data_files/mnt_data.txt";
    S_CHAR *dataFilePathRT = "../data_files/rt_data.txt";
    S_CHAR *dataFilePathCFG_DISP = "../data_files/dispatcher_data.txt";
    S_CHAR *dataFilePathINSTANCE = "../data_files/instance_data.txt";
    S_CHAR *dataFilePathTYPESPEC = "../data_files/type_data.txt";


    S_INT res, fileLength;

    initParameters();
    if (option == COMMON) {
        dataFilePath = dataFilePathCFG;
        fptr = fopen(dataFilePath, "rb");

        if (NULL == fptr) {
            printf("NVSHFT: %s does not exist.\n", dataFilePath);
            exit(1);
        }
    }
    else if (option == INSTANCE)
    {
        dataFilePath = dataFilePathINSTANCE;
        fptr = fopen(dataFilePath, "rb");

        if (NULL == fptr) {
            printf("NVSHFT: %s does not exist.\n", dataFilePath);
            exit(1);
        }
    }
    else if (option == TYPE)
    {
        dataFilePath = dataFilePathTYPESPEC;
        fptr = fopen(dataFilePath, "rb");

        if (NULL == fptr) {
            printf("NVSHFT: %s does not exist.\n", dataFilePath);
            exit(1);

        }
    }
    else if (option == MAINTENANCE)
    {
        dataFilePath = dataFilePathMNT;
        fptr = fopen(dataFilePath, "rb");

        if (NULL == fptr) {
            printf("NVSHFT: %s does not exist.\n", dataFilePath);
            exit(1);
        }
    }
    else if (option == RUNTIME) {
        dataFilePath = dataFilePathRT;
        fptr = fopen(dataFilePath, "rb");

        if (NULL == fptr) {
            printf("NVSHFT: %s does not exist.\n", dataFilePath);
            exit(1);
        }
    }
    else if (option == DISPATCHER)
    {
        dataFilePath = dataFilePathCFG_DISP;
        fptr = fopen(dataFilePath, "rb");

        if (NULL == fptr) {
            printf("NVSHFT: %s does not exist.\n", dataFilePath);
            exit(1);
        }
    }
    else {
        printf("\nNVSHFT: Invalid Choice for Reading ASCII file\n");
        exit(1);
    }

    fseek(fptr, 0, SEEK_END);
    fileLength = ftell(fptr);
    rewind(fptr);

    dataFileBuffer = (US_CHAR*)malloc(sizeof(US_CHAR) * (fileLength + 1));
    if (dataFileBuffer == NULL) {
        fputs("Could not allocate buffer for data file content\n", stderr);
        exit(2);
    }

    res = fread(dataFileBuffer, 1, fileLength, fptr);
    if (res != fileLength) {
        fprintf(stderr, "NVSHFT: Problem reading from '%s', error %d %d\n",
            dataFilePath, res, fileLength);
        exit(3);
    }
    dataFileBuffer[fileLength] = '\0';
    fclose(fptr);
    *length = fileLength;
    return dataFileBuffer;
}

US_CHAR* readNVSHFile(S_INT option, US_INT *length) {
    FILE *fptr;
    S_CHAR *dataFilePath;
    US_CHAR *dataFileBuffer;

    S_CHAR *dataFilePathCFG = "../data_files/cfg_data.bin";
    S_CHAR *dataFilePathMNT = "../data_files/mnt_data.bin";
    S_CHAR *dataFilePathRT = "../data_files/rt_data.bin";
    S_CHAR *dataFilePathCFG_DISP = "../data_files/dispatcher_data.bin";

    S_INT res, fileLength;

    initParameters();
    if (option == COMMON) {
        dataFilePath = dataFilePathCFG;
        fptr = fopen(dataFilePath, "rb");

        if (NULL == fptr) {
            printf("NVSHFT: cfg_data.bin does not exist.\n");
            exit(1);
        }
    }
    else if (option == MAINTENANCE) {
        dataFilePath = dataFilePathMNT;
        fptr = fopen(dataFilePath, "rb");

        if (NULL == fptr) {
            printf("NVSHFT: mnt_data.bin does not exist.\n");
            exit(1);
        }
    }
    else if (option == RUNTIME) {
        dataFilePath = dataFilePathRT;
        fptr = fopen(dataFilePath, "rb");

        if (NULL == fptr) {
            printf("NVSHFT: rt_data.bin does not exist.\n");
            exit(1);
        }
    }
    else if (option == DISPATCHER) {
        dataFilePath = dataFilePathCFG_DISP;
        fptr = fopen(dataFilePath, "rb");

        if (NULL == fptr) {
            printf("NVSHFT: dispatcher_data.bin does not exist.\n");
            exit(1);
        }
    }
    else {
        printf("\nNVSHFT: Invalid Choice for Reading ASCII file\n");
        exit(1);
    }

    fseek(fptr, 0, SEEK_END);
    fileLength = ftell(fptr);
    rewind(fptr);

    dataFileBuffer = (US_CHAR*)malloc(sizeof(US_CHAR) * (fileLength + 1));
    if (dataFileBuffer == NULL) {
        fputs("Could not allocate buffer for data file content\n", stderr);
        exit(2);
    }

    res = fread(dataFileBuffer, 1, fileLength, fptr);
    if (res != fileLength) {
        fprintf(stderr, "NVSHFT: Problem reading from '%s', error %d %d\n",
            dataFilePath, res, fileLength);
        exit(3);
    }
    dataFileBuffer[fileLength] = '\0';
    fclose(fptr);
    *length = fileLength;
    return dataFileBuffer;
}