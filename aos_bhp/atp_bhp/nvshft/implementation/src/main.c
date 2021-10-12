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
* %name: main.c %
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
* Version  Date   		 Sign    Change description
* 1.0     130222  		wahmad   Initially created
* 1.1     270222  		wahmad   Few descriptions added
* 1.2	  130604  		wahmad   Lint corrections have been made
* 1.3     2017-01-27    nsyed    Adapted to BHP Project (Ver 1.0)
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pre_crc.h"
#include "nvshft.h"

S_INT main(S_INT argc, S_CHAR *argv[])
{
  US_INT fileLength, res;
  S_INT parseArg = -1;
  US_CHAR *dataFileBuffer;
  S_INT option[6] = {COMMON, MAINTENANCE, RUNTIME, DISPATCHER, INSTANCE, TYPE};
  S_INT i;

  /*
  parseArg = 1 ---> All NVSH
  parseArg = 2 ---> All ASCII
  parseArg = 3 ---> Version
  parseArg = 4 ---> Help

  parseArg = 5 ---> NVSH - Common
  parseArg = 6 ---> NVSH - Maintenance
  parseArg = 7 ---> NVSH - Runtime
  parseArg = 8 ---> NVSH - Configuration Dispatcher

  parseArg = 9 ---> ASCII - Common
  parseArg = 10 ---> ASCII - Maintenance
  parseArg = 11 ---> ASCII - Runtime
  parseArg = 12 ---> ASCII - Configuration Dispatcher

  parseArg = 13 ---> NVSH - InstanceSpecific
  parseArg = 14 ---> NVSH - TypeSpecific

  parseArg = -1;
  */
  switch(argc)
  {
  case 1:
    parseArg = -1;
    break;
  case 2:
    if(!strncmp(argv[1], "-n", nvshft_strlen((US_CHAR*)argv[1]))) parseArg = 1;
    else if(!strncmp(argv[1], "-a", nvshft_strlen((US_CHAR*)argv[1]))) parseArg = 2;
    else if(!strncmp(argv[1], "-v", nvshft_strlen((US_CHAR*)argv[1]))) parseArg = 3;
    else if(!strncmp(argv[1], "-h", nvshft_strlen((US_CHAR*)argv[1]))) parseArg = 4;
    else parseArg = -1;
    break;
  case 3:
    if (!strncmp(argv[1], "-n", nvshft_strlen((US_CHAR*)argv[1])))
    {
      if (!strncmp(argv[2], "-c", nvshft_strlen((US_CHAR*)argv[2]))) parseArg = 5;
      else if (!strncmp(argv[2], "-m", nvshft_strlen((US_CHAR*)argv[2]))) parseArg = 6;
      else if (!strncmp(argv[2], "-r", nvshft_strlen((US_CHAR*)argv[2]))) parseArg = 7;
      else if (!strncmp(argv[2], "-d", nvshft_strlen((US_CHAR*)argv[2]))) parseArg = 8;
      else if (!strncmp(argv[2], "-i", nvshft_strlen((US_CHAR*)argv[2]))) parseArg = 13;
      else if (!strncmp(argv[2], "-t", nvshft_strlen((US_CHAR*)argv[2]))) parseArg = 14;
      else parseArg = -1;    }
    else
      if (!strncmp(argv[1], "-a", nvshft_strlen((US_CHAR*)argv[1])))
      {
        if (!strncmp(argv[2], "-c", nvshft_strlen((US_CHAR*)argv[2]))) parseArg = 9;
        else if (!strncmp(argv[2], "-m", nvshft_strlen((US_CHAR*)argv[2]))) parseArg = 10;
        else if (!strncmp(argv[2], "-r", nvshft_strlen((US_CHAR*)argv[2]))) parseArg = 11;
        else if (!strncmp(argv[2], "-d", nvshft_strlen((US_CHAR*)argv[2]))) parseArg = 12;
        else if (!strncmp(argv[2], "-is", nvshft_strlen((US_CHAR*)argv[2]))) parseArg = 15;

        else parseArg = -1;
      }
      else parseArg = -1;
      break;
  default:
    parseArg = -1;
    break;
  }

  switch(parseArg)
  {
  case -1:
    printf("\nNVSHFT: Invalid argument(s)\n");
    showHelp();
    exit(1);
    break;
  case 1:
    for(i=0; i<6 ; i++)
    {
      dataFileBuffer = readASCIIFile(option[i], &fileLength);
      res = checkCRCVersionASCII(dataFileBuffer, fileLength, option[i]);

      if(res==0)
      {
        writeNVSHFile(dataFileBuffer, fileLength, option[i]);
      }
    }
    break;
  case 2:
    for(i=0; i<6; i++)
    {
      dataFileBuffer = readNVSHFile(option[i], &fileLength);
      res = 0;//checkCRCVersionNVSH(dataFileBuffer, fileLength, option[i]);

      if(res==0)
      {
        writeASCIIFile(dataFileBuffer, fileLength, option[i]);
      }
    }
    break;
  case 3:
    showVersion();
    break;
  case 5:
    dataFileBuffer = readASCIIFile(option[0], &fileLength);
    res = checkCRCVersionASCII(dataFileBuffer, fileLength, option[0]);
    
    if(res==0)
    {
      writeNVSHFile(dataFileBuffer, fileLength, option[0]);
    }
    break;
  case 6:
    dataFileBuffer = readASCIIFile(option[1], &fileLength);
    res = checkCRCVersionASCII(dataFileBuffer, fileLength, option[1]);
    
    if(res==0)
    {
      writeNVSHFile(dataFileBuffer, fileLength, option[1]);
    }
    
    break;
  case 7:
    dataFileBuffer = readASCIIFile(option[2], &fileLength);
    res = checkCRCVersionASCII(dataFileBuffer, fileLength, option[2]);
    
    if(res==0)
    {
      writeNVSHFile(dataFileBuffer, fileLength, option[2]);
    }
    break;
  case 8:
    dataFileBuffer = readASCIIFile(option[3], &fileLength);
    res = checkCRCVersionASCII(dataFileBuffer, fileLength, option[3]);

    if(res==0)
    {
      writeNVSHFile(dataFileBuffer, fileLength, option[3]);
    }
    break;
  case 13:
      dataFileBuffer = readASCIIFile(option[4], &fileLength);
      res = checkCRCVersionASCII(dataFileBuffer, fileLength, option[4]);

      if (res == 0)
      {
          writeNVSHFile(dataFileBuffer, fileLength, option[4]);
      }
      break;
  case 14:
      dataFileBuffer = readASCIIFile(option[5], &fileLength);
      res = checkCRCVersionASCII(dataFileBuffer, fileLength, option[5]);

      if (res == 0)
      {
          writeNVSHFile(dataFileBuffer, fileLength, option[5]);
      }
      break;
  case 9:
    dataFileBuffer = readNVSHFile(option[0], &fileLength);
    res = checkCRCVersionNVSH(dataFileBuffer, fileLength, option[0]);
    if(res==0)
    {
      writeASCIIFile(dataFileBuffer, fileLength, option[0]);
    }
    break;
  case 15:
      dataFileBuffer = readNVSHFile(option[4], &fileLength);
      res = 0;
      if (res == 0)
      {
          writeASCIIFile(dataFileBuffer, fileLength, option[4]);
      }
      break;
  case 10:
    dataFileBuffer = readNVSHFile(option[1], &fileLength);
    res = checkCRCVersionNVSH(dataFileBuffer, fileLength, option[1]);
    if(res==0)
    {
      writeASCIIFile(dataFileBuffer, fileLength, option[1]);

    }
    break;
  case 11:
    dataFileBuffer = readNVSHFile(option[2], &fileLength);
    res = checkCRCVersionNVSH(dataFileBuffer, fileLength, option[2]);
    if(res==0)
    {
      writeASCIIFile(dataFileBuffer, fileLength, option[2]);
    }
    break;
  case 12:
    dataFileBuffer = readNVSHFile(option[3], &fileLength);
    res = checkCRCVersionNVSH(dataFileBuffer, fileLength, option[3]);
    if(res==0)
    {
      writeASCIIFile(dataFileBuffer, fileLength, option[3]);
    }
    break;
    case 4:
  default:
    showHelp();
    exit(1);
    break;
  }
  return 0;
}
