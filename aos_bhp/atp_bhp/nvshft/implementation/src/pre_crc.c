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
* %name: preCRC.c %
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
* 1.2     130227  wahmad   Few Descriptions added + NVSHFT converted to nvshft in printf
* 1.3     130314  wahmad   NVSHFT version1.1 according to parameters updated 1.2 of CFPARAM 
*						   and 1.4 of IFSATPCU document
* 1.4     130603  wahmad   Changes due to new requirements added for NVSHFT such as 
						   interface_version and read_only_data.
* 1.5	  130604  wahmad   Lint corrections have been made
* 1.6	  130605  wahmad   Lint corrections and few more corrections in the code
* 1.7     140626  wahmad   Compilable on B-board
* 1.8     170127  nsyed    Adapted to BHP Project (Ver 1.0)
*******************************************************************************/

#include "nvshft.h"
#include "pre_crc.h"
#include "crc.h"

US_INT nvshft_strlen(US_CHAR *p)
{
	US_INT s=0U;

	if(p!=NULL)
	{
		while(*p++)s++;
	}
	
	return s;
}

void initParameters()
{
	S_INT i;

	for(i=0; i<NOP_MNT; i++)
	{
		parameterSize_MNT[i] = mnt[i].parameterSize_MNT;
		parameterName_MNT[i] = mnt[i].parameterName_MNT;
	}

	for(i=0; i<NOP_RT; i++)
	{
		parameterSize_RT[i] = rt[i].parameterSize_RT;
		parameterName_RT[i] = rt[i].parameterName_RT;
	}

	for(i=0; i<NOP_COMMON; i++)
	{
		parameterSize_COMMON[i] = common[i].parameterSize_COMMON;
		parameterName_COMMON[i] = common[i].parameterName_COMMON;
	}

	for(i=0; i<NOP_CFG_DISP; i++)
	{
		parameterSize_CFG_DISP[i] = cfg_disp[i].parameterSize_CFG_DISP;
		parameterName_CFG_DISP[i] = cfg_disp[i].parameterName_CFG_DISP;
	}
    for (i=0; i< NOP_TYPE;i++)
        {
        parameterSize_TYPE[i] = typespec[i].parameterSize_TYPE;
        parameterName_TYPE[i] = typespec[i].parameterName_TYPE;
        }
    for (i = 0; i< NOP_INSTANCE ; i++)
    {
        parameterSize_INSTANCE[i] = instance[i].parameterSize_INSTANCE;
        parameterName_INSTANCE[i] = instance[i].parameterName_INSTANCE;
    }
}

US_CHAR*
removeComments(const US_CHAR* str, US_INT len) {
  US_CHAR* strWithReplaced;
  US_CHAR *p;
  US_INT i;
  S_INT j=0;

  strWithReplaced = (US_CHAR*) malloc(sizeof(US_CHAR) * (len+1));
  memset(strWithReplaced, 0, sizeof(US_CHAR)*(len+1));
  for(i=0; i<len; i++)
  {
    if(str[i]=='#')
    {
	  /*p = (US_CHAR*)strchr((S_CHAR*)&str[i], '\n');*/
	  p = (US_CHAR*)memchr(&str[i], '\n', len);
      i = p - str;
      strWithReplaced[j]='\n';
      j++;
    }
    else
    {
      strWithReplaced[j]=str[i];
      j++;
    }
  }
  return strWithReplaced;
}

US_CHAR*
removeSpacesCarriages(const US_CHAR* str, US_INT len) {
  US_CHAR* strWithReplaced;
  US_INT i;
  S_INT j=0;
  
  strWithReplaced = (US_CHAR*) malloc(sizeof(US_CHAR) * (len+1));
  memset(strWithReplaced, 0, sizeof(US_CHAR)*(len+1));
  for(i=0; i<len; i++)
  {
    if((str[i]==' ') || (str[i]=='\r') || (str[i]=='\t'))/* || (str[i]=='\n'))*/
    {
    }
    else
    {
      strWithReplaced[j]=str[i];
      j++;
    }
  }
  return strWithReplaced;
}

US_CHAR*
removeSpacesCarriages1(const US_CHAR* str, US_INT len) {
  US_CHAR* strWithReplaced;
  US_INT i;
  S_INT j=0;
  
  strWithReplaced = (US_CHAR*) malloc(sizeof(US_CHAR) * (len+1));
  memset(strWithReplaced, 0, sizeof(US_CHAR)*(len+1));
  for(i=0; i<len; i++)
  {
    if((str[i]==' ') || (str[i]=='\r') || (str[i]=='\t') || (str[i]=='\n'))
    {
    }
    else
    {
      strWithReplaced[j]=str[i];
      j++;
    }
  }
  return strWithReplaced;
}


US_CHAR*
preCRC(const US_CHAR* str, S_INT len, US_INT noOfParams, US_CHAR *fileVersion, US_INT *crcInFile) {
  US_CHAR* strWithReplaced;
  US_CHAR* ch;
  US_INT i,j=0;
  S_CHAR formattedString[1820]="%s"; /* 455*4 -- (max:noOfParams * j(increment))*/
  US_CHAR tmpCRCInFile[20]={0};
  US_INT pow16[8]= {1, 16, 256, 4096, 65536, 1048576, 16777216, 268435456};

  ch = (US_CHAR*) malloc(sizeof(US_CHAR) * (len+1));
  memset(ch, 0, sizeof(US_CHAR)*(len+1));
  
  strWithReplaced = removeComments(str, len);

  if (sscanf((const S_CHAR*)strWithReplaced, (const S_CHAR*)"%*s %s", fileVersion) != 1)
  {
    fileVersion[0] = '\0';
  }

  strWithReplaced = removeSpacesCarriages(strWithReplaced, nvshft_strlen(strWithReplaced));
  
  for(i=0; i<(noOfParams+1); i++)
  {
    sscanf((const S_CHAR*)strWithReplaced, (const S_CHAR*)formattedString, &ch[nvshft_strlen(ch)]);
    /*strcpy(&formattedString[j], " %*s %s");*/
	memcpy(&formattedString[j], " %*s %s", 7);
    j = j+4;
  }
  
  sscanf((const S_CHAR*)strWithReplaced, (const S_CHAR*)formattedString, tmpCRCInFile);
	
  
  if((tmpCRCInFile[0] == 'C') && (tmpCRCInFile[1] == 'R') && (tmpCRCInFile[2] == 'C'))
  {
    j=0;
    for(i=10; i>2 ; i--)
    {
      switch(tmpCRCInFile[i])
      {
      case '0':
        j = j + (0*(pow16[10-i]));
        break;
      case '1':
        j = j + (1*(pow16[10-i]));
        break;
      case '2':
        j = j + (2*(pow16[10-i]));
        break;
      case '3':
        j = j + (3*(pow16[10-i]));
        break;
      case '4':
        j = j + (4*(pow16[10-i]));
        break;
      case '5':
        j = j + (5*(pow16[10-i]));
        break;
      case '6':
        j = j + (6*(pow16[10-i]));
        break;
      case '7':
        j = j + (7*(pow16[10-i]));
        break;
      case '8':
        j = j + (8*(pow16[10-i]));
        break;
      case '9':
        j = j + (9*(pow16[10-i]));
        break;
      case 'a': case 'A':
        j = j + (10*(pow16[10-i]));
        break;
      case 'b': case 'B':
        j = j + (11*(pow16[10-i]));
        break;
      case 'c': case 'C':
        j = j + (12*(pow16[10-i]));
        break;
      case 'd': case 'D':
        j = j + (13*(pow16[10-i]));
        break;
      case 'e': case 'E':
        j = j + (14*(pow16[10-i]));
        break;
      case 'f': case 'F':
        j = j + (15*(pow16[10-i]));
        break;
      default:
        printf("\nNVSHFT: Unable to extract CRC from text file\n"); /* Have to discuss with Håkan - Most probably Execution should never come to this point*/
        exit(1);
        break;
      }
    }
    *crcInFile = j; /*atoi(&tmpCRCInFile[3]);*/
  }
  else
  {
    *crcInFile = 0U;
  }

  /*  sscanf(strWithReplaced, "%s", &tempVersion);
    *fileVersion = atof(tempVersion);*/
  free(strWithReplaced);
  return ch;
}

S_INT checkCRCVersionASCII(const US_CHAR* str, S_INT len, S_INT option)
{
  US_CHAR fileVersion[80];
  US_CHAR versionString[80];
  US_CHAR* strForCRC;
  US_INT calculatedCRC, crcInFile;
  US_INT noOfParams;
  S_INT return_val = 1;

  crcInit();

  switch(option)
  {
  case 1:
    noOfParams = NOP_COMMON;
    return_val = 0;
    break;
  case 2:
    noOfParams = NOP_MNT;
    return_val = 0;
    break;
  case 3:
    noOfParams = NOP_RT;
    return_val = 0;
    break;
  case 4:
    noOfParams = NOP_CFG_DISP;
    return_val = 0;
    break;
  case 5:
    noOfParams = NOP_INSTANCE;
    return_val = 0;
    break;
  case 6:
    noOfParams = NOP_TYPE;
    return_val = 0;
    break;
  default:
    printf("\nNVSHFT: Invalid option for file selection\n");
    exit(1);
    break;
  }

  strForCRC = preCRC(str, len, noOfParams, fileVersion, &crcInFile);
  calculatedCRC = crcFast(strForCRC, nvshft_strlen(strForCRC));

//  if(calculatedCRC != crcInFile)
//  {
//    printf("\nNVSHFT: Input ASCII file contains incorrect CRC.\n");
//    exit(1);
//  }

  (void) snprintf(versionString, sizeof(versionString), "%d.%d", CFPARAM_MAJOR, CFPARAM_MINOR);

  if (strncmp(versionString, fileVersion, sizeof(versionString)) != 0)
  {
    printf("\nNVSHFT: Input ASCII file version %s not compatible with version %s of NVSHFT.\n",
      fileVersion, versionString);
    exit(1);
  }

  free(strForCRC);
  return return_val;
}

S_INT checkCRCVersionNVSH(US_CHAR* str, S_INT len, S_INT option)
{
  uint64_t calculatedCRC;
  S_INT loc_crcInFile = (len)-8;
  US_CHAR calculatedCRC_String[10]={0};
  S_INT return_val = 1;
  
  calculatedCRC = calcCrc64(str+4, (len)-(8+4));
  
  calculatedCRC_String[0] = (US_CHAR)(((uint64_t)calculatedCRC)>>56);
  calculatedCRC_String[1] = (US_CHAR)(((uint64_t)calculatedCRC)>>48);
  calculatedCRC_String[2] = (US_CHAR)(((uint64_t)calculatedCRC)>>40);
  calculatedCRC_String[3] = (US_CHAR)(((uint64_t)calculatedCRC)>>32);
  calculatedCRC_String[4] = (US_CHAR)(((uint64_t)calculatedCRC)>>24);
  calculatedCRC_String[5] = (US_CHAR)(((uint64_t)calculatedCRC)>>16);
  calculatedCRC_String[6] = (US_CHAR)(((uint64_t)calculatedCRC)>>8);
  calculatedCRC_String[7] = (US_CHAR)((uint64_t)calculatedCRC);
  
  switch(option)
  {
  case 1:
//    if((strncmp((const S_CHAR*)calculatedCRC_String, (const S_CHAR*)&str[loc_crcInFile], 8)) || ((strncmp((const S_CHAR*)calculatedCRC_String, (const S_CHAR*)&str[len-8], 8))))
//    {
//      printf("\nNVSHFT: Input NVSH file contains incorrect CRC.\n");
//      exit(1);
//    }
    
    if(str[47]!= CFPARAM_MAJOR || str[48]!=CFPARAM_MINOR)
    {
      printf("\nNVSHFT: Input NVSH file not compatible with version of NVSHFT.\n");
      exit(1);
    }    

    if(str[4]!= 0 || str[5]!=0 || str[6]!=0 || str[7]!=INTERFACE_VERSION)
    {
      printf("\nNVSHFT: Input NVSH file format not compatible with NVSHFT.\n");
      exit(1);
    }

    return_val = 0;
    break;
  case 2:
//    if((strncmp((const S_CHAR*)calculatedCRC_String, (const S_CHAR*)&str[loc_crcInFile], 8)) || ((strncmp((const S_CHAR*)calculatedCRC_String, (const S_CHAR*)&str[len-8], 8))))
//    {
//      printf("\nNVSHFT: Input NVSH file contains incorrect CRC.\n");
//      exit(1);
//    }
    
    if(str[47]!= CFPARAM_MAJOR || str[48]!=CFPARAM_MINOR)
    {
      printf("\nNVSHFT: Input NVSH file not compatible with version of NVSHFT.\n");
      exit(1);
    }

    if(str[4]!= 0 || str[5]!=0 || str[6]!=0 || str[7]!=INTERFACE_VERSION)
    {
      printf("\nNVSHFT: Input NVSH file format not compatible with NVSHFT.\n");
      exit(1);
    }
    return_val = 0;
    break;
  case 3:
//    if((strncmp((const S_CHAR*)calculatedCRC_String, (const S_CHAR*)&str[loc_crcInFile], 8)) || ((strncmp((const S_CHAR*)calculatedCRC_String, (const S_CHAR*)&str[len-8], 8))))
//    {
//      printf("\nNVSHFT: Input NVSH file contains incorrect CRC.\n");
//      exit(1);
//    }
    
    if(str[45]!= CFPARAM_MAJOR || str[46]!=CFPARAM_MINOR)
    {
      printf("\nNVSHFT: Input NVSH file not compatible with version of NVSHFT.\n");
      exit(1);
    }

    if(str[4]!= 0 || str[5]!=0 || str[6]!=0 || str[7]!=INTERFACE_VERSION)
    {
      printf("\nNVSHFT: Input NVSH file format not compatible with NVSHFT.\n");
      exit(1);
    }

    return_val = 0;
    break;
  case 4:
//      if((strncmp((const S_CHAR*)calculatedCRC_String, (const S_CHAR*)&str[loc_crcInFile], 8)) || ((strncmp((const S_CHAR*)calculatedCRC_String, (const S_CHAR*)&str[len-8], 8))))
//      {
//        printf("\nNVSHFT: Input NVSH file contains incorrect CRC.\n");
//        exit(1);
//      }

      if(str[57]!= CFPARAM_MAJOR || str[58]!=CFPARAM_MINOR)
      {
    	printf("\nNVSHFT: Input NVSH file not compatible with version of NVSHFT.\n");
        exit(1);
      }

      if(str[4]!= 0 || str[5]!=0 || str[6]!=0 || str[7]!=INTERFACE_VERSION)
      {
        printf("\nNVSHFT: Input NVSH file format not compatible with NVSHFT.\n");
        exit(1);
      }

      return_val = 0;
      break;
  default:
    printf("\nNVSHFT: Invalid option for file selection\n");
    exit(1);
    break;
  }
  
  return return_val;
}
