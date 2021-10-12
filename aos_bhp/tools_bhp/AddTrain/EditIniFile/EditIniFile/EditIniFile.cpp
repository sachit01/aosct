// EditIniFile.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"


int _tmain(int argc, _TCHAR* argv[])
{
    if (argc == 5)
    {
        WritePrivateProfileStringW(argv[1], argv[2], argv[3], argv[4]);
        wprintf(L"WritePrivateProfileStringW(%ls, %ls, %ls, %ls)\n",argv[1], argv[2], argv[3], argv[4]);
        return EXIT_SUCCESS;
    }
    else
    {
        printf("EditIniFile usage:\n");
        printf("EditIniFile <chapter> <item> <stringvalue> <filename>\n");
        return EXIT_FAILURE;
    }	
}

