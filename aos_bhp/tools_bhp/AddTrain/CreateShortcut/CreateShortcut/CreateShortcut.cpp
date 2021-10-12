// CreateShortcut.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

HRESULT CreateLink(LPCWSTR lpszPathObj, LPCWSTR lpszPathWork, LPCWSTR lpszPathLink, LPCWSTR lpszDesc); 

int _tmain(int argc, _TCHAR* argv[])
{
    if (argc == 5)  // Program name + command-line arguments
    {
        CoInitialize(NULL);

        CreateLink(argv[1], argv[2], argv[3], argv[4]);
        wprintf(L"CreateLink(%ls, %ls, %ls, %ls)\n",argv[1], argv[2], argv[3], argv[4]);
        return EXIT_SUCCESS;
    }
    else
    {
        printf("CreateShortcut usage:\n");
        printf("CreateShortcut <targetpath> <workdirpath> <shortcutfilename> <shortcutdescr>\n");
        return EXIT_FAILURE;
    }	
}
HRESULT CreateLink(LPCWSTR lpszPathObj, LPCWSTR lpszPathWork, LPCWSTR lpszPathLink, LPCWSTR lpszDesc) 
{ 
    HRESULT hres; 
    IShellLink* psl; 
 
    // Get a pointer to the IShellLink interface. It is assumed that CoInitialize
    // has already been called.
    hres = CoCreateInstance(CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER, IID_IShellLink, (LPVOID*)&psl); 
    if (SUCCEEDED(hres)) 
    { 
        IPersistFile* ppf; 
 
        // Set the path to the shortcut target and add the description. 
        psl->SetPath(lpszPathObj); 
        psl->SetWorkingDirectory(lpszPathWork);
        psl->SetDescription(lpszDesc); 
 
        // Query IShellLink for the IPersistFile interface, used for saving the 
        // shortcut in persistent storage. 
        hres = psl->QueryInterface(IID_IPersistFile, (LPVOID*)&ppf); 
 
        if (SUCCEEDED(hres)) 
        { 
            // WCHAR wsz[MAX_PATH]; 
 
            // Ensure that the string is Unicode. 
            // MultiByteToWideChar(CP_ACP, 0, lpszPathLink, -1, wsz, MAX_PATH); 
            
            // Add code here to check return value from MultiByteWideChar 
            // for success.
 
            // Save the link by calling IPersistFile::Save. 
            hres = ppf->Save(lpszPathLink, TRUE); 
            ppf->Release(); 
        }
        else
            wprintf(L"psl->QueryInterface failed(%ld)",hres);
        psl->Release(); 
    } 
    else
        wprintf(L"psl->CoCreateInstance failed(%ld)",hres);

    return hres; 
} 



