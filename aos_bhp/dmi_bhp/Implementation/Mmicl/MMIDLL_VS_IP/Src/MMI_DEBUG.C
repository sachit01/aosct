/****************************************************************************
*           (C) COPYRIGHT Bombardier , SWEDEN 2011
*           ======================================
*
*    The copyright to the computer program herein is the
*    property of Bombardier, Sweden. All rights reserved.
*    The program may be used and/or copied only with the
*    written permission from Bombardier, or in accordance
*    with the terms and conditions stipulated in the
*    agreement/contract under which the program has been
*    supplied.
*
*
*    MODULE NAME:  mmi_debug.c
*
*    REVISION:     1.0
*
*    PREPARED:     Bo Hermansson
*
*    DESCRIPTION:  Debug functions used to display trace information in a separate window.
*
****************************************************************************/

/****************************************************************************
*
*    REVISION HISTORY :
*
*    Rev   Date        Name      Measures
*    -----------------------------------------------------------------------
*    1.0  2011-08-25   Bo H	     Start
****************************************************************************/


/* ========================================================================= */
/* Console handling functions. */


#ifdef DEBUG_CONSOLE

#include <stdio.h>
#include <windows.h>
#include "mmi_debug.h"

static HANDLE hConsole = INVALID_HANDLE_VALUE;
static const COORD consoleSize = {80,5000}; // Columns, rows
static const WORD consoleBackground = BACKGROUND_RED + BACKGROUND_GREEN + BACKGROUND_BLUE + BACKGROUND_INTENSITY;
static int prfVerbose = 1;

void prfInit(const char *title, int verboseI) 
{
    SMALL_RECT consoleRect; // = {0,0,consoleSize.X-1,consoleSize.Y-1};
    COORD zCord = {0,0};
    DWORD z;

    prfVerbose = verboseI;

  /* Behaviour in a DLL called from a Delphi programme:
   * Calling GetStdHandle() before AllocConsole works (does not return
   * INVALID_HANDLE_VALUE), but printing to it doesn't show anything.
   * Calling AllocCOnsole() returns True (OK) and displays a window after
   * which it works.
   */

    if (!AllocConsole()) 
    {
        z = GetLastError();
            /* We continue anyway as there may already be a console. */
    }

    hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hConsole == INVALID_HANDLE_VALUE)
        return;

    if (title)
        SetConsoleTitle(title);

    consoleRect.Left = 0; consoleRect.Top = 0;
    consoleRect.Right = consoleSize.X-1; consoleRect.Bottom = consoleSize.Y-1;

    SetConsoleScreenBufferSize((HANDLE)hConsole, consoleSize);
    SetConsoleWindowInfo((HANDLE)hConsole, true, &consoleRect);
    SetConsoleTextAttribute((HANDLE)hConsole, consoleBackground);

        /* Assign the whole screen the background color. */
    FillConsoleOutputAttribute((HANDLE)hConsole, consoleBackground, consoleSize.X*consoleSize.Y, zCord, &z);
}

/* Prints as printf but different colors depending on mode.
 * It can be ensured that a single printout never is interrupted by another
 * thread's (assuming all threads have the same priority).
 * For output with several calls, start by call prf with prfBegin; the printing
 * must be ended by prfEnd (fmt is ignored in both cases).
 * After printing, the prioroty is always reset to THREAD_PRIORITY_NORMAL.
 *
 * verbose >= 0: Print only prtInfo and prtError.
 *         >= 1: Print everything.
 *         >= 2: Prevent task switching while printing.
 *
 * If hConsole == INVALID_HANDLE_VALUE, nothing is displayed (this is the
 * case if prfInit() hasn't been called).
 */
void prf(const prfT p, const char *fmt, ...) 
{
    va_list argptr;
    int oldPriority;

    if (hConsole == INVALID_HANDLE_VALUE) 
    {
        return;
    }

    if (prfVerbose >= 2) 
    {
        oldPriority = GetThreadPriority(GetCurrentThread());
        if (p == prfBegin) 
        {
            SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
            return;
        } 
        else if (p == prfEnd) 
        {
            SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_NORMAL);
            return;
        }

            /* By rising the priority of the calling thread, we makes sure the printout is not interrupted. */
        SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
    } 
    else if (p == prfBegin || p == prfEnd)
        return;

            /* Adding WORD's seems to result in DWORD or something which results in warnings below, thus the casts. */
    switch(p) 
    {
        case prfInfo:
            SetConsoleTextAttribute((HANDLE)hConsole, consoleBackground); /* Black text  */
            break;
            
        case prfRcv:
            if (prfVerbose < 1)
                return;
            SetConsoleTextAttribute((HANDLE)hConsole, (WORD)(consoleBackground + FOREGROUND_RED + FOREGROUND_INTENSITY));
            break;
            
        case prfTx:
            if (prfVerbose < 1)
                return;
            SetConsoleTextAttribute((HANDLE)hConsole, (WORD)(consoleBackground + FOREGROUND_BLUE + FOREGROUND_INTENSITY));
            break;
            
        case prfError:
        default:
            SetConsoleTextAttribute((HANDLE)hConsole, (WORD)(consoleBackground + FOREGROUND_GREEN + FOREGROUND_INTENSITY));
            break;
    }
    
    va_start(argptr, fmt);
    {
        char buf[1024];    /* ... so we use WriteConsole instead. */
        DWORD z;
        vsprintf(buf, fmt, argptr);
        WriteConsole(hConsole, buf, strlen(buf), &z, NULL);
    }
    
    va_end(argptr);

    if (prfVerbose >= 2)
        SetThreadPriority(GetCurrentThread(), oldPriority);
}


void prtBundle(uint16 length, void *bundle) 
{
    int i;
    prf(prfTx, "%ld<", GetTickCount());
    for (i = 0; i < length; i++) 
    {
        if (i)
            prf(prfTx, " ");
        prf(prfTx, "%02x", ((byte*)bundle)[i]);
    }
    prf(prfTx, ">\n");
}

void prtRBundle(uint16 length, void *bundle) 
{
    int i;
    prf(prfRcv, "%ld[", GetTickCount());
    for (i = 0; i < length; i++) 
    {
        if (i)
            prf(prfRcv, " ");
        prf(prfRcv, "%02x", ((byte*)bundle)[i]);
    }
    prf(prfRcv, "]\n");
}

#endif // DEBUG_CONSOLE
