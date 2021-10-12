/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The main function will defines the entry point for dispatcher Application.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
*2016-11-26    saprasad     Remove compilation  error and warning for  Linux Env
*2017-01-03    spandita     Added Preinit() call
*******************************************************************************/


/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <stdio.h>
#include "disp_application.hpp"
#include <iostream>
#ifdef WIN32
#include <windows.h>
#else
#include <unistd.h>
#define Sleep(a) usleep(a)
#endif
int main(void)
{
  uint8_t state = 0;
  bool running = true;

  while (running)
  {
    switch (state)
    {
    case 0:
      printf("Adding components...\n");

      Dispatcher::DispApplication::instance();
      Dispatcher::DispApplication::instance().addAllComponents();
      state = 1;
      break;

    case 1:
      printf("Running preInit...\n");
      Dispatcher::DispApplication::instance().preInit();
      state = 2;
      break;
    case 2:
      printf("Initializing Dispatcher application...\n");
      if (Dispatcher::DispApplication::instance().init())
      {
        state = 3;
        printf("Running Dispatcher Application...\n");
      }
      break;

    case 3:
      // Run Dispatcher
      Dispatcher::DispApplication::instance().run();
      break;

    default:
      printf("Invalid state in main\n");
      break;

    }
    Sleep(100);
  }

  return 0;
}
