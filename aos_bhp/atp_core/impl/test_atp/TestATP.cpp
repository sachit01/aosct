// TestATP.cpp : main project file.

#include "stdio.h"

#include "ATPBase.hpp"
#include "RadioHandler.hpp"

using namespace ATP;

int main(void)
{
    printf("RadioHandler::instance().init()\n");
    RadioHandler::instance().init();

    while (true)
    {
      printf("RadioHandler::instance().run()\n");
      RadioHandler::instance().run();
      printf("Hit any key to continue\n");
      getchar();
    }

    return 0;
}
