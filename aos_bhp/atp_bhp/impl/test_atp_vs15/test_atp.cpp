// test_atp.cpp : Defines the entry point for the console application.
//
#include "atp_main.hpp"


int main(void)
{
  const char* argv[3] = { "atp_bhp_sil", "-a", static_cast<char*>(NULL) };

  ATP::AtpMain::instance().mainFunction(2, argv);

  return 0;
}
