#include <vfw_version.h>
#include <vfw_halt.h>

int main(const int argc, const char* const* const argv)
{
  const char* module_C = __FILE__;
  const char* message;

  VFW_ASSERT_VERSION();

  if (argc > 2)
  {
    message = "platform_halt: too many arguments";
  }
  else if (argc == 2)
  {
    message = argv[1];
  }
  else
  {
    message = "platform_halt: no halt reason given";
  }

  VFW_HALT((message));

  return 0;
}
