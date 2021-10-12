## Will be overridden if DISPCONFIG is set in environment
## e.g. make DISPCONFIG=my_config.mk
DISPCONFIG := ./pathConfig.mk

ifneq ($(shell if ! test -f $(DISPCONFIG) ; then echo no file $(DISPCONFIG) ; unset DISPCONFIG ; fi),)
  $(error Could not find the path config file $(DISPCONFIG))
endif

include $(DISPCONFIG)

ifeq ($(TARGET), PPC)
  include $(CROSS_Linux_ppc)/distribution/include/OS_environment.mk
  LD_LIBRARY_PATH = $(CROSS_Linux_ppc)/distribution/usr/i686-pc-linux-gnu/powerpc-unknown-linux-uclibc/lib
else ifeq ($(TARGET), ARM)
  include $(CROSS_FreeBSD_arm)/distribution/include/OS_environment.mk
  LD_LIBRARY_PATH = $(CROSS_FreeBSD_arm)/distribution/lib  
endif

export LD_LIBRARY_PATH

LD_FLAGS        += -Wl,-rpath=.

EXTRA_LIBS += $(VFW_PATH)/distribution/$(OS_NAME)/lib/libVfw.so
EXTRA_LIBS += $(BDS_PATH)/lib/$(OS_NAME)/libdialib.a

ifeq ("$(OS_NAME)","Linux_ppc")
  EXTRA_LIBS += -lrt
else
  EXTRA_LIBS += -Xlinker $(CROSS_FreeBSD_arm)/distribution/sysroot/usr/lib/libc.so
endif


