## Will be overridden if ATPCONFIG is set in environment
## e.g. make ATPCONFIG=my_config.mk
ATPCONFIG := ./pathConfig.mk

ifneq ($(shell if ! test -f $(ATPCONFIG) ; then echo no file $(ATPCONFIG) ; unset ATPCONFIG ; fi),)
  $(error Could not find the path config file $(ATPCONFIG))
endif

include $(ATPCONFIG)

ifeq ($(TARGET), PPC)
  include $(CROSS_Linux_ppc)/distribution/include/OS_environment.mk
  LD_LIBRARY_PATH = $(CROSS_Linux_ppc)/distribution/usr/i686-pc-linux-gnu/powerpc-unknown-linux-uclibc/lib
  EXTRA_LIBS += -L$(SPL_ROOT)/lib/atpcuA_ab_tbsw_sil4__/ -lsplA
else ifeq ($(TARGET), ARM)
  include $(CROSS_FreeBSD_arm)/distribution/include/OS_environment.mk
  LD_LIBRARY_PATH = $(CROSS_FreeBSD_arm)/distribution/i686-pc-linux-gnu/arm-unknown-freebsd8/lib
  EXTRA_LIBS += -L$(SPL_ROOT)/lib/atpcuB_ab_tbsw_sil4__/ -lsplB
else ifeq ($(TARGET), WIN32)
  MY_ADDED_DEFINES += -DWIN32 -m32
  OS_NAME        = WIN32
else ifeq ($(TARGET), HOST)
  OS_NAME        = Linux_x86
else
  $(error TARGET=[PPC|ARM|WIN32|HOST])
endif

export LD_LIBRARY_PATH

LD_FLAGS        += -Wl,-rpath=.

ifneq ($(ENV), SIL)
  EXTRA_LIBS += $(VFW_PATH)/distribution/$(OS_NAME)/lib/libVfw.so
  EXTRA_LIBS += $(BDS_PATH)/lib/$(OS_NAME)/libdialib.a
endif

ifeq ($(TARGET),PPC)
  EXTRA_LIBS += -lrt
else ifeq ($(TARGET),ARM)
  EXTRA_LIBS += -Xlinker $(CROSS_FreeBSD_arm)/distribution/sysroot/usr/lib/libc.so
endif

# set VIO lib path
ifneq ($(ENV), HIL)
ifneq ($(ENV), SIL)
  EXTRA_LIBS += $(VIOH_PATH)/lib/$(OS_NAME)/libvioclient.so
endif
endif
