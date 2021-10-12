ifeq ($(TARGET),PPC)
OPTIMIZATION    := -pipe -fauto-inc-dec -fcompare-elim -fcprop-registers -fdce -fdefer-pop -fguess-branch-probability -fif-conversion2 -fif-conversion -fipa-pure-const -fipa-profile -fipa-reference -fmerge-constants -fsplit-wide-types -ftree-ch -ftree-copyrename -ftree-dominator-opts -ftree-fre -ftree-sra -ftree-pta -ftree-ter -fomit-frame-pointer -fthread-jumps -falign-functions -falign-jumps -falign-loops -falign-labels -fcaller-saves -fcrossjumping -fno-delete-null-pointer-checks -fdevirtualize -fgcse -fgcse-lm -finline-small-functions -findirect-inlining -foptimize-sibling-calls -fpartial-inlining -fpeephole2 -fregmove -freorder-blocks -freorder-functions -frerun-cse-after-loop -fschedule-insns -fschedule-insns2 -fstrict-aliasing -ftree-switch-conversion -ftree-pre
else
OPTIMIZATION    := -pipe -fcprop-registers -fdefer-pop -fguess-branch-probability -fif-conversion2 -fif-conversion -fmerge-constants -ftree-ch -ftree-copyrename -ftree-dominator-opts -ftree-fre -ftree-sra -ftree-ter -fomit-frame-pointer -fthread-jumps -falign-functions -falign-jumps -falign-loops -falign-labels -fcaller-saves -fcrossjumping -fno-delete-null-pointer-checks -fgcse -fgcse-lm -foptimize-sibling-calls -fpeephole2 -fregmove -freorder-blocks -freorder-functions -frerun-cse-after-loop -fschedule-insns -fschedule-insns2 -fstrict-aliasing -ftree-pre
endif

WARN            := -Wall -Wno-unused -Wno-unknown-pragmas

ifneq ($(TARGET), HOST)
	WARN += -Werror
endif

ifeq ($(TARGET), PPC)
	WARN += -Wsign-conversion
endif

ifneq ($(TARGET), WIN32)
	WARN += -Werror=long-long
endif

CWARN           := $(WARN)
CXXWARN         := $(WARN) -fpermissive -Wno-reorder -Wno-unknown-pragmas
CFLAGS          += $(CWARN) $(OPTIMIZATION) -ansi -pedantic $(OS_DEPENDANT_CFLAGS)
CXXFLAGS        += $(CXXWARN) $(OPTIMIZATION) -ansi $(OS_DEPENDANT_CXXFLAGS)

ifdef DEBUG
	CFLAGS += -g
	CXXFLAGS += -g
endif

ifeq ($(TARGET), ARM)
	# Don't pass linker options to the compiler:
	CFLAGS := $(filter-out -Wl%,$(CFLAGS))
	CXXFLAGS := $(filter-out -Wl%,$(CXXFLAGS))
endif
