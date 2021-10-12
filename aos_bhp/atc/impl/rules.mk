# ----------------------------build definitions---------------------------

ifeq ($(TARGET), WIN32)
	EXE_SUFFIX = .exe

	ifeq ($(OS),Windows_NT)
		OS_detected := Windows
	else
		OS_detected := $(shell uname -s)
	endif

	ifeq ($(OS_detected),Linux)
		CROSS_PREFIX   = i686-w64-mingw32-
	endif

	CC             = ${CROSS_PREFIX}gcc
	CXX            = ${CROSS_PREFIX}g++
	GCOV           = ${CROSS_PREFIX}gcov
	STDC_LIB_PATH  = ${CROSS_PREFIX}/distribution/lib
	LD             = ${CROSS_PREFIX}ld
	AR             = ${CROSS_PREFIX}ar
	RANLIB         = ${CROSS_PREFIX}ranlib
	STRIP          = ${CROSS_PREFIX}strip
endif

# Note, these MY_ must be first at each line!
ifndef VERBOSE
    MY_CC    = @$(CC)
    MY_CXX   = @$(CXX)
    MY_RM    = @$(RM)
    MY_MKDIR = @$(MKDIR)
    MY_FLINT_BINARY = @$(FLINT_BINARY)
else
    INSTALL := $(INSTALL) --verbose
    MY_CC    = $(CC)
    MY_CXX   = $(CXX)
    MY_RM    = $(RM)
    MY_MKDIR = $(MKDIR)
    MY_FLINT_BINARY = $(FLINT_BINARY)
endif

# -----------------------------build commands-----------------------------

define cxx-command
	@echo -e "   Compiling (CXX)  $(notdir $@)"
	@$(MKDIR) $(dir $@)
	$(MY_CXX) $(CC_FLAGS) $(CXXFLAGS) $(INCLUDE) -c $< -o $@
endef

define cc-command
	@echo -e "   Compiling (CC)  $(notdir $@)"
	@$(MKDIR) $(dir $@)
	$(MY_CC) $(CC_FLAGS) $(CFLAGS) $(INCLUDE) -c $< -o $@
endef

# ----------------------------lint definitions----------------------------

FLINT_BINARY = $(FLINT_PATH)/src/flexelint

LINT_DEFINES = $(MY_ADDED_DEFINES)

ifeq ($(TARGET), ARM)
	LINT_DEFINES += -DARM
else ifeq ($(TARGET), PPC)
	LINT_DEFINES += -DPPC
endif

# -----------------------------lint commands------------------------------

# -zero sets the exit code to 0. This is useful to prohibit the premature termination of make files
define lint-cxx-command
	@echo -e "   Linting (CXX) $(notdir $@)"
	$(MY_MKDIR) $(@D)
	$(MY_FLINT_BINARY) -zero $(LINT_DEFINES) $(INCLUDE) $(LINTINCLUDE_CXX) -i./TestAndTools/Lint -i./TestAndTools/Lint/Target -i./TestAndTools/Lint/gsp2 project_rules.lnt std.lnt options_generic.lnt POSIX_functions_deprecated.lnt options_target.lnt '+libclass(foreign)' -u $< > $@
endef

define lint-cxx-misra-command
	@echo -e "   Linting (CXX) $(notdir $@)"
	$(MY_MKDIR) $(@D)
	$(MY_FLINT_BINARY) -zero -i./TestAndTools/Lint -i./TestAndTools/Lint/Target -i./TestAndTools/Lint/gsp2 au-misra-cpp_20131002.lnt POSIX_functions_deprecated.lnt size-options-target.lnt co-gcc.lnt gsp2_options_and_rules.lnt '+libclass(foreign)' -u $(LINT_DEFINES) $(INCLUDE) $(LINTINCLUDE_CXX) $< > $@
endef

define lint-cc-command
	@echo -e "   Linting (CC) $(notdir $@)"
	$(MY_MKDIR) $(@D)
	$(MY_FLINT_BINARY) -zero $(LINT_DEFINES) $(INCLUDE) $(LINTINCLUDE_CC) -i./TestAndTools/Lint -i./TestAndTools/Lint/Target -i./TestAndTools/Lint/gsp2 project_rules.lnt std.lnt options_generic.lnt POSIX_functions_deprecated.lnt options_target.lnt '+libclass(foreign)' -u $< > $@
endef


ifeq ($(TARGET), PPC)
	LINTINCLUDE_CC      += -i$(CROSS_Linux_ppc)/distribution/usr/lib/gcc/powerpc-unknown-linux-uclibc/4.6.1/include
	LINTINCLUDE_CC      += -i$(CROSS_Linux_ppc)/distribution/usr/lib/gcc/powerpc-unknown-linux-uclibc/4.6.1/include-fixed
	LINTINCLUDE_CC      += -i$(CROSS_Linux_ppc)/distribution/usr/lib/gcc/powerpc-unknown-linux-uclibc/4.6.1/../../../../powerpc-unknown-linux-uclibc/include
	LINTINCLUDE_CC      += -i$(CROSS_Linux_ppc)/distribution/usr/powerpc-unknown-linux-uclibc/sysroot/usr/include
	LINTINCLUDE_CXX     += -i$(CROSS_Linux_ppc)/distribution/usr/lib/gcc/powerpc-unknown-linux-uclibc/4.6.1/../../../../powerpc-unknown-linux-uclibc/include/c++/4.6.1
	LINTINCLUDE_CXX     += -i$(CROSS_Linux_ppc)/distribution/usr/lib/gcc/powerpc-unknown-linux-uclibc/4.6.1/../../../../powerpc-unknown-linux-uclibc/include/c++/4.6.1/powerpc-unknown-linux-uclibc
	LINTINCLUDE_CXX     += -i$(CROSS_Linux_ppc)/distribution/usr/lib/gcc/powerpc-unknown-linux-uclibc/4.6.1/../../../../powerpc-unknown-linux-uclibc/include/c++/4.6.1/backward
	LINTINCLUDE_CXX     += -i$(CROSS_Linux_ppc)/distribution/usr/lib/gcc/powerpc-unknown-linux-uclibc/4.6.1/include
	LINTINCLUDE_CXX     += -i$(CROSS_Linux_ppc)/distribution/usr/lib/gcc/powerpc-unknown-linux-uclibc/4.6.1/include-fixed
	LINTINCLUDE_CXX     += -i$(CROSS_Linux_ppc)/distribution/usr/lib/gcc/powerpc-unknown-linux-uclibc/4.6.1/../../../../powerpc-unknown-linux-uclibc/include
	LINTINCLUDE_CXX     += -i$(CROSS_Linux_ppc)/distribution/usr/powerpc-unknown-linux-uclibc/sysroot/usr/include
else ifeq ($(TARGET), ARM)
	LINTINCLUDE_CC      += -i$(CROSS_FreeBSD_arm)/distribution/bin/../lib/gcc/arm-unknown-freebsd8/4.2.1/include
	LINTINCLUDE_CC      += -i$(CROSS_FreeBSD_arm)/distribution/bin/../lib/gcc/arm-unknown-freebsd8/4.2.1/../../../../arm-unknown-freebsd8/include
	LINTINCLUDE_CC      += -i$(CROSS_FreeBSD_arm)/distribution/bin/../sysroot/usr/include
	LINTINCLUDE_CXX     += -i$(CROSS_FreeBSD_arm)/distribution/bin/../lib/gcc/arm-unknown-freebsd8/4.2.1/../../../../arm-unknown-freebsd8/include/c++/4.2.1
	LINTINCLUDE_CXX     += -i$(CROSS_FreeBSD_arm)/distribution/bin/../lib/gcc/arm-unknown-freebsd8/4.2.1/../../../../arm-unknown-freebsd8/include/c++/4.2.1/arm-unknown-freebsd8
	LINTINCLUDE_CXX     += -i$(CROSS_FreeBSD_arm)/distribution/bin/../lib/gcc/arm-unknown-freebsd8/4.2.1/../../../../arm-unknown-freebsd8/include/c++/4.2.1/backward
	LINTINCLUDE_CXX     += -i$(CROSS_FreeBSD_arm)/distribution/bin/../lib/gcc/arm-unknown-freebsd8/4.2.1/include
	LINTINCLUDE_CXX     += -i$(CROSS_FreeBSD_arm)/distribution/bin/../lib/gcc/arm-unknown-freebsd8/4.2.1/../../../../arm-unknown-freebsd8/include
	LINTINCLUDE_CXX     += -i$(CROSS_FreeBSD_arm)/distribution/bin/../sysroot/usr/include
endif


LINT_CONFIG=-i$(LINT_ROOT) au-misra-cpp_20131002.lnt deprecatePOSIX.lnt co-gcc.lnt env-vc10.lnt AOS.lnt $(LINTINCLUDE_CXX)

lint:
	$(MY_FLINT_BINARY) -zero $(LINT_DEFINES) $(LINT_CONFIG) $(INCLUDE) $(CC_FILES) $(CXX_FILES)

# --------------------------------doxygen---------------------------------

ATC_DOC_IN_PATH   := $(ATC_ROOT)/spec/input
ATC_DOC_OUT_PATH  := $(ATC_ROOT)/spec/output/logs
ATC_DOC_MODULES   := $(patsubst $(ATC_ROOT)/impl/%, $(ATC_DOC_IN_PATH)/%, $(ATC_MODULES))
ATC_DOC_MODULES   += $(ATC_DOC_IN_PATH)/atc_all/
ATC_DOXY_FILES    := $(foreach moduleDir, $(ATC_DOC_MODULES), $(wildcard $(moduleDir)*_doxygen_setup))
ATC_HTML_FILES    := $(foreach doxyFile,$(ATC_DOXY_FILES),$(patsubst $(ATC_DOC_IN_PATH)/%_doxygen_setup, $(ATC_DOC_OUT_PATH)/%/DUMMY, $(doxyFile)))

CORE_DOC_IN_PATH  := $(CORE_ROOT)/spec/input
CORE_DOC_OUT_PATH := $(CORE_ROOT)/spec/output/logs
CORE_DOC_MODULES  := $(patsubst $(CORE_ROOT)/impl/%, $(CORE_DOC_IN_PATH)/%, $(CORE_MODULES))
CORE_DOC_MODULES  += $(CORE_DOC_IN_PATH)/atp_core_all/
CORE_DOXY_FILES   := $(foreach moduleDir, $(CORE_DOC_MODULES), $(wildcard $(moduleDir)*_doxygen_setup))
CORE_HTML_FILES   := $(foreach doxyFile,$(CORE_DOXY_FILES),$(patsubst $(CORE_DOC_IN_PATH)/%_doxygen_setup, $(CORE_DOC_OUT_PATH)/%/DUMMY, $(doxyFile)))

BHP_DOC_IN_PATH   := $(BHP_ROOT)/spec/input
BHP_DOC_OUT_PATH  := $(BHP_ROOT)/spec/output/logs
BHP_DOC_MODULES   := $(patsubst $(BHP_ROOT)/impl/%, $(BHP_DOC_IN_PATH)/%, $(BHP_MODULES))
BHP_DOC_MODULES   += $(BHP_DOC_IN_PATH)/atp_bhp_all/
BHP_DOXY_FILES    := $(foreach moduleDir, $(BHP_DOC_MODULES), $(wildcard $(moduleDir)*_doxygen_setup))
BHP_HTML_FILES    := $(foreach doxyFile,$(BHP_DOXY_FILES),$(patsubst $(BHP_DOC_IN_PATH)/%_doxygen_setup, $(BHP_DOC_OUT_PATH)/%/DUMMY, $(doxyFile)))

DISP_DOC_IN_PATH  := $(DISP_ROOT)/spec/input
DISP_DOC_OUT_PATH := $(DISP_ROOT)/spec/output/logs
DISP_DOC_MODULES  := $(patsubst $(DISP_ROOT)/impl/%, $(DISP_DOC_IN_PATH)/%, $(DISP_MODULES))
DISP_DOC_MODULES  += $(DISP_DOC_IN_PATH)/dispatcher_all/
DISP_DOXY_FILES   := $(foreach moduleDir, $(DISP_DOC_MODULES), $(wildcard $(moduleDir)*_doxygen_setup))
DISP_HTML_FILES   := $(foreach doxyFile,$(DISP_DOXY_FILES),$(patsubst $(DISP_DOC_IN_PATH)/%_doxygen_setup, $(DISP_DOC_OUT_PATH)/%/DUMMY, $(doxyFile)))

# TODO:
# * TARGET and ENV aren't needed here
# * the tag files should be saved to the output dir, NOT the input dir

define doxygen-command
	@echo -e "   Documenting $(notdir $(abspath $(dir $@)))"
	$(MY_MKDIR) $(@D)
	@grep @INCLUDE $< | sed 's|@INCLUDE = ||' | sed 's|P:\\|$(PROJS_ROOT)/|g' | sed 's|\\\([a-zA-Z]\)|/\1|g' > $(@D)/doxyFile1
	@cp `cat $(@D)/doxyFile1`                        $(@D)/doxyFile2
	@grep -v @INCLUDE $<                          >> $(@D)/doxyFile2
	@sed -i 's|CLANG_.*||g'                          $(@D)/doxyFile2
	@sed -i 's|HAVE_DOT.*||g'                        $(@D)/doxyFile2
	@sed -i 's|\(GENERATE_LATEX[ ]\+=\) YES|\1 NO|g' $(@D)/doxyFile2
	@sed -i 's|C:\\Tools\\PlantUML\\plantuml.jar||g' $(@D)/doxyFile2
	@sed -i 's|P:\\|$(PROJS_ROOT)/|g'                $(@D)/doxyFile2
	@sed -i 's|\\\([a-zA-Z]\)|/\1|g'                 $(@D)/doxyFile2
	$(MY_MKDIR) `grep ^OUTPUT_DIRECTORY $(@D)/doxyFile2 | sed 's|[A-Z_]\+[ ]*=[ ]*||'`
	@cd $(dir $<) && doxygen $(@D)/doxyFile2 > $(@D)/doxyLog.txt 2> $(@D)/doxyWarnings.txt
	@cat $(@D)/doxyWarnings.txt
endef

$(ATC_DOC_OUT_PATH)/%/DUMMY: $(ATC_DOC_IN_PATH)/%_doxygen_setup
	$(MY_MKDIR) $(ATC_DOC_OUT_PATH)/atc_all
	$(doxygen-command)

$(CORE_DOC_OUT_PATH)/%/DUMMY: $(CORE_DOC_IN_PATH)/%_doxygen_setup
	$(MY_MKDIR) $(CORE_DOC_OUT_PATH)/atp_core_all
	$(doxygen-command)

$(BHP_DOC_OUT_PATH)/%/DUMMY: $(BHP_DOC_IN_PATH)/%_doxygen_setup
	$(MY_MKDIR) $(BHP_DOC_OUT_PATH)/atp_bhp_all
	$(doxygen-command)

$(DISP_DOC_OUT_PATH)/%/DUMMY: $(DISP_DOC_IN_PATH)/%_doxygen_setup
	$(MY_MKDIR) $(DISP_DOC_OUT_PATH)/dispatcher_all
	$(doxygen-command)
