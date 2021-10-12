##################################################################
#
# (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
#
# We reserve all rights in this file and in the information
# contained therein. Reproduction, use or disclosure to third
# parties without express authority is strictly forbidden.
#
# DESCRIPTION:
# Makefile for AOS NVSHFR
#
##################################################################

INSTALL:=install
MKDIR:=mkdir -p

# TODO should not be necessary:
include pathConfig.mk

include $(BHP_ROOT)/impl/buildkit.target.mk
include $(ATC_ROOT)/impl/compiler_flags.mk
include $(ATC_ROOT)/impl/rules.mk

## Directory for object files
OBJDIR := ./obj/$(OS_NAME)

# The file we want (and where we want it)
PROJ_NAME = nvshfr

ifneq ($(TARGET), HOST)
	PROJ_NAME := release/$(OS_NAME)/$(PROJ_NAME)
endif

# ---------------------------source files---------------------------------
CXX_FILES := $(ATC_ROOT)/nvshfr/src/abstract_data.cpp \
             $(ATC_ROOT)/nvshfr/src/app_error.cpp \
             $(ATC_ROOT)/nvshfr/src/bcd_parameter.cpp \
             $(ATC_ROOT)/nvshfr/src/bin_parser.cpp \
             $(ATC_ROOT)/nvshfr/src/definition_parser.cpp \
             $(ATC_ROOT)/nvshfr/src/generic_parameter.cpp \
             $(ATC_ROOT)/nvshfr/src/hex_parameter.cpp \
             $(ATC_ROOT)/nvshfr/src/iterative_parameter_list.cpp \
             $(ATC_ROOT)/nvshfr/src/nvshfr_main.cpp \
             $(ATC_ROOT)/nvshfr/src/parameter.cpp \
             $(ATC_ROOT)/nvshfr/src/parameter_list.cpp \
             $(ATC_ROOT)/nvshfr/src/stream_helper.cpp

CC_FILES :=

# -------------------------end of source files----------------------------

# Files needed to make the executable file, this will automatically add one .o file for every .c or .cpp file.
# NOTE! If there are source files with same name but in different folders then this makefile will fail, since it will put all obj files in one and same folder.
OBJ_FILES := $(patsubst $(PROJS_ROOT)/%.cpp,$(OBJDIR)/%.o,$(CXX_FILES))
OBJ_FILES += $(patsubst $(PROJS_ROOT)/%.c,$(OBJDIR)/%.o, $(CC_FILES))

# ----------------------------include paths-------------------------------
# Include paths where to look for header files, all folder with header files need to be listed here or else there will be a "No such file or directory" error when building.
ifeq ($(TARGET), HOST)
	INCLUDE += -I$(VFW_PATH)/distribution/Linux_ppc/include
else
	INCLUDE += -I$(VFW_PATH)/distribution/$(OS_NAME)/include
endif

# ---------------------------------libs-----------------------------------
EXTRA_LIBS =

# ---------------------------------flags----------------------------------
CC_FLAGS += $(MY_ADDED_DEFINES)

CXXFLAGS := $(filter-out -Werror=long-long,$(CXXFLAGS))

# -----------------------------build recipes------------------------------
clean:
	$(MY_RM) -f $(OBJ_FILES) $(OBJ_FILES:.o=.d) $(PROJ_NAME)

# If user type just "make" on command line this is what is built
.DEFAULT_GOAL := all
.PHONY: all
all:$(PROJ_NAME)

$(OBJDIR)/%.o: $(PROJS_ROOT)/%.cpp
	$(cxx-command)

$(OBJDIR)/%.o: $(PROJS_ROOT)/%.c
	$(cc-command)

# This is how to link the objects files into the executable file.
$(PROJ_NAME): $(OBJ_FILES)
	@echo ""
	$(MY_MKDIR) $(@D)
	$(MY_CXX) $(OBJ_FILES) $(LD_FLAGS) $(EXTRA_LIBS) -o $(PROJ_NAME)
	@echo ""
	@echo -e " Successfully built $(PROJ_NAME)"

# ------------------------------dependencies-----------------------------
# Automatic dependency graph generation
# -MMD means that for each .o file a .d file shall also be generated.
# -include means we include the dependency file for each object file to make into this Makefile.
CC_FLAGS += -MMD
-include $(OBJ_FILES:.o=.d)

# -------------------------------end of file------------------------------
