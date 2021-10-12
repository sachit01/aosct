##################################################################
#
# (C) COPYRIGHT Bombardier Transportation Sweden AB, 2020
#
# We reserve all rights in this file and in the information
# contained therein. Reproduction, use or disclosure to third
# parties without express authority is strictly forbidden.
#
# DESCRIPTION:
# Makefile for AOS NVSHFT
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

# The files we want (and where we want them)
PROJ_NAME1 := nvshft_generic
PROJ_NAME2 := calc_crc

ifneq ($(TARGET), HOST)
	PROJ_NAME1 := release/$(OS_NAME)/$(PROJ_NAME1)
	PROJ_NAME2 := release/$(OS_NAME)/$(PROJ_NAME2)
endif

# ---------------------------source files---------------------------------
CC_FILES1 := $(ATC_ROOT)/nvshft/src/crc32.c $(ATC_ROOT)/nvshft/src/file_io.c $(ATC_ROOT)/nvshft/src/nvshft.c
CC_FILES2 := $(ATC_ROOT)/nvshft/src/crc32.c $(ATC_ROOT)/nvshft/src/file_io.c $(ATC_ROOT)/nvshft/src/calc_crc.c

ifeq ($(TARGET), HOST)
	CC_FILES1 += $(ATC_ROOT)/nvshft/src/vfw_sim.c
endif

# -------------------------end of source files----------------------------

# Files needed to make the executable file, this will automatically add one .o file for every .c or .cpp file.
# NOTE! If there are source files with same name but in different folders then this makefile will fail, since it will put all obj files in one and same folder.
OBJ_FILES1 := $(patsubst $(PROJS_ROOT)/%.c,$(OBJDIR)/%.o, $(CC_FILES1))
OBJ_FILES2 := $(patsubst $(PROJS_ROOT)/%.c,$(OBJDIR)/%.o, $(CC_FILES2))

# ----------------------------include paths-------------------------------
# Include paths where to look for header files, all folder with header files need to be listed here or else there will be a "No such file or directory" error when building.
ifeq ($(TARGET), HOST)
	INCLUDE += -I$(VFW_PATH)/distribution/Linux_ppc/include
else
	INCLUDE += -I$(VFW_PATH)/distribution/$(OS_NAME)/include
endif

# ---------------------------------libs-----------------------------------
ifneq ($(TARGET), HOST)
	EXTRA_LIBS = $(VFW_PATH)/distribution/$(OS_NAME)/lib/libVfw.so
endif

# ---------------------------------flags----------------------------------
CC_FLAGS += $(MY_ADDED_DEFINES)

# -----------------------------build recipes------------------------------
clean:
	$(MY_RM) -f $(OBJ_FILES1) $(OBJ_FILES2) $(OBJ_FILES1:.o=.d) $(OBJ_FILES2:.o=.d) $(PROJ_NAME1) $(PROJ_NAME2)

# If user type just "make" on command line this is what is built
.DEFAULT_GOAL := all
.PHONY: all
all: $(PROJ_NAME1)

$(OBJDIR)/%.o: $(PROJS_ROOT)/%.cpp
	$(cxx-command)

$(OBJDIR)/%.o: $(PROJS_ROOT)/%.c
	$(cc-command)

# This is how to link the objects files into the executable file.
$(PROJ_NAME1): $(OBJ_FILES1)
	$(MY_MKDIR) $(@D)
	$(MY_CC) $(OBJ_FILES1) $(LD_FLAGS) $(EXTRA_LIBS) -o $(PROJ_NAME1)
	@echo ""
	@echo -e "Successfully built $(PROJ_NAME1)"
	@echo ""

$(PROJ_NAME2): $(OBJ_FILES2)
	$(MY_MKDIR) $(@D)
	$(MY_CC) $(OBJ_FILES2) $(LD_FLAGS) $(EXTRA_LIBS) -o $(PROJ_NAME2)
	@echo ""
	@echo -e "Successfully built $(PROJ_NAME2)"
	@echo ""

# ------------------------------dependencies-----------------------------
# Automatic dependency graph generation
# -MMD means that for each .o file a .d file shall also be generated.
# -include means we include the dependency file for each object file to make into this Makefile.
CC_FLAGS += -MMD
-include $(OBJ_FILES1:.o=.d)
-include $(OBJ_FILES2:.o=.d)

# -------------------------------end of file------------------------------
