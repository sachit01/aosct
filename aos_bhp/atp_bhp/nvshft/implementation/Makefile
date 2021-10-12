include ../../../atc/impl/rules.mk

SOURCES  = src/crc.c src/crc64.c src/main.c src/nvshft.c src/nvshft_params.c src/pre_crc.c
INCLUDES = src/crc.h src/nvshft.h src/pre_crc.h
EXEDIR   = ../release
EXEFILE  = $(EXEDIR)/nvshft$(EXE_SUFFIX)

.DEFAULT_GOAL := $(EXEFILE)

$(EXEFILE): Makefile ${SOURCES} ${INCLUDES}
	@mkdir -p `dirname $@`
	$(CC) -o $@ ${SOURCES}

nvshft: $(EXEFILE)

cfg-files:
	@cd ../release; ./nvshft -n

clean:
	rm $(EXEFILE)
