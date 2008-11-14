#(C)2004-2008 SourceMM Development Team
# Makefile written by David "BAILOPAN" Anderson

#Edit these paths for your own setup.
HL2SDK_ORIG = ../../hl2sdk
HL2SDK_OB = ../../hl2sdk-ob
SOURCEMM14 = ../core-1.4
SOURCEMM16 = ../core
SRCDS_BASE = ~/srcds

#Edit these below for other projects.
OPT_FLAGS = -O3 -funroll-loops -s -pipe
GCC4_FLAGS = -fvisibility=hidden -fvisibility-inlines-hidden
DEBUG_FLAGS = -g -ggdb3 -D_DEBUG
CPP = gcc-4.1
BINARY = stub_mm_i486.so

OBJECTS = stub_mm.cpp

LINK = -static-libgcc
INCLUDE = -I. -I..

#Usually no need to edit below

ifeq "$(ENGINE)" "original"
	HL2SDK = $(HL2SDK_ORIG)
	HL2PUB = $(HL2SDK_ORIG)/public
	HL2LIB = $(HL2SDK_ORIG)/linux_sdk
	CFLAGS += -DENGINE_ORIGINAL
	METAMOD = $(SOURCEMM14)
	INCLUDE += -I$(HL2SDK)/public/dlls
	SRCDS = $(SRCDS_BASE)
endif
ifeq "$(ENGINE)" "orangebox"
	HL2SDK = $(HL2SDK_OB)
	HL2PUB = $(HL2SDK_OB)/public
	HL2LIB = $(HL2SDK_OB)/lib/linux
	CFLAGS += -DENGINE_ORANGEBOX
	METAMOD = $(SOURCEMM16)
	INCLUDE += -I$(HL2SDK)/public/game/server
	SRCDS = $(SRCDS_BASE)/orangebox
endif
ifeq "$(ENGINE)" ""
	echo "You must supply ENGINE=orangebox or ENGINE=original"
	false
endif

LINK += $(HL2LIB)/tier1_i486.a vstdlib_i486.so tier0_i486.so

INCLUDE += -I$(HL2PUB) -I$(HL2PUB)/dlls -I$(HL2PUB)/engine -I$(HL2PUB)/tier0 \
	-I$(HL2PUB)/tier1 -I$(HL2PUB)/vstdlib -I$(HL2SDK)/tier1 -I$(METAMOD) \
	-I$(METAMOD)/sourcehook

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug.$(ENGINE)
	CFLAGS += $(DEBUG_FLAGS)
else
	BIN_DIR = Release.$(ENGINE)
	CFLAGS += $(OPT_FLAGS)
endif

GCC_VERSION := $(shell $(CPP) -dumpversion >&1 | cut -b1)

CFLAGS += -D_LINUX -Dstricmp=strcasecmp -D_stricmp=strcasecmp -D_strnicmp=strncasecmp \
	-Dstrnicmp=strncasecmp -D_snprintf=snprintf -D_vsnprintf=vsnprintf -D_alloca=alloca \
	-Dstrcmpi=strcasecmp -Wall -Wno-non-virtual-dtor -Werror -fPIC -fno-exceptions \
	-fno-rtti -msse -m32 -fno-strict-aliasing

ifeq "$(GCC_VERSION)" "4"
	CFLAGS += $(GCC4_FLAGS)
endif

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)
	rm -f vstdlib_i486.so
	rm -f tier0_i486.so
	ln -sf $(SRCDS)/bin/vstdlib_i486.so vstdlib_i486.so
	ln -sf $(SRCDS)/bin/tier0_i486.so tier0_i486.so
	$(MAKE) -f Makefile stub_mm
	rm -rf $(BINARY)
	ln -sf $(BIN_DIR)/$(BINARY) $(BINARY)

stub_mm: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) -m32 $(OBJ_LINUX) $(LINK) -shared -ldl -lm -o$(BIN_DIR)/$(BINARY)

default: all

clean:
	rm -rf $(BIN_DIR)/*.o
	rm -rf $(BIN_DIR)/$(BINARY)

