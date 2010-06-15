#(C)2004-2008 SourceMM Development Team
# Makefile written by David "BAILOPAN" Anderson

HL2SDK = ../../hl2sdk
SRCDS = ~/srcds

### EDIT BELOW FOR OTHER PROJECTS ###

OPT_FLAGS = -O2 -funroll-loops -pipe -fno-strict-aliasing
GCC4_FLAGS = -fvisibility=hidden -fvisibility-inlines-hidden
DEBUG_FLAGS = -g -ggdb3
CPP = gcc-4.1
BINARY = metamod.1.ep1.so

HL2PUB = $(HL2SDK)/public
HL2LIB = $(HL2SDK)/linux_sdk

OBJECTS = oslink.cpp 			\
		  util.cpp 				\
		  concommands.cpp 		\
		  CSmmAPI.cpp 			\
          sourcemm.cpp 			\
		  CPlugin.cpp 			\
		  vsp_bridge.cpp		\
		  gamedll_bridge.cpp	\
		  sourcehook/sourcehook.cpp

LINK = $(HL2LIB)/tier1_i486.a  vstdlib_i486.so tier0_i486.so -static-libgcc

INCLUDE = -I. -I$(HL2PUB) -I$(HL2PUB)/dlls -I$(HL2PUB)/engine -I$(HL2PUB)/tier0 -I$(HL2PUB)/tier1 \
          -I$(HL2PUB)/vstdlib -I$(HL2SDK)/tier1 -I. -Isourcehook -I../loader

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS += $(DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS += $(OPT_FLAGS)
endif

GCC_VERSION := $(shell $(CPP) -dumpversion >&1 | cut -b1)

CFLAGS += -D_LINUX -DNDEBUG -Dstricmp=strcasecmp -D_stricmp=strcasecmp -D_strnicmp=strncasecmp \
		-Dstrnicmp=strncasecmp -D_snprintf=snprintf -D_vsnprintf=vsnprintf -D_alloca=alloca \
		-Dstrcmpi=strcasecmp -Wall -Wno-non-virtual-dtor -Wno-uninitialized -Werror -fPIC \
		-fno-exceptions -fno-rtti -msse -m32

ifeq "$(GCC_VERSION)" "4"
	CFLAGS += $(GCC4_FLAGS)
endif

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)/sourcehook
	mkdir -p $(BIN_DIR)
	ln -sf $(SRCDS)/bin/vstdlib_i486.so vstdlib_i486.so
	ln -sf $(SRCDS)/bin/tier0_i486.so tier0_i486.so
	$(MAKE) sourcemm
	rm -rf $(BINARY)

sourcemm: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) -m32 $(CFLAGS) $(OBJ_LINUX) $(LINK) -shared -ldl -lm -o$(BIN_DIR)/$(BINARY)

debug:	
	$(MAKE) all DEBUG=true

default: all

clean:
	rm -rf Release/sourcehook/*.o
	rm -rf Release/*.o
	rm -rf Release/$(BINARY)
	rm -rf Debug/sourcehook/*.o
	rm -rf Debug/*.o
	rm -rf Debug/$(BINARY)
