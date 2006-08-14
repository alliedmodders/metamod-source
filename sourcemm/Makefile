#(C)2004-2005 SourceMM Development Team
# Makefile written by David "BAILOPAN" Anderson

HL2SDK = ../hl2sdk
SMM_ROOT = ..
SRCDS = ~/srcds
EXTRA_FLAGS = 

### EDIT BELOW FOR OTHER PROJECTS ###

OPT_FLAGS = -O2 -funroll-loops -s -pipe
DEBUG_FLAGS = -g -ggdb3
CPP = gcc-4.1
BINARY = server_i486.so

OBJECTS = oslink.cpp util.cpp convar.cpp concommands.cpp \
	CSmmAPI.cpp sourcemm.cpp CPlugin.cpp sourcehook.cpp

LINK = vstdlib_i486.so tier0_i486.so -static-libgcc

HL2PUB = $(HL2SDK)/public

INCLUDE = -I. -I$(HL2PUB) -I$(HL2PUB)/dlls -I$(HL2PUB)/engine -I$(HL2PUB)/tier0 -I$(HL2PUB)/tier1 \
	-I$(HL2PUB)/vstdlib -I$(HL2SDK)/tier1 -I$(SMM_ROOT) -I$(SMM_ROOT)/sourcehook

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS = $(DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS = $(OPT_FLAGS)
endif

CFLAGS += -D_LINUX -DNDEBUG -Dstricmp=strcasecmp -D_stricmp=strcasecmp -D_strnicmp=strncasecmp -Dstrnicmp=strncasecmp -D_snprintf=snprintf -D_vsnprintf=vsnprintf -D_alloca=alloca -Dstrcmpi=strcasecmp -Wall -Wno-non-virtual-dtor -Werror -fPIC -fno-exceptions -fno-rtti -msse $(EXTRA_FLAGS)

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)
	ln -sf $(SRCDS)/bin/vstdlib_i486.so vstdlib_i486.so
	ln -sf $(SRCDS)/bin/tier0_i486.so tier0_i486.so
	ln -sf $(HL2SDK)/tier1/convar.cpp convar.cpp
	ln -sf $(SMM_ROOT)/sourcehook/sourcehook.cpp sourcehook.cpp
	$(MAKE) sourcemm
	rm -rf $(BINARY)
	ln -sf $(BIN_DIR)/$(BINARY) $(BINARY)

sourcemm: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(CFLAGS) $(OBJ_LINUX) $(LINK) -shared -ldl -lm -o$(BIN_DIR)/$(BINARY)

debug:	
	$(MAKE) all DEBUG=true

default: all

clean:
	rm -rf Release/*.o
	rm -rf Release/$(BINARY)
	rm -rf Debug/*.o
	rm -rf Debug/$(BINARY)
	
