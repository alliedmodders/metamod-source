# (C)2004-2010 SourceMod Development Team
# Makefile written by David "BAILOPAN" Anderson

HL2SDK_OB = ../../hl2sdk-ob
HL2SDK_CSS = ../../hl2sdk-css
HL2SDK_OB_VALVE = ../../hl2sdk-ob-valve
HL2SDK_L4D = ../../hl2sdk-l4d
HL2SDK_L4D2 = ../../hl2sdk-l4d2
HL2SDK_CSGO = ../../hl2sdk-csgo

#####################################
### EDIT BELOW FOR OTHER PROJECTS ###
#####################################

OBJECTS = metamod.cpp \
		metamod_util.cpp \
		metamod_console.cpp \
		metamod_oslink.cpp \
		metamod_plugins.cpp \
		sourcehook/sourcehook.cpp \
		sourcehook/sourcehook_hookmangen.cpp \
		sourcehook/sourcehook_impl_chookidman.cpp \
		sourcehook/sourcehook_impl_chookmaninfo.cpp \
		sourcehook/sourcehook_impl_cproto.cpp \
		sourcehook/sourcehook_impl_cvfnptr.cpp \
		provider/console.cpp \
		provider/provider_ep2.cpp \
		vsp_bridge.cpp \
		gamedll_bridge.cpp

##############################################
### CONFIGURE ANY OTHER FLAGS/OPTIONS HERE ###
##############################################

C_OPT_FLAGS = -DNDEBUG -O3 -funroll-loops -pipe -fno-strict-aliasing
C_DEBUG_FLAGS = -D_DEBUG -DDEBUG -g -ggdb3
C_GCC4_FLAGS = -fvisibility=hidden
CPP_GCC4_FLAGS = -fvisibility-inlines-hidden
CPP = gcc

override ENGSET = false
ifeq "$(ENGINE)" "orangebox"
	HL2SDK = $(HL2SDK_OB)
	HL2PUB = $(HL2SDK)/public
	HL2LIB = $(HL2SDK)/lib/linux
	CFLAGS += -DSOURCE_ENGINE=3
	INCLUDE += -I$(HL2SDK)/public/game/server
	BINARY = metamod.2.ep2.so
	LIB_SUFFIX = _i486.so
	override ENGSET = true
endif
ifeq "$(ENGINE)" "css"
	HL2SDK = $(HL2SDK_CSS)
	HL2PUB = $(HL2SDK)/public
	HL2LIB = $(HL2SDK)/lib/linux
	CFLAGS += -DSOURCE_ENGINE=6
	INCLUDE += -I$(HL2SDK)/public/game/server
	BINARY = metamod.2.css.so
	LIB_PREFIX = lib
	LIB_SUFFIX = _srv.so
	override ENGSET = true
endif
ifeq "$(ENGINE)" "orangeboxvalve"
	HL2SDK = $(HL2SDK_OB_VALVE)
	HL2PUB = $(HL2SDK)/public
	HL2LIB = $(HL2SDK)/lib/linux
	CFLAGS += -DSOURCE_ENGINE=7
	INCLUDE += -I$(HL2SDK)/public/game/server
	BINARY = metamod.2.ep2v.so
	LIB_PREFIX = lib
	LIB_SUFFIX = _srv.so
	override ENGSET = true
endif
ifeq "$(ENGINE)" "left4dead"
	HL2SDK = $(HL2SDK_L4D)
	HL2PUB = $(HL2SDK)/public
	HL2LIB = $(HL2SDK)/lib/linux
	CFLAGS += -DSOURCE_ENGINE=8
	INCLUDE += -I$(HL2SDK)/public/game/server
	BINARY = metamod.2.l4d.so
	LIB_PREFIX = lib
	LIB_SUFFIX = .so
	override ENGSET = true
endif
ifeq "$(ENGINE)" "left4dead2"
	HL2SDK = $(HL2SDK_L4D2)
	HL2PUB = $(HL2SDK)/public
	HL2LIB = $(HL2SDK)/lib/linux
	CFLAGS += -DSOURCE_ENGINE=9
	INCLUDE += -I$(HL2SDK)/public/game/server
	BINARY = metamod.2.l4d2.so
	LIB_PREFIX = lib
	LIB_SUFFIX = _srv.so
	override ENGSET = true
endif
ifeq "$(ENGINE)" "csgo"
	HL2SDK = $(HL2SDK_CSGO)
	HL2PUB = $(HL2SDK)/public
	HL2LIB = $(HL2SDK)/lib/linux
	CFLAGS += -DSOURCE_ENGINE=12
	INCLUDE += -I$(HL2SDK)/public/game/server
	BINARY = metamod.2.csgo.so
	LIB_PREFIX = lib
	LIB_SUFFIX = .so
	override ENGSET = true
endif

CFLAGS += -DSE_EPISODEONE=1 -DSE_DARKMESSIAH=2 -DSE_ORANGEBOX=3 -DSE_BLOODYGOODTIME=4 -DSE_EYE=5 \
	-DSE_CSS=6 -DSE_ORANGEBOXVALVE=7 -DSE_LEFT4DEAD=8 -DSE_LEFT4DEAD2=9 -DSE_ALIENSWARM=10 \
	-DSE_PORTAL2=11 -DSE_CSGO=12

ifeq "$(ENGINE)" "csgo"
	LINK +=  $(HL2LIB)/interfaces_i486.a
endif

LINK += $(HL2LIB)/tier1_i486.a $(LIB_PREFIX)vstdlib$(LIB_SUFFIX) $(LIB_PREFIX)tier0$(LIB_SUFFIX) \
	-static-libgcc

INCLUDE += -I. -I.. -I../public -I$(HL2PUB) -I$(HL2PUB)/engine -I$(HL2PUB)/mathlib -I$(HL2PUB)/vstdlib \
	-I$(HL2PUB)/tier0 -I$(HL2PUB)/tier1 -I. -Isourcehook -I../loader

CFLAGS += -D_LINUX -DPOSIX -Dstricmp=strcasecmp -D_stricmp=strcasecmp -D_strnicmp=strncasecmp \
	-Dstrnicmp=strncasecmp -D_snprintf=snprintf -D_vsnprintf=vsnprintf -D_alloca=alloca \
	-Dstrcmpi=strcasecmp -Wall -Werror -Wno-uninitialized -mfpmath=sse -msse -DHAVE_STDINT_H -m32 \
	-DCOMPILER_GCC
CPPFLAGS += -Wno-non-virtual-dtor -fno-exceptions -fno-rtti

################################################
### DO NOT EDIT BELOW HERE FOR MOST PROJECTS ###
################################################

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug.$(ENGINE)
	CFLAGS += $(C_DEBUG_FLAGS)
else
	BIN_DIR = Release.$(ENGINE)
	CFLAGS += $(C_OPT_FLAGS)
endif

GCC_VERSION := $(shell $(CPP) -dumpversion >&1 | cut -b1)
ifeq "$(GCC_VERSION)" "4"
	CFLAGS += $(C_GCC4_FLAGS)
	CPPFLAGS += $(CPP_GCC4_FLAGS)
endif

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)
OBJ_LINUX := $(OBJ_LINUX:%.c=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

$(BIN_DIR)/%.o: %.c
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all: check
	mkdir -p $(BIN_DIR)
	mkdir -p $(BIN_DIR)/provider
	mkdir -p $(BIN_DIR)/sourcehook
	ln -sf $(HL2LIB)/$(LIB_PREFIX)vstdlib$(LIB_SUFFIX)
	ln -sf $(HL2LIB)/$(LIB_PREFIX)tier0$(LIB_SUFFIX)
	$(MAKE) -f Makefile metamod

check:
	if [ "$(ENGSET)" = "false" ]; then \
		echo "You must supply one of the following values for ENGINE:"; \
		echo "csgo, left4dead2, left4dead, orangeboxvalve, css or orangebox"; \
		exit 1; \
	fi

metamod: check $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(OBJ_LINUX) $(LINK) -m32 -shared -ldl -lm -o$(BIN_DIR)/$(BINARY)

debug:
	$(MAKE) -f Makefile all DEBUG=true

default: all

clean: check
	rm -rf $(BIN_DIR)/*.o
	rm -rf $(BIN_DIR)/sourcehook/*.o
	rm -rf $(BIN_DIR)/provider/*.o
	rm -rf $(BIN_DIR)/$(BINARY)

