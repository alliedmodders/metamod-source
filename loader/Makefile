# (C)2004-2009 SourceMod Development Team
# Makefile written by David "BAILOPAN" Anderson

#####################################
### EDIT BELOW FOR OTHER PROJECTS ###
#####################################

OBJECTS = loader.cpp			\
		  utility.cpp			\
		  serverplugin.cpp		\
		  gamedll.cpp

##############################################
### CONFIGURE ANY OTHER FLAGS/OPTIONS HERE ###
##############################################

C_OPT_FLAGS = -DNDEBUG -O3 -funroll-loops -pipe -fno-strict-aliasing
C_DEBUG_FLAGS = -D_DEBUG -DDEBUG -g -ggdb3
C_GCC4_FLAGS = -fvisibility=hidden
CPP_GCC4_FLAGS = -fvisibility-inlines-hidden
CPP = gcc-4.1

ifneq (,$(filter left4dead2 csgo,$(ENGINE)))
	BINARY = server.so
	CFLAGS += -DLIB_PREFIX=\"lib\" -DLIB_SUFFIX=\".so\"
else
	BINARY = server_i486.so
	CFLAGS += -DLIB_PREFIX=\"\" -DLIB_SUFFIX=\"_i486.so\"
endif

LINK += -static-libgcc
	
INCLUDE += -I. -I../core/sourcehook

CFLAGS += -D_LINUX -Dstricmp=strcasecmp -D_stricmp=strcasecmp -D_strnicmp=strncasecmp \
	-Dstrnicmp=strncasecmp -D_snprintf=snprintf -D_vsnprintf=vsnprintf -D_alloca=alloca \
	-Dstrcmpi=strcasecmp -Wall -Werror -Wno-uninitialized -mfpmath=sse -msse -DHAVE_STDINT_H -m32
CPPFLAGS += -Wno-non-virtual-dtor -fno-exceptions -fno-rtti

################################################
### DO NOT EDIT BELOW HERE FOR MOST PROJECTS ###
################################################

ifeq "$(DEBUG)" "true"
	ifeq "$(ENGINE)" ""
		BIN_DIR = Debug
	else
		BIN_DIR = Debug.$(ENGINE)
	endif
	CFLAGS += $(C_DEBUG_FLAGS)
else
	ifeq "$(ENGINE)" ""
		BIN_DIR = Release
	else
		BIN_DIR = Release.$(ENGINE)
	endif
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

all:
	mkdir -p $(BIN_DIR)
	$(MAKE) -f Makefile metamod

metamod: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(OBJ_LINUX) $(LINK) -m32 -shared -ldl -lm -o$(BIN_DIR)/$(BINARY)

debug:
	$(MAKE) -f Makefile all DEBUG=true

default: all

clean:
	rm -rf $(BIN_DIR)/*.o
	rm -rf $(BIN_DIR)/$(BINARY)

