#(C)2004-2007 SourceMM Development Team
# Makefile written by David "BAILOPAN" Anderson and Pavol Marko

OPT_FLAGS = -O3 -funroll-loops -s -pipe
DEBUG_FLAGS = -g -ggdb3
CPP = gcc
LINK = -lstdc++
INCLUDE = -I. -I..
MAX_PARAMS=20

BINARY = sourcehook_test
OBJECTS = main.cpp sourcehook.cpp $(shell ls -t test*.cpp)
HEADERS = ../sh_list.h ../sh_tinyhash.h ../sh_memory.h ../sh_string.h ../sh_vector.h ../sourcehook_impl.h ../FastDelegate.h ../sourcehook.h ../sh_memfuncinfo.h

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS = $(DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS = $(OPT_FLAGS)
endif

CFLAGS += -Wall
# Also, enable SH_ASSERT
CFLAGS += -DSH_DEBUG

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)
	

default: all

$(BIN_DIR)/%.o: %.cpp $(HEADERS)
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

../sourcehook.h: ../generate/sourcehook.hxx
	(cd ../generate; ./shworker.bin iter sourcehook.hxx sourcehook.h $(MAX_PARAMS); cp sourcehook.h ..)

../sourcehook.hxx: ../generate/sh_memfuncinfo.hxx
	(cd ../generate; ./shworker.bin iter sh_memfuncinfo.hxx sh_memfuncinfo.h $(MAX_PARAMS); cp sh_memfuncino.h ..)

../FastDelegate.hxx: ../generate/FastDelegate.hxx
	(cd ../generate; ./shworker.bin iter FastDelegate.hxx FastDelegate.h $(MAX_PARAMS); cp FastDelegate.h ..)
	
debug:
	$(MAKE) all DEBUG=true
	
all:
	mkdir -p $(BIN_DIR)
	ln -sf ../sourcehook.cpp sourcehook.cpp
	$(MAKE) $(BINARY)
	rm -f $(BINARY)
	rm -f sourcehook.cpp
	ln -sf $(BIN_DIR)/$(BINARY) $(BINARY)


$(BINARY): $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(CFLAGS) $(OBJ_LINUX) $(LINK) -o$(BIN_DIR)/$(BINARY)

clean:
	rm -rf Release/*.o
	rm -rf Release/$(BINARY)
	rm -rf Debug/*.o
	rm -rf Debug/$(BINARY)
