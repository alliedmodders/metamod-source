# (C)2004-2009 Metamod:Source Development Team
# Makefile written by David "BAILOPAN" Anderson and Pavol Marko

OPT_FLAGS = -O3 -funroll-loops -s -pipe
DEBUG_FLAGS = -g -ggdb3
CPP = gcc
LINK = -lstdc++
INCLUDE = -I. -I.. -I../..
MAX_PARAMS=20

BINARY = sourcehook_test
# The hook generator is split per architecture; pick what the compiler is
# actually targeting, so -m32 on an x86_64 host still resolves correctly.
HOOKMANGEN_ARCH := $(if $(shell echo | $(CPP) $(CFLAGS) -E -dM - 2>/dev/null | grep __x86_64__),sourcehook_hookmangen_x86_64.cpp,sourcehook_hookmangen_x86.cpp)

ifeq ($(HOOKMANGEN_ARCH),sourcehook_hookmangen_x86_64.cpp)
  $(error The x86_64 hook generator pulls in metamod.h and the HL2SDK, which this standalone build does not have. Build 32-bit, e.g. CPP="gcc -m32" or CPP=i686-linux-gnu-gcc, which is also the only configuration AMBuilder builds these tests in)
endif

OBJECTS = main.cpp sourcehook.cpp sourcehook_hookmangen.cpp $(HOOKMANGEN_ARCH) sourcehook_impl_chookmaninfo.cpp sourcehook_impl_chookidman.cpp sourcehook_impl_cproto.cpp sourcehook_impl_cvfnptr.cpp $(shell ls -t test*.cpp)
HEADERS = ../sh_list.h ../sh_tinyhash.h ../sh_memory.h ../sh_string.h ../sh_vector.h ../sourcehook_impl.h ../FastDelegate.h ../sourcehook.h ../sh_memfuncinfo.h ../sh_pagealloc.h

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS = $(DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS = $(OPT_FLAGS)
endif

CFLAGS += -Wall -Wno-non-virtual-dtor
# Also, enable SH_ASSERT
# SourceHook swaps vtable entries out from under the compiler; GCC's
# devirtualisation assumes it cannot happen (see issue #51). AMBuilder
# turns it off from gcc-4.9 on, and so must this.
CFLAGS += -fno-devirtualize
CFLAGS += -DSH_DEBUG -DSOURCEHOOK_TESTS

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
	ln -sf ../sourcehook.cpp
	ln -sf ../sourcehook_hookmangen.cpp
	ln -sf ../$(HOOKMANGEN_ARCH)
	ln -sf ../sourcehook_impl_chookidman.cpp
	ln -sf ../sourcehook_impl_chookmaninfo.cpp
	ln -sf ../sourcehook_impl_cproto.cpp
	ln -sf ../sourcehook_impl_cvfnptr.cpp
	$(MAKE) $(BINARY)
	rm -f $(BINARY)
	rm -f sourcehook.cpp
	rm -f sourcehook_hookmangen.cpp
	rm -f $(HOOKMANGEN_ARCH)
	rm -f sourcehook_impl_chookidman.cpp
	rm -f sourcehook_impl_chookmaninfo.cpp
	rm -f sourcehook_impl_cproto.cpp
	rm -f sourcehook_impl_cvfnptr.cpp
	ln -sf $(BIN_DIR)/$(BINARY) $(BINARY)


$(BINARY): $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(CFLAGS) $(OBJ_LINUX) $(LINK) -o $(BIN_DIR)/$(BINARY)

clean:
	rm -rf Release/*.o
	rm -rf Release/$(BINARY)
	rm -rf Debug/*.o
	rm -rf Debug/$(BINARY)
