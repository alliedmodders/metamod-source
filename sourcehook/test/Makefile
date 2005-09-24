#(C)2004-2005 SourceMM Development Team
# Makefile written by David "BAILOPAN" Anderson

OPT_FLAGS = -O3 -funroll-loops -s -pipe
DEBUG_FLAGS = -g -ggdb3
CPP = gcc
BINARY = sourcehook_test

OBJECTS = test1.cpp test2.cpp test3.cpp test4.cpp testbail.cpp testbail2.cpp testlist.cpp main.cpp sourcehook.cpp

LINK = -lstdc++

INCLUDE = -I. -I..

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS = $(DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS = $(OPT_FLAGS)
endif

# CFLAGS += 

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)
	ln -sf ../sourcehook.cpp sourcehook.cpp
	$(MAKE) sourcehook_test
	rm -f $(BINARY)
	ln -sf $(BIN_DIR)/$(BINARY) $(BINARY)

sourcehook_test: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(CFLAGS) $(OBJ_LINUX) $(LINK) -o$(BIN_DIR)/$(BINARY)

debug:	
	$(MAKE) all DEBUG=true

default: all

clean:
	rm -rf Release/*.o
	rm -rf Release/$(BINARY)
	rm -rf Debug/*.o
	rm -rf Debug/$(BINARY)
