CC=gcc
CCFLAGS=-c -g -I.. -DSH_DEBUG

test.bin: bin bin/main.o bin/test1.o bin/test2.o bin/test3.o bin/test4.o bin/sourcehook.o
	$(CC) bin/main.o bin/test1.o bin/test2.o bin/test3.o bin/test4.o bin/sourcehook.o -lstdc++ -o test.bin

bin:
	mkdir bin

bin/main.o: main.cpp ../sourcehook.h
	$(CC) $(CCFLAGS) -o bin/main.o main.cpp

bin/test1.o: test1.cpp ../sourcehook.h
	$(CC) $(CCFLAGS) -o bin/test1.o test1.cpp

bin/test2.o: test2.cpp ../sourcehook.h
	$(CC) $(CCFLAGS) -o bin/test2.o test2.cpp

bin/test3.o: test3.cpp ../sourcehook.h
	$(CC) $(CCFLAGS) -o bin/test3.o test3.cpp

bin/test4.o: test4.cpp ../sourcehook.h
	$(CC) $(CCFLAGS) -o bin/test4.o test4.cpp

bin/sourcehook.o: ../sourcehook.cpp ../sourcehook.h
	$(CC) $(CCFLAGS) -o bin/sourcehook.o ../sourcehook.cpp

../sourcehook.h: ../generate/sourcehook.hxx
	(cd ../generate; ./generate 16)
