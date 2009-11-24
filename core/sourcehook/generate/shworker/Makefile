CPP = gcc

default: shworker

bin: 
	mkdir bin

fd_hopter.o: fd_hopter.cpp bin
	$(CPP) -fPIC -O2 -o bin/$@ -c $<

main.o: shworker.cpp bin
	$(CPP) -fPIC -O2 -o bin/$@ -c $<

shworker: fd_hopter.o main.o bin
	$(CPP) -ldl -lstdc++ bin/fd_hopter.o bin/main.o -o bin/shworker.bin
