default: shworker

bin: 
	mkdir bin

fd_hopter.o: fd_hopter.cpp
	gcc -fPIC -o bin/$@ -c $<

main.o: main.cpp
	gcc -fPIC -o bin/$@ -c $<

shworker: fd_hopter.o main.o
	gcc -ldl -lstdc++ bin/fd_hopter.o bin/main.o -o bin/shworker


