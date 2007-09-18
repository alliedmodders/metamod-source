default: shworker

bin: 
	mkdir bin

fd_hopter.o: fd_hopter.cpp bin
	gcc -fPIC -O2 -o bin/$@ -c $<

main.o: shworker.cpp bin
	gcc -fPIC -O2 -o bin/$@ -c $<

shworker: fd_hopter.o main.o bin
	gcc -ldl -lstdc++ bin/fd_hopter.o bin/main.o -o bin/shworker.bin
