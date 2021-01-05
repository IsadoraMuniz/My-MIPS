flex Cmenos.l
bison -d Cmenos.y
gcc -c *.c
gcc -o Cmenos *.o -ly -lfl
./Cmenos teste1.Cmenos
