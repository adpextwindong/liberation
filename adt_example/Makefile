CC = gcc
CFLAGS = "-Wall -std=c99"

clean: rm *.o

adt.o:
	cc -c adt.c -o adt.o

adt: adt.o
	cc adt.o -o main_adt.exe
