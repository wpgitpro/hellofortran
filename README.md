# hellofortran
Sampling of programs written in Fortran

Compiling f90 program with c subs
gfortran -c fprog.f90
gcc -c -fno-leading-underscore csub.c
gfortran fprog.o csub.o -o fprog

./fprog
abcd 5 4711 4712.000000 13 14
