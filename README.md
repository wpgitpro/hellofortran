# hellofortran
Sampling of programs written in Fortran

Compiling f90 program with c subs
gfortran -c fprog.f90
gcc -c -fno-leading-underscore csub.c
gfortran fprog.o csub.o -o fprog
