# primtall
Fortran finn primtall
# Compile
gfortran prime.f90 -o prime - singlecore
gfortran prime.f90 -fopenmp -o prime - multicore

# Run
./prime

# Todo
Multicore