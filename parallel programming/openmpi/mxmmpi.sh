#!/bin/bash

# Shell Script to run the code

# Remove old executable
rm mxmmpi.x 

# Compile and Build the executable
mpif90 -o mxmmpi.x matmat_mpi.f90

# Run the executable with 4 processes
mpirun -v -np 4 mxmmpi.x
