#!/bin/bash

# Shell Script to run the code

# Remove old executable
rm hellompi.x 

# Compile and Build the executable
mpif90 -o hellompi.x hello_mpi.f90

# Run the executable with 4 processes
mpirun -np 4 hellompi.x
