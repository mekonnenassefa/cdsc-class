#!/bin/bash

# Shell Script to run the code

# Remove old executable
rm bcast.x 

# Compile and Build the executable
mpif90 -o bcast.x bcast.f90

# Run the executable with 4 processes
mpirun -np 4 bcast.x
