#!/bin/bash

# Shell Script to run the code

# Remove old executable
rm sendrecv.x 

# Compile and Build the executable
mpif90 -o sendrecv.x send_recv.f90

# Run the executable with 4 processes
mpirun -np 4 sendrecv.x
