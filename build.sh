#!/bin/bash
python3 -m numpy.f2py --f90flags="-fopenmp -O3 -ffast-math" \
-lgomp \
-c \
kinds/kinds.f90 \
qrmod/qrmod.f90 \
-m f_qrmod

# python3 -m numpy.f2py --quiet --f90flags="-fopenmp -O3 -ffast-math"
