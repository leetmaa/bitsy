bitsy.f90
==========
A fortran module for SHA-256 hashing.

Compile and test
-----------------
To run the unit-tests install 'funit' [www.funit.org][1] and run with something like:

    FC=gfortran -fno-range-check -O3 funit

The '-fno-range-check' flag is required since the fortran standard otherwise doesn't allow us to work with all bits in the integers (as if they were unsigned).

Note
------
The quick and dirty routine (dirtys_sha256) operates on whatever bits that come in, without swapping to big-endian words, and does therefore not pas any of the standard tests - but works at roughly twice the speed. Use this if you want a good hash function but don't care about following the SHA-256 standard specifications.

Note that this code will not produce the same results on big-endian machines and the module was was only tested on a little-endian Ubuntu LTS 12.04 system using gfortran 4.6.3.


If you found this useful, please let me know.

Mikael Leetmaa <leetmaa@kth.se>

[1]: http::/www.funit.org
 
