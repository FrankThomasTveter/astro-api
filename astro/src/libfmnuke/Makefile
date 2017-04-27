#! gmake
#
# Makefile    - template Makefile to compile and maintain 
#             a fortran object library and link binaries
#             using the C preprocessor (cpp) on fortran files
#             with the '.F' suffix
#
# Calling:
#	make		- make all binaries
#	make lib	- make library
#	make check_src	- checks if source files have been
#			  changed since last library update
#	make check_libs	- checks if external libaries have been
#			  changed since last binary update
#	make check	- performs both above checks
#
# + Get the list of All fortran source files (suffix .F) 
#   in the directory.
# + Build a library which contains all fortran objects.
#   (the objects are build with selected options)
# + Link binaries from the maintained and external libraries
#
# NOTE:	Names in <> are sympolic names to be replaced by the 
#	actual ones.
#	In [] are sample or recommented settings.
#
# Macro Description (to be adapted):
# MAINS		- list of fortran files containing main programs
#		  [<prog1.F>, <prog2.F>, ...]
# LIB_NAM	- full pathname of the library to be maintained
#		  [$(HOME)/lib/lib<proj>.a]
# BIN		- directory to place the binaries in
#		  [$(HOME)/bin]
# LIBS		- library options: 
#		  -L<lib1home> -l<lib1-name>
# INCL		- include options: 
#		  -I<includedir>
# OPTIONS - fortran compile & link options: 
#				   -g - create debugger info
#
#-------------------------------------------------------------------
#
# List of Main Programs
#	(Put a list of FORTRAN source files containing main programs
#	 here) 
#
# MAINS = 
#
# Full library name
#	(All objects will be stored in a library (using the UNIX 
#	 'ar' command) - put the full pathname of this library here)
#
 LIB_NAM = libfmnuke.a
#
# Target Directory for Tools Binaries
#	(All binaries will be installed in a dedicated 
#	 'bin' directory - put the path here)
#
#BIN = $(HOME)/bin
#
# Tools Libraries
#	(Specify libraries to be linked in here. (e.g. orbit/attitude libs
#	 MIND the ORDER!)
#
# LIBS= 
#
# Include Directory
#	(Specify directories to pick up include files (MIND the ORDER!)
#
 INCL = ./
#
# Compilation flags 
#	(Put the compilation flags here (-g is recommented))
#
# OPTIONS = -g -Kieee -r8 -i4 -DLITTLE_ENDIAN -DPREC32
# OPTIONS = -fbounds-check  -g  -fdefault-real-8
 OPTIONS =-O2 -fPIC -fdefault-real-8 -fdefault-double-8
# OPTIONS = -fdefault-real-8
### -ffixed-form

 COMPILER = gfortran
#
#------------------------------------------------------------------------
#		the rest not is not likely to be changed
#------------------------------------------------------------------------
#
# BIN = ../../bin
# BIN_SUFFIX = .bin
#
 include MakeInclude_F
#
