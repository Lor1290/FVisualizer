# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.31

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/polinomio/Documents/code/8-fortran/gtk-fortran

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/polinomio/Documents/code/8-fortran/gtk-fortran/build

# Include any dependencies generated for this target.
include examples/CMakeFiles/gtkzero_gapp.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include examples/CMakeFiles/gtkzero_gapp.dir/compiler_depend.make

# Include the progress variables for this target.
include examples/CMakeFiles/gtkzero_gapp.dir/progress.make

# Include the compile flags for this target's objects.
include examples/CMakeFiles/gtkzero_gapp.dir/flags.make

examples/CMakeFiles/gtkzero_gapp.dir/codegen:
.PHONY : examples/CMakeFiles/gtkzero_gapp.dir/codegen

examples/CMakeFiles/gtkzero_gapp.dir/gtkzero_gapp.f90.o: examples/CMakeFiles/gtkzero_gapp.dir/flags.make
examples/CMakeFiles/gtkzero_gapp.dir/gtkzero_gapp.f90.o: /home/polinomio/Documents/code/8-fortran/gtk-fortran/examples/gtkzero_gapp.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --progress-dir=/home/polinomio/Documents/code/8-fortran/gtk-fortran/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object examples/CMakeFiles/gtkzero_gapp.dir/gtkzero_gapp.f90.o"
	cd /home/polinomio/Documents/code/8-fortran/gtk-fortran/build/examples && /usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /home/polinomio/Documents/code/8-fortran/gtk-fortran/examples/gtkzero_gapp.f90 -o CMakeFiles/gtkzero_gapp.dir/gtkzero_gapp.f90.o

examples/CMakeFiles/gtkzero_gapp.dir/gtkzero_gapp.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Preprocessing Fortran source to CMakeFiles/gtkzero_gapp.dir/gtkzero_gapp.f90.i"
	cd /home/polinomio/Documents/code/8-fortran/gtk-fortran/build/examples && /usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /home/polinomio/Documents/code/8-fortran/gtk-fortran/examples/gtkzero_gapp.f90 > CMakeFiles/gtkzero_gapp.dir/gtkzero_gapp.f90.i

examples/CMakeFiles/gtkzero_gapp.dir/gtkzero_gapp.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Compiling Fortran source to assembly CMakeFiles/gtkzero_gapp.dir/gtkzero_gapp.f90.s"
	cd /home/polinomio/Documents/code/8-fortran/gtk-fortran/build/examples && /usr/bin/f95 $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /home/polinomio/Documents/code/8-fortran/gtk-fortran/examples/gtkzero_gapp.f90 -o CMakeFiles/gtkzero_gapp.dir/gtkzero_gapp.f90.s

# Object files for target gtkzero_gapp
gtkzero_gapp_OBJECTS = \
"CMakeFiles/gtkzero_gapp.dir/gtkzero_gapp.f90.o"

# External object files for target gtkzero_gapp
gtkzero_gapp_EXTERNAL_OBJECTS =

examples/gtkzero_gapp: examples/CMakeFiles/gtkzero_gapp.dir/gtkzero_gapp.f90.o
examples/gtkzero_gapp: examples/CMakeFiles/gtkzero_gapp.dir/build.make
examples/gtkzero_gapp: examples/CMakeFiles/gtkzero_gapp.dir/compiler_depend.ts
examples/gtkzero_gapp: src/libgtk-4-fortran.a
examples/gtkzero_gapp: examples/CMakeFiles/gtkzero_gapp.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --bold --progress-dir=/home/polinomio/Documents/code/8-fortran/gtk-fortran/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking Fortran executable gtkzero_gapp"
	cd /home/polinomio/Documents/code/8-fortran/gtk-fortran/build/examples && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/gtkzero_gapp.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
examples/CMakeFiles/gtkzero_gapp.dir/build: examples/gtkzero_gapp
.PHONY : examples/CMakeFiles/gtkzero_gapp.dir/build

examples/CMakeFiles/gtkzero_gapp.dir/clean:
	cd /home/polinomio/Documents/code/8-fortran/gtk-fortran/build/examples && $(CMAKE_COMMAND) -P CMakeFiles/gtkzero_gapp.dir/cmake_clean.cmake
.PHONY : examples/CMakeFiles/gtkzero_gapp.dir/clean

examples/CMakeFiles/gtkzero_gapp.dir/depend:
	cd /home/polinomio/Documents/code/8-fortran/gtk-fortran/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/polinomio/Documents/code/8-fortran/gtk-fortran /home/polinomio/Documents/code/8-fortran/gtk-fortran/examples /home/polinomio/Documents/code/8-fortran/gtk-fortran/build /home/polinomio/Documents/code/8-fortran/gtk-fortran/build/examples /home/polinomio/Documents/code/8-fortran/gtk-fortran/build/examples/CMakeFiles/gtkzero_gapp.dir/DependInfo.cmake "--color=$(COLOR)"
.PHONY : examples/CMakeFiles/gtkzero_gapp.dir/depend

