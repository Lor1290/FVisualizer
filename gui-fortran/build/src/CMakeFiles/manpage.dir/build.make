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

# Utility rule file for manpage.

# Include any custom commands dependencies for this target.
include src/CMakeFiles/manpage.dir/compiler_depend.make

# Include the progress variables for this target.
include src/CMakeFiles/manpage.dir/progress.make

src/CMakeFiles/manpage: src/gtk-4-fortran-modscan.1

src/CMakeFiles/manpage.dir/codegen:
.PHONY : src/CMakeFiles/manpage.dir/codegen

manpage: src/CMakeFiles/manpage
manpage: src/CMakeFiles/manpage.dir/build.make
.PHONY : manpage

# Rule to build all files generated by this target.
src/CMakeFiles/manpage.dir/build: manpage
.PHONY : src/CMakeFiles/manpage.dir/build

src/CMakeFiles/manpage.dir/clean:
	cd /home/polinomio/Documents/code/8-fortran/gtk-fortran/build/src && $(CMAKE_COMMAND) -P CMakeFiles/manpage.dir/cmake_clean.cmake
.PHONY : src/CMakeFiles/manpage.dir/clean

src/CMakeFiles/manpage.dir/depend:
	cd /home/polinomio/Documents/code/8-fortran/gtk-fortran/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/polinomio/Documents/code/8-fortran/gtk-fortran /home/polinomio/Documents/code/8-fortran/gtk-fortran/src /home/polinomio/Documents/code/8-fortran/gtk-fortran/build /home/polinomio/Documents/code/8-fortran/gtk-fortran/build/src /home/polinomio/Documents/code/8-fortran/gtk-fortran/build/src/CMakeFiles/manpage.dir/DependInfo.cmake "--color=$(COLOR)"
.PHONY : src/CMakeFiles/manpage.dir/depend

