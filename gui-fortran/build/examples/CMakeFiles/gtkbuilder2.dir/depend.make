# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.31

# Note that incremental build could trigger a call to cmake_copy_f90_mod on each re-build
examples/CMakeFiles/gtkbuilder2.dir/gtkbuilder2.f90.o: src/modules/g.mod
examples/CMakeFiles/gtkbuilder2.dir/gtkbuilder2.f90.o: src/modules/gtk.mod
examples/CMakeFiles/gtkbuilder2.dir/gtkbuilder2.f90.o.provides.build: examples/CMakeFiles/gtkbuilder2.dir/handlers.mod.stamp
examples/CMakeFiles/gtkbuilder2.dir/handlers.mod.stamp: examples/CMakeFiles/gtkbuilder2.dir/gtkbuilder2.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod examples/gtkbuilder2_mod//handlers.mod examples/CMakeFiles/gtkbuilder2.dir/handlers.mod.stamp GNU
examples/CMakeFiles/gtkbuilder2.dir/gtkbuilder2.f90.o.provides.build: examples/CMakeFiles/gtkbuilder2.dir/widgets.mod.stamp
examples/CMakeFiles/gtkbuilder2.dir/widgets.mod.stamp: examples/CMakeFiles/gtkbuilder2.dir/gtkbuilder2.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod examples/gtkbuilder2_mod//widgets.mod examples/CMakeFiles/gtkbuilder2.dir/widgets.mod.stamp GNU
examples/CMakeFiles/gtkbuilder2.dir/gtkbuilder2.f90.o.provides.build:
	$(CMAKE_COMMAND) -E touch examples/CMakeFiles/gtkbuilder2.dir/gtkbuilder2.f90.o.provides.build
examples/CMakeFiles/gtkbuilder2.dir/build: examples/CMakeFiles/gtkbuilder2.dir/gtkbuilder2.f90.o.provides.build
