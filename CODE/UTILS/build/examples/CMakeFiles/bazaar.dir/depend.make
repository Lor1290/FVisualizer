# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.29

# Note that incremental build could trigger a call to cmake_copy_f90_mod on each re-build
examples/CMakeFiles/bazaar.dir/bazaar.f90.o: src/modules/cairo.mod
examples/CMakeFiles/bazaar.dir/bazaar.f90.o: src/modules/g.mod
examples/CMakeFiles/bazaar.dir/bazaar.f90.o: src/modules/gdk.mod
examples/CMakeFiles/bazaar.dir/bazaar.f90.o: src/modules/gdk_pixbuf.mod
examples/CMakeFiles/bazaar.dir/bazaar.f90.o: src/modules/gtk.mod
examples/CMakeFiles/bazaar.dir/bazaar.f90.o: src/modules/gtk_sup.mod
examples/CMakeFiles/bazaar.dir/bazaar.f90.o.provides.build: examples/CMakeFiles/bazaar.dir/handlers.mod.stamp
examples/CMakeFiles/bazaar.dir/handlers.mod.stamp: examples/CMakeFiles/bazaar.dir/bazaar.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod examples/bazaar_mod//handlers.mod examples/CMakeFiles/bazaar.dir/handlers.mod.stamp GNU
examples/CMakeFiles/bazaar.dir/bazaar.f90.o.provides.build: examples/CMakeFiles/bazaar.dir/my_widgets.mod.stamp
examples/CMakeFiles/bazaar.dir/my_widgets.mod.stamp: examples/CMakeFiles/bazaar.dir/bazaar.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod examples/bazaar_mod//my_widgets.mod examples/CMakeFiles/bazaar.dir/my_widgets.mod.stamp GNU
examples/CMakeFiles/bazaar.dir/bazaar.f90.o.provides.build: examples/CMakeFiles/bazaar.dir/various_functions.mod.stamp
examples/CMakeFiles/bazaar.dir/various_functions.mod.stamp: examples/CMakeFiles/bazaar.dir/bazaar.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod examples/bazaar_mod//various_functions.mod examples/CMakeFiles/bazaar.dir/various_functions.mod.stamp GNU
examples/CMakeFiles/bazaar.dir/bazaar.f90.o.provides.build:
	$(CMAKE_COMMAND) -E touch examples/CMakeFiles/bazaar.dir/bazaar.f90.o.provides.build
examples/CMakeFiles/bazaar.dir/build: examples/CMakeFiles/bazaar.dir/bazaar.f90.o.provides.build
