# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.31

# Note that incremental build could trigger a call to cmake_copy_f90_mod on each re-build
examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o: src/modules/cairo.mod
examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o: src/modules/g.mod
examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o: src/modules/gdk.mod
examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o: src/modules/gdk_events.mod
examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o: src/modules/gdk_pixbuf_hl.mod
examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o: src/modules/gtk.mod
examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o: src/modules/gtk_draw_hl.mod
examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o: src/modules/gtk_hl_container.mod
examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o.provides.build: examples/CMakeFiles/hl_cairo_clock.dir/cl_handlers.mod.stamp
examples/CMakeFiles/hl_cairo_clock.dir/cl_handlers.mod.stamp: examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod examples/hl_cairo_clock_mod//cl_handlers.mod examples/CMakeFiles/hl_cairo_clock.dir/cl_handlers.mod.stamp GNU
examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o.provides.build:
	$(CMAKE_COMMAND) -E touch examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o.provides.build
examples/CMakeFiles/hl_cairo_clock.dir/build: examples/CMakeFiles/hl_cairo_clock.dir/hl_cairo_clock.f90.o.provides.build
