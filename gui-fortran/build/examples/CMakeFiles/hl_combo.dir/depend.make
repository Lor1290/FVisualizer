# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.31

# Note that incremental build could trigger a call to cmake_copy_f90_mod on each re-build
examples/CMakeFiles/hl_combo.dir/hl_combo.f90.o: src/modules/gtk.mod
examples/CMakeFiles/hl_combo.dir/hl_combo.f90.o: src/modules/gtk_hl_button.mod
examples/CMakeFiles/hl_combo.dir/hl_combo.f90.o: src/modules/gtk_hl_combobox.mod
examples/CMakeFiles/hl_combo.dir/hl_combo.f90.o: src/modules/gtk_hl_container.mod
examples/CMakeFiles/hl_combo.dir/hl_combo.f90.o.provides.build: examples/CMakeFiles/hl_combo.dir/handlers.mod.stamp
examples/CMakeFiles/hl_combo.dir/handlers.mod.stamp: examples/CMakeFiles/hl_combo.dir/hl_combo.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod examples/hl_combo_mod//handlers.mod examples/CMakeFiles/hl_combo.dir/handlers.mod.stamp GNU
examples/CMakeFiles/hl_combo.dir/hl_combo.f90.o.provides.build:
	$(CMAKE_COMMAND) -E touch examples/CMakeFiles/hl_combo.dir/hl_combo.f90.o.provides.build
examples/CMakeFiles/hl_combo.dir/build: examples/CMakeFiles/hl_combo.dir/hl_combo.f90.o.provides.build
