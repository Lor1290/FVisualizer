file(REMOVE_RECURSE
  ".4.6.0"
  "libgtk-4-fortran.pdb"
  "libgtk-4-fortran.so"
  "libgtk-4-fortran.so.4.6.0"
)

# Per-language clean rules from dependency scanning.
foreach(lang Fortran)
  include(CMakeFiles/gtk-fortran_shared.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
