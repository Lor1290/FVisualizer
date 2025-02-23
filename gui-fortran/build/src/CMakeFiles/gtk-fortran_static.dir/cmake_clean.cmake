file(REMOVE_RECURSE
  "libgtk-4-fortran.a"
  "libgtk-4-fortran.pdb"
)

# Per-language clean rules from dependency scanning.
foreach(lang Fortran)
  include(CMakeFiles/gtk-fortran_static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
