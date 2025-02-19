# Source:
# https://gitlab.kitware.com/cmake/community/-/wikis/FAQ#can-i-do-make-uninstall-with-cmake

if(NOT EXISTS "/home/polinomio/Documents/code/fortran/gtk-fortran/build/install_manifest.txt")
    message(FATAL_ERROR "Cannot find install manifest: /home/polinomio/Documents/code/fortran/gtk-fortran/build/install_manifest.txt")
endif()

file(READ "/home/polinomio/Documents/code/fortran/gtk-fortran/build/install_manifest.txt" files)
# Building a list from the lines of the .txt file:
string(REGEX REPLACE "\n" ";" files "${files}")

foreach(file ${files})
    message(STATUS "Uninstalling $ENV{DESTDIR}${file}")

    if(IS_SYMLINK "$ENV{DESTDIR}${file}" OR EXISTS "$ENV{DESTDIR}${file}")
        exec_program(
            "/usr/bin/cmake" ARGS "-E remove \"$ENV{DESTDIR}${file}\""
            OUTPUT_VARIABLE rm_out
            RETURN_VALUE rm_retval
            )

        if(NOT "${rm_retval}" STREQUAL 0)
            message(FATAL_ERROR "Problem when removing $ENV{DESTDIR}${file}")
        endif()
    else()
        message(STATUS "File $ENV{DESTDIR}${file} does not exist.")
    endif()
endforeach(file)
