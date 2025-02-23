export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
gfortran ${1} $(pkg-config --cflags --libs gtk-4-fortran) -no-pie -o ${2}
./${2} 2>/dev/null
