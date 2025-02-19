module global
        use, intrinsic :: iso_c_binding
        type(c_ptr) :: app
endmodule global

module math
        use, intrinsic :: iso_c_binding, only: dp => c_double
        implicit none

        contains
endmodule math

module handler
endmodule handler

program main
        use handler
        use global

        implicit none
        integer(c_int) :: status

        app = gtk_application_new('gtk-fortran.main' // c_null_char, G_APPLICATION_FLAGS_NONE)

endprogram main
