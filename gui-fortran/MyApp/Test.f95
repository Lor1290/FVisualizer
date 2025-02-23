module math
    use, intrinsic :: iso_c_binding, only: dp=>c_double
    
    implicit none
    contains 
        pure real(dp) function iterate(x0, r) result(x)
            real(dp), intent(in) :: x0, r
            integer :: i

            x = x0 
            do 50 i = 0, 20000
                x = r*x*(1_dp-x)
50          continue 

        endfunction iterate 
endmodule math       

module handler
    use, intrinsic :: iso_c_binding
    use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE, &
                   gtk_application_window_new, gtk_widget_show, &
                   gtk_window_set_title, g_signal_connect, gtk_box_new, &
                   GTK_ORIENTATION_VERTICAL, gtk_window_set_child, &
                   gtk_button_new_with_label, gtk_box_append, &
                   gtk_window_set_default_size, gtk_label_new, &
                   gtk_spin_button_new, gtk_adjustment_new

    
    use g, only: g_application_run, g_object_unref

    implicit none
    contains

        ! loop function
        subroutine activate(app, gdata) bind(c)
            type(c_ptr), value, intent(in) :: app, gdata
            type(c_ptr) :: window
            type(c_ptr) :: box
            type(c_ptr) :: my_button 
            type(c_ptr) :: label
            type(c_ptr) :: r_spin_button

            ! Create a window and set: title + size 
            window = gtk_application_window_new(app) 
            call gtk_window_set_title(window, "MyFirstApp"//c_null_char)
            call gtk_window_set_default_size(window, 600, 400)

            ! Create a box and append to widnow
            box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10_c_int)
            call gtk_window_set_child(window, box) 

            ! Create a button and append to box
            my_button = gtk_button_new_with_label("Compute"//c_null_char)    
            call gtk_box_append(box, my_button)
            call g_signal_connect(my_button, "clicked"//c_null_char, c_funloc(my_button_clicked))

            ! Create a label and append to box
            label = gtk_label_new("r param."//c_null_char)
            call gtk_box_append(box, label)

            ! Create a spin button and append to box
            r_spin_button = gtk_spin_button_new(gtk_adjustment_new(3._dp, 0._dp, 4._dp, 0.1_dp, 0._dp, 0._dp), &
                                                                   0.0_dp, 15_c_int)                     
            call gtk_box_append(box, r_spin_button)

            ! View the window
            call gtk_widget_show(window) 
        endsubroutine activate    

        subroutine my_button_clicked(widget, gdata) bind(c)
            type(c_ptr), value, intent(in) :: widget, gdata 
            write(6, 100)
100         format(1X, "Button Clicked!")
        endsubroutine my_button_clicked

endmodule handler

program main
    use handler
    
    implicit none
    type(c_ptr) :: app  
    integer(c_int) :: status

    ! Dfn. the app:  -name -termin. -flag (https://docs.gtk.org/gio/flags.ApplicationFlags.html)
    app = gtk_application_new("gtk.MyFirstApp"//c_null_char, G_APPLICATION_FLAGS_NONE)
   
    ! Connect the app  
    call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), c_null_ptr)
   
    ! Run the app: -name -int arg -char **args
    status = g_application_run(app, 0_c_int, [c_null_ptr])
    
    ! Ret. status code (freee the memory)
    call g_object_unref(app)
endprogram main    
