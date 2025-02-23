module globals
        use, intrinsic :: iso_c_binding
        type(c_ptr) :: app
end module globals

module math
        use, intrinsic :: iso_c_binding, only: dp => c_double
        implicit none

        contains
                pure real(dp) function iterate(x0, r) result(x)
                        real(dp), intent(in) :: x0, r
                        integer(kind=4) :: y

                        x = x0
                        do 100 y = 0, 20000
                                x = r*x*(1_dp-x)
100                     continue
                endfunction iterate
endmodule math

module handler
        use globals, only: app
        use math, only: iterate
        use, intrinsic :: iso_c_binding
        
        use cairo, only: cairo_paint
        use g, only: g_application_run, g_object_unref, g_application_quit
        use gdk, only: gdk_cairo_set_source_pixbuf
        use gdk_pixbuf, only: gdk_pixbuf_new, gdk_pixbuf_get_rowstride, gdk_pixbuf_get_pixels, gdk_pixbuf_get_n_channels
        use gtk, only: g_signal_connect, gtk_application_new, gtk_application_window_new, gtk_window_set_child, &
        gtk_window_set_title, gtk_window_set_default_size, gtk_widget_show, gtk_box_new, gtk_button_new_with_label, gtk_label_new, &
        gtk_box_append, gtk_spin_button_new, gtk_spin_button_get_value, gtk_adjustment_new, gtk_widget_set_size_request, &
        gtk_drawing_area_set_draw_func, gtk_drawing_area_new, gtk_widget_queue_draw, GDK_COLORSPACE_RGB, GTK_ORIENTATION_VERTICAL, G_APPLICATION_FLAGS_NONE 
          
        implicit none
        type(c_ptr) :: spin_button, draw_area, pixbuf
        integer(kind =c_int) :: nch, rowstride, pixwidth, pixheigth
        character(kind=c_char), dimension(:), pointer :: pixel 
        
        contains
                subroutine active(app, gdata) bind(c)
                        integer, parameter :: dp = kind(1.0d0)
                        
                        type(c_ptr), value, intent(in) :: app, gdata
                        type(c_ptr) :: box
                        type(c_ptr) :: c_button
                        type(c_ptr) :: r_button
                        type(c_ptr) :: e_button
                        type(c_ptr) :: d_button
                        type(c_ptr) :: real_num
                        type(c_ptr) :: window

                        ! WINDOW
                        window = gtk_application_window_new(app)
                        call gtk_window_set_title(window, 'fortran - GUI' // c_null_char)
                        call gtk_window_set_default_size(window, 600, 400)                                              

                        ! MAIN - BOX
                        box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10_c_int)
                        call gtk_window_set_child(window, box)
                       
                        ! DRAWING AREA
                        draw_area = gtk_drawing_area_new()
                        pixwidth = 900
                        pixheigth = 600

                        call gtk_widget_set_size_request(draw_area, pixwidth, pixheigth)
                        call gtk_drawing_area_set_draw_func(draw_area, c_funloc(draw), c_null_ptr, c_null_funptr)
                        call gtk_box_append(box, draw_area) 
        
                        pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, 0, 8_c_int, pixwidth, pixheigth)
                        nch = gdk_pixbuf_get_n_channels(pixbuf)
                        rowstride = gdk_pixbuf_get_rowstride(pixbuf)
                        call c_f_pointer(gdk_pixbuf_get_pixels(pixbuf), pixel, (/pixwidth*pixheigth*nch/))
                        pixel = char(0)

                        ! RANDOM - FUNC 
                        real_num = gtk_label_new('r parameter' // c_null_char)
                        call gtk_box_append(box, real_num)
                        spin_button = gtk_spin_button_new(gtk_adjustment_new(3._dp, 0._dp, 4._dp, 0.1_dp, 0._dp, 0._dp), 0.0_dp, 15_c_int)
                        call gtk_box_append(box, spin_button)
                        
                        ! SIMPLE - BUTTON
                        r_button = gtk_button_new_with_label('print random' // c_null_char)
                        call gtk_box_append(box, r_button)
                        call g_signal_connect(r_button, 'clicked' // c_null_char, c_funloc(button_random))

                        c_button = gtk_button_new_with_label('compute' // c_null_char)
                        call gtk_box_append(box, c_button)
                        call g_signal_connect(c_button, 'clicked' // c_null_char, c_funloc(button_clicked))
          
                        d_button = gtk_button_new_with_label('draw' // c_null_char)
                        call gtk_box_append(box, d_button)
                        call g_signal_connect(c_button, 'clicked' // c_null_char, c_funloc(button_draw))
          
                        e_button = gtk_button_new_with_label('exit' // c_null_char)
                        call gtk_box_append(box, e_button)
                        call g_signal_connect(e_button, 'clicked' // c_null_char, c_funloc(button_exit)) 

                        call gtk_widget_show(window)
                endsubroutine active
        
                ! **************************
                ! *** SUBROUTINE SECTION ***
                ! **************************

                ! ******** BUTTON ********
                subroutine button_clicked(widget, gdata) bind(c)
                        type(c_ptr), value, intent(in) :: widget, gdata
                        write(6, 100)
100                     format('button clicked!')
                endsubroutine button_clicked

                subroutine button_random(widget, gdata) bind(c)
                        type(c_ptr), value, intent(in) :: widget, gdata
                        integer, parameter :: dp = kind(1.0d0)
                        real(dp) :: r, x0
                       
                        call random_number(x0)
                        r = gtk_spin_button_get_value(spin_button)
                        
                        print *,  r, x0, iterate(x0, r)
               endsubroutine button_random

               subroutine button_exit(widget, gdata) bind(c) 
                      type(c_ptr), value,  intent(in) :: widget, gdata
                      call g_application_quit(app)
               endsubroutine button_exit
                
               subroutine button_draw(widget, gdata) bind(c)
                        type(c_ptr), value, intent(in) :: widget, gdata
                        integer, parameter :: dp = kind(1.0d0)
                        real(dp) :: r, x0
                        real(dp) :: rmin, rmax
                        integer :: p, n, xp, yp, xpmax, ypmax

                        call random_seed()

                        rmin = gtk_spin_button_get_value(spin_button)
                        rmax = 5_dp
                        xpmax = pixwidth-1
                        ypmax = pixheigth-1
                        pixel = char(0)

                        do 100 xp = 0, xpmax
                                r = rmin + xp * (rmax - rmin) / xpmax

                                do 200 n = 1, 100
                                        call random_number(x0)
                                        yp = ypmax - nint(iterate(x0, r) * ypmax)

                                        p = 1 + xp*nch + yp*rowstride
                                        pixel(p) = char(255)
                                        pixel(p+1) = char(150)
                                        pixel(p+2) = char(120)
200                             continue
100                     continue

                        call gtk_widget_queue_draw(draw_area)
                endsubroutine button_draw

               ! ******* ANIMATION *********
               subroutine draw(widget, cairo, gdata) bind(c)
                       type(c_ptr), value, intent(in) :: widget, cairo, gdata
                                             
                       call gdk_cairo_set_source_pixbuf(cairo, pixbuf, 0d0, 0d0)
                       call cairo_paint(cairo)
               endsubroutine draw
endmodule handler

! *******************
! **** MAIN LOOP ****
! *******************
program main
        use handler
        use globals, only: app
                
        implicit none
        integer(c_int) :: status

        app = gtk_application_new('gtk-fortran.main' // c_null_char, G_APPLICATION_FLAGS_NONE) 
        call g_signal_connect(app, 'activate' // c_null_char, c_funloc(active), c_null_ptr)          
        status = g_application_run(app, 0_c_int, [c_null_ptr])
        call g_object_unref(app)

endprogram main
