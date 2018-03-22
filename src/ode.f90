module ode
    use paramets
    use eos
    contains
    function f(r,y,eps)
        integer,parameter                  :: kr = selected_real_kind(16) 
        real(kr), intent(in), dimension(2) :: y
        real(kr), intent(in)               :: r
        real(kr), dimension(2)             :: f
        real(kr)                           :: eps,ta,tb,tc,td
        if (r <= int(1e-40).or. y(2) <= int(1e-40)) then 
            ta   = - 4.0_kr * G_g * eps * y(1) * r * pi/ c_g**4.0_kr
            tb   = 1.0_kr
            tc   = 1.0_kr
            td   = 1.0_kr 
            f(1) = ta * tb * tc / td
            f(2) = (4.0_kr * eps**2.0_kr * pi * r**2.0 / c_g**2.0_kr) 
        else
            ta   = - G_g * eps * y(2) / (r**2.0 * c_g**2.0_kr )
            tb   = 1.0_kr + y(1) / eps
            tc   = 1.0_kr + 4.0 * pi * r **3.0_kr * y(1) / (y(2) * c_g**2.0_kr)
            td   = 1.0_kr - G_g * 2.0 * y(2) / (r*c_g**2.0_kr)
            f(1) = ta * tb * tc / td
            f(2) = (4.0_kr * eps * pi * r**2.0_kr / c_g**2.0_kr) 
        end if
    end function f
end module ode

   
