module rk
    use ode
    implicit none
    contains
    function rk_step(r,y,h,eps)
        integer, parameter                 :: kr = selected_real_kind(16)
        real(kr)                           :: r,eps,h
        real(kr), dimension(2)             :: k1, k2, k3, k4, rk_step
        real(kr), intent(in), dimension(2) :: y
        k1 = f(r,y,eps)
        k2 = f(r + h / 2.0_kr, y + (h / 2.0_kr) * k1,eps)
        k3 = f(r + h / 2.0_kr, y + (h / 2.0_kr) * k2,eps)
        k4 = f(r + h, y + h * k3,eps)
        rk_step  = y + (h / 6.0_kr) *  (k1 + 2.0_kr * k2 + 2.0_kr * k3 + k4)
        !write(*,*) y(1),y(2)
    end function
end module rk
    
