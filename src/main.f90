program main
    use paramets
    use ode
    use rk
    use eos
    implicit none
    logical                 :: cgs   = .false.
    logical                 :: mevfm = .true.
    logical                 :: mev4  = .false.
    logical                 :: gcm   = .false.
    real(kr)                :: gcmreal,cgsreal,mev4real,mevfmreal,geomreal
    real(kr)                :: pressure_arr_cgs
    real(kr), allocatable   :: pressure_array(:)
    real(kr), allocatable   :: eos_array(:)
    real(kr), allocatable   :: eos2(:)
    real(kr), allocatable   :: pol_array(:)
    real(kr), allocatable   :: cpb(:)
    real(kr), allocatable   :: U(:)
    integer                 :: n_arr = 0
    integer                 :: n_gesucht, EOF
    integer                 :: step
    integer                 :: k,schritte,k_alt
    integer                 :: mitte, pp
    integer                 :: j,dimensions,i
    integer                 :: anfang_lin
    integer                 :: anfang=1,ende
    integer                 :: laenge,endep
    real(kr)                :: r = 0.0_kr
    real(kr)                :: km
    real(kr)                :: alte_masse, pressure_start
    real(kr)                :: eps,eps1
    real(kr)                :: minimum
    real(kr)                :: delta_p
    real(kr)                :: first_value ! zum vergleich vom letzen
                                           ! und ersten wert
    real(kr), dimension(2)  :: dmdr
    real(kr), dimension(2)  :: y,y_cgs
    real(kr) :: last_mass, last_radi,last_p
    real(kr), dimension(2)  :: b,t
    real(kr)                :: a,m
    n_arr = 0
    step = 1
    j    = 0
    y    = y_start
    r    = r_min
    open(unit = 102, file = "../tables/tabelle_rel.csv", status = "new")
    open(unit = 118, file = "../tables/p_tabelle.csv", status = "new")
    open(unit = 128, file = "../tables/fit.csv", status = "new")

    ! === AUS TABELLE IN CGS UMRECHNEN (VON CGS) ===
    pressure_arr_cgs = 1.!pressure_array(2)
    y(1) = 1.!pressure_array(n_arr) - 10

    !NEU
    !pressure_arr_cgs = pressure_array(2) * p_mev4tomevfm * p_mevfmtocgs * (h_quer_c)**4.0_kr ! (h_bar_c)^4 rechnet von 1/fm^4 nach MeV^4 um
    !vergleich = (pressure_array(2)) * p_mev4tomevfm * p_mevfmtocgs * (h_quer_c)**4.0_kr

    !=== WERTEVERGLEICH ===
    write(*,*) ' 0'
    write(*,*) '=== VERGLEICH ==='
    write(*,*) 'user input-->'  ,vergleich
    write(*,*) 'array aus check...-->',pressure_arr_cgs
    write(*,*) 'Laenge der Liste-->', n_arr
    write(*,*) 'stepsize--->',stepsize
    write(*,*) ' '



    !=== UMRECHNUNG IN MEV4 (von cgs) ===
    pressure_array = pressure_array / (p_mevfmtocgs * p_mev4tomevfm)
    eos_array      = eos_array / (eps_mevfmtocgs * p_mev4tomevfm)
    eos2           = eos_array / (eps_mevfmtocgs * p_mev4tomevfm)



    !=== UNIT AUSGABE ===
    if (mev4 .eqv. .true.) then
    mevfmreal  = 1.0_kr
    gcmreal    = 1.0_kr
    cgsreal    = 1.0_kr
    geomreal   = p_mev4tomevfm * p_mevfmtocgs / p_geomtocgs
    !back     = eps_geomtocgs
    else if (mevfm .eqv. .true.) then
    mevfmreal  = p_mev4tomevfm
    gcmreal    = 1.0_kr
    cgsreal    = 1.0_kr
    geomreal   = p_mevfmtocgs / p_geomtocgs
    else if (gcm .eqv. .true.) then
    mevfmreal  = p_mev4tomevfm
    gcmreal    = p_mevfmtogcm
    cgsreal    = 1.0_kr
    geomreal   = (1.0_kr / p_mevfmtogcm) * p_mevfmtocgs / p_geomtocgs
    else if (cgs .eqv. .true.) then
    mevfmreal  = p_mev4tomevfm
    gcmreal    = 1.0_kr
    cgsreal    = p_mevfmtocgs
    geomreal   = 1.0_kr / p_geomtocgs
    end if

    eos_array      = eos_array * mevfmreal           ![mev/fm^3]
    eos2           = eos_array * mevfmreal           ![mev/fm^3]
    pressure_array = pressure_array * mevfmreal      ![mev/fm^3]
    eos_array      = eos_array * gcmreal             !mev/fm^3 --> [g/cm^3]
    eos2           = eos_array * mevfmreal           ![mev/fm^3]
    pressure_array = pressure_array * gcmreal        !mev/fm^3 --> [g/cm^3]
    eos_array      = eos_array * cgsreal             !mev/fm^3 --> [cgs]
    eos2           = eos_array * mevfmreal           ![mev/fm^3]
    pressure_array = pressure_array * cgsreal        !mev/fm^3 --> [cgs]
!====================================================================

    !allocate(pol_array(n_arr))
    !allocate(cpb(n_arr))
    !allocate(U(n_arr))

    open(unit=103,file='table.csv', status='new')
    open(unit=101,file='../tables/t0.benchout', status='old', action='read'&
         &,IOSTAT = EOF,FORM='formatted',access='sequential')
    write(102,*) 'e                                     ','p                &
                & ','pol                             ','cpb              ','U'
    do i = n_arr, 2, -1
            write(*,*) "i"
            write(102,*) eos_array(i),pressure_array(i),pol_array(i) &
                     &,cpb(i),U(i)
    end do

!======================================================================


   !in check_eps_p.csv schreiben
    !do i = 1,n_arr
    !   write(105,*) i,eos_array(i),pressure_array(i)
    !end do

    !call system('gnuplot -p fit')
    !open(unit = 109, file = "tmp.dat", status = "old")
    !read(109,*) a
    !write(*,*) 'a:',a

    !RUECKRECHNUNG IN GEOM UNITS
    pressure_array = pressure_array * geomreal
    eos_array      = eos_array * geomreal
    eos2           = eos_array * geomreal
    a              = a * sqrt(geomreal)


    minimum        = pressure_array(2)/2.0_kr

    !in fit.csv schreiben
    !do i = 1,n_arr
    !   write(128,*) i,eos_array(i),pressure_array(i)
    !end do

    call fit(m,t,pressure_array,eos_array,n_arr)
        laenge = n_arr
    write(*,*) 'm',m,'t',t


    !ANZAHL DER OUTER-LOOP SCHRITTE
    schritte = 1000
    mitte   = schritte/2
    !delta_p = (pressure_array(2) - pressure_array(mitte))/schritte
   delta_p = (pressure_array(2) - pressure_array(n_arr))/schritte
    !NEU delta_p = (pressure_array(2) - 0.0_kr)/schritte
    !do pp = 1, n_arr
    !    write(*,*) pressure_array(pp)
    !    end do
    k = 0
    anfang_lin = n_arr
    y_cgs(1) = 10.0_kr**35.0_kr

    outerloop: do k = 1 , 10000
        y(2) = 0.0_kr
        !y(1) = pressure_array(2) - (k-1) * delta_p
        !y_cgs(1) = y(1) * p_geomtocgs
        !if (y(1) > k*delta_p) then
            y(1) = pressure_array(2) - k * delta_p
            !pressure_start = p_geomtocgs*y(1)
            pressure_start = y(1)/geomreal
        write(*,*) k, km,  y_cgs(2), pressure_start
        if (y(1)==0.0_kr) then
            stop
        end if
        !    write(*,*) '>10^30',y_cgs(1)
        !    k_alt = k
        !else
        !    y(1) = pressure_array(2) - k_alt * delta_p
        !    delta_p = 0.99999998*delta_p
        !    y(1) = ((0.7)**k)*y(1)
        !    write(*,*) '<10^30',y_cgs(1)
        !end if
        !k = k + 1
        j = 0
        anfang = 1
        n_gesucht = 1
        dmdr = (/1.0_kr,1.0_kr/)
        alte_masse = -5.0_kr
    innerloop: do while (y(1) > 0.)!(n_gesucht <= n_arr)
        !write(*,*) y(1)
        r = r_min + j*stepsize
        y_cgs(1) = p_geomtocgs * y(1)
        y_cgs(2) = m_geomtocgs * y(2)/M_s
        km = r/100000.0_kr
        if (modulo(int(r),int(1000)) == 0.0_kr) then
            write(102,*) km, y_cgs(1)/geomreal, y_cgs(2)/geomreal,dmdr(2)
        end if
        last_mass = y_cgs(2)
        last_radi = km

        !=====================================================================
        ! LINEARE SUCHE MIT TIPP
        n_gesucht = linear_search(y(1), pressure_array,n_arr,y_start(1),anfang_lin)
        anfang_lin = n_gesucht
        ! LINEARE SUCHE AB MITTE (eos.f90)
        !n_gesucht = linear_search(y(1),pressure_array,n_arr,y_start(1))
        !=====================================================================

        !if (n_gesucht < n_arr) then
        !BINAERE SUCHE
        !dimensions = n_arr
        !ende       = n_arr
        !n_gesucht = binaery_search(y(1),pressure_array,dimensions,ende,anfang,y_start(1))

        !if (n_gesucht .eq. n_arr)  then
        !write(*,*) 'error:', '','','end of table before convergence of mass'
        !if (dmdr(1) < 0.0_kr) then
        !write(*,*) 'druck <0',n_gesucht
        !exit
        !end if
        !anfang     = n_gesucht-1
        !COMMENT===============================================================
        ! eos_array      = eos array aus der generierten tabelle
        ! pressure_array = pressure array aus tabelle
        ! y(1)           = kommt von y_selber
        !======================================================================
        eps       = interpolation(n_gesucht,y(1),n_arr,eos_array,pressure_array)
        !eps       = interpolation(n_gesucht,y(1),n_arr,eos_array,pressure_array)
        if (y(1) <= pressure_array(n_arr) .and. y(1) >= 0.0_kr .and. y(1)==y(1)) then
            !stop
            eps = m*y(1)+t(2)
            !eps = a*sqrt(y(1))
            !write(*,*) eps,y(1)
            write(128,*) y(1),eps
            !NEU stop
        end if
        if ((y_cgs(2) - alte_masse) <= 0.0006_kr &
            & .and. y_cgs(2) > 2.2_kr) then
            exit innerloop
        end if
        y         = rk_step(r,y,stepsize,eps)
        dmdr      = f(r,y,eps)
        j = j + 1
        km = r / 100000.0_kr
        alte_masse = y_cgs(2)
    end do innerloop
    !NEU if (y_cgs(1) < 0.71 .and. y_cgs(1) > 0.69) then
    !NEU    stop
    !NEU end if
    write(102,*)
    write(102,*)
    n_arr = laenge
    !if (modulo(int(k),int(100)) == 0.0_kr) then
    !end if
    ! ersten value speichern zum vergleich
    if (k == 1) then
        first_value = y_cgs(2)
    end if
    !NEU if (modulo(int(k),int(1000)) == 0.0_kr) then
        !write(*,*) 'mass:','',y_cgs(2),'km',last_radi
        write(118,*) last_radi,y_cgs(2), pressure_start
    !NEU end if
    r = r_min
    end do outerloop

    call system('gnuplot -p ../plots/p_loop.p')
    write(*,*) ' '
    write(*,*) '===ENDE==='
    write(*,*) 'first value', first_value
    write(*,*) 'mass',y_cgs(2)
    write(*,*) 'end pressure',y_cgs(1)
    write(*,*) 'radius', km
    write(*,*) 'loops', j
    write(*,*) 'dM/dr', dmdr(2)
    write(*,*) 'dp/dr', dmdr(1)
    write(*,*) ' '
    close(102)
    close(118)
    close(128)
    deallocate(pressure_array)
    deallocate(eos_array)
    deallocate(eos2)
    deallocate(cpb)
    deallocate(pol_array)
    deallocate(U)
end program main
