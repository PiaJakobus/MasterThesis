program mainwrite
    use paramets
    
    real(kr)                :: a   
    logical                 :: cgs   = .true.
    logical                 :: mevfm = .false.
    logical                 :: mev4  = .false.
    logical                 :: gcm   = .false.
    logical                 :: fm4   = .false.
    real(kr)                :: fm4real
    real(kr)                :: gcmreal
    real(kr)                :: cgsreal
    real(kr)                :: mev4real
    real(kr)                :: mevfmreal
    real(kr)                :: geomreal
    real(kr)                :: n_n0
    real(kr)                :: p
    real(kr)                :: eps      !cgs
    real(kr)                :: p_skyrme !cgs
    real(kr)                :: eps_skyrme
    real(kr)                :: eps_fn
    real(kr)                :: p_gm
    real(kr)                :: h  !Schrittweite
    integer                 :: i = 0
    integer                 :: n
    logical                 :: loop1 = .false.
    schritte = 10000
    h        = (p_max - p_min)/schritte
    p        = p_max
    n        = n_ende
    
    !================UNITS================    
     if (mev4 .eqv. .true.) then
    fm4real    = 1.0_kr
    mevfmreal  = 1.0_kr
    gcmreal    = 1.0_kr
    cgsreal    = 1.0_kr
    geomreal   = p_mev4tomevfm * p_mevfmtocgs / p_geomtocgs
    else if (fm4 .eqv. .true.) then
    fm4real    = 1.0_kr / (h_quer_c)**4.0_kr
    mevfmreal  = 1.0_kr
    gcmreal    = 1.0_kr
    cgsreal    = 1.0_kr
    geomreal   = h_quer_c * p_mev4tomevfm * p_mevfmtocgs / p_geomtocgs
    !back     = eps_geomtocgs
    else if (mevfm .eqv. .true.) then
    fm4real    = 1.0_kr
    mevfmreal  = p_mev4tomevfm
    gcmreal    = 1.0_kr
    cgsreal    = 1.0_kr
    geomreal   = p_mevfmtocgs / p_geomtocgs
    else if (gcm .eqv. .true.) then
    fm4real    = 1.0_kr
    mevfmreal  = p_mev4tomevfm
    gcmreal    = p_mevfmtogcm
    cgsreal    = 1.0_kr
    geomreal   = (1.0_kr / p_mevfmtogcm) * p_mevfmtocgs / p_geomtocgs
    else if (cgs .eqv. .true.) then
    fm4real    = 1.0_kr
    mevfmreal  = p_mev4tomevfm
    gcmreal    = 1.0_kr
    cgsreal    = p_mevfmtocgs
    geomreal   = 1.0_kr / p_geomtocgs
    end if

    !=====================================



    !STERN WIRD VON INNEN NACH AUSSEN DURCHLAUFEN	
    !============================================
    open(unit=120,file='eos.csv',status='new',action='write')
    open(unit=121,file='skyrme.csv',status='new',action='write')
    
    do while (n >= n_start)
        !=========CGS UNITS===========
        eps_skyrme = (mn_nu * n + (0.3_kr / mn_nu) * (3.0_kr * pi**2.0_kr)&
                     &**(2.0_kr/3.0_kr) * n**(5.0_kr/3.0_kr)&
                     & + (t3 / 24.0_kr) * n**3.0_kr - (t0 / 4.0_kr) * &
                     & n**2.0_kr)
        p_skyrme   = (0.2_kr / mn_nu) * (3.0_kr*pi**2.0_kr)**(2.0_kr/3.0_kr)&
                     & * n**(5.0_kr/3.0_kr) + (t3 / &
                     &12.0_kr) * n**(3.0_kr) - (t0 / 4.0_kr) * n**2.0_kr
        !fuer free neutron gas muss p in geom units sein
        p_gm       = p_skyrme * p_mev4tomevfm * p_mevfmtocgs / p_geomtocgs
        eps_fn     = (p_gm/K_fn)**(1.0_kr/gamma_nr)/geomreal/2.0_kr
 
        eps_skyrme = eps_skyrme * fm4real   !in 1/fm4   
        p_skyrme   = p_skyrme * fm4real      
        !eps_fn     = p_skyrme * fm4real      
        eps_skyrme = eps_skyrme * mevfmreal ! in mevfm
        p_skyrme   = p_skyrme * mevfmreal    
        !eps_fn     = p_skyrme * fm4real      
        eps_skyrme = eps_skyrme * gcmreal   !in g/cm
        p_skyrme   = p_skyrme * gcmreal    
        !eps_fn     = p_skyrme * fm4real      
        eps_skyrme = eps_skyrme * cgsreal   !in cgs
        p_skyrme   = p_skyrme * cgsreal  
        !eps_fn     = p_skyrme * fm4real      
        
        !p_gm       = p_skyrme * geomreal
        !p_skyrme   = p_skyrme / geomreal
        !eps_skyrme = eps_skyrme / geomreal
        n_n0       = n/(h_quer_c**3.0_kr*n0)
        write(121,*)  p_skyrme,eps_skyrme, eps_fn,n_n0
        n          = n / 1.01_kr
        if (i == 0) then
            write(*,*) '========================='
            write(*,*) 'Druck bei r=0:',p_skyrme
            write(*,*) '========================='
        end if 

        !========RUECKRECHNUNG IN GEOM UNITS====
        p_skyrme   = p_skyrme * geomreal  
        eps_skyrme = eps_skyrme * geomreal 
        eps_fn     = eps_fn * geomreal
        write(120,*) eps_skyrme,p_skyrme
        i = i + 1
    end do 
    close(21)
    close(20)
    write(*,*) 'Eintraege:',i
    write(*,*) '========================='
end program mainwrite

