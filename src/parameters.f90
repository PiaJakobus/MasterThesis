module paramets
    implicit none
    integer, parameter :: kr = selected_real_kind(16)


    
!naturkonstanten im CGS-System
!===============================
    real(kr), parameter :: c     = 29979245800.0_kr                      ! [cm/s]
    real(kr), parameter :: G     = 6.6740831_kr*10.0_kr**(-8.0_kr)       ! [cm^3 g^-1 s^-2]
    real(kr), parameter :: h_bar = 1.05457266_kr*10.0_kr**(-27.0_kr)     ! [erg s]
    real(kr), parameter :: m_n   = 1.674927471214_kr*10.0_kr**(-24.0_kr) ! [g]
    real(kr), parameter :: m_e   = 9.1093835611_kr*10.0_kr**(-28.0_kr)        ! [g]
    real(kr), parameter :: cb    = c**2.0_kr                             ! [cm^2/s^2]
    real(kr), parameter :: pi    = 3.14159265359_kr
    real(kr), parameter :: M_s   = 1.98892_kr*10.0_kr**33.0_kr           ! [g]


! SKYRME PARAMETRIZATION NATURAL UNITS
!=======================================   
    real(kr), parameter  :: h_quer_c     = 197.32705_kr                        ![MeVfm]
    real(kr), parameter  :: n0           = 0.16_kr                             ![fm^-3]
    !===EINHEIT DER SKYRME GLEICHUNG p = [MeV^4] eps = [MeV^4]===NATURAL  UNITS!!===
    ! [MeV/fm^3] = [Mev^4]
    !===============================================================================
    real(kr),parameter             :: n_start      = 0.0001_kr*n0*(h_quer_c)**3.0_kr       ![MeV^3]
    real(kr), parameter            :: n_ende       = 20.0_kr*n0*(h_quer_c)**3.0_kr       ![MeV^3]
    real(kr), parameter            :: h_barx       = 1.0_kr
    real(kr), parameter            :: mn_nu        = 939.5654133_kr                      ![MeV]
    real, parameter                :: t0           = 1024.1_kr/(197.33_kr**3.0_kr)       ![1/MeV^2]
    real, parameter                :: t3           = 14600.8_kr/(197.33_kr**6.0_kr)      ![1/MeV^5]
    !===UMRECHNUNG ZU [MeV/fm^3], [g/cm^3] und CGS=====
    real(kr), parameter :: eps_mev4tomevfm     = 1.0_kr/(h_quer_c**3.0_kr)                 ![MeV/fm^3]
    !real(kr), parameter :: eps_mev4togcm      = eps_mev4tomevfm * 1.78266567_kr*10.0_kr**12.0_kr            !=E[J]/c^2 * 1000 --> [g/cm^3]
    !real(kr), parameter :: eps_mevfmtocgs      = 1.60218_kr*10.0_kr**33.0_kr               ![erg/cm^3]
    real(kr),parameter  :: eps_mevfmtogcm      = 1.7826657_kr*10.0_kr**12.0_kr             ! [g/cm^3] 
    real(kr), parameter :: eps_mevfmtocgs      = eps_mevfmtogcm * c**2.0_kr               ![erg/cm^3]
    real(kr), parameter :: p_mev4tomevfm       = 1.0_kr/(h_quer_c**3.0_kr)                 ![MeV/fm^3]   
    real(kr), parameter :: p_mevfmtocgs     = 1.60218_kr*10.0_kr**33.0_kr     
    real(kr), parameter :: p_mevfmtogcm     = 1.7826657_kr*10.0_kr**12.0_kr
!================================================================================================================================
! DER CLOU BESTEHT DARIN DIE EINHEIT DER GLEICHUNG [MeV^4] ERSTMAL IN [MeV/fm^3] UMZURECHNEN, DAS HABE ICH JA SCHON HINBEKOMMEN
! ANSCHLIESSEND WIRD NUN [MeV] IN [g] MITTELS E=mc^2 UND FEMTOMETER = 10^-13CM UMGERECHNET (DAS ERGIBT DANN [g/cm^3])
! DANN WIRD GRAMM IN ERG MITTELS NATURAL UNITS, WO MASSE=1/MeV (WIKI:-->DURCH C^2 TEILEN) IST ZURUECKGERECHNET [ERG/CM^3]
! P WIRD AUS EINEM GRUND DEN ICH NOCH NICHT VERSTEHE EINFACH IN [ERG/CM^3] UMGERECHNET...???


!muliplikationsfaktoren von geometrischen in CGS-Einheiten
!===========================================================

    real(kr), parameter :: c_geomtocgs   = 1.0_kr  
    real(kr), parameter :: G_geomtocgs   = 1.0_kr
    real(kr), parameter :: p_geomtocgs   = c**4.0_kr/G
    real(kr), parameter :: eps_geomtocgs = c**4.0_kr/G
    real(kr), parameter :: m_geomtocgs   = c**2.0_kr/G
    real(kr), parameter :: cb_geomtocgs  = 1.0_kr
    real(kr), parameter :: gamma_nr      = 5.0_kr/3.0_kr
    real(kr), parameter :: gamma_r       = 4.0_kr/3.0_kr
    real(kr), parameter :: gamma_ur      = 1.0_kr
    real(kr), parameter :: Knr_geomtocgs = p_geomtocgs/(eps_geomtocgs**gamma_nr)
    real(kr), parameter :: Kr_geomtocgs  = p_geomtocgs/(eps_geomtocgs**gamma_r)
    

!Berechnung der Parameter in geometrized units
!===============================================

    real(kr), parameter :: K_gnr    = h_bar**2.0_kr/(15.0_kr*pi**2.0_kr*m_e)*(((3.0_kr*pi**2.0_kr)/&
                                      &(m_n*c**2.0_kr*2.0_kr))**(gamma_nr))/Knr_geomtocgs
    real(kr), parameter :: K_fn     = h_bar**2.0_kr/(15.0_kr*pi**2.0_kr*m_n)*(((3.0_kr*pi**2.0_kr)/&
                                      &(2.0_kr*m_n*c**2.0_kr))**(gamma_nr))/Knr_geomtocgs
    real(kr), parameter :: K_gr     = h_bar*c/(12.0_kr*pi**2.0_kr)*((3.0_kr*pi**2.0_kr)/&
                                      &(2.0_kr*m_n*c**2))**(gamma_r)/(Kr_geomtocgs)
    real(kr), parameter :: K_ur     = 1.0_kr
    real(kr), parameter :: m_gn     = m_n/m_geomtocgs
    real(kr), parameter :: m_ge     = m_e/m_geomtocgs
    real(kr), parameter :: G_g      = 1.0_kr
    real(kr), parameter :: c_g      = 1.0_kr 





!STARTWERTE DES DRUCKS UND DER MASSE IN CGS-EINHEITEN

!============================FUER DIE MAIN============================================ 
     real(kr), parameter               :: vergleich  = 5.87_kr*10.0_kr**37.0_kr!5.87_kr*10.0_kr**37.0_kr 
     real(kr), parameter               :: p_start    = vergleich/p_geomtocgs               !SKYRME
     !real(kr), parameter               :: p_start   = 9.0_kr*10.0_kr**25.0_kr/p_geomtocgs !REL
     !real(kr), parameter               :: p_start   = 2.5_kr*10.0_kr**22.0_kr/p_geomtocgs !NONREL
     real(kr), parameter               :: m0        = 0.0_kr
     real(kr), parameter, dimension(2) :: y_start   = (/p_start,m0/)
     real(kr), parameter               :: r_min     = 0.0_kr
     real(kr), parameter               :: stepsize  = 20.0_kr
!=============================WRITE WERTE============================================
     !real(kr), parameter               :: p_max     = 2.6_kr*10.0_kr**22.0_kr/p_geomtocgs  !NONREL
     real(kr), parameter               :: p_max   = 1.0_kr*10.0_kr**26.0_kr/p_geomtocgs    !REL
     real(kr), parameter               :: p_min     = 1.0_kr*10.0_kr**0.0_kr/p_geomtocgs
     !=============================LOOP====================================================
     !real(kr), parameter               :: p_minimal = 5.62_kr*10.0_kr**24.0_kr/p_geomtocgs
     !real(kr), parameter               :: p_maximal = 5.62_kr*10.0_kr**35.0_kr/p_geomtocgs
     !real(kr), parameter               :: h         = ((p_start-p_minimal)/1000.0_kr)
!==============================================================

end module paramets

