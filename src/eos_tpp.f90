module eos
    use paramets
    implicit none
    contains

    subroutine read_table(pressure_array,eos_array,n_arr)
        integer, parameter                   :: kr = selected_real_kind(16)
        real(kr), allocatable,intent(inout)  :: pressure_array(:),eos_array(:)
        integer, intent(inout)               :: n_arr
        character(len=25)                    :: laenge,a,b,c,d,e,f,g,h
        integer                              :: EOF, i
        real(kr)                             :: aa,bb,cc,cp,ee,ff, gg,pol,ii,jj,kk,ll,mm,nn
        open(unit=105,file='check_p_eps.csv', status='unknown', action='write')
        open(unit=101,file='table.csv', status='old', action='read')
	!NEU open(unit=101,file='t0.star', status='old', action='read',IOSTAT = EOF,FORM='formatted')!,access='sequential')
        !=== ERSTE SCHLEIFE NUR ZUR ERMITTLUNG DER LAENGE DER LISTE ===
        do while(EOF == 0)
            read(101,*,iostat=EOF)
            if (EOF < 0) then
                rewind(101)
                exit
                close(101)
            else
                n_arr = n_arr + 1
            end if
        end do
        ! ===  ARRAY WIRD ERSTELLT ===
        allocate(pressure_array(n_arr))
        allocate(eos_array(n_arr))
        rewind(101)
        !NEU n_arr = n_arr -1 !WEIL LETZTER (erster im pressure.csv)  EINTRAG NULL
        !NEU do i=1,n_arr
	    do i= n_arr, 1, -1
             !NEU read(101,*,iostat=EOF) pressure_array(i),a,eos_array(i)
             read(101,*,iostat=EOF) eos_array(i),pressure_array(i)

             write(*,*) pressure_array(i), eos_array(i)
             !read(101,*,iostat=EOF) eos_array(i), pressure_array(i)
        end do
       return
    end subroutine read_table




    function linear_search(y_1,pressure_array,n_arr,y_start_1,anfang_lin)
            integer, parameter                       :: kr = selected_real_kind(16)
            real(kr), intent(in)                     :: y_1,y_start_1
            integer, intent(in)                      :: n_arr,anfang_lin
            real(kr), dimension(n_arr), intent(in)   :: pressure_array
            integer                                  :: linear_search
            integer                                  :: k
            !=== STERN WIRD VON INNEN NACH AUSSEN DURCHLAUFFEN ===
            k = anfang_lin
            !if (y_1 >  pressure_array(1) .or. y_1 <  pressure_array(n_arr)) then
            !    write(*,*) 'y_1_start nicht in der tabelle'
            !    stop
            !end if
            !=== STARTPUNKT IST DER LETZTE WERT DER SUCHE ===
            do while (y_1 >= pressure_array(k) .and. k > 2)
                k = k - 1
                linear_search = k
                !return
            end do
            do while (y_1 < pressure_array(k) .and. k < n_arr -1)
                k = k + 1
                linear_search = k - 1
                !return
            end do
            !=== UEBERPRUEFT SONDERFAELLE ===
            !if (y_1 < pressure_array(n_arr)) then
            !    write(*,*) '===fertig==='
            !    return
            !    stop
            !end if
            if (k <= 2) then
                linear_search = 1 !y_1_start nahe des sterninneren
            end if
            if (y_1 .ne. y_1) then
                write(*,*) n_arr,'sth wrong with interpolation or rk'
                stop
            end if
            end function linear_search





    function binaery_search(y_1,pressure_array,n_arr,ende,anfang,erster_wert)
            integer, parameter                       :: kr = &
                                                        &selected_real_kind(16)
            real(kr), intent(in)                     :: y_1,erster_wert
            integer, intent(inout)                   :: ende,anfang
            integer, intent(in)                      :: n_arr
            real(kr), dimension(n_arr), intent(in)   :: pressure_array
            integer                                  :: mitte,binaery_search
            !if (erster_wert >  pressure_array(2) .or. erster_wert <
            ! pressure_array(n_arr)) then
            !    write(*,*) 'y_1_start nicht in der tabelle'
            !    stop
            !end if
            do while (anfang <= ende)
                mitte = anfang + int((ende - anfang)/2)
                if (y_1 < pressure_array(mitte) .and. y_1 >= &
                    & pressure_array(mitte+1)) then
                    binaery_search = mitte + 1
                    if (binaery_search .eq. n_arr) then
                        return
                    end if
                    return
                else if (pressure_array(mitte) > y_1) then
                    anfang = mitte + 1
                else if (pressure_array(mitte) <= y_1) then
                    ende = mitte
                end if
            end do
            write(*,*) 'not found :('
            stop
    end function binaery_search



    !=== FITTET EOS VON UNTEN AN ===
    function interpolation(n_neu,y_1,n_arr,eos_array,pressure_array)
        integer, parameter                      :: kr = selected_real_kind(16)
        integer, intent(in)                     :: n_arr
        integer, intent(in)                     :: n_neu
        real(kr), intent(in)                    :: y_1
        real(kr), dimension(n_arr)              :: pressure_array
        real(kr), dimension(n_arr)              :: eos_array
        real(kr)                                :: interpolation
        interpolation = eos_array(n_neu) + (y_1-pressure_array(n_neu))*&
                        & (eos_array(n_neu+1)-eos_array(n_neu)) &
                        & /(pressure_array(n_neu+1)-pressure_array(n_neu))
        return
    end function interpolation


    function spline_interpolation(i,y_1,n_arr,eos_array,pressure_array)
        integer, parameter                      :: kr = selected_real_kind(16)
        integer, intent(in)                     :: i
        integer, intent(in)                     :: n_arr
        real(kr), intent(in)                    :: y_1
        real(kr), dimension(n_arr)              :: pressure_array
        real(kr), dimension(n_arr)              :: eos_array
        real(kr)                                :: spline_interpolation
        real(kr), dimension(n_arr)              :: der
        real(kr)                                :: a,b
        real(kr)                                :: abstand
        der(i) = (eos_array(i+1)-eos_array(i)) / (pressure_array(i+1)-pressure_array(i))
        abstand = (y_1-pressure_array(i))/(pressure_array(i+1)-pressure_array(i))
        a = der(i)*(pressure_array(i+1)-pressure_array(i)-(eos_array(i+1)-eos_array(i)))
        b = - der(i+1)*(pressure_array(i+1)-pressure_array(i)) + &
            & (eos_array(i+1)-eos_array(i))
        spline_interpolation = (1-abstand)*eos_array(i)+abstand*eos_array(i+1)+abstand* &
                              & (1-abstand)*(a*(1-abstand)+b*abstand)
        return
    end function spline_interpolation



    subroutine fit(m,t,pressure_array,eos_array,n_arr)
        integer, intent(in)                   :: n_arr
        real(kr), dimension(n_arr), intent(in) :: pressure_array
        real(kr), dimension(n_arr), intent(in) :: eos_array
        real(kr)                               :: p0,y0
        real(kr), dimension(2),intent(inout)                 :: t
        real(kr),intent(inout)                               :: m
        !==== annahme dass eos nullpunkt schneidet
        p0 = 0.0_kr
        y0 = 0.0_kr
        t  = (/p0,y0/)
        m  = (eos_array(n_arr) - p0)/(pressure_array(n_arr)-y0)
        write(*,*) m,t,eos_array(n_arr),pressure_array(n_arr),'==='
        !fit = a*p + b(2)
    end subroutine













end module eos
