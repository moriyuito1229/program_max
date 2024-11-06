    program max
    implicit none

    integer ::          &
    i,    &
    j,    &
    floc, &
    itime

    integer :: number(0:2000)

    integer, parameter ::          &
    imax = 5000, &
    ID1 = 60100, &
    ID2 = 1000300100, &
    timewri = 10, &
    time = 2000 

    double precision, parameter ::  &
    d2   = 0.60d0,                  & ! d2 / D
    den1 = 0.10d0,                  & ! rho1 / rho2
    a1 = 1.5d0 * (den1 * (d2**2) - (1 - d2)**2) / ((1 - d2) * d2 * (1 + (den1 - 1) * d2)), &
    a3 = 3d0 * (den1 * (d2**3) + (1 - d2)**3) / (((1 - d2)**2) * (d2**2) * (1 + (den1 - 1) * d2)) - (7d0 / 6d0) * (a1**2)

    double precision :: &
    f(0:2000),&
    x(0:2000)

    character(5) :: &
    ID1_str

    character(10) :: &
    ID2_str

    character(4) :: &
    time_str,  &
    itime_str    
    !Euler_602001000300150_4000.dat
    write(time_str, '(I4.4)') time
    write(ID1_str, '(I5)') ID1
    write(ID2_str, '(I10)') ID2

    !===== fmax============================================================
    open (2, file='Euler_'//ID1_str//ID2_str//'_'//time_str//'_max.dat')
    write (2, *) "a1 a3 a3/a1"
    write (2, *) a1, a3, a3/a1
    write (2, *) "x fmax (a3/a1)fmax xi Ut/D"

    !===== read exponents from file =======================================
    do j = 10, time, timewri
        itime = itime + timewri
        write(itime_str, '(I4.4)') itime
        open(itime,file='Euler_'//ID1_str//ID2_str//'_'//itime_str//'.dat',status='old')
        read(itime,'()')
        do i = 0,2000
            read(itime,*) x(i), f(i), number(i)
        end do
        floc = maxloc(abs(f(0:2000)), 1)

        write (2, '(E22.15, 1X, E22.15, 1X, E22.15, 1X, i4.4, 1X,  i4.4, 1X)') &
            x(floc), abs(f(floc)), (a3/a1)*abs(f(floc)), number(floc), itime
        close(itime)
    end do
    close(2)
    end program max
