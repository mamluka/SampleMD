module lib_Math
    implicit none
contains

    function Distance(i,j) result (d)
        real :: i(3),j(3)
        real :: d

        d=sqrt(sum((i-j)**2))

    end function

    subroutine InitializeRandomNumber()
        integer :: i, n, clock
        integer, DIMENSION(:), ALLOCATABLE :: seed

        call RANDOM_SEED(size = n)
        allocate(seed(n))

        call SYSTEM_CLOCK(COUNT=clock)

        seed = clock + 37 * (/ (i - 1, i = 1, n) /)
        call RANDOM_SEED(PUT = seed)

        deallocate(seed)
    end subroutine

    function GaussDeviate() result(b)

        real :: a(2),r,s,b(2)

        r=2

        do while (r .ge. 1)
            a=-1.0+(1.0+1.0)*Random2DVector()
            r=sum(a**2)
        end do

        s=sqrt(-2.0 *log(r)/r)
        b=a*s

    end function GaussDeviate

    function Random2DVector() result (r)

        REAL :: r(2)
        CALL InitializeRandomNumber()         ! see example of RANDOM_SEED
        CALL RANDOM_NUMBER(r)

    end function

end module lib_Math
