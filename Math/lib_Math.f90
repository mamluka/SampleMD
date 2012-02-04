module lib_Math
    implicit none
    contains

    function Distance(i,j) result (d)
        real :: i(3),j(3)
        real :: d

        d=sqrt(sum((i-j)**2))

    end function

end module lib_Math
