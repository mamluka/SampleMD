module lib_Parse
    implicit none

    contains

    function ParseCoordinates(coord) result(coordString)
        integer :: coord(3)
        character(len=16) :: coordString

        write(coordString,"(A,I3.3,A,I3.3,A,I3.3,A)"),"(",coord(1),",",coord(2),",",coord(3),")"

    end function

end module lib_Parse
