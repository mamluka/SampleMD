module class_LeonardJonesPotential
    use class_PotentialBase
    implicit none

    private

    public :: LeonardJonesPotential
    type,extends(PotentialBase) :: LeonardJonesPotential
        real :: CutOfDistance = 2.4
    contains
        procedure,nopass :: CalculateForce
        procedure :: SizeOfGridCell
    end type

contains

    subroutine CalculateForce(g)
        type(Grid) :: g
        print *,"IT works!"
    end subroutine CalculateForce

    function SizeOfGridCell(this) result(rc)
        class(LeonardJonesPotential) :: this
        real :: rc
        rc = this%CutOfDistance
    end function


end module class_LeonardJonesPotential
