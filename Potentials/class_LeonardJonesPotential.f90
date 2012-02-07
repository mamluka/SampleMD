module class_LeonardJonesPotential
    use class_PotentialBase
    implicit none

    private

    public :: LeonardJonesPotential
    type,extends(PotentialBase) :: LeonardJonesPotential
        private
        real :: CutOfDistance = 4.2
    contains
        procedure :: Force
        procedure :: SizeOfGridCell
        procedure :: CutOffRadii
        procedure :: LoadPotentialParameters
    end type

contains

    subroutine Force(this,pi,pj,r)
        class(LeonardJonesPotential) :: this
        type(Particle):: pi,pj
        real :: r
    end subroutine Force

    function SizeOfGridCell(this) result(rc)
        class(LeonardJonesPotential) :: this
        real :: rc
        rc = this%CutOfDistance
    end function

    function CutOffRadii(this) result(rc)
        class(LeonardJonesPotential) :: this
        real :: rc
        rc = this%CutOfDistance
    end function

    subroutine LoadPotentialParameters(this,filename)
        class(LeonardJonesPotential) :: this
        character(len=*) :: filename
    end subroutine LoadPotentialParameters


end module class_LeonardJonesPotential
