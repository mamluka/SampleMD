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
        procedure :: SetReducers
    end type

contains

    subroutine Force(this,pi,pj,r,direction)
        class(LeonardJonesPotential) :: this
        type(Particle):: pi,pj
        real :: r,direction(3)
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

    subroutine LoadPotentialParameters(this)
        class(LeonardJonesPotential) :: this
    end subroutine LoadPotentialParameters

    subroutine SetReducers(this,reducers)
        class(LeonardJonesPotential) :: this
        type(ReducersDTO),target :: reducers

        this%Reducers = reducers


    end subroutine SetReducers


end module class_LeonardJonesPotential
