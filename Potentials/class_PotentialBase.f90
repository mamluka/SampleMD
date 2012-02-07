module class_PotentialBase
    use class_Grid
    use class_Particle
    implicit none

    type,abstract :: PotentialBase
    contains
        procedure(IForce),deferred :: Force
        procedure(ISizeOfGridCell),deferred :: SizeOfGridCell
        procedure(ICutOffRadii),deferred :: CutOffRadii
        procedure(ILoadPotentialParameters),deferred :: LoadPotentialParameters
    end type

    abstract interface
        subroutine IForce(this,pi,pj,r)
            import
            class(PotentialBase) :: this
            type(Particle) :: pi,pj
            real :: r
        end subroutine

        function ISizeOfGridCell(this) result(gridSize)
            import
            class(PotentialBase) :: this
            real :: gridSize
        end function

        function ICutOffRadii(this) result (rc)
            import
            class(PotentialBase) :: this
            real :: rc
        end function ICutOffRadii

        subroutine ILoadPotentialParameters(this,filename)
            import
            class(PotentialBase) :: this
            character(len=*) :: filename
        end subroutine ILoadPotentialParameters
    end interface

end module class_PotentialBase
