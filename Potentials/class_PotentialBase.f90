module class_PotentialBase
    use class_Grid
    implicit none

    type,abstract :: PotentialBase
    contains
        procedure(ICalculateForce),deferred,nopass :: CalculateForce
        procedure(ISizeOfGridCell),deferred :: SizeOfGridCell
    end type

    abstract interface
        subroutine ICalculateForce(g)
            import
            type(Grid) :: g
        end subroutine

        function ISizeOfGridCell(this) result(gridSize)
            import
            class(PotentialBase) :: this
            real :: gridSize
        end function
    end interface

end module class_PotentialBase
