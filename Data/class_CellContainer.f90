module class_CellContainer
    use class_Cell
    use class_Particle
    implicit none

    public :: CellContainer

    type CellContainer
        type(Cell),pointer :: C => null()
        logical :: Ghost = .false.
    contains
        procedure :: BoxedCell
    end type

contains

    function BoxedCell(this) result(cellPointer)
        type(Cell),target :: c
        type(Cell),pointer :: cellPointer
        class(CellContainer) :: this



        if (associated(this%C)) then
            cellPointer => this%C
        else
            c=EmptyCell()
            allocate(this%C,source=c)
            cellPointer => this%C
        endif

    end function

end module class_CellContainer
