module class_CellContainer
    use class_Cell
    use class_Particle
    implicit none

    public :: CellContainer

    type CellContainer
        type(Cell),pointer :: C => null()
        logical :: Ghost = .false.
    contains
        procedure :: New
        procedure :: Exists
        procedure :: CurrentCell
    end type

contains

    function New(this,x,y,z) result(cellPointer)
        type(Cell),target :: c
        type(Cell),pointer :: cellPointer
        class(CellContainer) :: this
        integer :: x,y,z

        c=EmptyCell()
        call c%SetCellCoordinates(x,y,z)

        allocate(this%C,source=c)
        cellPointer => this%C


    end function

    function CurrentCell(this) result(cellPointer)
        type(Cell),pointer :: cellPointer
        class(CellContainer) :: this

        cellPointer => this%C
    end function

    function Exists(this) result(cellExists)
        class(CellContainer) :: this
        logical :: cellExists

        if (associated(this%C)) then
            cellExists = .true.
        else
            cellExists = .false.
        endif



    end function

end module class_CellContainer
