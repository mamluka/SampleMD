module class_CellNeightbor
    use class_Cell
    implicit none

    public :: CellNeightbor

    type :: CellNeightbor
        type(Cell),pointer :: C
        logical :: Ghost
    end type
end module class_CellNeightbor
