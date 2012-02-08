module class_ReducersDTO
    implicit none

    public :: ReducersDTO
    type :: ReducersDTO
        logical :: HasDimensionlessReduction = .false.
        real :: Time
        real :: Energy
        real :: Length
        real :: Mass
    end type
contains


end module class_ReducersDTO
