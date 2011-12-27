module class_LinkedCell
    implicit none

    public :: LinkedCell

    type LinkedCell
        real :: a
        real, pointer :: NextCell
    end type

end module class_LinkedCell
