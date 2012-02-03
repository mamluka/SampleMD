module class_AtomProperties
    implicit none

    public :: AtomProperties

    type :: AtomProperties
    contains
    procedure ,nopass:: AtomMassInAMU
    end type

    contains

    function AtomMassInAMU(atomType) result(mass)
    real :: mass
    character (len=*) :: atomType

    select case (atomType)
    case ("C")
        mass = 12.0107
    case default
        mass = 12.0107
    end select

    end function

end module class_AtomProperties
