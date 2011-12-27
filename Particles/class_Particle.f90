module class_Particle
    implicit none


    public :: Particle

    type Particle
        real ,allocatable  :: Velocity(:)
        real ,allocatable :: Position(:)
        real  :: Mass
        real ,allocatable :: Force(:)
    end type

    contains
    function Make_3DParticle() result (p)
        type(Particle) :: p

        allocate(p%Velocity(3))
        allocate(p%Force(3))
        allocate(p%Position(3))

    end function

end module class_Particle
