module class_Particle
    implicit none
    private

    public :: Particle

    type Particle
        real ,allocatable  :: Velocity(:)
        real ,allocatable :: Position(:)
        real  :: Mass
        real ,allocatable :: Force(:)
        contains
        procedure :: SetPosition
    end type



    contains

    function AllocateParticle() result (p)
        type(Particle) :: p

        allocate(p%Velocity(3))
        allocate(p%Force(3))
        allocate(p%Position(3))

    end function

    function AllocateParticleWithPosition(x,y,z) result (p)
        type(Particle) ::  p
        p = AllocateParticle()
        call p%SetPosition(x,y,z)
    end function

    subroutine SetPosition(this,x,y,z)
        type(Particle) :: this
        if (.not. allocated(p%Position)
            allocate(p%Position(3))
        end if

        p%Position(1) = x
        p%Position(2) = y
        p%Position(3) = z

    end subroutine

end module class_Particle
