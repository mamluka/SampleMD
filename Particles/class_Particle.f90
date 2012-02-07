module class_Particle
    implicit none
    private

    public :: Particle,AllocateParticleWithPosition,AllocateParticle

    type Particle
        real ,allocatable  :: Velocity(:)
        real ,allocatable :: Position(:)
        real  :: Mass
        real ,allocatable :: Force(:)
        real ,allocatable :: ForceFromPreviousIteration(:)
        integer :: ID
        contains
        procedure :: SetPosition
        procedure :: GiveID
    end type



    contains

    function AllocateParticle() result (p)
        type(Particle) :: p

        allocate(p%Velocity(3))
        allocate(p%Force(3))
        allocate(p%Position(3))
        allocate(p%ForceFromPreviousIteration(3))

    end function

    function AllocateParticleWithPosition(x,y,z) result (p)
        type(Particle) :: p
        real :: x,y,z
        p = AllocateParticle()
        call p%SetPosition(x,y,z)
    end function

    subroutine GiveID(this,id)
        class(Particle) :: this
        integer :: id
        this%ID = id
    end subroutine GiveID

    subroutine SetPosition(this,x,y,z)
        class(Particle) :: this
        real :: x,y,z
        if (.not. allocated(this%Position)) then
            allocate(this%Position(3))
        end if

        this%Position(1) = x
        this%Position(2) = y
        this%Position(3) = z

    end subroutine

end module class_Particle
