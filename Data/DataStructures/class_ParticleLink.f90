!! ParticleLink.f90

module class_ParticleLink
    use class_Particle

    private
    public :: ParticleLink,CreateParticleLink
    type ParticleLink
        private
        type(Particle), pointer :: value => null() ! value stored in ParticleLink
        type(ParticleLink), pointer :: next => null()! next ParticleLink in list
    contains
        procedure :: getValue    ! return value pointer
        procedure :: nextParticleLink    ! return next pointer
        procedure :: setNextParticleLink ! set next pointer
        procedure :: nullifyNext
    end type ParticleLink

    interface CreateParticleLink
        module procedure CreateParticleLink ! construct/initialize a ParticleLink
    end interface

contains

    function nextParticleLink(this)
        class(ParticleLink) :: this
        type(ParticleLink), pointer :: nextParticleLink
        if (.not. associated(this%next) ) then
            nextParticleLink=>null()
        else
            nextParticleLink => this%next
        endif
    end function nextParticleLink

    subroutine setNextParticleLink(this,next)
        class(ParticleLink) :: this
        type(ParticleLink), pointer :: next
        this%next => next
    end subroutine setNextParticleLink

    function getValue(this)
        class(ParticleLink) :: this
        type(Particle), pointer :: getValue
        getValue => this%value
    end function getValue



    function CreateParticleLink(value)
        class(ParticleLink),pointer :: CreateParticleLink
        type(Particle),pointer :: value
        type(ParticleLink),target :: newLink

        allocate(CreateParticleLink,source=newLink)
        createParticleLink%value => value



    end function CreateParticleLink

    subroutine nullifyNext(this)
        class(ParticleLink) :: this

        this%next => null()

    end subroutine nullifyNext

end module class_ParticleLink
