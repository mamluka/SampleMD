!! ParticleLink.f90

module class_ParticleLink
    use class_Particle
    private
    public :: ParticleLink,CreateParticleLink
    type ParticleLink
        private
        class(Particle), pointer :: value => null() ! value stored in ParticleLink
        type(ParticleLink), pointer :: next => null()! next ParticleLink in list
    contains
        procedure :: getValue    ! return value pointer
        procedure :: nextParticleLink    ! return next pointer
        procedure :: setNextParticleLink ! set next pointer
    end type ParticleLink

    interface CreateParticleLink
        module procedure CreateParticleLink ! construct/initialize a ParticleLink
    end interface

contains

    function nextParticleLink(this)
        class(ParticleLink) :: this
        class(ParticleLink), pointer :: nextParticleLink
        if (.not.associated(this%next) ) then
            nextParticleLink=>null()
        else
            nextParticleLink => this%next
        endif
    end function nextParticleLink

    subroutine setNextParticleLink(this,next)
        class(ParticleLink) :: this
        class(ParticleLink), pointer :: next
        this%next => next
    end subroutine setNextParticleLink

    function getValue(this)
        class(ParticleLink) :: this
        class(Particle), pointer :: getValue
        getValue => this%value
    end function getValue



    function CreateParticleLink(value)
        class(ParticleLink),pointer :: createParticleLink
        class(Particle) :: value

        allocate(createParticleLink)
        allocate(createParticleLink%value, source=value)



    end function CreateParticleLink

end module class_ParticleLink
