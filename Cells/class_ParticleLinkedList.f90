module class_ParticleLinkedList
    use class_ParticleLink
    use class_Particle
    private
    public :: ParticleLinkedList
    type :: ParticleLinkedList
        private
        class(ParticleLink),pointer :: firstLink => null() ! first link in list
        class(ParticleLink),pointer :: lastLink => null()  ! last link in list
        class(ParticleLink),pointer :: currLink => null()  ! list iterator
    contains
        procedure, non_overridable :: AddValue     ! add class(Particle) to list
        procedure, non_overridable :: FirstValue   ! value of first link in list
        procedure, non_overridable :: Reset        ! reset list iterator
        procedure, non_overridable :: Next         ! increment list iterator
        procedure, non_overridable :: CurrentValue ! get value from currLink
        procedure, non_overridable :: MoreValues   ! more values for iterator?
    end type ParticleLinkedList

    interface Add
        module procedure addValue
    end interface

contains

    subroutine AddValue(this, value)
        class(ParticleLinkedList) :: this
        class(Particle) ,pointer :: value
        class(ParticleLink), pointer :: newLink

        class(ParticleLink) ,pointer :: firstLink,nextLink

        if (.not. associated(this%firstLink)) then
            firstLink => this%firstLink
            this%firstLink => CreateParticleLink(value, firstLink)
            this%lastLink => this%firstLink
            this%currLink => this%firstLink
        else
            nextLink => this%lastLink%nextParticleLink()
            newLink => CreateParticleLink(value, nextLink)
            call this%lastLink%setNextParticleLink(newLink)
            this%lastLink => newLink

        end if

    end subroutine AddValue

    function FirstValue(this)
        class(ParticleLinkedList) :: this
        class(Particle), pointer :: firstValue

        firstValue => this%firstLink%getValue()

    end function FirstValue

    function CurrentValue(this)
        class(ParticleLinkedList) :: this
        class(Particle), pointer :: CurrentValue
        CurrentValue => this%currLink%getValue()
    end function CurrentValue

    subroutine Next(this)
        class(ParticleLinkedList) :: this
        this%currLink => this%currLink%nextParticleLink()
    end subroutine Next

    function MoreValues(this)
        class(ParticleLinkedList) :: this
        logical moreValues
        moreValues = associated(this%currLink)
    end function MoreValues

    subroutine Reset(this)
        class(ParticleLinkedList) :: this
        this%currLink => this%firstLink
    end subroutine Reset

end module class_ParticleLinkedList
