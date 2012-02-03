module class_Cell
    use class_ParticleLink
    use class_Particle
    private
    public :: Cell,CellContainer,EmptyCell
    type :: Cell
        private
        class(ParticleLink),pointer :: firstLink => null() ! first link in list
        class(ParticleLink),pointer :: lastLink => null()  ! last link in list
        class(ParticleLink),pointer :: currLink => null()  ! list iterator
    contains
        procedure, non_overridable :: AddParticle     ! add class(Particle) to list
        procedure, non_overridable :: FirstValue   ! value of first link in list
        procedure, non_overridable :: Reset        ! reset list iterator
        procedure, non_overridable :: Next         ! increment list iterator
        procedure, non_overridable :: CurrentValue ! get value from currLink
        procedure, non_overridable :: MoreValues   ! more values for iterator?
        procedure, non_overridable :: AreThereMoreParticles   ! more values for iterator?
    end type Cell

    type CellContainer
        type(Cell),pointer :: C => null()
    end type




contains

    subroutine AddParticle(this, value)
        class(Cell) :: this
        class(Particle) ,pointer :: value
        class(ParticleLink), pointer :: newLink

        class(ParticleLink) ,pointer :: firstLink,nextLink

        if (.not. associated(this%firstLink)) then
            firstLink => this%firstLink
            this%firstLink => CreateParticleLink(value)
            this%lastLink => this%firstLink
            this%currLink => this%firstLink
        else

            newLink => CreateParticleLink(value)
            call this%lastLink%setNextParticleLink(newLink)
            this%lastLink => newLink

        end if

    end subroutine AddParticle

    function FirstValue(this)
        class(Cell) :: this
        class(Particle), pointer :: firstValue

        firstValue => this%firstLink%getValue()

    end function FirstValue

    function CurrentValue(this)
        class(Cell) :: this
        class(Particle), pointer :: CurrentValue
        CurrentValue => this%currLink%getValue()
    end function CurrentValue

    subroutine Next(this)
        class(Cell) :: this
        this%currLink => this%currLink%nextParticleLink()


    end subroutine Next

    function MoreValues(this)
        class(Cell) :: this
        logical moreValues
        moreValues = associated(this%currLink)
    end function MoreValues

    function AreThereMoreParticles(this)
        class(Cell) :: this
        logical AreThereMoreParticles
        AreThereMoreParticles = associated(this%currLink%nextParticleLink())
    end function AreThereMoreParticles

    subroutine Reset(this)
        class(Cell) :: this
        this%currLink => this%firstLink
    end subroutine Reset

    function EmptyCell() result (c)
        type(Cell) :: c
    end function

end module class_Cell
