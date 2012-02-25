module class_Cell
    use class_ParticleLink
    use class_Particle
    use class_ParticlePredicateBase

    public :: Cell,EmptyCell
    type :: Cell
        private
        type(ParticleLink),pointer :: firstLink => null() ! first link in list
        type(ParticleLink),pointer :: lastLink => null()  ! last link in list
        type(ParticleLink),pointer :: currLink => null()  ! list iterator
        integer :: NumberOfParticles=0
        integer :: CellCoordinates(3)
    contains
        procedure, non_overridable :: AddParticle
        procedure, non_overridable :: SetCellCoordinates
        procedure, non_overridable :: GetCellCoordinates
        procedure, non_overridable :: FirstValue
        procedure, non_overridable :: Reset
        procedure, non_overridable :: Next
        procedure, non_overridable :: CurrentValue
        procedure, non_overridable :: RemoveParticle
        procedure, non_overridable :: RemoveWhenTrue
        procedure, non_overridable :: AreThereMoreParticles
        procedure, non_overridable :: ParticleCount
    end type Cell

contains

    subroutine AddParticle(this, value)
        class(Cell) :: this
        type(Particle) ,pointer :: value
        type(ParticleLink), pointer :: newLink

        type(ParticleLink) ,pointer :: firstLink,nextLink

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

        this%NumberOfParticles=this%NumberOfParticles+1

    end subroutine AddParticle

    function FirstValue(this)
        class(Cell) :: this
        type(Particle), pointer :: firstValue

        firstValue => this%firstLink%getValue()

    end function FirstValue

    function CurrentValue(this)
        class(Cell) :: this
        type(Particle), pointer :: CurrentValue
        CurrentValue => this%currLink%getValue()
    end function CurrentValue

    subroutine Next(this)
        class(Cell) :: this
        this%currLink => this%currLink%nextParticleLink()


    end subroutine Next

    function AreThereMoreParticles(this)
        class(Cell) :: this
        logical AreThereMoreParticles
        AreThereMoreParticles = associated(this%currLink)
    end function AreThereMoreParticles



    subroutine Reset(this)
        class(Cell) :: this
        this%currLink => this%firstLink
    end subroutine Reset

    function ParticleCount(this) result(total)
        class(Cell) :: this
        total = this%NumberOfParticles
    end function

    subroutine RemoveParticle(this,particleToBeDeleted)
        class(Cell) :: this
        type(Particle),pointer :: particleToBeDeleted,currentParticle
        integer :: particleID
        type(ParticleLink),pointer  :: previousLink,nextLink

        previousLink => null()

        call this%Reset()

        do while (this%AreThereMoreParticles())

            currentParticle => this%CurrentValue()
            particleID = currentParticle%ID

            if ( particleID == particleToBeDeleted%ID ) then

                if (.not. associated(previousLink)) then
                    this%firstLink => this%currLink%nextParticleLink()
                    this%lastLink => this%firstLink
                    this%currLink => this%firstLink
                else
                    nextLink => this%currLink%nextParticleLink()
                    call previousLink%setNextParticleLink(nextLink)
                end if
                this%NumberOfParticles=this%NumberOfParticles-1
                exit

            end if
            previousLink => this%currLink

            call this%Next()

        end do


    end subroutine RemoveParticle

    subroutine RemoveWhenTrue(this,predicateObject,removedParticlesCell)
        class(Cell) :: this
        class(ParticlePredicateBase) :: predicateObject
        type(Cell) :: removedParticlesCell

        type(Particle),pointer :: particleToBeDeleted,currentParticle
        integer :: particleID
        type(ParticleLink),pointer  :: previousLink,nextLink

        previousLink => null()

        call this%Reset()

        do while (this%AreThereMoreParticles())

            currentParticle => this%CurrentValue()

            if ( predicateObject%IsTrueForThisParticle(currentParticle) ) then

                !print *,currentParticle%ID

                if (.not. associated(previousLink)) then
                    this%firstLink => this%currLink%nextParticleLink()
                    this%lastLink => this%firstLink
                    this%currLink => this%firstLink
                else
                    nextLink => this%currLink%nextParticleLink()
                    call previousLink%setNextParticleLink(nextLink)
                end if
                this%NumberOfParticles=this%NumberOfParticles-1

                call removedParticlesCell%AddParticle(currentParticle)

            end if
            previousLink => this%currLink

            call this%Next()

        end do

    end subroutine RemovewhenTrue


    function EmptyCell() result (c)
        type(Cell) :: c
    end function

    subroutine SetCellCoordinates(this,x,y,z)
        class(Cell) :: this
        integer :: x,y,z

        this%CellCoordinates = (/x,y,z/)

    end subroutine SetCellCoordinates

    function GetCellCoordinates(this) result(coord)
        class(Cell) :: this
        integer :: coord(3)


        coord = this%CellCoordinates

    end function GetCellCoordinates

end module class_Cell
