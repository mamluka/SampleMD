module class_ParticlePredicateForCellMigration
    use class_Particle
    use class_ParticlePredicateBase
    use class_Grid
    use class_Cell
    use lib_Parse
    implicit none

    public :: ParticlePredicateForCellMigration

    type, extends(ParticlePredicateBase) :: ParticlePredicateForCellMigration
        integer :: CurrentCellCoordinates(3)
        type(Grid) :: currentGrid
    contains
        procedure :: IsTrueForThisParticle
        procedure :: PredicateSetup
    end type

contains

    subroutine PredicateSetup(this,g,currentCellCoord)
        class(ParticlePredicateForCellMigration) :: this
        type(Grid) :: g
        integer :: currentCellCoord(3)

        this%CurrentCellCoordinates = currentCellCoord
        this%currentGrid = g

    end subroutine PredicateSetup

    function IsTrueForThisParticle(this,p) result (boolResult)
        class(ParticlePredicateForCellMigration) :: this
        type(Particle) :: p
        logical :: boolResult

        integer :: currentParticleCoordinates(3)

        currentParticleCoordinates = this%currentGrid%ParticleCellCoordinatesByPosition(p%Position)
        if ( (this%CurrentCellCoordinates(1) /=  currentParticleCoordinates(1)) .or. (this%CurrentCellCoordinates(2) /=  currentParticleCoordinates(2)) .or. (this%CurrentCellCoordinates(3) /=  currentParticleCoordinates(3))) then
            boolResult = .true.
        else
            boolResult = .false.
        end if


    end function IsTrueForThisParticle
end module class_ParticlePredicateForCellMigration
