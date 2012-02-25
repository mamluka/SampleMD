module class_ParticlePredicateForID
    use class_Particle
    use class_ParticlePredicateBase
    implicit none

    public :: ParticlePredicateForID

    type, extends(ParticlePredicateBase) :: ParticlePredicateForID
        integer :: ParticleID
        contains
        procedure :: IsTrueForThisParticle
    end type

    contains

    function IsTrueForThisParticle(this,p) result (boolResult)
        class(ParticlePredicateForID) :: this
        type(Particle) :: p
        logical :: boolResult

        if (p%ID == this%ParticleID) then
            boolResult = .true.
        else
            boolResult = .false.
        end if

    end function IsTrueForThisParticle


end module class_ParticlePredicateForID
