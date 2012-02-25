module class_ParticlePredicateBase
    use class_Particle
    implicit none

    public :: ParticlePredicateBase

    type,abstract :: ParticlePredicateBase
        contains
        procedure(IParticlePredicateProc),deferred :: IsTrueForThisParticle
    end type


    abstract interface
   function IParticlePredicateProc(this,p) result(boolResult)
        import
        class(ParticlePredicateBase) :: this
        type(Particle) :: p
        logical :: boolResult
    end function IParticlePredicateProc
    end interface

end module class_ParticlePredicateBase
