module class_ParticlePointer
    use class_Particle
    implicit none

    public :: ParticlePointer

    type :: ParticlePointer
        type(Particle),pointer :: p
    end type
end module class_ParticlePointer
