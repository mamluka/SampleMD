module class_DataAnalyzerBase
    use class_ParticlePointer
    use lib_ConfigurationManager
    use lib_Particles
    implicit none

    public :: DataAnalyzerBase

    type,abstract :: DataAnalyzerBase
        type(ParticlePointer),allocatable:: ParticlePointers(:)
    contains
        procedure(IAnalyze),deferred :: Analyze
        procedure :: LoadParticles
    end type

    abstract interface
        subroutine IAnalyze(this,configurations)
            import
            class(DataAnalyzerBase) :: this
            type(ConfigurationsDTO) :: configurations
        end subroutine
    end interface

    contains

    subroutine LoadParticles(this,particlePointers)
        class(DataAnalyzerBase) :: this
        type(ParticlePointer),allocatable,target:: particlePointers(:)

        call CopyParticlePointer(particlePointers,this%ParticlePointers)

    end subroutine LoadParticles



end module class_DataAnalyzerBase
