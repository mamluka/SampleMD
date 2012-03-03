module class_DataAnalyzerBase
    use class_ParticlePointer
    use lib_ConfigurationManager
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
            type(SimulationConfigurations) :: configurations
        end subroutine
    end interface

    contains

    subroutine LoadParticles(this,particles)
        class(DataAnalyzerBase) :: this
        type(Particle),allocatable,target:: particles(:)

        integer :: arraySize,i

        arraySize = size(particles)

        allocate(this%ParticlePointers(arraySize))

        do i=1,arraySize
            this%ParticlePointers(i)%p=>particles(i)
        end do



    end subroutine LoadParticles



end module class_DataAnalyzerBase
