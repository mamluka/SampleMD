module class_PressureAnalyzer
    use class_DataAnalyzerBase
    use class_ParticlePointer
    use class_Particle
    use lib_ConfigurationManager
    use class_Logger
    implicit none

    type,extends(DataAnalyzerBase) :: PressureAnalyzer
    contains
        procedure :: Analyze
    end type

contains

    subroutine Analyze(this,configurations)
        class(PressureAnalyzer) :: this
        type(ConfigurationsDTO) :: configurations

        type(FileLogger) :: logger


        real :: Pint
        integer :: i
        type(Particle),pointer :: currentParticle

        Pint=0

        do i=1,size(this%ParticlePointers)
            currentParticle => this%ParticlePointers(i)%p
            Pint = Pint + currentParticle%Mass*sum(currentParticle%Velocity**2) + dot_product(currentParticle%Force,currentParticle%Position)
        end do

        call logger%LogTextWithReal("pressure.log","The current internal pressure is ",Pint)

    end subroutine

    function CreateAnalyzer(particlePointers) result (analyzer)
        type(ParticlePointer),allocatable :: particlePointers(:)
        type(PressureAnalyzer),target :: target
        class(DataAnalyzerBase),pointer :: analyzer

        allocate(analyzer,source=target)
        call analyzer%LoadParticles(particlePointers)

    end function

end module class_PressureAnalyzer
