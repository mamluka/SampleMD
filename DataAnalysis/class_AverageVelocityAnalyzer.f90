module class_AverageVelocityAnalyzer
    use class_DataAnalyzerBase
    use class_ParticlePointer
    use lib_ConfigurationManager
    use lib_DataAnalysis
    use class_Logger
    implicit none

    type,extends(DataAnalyzerBase) :: AverageVelocityAnalyzer
    contains
        procedure :: Analyze
    end type

contains

    subroutine Analyze(this,configurations)
        class(AverageVelocityAnalyzer) :: this
        type(ConfigurationsDTO) :: configurations

        real :: totalVelocity
        real :: averageVelocity
        integer :: particleCount,i

        type(FileLogger) :: logger


        particleCount = size(this%ParticlePointers)

        do i=1,particleCount
            totalVelocity=totalVelocity +  sqrt(sum(this%ParticlePointers(i)%p%Velocity**2))
        end do

        averageVelocity = totalVelocity/particleCount

        call logger%LogTextWithReal("velocity.log","Average speed is :",averageVelocity)

    end subroutine

    function CreateAnalyzer(particlePointers) result (analyzer)
        type(ParticlePointer),allocatable :: particlePointers(:)
        type(AverageVelocityAnalyzer),target :: target
        class(DataAnalyzerBase),pointer :: analyzer

        allocate(analyzer,source=target)
        call analyzer%LoadParticles(particlePointers)

    end function
end module class_AverageVelocityAnalyzer
