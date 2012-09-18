module class_TemperatureAnalyzer
    use class_DataAnalyzerBase
    use class_ParticlePointer
    use lib_ConfigurationManager
    use lib_DataAnalysis
    use class_Logger
    implicit none

    type,extends(DataAnalyzerBase) :: TemperatureAnalyzer
    contains
        procedure :: Analyze
    end type

contains

    subroutine Analyze(this,configurations)
        class(TemperatureAnalyzer) :: this
        type(ConfigurationsDTO) :: configurations

        real :: T
        type(FileLogger) :: logger

        T=CalculateTemperature(this%ParticlePointers,configurations%Reducers%Energy)

       call logger%LogTextWithReal("temprature.log","The current temperature is ",T)

    end subroutine

    function CreateAnalyzer(particlePointers) result (analyzer)
        type(ParticlePointer),allocatable :: particlePointers(:)
        type(TemperatureAnalyzer),target :: target
        class(DataAnalyzerBase),pointer :: analyzer

        allocate(analyzer,source=target)
        call analyzer%LoadParticles(particlePointers)

    end function

end module class_TemperatureAnalyzer
