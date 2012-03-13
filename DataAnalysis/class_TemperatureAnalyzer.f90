module class_TemperatureAnalyzer
    use class_DataAnalyzerBase
    use class_ParticlePointer
    use lib_ConfigurationManager
    use lib_DataAnalysis

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

        T=CalculateTemperature(this%ParticlePointers,configurations%Reducers%Energy)

    end subroutine

    function CreateAnalyzer(particlePointers) result (analyzer)
        type(ParticlePointer),allocatable :: particlePointers(:)
        type(TemperatureAnalyzer),target :: target
        class(DataAnalyzerBase),pointer :: analyzer

        allocate(analyzer,source=target)
        call analyzer%LoadParticles(particlePointers)

    end function

end module class_TemperatureAnalyzer
