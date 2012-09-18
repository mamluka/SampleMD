module lib_DataAnalysisManager
    use class_KineticEnergyAnalyzer,CreateKineticEnergyAnalyzer=> CreateAnalyzer
    use class_TemperatureAnalyzer,CreateTemperatureAnalyzer => CreateAnalyzer
    use class_AverageVelocityAnalyzer,CreateAverageVelocityAnalyzer => CreateAnalyzer
    use class_PressureAnalyzer,CreatePressureAnalyzer => CreateAnalyzer
    use lib_ConfigurationManager
    use class_DataAnalyzersContainer
    use class_Particle
    implicit none

contains

    function DataAnalyzersFromConfiguration(configurations,particlePointers) result(dataAnalyzers)
        type(DataAnalyzersContainer) :: dataAnalyzers
        type(ConfigurationsDTO) :: configurations
        type(ParticlePointer),allocatable :: particlePointers(:)

        class(DataAnalyzerBase),pointer :: currentAnalyzer

        if (configurations%DataAnalyzersList%KineticEnergy == .true. ) then
            currentAnalyzer=>CreateKineticEnergyAnalyzer(particlePointers)
            call dataAnalyzers%AddAnalyzer(currentAnalyzer)
        end if

        if (configurations%DataAnalyzersList%Temperature == .true. ) then
            currentAnalyzer=>CreateTemperatureAnalyzer(particlePointers)
            call dataAnalyzers%AddAnalyzer(currentAnalyzer)
        end if

        if (configurations%DataAnalyzersList%AverageVelocity == .true. ) then
            currentAnalyzer=>CreateAverageVelocityAnalyzer(particlePointers)
            call dataAnalyzers%AddAnalyzer(currentAnalyzer)
        end if

        if (configurations%DataAnalyzersList%Pressure == .true. ) then
            currentAnalyzer=>CreatePressureAnalyzer(particlePointers)
            call dataAnalyzers%AddAnalyzer(currentAnalyzer)
        end if

    end function DataAnalyzersFromConfiguration

end module lib_DataAnalysisManager
