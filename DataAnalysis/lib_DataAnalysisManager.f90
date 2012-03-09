module lib_DataAnalysisManager
    use class_KineticEnergyAnalyzer,CreateKineticEnergyAnalyzer=> CreateAnalyzer
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

        particlePointers(7)%p%Mass=11

        if (configurations%DataAnalyzersList%KineticEnergy == .true. ) then
            currentAnalyzer=>CreateKineticEnergyAnalyzer(particlePointers)
            currentAnalyzer%ParticlePointers(7)%p%Mass=12
            call dataAnalyzers%AddAnalyzer(currentAnalyzer)
        end if

    end function DataAnalyzersFromConfiguration

end module lib_DataAnalysisManager
