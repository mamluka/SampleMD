module lib_DataAnalysisManager
    use class_KineticEnergyAnalyzer,CreateKineticEnergyAnalyzer=> CreateAnalyzer
    use lib_ConfigurationManager
    use class_DataAnalyzersContainer
    use class_Particle
    implicit none

contains

    function DataAnalyzersFromConfiguration(configurations,particles) result(dataAnalyzers)
        type(DataAnalyzersContainer) :: dataAnalyzers
        type(SimulationConfigurations) :: configurations
        type(Particle),allocatable :: particles(:)

        class(DataAnalyzerBase),pointer :: currentAnalyzer


        if (configurations%DataAnalyzersList%KineticEnergy == .true. ) then
            currentAnalyzer=>CreateKineticEnergyAnalyzer(particles)
            call dataAnalyzers%AddAnalyzer(currentAnalyzer)
        end if

    end function DataAnalyzersFromConfiguration

end module lib_DataAnalysisManager
