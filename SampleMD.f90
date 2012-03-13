program SampleMD
    use class_RandomVelocityBootstapper
    use class_IntegrationRunnerSelector
    use lib_ConfigurationManager
    use lib_PotentialManager
    use class_DataFileReader
    use class_Grid
    use class_ParticlePredicateForID
    use lib_DataAnalysisManager
    use class_ParticlePointer
    use class_DataAnalyzerBase
    implicit none

    type(IntegrationRunnerSelector) :: runnerSelector
    class(IntegrationRunnerBase) ,pointer :: runner

    type(ConfigurationsDTO) :: configurations

    type(DataFileReader) :: dataReader
    type(ParticlePointer),allocatable:: particlePointers(:)
    type(Grid) :: g
    class(PotentialBase),pointer :: potential
    type(Particle),pointer :: p

    type(DataAnalyzersContainer) :: dataAnalyzers
    class(DataAnalyzerBase),pointer :: da

    configurations = LoadConfigurations()

    call dataReader%LoadParticlesUsingConfigurations(configurations,particlePointers)

    potential => LoadPotentialByConfiguration(configurations)

    dataAnalyzers = DataAnalyzersFromConfiguration(configurations,particlePointers)

    da => dataAnalyzers%CurrentAnalyzer()

    call g%CreateGrid(particlePointers,potential%SizeOfGridCell(),dataReader%SimulationBoxCoordinates)

    runner => runnerSelector%Select("standard")
    call runner%Setup(g,potential,dataAnalyzers,configurations,particlePointers)

    call runner%Start()

end program SampleMD


