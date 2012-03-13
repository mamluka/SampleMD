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

    call g%CreateGrid(particlePointers,potential%SizeOfGridCell())

    runner => runnerSelector%Select("standard")
    call runner%Setup(g,potential,dataAnalyzers,configurations,particlePointers)

    call runner%Start()

    p=>da%ParticlePointers(7)%p

    particlePointers(7)%p%Mass=13
end program SampleMD


