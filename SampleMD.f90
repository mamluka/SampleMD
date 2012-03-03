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
    implicit none

    type(IntegrationRunnerSelector) :: runnerSelector
    class(IntegrationRunnerBase) ,pointer :: runner

    type(SimulationConfigurations) :: configurations

    type(DataFileReader) :: dataReader
    type(ParticlePointer),allocatable:: particlePointers(:)
    type(Grid) :: g
    class(PotentialBase),pointer :: potential

    type(DataAnalyzersContainer) :: dataAnalyzers

    configurations = LoadSimulationConfigurations("/home/mamluka/SampleMD/mdconfig.xml")

    call dataReader%LoadParticlesUsingConfigurations(configurations,particlePointers)

    potential => LoadPotentialByConfiguration(configurations)

    call g%CreateGrid(particlePointers,potential%SizeOfGridCell())

    dataAnalyzers = DataAnalyzersFromConfiguration(configurations,particlePointers)

    runner => runnerSelector%Select("standard")
    call runner%Setup(g,potential,dataAnalyzers,configurations)

    call runner%Start()

end program SampleMD


