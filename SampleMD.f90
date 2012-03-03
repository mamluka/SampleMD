program SampleMD
    use class_RandomVelocityBootstapper
    use class_IntegrationRunnerSelector
    use lib_ConfigurationManager
    use lib_PotentialManager
    use class_DataFileReader
    use class_Grid
    use class_ParticlePredicateForID
    use lib_DataAnalysisManager
    implicit none

    type(IntegrationRunnerSelector) :: runnerSelector
    class(IntegrationRunnerBase) ,pointer :: runner

    type(SimulationConfigurations) :: configurations

    type(DataFileReader) :: dataReader
    type(Particle),allocatable ,target:: particles(:)
    type(Grid) :: g
    class(PotentialBase),pointer :: potential

    type(DataAnalyzersContainer) :: dataAnalyzers

    type(Particle),pointer ::p

    configurations = LoadSimulationConfigurations("/home/mamluka/SampleMD/mdconfig.xml")

    call dataReader%LoadParticlesUsingConfigurations(configurations,particles)

    potential => LoadPotentialByConfiguration(configurations)

    call g%CreateGrid(particles,potential%SizeOfGridCell())

    p=>particles(1)


    dataAnalyzers = DataAnalyzersFromConfiguration(configurations,particles)

    print *,p%Position

    runner => runnerSelector%Select("standard")
    call runner%Setup(g,potential,dataAnalyzers,configurations)

    call runner%Start()

    print *,p%Position,p%Mass

end program SampleMD


