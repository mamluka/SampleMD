program SampleMD
    use class_RandomVelocityBootstapper
    use class_IntegrationRunnerSelector
    use lib_ConfigurationManager
    use class_DataFileReader
    use class_Grid
implicit none

    type(IntegrationRunnerSelector) :: runnerSelector
    class(IntegrationRunnerBase) ,pointer :: runner

    type(SimulationConfigurations) :: configurations

    type(DataFileReader) :: dataReader
    type(Particle),allocatable :: particles(:)
    type(Grid) :: g

    call dataReader%LoadParticlesUsingConfigurations(configurations,particles)





    runner => runnerSelector%Select("standard")

    call runner%Setup(g,configurations)
    call runner%Start()

    print *, "It works"


end program SampleMD
