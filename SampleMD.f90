program SampleMD
    use class_RandomVelocityBootstapper
    use class_IntegrationRunnerSelector
    use lib_ConfigurationManager
    use lib_PotentialManager
    use class_DataFileReader
    use class_Grid
    use class_Cell
    use testme
    use IFPORT
    use lib_Math
    implicit none

    type(IntegrationRunnerSelector) :: runnerSelector
    class(IntegrationRunnerBase) ,pointer :: runner

    type(SimulationConfigurations) :: configurations

    type(DataFileReader) :: dataReader
    type(Particle),allocatable ,target:: particles(:)
    type(Grid) :: g
    class(PotentialBase),pointer :: potential
    type(Cell),pointer :: c
    type(Particle),pointer :: p

    real :: pos(3)

    configurations = LoadSimulationConfigurations("/home/mamluka/SampleMD/mdconfig.xml")

    call dataReader%LoadParticlesUsingConfigurations(configurations,particles)

    potential => LoadPotentialByConfiguration(configurations)

    call g%CreateGrid(particles,potential%SizeOfGridCell())

    c => g%GetCell(2,2,2)
    call c%RemoveParticle(c%CurrentValue())

    call c%Reset()

    do while (c%AreThereMoreParticles())
        p=> c%CurrentValue()
        print *, p%ID
        call c%Next()
    end do

    runner => runnerSelector%Select("standard")
    call runner%Setup(g,potential,configurations)

    call runner%Start()

end program SampleMD


