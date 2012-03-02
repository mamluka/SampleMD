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
    use class_ParticlePredicateForID
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
    type(ParticlePredicateForID) :: predicate

    real :: pos(3)

    configurations = LoadSimulationConfigurations("/home/mamluka/SampleMD/mdconfig.xml")

    call dataReader%LoadParticlesUsingConfigurations(configurations,particles)

    potential => LoadPotentialByConfiguration(configurations)

    call g%CreateGrid(particles,potential%SizeOfGridCell())

    !    predicate%ParticleID=73;
    !
    !
    !    c=>g%GetCell(1,1,1)
    !    call c%RemoveWhenTrue(predicate)
    !
    !    call c%Reset()
    !
    !    do while (c%AreThereMoreParticles())
    !        p=> c%CurrentValue()
    !
    !        print *,p%ID
    !
    !        call c%Next()
    !    end do
    !allocate(aa,source=12123)

    runner => runnerSelector%Select("standard")
    call runner%Setup(g,potential,configurations)

    call runner%Start()

end program SampleMD


