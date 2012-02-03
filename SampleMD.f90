program SampleMD
    use class_RandomVelocityBootstapper
    use class_IntegrationRunnerSelector
    use lib_ConfigurationManager
    use lib_PotentialManager
    use class_DataFileReader
    use class_Grid
    implicit none



    type cellc
        type(Cell),pointer :: cell
    end type

    type(IntegrationRunnerSelector) :: runnerSelector
    class(IntegrationRunnerBase) ,pointer :: runner

    type(SimulationConfigurations) :: configurations

    type(DataFileReader) :: dataReader
    type(Particle),allocatable ,target:: particles(:)
    type(Grid) :: g
    class(PotentialBase),pointer :: potential

    type(Cell),pointer :: c1
    type(Cell),pointer :: c3
    type(Cell),target :: c2

    type(Cell) :: cool

    class(Particle) ,pointer :: p
    type(Particle),target:: p2


    p2%Mass=1

    p=>p2

    c2=EmptyCell()
    c1=>c2
    call c1%AddParticle(p)

    c2=EmptyCell()
    p%Mass=2

    call c1%AddParticle(p)
    c3=>c2

    print *,c1%MoreValues()
    print *,c3%MoreValues()






    configurations = LoadSimulationConfigurations("mdconfig.xml")

    call dataReader%LoadParticlesUsingConfigurations(configurations,particles)
    !
    potential => LoadPotentialByName("lg")

    call g%CreateGrid(particles,potential%SizeOfGridCell())

    runner => runnerSelector%Select("standard")

    call runner%Setup(g,potential,configurations)

    call runner%Start()



end program SampleMD


