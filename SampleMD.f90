program SampleMD
    use class_ConfigurationManager
    use class_ParticleLinkedList
    use class_Particle
    use class_DataFileReader
    use lib_GridAlgorithms
    use iso_fortran_env
    use class_Grid
    use class_RandomVelocityBootstapper

    implicit none

    type (SimulationConfigurations) :: configurations
    type (ParticleLinkedList) :: list
    class (Particle) ,pointer :: p
    type (Particle) , target :: atom
    type (DataFileReader) :: dataReader
    character :: type
    real :: x,y,z
    integer :: inputStatus
    type(Particle),allocatable :: particles(:)
    real :: box(8,3)
    real ::boxsize(3)
    integer :: i
    type(Grid) :: g
    type(RandomVelocityBootstapper) :: vb

    !   configurations=LoadSimulationConfigurations("mdconfig.xml")

    !    print *,configurations%Dimension,configurations%TimeStep

    call dataReader%LoadParticlesIntoAnArray("data.xyz",particles)
    call vb%LoadVelocityIntoAnArray(particles)

    g=CreateGrid(particles,1.78)













end program SampleMD
