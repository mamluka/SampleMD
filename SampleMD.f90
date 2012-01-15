program SampleMD
    use class_ConfigurationManager
    use class_ParticleLinkedList
    use class_Particle
    use class_DataFileReader
    use lib_GridAlgorithms
    use iso_fortran_env
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
    integer :: i

    !   configurations=LoadSimulationConfigurations("mdconfig.xml")

    !    print *,configurations%Dimension,configurations%TimeStep

    call dataReader%LoadParticlesIntoAnArray("data.xyz",particles)

    box = DetermineSimulationBoxCoordinates(particles)

    do  i=1,8
        print *,box(i,:)
    end do










end program SampleMD
