program SampleMD
    use class_ConfigurationManager
    use class_ParticleLinkedList
    use class_Particle
    use class_DataLoader
    use iso_fortran_env
    implicit none

    type (SimulationConfigurations) :: configurations
    type (ParticleLinkedList) :: list
    class (Particle) ,pointer :: p
    type (Particle) , target :: atom
    type (DataLoader) :: dl
    character :: type
    real :: x,y,z
    integer :: inputStatus
    !   configurations=LoadSimulationConfigurations("mdconfig.xml")

    !    print *,configurations%Dimension,configurations%TimeStep











end program SampleMD
