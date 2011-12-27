program SampleMD
    use class_ConfigurationManager
    implicit none

    type (SimulationConfigurations) :: configurations

    configurations=LoadSimulationConfigurations("c:\mdconfig.xml")

    print *,configurations%Dimension,configurations%TimeStep


end program SampleMD
