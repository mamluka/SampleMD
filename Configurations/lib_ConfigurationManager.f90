module lib_ConfigurationManager
    use class_SimulationConfigurationsDTO
    use class_ReducersDTO
    use class_DataOptionsDTO
    use class_DataAnalyzersListDTO
    use class_ConfigurationsDTO
    implicit none

contains

    function LoadSimulationConfigurations(filename) result (configurations)
        type(ConfigurationsDTO) :: configurations

        type(SimulationConfigurationsDTO) :: SimulationConfigurations
        type(ReducersDTO) :: Reducers
        type(DataOptionsDTO) :: DataOptions
        type(DataAnalyzersListDTO) ::DataAnalyzersList

        SimulationConfigurations%TimeStep=0.00217
        SimulationConfigurations%EndOfSimulation=0.00217*100
        SimulationConfigurations%Dimension=3
        SimulationConfigurations%PotentialName="argon"
        SimulationConfigurations%DataFilename="argon.xyz"

        allocate(configurations%SimulationConfigurations,source=SimulationConfigurations)

        Reducers%HasDimensionlessReduction = .true.
        Reducers%Time=2.1555
        Reducers%Energy=1.65E-12
        Reducers%Length=3.4
        Reducers%Mass=39.948

        allocate(configurations%Reducers,source=Reducers)

        DataOptions%TempForInitialVelocity = 300
        DataOptions%BootstrapperType = "dimensionless-random"
        DataOptions%UseVelocityStrapper = .true.

        allocate(configurations%DataOptions,source=DataOptions)

        DataAnalyzersList%KineticEnergy = .true.

        allocate(configurations%DataAnalyzersList,source=DataAnalyzersList)




    end function

end module lib_ConfigurationManager
