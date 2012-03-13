module lib_ConfigurationManager
    use class_SimulationConfigurationsDTO
    use class_ReducersDTO
    use class_DataOptionsDTO
    use class_DataAnalyzersListDTO
    use class_ConfigurationsDTO
    use class_ThermostatPlansContainer
    use class_ThermostatPlan
    implicit none

contains

    function LoadConfigurations() result (configurations)
        type(ConfigurationsDTO) :: configurations

        type(SimulationConfigurationsDTO) :: SimulationConfigurations
        type(ReducersDTO) :: Reducers
        type(DataOptionsDTO) :: DataOptions
        type(DataAnalyzersListDTO) ::DataAnalyzersList
        type(ThermostatPlan),pointer :: plan


        SimulationConfigurations%TimeStep=0.00217
        SimulationConfigurations%EndOfSimulation=0.00217*100000
        SimulationConfigurations%Dimension=3
        SimulationConfigurations%PotentialName="argon"
        SimulationConfigurations%DataFilename="argon.xyz"

        allocate(configurations%SimulationConfigurations,source=SimulationConfigurations)

        Reducers%HasDimensionlessReduction = .true.
        Reducers%Time=2.1555
        Reducers%Energy=1.65E-21
        Reducers%Length=3.4
        Reducers%Mass=39.948

        allocate(configurations%Reducers,source=Reducers)

        DataOptions%TempForInitialVelocity = 50
        DataOptions%BootstrapperType = "dimensionless-random"
        DataOptions%UseVelocityStrapper = .true.

        allocate(configurations%DataOptions,source=DataOptions)

        DataAnalyzersList%KineticEnergy = .true.
        DataAnalyzersList%Temperature = .true.

        allocate(configurations%DataAnalyzersList,source=DataAnalyzersList)

        plan => plan%CreateThermostatPlan(200.0,360.0,0.02,.false.)

        call configurations%ThermostatPlans%AddThermostatPlan(plan)

        plan => plan%CreateThermostatPlan(550.0,70.0,0.01,.false.)

        call configurations%ThermostatPlans%AddThermostatPlan(plan)


    end function

end module lib_ConfigurationManager
