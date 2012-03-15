module lib_ConfigurationManager
    use class_SimulationConfigurationsDTO
    use class_ReducersDTO
    use class_DataOptionsDTO
    use class_DataAnalyzersListDTO
    use class_ConfigurationsDTO
    use class_ThermostatPlansContainer
    use class_ThermostatPlanBase
    use lib_ThermostatFactory
    implicit none

contains

    function LoadConfigurations() result (configurations)
        type(ConfigurationsDTO) :: configurations

        type(SimulationConfigurationsDTO) :: SimulationConfigurations
        type(ReducersDTO) :: Reducers
        type(DataOptionsDTO) :: DataOptions
        type(DataAnalyzersListDTO) ::DataAnalyzersList
        class(ThermostatPlanBase),pointer :: plan



        Reducers%HasDimensionlessReduction = .true.
        Reducers%Time=2.1555
        Reducers%Energy=1.65E-21
        Reducers%Length=3.4
        Reducers%Mass=39.948

        allocate(configurations%Reducers,source=Reducers)

        SimulationConfigurations%TimeStep=0.00217
        SimulationConfigurations%EndOfSimulation=0.00217*500000
        SimulationConfigurations%Dimension=3
        SimulationConfigurations%PotentialName="argon"
        SimulationConfigurations%DataFilename="argon.xyz"

        if (Reducers%HasDimensionlessReduction == .true.) then
            SimulationConfigurations%TimeStep = SimulationConfigurations%TimeStep/Reducers%Time
            SimulationConfigurations%EndOfSimulation = SimulationConfigurations%EndOfSimulation/Reducers%Time
        end if

        allocate(configurations%SimulationConfigurations,source=SimulationConfigurations)

        DataOptions%TempForInitialVelocity = 360
        DataOptions%BootstrapperType = "dimensionless-random"
        DataOptions%UseVelocityStrapper = .true.

        allocate(configurations%DataOptions,source=DataOptions)

        DataAnalyzersList%KineticEnergy = .true.
        DataAnalyzersList%Temperature = .true.

        allocate(configurations%DataAnalyzersList,source=DataAnalyzersList)

        plan => RelaxationThermostat(360.0,20.0,5.0)
        call plan%SetupThermostat(SimulationConfigurations,Reducers)

        call configurations%ThermostatPlans%AddThermostatPlan(plan)

        plan => DeltaFromHereThermostat(60.0,0.005,.false.)
        call plan%SetupThermostat(SimulationConfigurations,Reducers)

        call configurations%ThermostatPlans%AddThermostatPlan(plan)


    end function

end module lib_ConfigurationManager
