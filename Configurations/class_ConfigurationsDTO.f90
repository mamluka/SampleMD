module class_ConfigurationsDTO
    use class_SimulationConfigurationsDTO
    use class_ReducersDTO
    use class_DataOptionsDTO
    use class_DataAnalyzersListDTO
    use class_ThermostatPlansContainer
    implicit none
    type :: ConfigurationsDTO
        type(SimulationConfigurationsDTO),allocatable :: SimulationConfigurations
        type(ReducersDTO),allocatable :: Reducers
        type(DataOptionsDTO),allocatable :: DataOptions
        type(DataAnalyzersListDTO),allocatable ::DataAnalyzersList
        type(ThermostatPlansContainer) :: ThermostatPlans
    end type
end module class_ConfigurationsDTO
