module class_ConfigurationsDTO
    use class_SimulationConfigurationsDTO
    use class_ReducersDTO
    use class_DataOptionsDTO
    use class_DataAnalyzersListDTO
    implicit none
    type :: ConfigurationsDTO
        type(SimulationConfigurationsDTO),allocatable :: SimulationConfigurations
        type(ReducersDTO),allocatable :: Reducers
        type(DataOptionsDTO),allocatable :: DataOptions
        type(DataAnalyzersListDTO),allocatable ::DataAnalyzersList
    end type
end module class_ConfigurationsDTO
