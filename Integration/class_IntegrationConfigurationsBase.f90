module class_IntegrationConfigurationsBase
    use class_ThermostatPlansContainer
    implicit none

        type,abstract :: IntegrationConfigurationsBase
            real :: TimeStep
            real :: EndOfSimulation
            type(ThermostatPlansContainer) :: ThermostatPlans
        end type

    contains
end module class_IntegrationConfigurationsBase
