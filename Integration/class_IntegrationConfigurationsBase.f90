module class_IntegrationConfigurationsBase
    implicit none

        type,abstract :: IntegrationConfigurationsBase
            real :: TimeStep
            real :: EndOfSimulation
        end type

    contains
end module class_IntegrationConfigurationsBase
