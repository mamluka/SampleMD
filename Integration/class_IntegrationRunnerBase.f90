module class_IntegrationRunnerBase
    use class_PotentialBase
    use lib_ConfigurationManager
    use class_Grid
    implicit none

    public :: IntegrationRunnerBase

    type,abstract :: IntegrationRunnerBase
        class(PotentialBase) ,pointer :: Potential
        type(Grid) :: G
        type(SimulationConfigurations) :: GlobalConfigurations
    contains
        procedure(IStart),deferred :: Start
        procedure(ISetup),deferred :: Setup
        procedure(ILoadIntegraionConfigurations),deferred :: LoadIntegraionConfigurations
    end type

    abstract interface
        subroutine IStart(this)
            import
            class(IntegrationRunnerBase) :: this
        end subroutine

        subroutine ISetup(this,g,potential,configurations)
            import
            class(IntegrationRunnerBase) :: this
            type(Grid) :: g
            type(SimulationConfigurations) :: configurations
            class(PotentialBase),pointer :: potential
        end subroutine

        subroutine ILoadIntegraionConfigurations(this,simConfigurations)
            import
            class(IntegrationRunnerBase) :: this
            type(SimulationConfigurations) :: simConfigurations
        end subroutine ILoadIntegraionConfigurations
    end interface

    contains




end module class_IntegrationRunnerBase
