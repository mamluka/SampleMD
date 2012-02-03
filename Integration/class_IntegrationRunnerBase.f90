module class_IntegrationRunnerBase
    use class_PotentialBase
    use lib_ConfigurationManager
    use class_Grid
    implicit none

    public :: IntegrationRunnerBase

    type,abstract :: IntegrationRunnerBase
        class(PotentialBase) ,pointer :: Potential
        type(Grid) :: G
        type(SimulationConfigurations) :: Configurations
    contains
        procedure(IStart),deferred,nopass :: Start
        procedure(ISetup),deferred :: Setup
    end type

    abstract interface
        subroutine IStart()
        end subroutine

        subroutine ISetup(this,g,potential,configurations)
            import
            class(IntegrationRunnerBase) :: this
            type(Grid) :: g
            type(SimulationConfigurations) :: configurations
            class(PotentialBase),pointer :: potential
        end subroutine
    end interface

    contains




end module class_IntegrationRunnerBase
