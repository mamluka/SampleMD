module class_IntegrationRunnerBase
    use class_PotentialBase
    use lib_ConfigurationManager
    use class_Grid
    use class_DataAnalyzersContainer
    use class_ThermostatPlansContainer
    implicit none

    public :: IntegrationRunnerBase

    type,abstract :: IntegrationRunnerBase
        class(PotentialBase) ,pointer :: Potential
        type(Grid) :: G
        type(ConfigurationsDTO) :: GlobalConfigurations
        type(DataAnalyzersContainer) :: dataAnalyzers
        type(ParticlePointer),allocatable :: particlePointers(:)
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

        subroutine ISetup(this,g,potential,dataAnalyzers,configurations,particlePointers)
            import
            class(IntegrationRunnerBase) :: this
            type(Grid) :: g
            type(ConfigurationsDTO) :: configurations
            type(DataAnalyzersContainer) :: dataAnalyzers
            class(PotentialBase),pointer :: potential
            type(ParticlePointer),allocatable :: particlePointers(:)
        end subroutine

        subroutine ILoadIntegraionConfigurations(this,simConfigurations)
            import
            class(IntegrationRunnerBase) :: this
            type(ConfigurationsDTO) :: simConfigurations
        end subroutine ILoadIntegraionConfigurations
    end interface

    contains




end module class_IntegrationRunnerBase
