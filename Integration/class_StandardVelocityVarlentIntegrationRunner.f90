module class_StandardVelocityVarlentIntegrationRunner
    use class_IntegrationRunnerBase
    use class_Grid
    use lib_PotentialManager
    implicit none
    private

    public :: StandardVelocityVarlentIntegrationRunner,Create

    type,extends(IntegrationRunnerBase) :: StandardVelocityVarlentIntegrationRunner
        contains
        procedure,nopass :: Start
        procedure :: Setup
    end type

    contains

    subroutine Start()
        print *,"Standard"
    end subroutine

    subroutine Setup(this,g,configurations)
        class(StandardVelocityVarlentIntegrationRunner) :: this
        type(Grid) :: g
        type(SimulationConfigurations) :: configurations

        this%Potential => LoadPotentialByName(configurations%PotentialName)
    end subroutine

    function Create() result(runner)
        type(StandardVelocityVarlentIntegrationRunner),pointer :: runner
        allocate(runner)
    end function

end module class_StandardVelocityVarlentIntegrationRunner
