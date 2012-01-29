module class_IntegrationRunnerBase
    implicit none

    type,abstract :: IntegrationRunnerBase
        contains
        procedure(IStart),deferred,nopass :: Start
    end type

    abstract interface
        subroutine IStart()
        end subroutine
    end interface

end module class_IntegrationRunnerBase
