module class_IntegrationRunnerSelector
    use class_IntegrationRunnerBase
    implicit none

    type IntegrationRunnerSelector
    contains
        procedure,nopass:: Select
    end type

contains

    function Select(runnerName) result (runner)
        class(IntegrationRunnerBase),pointer :: runner
        character (len=*) :: runnerName

        select case (runnerName)
            case ("standard")

            case default
        end select

    end function

end module class_IntegrationRunnerSelector
