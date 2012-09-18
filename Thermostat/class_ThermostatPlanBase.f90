module class_ThermostatPlanBase
    use class_SimulationConfigurationsDTO
    use class_ReducersDTO
    implicit none

    public :: ThermostatPlanBase

    type,abstract :: ThermostatPlanBase
        integer :: ApplyRate = 50
        type(SimulationConfigurationsDTO) :: Simulation
        type(ReducersDTO) :: reducers
    contains
        procedure(ICalculateBeta),deferred  :: CalculateBeta
        procedure(IIsFinished),deferred :: IsFinished
        procedure :: SetupThermostat
    end type

    abstract interface
        function ICalculateBeta(this,currentTemp,time) result(beta)
            import
            class(ThermostatPlanBase) :: this
            real :: currentTemp
            real :: time
            real :: beta
        end function

        function IIsFinished(this,currentTemp,time) result(fResult)
            import
            class(ThermostatPlanBase) :: this
            real :: currentTemp
            real :: time
            logical :: fResult
        end function
    end interface

contains

    subroutine SetupThermostat(this,simulation,reducers)
        class(ThermostatPlanBase) :: this
        type(SimulationConfigurationsDTO) :: simulation
        type(ReducersDTO) :: reducers

        this%Simulation = simulation
        this%Reducers = reducers
    end subroutine

end module class_ThermostatPlanBase
