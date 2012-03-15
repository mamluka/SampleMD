module class_RelaxationThermostatPlan
    use class_ThermostatPlanBase
    implicit none

    type,extends(ThermostatPlanBase) :: RelaxationThermostatPlan
        real :: RelaxationTemp
        real :: Duration
        real :: TimeStepMultiplier
        real :: internalTime=0
    contains
        procedure :: CalculateBeta
        procedure :: IsFinished
    end type

contains

    function CalculateBeta(this,currentTemp,time) result(beta)
        class(RelaxationThermostatPlan) :: this
        real :: currentTemp
        real :: time
        real :: beta

        real :: endTemp,rate,gammaParam,T,dt

        T=currentTemp

        endTemp = this%RelaxationTemp
        dt = this%Simulation%TimeStep
        gammaParam = this%TimeStepMultiplier*dt

        beta = sqrt(1.0+gammaParam*(endTemp/T-1))

        this%internalTime = this%internalTime+this%Simulation%TimeStep*this%Reducers%Time*this%ApplyRate

    end function

    function IsFinished(this,currentTemp,time) result(fResult)
        class(RelaxationThermostatPlan) :: this
        real :: currentTemp
        real :: time
        logical :: fResult

        fResult = .false.

        if ( this%internalTime .gt. this%Duration) then
            fResult = .true.
        end if

    end function

end module class_RelaxationThermostatPlan
