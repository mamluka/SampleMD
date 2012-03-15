module class_DeltaThermostatPlan
    use class_ThermostatPlanBase
    implicit none

    type,extends(ThermostatPlanBase) :: DeltaThermostatPlan
        real :: EndTemp
        real :: Rate
        real :: MultiplyRateByTimeStep
    contains
        procedure :: CalculateBeta
        procedure :: IsFinished
    end type

contains

    function CalculateBeta(this,currentTemp,time) result(beta)
        class(DeltaThermostatPlan) :: this
        real :: currentTemp
        real :: time
        real :: beta

        real :: endTemp,rate,gammaParam,T

        T=currentTemp

        endTemp = this%EndTemp
        rate = this%Rate

        if (this%MultiplyRateByTimeStep == .true. ) then
            gammaParam = rate*this%Simulation%TimeStep/this%Reducers%Time
        else
            gammaParam = rate
        end if

        beta = sqrt(1.0+gammaParam*(endTemp/T-1))
    end function

    function IsFinished(this,currentTemp,time) result(fResult)
        class(DeltaThermostatPlan) :: this
        real :: currentTemp
        real :: time
        logical :: fResult

        fResult = .false.

        if ( currentTemp .gt. this%EndTemp) then
            fResult = .true.
        end if

    end function

end module class_DeltaThermostatPlan
