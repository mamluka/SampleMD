module class_ConstantRateThermostatPlan
    use class_ThermostatPlanBase
    implicit none


    type,extends(ThermostatPlanBase) :: ConstantRateThermostatPlan
        real :: EndTemp
        real :: Rate
    contains
        procedure :: CalculateBeta
        procedure :: IsFinished
    end type

contains

    function CalculateBeta(this,currentTemp,time) result(beta)
        class(ConstantRateThermostatPlan) :: this
        real :: currentTemp
        real :: time
        real :: beta

        real :: endTemp,rate,gammaParam,T
        rate = this%Rate

        if (currentTemp > this%EndTemp) then
            rate = 1-rate
        end if

        beta = rate
    end function

    function IsFinished(this,currentTemp,time) result(fResult)
        class(ConstantRateThermostatPlan) :: this
        real :: currentTemp
        real :: time
        logical :: fResult

        fResult = .false.

        if ( currentTemp .gt. this%EndTemp) then
            fResult = .true.
        end if

    end function

end module class_ConstantRateThermostatPlan
