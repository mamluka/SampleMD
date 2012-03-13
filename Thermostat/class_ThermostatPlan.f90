module class_ThermostatPlan
    implicit none
    private
    public :: ThermostatPlan

    type ThermostatPlan
        real :: StartTemp
        real :: EndTemp
        real :: Rate
        logical :: MultiplyRateByTimeStep = .false.
    contains
        procedure,nopass:: CreateThermostatPlan
    end type

contains

    function CreateThermostatPlan(startTemp,endTemp,rate,multiplyRateByTimeStep) result(plan)
        type(ThermostatPlan),pointer :: plan
        real :: startTemp,endTemp,rate
        logical :: multiplyRateByTimeStep

        allocate(plan)
        plan%StartTemp = startTemp
        plan%EndTemp = endTemp
        plan%Rate = rate
        plan%MultiplyRateByTimeStep = multiplyRateByTimeStep

    end function

end module class_ThermostatPlan
