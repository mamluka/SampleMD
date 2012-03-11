module class_ThermostatPlan
    implicit none
    private
    public :: ThermostatPlan

    type ThermostatPlan
        real :: StartTemp
        real :: EndTemp
        real :: Rate
    contains
        procedure,nopass:: CreateThermostatPlan
    end type

contains

    function CreateThermostatPlan(startTemp,endTemp,rate) result(plan)
        type(ThermostatPlan),pointer :: plan
        real :: startTemp,endTemp,rate

        allocate(plan)
        plan%StartTemp = startTemp
        plan%EndTemp = endTemp
        plan%Rate = rate

    end function

end module class_ThermostatPlan
