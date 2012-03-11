module class_ThermostatPlan
    implicit none
    private
    public :: ThermostatPlan

    type ThermostatPlan
    contains
        procedure,nopass:: CreateThermostatPlan

    end type

contains

    function CreateThermostatPlan(startTemp,endTemp,rate) result(plan)
        type(ThermostatPlan) :: plan
        real :: startTemp,endTemp,rate


    end function

end module class_ThermostatPlan
