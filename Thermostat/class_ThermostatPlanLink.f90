module class_ThermostatPlanLink
    use class_ThermostatPlan
    implicit none

    private
    public :: ThermostatPlanLink,CreateThermostatPlanLink

    type ThermostatPlanLink
        private
        type(ThermostatPlan), pointer :: value => null() ! value stored in ThermostatPlanLink
        type(ThermostatPlanLink), pointer :: next => null()! next ThermostatPlanLink in list
    contains
        procedure :: getThermostatPlan   ! return value pointer
        procedure :: nextThermostatPlanLink    ! return next pointer
        procedure :: setNextThermostatPlanLink ! set next pointer
        procedure :: nullifyNext
    end type

contains

    function nextThermostatPlanLink(this)
        class(ThermostatPlanLink) :: this
        type(ThermostatPlanLink), pointer :: nextThermostatPlanLink
        if (.not. associated(this%next) ) then
            nextThermostatPlanLink=>null()
        else
            nextThermostatPlanLink => this%next
        endif
    end function nextThermostatPlanLink

    subroutine setNextThermostatPlanLink(this,next)
        class(ThermostatPlanLink) :: this
        type(ThermostatPlanLink), pointer :: next
        this%next => next
    end subroutine setNextThermostatPlanLink

    function getThermostatPlan(this)
        class(ThermostatPlanLink) :: this
        type(ThermostatPlan), pointer :: getThermostatPlan
        getThermostatPlan => this%value

    end function getThermostatPlan

    function CreateThermostatPlanLink(value)
        class(ThermostatPlanLink),pointer :: CreateThermostatPlanLink
        type(ThermostatPlan),pointer :: value
        type(ThermostatPlanLink),target :: newLink

        allocate(CreateThermostatPlanLink,source=newLink)
        CreateThermostatPlanLink%value=>value

    end function CreateThermostatPlanLink

    subroutine nullifyNext(this)
        class(ThermostatPlanLink) :: this
        this%next => null()
    end subroutine nullifyNext
end module class_ThermostatPlanLink
