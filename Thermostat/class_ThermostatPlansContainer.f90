module class_ThermostatPlansContainer
    use class_ThermostatPlanLink
    use class_ThermostatPlan
    implicit none

    private
    public :: ThermostatPlansContainer

    type :: ThermostatPlansContainer
        private
        type(ThermostatPlanLink),pointer :: firstLink => null() ! first link in list
        type(ThermostatPlanLink),pointer :: lastLink => null()  ! last link in list
        type(ThermostatPlanLink),pointer :: currLink => null()  ! list iterator
    contains
        procedure :: AddThermostatPlan
        procedure :: CurrentThermostatPlan
        procedure :: Next
        procedure :: AreThereMorePlans
        procedure :: IsLastPlan
        procedure :: Reset
    end type

contains

    subroutine AddThermostatPlan(this, value)
        class(ThermostatPlansContainer) :: this
        type(ThermostatPlan),pointer,intent(in) :: value
        type(ThermostatPlanLink), pointer :: newLink

        type(ThermostatPlanLink) ,pointer :: firstLink,nextLink

        if (.not. associated(this%firstLink)) then
            this%firstLink => CreateThermostatPlanLink(value)
            this%lastLink => this%firstLink
            this%currLink => this%firstLink
        else
            newLink => CreateThermostatPlanLink(value)
            call this%lastLink%setNextThermostatPlanLink(newLink)
            this%lastLink => newLink
        end if
    end subroutine

    function CurrentThermostatPlan(this)
        class(ThermostatPlansContainer) :: this
        class(ThermostatPlan), pointer :: CurrentThermostatPlan
        CurrentThermostatPlan => this%currLink%getThermostatPlan()
    end function

    subroutine Next(this)
        class(ThermostatPlansContainer) :: this
        this%currLink => this%currLink%nextThermostatPlanLink()


    end subroutine

    function AreThereMorePlans(this)
        class(ThermostatPlansContainer) :: this
        logical AreThereMorePlans
        AreThereMorePlans = associated(this%currLink)
    end function

    function IsLastPlan(this)
        class(ThermostatPlansContainer) :: this
        logical IsLastPlan
        if (associated(this%currLink%nextThermostatPlanLink()) == .false.)  IsLastPlan = .true.
    end function



    subroutine Reset(this)
        class(ThermostatPlansContainer) :: this
        this%currLink => this%firstLink
    end subroutine
end module class_ThermostatPlansContainer
