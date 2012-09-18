module lib_ThermostatFactory
    use class_RelaxationThermostatPlan
    use class_DeltaThermostatPlan
    use class_ConstantRateThermostatPlan
    implicit none

    contains

    function DeltaFromHereThermostat(endTemp,rate,useTimeStepAsRate) result(plan)
        type(DeltaThermostatPlan),pointer :: plan
        real :: endTemp,rate
        logical :: useTimeStepAsRate

        allocate(plan)
        plan%EndTemp = endTemp
        plan%Rate = rate
        plan%UseTimeStepAsRate = useTimeStepAsRate

    end function

    function RelaxationThermostat(relaxationTemp,duration,timeStepMultiplier) result(plan)
        type(RelaxationThermostatPlan),pointer :: plan
        real :: relaxationTemp,duration
        real :: timeStepMultiplier

        allocate(plan)
        plan%RelaxationTemp = relaxationTemp
        plan%Duration = duration
        plan%TimeStepMultiplier = timeStepMultiplier

    end function

    function ConstantRateThermostat(endTemp,rate) result(plan)
        type(ConstantRateThermostatPlan),pointer :: plan
        real :: endTemp,rate

        allocate(plan)
        plan%Rate = rate
        plan%EndTemp = endTemp

    end function

end module lib_ThermostatFactory
