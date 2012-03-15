module lib_ThermostatFactory
    use class_RelaxationThermostatPlan
    use class_DeltaThermostatPlan
    implicit none

    contains

    function DeltaFromHereThermostat(endTemp,rate,multiplyRateByTimeStep) result(plan)
        type(DeltaThermostatPlan),pointer :: plan
        real :: endTemp,rate
        logical :: multiplyRateByTimeStep

        allocate(plan)
        plan%EndTemp = endTemp
        plan%Rate = rate
        plan%MultiplyRateByTimeStep = multiplyRateByTimeStep

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

end module lib_ThermostatFactory
