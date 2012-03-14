module class_StandardVelocityVarlentIntegrationRunner
    use class_IntegrationRunnerBase
    use class_Grid
    use class_Particle
    use lib_Integration
    use class_Cell
    use class_CellNeightbor
    use class_IntegrationConfigurationsBase
    use class_ParticlePredicateForCellMigration
    use class_DataAnalyzersContainer
    use lib_Parse
    use lib_Particles
    use lib_DataAnalysis
    implicit none
    private
    public :: StandardVelocityVarlentIntegrationRunner,Create

    type,extends(IntegrationConfigurationsBase) :: StandardIntegrationConfigurations

    end type

    type,extends(IntegrationRunnerBase) :: StandardVelocityVarlentIntegrationRunner
        type(StandardIntegrationConfigurations) :: Configurations
    contains
        procedure :: Start
        procedure :: Setup
        procedure :: LoadIntegraionConfigurations
    end type



contains

    subroutine Start(this)
        class(StandardVelocityVarlentIntegrationRunner) :: this

        real :: time

        integer :: stepCounter

        time=0

        stepCounter = 1

        call this%G%DumpDataToFile("/home/mamluka/mddata/argon",0)

        print *,"Starting..."

        call CalculateForces(this)

        do while (time .lt. this%Configurations%EndOfSimulation)

            time=time + this%Configurations%TimeStep

            call ComputePositions(this)

            call RedistributeParticlesToCells(this)

            if (mod(stepCounter,1) == 0) then

                call this%G%DumpDataToFile("/home/mamluka/mddata/argon",stepCounter)

            end if

            stepCounter = stepCounter + 1

            call CalculateForces(this)

            call ComputeVelocities(this)

            call ScaleVelocities(this,stepCounter)

            call AnalyzeData(this)

        end do

    end subroutine

    subroutine Setup(this,g,potential,dataAnalyzers,configurations,particlePointers)
        class(StandardVelocityVarlentIntegrationRunner) :: this
        type(Grid) :: g
        type(ConfigurationsDTO) :: configurations
        type(DataAnalyzersContainer) :: dataAnalyzers
        class(PotentialBase),pointer :: potential
        type(ParticlePointer),allocatable :: particlePointers(:)

        this%G=g
        call this%LoadIntegraionConfigurations(configurations)
        this%GlobalConfigurations = configurations
        this%Potential => potential
        this%DataAnalyzers = dataAnalyzers

        call CopyParticlePointer(particlePointers,this%ParticlePointers)

    end subroutine

    function Create() result(runner)
        type(StandardVelocityVarlentIntegrationRunner),pointer :: runner
        allocate(runner)
    end function

    subroutine CalculateForces(this)
        class(StandardVelocityVarlentIntegrationRunner) :: this
        type(Grid) :: g

        integer :: TotalX,TotalY,TotalZ
        integer :: IndexX,IndexY,IndexZ
        integer :: NeighborIndex

        integer :: flop

        type(Cell),pointer :: currentCell
        type(Cell) :: currentNeighborCell

        type(Particle),pointer :: currentParticle,currentInteractionParticle

        type(CellNeightbor) :: CellNeighbors(27)

        integer :: InnerMaxX,InnerMinX,InnerMaxY,InnerMinY,InnerMaxZ,InnerMinZ

        real :: Distance,forceDirection(3)

        integer :: ncounter

        logical :: isGhostCell

        real :: minDistance

        minDistance=10


        forceDirection=0

        ncounter=0

        g=this%G

        flop=0

        TotalX = g%GridSize(1)
        TotalY = g%GridSize(2)
        TotalZ = g%GridSize(3)

        InnerMaxX=g%gridSize(1)+1
        InnerMinX=2
        InnerMaxY=g%gridSize(2)+1
        InnerMinY=2
        InnerMaxZ=g%gridSize(3)+1
        InnerMinZ=2

        do IndexZ=InnerMinZ,InnerMaxZ
            do IndexY=InnerMinY,InnerMaxY
                do IndexX=InnerMinX,InnerMaxX

                    currentCell => g%GetCellWithGhostIncluded(IndexX,IndexY,IndexZ)

                    call currentCell%Reset()

                    CellNeighbors = g%DetermineCellNeighborsAndSelf(IndexX,IndexY,IndexZ)

                    do while (currentCell%AreThereMoreParticles())

                        currentParticle => currentCell%CurrentValue()

                        currentParticle%Force=0

                        do NeighborIndex=1,size(CellNeighbors)

                            currentNeighborCell=CellNeighbors(NeighborIndex)%C

                            call currentNeighborCell%Reset()

                            do while (currentNeighborCell%AreThereMoreParticles())

                                currentInteractionParticle=>currentNeighborCell%CurrentValue()

                                if ( .not. ( currentParticle%ID == currentInteractionParticle%ID)) then

                                    isGhostCell = CellNeighbors(NeighborIndex)%Ghost

                                    if ( isGhostCell == .true. ) then
                                        Distance=DistanceBetweenParticlesWithPeriodicConditions(currentParticle,currentInteractionParticle,currentCell%GetCellCoordinates(),currentNeighborCell%GetCellCoordinates(),g%SimulationBoxSize)
                                    else
                                        Distance=DistanceBetweenParticles(currentParticle,currentInteractionParticle)
                                    endif

                                    if (Distance .le. (this%Potential%CutOffRadii())) then

                                        ncounter=ncounter+1

                                        forceDirection = DirectionBetweenParticlesWithPeriodicConditions(currentParticle,currentInteractionParticle,currentCell%GetCellCoordinates(),currentNeighborCell%GetCellCoordinates(),g%SimulationBoxSize)

                                        call this%Potential%Force(currentParticle,currentInteractionParticle,Distance,forceDirection)

                                        flop=flop+1
                                    end if

                                    if (minDistance .gt. Distance) then
                                        minDistance = Distance
                                    end if

                                end if

                                call currentNeighborCell%Next()

                            end do

                        end do

                        ncounter=0

                        call currentCell%Next()
                    end do

                end do
            end do
        end do

        print *,minDistance

    end subroutine CalculateForces

    subroutine ComputePositions(this)
        class(StandardVelocityVarlentIntegrationRunner) :: this
        type(Grid) :: g

        call this%g%ForEachParticleWithConfigurations(CalculatePositionIterator,this%Configurations)


    end subroutine ComputePositions

    subroutine CalculatePositionIterator(p,configurations)
        type(Particle),pointer :: p
        class(IntegrationConfigurationsBase) :: configurations

        real ::a,dt

        dt = configurations%TimeStep
        a=dt*0.5/p%Mass

        p%Position = p%Position + dt*(p%Velocity+a*p%Force)
        p%ForceFromPreviousIteration = p%Force

    end subroutine CalculatePositionIterator

    subroutine RedistributeParticlesToCells(this)
        class(StandardVelocityVarlentIntegrationRunner) :: this

        call this%g%ForEachCell(RedistributeParticlesToCellsIterator)

    end subroutine RedistributeParticlesToCells

    subroutine RedistributeParticlesToCellsIterator(g,c)
        type(Grid) :: g
        type(Cell) :: c
        type(Cell) :: removedParticlesCell


        type(ParticlePredicateForCellMigration) :: predicate
        integer :: currentCellCoordinates(3),movedToCellCoordinates(3)
        type(Particle),pointer :: currentDeletedParticle
        type(Cell),pointer :: movedToCell
        integer :: movedCellCounter
        integer :: particleCount

        movedCellCounter=0

        particleCount = c%ParticleCount()

        currentCellCoordinates = c%GetCellCoordinates()

        call predicate%PredicateSetup(g,currentCellCoordinates)

        call c%RemoveWhenTrue(predicate,removedParticlesCell)

        if (removedParticlesCell%ParticleCount() .gt. 0) then

            do while (removedParticlesCell%AreThereMoreParticles())
                currentDeletedParticle => removedParticlesCell%CurrentValue()
                movedToCellCoordinates = g%ParticleCellCoordinatesByPosition(currentDeletedParticle%Position)
                call g%RebaseParticlePosition(currentDeletedParticle)
                movedToCell => g%GetCell(movedToCellCoordinates(1),movedToCellCoordinates(2),movedToCellCoordinates(3))

                call movedToCell%AddParticle(currentDeletedParticle)

                call removedParticlesCell%Next()
            end do


        end if

    end subroutine RedistributeParticlesToCellsIterator

    subroutine ComputeVelocities(this)
        class(StandardVelocityVarlentIntegrationRunner) :: this

        call this%g%ForEachParticleWithConfigurations(CalculateVelocitiesIterator,this%Configurations)

    end subroutine ComputeVelocities

    subroutine CalculateVelocitiesIterator(p,configurations)
        type(Particle),pointer :: p
        class(IntegrationConfigurationsBase) :: configurations

        real ::a,dt

        dt = configurations%TimeStep
        a=dt*0.5/p%Mass

        p%Velocity = p%Velocity + a*(p%ForceFromPreviousIteration+ p%Force)

    end subroutine CalculateVelocitiesIterator

    subroutine ScaleVelocities(this,step)
        class(StandardVelocityVarlentIntegrationRunner) :: this
        integer :: step,arraySize,i

        type(ThermostatPlan),pointer :: plan

        real :: beta,gammaParam
        real :: startTemp,endTemp,rate

        real :: T,Tafter

        if (mod(step,50) /= 0) return

        T = CalculateTemperature(this%ParticlePointers,this%GlobalConfigurations%Reducers%Energy)

        plan => this%Configurations%ThermostatPlans%CurrentThermostatPlan()
        startTemp = plan%StartTemp
        endTemp = plan%EndTemp
        rate = plan%Rate

        if ( ( T .gt. endTemp ) .and. (this%Configurations%ThermostatPlans%IsLastPlan() /= .true. ) ) then
            call this%Configurations%ThermostatPlans%Next()
            print *,"Next Plan"
        end if

        if (plan%MultiplyRateByTimeStep == .true. ) then
            gammaParam = rate*this%Configurations%TimeStep/this%GlobalConfigurations%Reducers%Time
        else
            gammaParam = rate
        end if

        beta = sqrt(1.0+gammaParam*(endTemp/T-1))

        arraySize = size(this%ParticlePointers)


        do i=1,arraySize
            this%ParticlePointers(i)%p%Velocity = this%ParticlePointers(i)%p%Velocity*beta
        end do

        Tafter = CalculateTemperature(this%ParticlePointers,this%GlobalConfigurations%Reducers%Energy)

        print *,"beta:",beta,"T after:",Tafter,"step:",step

    end subroutine




    subroutine AnalyzeData(this)
        class(StandardVelocityVarlentIntegrationRunner) :: this
        class(DataAnalyzerBase),pointer :: currentAnalyzer

        call this%DataAnalyzers%Reset()

        do while (this%DataAnalyzers%AreThereMoreAnalyzers())

            currentAnalyzer => this%DataAnalyzers%CurrentAnalyzer()
            call currentAnalyzer%Analyze(this%GlobalConfigurations)

            call this%DataAnalyzers%Next()
        end do

    end subroutine AnalyzeData


    subroutine LoadIntegraionConfigurations(this,simConfigurations)
        class(StandardVelocityVarlentIntegrationRunner) :: this
        type(ConfigurationsDTO) :: simConfigurations
        type(ThermostatPlan),pointer :: plan

        this%Configurations%TimeStep = simConfigurations%SimulationConfigurations%TimeStep
        this%Configurations%EndOfSimulation = simConfigurations%SimulationConfigurations%EndOfSimulation


        do while (simConfigurations%ThermostatPlans%AreThereMorePlans())
            plan=>simConfigurations%ThermostatPlans%CurrentThermostatPlan()

            call this%Configurations%ThermostatPlans%AddThermostatPlan(plan)

            call simConfigurations%ThermostatPlans%Next()
        end do

    end subroutine LoadIntegraionConfigurations



end module class_StandardVelocityVarlentIntegrationRunner
