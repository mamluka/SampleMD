module class_StandardVelocityVarlentIntegrationRunner
    use class_IntegrationRunnerBase
    use class_Grid
    use class_Particle
    use lib_Integration
    use class_Cell
    use class_CellNeightbor
    use class_IntegrationConfigurationsBase
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

        time=0

        print *,"Starting..."

        call CalculateForces(this)

        this%Configurations%EndOfSimulation=this%Configurations%TimeStep*30

        do while (time .lt. this%Configurations%EndOfSimulation)

            time=time + this%Configurations%TimeStep

            call ComputePositions(this)

            call RedistributeParticlesToCells(this)

            call CalculateForces(this)

            call ComputeVelocities(this)

        end do

        call this%G%DumpDataToFile()



    end subroutine

    subroutine Setup(this,g,potential,configurations)
        class(StandardVelocityVarlentIntegrationRunner) :: this
        type(Grid) :: g
        type(SimulationConfigurations) :: configurations
        class(PotentialBase),pointer :: potential

        this%G=g
        call this%LoadIntegraionConfigurations(configurations)
        this%GlobalConfigurations = configurations
        this%Potential => potential

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

                    currentCell => g%GetCell(IndexX,IndexY,IndexZ)

                    call currentCell%Reset()

                    CellNeighbors = g%DetermineCellNeighborsAndSelf(IndexX,IndexY,IndexZ)

                    do while (currentCell%AreThereMoreParticles())

                        currentParticle => currentCell%CurrentValue()

                        currentParticle%Force=0

                        do NeighborIndex=1,size(CellNeighbors)

                            currentNeighborCell=CellNeighbors(NeighborIndex)%C

                            call currentNeighborCell%Reset()

                            !print *,currentNeighborCell%ParticleCount(),currentNeighborCell%GetCellCoordinates()

                            !print *,CellNeighbors(NeighborIndex)%Ghost

                            do while (currentNeighborCell%AreThereMoreParticles())

                                currentInteractionParticle=>currentNeighborCell%CurrentValue()

                                if ( .not. ( currentParticle%ID == currentInteractionParticle%ID)) then

                                    isGhostCell = CellNeighbors(NeighborIndex)%Ghost

                                    if (currentNeighborCell%ParticleCount() == 8) then
                                        !print *,currentInteractionParticle%Position
                                    end if

                                    if ( isGhostCell == .true. ) then
                                        Distance=DistanceBetweenParticlesWithPeriodicConditions(currentParticle,currentInteractionParticle,currentCell%GetCellCoordinates(),currentNeighborCell%GetCellCoordinates(),g%SimulationBoxSize)
                                    else
                                        Distance=DistanceBetweenParticles(currentParticle,currentInteractionParticle)
                                    endif

                                    !print *,currentParticle%Position-currentInteractionParticle%Position


                                    if (Distance .le. (this%Potential%CutOffRadii())) then

                                        ncounter=ncounter+1

                                        forceDirection = DirectionBetweenParticlesWithPeriodicConditions(currentParticle,currentInteractionParticle,currentCell%GetCellCoordinates(),currentNeighborCell%GetCellCoordinates(),g%SimulationBoxSize)

                                        call this%Potential%Force(currentParticle,currentInteractionParticle,Distance,forceDirection)

                                        flop=flop+1
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

        !print *,"number of oporations on particles",flop



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

        call this%g%ForEachParticleWithGridObjectAndCoordinates(RedistributeParticlesToCellsIterator)

    end subroutine RedistributeParticlesToCells

    subroutine RedistributeParticlesToCellsIterator(g,p,currentCellCoordinates)
        type(Particle),pointer :: p
        type(Grid) :: g
        integer :: currentCellCoordinates(3)
        integer :: currentParticleCoordinates(3)

        currentParticleCoordinates=g%ParticleCellCoordinatesByPosition(p%Position)
        if ( (currentCellCoordinates(1) /= currentParticleCoordinates(1)) .or. (currentCellCoordinates(2) /= currentParticleCoordinates(2)) .or. (currentCellCoordinates(3) /= currentParticleCoordinates(3))) then
            call g%MoveParticleBetweenCells(currentCellCoordinates,currentParticleCoordinates,p)
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


    subroutine LoadIntegraionConfigurations(this,simConfigurations)
        class(StandardVelocityVarlentIntegrationRunner) :: this
        type(SimulationConfigurations) :: simConfigurations

        type(StandardIntegrationConfigurations) :: standardConfigurations

        standardConfigurations%TimeStep = simConfigurations%TimeStep
        standardConfigurations%EndOfSimulation = simConfigurations%EndOfSimulation


        this%Configurations = standardConfigurations


    end subroutine LoadIntegraionConfigurations



end module class_StandardVelocityVarlentIntegrationRunner
