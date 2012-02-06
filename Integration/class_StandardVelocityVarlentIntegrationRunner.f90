module class_StandardVelocityVarlentIntegrationRunner
    use class_IntegrationRunnerBase
    use class_Grid
    use class_Particle
    use lib_Integration
    use class_Cell
    use class_CellNeightbor
    implicit none

    public :: StandardVelocityVarlentIntegrationRunner,Create

    type,extends(IntegrationRunnerBase) :: StandardVelocityVarlentIntegrationRunner
    contains
        procedure :: Start
        procedure :: Setup
    end type

contains

    subroutine Start(this)
        class(StandardVelocityVarlentIntegrationRunner) :: this

        print *,"Starting..."

        print *,"Calculating forces for the first time"

        call CalculateForces(this%G)

    end subroutine

    subroutine Setup(this,g,potential,configurations)
        class(StandardVelocityVarlentIntegrationRunner) :: this
        type(Grid) :: g
        type(SimulationConfigurations) :: configurations
        class(PotentialBase),pointer :: potential

        this%G=g
        this%Configurations=configurations
        this%Potential => potential
    end subroutine

    function Create() result(runner)
        type(StandardVelocityVarlentIntegrationRunner),pointer :: runner
        allocate(runner)
    end function

    subroutine CalculateForces(g)
        type(Grid) :: g

        integer :: TotalX,TotalY,TotalZ
        integer :: IndexX,IndexY,IndexZ
        integer :: NeighborIndex

        integer :: flop

        type(Cell),pointer :: currentCell
        type(Cell) :: currentNeighborCell

        type(Particle),pointer :: currentParticle,currentInteractionParticle

        type(CellNeightbor) :: CellNeighbors(26)

        integer :: InnerMaxX,InnerMinX,InnerMaxY,InnerMinY,InnerMaxZ,InnerMinZ

        real :: Distance

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

                    CellNeighbors = g%DetermineCellNeighbors(IndexX,IndexY,IndexZ)

                    do while (currentCell%AreThereMoreParticles())

                        currentParticle => currentCell%CurrentValue()

                        currentParticle%Force(:)=0

                        do NeighborIndex=1,size(CellNeighbors)

                            currentNeighborCell=CellNeighbors(NeighborIndex)%C



                            !print *,CellNeighbors(NeighborIndex)%Ghost

                            do while (currentNeighborCell%AreThereMoreParticles())

                                currentInteractionParticle=>currentNeighborCell%CurrentValue()



                                if (CellNeighbors(NeighborIndex)%Ghost == .true. ) then
                                    Distance=DistanceBetweenParticlesWithPeriodicConditions(currentParticle,currentInteractionParticle,currentCell%GetCellCoordinates(),currentNeighborCell%GetCellCoordinates(),g%SimulationBoxSize)
                                else
                                    Distance=DistanceBetweenParticles(currentParticle,currentInteractionParticle)
                                endif

                                flop=flop+1


                                call currentNeighborCell%Next()

                            end do

                        end do

                        call currentCell%Next()
                    end do


                end do
            end do
        end do

        print *,"number of oporations on particles",flop



    end subroutine CalculateForces



end module class_StandardVelocityVarlentIntegrationRunner
