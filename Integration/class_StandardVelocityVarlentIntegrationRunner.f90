module class_StandardVelocityVarlentIntegrationRunner
    use class_IntegrationRunnerBase
    use class_Grid
    use class_Particle
    implicit none
    private

    public :: StandardVelocityVarlentIntegrationRunner,Create

    type,extends(IntegrationRunnerBase) :: StandardVelocityVarlentIntegrationRunner
        contains
        procedure,nopass :: Start
        procedure :: Setup
    end type

    contains

    subroutine Start()

    end subroutine

    subroutine Setup(this,g,potential,configurations)
        class(StandardVelocityVarlentIntegrationRunner) :: this
        type(Grid) :: g
        type(SimulationConfigurations) :: configurations
        class(PotentialBase),pointer :: potential

        this%Potential => potential
    end subroutine

    function Create() result(runner)
        type(StandardVelocityVarlentIntegrationRunner),pointer :: runner
        allocate(runner)
    end function

    subroutine CalculateForces(g)
        integer :: TotalX,TotalY,TotalZ
        integer :: IndexX,IndexY,IndexZ

        type(Grid) :: g

        type(Cell) :: currentCell

        type(Particle) :: currentParticle

        type(Cell) :: CellNeighbors(26)

        TotalX = g%GridSize(1)
        TotalY = g%GridSize(2)
        TotalZ = g%GridSize(3)



        do IndexZ=1,TotalZ
            do IndexY=1,TotalY
                do IndexX=1,TotalX

                    currentCell = g%GetCell(IndexX,IndexY,IndexZ)
                    CellNeighbors = g%DetermineCellNeighbors(IndexX,IndexY,IndexZ)

                    do while ( currentCell%AreThereMoreParticles() )

                            currentParticle = currentCell%CurrentValue()

                            currentParticle%Force(:)=0



                            call currentCell%Next()
                    end do


                end do
            end do
        end do



    end subroutine CalculateForces



end module class_StandardVelocityVarlentIntegrationRunner
