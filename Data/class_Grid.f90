module class_Grid
    use class_Cell
    use lib_GridAlgorithms
    use class_CellNeightbor
    use class_IntegrationConfigurationsBase
    implicit none

    public :: Grid
    private
    type Grid
        type(CellContainer),allocatable :: CellContainers(:,:,:)
        integer :: GridSize(3)
        real :: SimulationBoxSize(3)
        real :: SimulationBoxCoordinates(8,3)
        real :: GridPartitioningLength
    contains
        procedure :: CreateGrid
        procedure :: GetCell
        procedure :: DetermineCellNeighborsAndSelf
        procedure :: IsGhost
        procedure :: ForEachParticle
        procedure :: ForEachParticleWithConfigurations
        procedure :: ForEachParticleWithGridObjectAndCoordinates
        procedure :: DumpDataToFile
        procedure :: ParticleCellCoordinatesByPosition
        procedure :: MoveParticleBetweenCells
    end type Grid

    abstract interface
        subroutine IMapParticlesWithConfigurations(p,configurations)
            import
            type(Particle),pointer :: p
            class(IntegrationConfigurationsBase) :: configurations
        end subroutine

        subroutine IMapParticles(p)
            import
            type(Particle),pointer :: p
        end subroutine

        subroutine IMapParticlesWithGridObjectAndCoordinates(g,p,currentGridCoordinates)
            import
            type(Particle),pointer :: p
            type(Grid) :: g
            integer :: currentGridCoordinates(3)
        end subroutine
    end interface

contains

    subroutine CreateGrid(this,particles,rc)
        class(Grid) :: this
        type(Particle),allocatable :: particles(:)
        real :: rc

        real ::box(8,3)

        integer :: GhostCellsWidth = 2

        real :: addToGrid

        addToGrid = 1.342857142857143

        box = DetermineSimulationBoxCoordinates(particles,addToGrid)

        this%SimulationBoxSize = DetermineSimulationBoxDimensions(box)
        this%SimulationBoxCoordinates = box

        call AllocateGridByCutOffRadiiWithGhostCells(this%CellContainers,rc,box)

        this%GridSize(1) = size(this%CellContainers,1)-GhostCellsWidth
        this%GridSize(2) = size(this%CellContainers,2)-GhostCellsWidth
        this%GridSize(3) = size(this%CellContainers,3)-GhostCellsWidth

        this%GridPartitioningLength = rc


        call DistributeParticlesToGrid(this%CellContainers,particles,rc,box,this%GridSize)

    end subroutine CreateGrid

    function GetCell(this,x,y,z) result(c)
        type(Cell),pointer :: c
        class(Grid) :: this

        integer :: x,y,z
        c=>this%CellContainers(x,y,z)%C

    end function

    function IsGhost(this,x,y,z) result(ghost)
        logical :: ghost
        class(Grid) :: this

        integer :: x,y,z
        ghost=this%CellContainers(x,y,z)%Ghost

    end function

    function DetermineCellNeighborsAndSelf(this,x,y,z) result (cellNeighbors)
        integer :: x,y,z
        class(Grid) :: this

        type(Cell),pointer :: currentCell

        type(CellNeightbor) :: cellNeighbors(27)

        type(CellContainer) :: cellsContainerCube(3,3,3)

        integer :: I,J,K,counter

        counter=1

        cellsContainerCube=this%CellContainers((/x-1,x,x+1/),(/y-1,y,y+1/),(/z-1,z,z+1/))

        do I=1,3
            do J=1,3
                do K=1,3

                    currentCell => cellsContainerCube(I,J,K)%C
                    cellNeighbors(counter)%C => currentCell
                    cellNeighbors(counter)%Ghost = cellsContainerCube(I,J,K)%Ghost
                    counter=counter+1

                end do
            end do
        end do






    end function

    subroutine ForEachParticleWithConfigurations(this,proc,configurations)
        class(Grid) :: this
        procedure(IMapParticlesWithConfigurations),pointer,intent(in) :: proc
        class(IntegrationConfigurationsBase) :: configurations
        type(Cell),pointer :: currentCell
        integer :: I,J,K

        type(Particle),pointer :: currentParticle

        do I=1,this%GridSize(1)
            do J=1,this%GridSize(2)
                do K=1,this%GridSize(3)

                    currentCell => this%CellContainers(I,J,K)%C

                    call currentCell%Reset()

                    do while (currentCell%AreThereMoreParticles())

                        currentParticle => currentCell%CurrentValue()

                        call proc(currentParticle,configurations)

                        call currentCell%Next()

                    end do

                end do
            end do
        end do

    end subroutine ForEachParticleWithConfigurations

    subroutine ForEachParticle(this,proc)
        class(Grid) :: this
        procedure(IMapParticles),pointer,intent(in) :: proc
        type(Cell),pointer :: currentCell
        integer :: I,J,K

        type(Particle),pointer :: currentParticle

        do I=1,this%GridSize(1)
            do J=1,this%GridSize(2)
                do K=1,this%GridSize(3)

                    currentCell => this%CellContainers(I,J,K)%C

                    call currentCell%Reset()

                    do while (currentCell%AreThereMoreParticles())

                        currentParticle => currentCell%CurrentValue()

                        call proc(currentParticle)

                        call currentCell%Next()

                    end do

                end do
            end do
        end do

    end subroutine

    subroutine ForEachParticleWithGridObjectAndCoordinates(this,proc)
        class(Grid) :: this
        procedure(IMapParticlesWithGridObjectAndCoordinates),pointer,intent(in) :: proc
        type(Cell),pointer :: currentCell
        integer :: I,J,K

        type(Particle),pointer :: currentParticle

        do I=1,this%GridSize(1)
            do J=1,this%GridSize(2)
                do K=1,this%GridSize(3)

                    currentCell => this%CellContainers(I,J,K)%C

                    call currentCell%Reset()

                    do while (currentCell%AreThereMoreParticles())

                        currentParticle => currentCell%CurrentValue()

                        call proc(this,currentParticle,currentCell%GetCellCoordinates())

                        call currentCell%Next()

                    end do

                end do
            end do
        end do
    end subroutine ForEachParticleWithGridObjectAndCoordinates

    function ParticleCellCoordinatesByPosition(this,particlePos) result(cord)
        class(Grid) :: this

        real :: particlePos(3)
        real :: cord(3)

        real :: box(8,3)

        real :: particleDeltaX,particleDeltaY,particleDeltaZ

        real :: rc,xIndex,yIndex,zIndex

        real :: gridSize(3)

        gridSize = this%GridSize

        rc=this%GridPartitioningLength

        box = this%SimulationBoxCoordinates

        particleDeltaX = (particlePos(1)-box(1,1))/rc
        particleDeltaY = (particlePos(2)-box(1,2))/rc
        particleDeltaZ = (particlePos(3)-box(1,3))/rc



        if ( particleDeltaX .lt. 0 ) then
            xIndex=gridSize(1)
        else
            xIndex=ceiling(particleDeltaX)
        end if

        if ( particleDeltaY .lt. 0 ) then
            yIndex=gridSize(2)
        else
            yIndex=ceiling(particleDeltaY)
        end if

        if ( particleDeltaZ .lt. 0 ) then
            zIndex=gridSize(3)
        else
            zIndex=ceiling(particleDeltaZ)
        end if

        if (xIndex == 0)  xIndex=1
        if (yIndex == 0)  yIndex=1
        if (zIndex == 0)  zIndex=1

        if (xIndex > gridSize(1)) xIndex=1
        if (yIndex > gridSize(2)) yIndex=1
        if (zIndex > gridSize(3)) zIndex=1

        cord = (/xIndex,yIndex,zIndex/)

    end function

    subroutine MoveParticleBetweenCells(this,currentCellCoord,targetCellCoord,p)
        class(Grid) :: this
        type(Particle),pointer :: p
        integer :: currentCellCoord(3)
        integer :: targetCellCoord(3)

        type(Particle),pointer :: p2

        type(Cell),pointer :: currentCell,targetCell

        currentCell => this%GetCell(currentCellCoord(1),currentCellCoord(2),currentCellCoord(3))
        targetCell => this%GetCell(targetCellCoord(1),targetCellCoord(2),targetCellCoord(3))


        call currentCell%RemoveParticle(p)

        print *,p%ID

       ! call targetCell%AddParticle(p)

    end subroutine MoveParticleBetweenCells

    subroutine DumpDataToFile(this)
        class(Grid) :: this
        type(Cell),pointer :: currentCell
        integer :: I,J,K

        type(Particle),pointer :: currentParticle

        open(unit=98,file="dump.data",form="formatted",access="append")

        do I=1,this%GridSize(1)
            do J=1,this%GridSize(2)
                do K=1,this%GridSize(3)

                    currentCell => this%CellContainers(I,J,K)%C

                    call currentCell%Reset()

                    do while (currentCell%AreThereMoreParticles())

                        currentParticle => currentCell%CurrentValue()

                        write(98,'(I6.4,9(F15.8,2X))'),currentParticle%ID, currentParticle%Position,currentParticle%Force,currentParticle%Velocity

                        call currentCell%Next()

                    end do

                end do
            end do
        end do

        write (98,*),"-------------------------------------------------------------"

        close(98)

    end subroutine DumpDataToFile





end module class_Grid
