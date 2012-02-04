module class_Grid
    use class_Cell
    use lib_GridAlgorithms
        use class_NeighborCell
    implicit none

    public :: Grid

    type Grid
        type(CellContainer),allocatable :: CellContainers(:,:,:)
        integer :: GridSize(3)
        real :: SimulationBoxSize(3)
        contains
        procedure :: CreateGrid
        procedure :: GetCell
        procedure :: DetermineCellNeighbors
        procedure :: IsGhost
    end type Grid


    contains

    subroutine CreateGrid(this,particles,rc)
        class(Grid) :: this
        type(Particle),allocatable :: particles(:)
        real :: rc

        real ::box(8,3)

        integer :: GhostCellsWidth =2

        box = DetermineSimulationBoxCoordinates(particles)

        this%SimulationBoxSize = DetermineSimulationBoxDimensions(box)

        call AllocateGridByCutOffRadiiWithGhostCells(this%CellContainers,rc,box)

        this%GridSize(1) = size(this%CellContainers,1)-GhostCellsWidth
        this%GridSize(2) = size(this%CellContainers,2)-GhostCellsWidth
        this%GridSize(3) = size(this%CellContainers,3)-GhostCellsWidth

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

    function DetermineCellNeighbors(this,x,y,z) result (cellNeighbors)
        integer :: x,y,z
        class(Grid) :: this

        type(Cell),pointer :: currentCell

        type(CellNeightbor) :: cellNeighbors(26)

        type(CellContainer) :: cellsContainerCube(3,3,3)

        integer :: I,J,K,counter

        counter=1

        cellsContainerCube=this%CellContainers((/x-1,x,x+1/),(/y-1,y,y+1/),(/z-1,z,z+1/))

        do I=1,3
            do J=1,3
                do K=1,3
                    if (.not. (I==2 .and. J==2 .and. K==2)) then
                        currentCell => cellsContainerCube(I,J,K)%C
                        cellNeighbors(counter)%C => currentCell
                        cellNeighbors(counter)%Ghost = cellsContainerCube(I,J,K)%Ghost
                        counter=counter+1
                    endif
                end do
            end do
        end do






    end function



end module class_Grid
