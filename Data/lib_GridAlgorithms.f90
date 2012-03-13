module lib_GridAlgorithms
    use class_Particle
    use class_Cell
    use class_CellContainer
    use class_ParticlePointer
    implicit none

contains

    function DetermineSimulationBoxCoordinates(particles) result(box)
        type(Particle),allocatable :: particles(:)
        real :: particlePosition(size(particles),3)
        real :: box(8,3)

        integer :: i,xPosition=1,yPosition=2,zPosition=3

        do i=1,size(particles)
            particlePosition(i,:) =  particles(i)%Position
        end do

        box(1:4,xPosition) = minval(particlePosition(:,xPosition))
        box(5:8,xPosition) = maxval(particlePosition(:,xPosition))

        box((/1,4,5,8/),yPosition) = minval(particlePosition(:,yPosition))
        box((/2,3,6,7/),yPosition) = maxval(particlePosition(:,yPosition))

        box((/1,2,5,6/),zPosition) = minval(particlePosition(:,zPosition))
        box((/3,4,7,8/),zPosition) = maxval(particlePosition(:,zPosition))

    end function DetermineSimulationBoxCoordinates

    function DetermineSimulationBoxDimensions(box) result (boxsize)
        real :: box(8,3)
        real :: boxsize(3)

        boxsize(1) = abs(box(1,1)-box(5,1))
        boxsize(2) = abs(box(1,2)-box(2,2))
        boxsize(3) = abs(box(1,3)-box(4,3))

    end function DetermineSimulationBoxDimensions

    subroutine AllocateGridByCutOffRadiiWithGhostCells(cellContainers,rc,box)
        type(CellContainer),allocatable,intent(inout) :: CellContainers(:,:,:)
        real ,intent(in) :: rc
        real ,intent(in):: box(8,3)
        real :: boxsize(3)
        integer :: numberOfXCells,numberOfYCells,numberOfZCells

        integer :: ghostCellsAddition=2
        integer :: I,J,K


        boxsize=DetermineSimulationBoxDimensions(box)

        numberOfXCells = ceiling(boxsize(1)/rc)+ghostCellsAddition
        numberOfYCells = ceiling(boxsize(2)/rc)+ghostCellsAddition
        numberOfZCells = ceiling(boxsize(3)/rc)+ghostCellsAddition

        allocate(cellContainers(numberOfXCells,numberOfYCells,numberOfZCells))





    end subroutine

    subroutine DistributeParticlesToGrid(cellContainers,particlePointers,rc,box,gridSize,boxSize)
        type(CellContainer),allocatable,intent(inout) :: cellContainers(:,:,:)
        real ,intent(in) :: rc
        type(ParticlePointer),allocatable,target :: particlePointers(:)
        real :: box(8,3)
        integer :: gridSize(3)
        real :: boxsize(3)

        type(Cell),pointer :: currentCell
        type(Particle),pointer ::particlePointer
        integer :: i,xIndex,yIndex,zIndex

        integer :: BoundryShift=1

        integer :: BoundryX,BoundryY,BoundryZ
        integer :: InnerMaxX,InnerMinX,InnerMaxY,InnerMinY,InnerMaxZ,InnerMinZ
        integer :: OuterMaxX,OuterMinX,OuterMaxY,OuterMinY,OuterMaxZ,OuterMinZ

        real :: xVector,yVector,zVector

        open(unit=98,file="cellsIndex.data")

        do i=1,size(particlePointers)

            xVector = abs(box(1,1)-particlePointers(i)%p%Position(1))
            yVector = abs(box(1,2)-particlePointers(i)%p%Position(2))
            zVector = abs(box(1,3)-particlePointers(i)%p%Position(3))

            xIndex = ceiling((xVector)/rc)
            yIndex = ceiling((yVector)/rc)
            zIndex = ceiling((zVector)/rc)

            if (xIndex == 0)  xIndex=1
            if (yIndex == 0)  yIndex=1
            if (zIndex == 0)  zIndex=1

            write (98,*),xIndex,yIndex,zIndex

            particlePointer => particlePointers(i)%p

            if (cellContainers(xIndex+BoundryShift,yIndex+BoundryShift,zIndex+BoundryShift)%Exists()) then
                currentCell => cellContainers(xIndex+BoundryShift,yIndex+BoundryShift,zIndex+BoundryShift)%CurrentCell()
            else
                currentCell => cellContainers(xIndex+BoundryShift,yIndex+BoundryShift,zIndex+BoundryShift)%New(xIndex,yIndex,zIndex)
            end if

            call currentCell%AddParticle(particlePointer)

            currentCell => null()

        end do

        close(98)

        InnerMaxX=gridSize(1)+1
        InnerMinX=2
        InnerMaxY=gridSize(2)+1
        InnerMinY=2
        InnerMaxZ=gridSize(3)+1
        InnerMinZ=2

        OuterMaxX=gridSize(1)+2
        OuterMinX=1
        OuterMaxY=gridSize(2)+2
        OuterMinY=1
        OuterMaxZ=gridSize(3)+2
        OuterMinZ=1

        do BoundryY=InnerMinY,InnerMaxY
            do BoundryZ=InnerMinZ,InnerMaxZ
                cellContainers(OuterMinX,BoundryY,BoundryZ)%C => cellContainers(InnerMaxX,BoundryY,BoundryZ)%C
                cellContainers(OuterMaxX,BoundryY,BoundryZ)%C => cellContainers(innerMinX,BoundryY,BoundryZ)%C

                cellContainers(OuterMinX,BoundryY,BoundryZ)%Ghost = .true.
                cellContainers(OuterMaxX,BoundryY,BoundryZ)%Ghost = .true.
            end do
        end do

        do BoundryX=InnerMinX,InnerMaxX
            do BoundryY=InnerMinY,InnerMaxY
                cellContainers(BoundryX,BoundryY,OuterMinZ)%C => cellContainers(BoundryX,BoundryY,InnerMaxZ)%C
                cellContainers(BoundryX,BoundryY,OuterMaxZ)%C => cellContainers(BoundryX,BoundryY,InnerMinZ)%C

                cellContainers(BoundryX,BoundryY,OuterMinZ)%Ghost = .true.
                cellContainers(BoundryX,BoundryY,OuterMaxZ)%Ghost = .true.
            end do
        end do

        do BoundryX=InnerMinX,InnerMaxX
            do BoundryZ=InnerMinZ,InnerMaxZ
                cellContainers(BoundryX,OuterMinY,BoundryZ)%C => cellContainers(BoundryX,InnerMaxY,BoundryZ)%C
                cellContainers(BoundryX,OuterMaxY,BoundryZ)%C => cellContainers(BoundryX,InnerMinY,BoundryZ)%C

                cellContainers(BoundryX,OuterMinY,BoundryZ)%Ghost = .true.
                cellContainers(BoundryX,OuterMaxY,BoundryZ)%Ghost = .true.
            end do
        end do

        do BoundryX=InnerMinX,InnerMaxX
            cellContainers(BoundryX,OuterMinY,OuterMinZ)%C => cellContainers(BoundryX,InnerMaxY,InnerMaxZ)%C
            cellContainers(BoundryX,OuterMaxY,OuterMaxZ)%C => cellContainers(BoundryX,InnerMinY,InnerMinZ)%C

            cellContainers(BoundryX,OuterMinY,OuterMaxZ)%C => cellContainers(BoundryX,InnerMaxY,InnerMinZ)%C
            cellContainers(BoundryX,OuterMaxY,OuterMinZ)%C => cellContainers(BoundryX,InnerMinY,InnerMaxZ)%C

            cellContainers(BoundryX,OuterMinY,OuterMinZ)%Ghost = .true.
            cellContainers(BoundryX,OuterMaxY,OuterMaxZ)%Ghost = .true.

            cellContainers(BoundryX,OuterMinY,OuterMaxZ)%Ghost = .true.
            cellContainers(BoundryX,OuterMaxY,OuterMinZ)%Ghost = .true.
        end do

        do BoundryY=InnerMinY,InnerMaxY
            cellContainers(OuterMinX,BoundryY,OuterMinZ)%C => cellContainers(InnerMaxX,BoundryY,InnerMaxZ)%C
            cellContainers(OuterMaxX,BoundryY,OuterMaxZ)%C => cellContainers(InnerMinX,BoundryY,InnerMinZ)%C

            cellContainers(OuterMinX,BoundryY,OuterMaxZ)%C => cellContainers(InnerMaxX,BoundryY,InnerMinZ)%C
            cellContainers(OuterMaxX,BoundryY,OuterMinZ)%C => cellContainers(InnerMinX,BoundryY,InnerMaxZ)%C

            cellContainers(OuterMinX,BoundryY,OuterMinZ)%Ghost = .true.
            cellContainers(OuterMaxX,BoundryY,OuterMaxZ)%Ghost = .true.

            cellContainers(OuterMinX,BoundryY,OuterMaxZ)%Ghost = .true.
            cellContainers(OuterMaxX,BoundryY,OuterMinZ)%Ghost = .true.
        end do

        do BoundryZ=InnerMinZ,InnerMaxZ
            cellContainers(OuterMinX,OuterMinY,BoundryZ)%C => cellContainers(InnerMaxX,InnerMaxY,BoundryZ)%C
            cellContainers(OuterMaxX,OuterMaxY,BoundryZ)%C => cellContainers(InnerMinX,InnerMinY,BoundryZ)%C

            cellContainers(OuterMinX,OuterMaxY,BoundryZ)%C => cellContainers(InnerMaxX,InnerMinY,BoundryZ)%C
            cellContainers(OuterMaxX,OuterMinY,BoundryZ)%C => cellContainers(InnerMinX,InnerMaxY,BoundryZ)%C

            cellContainers(OuterMinX,OuterMinY,BoundryZ)%Ghost = .true.
            cellContainers(OuterMaxX,OuterMaxY,BoundryZ)%Ghost = .true.

            cellContainers(OuterMinX,OuterMaxY,BoundryZ)%Ghost = .true.
            cellContainers(OuterMaxX,OuterMinY,BoundryZ)%Ghost = .true.
        end do

        cellContainers(OuterMinX,OuterMinY,OuterMinZ)%C => cellContainers(InnerMaxX,InnerMaxY,InnerMaxZ)%C

        cellContainers(OuterMaxX,OuterMinY,OuterMinZ)%C => cellContainers(InnerMinX,InnerMaxY,InnerMaxZ)%C
        cellContainers(OuterMinX,OuterMaxY,OuterMinZ)%C => cellContainers(InnerMaxX,InnerMinY,InnerMaxZ)%C
        cellContainers(OuterMinX,OuterMinY,OuterMaxZ)%C => cellContainers(InnerMaxX,InnerMaxY,InnerMinZ)%C

        cellContainers(OuterMaxX,OuterMaxY,OuterMinZ)%C => cellContainers(InnerMinX,InnerMinY,InnerMaxZ)%C
        cellContainers(OuterMaxX,OuterMinY,OuterMaxZ)%C => cellContainers(InnerMinX,InnerMaxY,InnerMinZ)%C
        cellContainers(OuterMinX,OuterMaxY,OuterMaxZ)%C => cellContainers(InnerMaxX,InnerMinY,InnerMinZ)%C

        cellContainers(OuterMaxX,OuterMaxY,OuterMaxZ)%C => cellContainers(InnerMinX,InnerMinY,InnerMinZ)%C

        cellContainers(OuterMinX,OuterMinY,OuterMinZ)%Ghost = .true.

        cellContainers(OuterMaxX,OuterMinY,OuterMinZ)%Ghost = .true.
        cellContainers(OuterMinX,OuterMaxY,OuterMinZ)%Ghost = .true.
        cellContainers(OuterMinX,OuterMinY,OuterMaxZ)%Ghost = .true.

        cellContainers(OuterMaxX,OuterMaxY,OuterMinZ)%Ghost = .true.
        cellContainers(OuterMaxX,OuterMinY,OuterMaxZ)%Ghost = .true.
        cellContainers(OuterMinX,OuterMaxY,OuterMaxZ)%Ghost = .true.

        cellContainers(OuterMaxX,OuterMaxY,OuterMaxZ)%Ghost = .true.

    end subroutine DistributeParticlesToGrid








end module lib_GridAlgorithms
