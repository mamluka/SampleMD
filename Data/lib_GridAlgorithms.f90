module lib_GridAlgorithms
    use class_Particle
    use class_ParticleLinkedList
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

    subroutine AllocateGridByCutOffRadii(g,rc,box)
        type(ParticleLinkedList),allocatable,intent(inout) :: g(:,:,:)
        real ,intent(in) :: rc
        real ,intent(in):: box(8,3)
        real :: boxsize(3)
        integer :: numberOfXCells,numberOfYCells,numberOfZCells


        boxsize=DetermineSimulationBoxDimensions(box)

        numberOfXCells = ceiling(boxsize(1)/rc)
        numberOfYCells = ceiling(boxsize(2)/rc)
        numberOfZCells = ceiling(boxsize(3)/rc)

        print *,numberOfXCells,numberOfYCells,numberOfZCells

        allocate(g(numberOfXCells,numberOfYCells,numberOfZCells))

    end subroutine

    subroutine DistributeParticlesToGrid(g,particles,rc,box)
        type(ParticleLinkedList),allocatable,intent(inout) :: g(:,:,:)
        real ,intent(in) :: rc
        type(Particle),allocatable,target :: particles(:)
        class(Particle),pointer ::particlePointer
        integer :: i,xIndex,yIndex,zIndex
        real :: box(8,3)


        open(unit=98,file="cellsIndex.data")

        do i=1,size(particles)
            xIndex = ceiling(abs(box(1,1)-particles(i)%Position(1))/rc)
            yIndex = ceiling(abs(box(1,2)-particles(i)%Position(2))/rc)
            zIndex = ceiling(abs(box(1,3)-particles(i)%Position(3))/rc)

            if (xIndex == 0)  xIndex=1
            if (yIndex == 0)  yIndex=1
            if (zIndex == 0)  zIndex=1


            write (98,*),xIndex,yIndex,zIndex


            particlePointer => particles(i)

            call g(xIndex,yIndex,zIndex)%AddValue(particlePointer)

        end do

        close(98)


    end subroutine DistributeParticlesToGrid



end module lib_GridAlgorithms
