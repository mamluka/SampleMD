module lib_GridAlgorithms
    use class_Particle
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

end module lib_GridAlgorithms
