module lib_Integration
    use lib_Math
    use class_Particle
    implicit none

contains

    function DistanceBetweenParticles(particleI,particleJ) result (d)
        type(Particle) :: particleI,particleJ
        real :: d

        d=Distance(particleI%Position,particleJ%Position)

    end function

    function DistanceBetweenParticlesWithPeriodicConditions(particleI,particleJ,CellPosition,NeighborPosition,box) result (d)

        type(Particle) :: particleI,particleJ
        real :: d
        real :: box(3),new(3)
        integer :: CellPosition(3),NeighborPosition(3)

        new = particleI%Position

        if (abs(CellPosition(1)-NeighborPosition(1)) > 1 ) then
            new(1) = particleJ%Position(1) + (CellPosition(1)-NeighborPosition(1))/abs(CellPosition(1)-NeighborPosition(1))*box(1)
        end if

        if (abs(CellPosition(2)-NeighborPosition(2)) > 1 ) then
            new(2) = particleJ%Position(2) + (CellPosition(2)-NeighborPosition(2))/abs(CellPosition(2)-NeighborPosition(2))*box(2)
        end if

        if (abs(CellPosition(3)-NeighborPosition(3)) > 1 ) then
            new(3)= particleJ%Position(3) + (CellPosition(3)-NeighborPosition(3))/abs(CellPosition(3)-NeighborPosition(3))*box(3)
        end if




        !new = (particleJ%Position-box)

        d=Distance(particleI%Position,new)

    end function
end module lib_Integration
