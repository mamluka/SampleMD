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

    function DistanceBetweenParticlesWithPeriodicConditions(particleI,particleJ,box) result (d)

        type(Particle) :: particleI,particleJ
        real :: d
        real :: box(3),new(3)


!        print *,particleI%Position
!        print *,particleJ%Position
!
!        print *,particleJ%Position-box
!
!        print *,particleI%Position-(particleJ%Position-box)

        new = (particleJ%Position-box)

        d=Distance(particleI%Position,new)

    end function
end module lib_Integration
