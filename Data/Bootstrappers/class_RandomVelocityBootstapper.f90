module class_RandomVelocityBootstapper
    use class_VelocityBootstrapperBase
    implicit none

    private

    public :: RandomVelocityBootstapper

    type,extends(VelocityBootstrapperBase) :: RandomVelocityBootstapper
        contains
        procedure,nopass ::LoadVelocityIntoAnArray => LoadRandomVelocityIntoAnArray
    end type

    contains

    subroutine LoadRandomVelocityIntoAnArray(particles)
        type(Particle),allocatable :: particles(:)






    end subroutine LoadRandomVelocityIntoAnArray

end module class_RandomVelocityBootstapper
