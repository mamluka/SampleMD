module class_RandomVelocityBootstapper
    use class_VelocityBootstrapperBase
    implicit none

    private

    public :: RandomVelocityBootstapper

    type,extends(VelocityBootstrapperBase) :: RandomVelocityBootstapper
    contains
        procedure,nopass ::LoadVelocityIntoAnArray
    end type

contains

    subroutine LoadVelocityIntoAnArray(particles,dataOptions)
        type(Particle),allocatable :: particles(:)
        type(DataOptionsDTO) :: dataOptions

    end subroutine LoadVelocityIntoAnArray

end module class_RandomVelocityBootstapper
