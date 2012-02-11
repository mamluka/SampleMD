module class_DimensionlessRandomVelocityBootstapper
    use class_VelocityBootstrapperBase
    implicit none

    private

    public :: DimensionlessRandomVelocityBootstapper

    type,extends(VelocityBootstrapperBase) :: DimensionlessRandomVelocityBootstapper
        contains
        procedure,nopass ::LoadVelocityIntoAnArray
    end type

    contains

    subroutine LoadVelocityIntoAnArray(particles,dataOptions)
        type(Particle),allocatable :: particles(:)
        type(DataOptionsDTO) :: dataOptions



    end subroutine LoadVelocityIntoAnArray

end module class_DimensionlessRandomVelocityBootstapper
