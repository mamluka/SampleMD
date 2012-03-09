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

    subroutine LoadVelocityIntoAnArray(particles,configurations)
        type(Particle),allocatable :: particles(:)
        type(ConfigurationsDTO) :: Configurations

    end subroutine LoadVelocityIntoAnArray

end module class_RandomVelocityBootstapper
