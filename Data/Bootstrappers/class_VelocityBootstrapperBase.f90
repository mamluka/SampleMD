module class_VelocityBootstrapperBase
    use class_Particle
    use lib_ConfigurationManager
    implicit none

    type,abstract :: VelocityBootstrapperBase
    contains
        procedure(ILoadVelocityIntoAnArray),nopass,deferred:: LoadVelocityIntoAnArray
    end type

    abstract interface
        subroutine ILoadVelocityIntoAnArray(particles,configurations)
            import
            type(Particle),allocatable :: particles(:)
            type(ConfigurationsDTO) :: Configurations
        end subroutine ILoadVelocityIntoAnArray
    end interface



contains



end module class_VelocityBootstrapperBase
