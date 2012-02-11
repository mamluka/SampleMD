module class_VelocityBootstrapperBase
    use class_Particle
    use class_DataOptionsDTO
    implicit none

    type,abstract :: VelocityBootstrapperBase
    contains
        procedure(ILoadVelocityIntoAnArray),nopass,deferred:: LoadVelocityIntoAnArray
    end type

    abstract interface
        subroutine ILoadVelocityIntoAnArray(particles,dataOptions)
            import
            type(Particle),allocatable :: particles(:)
            type(DataOptionsDTO) :: dataOptions
        end subroutine ILoadVelocityIntoAnArray
    end interface



contains



end module class_VelocityBootstrapperBase
