module class_VelocityBootstrapperBase
    use class_Particle
    implicit none

    type,abstract :: VelocityBootstrapperBase
    contains
        procedure(LoadVelocityIntoAnArray) ,nopass,deferred:: LoadVelocityIntoAnArray
    end type

    abstract interface
        subroutine LoadVelocityIntoAnArray (particles)
            import
            type(Particle) ,allocatable ,intent(in):: particles(:)

        end subroutine LoadVelocityIntoAnArray
    end interface

contains



end module class_VelocityBootstrapperBase
