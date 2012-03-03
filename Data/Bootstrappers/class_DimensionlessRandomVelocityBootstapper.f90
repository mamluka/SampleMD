module class_DimensionlessRandomVelocityBootstapper
    use class_VelocityBootstrapperBase
    use lib_ConfigurationManager
    use lib_Math
    implicit none

    private

    public :: DimensionlessRandomVelocityBootstapper

    type,extends(VelocityBootstrapperBase) :: DimensionlessRandomVelocityBootstapper
        contains
        procedure,nopass ::LoadVelocityIntoAnArray
    end type

    contains

    subroutine LoadVelocityIntoAnArray(particles,configurations)
        type(Particle),allocatable :: particles(:)
        type(SimulationConfigurations) :: configurations

        real :: factor

        real :: T,m,timeReducer,lengthReducer,massReducer

        integer :: i

        real,allocatable:: velocityTempArray(:)

        allocate(velocityTempArray(3))

        timeReducer = configurations%Reducers%Time * 1E-12
        lengthReducer = configurations%Reducers%Length * 1E-10
        massReducer = configurations%Reducers%Mass

        T=configurations%DataOptions%TempForInitialVelocity

        call InitializeRandomNumber()

        do i=1,size(particles)
            m=particles(i)%Mass*massReducer

            factor = sqrt(Kb*T/(m*amu))*(timeReducer/lengthReducer)
            velocityTempArray = factor*GaussDeviateFor3DVector()
            particles(i)%Velocity = velocityTempArray
        end do

    end subroutine LoadVelocityIntoAnArray

end module class_DimensionlessRandomVelocityBootstapper
