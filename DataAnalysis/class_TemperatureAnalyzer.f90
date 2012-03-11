module class_TemperatureAnalyzer
       use class_DataAnalyzerBase
    use class_ParticlePointer
    use lib_ConfigurationManager
    use lib_Math
    implicit none

    type,extends(DataAnalyzerBase) :: TemperatureAnalyzer
    contains
        procedure :: Analyze
    end type

contains

    subroutine Analyze(this,configurations)
        class(TemperatureAnalyzer) :: this
        type(ConfigurationsDTO) :: configurations

        real :: Ek
        integer :: i
        integer :: particleCount

        real :: T

        Ek=0

        particleCount = size(this%ParticlePointers)

        do i=1,particleCount
            Ek=Ek+0.5*sum(this%ParticlePointers(i)%p%Velocity**2)*this%ParticlePointers(i)%p%Mass
        end do

        T=(2.0/3.0)*Ek*(1.0/particleCount)*configurations%Reducers%Energy/Kb

        print *,T


    end subroutine

    function CreateAnalyzer(particlePointers) result (analyzer)
        type(ParticlePointer),allocatable :: particlePointers(:)
        type(TemperatureAnalyzer),target :: target
        class(DataAnalyzerBase),pointer :: analyzer

        allocate(analyzer,source=target)
        call analyzer%LoadParticles(particlePointers)

    end function

end module class_TemperatureAnalyzer
