module class_KineticEnergyAnalyzer
    use class_DataAnalyzerBase
    use class_ParticlePointer
    use lib_ConfigurationManager
    implicit none

    type,extends(DataAnalyzerBase) :: KineticEnergyAnalyzer
    contains
        procedure :: Analyze
    end type

contains

    subroutine Analyze(this,configurations)
        class(KineticEnergyAnalyzer) :: this
        type(SimulationConfigurations) :: configurations

        real :: Ek
        integer :: i

        Ek=0

        do i=1,size(this%ParticlePointers)
            Ek=Ek+0.5*sum(this%ParticlePointers(i)%p%Position**2)*this%ParticlePointers(i)%p%Mass
        end do

        print *,Ek

    end subroutine

    function CreateAnalyzer(particlePointers) result (analyzer)
        type(ParticlePointer),allocatable :: particlePointers(:)
        type(KineticEnergyAnalyzer),target :: target
        class(DataAnalyzerBase),pointer :: analyzer

        allocate(analyzer,source=target)

        call analyzer%LoadParticles(particlePointers)

    end function

end module class_KineticEnergyAnalyzer
