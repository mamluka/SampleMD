module class_KineticEnergyAnalyzer
    use class_DataAnalyzerBase
    use class_Particle
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

    function CreateAnalyzer(particles) result (analyzer)
        type(Particle),allocatable :: particles(:)
        type(KineticEnergyAnalyzer),target :: target
        class(DataAnalyzerBase),pointer :: analyzer

        particles(1)%Mass = 7

        allocate(analyzer,source=target)

        call analyzer%LoadParticles(particles)

    end function

end module class_KineticEnergyAnalyzer
