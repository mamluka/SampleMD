program SampleMD
    use class_RandomVelocityBootstapper
    use class_IntegrationRunnerSelector
    use lib_ConfigurationManager
    use lib_PotentialManager
    use class_DataFileReader
    use class_Grid
    implicit none

    type(IntegrationRunnerSelector) :: runnerSelector
    class(IntegrationRunnerBase) ,pointer :: runner

    type(SimulationConfigurations) :: configurations

    type(DataFileReader) :: dataReader
    type(Particle),allocatable ,target:: particles(:)
    type(Grid) :: g
    class(PotentialBase),pointer :: potential

    type(Cell),pointer :: c
    type(Particle) :: p

    integer :: I,J,K


    configurations = LoadSimulationConfigurations("mdconfig.xml")

    call dataReader%LoadParticlesUsingConfigurations(configurations,particles)
    !
    potential => LoadPotentialByName("lg")

    call g%CreateGrid(particles,potential%SizeOfGridCell())

!    do I=1,g%GridSize(1)+2
!        do J=1,g%GridSize(2)+2
!            do K=1,g%GridSize(3)+2
!
!                c=>g%GetCell(I,J,K)
!                call c%Reset()
!
!                print *,I,J,K
!                print *,c%ParticleCount()
!
!                do while (c%AreThereMoreParticles())
!                    p=c%CurrentValue()
!                    print *,p%Position(1),p%Position(2),p%Position(3)
!                    call c%Next()
!                end do
!
!                print *,"-------------------"
!
!            end do
!        end do
!    end do

!    do I=1,g%GridSize(1)+2
!        do J=1,g%GridSize(2)+2
!            do K=1,g%GridSize(3)+2
!
!                c=>g%GetCell(I,J,K)
!                call c%Reset()
!
!                print *,I,J,K,g%IsGhost(I,J,K)
!
!
!            end do
!        end do
!    end do

    runner => runnerSelector%Select("standard")

    call runner%Setup(g,potential,configurations)

    call runner%Start()



end program SampleMD


