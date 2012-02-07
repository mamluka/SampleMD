module class_DataFileReader
    use class_Particle
    use lib_ConfigurationManager
    use class_AtomProperties
    implicit none
    public :: DataFileReader

    type DataFileReader
        integer :: TotalNumberOfAtoms
    contains
        procedure,nopass :: LoadParticlesIntoAnArray
        procedure,nopass :: LoadPArticlesUsingConfigurations
    end type DataFileReader

contains

    subroutine LoadParticlesIntoAnArray(filename,particles)
        type(Particle),allocatable :: particles(:)
        character (len=*) :: filename

        integer :: inputStatus
        character :: type
        real :: x,y,z
        integer :: totalNumberOfAtoms,atomCounter

        type(Particle) :: p
        type(AtomProperties) :: atomProperties

        open(unit=99,file = filename,status='old',iostat = inputStatus )
        read(99,*),totalNumberOfAtoms

        allocate (particles(totalNumberOfAtoms))

        atomCounter = 1
        do atomCounter = 1,totalNumberOfAtoms

            if (inputStatus /= 0) exit
            read (99,*),type,x,y,z
            p=AllocateParticleWithPosition(x,y,z)
            p%Mass=atomProperties%AtomMassInAMU(type)
            call p%GiveID(atomCounter)

            particles(atomCounter) = p

        end do
        close(99)

    end subroutine LoadParticlesIntoAnArray

    subroutine LoadPArticlesUsingConfigurations(configurations,particles)

        type(SimulationConfigurations) :: configurations
        type(Particle),allocatable :: particles(:)

        call LoadParticlesIntoAnArray(configurations%DataFilename,particles)

    end subroutine LoadPArticlesUsingConfigurations

end module class_DataFileReader
