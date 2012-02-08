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

    abstract interface
        subroutine IMapParticles(p,configurations)
            import
            type(Particle) :: p
            type(SimulationConfigurations) :: configurations
        end subroutine IMapParticles
    end interface

contains

    subroutine LoadParticlesIntoAnArray(filename,particles)
        type(Particle),allocatable :: particles(:)
        character (len=*) :: filename

        integer :: inputStatus
        character(len=2) :: type
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

        if (configurations%Reducers%HasDimensionlessReduction == .true. ) then
           call ForEachParticle(particles,ReduceToDimensionlessParameters,configurations)
        end if

    end subroutine LoadParticlesUsingConfigurations

    subroutine ReduceToDimensionlessParameters(p,configurations)
        type(Particle) :: p
        type(SimulationConfigurations) :: configurations

        p%Position = p%Position/configurations%Reducers%Length
        p%Mass = p%Mass/configurations%Reducers%Mass

    end subroutine ReduceToDimensionlessParameters

    subroutine ForEachParticle(particles,proc,configurations)
        type(Particle) :: particles(:)
        procedure(IMapParticles) :: proc
        type(SimulationConfigurations) :: configurations
        type(SimulationConfigurations),pointer :: configurationsPointer

        integer :: i

        do i=1,size(particles)
            call proc(particles(i),configurations)
        end do

    end subroutine ForEachParticle



end module class_DataFileReader
