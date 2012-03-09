module class_DataFileReader
    use class_Particle
    use lib_ConfigurationManager
    use class_AtomProperties
    use class_DataOptionsDTO
    use lib_BootstrapperManager
    use class_ParticlePointer
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
            type(ConfigurationsDTO) :: configurations
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

    subroutine LoadParticlesUsingConfigurations(configurations,particlePointers)

        type(ConfigurationsDTO) :: configurations
        type(ParticlePointer),allocatable :: particlePointers(:)
        type(Particle),allocatable :: particles(:)

        integer :: i,arraySize

        call LoadParticlesIntoAnArray(configurations%SimulationConfigurations%DataFilename,particles)

        if (configurations%Reducers%HasDimensionlessReduction == .true. ) then
            call ForEachParticle(particles,ReduceToDimensionlessParameters,configurations)
        end if

        if (configurations%DataOptions%UseVelocityStrapper == .true. ) then
            call BootstrapVelocity(particles,configurations)
        end if

        arraySize = size(particles)

        allocate(particlePointers(arraySize))

        do i=1,size(particles)
            allocate(particlePointers(i)%p,source=particles(i))
        end do

    end subroutine LoadParticlesUsingConfigurations

    subroutine ReduceToDimensionlessParameters(p,configurations)
        type(Particle) :: p
        type(ConfigurationsDTO) :: configurations

        p%Position = p%Position/configurations%Reducers%Length
        p%Mass = p%Mass/configurations%Reducers%Mass

    end subroutine ReduceToDimensionlessParameters

    subroutine ForEachParticle(particles,proc,configurations)
        type(Particle) :: particles(:)
        procedure(IMapParticles) :: proc
        type(ConfigurationsDTO) :: configurations
        type(ConfigurationsDTO),pointer :: configurationsPointer

        integer :: i

        do i=1,size(particles)
            call proc(particles(i),configurations)
        end do

    end subroutine ForEachParticle

    subroutine BootstrapVelocity(particles,configurations)
        type(Particle),allocatable :: particles(:)
        type(ConfigurationsDTO) :: configurations

        class(VelocityBootstrapperBase),pointer :: strapper

        strapper => LoadVelocityBootstrapperByType(configurations%dataOptions%BootstrapperType)

        call strapper%LoadVelocityIntoAnArray(particles,configurations)



    end subroutine BootstrapVelocity



end module class_DataFileReader
