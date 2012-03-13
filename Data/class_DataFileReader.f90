module class_DataFileReader
    use class_Particle
    use lib_ConfigurationManager
    use class_AtomProperties
    use class_DataOptionsDTO
    use lib_BootstrapperManager
    use class_ParticlePointer
    use lib_GridAlgorithms
    implicit none
    public :: DataFileReader

    type DataFileReader
        integer :: TotalNumberOfAtoms
        real :: SimulationBoxSize(3)
        real :: SimulationBoxCoordinates(8,3)
    contains
        procedure :: LoadParticlesIntoAnArray
        procedure :: LoadPArticlesUsingConfigurations
    end type DataFileReader

    abstract interface
        subroutine IMapParticles(p,configurations)
            import
            type(Particle) :: p
            type(ConfigurationsDTO) :: configurations
        end subroutine IMapParticles
    end interface

contains

    subroutine LoadParticlesIntoAnArray(this,filename,particles)
        class(DataFileReader) :: this
        type(Particle),allocatable :: particles(:)
        type(Particle),allocatable :: particlesHolder(:)
        character (len=*) :: filename

        integer :: inputStatus
        character(len=2) :: type
        real :: x,y,z
        integer :: totalNumberOfAtoms,atomCounter,i,finelCounter

        type(Particle) :: p
        type(AtomProperties) :: atomProperties
        real :: box(8,3)
        real :: xVector,yVector,zVector

        open(unit=99,file = filename,status='old',iostat = inputStatus )
        read(99,*),totalNumberOfAtoms

        allocate (particlesHolder(totalNumberOfAtoms))

        atomCounter = 1
        do atomCounter = 1,totalNumberOfAtoms

            if (inputStatus /= 0) exit
            read (99,*),type,x,y,z
            p=AllocateParticleWithPosition(x,y,z)

            p%Mass=atomProperties%AtomMassInAMU(type)

            call p%GiveID(atomCounter)

            particlesHolder(atomCounter) = p

        end do
        close(99)

        box = DetermineSimulationBoxCoordinates(particlesHolder)
        this%SimulationBoxCoordinates = box
        this%SimulationBoxSize = DetermineSimulationBoxDimensions(box)

        finelCounter = 0
        do i=1,totalNumberOfAtoms

            xVector = abs(box(1,1)-particlesHolder(i)%Position(1))
            yVector = abs(box(1,2)-particlesHolder(i)%Position(2))
            zVector = abs(box(1,3)-particlesHolder(i)%Position(3))

            if ( (xVector .ge. this%SimulationBoxSize(1)) .or. (yVector .ge. this%SimulationBoxSize(2)) .or. (zVector .ge. this%SimulationBoxSize(3)) ) then
                cycle
            end if

            finelCounter = finelCounter+1
        end do

        allocate (particles(finelCounter))

        finelCounter=1
        do i=1,totalNumberOfAtoms

            xVector = abs(box(1,1)-particlesHolder(i)%Position(1))
            yVector = abs(box(1,2)-particlesHolder(i)%Position(2))
            zVector = abs(box(1,3)-particlesHolder(i)%Position(3))

            if ( (xVector .ge. this%SimulationBoxSize(1)) .or. (yVector .ge. this%SimulationBoxSize(2)) .or. (zVector .ge. this%SimulationBoxSize(3)) ) then
                cycle
            end if

            particles(finelCounter) = particlesHolder(i)

            finelCounter=finelCounter+1
        end do

    end subroutine LoadParticlesIntoAnArray

    subroutine LoadParticlesUsingConfigurations(this,configurations,particlePointers)
        class(DataFileReader) :: this
        type(ConfigurationsDTO) :: configurations
        type(ParticlePointer),allocatable :: particlePointers(:)
        type(Particle),allocatable :: particles(:)

        integer :: i,arraySize

        call this%LoadParticlesIntoAnArray(configurations%SimulationConfigurations%DataFilename,particles)

        if (configurations%Reducers%HasDimensionlessReduction == .true. ) then
            call ForEachParticle(particles,ReduceToDimensionlessParameters,configurations)
            this%SimulationBoxCoordinates = this%SimulationBoxCoordinates/configurations%Reducers%Length
            this%SimulationBoxSize = this%SimulationBoxSize/configurations%Reducers%Length
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
