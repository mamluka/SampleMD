module class_DataFileReader
    use class_Particle
    implicit none
    public :: DataFileReader

    type DataFileReader
        integer :: TotalNumberOfAtoms
    contains
        procedure,nopass :: LoadParticlesIntoAnArray
    end type DataFileReader

contains

    subroutine LoadParticlesIntoAnArray(filename,particles)
        type(Particle),allocatable :: particles(:)
        character (len=*) :: filename

        integer :: inputStatus
        character :: type
        real :: x,y,z
        integer :: totalNumberOfAtoms,atomCounter

        open(unit=99,file = filename,status='old',iostat = inputStatus )
        read(99,*),totalNumberOfAtoms

        allocate (particles(totalNumberOfAtoms))

        atomCounter = 1
        do atomCounter = 1,totalNumberOfAtoms

            if (inputStatus /= 0) exit
            read (99,*),type,x,y,z
            particles(atomCounter) = AllocateParticleWithPosition(x,y,z)

        end do
        close(99)

    end subroutine LoadParticlesIntoAnArray

end module class_DataFileReader
