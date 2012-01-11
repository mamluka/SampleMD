module class_DataLoader
    use class_Particle
    implicit none
    public :: DataLoader

    type DataLoader
    contains
        procedure,nopass :: LoadParticlesIntoAnArray
    end type DataLoader
contains

    function LoadParticlesIntoAnArray(filename) result (pArray)
        character (len=*) :: filename
        integer :: inputStatus
        character :: type
        real :: x,y,z
        integer :: totalNumberOfAtoms
        type (Particle),allocatable :: pArray(:)



        open(unit=99,file = "data.xyz",status='old',iostat = inputStatus )
        read(99,*),totalNumberOfAtoms

        allocate (pArray(totalNumberOfAtoms))

        do
            if (inputStatus /= 0) exit
            read (99,*),type,x,y,z


        end do
        close(99)




    end function

end module class_DataLoader
