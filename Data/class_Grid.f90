module class_Grid
    use class_ParticleLinkedList
    use lib_GridAlgorithms
    implicit none

    public :: Grid

    type Grid
        type(ParticleLinkedList),allocatable :: G(:,:,:)
        contains

    end type Grid

    interface CreateGrid
        module procedure CreateGrid
    end interface

    contains

    function CreateGrid(particles) result(g)
        type(Grid) :: g
        type(Particle),allocatable :: particles(:)
        real :: box(8,3)






    end function CreateGrid

end module class_Grid
