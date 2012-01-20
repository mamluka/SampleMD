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

    function CreateGrid(particles,rc) result(g)
        type(Grid) :: g
        real :: rc
        type(Particle),allocatable :: particles(:)
        real ::box(8,3)

        box = DetermineSimulationBoxCoordinates(particles)

        call AllocateGridByCutOffRadii(g%G,rc,box)

        call DistributeParticlesToGrid(g%G,particles,rc,box)

    end function CreateGrid

end module class_Grid
