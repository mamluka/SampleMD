module class_Grid
    use class_ParticleLinkedList
    use lib_GridAlgorithms
    implicit none

    public :: Grid

    type Grid
        type(ParticleLinkedList),allocatable :: G(:,:,:)
        contains
        procedure :: CreateGrid
    end type Grid

    contains

    subroutine CreateGrid(this,particles)
        class(Grid) :: this

        type(Particle),allocatable :: particles(:)


        real :: rc
        real ::box(8,3)




        box = DetermineSimulationBoxCoordinates(particles)

        call AllocateGridByCutOffRadii(this%G,rc,box)

        call DistributeParticlesToGrid(this%G,particles,rc,box)

    end subroutine CreateGrid

end module class_Grid
