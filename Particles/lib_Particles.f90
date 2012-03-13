module lib_Particles
    use class_ParticlePointer
    implicit none
    contains

    subroutine CopyParticlePointer(copiedFrom,copiedTo)
        type(ParticlePointer),allocatable,target:: copiedFrom(:)
        type(ParticlePointer),allocatable:: copiedTo(:)

        integer :: arraySize,i

        arraySize = size(copiedFrom)

        allocate(copiedTo(arraySize))

        do i=1,arraySize
            copiedTo(i)%p=>copiedFrom(i)%p
        end do
    end subroutine
end module lib_Particles
