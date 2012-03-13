module lib_DataAnalysis
    use lib_Math
    use class_ParticlePointer
    implicit none
    contains

    function CalculateTemperature(particlePointers,energyReducer) result(T)
        real :: energyReducer
        type(ParticlePointer),allocatable :: particlePointers(:)

        real :: Ek
        integer :: i
        integer :: particleCount

        real :: T

        Ek=0

        particleCount = size(ParticlePointers)

        do i=1,particleCount
            Ek=Ek+0.5*sum(ParticlePointers(i)%p%Velocity**2)*ParticlePointers(i)%p%Mass
        end do

        T=(2.0/3.0)*Ek*(1.0/particleCount)*energyReducer/Kb

    end function
end module lib_DataAnalysis
