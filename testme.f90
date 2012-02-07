module testme
    use class_Particle
    implicit none
contains

    subroutine A(p)
        type(Particle),pointer :: p
        print *,p%Mass
    end subroutine A
end module testme
