module class_DataOptionsDTO
    implicit none

    type :: DataOptionsDTO
        integer :: TempForInitialVelocity
        character(len=:),allocatable :: BootstrapperType
        logical :: UseVelocityStrapper = .false.
    end type
end module class_DataOptionsDTO
