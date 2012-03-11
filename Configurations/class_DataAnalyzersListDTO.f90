module class_DataAnalyzersListDTO
    implicit none

    type :: DataAnalyzersListDTO
        logical :: KineticEnergy = .false.
        logical :: Temperature = .false.
    end type
end module class_DataAnalyzersListDTO
