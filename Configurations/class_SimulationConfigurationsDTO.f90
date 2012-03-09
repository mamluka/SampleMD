module class_SimulationConfigurationsDTO
    implicit none

    type :: SimulationConfigurationsDTO
        real :: TimeStep
        real :: EndOfSimulation
        integer :: Dimension
        character (len=:),allocatable :: PotentialName
        character (len=:),allocatable :: DataFilename
    end type
end module class_SimulationConfigurationsDTO
