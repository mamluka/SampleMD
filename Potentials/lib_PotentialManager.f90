module lib_PotentialManager
    use class_PotentialBase
    use class_LeonardJonesPotential
    use class_ArgonModifiedLeonardJonesPotential
    use lib_ConfigurationManager
    implicit none
contains

    function LoadPotentialByName(potentialName) result(potential)
        character (len=*) :: potentialName
        class(PotentialBase),pointer :: potential
        type(LeonardJonesPotential),target :: lg
        type(ArgonModifiedLeonardJonesPotential),target :: aragon

        select case (potentialName)
            case ("lg")
                potential => lg
            case ("aragon")
                potential => aragon
            case default

        end select
\
    end function LoadPotentialByName

    function LoadPotentialByConfiguration(configurations) result(potential)
        type(SimulationConfigurations) :: configurations
        class(PotentialBase),pointer :: potential

        type(LeonardJonesPotential),target :: lg
        type(ArgonModifiedLeonardJonesPotential) ,target:: argon

        select case (configurations%PotentialName)
            case ("lg")
                !allocate(lg)
                potential => lg
            case ("argon")
                !allocate(argon)
                potential => argon
            case default
                !allocate(lg)
                potential => lg
        end select

        call potential%LoadPotentialParameters(configurations%PotentialDataFile)

        if (configurations%Reducers%HasDimensionlessReduction == .true.) then
            call potential%SetReducers(configurations%Reducers)
        end if
\
    end function LoadPotentialByConfiguration





end module lib_PotentialManager
