module lib_PotentialManager
    use class_PotentialBase
    use class_LeonardJonesPotential
    implicit none
contains

    function LoadPotentialByName(potentialName) result(potential)
        character (len=*) :: potentialName
        class(PotentialBase),pointer :: potential
        type(LeonardJonesPotential),target :: lg

        select case (potentialName)
            case ("lg")
                potential => lg
            case default

        end select







    end function LoadPotentialByName

end module lib_PotentialManager
