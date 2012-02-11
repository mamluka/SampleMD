module lib_BootstrapperManager
    use class_VelocityBootstrapperBase
    use class_RandomVelocityBootstapper
    use class_DimensionlessRandomVelocityBootstapper
    implicit none

contains

    function LoadVelocityBootstrapperByType(strapperType) result (strapper)
        character(len=*) :: strapperType
        class(VelocityBootstrapperBase),pointer :: strapper

        type(RandomVelocityBootstapper),target :: rvb
        type(DimensionlessRandomVelocityBootstapper),target :: drvb

        select case (strapperType)
            case("random")
                strapper => rvb
            case("dimensionless-random")
                strapper => drvb
            case default
                strapper => rvb
        end select

    end function



end module lib_BootstrapperManager
