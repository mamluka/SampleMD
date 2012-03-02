module class_DataAnalyzerBase
    use class_Particle
    use lib_ConfigurationManager
    implicit none

    public :: DataAnalyzerBase

    type,abstract :: DataAnalyzerBase
    contains
        procedure(IAnalyze),deferred :: Analyze
    end type

    abstract interface
        subroutine IAnalyze(this,particles,configurations)
            import
            class(DataAnalyzerBase) :: this
            type(Particle),allocatable :: particles
            type(SimulationConfigurations) :: configurations
        end subroutine
    end interface

end module class_DataAnalyzerBase
