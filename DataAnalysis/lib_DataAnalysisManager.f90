module lib_DataAnalysisManager
    use lib_StandatdDataAnalysis,StandardDataAnalysis=>Analyze
    implicit none

contains

    subroutine ApplyDataAnalysis(particles)
        type(ParticleLinkedList) :: particles
        call StandardDataAnalysis(particles)
    end subroutine ApplyDataAnalysis

end module lib_DataAnalysisManager
