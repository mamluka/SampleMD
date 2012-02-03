module lib_DataAnalysisManager
    use lib_StandatdDataAnalysis,StandardDataAnalysis=>Analyze
    implicit none

contains

    subroutine ApplyDataAnalysis(c)
        type(Cell) :: c
        call StandardDataAnalysis(c)
    end subroutine ApplyDataAnalysis

end module lib_DataAnalysisManager
