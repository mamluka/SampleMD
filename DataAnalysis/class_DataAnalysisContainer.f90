module class_DataAnalysisContainer
    use class_DataAnalyzerBase
    use class_AnalyzerLink
    implicit none

    type :: DataAnalysisContainer
        private
        type(AnalyzerLink),pointer :: firstLink => null() ! first link in list
        type(AnalyzerLink),pointer :: lastLink => null()  ! last link in list
        type(AnalyzerLink),pointer :: currLink => null()  ! list iterator
    contains
        procedure :: AddAnalyzer
    end type

    contains

    subroutine AddAnalyzer(this, value)
        class(DataAnalysisContainer) :: this
        class(DataAnalyzerBase) :: value
        type(AnalyzerLink), pointer :: newLink

        type(AnalyzerLink) ,pointer :: firstLink,nextLink

        if (.not. associated(this%firstLink)) then
            this%firstLink => CreateAnalyzerLink(value)
            this%lastLink => this%firstLink
            this%currLink => this%firstLink
        else
            newLink => CreateAnalyzerLink(value)
            call this%lastLink%setNextAnalyzerLink(newLink)
            this%lastLink => newLink
        end if

    end subroutine AddAnalyzer

    subroutine Next(this)
        class(DataAnalysisContainer) :: this
        this%currLink => this%currLink%nextAnalyzerLink()


    end subroutine Next

    function AreThereMoreAnalyzers(this)
        class(DataAnalysisContainer) :: this
        logical AreThereMoreAnalyzers
        AreThereMoreAnalyzers = associated(this%currLink)
    end function AreThereMoreAnalyzers



    subroutine Reset(this)
        class(DataAnalysisContainer) :: this
        this%currLink => this%firstLink
    end subroutine Reset
end module class_DataAnalysisContainer
