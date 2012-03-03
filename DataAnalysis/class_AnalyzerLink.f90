module class_AnalyzerLink
    use class_DataAnalyzerBase

    private
    public :: AnalyzerLink,CreateAnalyzerLink

    type AnalyzerLink
        private
        class(DataAnalyzerBase), pointer :: value => null() ! value stored in AnalyzerLink
        type(AnalyzerLink), pointer :: next => null()! next AnalyzerLink in list
    contains
        procedure :: getAnalyzer   ! return value pointer
        procedure :: nextAnalyzerLink    ! return next pointer
        procedure :: setNextAnalyzerLink ! set next pointer
        procedure :: nullifyNext
    end type

contains

    function nextAnalyzerLink(this)
        class(AnalyzerLink) :: this
        type(AnalyzerLink), pointer :: nextAnalyzerLink
        if (.not. associated(this%next) ) then
            nextAnalyzerLink=>null()
        else
            nextAnalyzerLink => this%next
        endif
    end function nextAnalyzerLink

    subroutine setNextAnalyzerLink(this,next)
        class(AnalyzerLink) :: this
        type(AnalyzerLink), pointer :: next
        this%next => next
    end subroutine setNextAnalyzerLink

    function getAnalyzer(this)
        class(AnalyzerLink) :: this
        class(DataAnalyzerBase), pointer :: getAnalyzer
        getAnalyzer => this%value
    end function getAnalyzer

    function CreateAnalyzerLink(value)
        class(AnalyzerLink),pointer :: CreateAnalyzerLink
        class(DataAnalyzerBase) :: value
        type(AnalyzerLink),target :: newLink

        allocate(CreateAnalyzerLink,source=newLink)
        allocate(CreateAnalyzerLink%value, source=value)

    end function CreateAnalyzerLink

    subroutine nullifyNext(this)
        class(AnalyzerLink) :: this

        this%next => null()

    end subroutine nullifyNext

end module class_AnalyzerLink
