module class_ErrorHandler
    implicit none
    contains

    subroutine die(str)
        character(len=*), intent(in), optional   :: str
        if (present(str)) then
            write(unit=0,fmt="(a)") trim(str)
        endif
        write(unit=0,fmt="(a)") "Stopping Program"
        stop
    end subroutine die

end module class_ErrorHandler
