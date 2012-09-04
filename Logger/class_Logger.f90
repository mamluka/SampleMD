module class_Logger
    implicit none


    type :: FileLogger
        contains
        procedure :: LogText
        procedure :: LogTextWithReal
    end type

    contains

    subroutine LogText(this,filename,text)
        class(FileLogger) :: this
        character (len=*) :: filename
        character (len=*) :: text
        logical :: isExists

        inquire(file=filename,exist=isExists)
        if (isExists == .true. ) then
            open(unit=90,file=filename,form='formatted',position='append',status='old')
        else
            open(unit=90,file=filename,form='formatted',position='append',status='new')
        endif

       write (90,'(A)'),text

        close(90)
    end subroutine

    subroutine LogTextWithReal(this,filename,text,realNumber)
        class(FileLogger) :: this
        character (len=*) :: filename
        character (len=*) :: text
        real :: realNumber
        logical :: isExists


        inquire(file=filename,exist=isExists)
        if (isExists == .true. ) then
            open(unit=90,file=filename,form='formatted',position='append',status='old')
        else
            open(unit=90,file=filename,form='formatted',position='append',status='new')
        endif

       write (90,'(A,F15.8)'),text,realnumber

        close(90)
    end subroutine


end module class_Logger
