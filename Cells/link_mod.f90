!! link.f90

module link_mod
    private
    public :: link,createlink
    type link
        private
        class(*), pointer :: value => null() ! value stored in link
        type(link), pointer :: next => null()! next link in list
    contains
        procedure :: getValue    ! return value pointer
        procedure :: printLinks  ! print linked list starting with this link
        procedure :: nextLink    ! return next pointer
        procedure :: setNextLink ! set next pointer
    end type link

    interface createlink
        module procedure createlink ! construct/initialize a link
    end interface

contains

    function nextLink(this)
        class(link) :: this
        class(link), pointer :: nextLink
        nextLink => this%next
    end function nextLink

    subroutine setNextLink(this,next)
        class(link) :: this
        class(link), pointer :: next
        this%next => next
    end subroutine setNextLink

    function getValue(this)
        class(link) :: this
        class(*), pointer :: getValue
        getValue => this%value
    end function getValue

    subroutine printLink(this)
        class(link) :: this

        select type(v => this%value)
            type is (integer)
            print *, v
            type is (character(*))
            print *, v(1:1)
            type is (real)
            print *, v
            class default
            stop 'printLink: unexepected type for link'
        end select

    end subroutine printLink

    subroutine printLinks(this)
        class(link) :: this
        class(link), pointer :: curr

        call printLink(this)
        curr => this%next
        do while(associated(curr))
            call printLink(curr)
            curr => curr%next
        end do

    end subroutine

    function createlink(value, next)
        class(link),pointer :: createlink
        class(*) :: value
        class(link), pointer :: next
        allocate(createlink)
        createlink%next => next
        allocate(createlink%value, source=value)

    end function createlink

end module link_mod
