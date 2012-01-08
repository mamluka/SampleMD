!! integerList.f90

module integer_list_mod
    use abstract_list_mod


    public :: integerList

    type,extends(list) :: integerList

      !  procedure :: addInteger                    ! add integer in list
      !  procedure :: current => currentInteger ! get integer pointed by iterator
      !  procedure :: printList => printIntegerList ! print the integer list
      !  generic :: add => addInteger
    end type integerList



!    subroutine printIntegerList(this)
!        class(integerList) :: this
!        class(*), pointer :: curr
!
!        call this%reset()
!        do while(this%moreValues())
!            curr => this%CurrentValue()
!            select type(curr)
!                type is (integer)
!                print *, curr
!            end select
!            call this%next()
!        end do
!        call this%reset()
!    end subroutine printIntegerList
!
!    subroutine addInteger(this, value)
!        class(integerList) :: this
!        integer value
!        class(*), allocatable :: v
!
!        allocate(v,source=value)
!        call this%list%add(v)
!
!    end subroutine addInteger
!
!    integer function currentInteger(this)
!        class(integerList) :: this
!        class(*), pointer :: v
!
!        v => this%currentValue()
!        select type(v)
!            type is (integer)
!            currentInteger = v
!        end select
!    end function currentInteger

end module integer_list_mod
