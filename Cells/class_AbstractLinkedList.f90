!! abstract_list.f90

module class_AbstractLinkedList
  use class_ParticleLink
  private
  public :: list
  type, abstract :: list
     private
     class(link),pointer :: firstLink => null() ! first link in list
     class(link),pointer :: lastLink => null()  ! last link in list
     class(link),pointer :: currLink => null()  ! list iterator
   contains
     procedure, non_overridable :: addValue     ! add class(*) to list
     procedure, non_overridable :: firstValue   ! value of first link in list
     procedure, non_overridable :: reset        ! reset list iterator
     procedure, non_overridable :: next         ! increment list iterator
     procedure, non_overridable :: currentValue ! get value from currLink
     procedure, non_overridable :: moreValues   ! more values for iterator?
     generic :: add => addValue
     procedure(printValues), deferred :: printList ! prints values in list
  end type list

  abstract interface
  subroutine printValues(this)
    import list
    class(list) :: this
  end subroutine
  end interface

contains

  subroutine addValue(this, value)
    class(list) :: this
    class(Particle), value
    class(link), pointer :: newLink

    if (.not. associated(this%firstLink)) then
       this%firstLink => link(value, this%firstLink)
       this%lastLink => this%firstLink
    else
       newLink => link(value, this%lastLink%nextLink())
       call this%lastLink%setNextLink(newLink)
       this%lastLink => newLink
    end if

  end subroutine addValue

  function firstValue(this)
    class(list) :: this
    class(Particle), pointer :: firstValue

    firstValue => this%firstLink%getValue()

  end function firstValue

  function currentValue(this)
   class(list) :: this
   class(Particle), pointer :: CurrentValue
   CurrentValue => this%currLink%getValue()
  end function CurrentValue

  subroutine next(this)
   class(list) :: this
   this%currLink => this%currLink%nextLink()
  end subroutine next

  function moreValues(this)
   class(list) :: this
   logical moreValues
   moreValues = associated(this%currLink)
  end function moreValues

  subroutine reset(this)
  class(list) :: this
    this%currLink => this%firstLink
  end subroutine reset

end module class_AbstractLinkedList
