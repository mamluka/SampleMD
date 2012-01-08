!! Link.f90

module class_ParticleLink
  use class_Particle
  private

  public :: Link
  type Link
     private
     class(*), pointer :: value => null() ! value stored in Link
     type(Link), pointer :: next => null()! next Link in list
     contains
     procedure :: Value    ! return value pointer
     procedure :: NextLink    ! return next pointer
     procedure :: SetNextLink ! set next pointer
  end type Link

  interface Link
   procedure constructor ! construct/initialize a Link
  end interface

contains

  function NextLink(this)
  type(Link) :: this
  type(Link), pointer :: NextLink
    nextLink => this%next
  end function NextLink

  subroutine SetNextLink(this,next)
  type(Link) :: this
  type(Link), pointer :: next
     this%next => next
  end subroutine SetNextLink

  function Value(this)
  type(Link) :: this
  class(Particle), pointer :: Value
  Value => this%value
  end function Value

   function constructor(value, next)
    type(Link),pointer :: constructor
    class(Particle) :: value
    type(Link), pointer :: next
    allocate(constructor)
    constructor%next => next
    allocate(constructor%value, source=value)
  end function constructor

end module class_ParticleLink
