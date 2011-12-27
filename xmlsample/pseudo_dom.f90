program pseudo_dom
!
! Move towards DOM-like routines
! for the pseudo schema
! See details in m_psdom.f90

use m_pseudo_types
use m_psdom

! Module
  use flib_dom

type(pseudo_t), target :: pseudo
type(grid_t)           :: global_grid
!
type(string)           :: s         ! to avoid memory leaks

! Pointers to make it easier to manage the data
!
type(header_t),  pointer   :: hp
type(vps_t),  pointer      :: pp

  type(fnode), pointer          :: myDoc
  type(fnode), pointer          :: myNode, np
  type(fnodeList), pointer      :: myList

  integer :: npseudos, i
  character(len=200)  :: value      ! Could be larger, or made into a string

! Parse
! No constructor method - this is fortran !
  myDoc => parsefile("pseudo.xml")  !  ,verbose=.true.)
  print *, "Number of active nodes: ", getNumberofAllocatedNodes()

!  call dumpTree(myDoc)
  print *, "Normalizing...(can take long if big file --- not really needed)"
  call normalize(myDoc)
!  call dumpTree(myDoc)

  print *, "Number of active nodes: ", getNumberofAllocatedNodes()  

!----------------------------------------------------------  

  myList => getElementsByTagName(myDoc, "pseudo")
  if (getLength(myList) == 0) then
     call die("Did not found any pseudo elements...")
  endif
  myNode => item(myList, 0)

  value = getAttribute(myNode,"version")
  if (value == "0.5") then
     print *, "Processing a PSEUDO version 0.5 XML file"
     pseudo%npots = 0
     global_grid%npts = 0
  else
     print *, "Can only work with PSEUDO version 0.5 XML files"
     STOP
  endif

  global_grid%npts = 0          ! To flag absence of global grid info
  myList => getChildNodes(myNode)
  do i=0, getLength(myList) - 1
     np => item(myList,i)
     s = getNodeName(np)
     if (s == "grid") then
        print *, "This file has a global grid... "
        call getGrid(np,global_grid)
        exit
     endif
  enddo
!
! Header
!
  myList => getElementsByTagName(myDoc, "header")
  if (getLength(myList) == 0) then
     call die("Did not found any header elements...")
  endif
  myNode => item(myList, 0)
  print *, "Processing header..."
         hp => pseudo%header
         
         hp%symbol = getAttribute(myNode,"symbol")
         if (hp%symbol == "" ) call die("Cannot determine atomic symbol")

         value = getAttribute(myNode,"zval")
         if (value == "") call die("Cannot determine zval")
         read(unit=value,fmt=*) hp%zval
!
         hp%creator = getAttribute(myNode,"creator")
         if (hp%creator == "" ) hp%creator="unknown"

         hp%flavor = getAttribute(myNode,"flavor")
         if (hp%flavor == "" ) hp%flavor="unknown"

         value = getAttribute(myNode,"relativistic")
         if (value == "") hp%relativistic = .false.
         hp%relativistic = (value == "yes")

         value = getAttribute(myNode,"polarized")
         if (value == "") hp%polarized = .false.
         hp%polarized = (value == "yes")

         hp%core_corrections = getAttribute(myNode,"core-corrections")
         if (hp%core_corrections == "" ) hp%core_corrections="nc"

!
!  Valence charge
!
  myList => getElementsByTagName(myDoc, "valence-charge")
  if (getLength(myList) == 0) then
     call die("Did not found the valence charge ...")
  endif
  np => item(myList,0)
  if (associated(np)) then
     print *, "Processing valence charge..."
   !
   !  Get the data (and possible private grid)
   !
     call getRadialFunction(np,global_grid,pseudo%valence_charge)
  endif

!  Core charge
!
  myList => getElementsByTagName(myDoc, "pseudocore-charge")
     np => item(myList,0)
     if (associated(np)) then
        print *, "Processing core charge..."
   !
   !  Get the data (and possible private grid)
   !
     call getRadialFunction(np,global_grid,pseudo%core_charge)
  endif

!
! Semilocal Pseudos
!
  myList => getElementsByTagName(myDoc, "semilocal")
  if (getLength(myList) == 0) then
     call die("Did not found the semilocal element...")
  endif
  np => item(myList, 0)
  if (associated(np)) then
        print *, "Processing semilocal..."

         value = getAttribute(np,"npots-down")
         if (value == "" ) call die("Cannot determine npots-down")
         read(unit=value,fmt=*) pseudo%npots_down

         value = getAttribute(np,"npots-up")
         if (value == "" ) call die("Cannot determine npots-up")
         read(unit=value,fmt=*) pseudo%npots_up

  else
     call die("Cannot find semilocal element")
  endif

  pseudo%npots = 0
  myList => getElementsByTagName(np, "vps")
  if (getLength(myList) == 0) then
     call die("Did not found any vps elements...")
  endif
  npseudos = getLength(myList) 
  do i = 0, npseudos - 1
     print *, "Processing vps i = ", i , "---------------------"
     myNode => item(myList, i)
     pseudo%npots = pseudo%npots + 1
     pp => pseudo%pot(pseudo%npots)
     call getVps(myNode,global_grid,pp)
  enddo

!
!  Show some of the information
!
call dump_pseudo(pseudo)

CONTAINS

!-----------------------------------------------------------------------
      subroutine die(str)
      character(len=*), intent(in), optional   :: str
      if (present(str)) then
         write(unit=0,fmt="(a)") trim(str)
      endif
      write(unit=0,fmt="(a)") "Stopping Program"
      stop
      end subroutine die


end program pseudo_dom
