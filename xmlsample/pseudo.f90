program pseudoread

use m_pseudo_types

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
        call get_grid_data(np,global_grid)
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
     call get_radfunc_data(np,global_grid,pseudo%valence_charge)
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
     call get_radfunc_data(np,global_grid,pseudo%core_charge)
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

         value = getAttribute(myNode,"l")
         if (value == "" ) call die("Cannot determine l for Vps")
         read(unit=value,fmt=*) pp%l

         value = getAttribute(myNode,"principal-n")
         if (value == "" ) call die("Cannot determine n for Vps")
         read(unit=value,fmt=*) pp%n

         value = getAttribute(myNode,"cutoff")
         if (value == "" ) call die("Cannot determine cutoff for Vps")
         read(unit=value,fmt=*) pp%cutoff

         value = getAttribute(myNode,"occupation")
         if (value == "" ) call die("Cannot determine occupation for Vps")
         read(unit=value,fmt=*) pp%occupation

         value = getAttribute(myNode,"spin")
         if (value == "" ) call die("Cannot determine spin for Vps")
         read(unit=value,fmt=*) pp%spin

         call get_radfunc_data(myNode,global_grid,pp%V)

  enddo

!
!  Show some of the information
!
call dump_pseudo(pseudo)

CONTAINS

!-----------------------------------------------------------------------
subroutine get_radfunc_data(element,global_grid,rp)
use m_converters, only: build_data_array
!
! Example of routine which packages parsing functionality for a
! common element. The <radfunc> element can appear under <vps>,
! <valence-charge>, and <pseudocore-charge> elements. 
! In all cases the parsing steps are exactly  the same.
! This routine accepts a pointer to the parent element and returns
! the data structure.
!
type(fnode), pointer      :: element
type(grid_t), intent(in)     :: global_grid
type(radfunc_t), intent(out) :: rp

type(fnode), pointer      :: np, radfuncp
type(fnodeList), pointer  :: lp
integer                   :: ndata
type(string)              :: pcdata, s

  s = getNodeName(element)
  print *, "Getting radfunc data from element ", char(s)
  lp => getElementsByTagName(element, "radfunc")
  radfuncp => item(lp,0)
  lp => getElementsByTagName(radfuncp, "grid")
  np => item(lp,0)
      if (associated(np))  then
         print *, " >> local grid found"
         call get_grid_data(np,rp%grid)
      else
         print *, " >> re-using global grid"
         rp%grid = global_grid
      endif

  lp => getElementsByTagName(radfuncp, "data")
  np => item(lp,0)
      if (associated(np))  then
         if (rp%grid%npts == 0) call die("Need grid information!")
         allocate(rp%data(rp%grid%npts))
         ndata = 0             ! To start the build up
         np => getFirstChild(np)
         do
            if (.not. associated(np)) exit
            if (getNodeType(np) /= TEXT_NODE) exit
            pcdata = getNodeValue(np)               ! text node 
            call build_data_array(char(pcdata),rp%data,ndata)
            np => getNextSibling(np)
         enddo
         if (ndata /= size(rp%data)) STOP "npts mismatch"
      else
         call die("Cannot find data element")
      endif
end subroutine get_radfunc_data
!-----------------------------------------------------------------------
subroutine get_grid_data(element,grid)
type(fnode), pointer  :: element
type(grid_t), intent(out)       :: grid

character(len=100)  :: value

         grid%type = getAttribute(element,"type")
         if (grid%type == "" ) call die("Cannot determine grid type")

         value = getAttribute(element,"npts")
         if (value == "" ) call die("Cannot determine grid npts")
         read(unit=value,fmt=*) grid%npts

         value = getAttribute(element,"scale")
         if (value == "" ) call die("Cannot determine grid scale")
         read(unit=value,fmt=*) grid%scale

         value = getAttribute(element,"step")
         if (value == "" ) call die("Cannot determine grid step")
         read(unit=value,fmt=*) grid%step

end subroutine get_grid_data

!-----------------------------------------------------------------------
      subroutine die(str)
      character(len=*), intent(in), optional   :: str
      if (present(str)) then
         write(unit=0,fmt="(a)") trim(str)
      endif
      write(unit=0,fmt="(a)") "Stopping Program"
      stop
      end subroutine die


end program pseudoread
