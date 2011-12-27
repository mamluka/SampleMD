module m_psdom

use m_pseudo_types
use flib_dom

private

public :: getVps
public :: getRadialFunction
public :: getGrid

CONTAINS

subroutine getVps(np,global_grid,pp)
type(fnode), pointer         :: np
type(vps_t), intent(inout)   :: pp
type(grid_t), intent(in)     :: global_grid

character(len=200)  :: value

         value = getAttribute(np,"l")
         if (value == "" ) call die("Cannot determine l for Vps")
         read(unit=value,fmt=*) pp%l

         value = getAttribute(np,"principal-n")
         if (value == "" ) call die("Cannot determine n for Vps")
         read(unit=value,fmt=*) pp%n

         value = getAttribute(np,"cutoff")
         if (value == "" ) call die("Cannot determine cutoff for Vps")
         read(unit=value,fmt=*) pp%cutoff

         value = getAttribute(np,"occupation")
         if (value == "" ) call die("Cannot determine occupation for Vps")
         read(unit=value,fmt=*) pp%occupation

         value = getAttribute(np,"spin")
         if (value == "" ) call die("Cannot determine spin for Vps")
         read(unit=value,fmt=*) pp%spin

         call getRadialFunction(np,global_grid,pp%V)

end subroutine getVps

!-----------------------------------------------------------------------
subroutine getRadialFunction(element,global_grid,rp)
use m_converters, only: build_data_array
!
! Example of routine which packages parsing functionality for a
! common element. The <radfunc> element can appear under <vps>,
! <valence-charge>, and <pseudocore-charge> elements. 
! In all cases the parsing steps are exactly  the same.
! This routine accepts a pointer to the parent element and returns
! the data structure.
!
type(fnode), pointer         :: element
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
         call getGrid(np,rp%grid)
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
end subroutine getRadialFunction

!-----------------------------------------------------------------------
subroutine getGrid(element,grid)
type(fnode), pointer  :: element
type(grid_t), intent(out)       :: grid

character(len=200)  :: value

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

end subroutine getGrid

!-----------------------------------------------------------------------
      subroutine die(str)
      character(len=*), intent(in), optional   :: str
      if (present(str)) then
         write(unit=0,fmt="(a)") trim(str)
      endif
      write(unit=0,fmt="(a)") "Stopping Program"
      stop
      end subroutine die


end module m_psdom

