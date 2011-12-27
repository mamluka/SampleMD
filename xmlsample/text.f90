program text

  use flib_dom

  implicit none

  type(fnode), pointer :: myDoc, n
  type(fnodeList), pointer :: myList, childList
  type(string)  :: s

  integer  :: i, j, nels

  myDoc => parsefile("big-file.xml")!! , verbose=.true.)
  print *, "allocated nodes: ", getNumberofAllocatedNodes()
!  call dumpTree(myDoc)
  call xmlize(myDoc,"dumptext.xml")

  myList => getElementsbyTagName(myDoc, "para")
  nels = getLength(myList)
  print *, "number of para elements: ", nels
  print *, "Dumping pure text under them..."
  do i = 0, nels - 1
     n => item(myList,i)
     childList => getChildNodes(n)
     do j = 0, getLength(childList) - 1
        n => item(childList,j)
        if (getNodeType(n) == TEXT_NODE) then
           s = getNodeValue(n)
           print *, "|", char(s), "|"
        endif
     enddo
  enddo

  print *, "Normalizing..."
  print *, "===================================================="
  call normalize(myDoc)
!  call dumpTree(myDoc)
  print *, "allocated nodes: ", getNumberofAllocatedNodes()
 myList => getElementsbyTagName(myDoc, "para")
  nels = getLength(myList)
  print *, "number of para elements: ", nels
  print *, "Dumping pure text under them..."
  do i = 0, nels - 1
     n => item(myList,i)
     childList => getChildNodes(n)
     do j = 0, getLength(childList) - 1
        n => item(childList,j)
        if (getNodeType(n) == TEXT_NODE) then
           s = getNodeValue(n)
           print *, "|", char(s), "|"
        endif
     enddo
  enddo


end program text
