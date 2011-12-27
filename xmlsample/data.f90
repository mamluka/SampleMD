program example

  use flib_dom

  type(fnode), pointer     :: myDoc
  type(fnode), pointer     :: myNode, temp,  refNode
  type(fnodeList), pointer :: myList

  myDoc => parsefile("pseudo.xml",verbose=.true.)
  call dumpTree(myDoc)
  call xmlize(myDoc,"out.xml")

! Get some nodes
  myList => getChildNodes(myDoc)
  print *, "Number of children of doc: ", getLength(myList) 

  myNode => item(myList, 0)
  myList => getChildNodes(myNode)
  print *, "Number of children of first child of doc: ", getLength(myList) 

  myNode => item(myList, 2)
  refNode => getNextSibling(myNode)
  call xmlize(refNode,"ref.xml")

!
!    Note: a single element dumped
!
     call xmlize(myNode,"node.xml")
     temp => cloneNode(myNode,deep=.true.)
     call xmlize(temp,"clone.xml")

     call destroyNode(myNode)
     call xmlize(myDoc,"nosemilocal.xml")
     myNode => insertBefore(getParentNode(refNode),temp,refNode)
     call xmlize(myDoc,"all.xml")

     myList => getElementsByTagName(myDoc,"data")
     print *, "Number of data nodes: ", getLength(myList) 

     call destroyNode(myDoc)

     print *, "Number of active nodes at the end: ", getNumberofAllocatedNodes()

end program example
