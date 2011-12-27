program features

  use flib_dom

  implicit none

  type(fnode), pointer :: myDoc

  myDoc => parsefile("test.xml" ) !! , verbose=.true.)
  print *, getNumberofAllocatedNodes()
  call dumpTree(myDoc)
  call xmlize(myDoc,"features.xml")
  call destroyNode(myDoc)
end program features
