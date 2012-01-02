module class_ConfigurationManager
    use flib_dom
    use class_ErrorHandler
    implicit none


    public :: SimulationConfigurations

    type SimulationConfigurations
        real :: TimeStep
        integer :: Dimension
    end type

contains

    function LoadSimulationConfigurations(filename) result (configurations)
        type (SimulationConfigurations) :: configurations
        character (len=*) :: filename

        type(fnode), pointer          :: confDoc
        type(fnode), pointer          :: rootNode,simulationNode
        type(fnodeList), pointer      :: tmpList,configNodes
        character (len=100) :: simulatiomNodeName,simulatiomNodeValue

        integer :: i


        confDoc => parsefile(filename)

        call normalize(confDoc)


        tmpList => getElementsByTagName(confDoc, "mdconfig")

        rootNode => item(tmpList, 0)

        tmpList => getElementsByTagName(rootNode, "sim")

        if (getLength(tmpList) == 0) then
            call die("When asked to load the simulation configuration the xml element was not found")
        endif


        simulationNode => item(tmpList, 0)

        configurations%Dimension = LoadXMLAttributeToInt(simulationNode,"dim")
        configurations%TimeStep = LoadXMLAttributeToReal(simulationNode,"timestep")


        !simAttribute = getAttribute(myNode,"dim")

       ! if (simAttribute == "") call die("When asked to load the dim from xml the attribute couldn't be found")
       ! read(unit=simAttribute,fmt=*) configurations%Dimension

    end function

    function LoadXMLAttributeToReal(node,nameOfAttr) result (returnValue)
        type(fnode) , pointer :: node
        character (len=*) :: nameOfAttr
        real :: returnValue
        character (len=100) :: simAttribute

        simAttribute = getAttribute(node,nameOfAttr)

        if (simAttribute == "") call die("When asked to load " // nameOfAttr // " from xml the attribute couldn't be found")
        read(unit=simAttribute,fmt=*) returnValue

    end function LoadXMLAttributeToReal

    function LoadXMLAttributeToInt(node,nameOfAttr) result (returnValue)
        type(fnode) , pointer :: node
        character (len=*) :: nameOfAttr
        integer :: returnValue
        character (len=100) :: simAttribute

        simAttribute = getAttribute(node,nameOfAttr)

        if (simAttribute == "") call die("When asked to load " // nameOfAttr // " from xml the attribute couldn't be found")
        read(unit=simAttribute,fmt=*) returnValue

    end function LoadXMLAttributeToInt

end module class_ConfigurationManager
