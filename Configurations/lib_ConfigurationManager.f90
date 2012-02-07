module lib_ConfigurationManager
    use flib_dom
    use class_ErrorHandler
    implicit none


    public :: SimulationConfigurations

    type :: ReducersDTO
        real :: Time
        real :: Energy
        real :: Length
        real :: Mass
    end type


    type SimulationConfigurations
        real :: TimeStep
        integer :: Dimension
        character (len=:),allocatable :: PotentialName
        character (len=:),allocatable :: PotentialDataFile
        character (len=:),allocatable :: DataFilename
        logical :: hasReduction = .false.
        type(ReducersDTO) :: Reducers
    end type



contains

    function LoadSimulationConfigurations(filename) result (configurations)
        type (SimulationConfigurations) :: configurations
        character (len=*) :: filename

        type(fnode), pointer          :: confDoc
        type(fnode), pointer          :: rootNode,simulationNode,reducerNode
        type(fnodeList), pointer      :: tmpList,configNodes
        character (len=100) :: simulatiomNodeName,simulatiomNodeValue

        real :: timeReducer,lengthReducer,energyReducer,massReducer

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
        configurations%DataFilename = char(getAttribute(simulationNode,"data-filename"))
        configurations%PotentialName = char(getAttribute(simulationNode,"potential"))
        configurations%PotentialDataFile = char(getAttribute(simulationNode,"potential-data-file"))

        tmpList => getElementsByTagName(rootNode, "dless")

        if (getLength(tmpList) .gt. 0) then
            reducerNode => item(tmpList, 0)
            timeReducer = LoadXMLAttributeToReal(reducerNode,"time")
            lengthReducer = LoadXMLAttributeToReal(reducerNode,"length")
            massReducer = LoadXMLAttributeToReal(reducerNode,"mass")
            energyReducer = LoadXMLAttributeToReal(reducerNode,"energy")

            configurations%HasReduction = .true.

            configurations%Reducers%Time = timeReducer
            configurations%Reducers%Length = LengthReducer
            configurations%Reducers%Mass = massReducer
            configurations%Reducers%Energy = energyReducer

        end if

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



end module lib_ConfigurationManager
