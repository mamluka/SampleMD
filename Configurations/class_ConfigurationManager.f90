module class_ConfigurationManager
    use flib_dom
    implicit none


    public :: SimulationConfigurations

    type SimulationConfigurations
        real :: TimeStep
        integer :: Dimension
    end type

contains

    function LoadSimulationConfigurations(filename) result (configurations)
        type (SimulationConfigurations) :: configurations
        character (len=200) :: filename

        type(fnode), pointer          :: confDoc
        type(fnode), pointer          :: rootNode,simulationNode
        type(fnodeList), pointer      :: tmpList,configNodes
        character (len=100) :: simulatiomNodeName,simulatiomNodeValue

        integer :: i

        confDoc => parsefile("c:\mdconfig.xml")
        !confDoc => parsefile(filename)


        tmpList => getElementsByTagName(confDoc, "mdconfig")

        rootNode => item(tmpList, 0)

        configNodes => getChildNodes(rootNode)

        do i = 0,getLength(configNodes)
            simulationNode => item(configNodes,i)

            simulatiomNodeName=char(getNodeName(simulationNode))
            simulatiomNodeValue=char(getNodeValue(simulationNode))


            print *,simulatiomNodeValue,simulatiomNodeName

            select case (simulatiomNodeName)
                case ("dim")
                    read(unit=simulatiomNodeValue,fmt=*) configurations%Dimension
                case ("timestep")
                    read(unit=simulatiomNodeValue,fmt=*) configurations%TimeStep
                case default

            end select


        enddo







    end function

end module class_ConfigurationManager
