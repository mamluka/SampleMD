module class_ArgonModifiedLeonardJonesPotential
    use class_PotentialBase
    use lib_Integration
    use flib_dom
    use lib_ConfigurationManager
    use class_ReducersDTO
    implicit none

    private

    public :: ArgonModifiedLeonardJonesPotential
    type,extends(PotentialBase) :: ArgonModifiedLeonardJonesPotential
        private
        real :: CutOfDistance
        real :: Sigma
        real :: Eps
        real :: Rcut
        real :: Rl
    contains
        procedure :: Force
        procedure :: SizeOfGridCell
        procedure :: CutOffRadii
        procedure :: LoadPotentialParameters
        procedure :: SetReducers
    end type

contains

    subroutine Force(this,pi,pj,r)
        class(ArgonModifiedLeonardJonesPotential) :: this
        type(Particle):: pi,pj

        real :: r


        real :: reducedm
        real :: reducedr
        real :: reducedDirection(3)
        real :: reducedSigma
        real :: reducedEpsilon

        real :: reducedrcut
        real :: reducedrl

        real :: SigmaOverR
        real :: dSpart1
        real :: dSpart2

        real :: dLGBasic(3)
        real :: LGBasic(3)

        real :: dS
        real :: S


        reducedm=1
        reducedSigma = 1
        reducedEpsilon = 1

        reducedDirection = (pj%Position-pi%Position)

        reducedrcut = this%Rcut/this%Sigma
        reducedrl = this%Rl/this%Sigma

        reducedr=r

        SigmaOverR = reducedSigma/reducedr

        dLGBasic = 24*reducedEpsilon*(2*SigmaOverR**12-SigmaOverR**6)*reducedDirection/reducedr**2

        if ( reducedr .le. reducedrl ) then
            pi%Force = pi%Force + dLGBasic
        elseif ( ( reducedr .gt. reducedrl ) .and. ( reducedr .le. reducedrcut )) then
            LGBasic = 4*reducedEpsilon*(SigmaOverR**12-SigmaOverR**6)

            dSpart1=(reducedr-reducedrl)/((reducedrcut-reducedrl)**3*reducedr)
            dSpart2 = (3*reducedrcut-reducedrl-2*reducedr)*(reducedr-reducedrl)/((reducedrcut-reducedrl)**3*reducedr)

            S=(1-(reducedr-reducedrl)**2*(3*reducedrcut-reducedrl-2*reducedr)/(reducedrcut-reducedrl)**3)

            pi%Force= pi%Force + LGBasic*2*reducedDirection*(dSpart1-dSpart2)+dLGBasic*S
        else
            pi%Force = pi%Force+0
        end if



    end subroutine Force

    function SizeOfGridCell(this) result(rc)
        class(ArgonModifiedLeonardJonesPotential) :: this
        real :: rc
        rc = this%Rcut/this%Reducers%Length
    end function

    function CutOffRadii(this) result(rc)
        class(ArgonModifiedLeonardJonesPotential) :: this
        real :: rc
        rc = this%Rcut/this%Reducers%Length

    end function

    subroutine LoadPotentialParameters(this,filename)
        class(ArgonModifiedLeonardJonesPotential) :: this
        character(len=*) :: filename

        type(fnode), pointer          :: confDoc
        type(fnode), pointer          :: rootNode,simulationNode
        type(fnodeList), pointer      :: tmpList,configNodes
        character (len=100) :: simulatiomNodeName,simulatiomNodeValue

        integer :: i


        confDoc => parsefile(filename)

        call normalize(confDoc)


        tmpList => getElementsByTagName(confDoc, "potential")

        rootNode => item(tmpList, 0)

        tmpList => getElementsByTagName(rootNode, "coeff")

        if (getLength(tmpList) == 0) then
            call die("When asked to load the simulation configuration the xml element was not found")
        endif

        simulationNode => item(tmpList, 0)

        this%Sigma = LoadXMLAttributeToReal(simulationNode,"sigma")
        this%Eps = LoadXMLAttributeToReal(simulationNode,"epsilon")
        this%Rcut = LoadXMLAttributeToReal(simulationNode,"rcut")
        this%Rl = LoadXMLAttributeToReal(simulationNode,"rl")

    end subroutine LoadPotentialParameters

    subroutine SetReducers(this,reducers)
        class(ArgonModifiedLeonardJonesPotential) :: this
        type(ReducersDTO),target :: reducers

        allocate(this%Reducers,source=reducers)

    end subroutine SetReducers


end module class_ArgonModifiedLeonardJonesPotential
