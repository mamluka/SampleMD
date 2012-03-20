module class_ArgonModifiedLeonardJonesPotential
    use class_PotentialBase
    use lib_Integration
    use lib_ConfigurationManager
    use class_ReducersDTO
    use class_ArgonModifiedLeonardJonesPotentialCofiguraions
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

    subroutine Force(this,pi,pj,r,direction)
        class(ArgonModifiedLeonardJonesPotential) :: this
        type(Particle):: pi,pj


        real :: r,direction(3)


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

        real :: dV(3)


        reducedm=1
        reducedSigma = 1
        reducedEpsilon = 1

        reducedDirection = direction

        reducedrcut = this%Rcut/this%Sigma
        reducedrl = this%Rl/this%Sigma

        reducedr=r

        SigmaOverR = reducedSigma/reducedr

        dLGBasic = 24.0*reducedEpsilon*(SigmaOverR**6-2.0*SigmaOverR**12)*reducedDirection/reducedr

        if ( reducedr .le. reducedrl ) then
            dV = dLGBasic
            pi%Force = pi%Force - dV
        elseif ( ( reducedr .gt. reducedrl ) .and. ( reducedr .le. reducedrcut )) then
            LGBasic = 4*reducedEpsilon*(SigmaOverR**12-SigmaOverR**6)

            dSpart1=(reducedr-reducedrl)/((reducedrcut-reducedrl)**3*reducedr)
            dSpart2 = (3*reducedrcut-reducedrl-2*reducedr)*(reducedr-reducedrl)/((reducedrcut-reducedrl)**3*reducedr)

            S=(1-(reducedr-reducedrl)**2*(3*reducedrcut-reducedrl-2*reducedr)/(reducedrcut-reducedrl)**3)

            dV=LGBasic*2*reducedDirection*(dSpart1-dSpart2)+dLGBasic*S

            pi%Force= pi%Force - dV
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

    subroutine LoadPotentialParameters(this)
        class(ArgonModifiedLeonardJonesPotential) :: this
        type(ArgonModifiedLeonardJonesPotentialCofiguraions) :: configurations

        this%Sigma = configurations%Sigma
        this%Eps = configurations%Eps
        this%Rcut = configurations%Rcut
        this%Rl = configurations%Rl

    end subroutine LoadPotentialParameters

    subroutine SetReducers(this,reducers)
        class(ArgonModifiedLeonardJonesPotential) :: this
        type(ReducersDTO),target :: reducers

        allocate(this%Reducers,source=reducers)

    end subroutine SetReducers


end module class_ArgonModifiedLeonardJonesPotential
