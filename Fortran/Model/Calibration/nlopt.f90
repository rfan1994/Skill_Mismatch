!
!   nlopt.f90
!   Skill_Mismatch
!
!   Created by Rong Fan on 4/22/20.
!   Copyright 2020 FanRong. All rights reserved.
!

subroutine nlopt(select_algorithm,pguess,param_range)
use Control
    implicit none
include 'nlopt.f'

integer, intent(in) :: select_algorithm
real(8), intent(in) :: param_range(2,*)
real(8), intent(inout) :: pguess(*)
integer :: algorithm, tempinit                                          ! nlopt parameter
real(8) :: xtolerabs, xtolerrel, ftolerabs, ftolerrel, fgoodenough      ! Tolerance
integer :: maxeval
integer(8) :: ptr                                                       ! Case number
integer :: ires                                                         ! Error number
real(8) :: fvalue, f_data_local
interface
    subroutine func_est_nlopt(L_N,N,theta,grad,need_gradient,f_data)
    use Control
    use Global_Variable
    use Simulation
        implicit none
    integer, intent(in) :: need_gradient
    integer, intent(in) :: N
    real(8), intent(in) :: theta(N), grad(N)
    real(8), intent(out) :: L_N
    real(8), intent(inout) :: f_data(*)
    end subroutine func_est_nlopt
end interface

! [1] Nelder-Mead
! [2] Subplex
! [3] Praxis
! [4] Direct-L
! [5] Controlled Random Search
! [6] Improved Stochastic Ranking Evolution
! [7] Evolutionary

    select case(select_algorithm)
    case(1)
        algorithm=NLOPT_LN_NELDERMEAD
    case(2)
        algorithm=NLOPT_LN_SBPLX
    case(3)
        algorithm=NLOPT_LN_PRAXIS
    case(4)
        algorithm=NLOPT_GN_DIRECT_L
    case(5)
        algorithm=NLOPT_GN_CRS2_LM
    case(6)
        algorithm=NLOPT_GN_ISRES
    case(7)
        algorithm=NLOPT_GN_ESCH
    case(8)
        algorithm=NLOPT_LN_BOBYQA
    end select

    fgoodenough = 1d-3
    ftolerabs = -huge(0d0)
    ftolerrel = -huge(0d0)                      ! any negative value means no stopping
    xtolerrel = 1d-3
    xtolerabs = 1d-3
    maxeval = 3000
    
    ! Initialize nlopt
    ptr = 0
    call nlo_create(ptr,algorithm,Nparam)
    ! Set the stopping criteria
    call nlo_set_stopval(ires,ptr,fgoodenough)  ! stops if function value less than fgoodenough
    call nlo_set_ftol_abs(ires,ptr,ftolerabs)   ! stops if function value less than ftoler found
    call nlo_set_ftol_rel(ires,ptr,ftolerrel)   ! stops if relative change in function value is less than this
    call nlo_set_xtol_abs1(ires,ptr,xtolerabs)  ! absolute stopping (checks absolute change in x)
    call nlo_set_xtol_rel(ires,ptr,xtolerrel)   ! relative stopping (checks percentage change in x)
    call nlo_set_maxeval(ires, ptr, maxeval)

    tempinit=(Nparam+1)*10
    call nlo_set_population(ires,ptr,tempinit)

    !set lower and upper bounds
    call nlo_set_lower_bounds(ires,ptr,param_range(1,1:Nparam))
    call nlo_set_upper_bounds(ires,ptr,param_range(2,1:Nparam))
    call nlo_set_min_objective(ires,ptr,func_est_nlopt,f_data_local)
    call nlo_optimize(ires,ptr,pguess(1:Nparam),fvalue)

    ! Report success or failure
    select case (ires)
    case (NLOPT_SUCCESS)
        write(*,'(A)') 'NL: success (generic)'
    case (NLOPT_STOPVAL_REACHED)
        write(*,'(A)') 'NL: success, f<=fgoodenough'
    case (NLOPT_FTOL_REACHED)
        write(*,'(A)') 'NL: success, ftolerabs or ftolerrel achieved'
    case (NLOPT_XTOL_REACHED)
        write(*,'(A)') 'NL: success, xtolerabs or xtolerrel reached'
    case (NLOPT_MAXEVAL_REACHED)
        write(*,'(A)') 'NL: failure, maxeval reached'
    case (NLOPT_MAXTIME_REACHED)
        write(*,'(A)') 'NL: failure, maxtime reached'
    case (NLOPT_FAILURE)
        write(*,'(A)') 'NL: failure (generic)'
    case (NLOPT_INVALID_ARGS)
        write(*,'(A)') 'NL: failure, invalid arguments'
    case (NLOPT_OUT_OF_MEMORY)
        write(*,'(A)') 'NL: failure, out of memory'
    case (NLOPT_ROUNDOFF_LIMITED)
        write(*,'(A)') 'NL: failure, roundoff errors limit further progress'
    case (NLOPT_FORCED_STOP)
        write(*,'(A)') 'NL: failure, forced stop'
    case default
        write(*,'(A)') 'NL: termination code not found'
    end select

    ! Free memory
    call nlo_destroy(ptr)

end subroutine
