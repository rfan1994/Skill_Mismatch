!
!   Global_Variable.f90
!   Skill_Mismatch
!
!   Created by Rong Fan on 2/13/20.
!   Copyright 2020 FanRong. All rights reserved.
!
module Global_Variable
    implicit none
! Parameters
integer, parameter :: iter_max = 2000                       
integer, parameter :: iter_burn = 500, iter_calibrate = 200, iter_IR = 200
real(8), parameter :: TOL_VF = 1d-6, TOL_Simulate = 1d-4, TOL_Brent = 0.01d0         ! Tolerance
integer, parameter :: N_GauLeg = 5
real(8), parameter :: Pareto_rng = 0.99d0
integer :: step
integer :: Transition

! ======================================================================
! Model parameters
! ======================================================================

! Parameters
real(8), parameter :: beta = 0.995d0,               &   ! Discount facter
                      s = 0.176d0,                  &   ! Search efficiency
                      delta = 0.024d0,              &
                      delta1 = 0.022d0,             &   ! Exogenous separation prob
                      delta2 = 0.002d0,             &
                      alpha = 0.5d0,                &   ! Matching function elasticity
                      rho_z = 0.947d0,              &   ! Persistence of log(z)
                      sigma_z = 0.0067d0,           &   ! Volatility of log(z)
                      depr_a = 0.01d0,              &   ! Human capital depreciation
                      depr_b = 0.015d0
real(8) :: b_UI

! Parameters to calibrate
real(8) :: beta_p, delta_p
real(8) :: psi, lambda_e, kappa, alpha_u, alpha_o
real(8) :: phi1, phi2, phi3, phi4                               ! Train, School, Adoption, Innovation
real(8) :: lambda_a, lambda_b, mu_a0, mu_b0, a_shock, b_shock
real(8) :: phi1_new, phi2_new, phi3_new, phi4_new, mu_a0_new, mu_b0_new

! Grid points
integer, parameter :: Na = 11, Nb = 11, Nz = 11, Nw = 101
real(8), dimension(Na) :: a_grid, a_pdf, a_cdf
real(8), dimension(Nb) :: b_grid, b_pdf, b_cdf
real(8) :: pdf(Na,Nb)
real(8) :: a_min, a_max, b_min, b_max, mu_e, e_min
real(8) :: z_grid(Nz), z_trans(Nz,Nz)
real(8) :: w_grid(Nw), w_pdf(Nw)
integer :: p_a0, p_a1, p_b0, p_b1
! Temporary variables
real(8), dimension(Na) :: Va_tmp, Va2_tmp
real(8), dimension(Nb) :: Vb_tmp, Vb2_tmp
real(8), dimension(Na,Nb) :: Vab_tmp, Vab2_tmp

! Value function
real(8), dimension(Na,Nb,Nz) :: V_abz, EV_abz0, EV_abz, V_max, Prod            ! Value function
real(8), dimension(Na,Nb,Nz) :: P_abz, EP_abz, Disutility                      ! Output
real(8) :: V_u, EV_n1                                                          ! Value of U and N
! Expectation
real(8) :: EV_EE(Na,Nb,Nb,Nz), EV_UE(Na,Nb,Nz)
real(8) :: EV_UEa(Na,Nb,Nz), EV_UEb(Na,Nb,Nz), EV_EEb(Na,Nb,Nb,Nz)
! Derivatives
real(8), dimension(Na,Nb,Nz) :: V_abz2, EV_abz02, EV_abz2, EP_abz2, Disutility2
real(8) :: EV_UEa2(Na,Nb,Nz), EV_UEb2(Na,Nb,Nz), EV_EEb2(Na,Nb,Nb,Nz)

! Policy function
real(8), dimension(Na,Nb,Nz) :: a_opt0, b_opt0, a_opt1, b_opt1
real(8) :: Match_a1a2(2,Nb,Nz), Match_b1b2(Na,2,Nz), Trans_b1b2(Na,Nb,2,Nz)     ! Transition Policy
real(8) :: PF_LF_e1, PF_LF_e2(Na,Nb,Nz), PF_LF1, PF_LF2(Na,Nb,Nz)
real(8) :: a_bar0
! Derivatives
real(8), dimension(Na,Nb,Nz) :: a_opt2, b_opt2


! ======================================================================
! Simulation
! ======================================================================

! Individial grid points
integer, parameter :: Nhh = 10000

! Shocks
real(8), dimension(Nhh) :: LF_ind, d_ind, d2_ind, fu_ind, fe_ind
real(8), dimension(Nhh) :: a_shock_ind, b_shock_ind
real(8), dimension(Nhh) :: a_ind, b_ind

! Individual simulation
real(8), dimension(2,Nhh) :: HH0, HH1                       ! Skill, Technology
real(8), dimension(Nhh) :: V_abz_HH, EP_abz_HH, Prod_HH
real(8), dimension(Nhh) :: Disutility_HH, Wage_HH
real(8) :: UEa_HH(Na,Nhh), UEb_HH(Nb,Nhh), EEb_HH(Nb,Nhh)
real(8), allocatable :: IR_HH(:,:,:), Value_IR_HH(:,:,:)

! Track variables
integer :: z_point
real(8) :: z_value
real(8) :: u, LFPR, Mismatch, f, d, EU_exo, EU_endo, EN, UN, EE
real(8) :: Revenue, return_L
real(8) :: Vacancy, Vacancy_e, Vacancy_u
real(8) :: train, R_D1, R_D2, schooling
real(8) :: w_grow_in, w_grow_out
real(8) :: mu_a, mu_b
real(8), dimension(Na) :: Vacancy_a
real(8), dimension(Nb) :: Vacancy_u_b, Vacancy_e_b, Vacancy_b
real(8) :: a_star, b_star
real(8) :: w_mean, w_var, b_mean, b_var, a_mean, a_var, corr_ab
real(8) :: u_mean, LFPR_mean, Mismatch_mean,                                &
           f_mean, d_mean, EN_mean, UN_mean, EE_mean,                       &
           u_st, LFPR_st, f_st, d_st,                                       &
           corr_u_z, corr_LFPR_z, corr_f_z, corr_d_z
real(8) :: a_var_mean, b_var_mean, corr_ab_mean,                            &
           return_L_mean, w_grow_in_mean, w_grow_out_mean,                  &
           Revenue_mean, UI_mean,                                           &
           train_mean, R_D1_mean, R_D2_mean, schooling_mean
           

integer, dimension(iter_max) :: z_sequence
real(8), dimension(iter_max) :: z_IR,                  &     ! Productivity shocks
                                u_IR,                  &     ! Unemployment rate
                                LFPR_IR,               &     ! Labor force participation rate
                                Mismatch_IR,           &     ! Mismatch
                                f_IR,                  &     ! Job arriving rate
                                d_IR,                  &     ! Separation rate
                                EU_endo_IR,            &     ! Endogenous separation
                                EN_IR,                 &     ! EN transition
                                UN_IR,                 &     ! UN+NN transition
                                EE_IR,                 &     ! EE transition  
                                Revenue_IR,            &     ! Total revenue
                                Vacancy_e_IR,          &     ! Expected value of hiring E
                                Vacancy_u_IR,          &     ! Expected value of hiring
                                R_D1_IR,               &     ! Technology adoption
                                R_D2_IR,               &     ! Innovation
                                train_IR,              &     ! On the job training
                                schooling_IR,          &     ! Choice of entering skill level
                                return_L_IR,           &     ! Rerurn to labor
                                w_mean_IR,             &     ! Wage mean
                                w_var_IR,              &     ! Wage variance
                                w_grow_in_IR,          &     ! Within-job wage growth
                                w_grow_out_IR,         &     ! Between-job wage growth
                                mu_a_IR,               &     ! Average mu_a
                                mu_b_IR,               &     ! Average mu_b
                                a_mean_IR,             &     ! Mean of skill distribution
                                a_var_IR,              &     ! Variance of skill distribution
                                b_mean_IR,             &     ! Mean of technology distribution
                                b_var_IR,              &     ! Variance of technology distribution
                                corr_ab_IR,            &     ! Covariance between a and b
                                a_bar_IR                     ! Posting stardard
real(8), dimension(iter_max) :: N1_IR,                 &     ! Population: high skill
                                N2_IR,                 &     ! Population: low skill
                                LF1_IR,                &     ! LF: high skill
                                LF2_IR,                &     ! LF: low skill
                                e1_IR,                 &     ! Employment: high skill
                                e2_IR,                 &     ! Employment: low skill
                                ur1_IR,                &     ! Unemployment rate: high skill
                                ur2_IR                       ! Unemployment rate: low skill
                            
end module Global_Variable
