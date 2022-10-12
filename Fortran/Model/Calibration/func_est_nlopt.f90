!
!   func_est_nlopt.f90
!   Skill_Mismatch
!
!   Created by Rong Fan on 4/22/20.
!   Copyright 2020 FanRong. All rights reserved.
!
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
integer :: i, j

	if (need_gradient .ne. 0) then
        STOP 'ERROR: nloptfcn: gradient requested, but have not programmed this'
    endif
    
    iunit=iunit+1

    b_UI = theta(1); e_min = theta(2); lambda_e = theta(3); psi = theta(4)  
    kappa = theta(5); alpha_u = theta(6); alpha_o = theta(7)
    lambda_a = theta(8); lambda_b = theta(9); a_shock = theta(10); b_shock = theta(11)
    phi1 = theta(12); phi2 = theta(13); phi3 = theta(14); phi4 = theta(15)
    mu_a0 = 2d0; mu_b0 = theta(16)

    print *, '=============================================================================='

        FMT = '(4A10)'
        write(*,FMT) 'N', 'b_UI', 'e_min', 'lambda_e'
        FMT = '(I10,3F10.2)'
        write(*,FMT) iunit, b_UI, e_min, lambda_e
        FMT = '(4A10)'
        write(*,FMT) 'psi', 'kappa', 'alpha_u', 'alpha_o'
        FMT = '(4F10.2)'
        write(*,FMT) psi, kappa, alpha_u, alpha_o
        FMT = '(4A10)'
        write(*,FMT) 'lambda_a', 'lambda_b', 'a_shock', 'b_shock'
        FMT = '(4F10.2)'
        write(*,FMT) lambda_a, lambda_b, a_shock, b_shock
        FMT = '(5A10)'
        write(*,FMT) 'phi1', 'phi2', 'phi3', 'phi4', 'mu_b'
        FMT = '(5F10.2)'
        write(*,FMT) phi1, phi2, phi3, phi4, mu_b0

    call set_zero
    call VF(0)
    call Initialize_Distribution
    call Simulate(iter_burn,0)
    call Simulate(iter_calibrate,0)
    
    SMM_model(1) = u_mean; SMM_model(2) = LFPR_mean
    SMM_model(3) = d_mean; SMM_model(4) = EN_mean; SMM_model(5) = UN_mean; SMM_model(6) = EE_mean
    SMM_model(7) = return_L_mean; SMM_model(8)= w_grow_in_mean; SMM_model(9) = w_grow_out_mean
    SMM_model(10) = a_var_mean/b_var_mean; SMM_model(11) = corr_ab_mean
    SMM_model(12) = train_mean/revenue_mean; SMM_model(13) = R_D1_mean/revenue_mean
    
    L_N = sum(SMM_weight*(SMM_model-SMM_data)**2d0)
    
    write(*,*) ''
    write(*,'(A10,F12.4)') 'L_N:', L_N
    FMT = '(6A10)'
    write(*,FMT) 'u', 'LFPR', 'd', 'EN', 'UN', 'EE'
    FMT = '(6F10.4)'
    write(*,FMT) u_mean, LFPR_mean, d_mean, EN_mean, UN_mean, EE_mean
    FMT = '(3A20)'
    write(*,FMT) 'w_grow_in', 'w_grow_out', 'return_L'
    FMT = '(3F20.4)'
    write(*,FMT) w_grow_in, w_grow_out, return_L
    FMT = '(4A10)'
    write(*,FMT) 'var_ab', 'corr_ab', 'tr/rev', 'RD/rev'
    FMT = '(4F10.4)'
    write(*,FMT) a_var/b_var, corr_ab, train/revenue, R_D1/revenue
    
    call File_Write
contains

subroutine File_Write
    implicit none
    open(unit=2, file='Calibrate0.txt', status='old', position='append')
        FMT = '(I12,18F12.4)'
        write(2,FMT) iunit, b_UI, e_min, lambda_e,              &
                     psi, kappa, alpha_u, alpha_o,              &
                     lambda_a, lambda_b, a_shock, b_shock,      &
                     phi1, phi2, phi3, phi4, mu_a0, mu_b0,      &
                     L_N
    close(2)
    
    open(unit=2, file='Calibrate1.txt', status='old', position='append')
        FMT = '(I12,6F12.4)'
        write(2,FMT) iunit,                                                 &
                     u_mean, LFPR_mean, d_mean, EN_mean, UN_mean, EE_mean
    close(2)
    
    open(unit=2, file='Calibrate2.txt', status='old', position='append')
        FMT = '(I12,7F12.4)'
        write(2,FMT) iunit,                                                 &
                     return_L_mean, w_grow_in_mean, w_grow_out_mean,        &
                     a_var_mean/b_var_mean, corr_ab_mean,                   &
                     train_mean/revenue_mean, (R_D1_mean)/revenue_mean
    close(2)
    
    open(unit=2, file='Calibrate3.txt', status='old', position='append')
        FMT = '(I12,7F12.4)'
        write(2,FMT) iunit,                                                 &
                     a_var_mean, b_var_mean,                                &
                     train_mean, R_D1_mean, R_D2_mean,                      &
                     revenue_mean, UI_mean
    close(2)
    
    open(unit=2, file='BC moment1.txt', status='old', position = 'append')
        FMT = '(I12,6F12.4)'
        write(2,FMT) iunit,                                                 &
                     u_mean, LFPR_mean, f_mean, d_mean, Mismatch_mean, EE_mean
    close(2)
    
    open(unit=2, file='BC moment2.txt', status='old', position = 'append')
        FMT = '(I12,4F12.4)'
        write(2,FMT) iunit,                                                 &
                     u_st, LFPR_st, f_st, d_st
    close(2)
    
    open(unit=2, file='BC moment3.txt', status='old', position = 'append')
        FMT = '(I12,4F12.4)'
        write(2,FMT) iunit,                                                 & 
                     corr_u_z, corr_LFPR_z, corr_f_z, corr_d_z
    close(2)
end subroutine File_Write

subroutine Set_Zero
    implicit none
    LFPR = 0d0; train = 0d0; R_D1 = 0d0; R_D2 = 0d0; revenue = 0d0
    EE = 0d0; w_grow_in = 0d0; w_grow_out = 0d0; a_var = 0d0; b_var = 0d0; corr_ab = 0d0
    u_mean = 0d0; LFPR_mean = 0d0; f_mean = 0d0; d_mean = 0d0
    u_st = 0d0; LFPR_st = 0d0; f_st = 0d0; d_st = 0d0
    corr_u_z = 0d0; corr_LFPR_z = 0d0; corr_f_z = 0d0; corr_d_z = 0d0
end subroutine Set_Zero

end subroutine
