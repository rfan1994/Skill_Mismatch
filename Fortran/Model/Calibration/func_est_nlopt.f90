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
    j = 1
    do i = 1,Nparam_total
        if (param_flag(i)==1) then
            Param(i) = theta(j);
            j = j+1
        endif
    end do

    psi = Param(1); lambda_e = Param(2); kappa = Param(3); alpha_u = Param(4); alpha_o = Param(5)
    lambda_a = Param(6); lambda_b = Param(7); a_shock = Param(8); b_shock = Param(9)
    phi1 = Param(10); phi2 = Param(11); phi3 = Param(12); phi4 = Param(13)
    mu_a0 = Param(14); mu_b0 = Param(15)

    print *, '=============================================================================='

        FMT = '(6A10)'
        write(*,FMT) 'N', 'psi', 'lambda_e', 'kappa', 'alpha_u', 'alpha_o'
        FMT = '(I10,5F10.2)'
        write(*,FMT) iunit, psi, lambda_e, kappa, alpha_u, alpha_o
        FMT = '(6A10)'
        write(*,FMT) '', '', 'lambda_a', 'lambda_b', 'a_shock', 'b_shock'
        FMT = '(2A10,4F10.2)'
        write(*,FMT) '', '', lambda_a, lambda_b, a_shock, b_shock
        FMT = '(6A10)'
        write(*,FMT) 'phi1', 'phi2', 'phi3', 'phi4', 'a', 'b'
        FMT = '(6F10.2)'
        write(*,FMT) phi1, phi2, phi3, phi4, mu_a0, mu_b0

    call cpu_time(t1)
    call set_zero
    call VF(0)
    call Initialize_Distribution
    call Simulate(iter_burn,0)
    call Simulate(iter_calibrate,0)
    call cpu_time(t2)
    
    SMM_model(1) = u_mean; SMM_model(2) = LFPR_mean
    SMM_model(3) = d_mean; SMM_model(4) = EN_mean; SMM_model(5) = UN_mean; SMM_model(6) = EE_mean
    SMM_model(7) = return_L_mean; SMM_model(8)= w_grow_in_mean; SMM_model(9) = w_grow_out_mean
    SMM_model(10) = a_var_mean/b_var_mean; SMM_model(11) = corr_ab_mean
    SMM_model(12) = train_mean/revenue_mean; SMM_model(13) = R_D1_mean/revenue_mean
    
    L_N = 0d0
    do i = 1,Nmoment
        if (SMM_weight(i)>0d0) L_N = L_N+SMM_weight(i)*((SMM_model(i)-SMM_data(i))/SMM_data(i))**2d0
    end do
    
    write(*,*) ''
    write(*,'(A10,F12.4)') 'Time:', t2-t1
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
        FMT = '(I12,16F12.4)'
        write(2,FMT) iunit, psi, lambda_e, kappa, alpha_u, alpha_o,         &
                     lambda_a, lambda_b, a_shock, b_shock,                  &
                     phi1, phi2, phi3, phi4, mu_a0, mu_b0,                  &
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
