!
!   main.f90
!   Skill_Mismatch
!
!   Created by FanRong on 1/28/20.
!   Copyright 2020 FanRong. All rights reserved.
!
program main
use Control
use Global_Variable
use My_Function
use Value_Function
use Simulation
    implicit none
integer :: num, N, iostatus

      
    ! ======================================================================
    ! param
    ! ======================================================================
    
    open(unit=2, file='Simulation', status='old')
        N = 0
        do
            N = N+1
            read(2,*,IOSTAT=iostatus) param(:,N)
            if (iostatus < 0) exit           
        end do
    close(2)

    ! ======================================================================
    ! Simulation
    ! ======================================================================

    call File_Create

    do num = 1,N-1
        iunit = num; b_UI = param(1,num); UN0 = param(2,num)
        psi = param(3,num); lambda_e = param(4,num)
        kappa = param(5,num); alpha_u= param(6,num); alpha_o = param(7,num);
        lambda_a = param(8,num); lambda_b = param(9,num); a_shock = param(10,num); b_shock = param(11,num)
        phi1 = param(12,num); phi2 = param(13,num); phi3 = param(14,num); phi4 = param(15,num)
        mu_a0 = param(16,num); mu_b0 = param(17,num)

        write(*,*) '=============================================================================='
        FMT = '(6A10)'
        write(*,FMT) '', '', '', 'N', 'b_UI', 'UN0'
        FMT = '(3A10,I10,2F10.2)'
        write(*,FMT) '', '', '', iunit, b_UI, UN0
        FMT = '(6A10)'
        write(*,FMT) '', 'psi', 'lambda_e', 'kappa', 'alpha_u', 'alpha_o'
        FMT = '(A10,6F10.2)'
        write(*,FMT) '', psi, lambda_e, kappa, alpha_u, alpha_o
        FMT = '(6A10)'
        write(*,FMT) '', '', 'lambda_a', 'lambda_b', 'a_shock', 'b_shock'
        FMT = '(2A10,4F10.2)'
        write(*,FMT) '', '', lambda_a, lambda_b, a_shock, b_shock
        FMT = '(6A10)'
        write(*,FMT) 'phi1', 'phi2', 'phi3', 'phi4', 'a', 'b'
        FMT = '(6F10.2)'
        write(*,FMT) phi1, phi2, phi3, phi4, mu_a0, mu_b0
        write(*,*) ''
        
        call cpu_time(t1)
        call Set_Zero
        call VF(1)
        call Initialize_Distribution
        call Simulate(iter_burn,0)
        call Simulate(iter_IR,1)
        call cpu_time(t2)
        write(*,'(A15,F12.4)') 'Time:', (t2-t1)/60d0
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
    end do
    
contains

! ======================================================================

subroutine File_Create
    implicit none
    open(2, file='Calibrate0.txt', status='replace')
        FMT = '(28A12)'
        write(2,FMT) 'number', 'b_UI', 's',                                     &
                     'psi', 'lambda_e', 'kappa', 'alpha_u', 'alpha_o',          &
                     'lambda_a', 'lambda_b', 'a_shock', 'b_shock',              &
                     'phi1', 'phi2', 'phi3', 'phi4', 'a', 'b'
    close(2)
    
    open(2, file='Calibrate1.txt', status='replace')
        FMT = '(7A12)'
        write(2,FMT) 'number',                                                  &
                     'u', 'LFPR', 'd', 'EN', 'UN', 'EE'
    close(2)
    
    open(2, file='Calibrate2.txt', status='replace')
        FMT = '(8A12)'
        write(2,FMT) 'number',                                                  &
                     'return_L', 'w_grow_in', 'w_grow_out',                     &
                     'var_a_b', 'corr_ab', 'train_rev', 'R_D_rev'
    close(2)
    
    open(2, file='Calibrate3.txt', status='replace')
        FMT = '(8A12)'
        write(2,FMT) 'number',                                                  &
                     'a_var', 'b_var', 'train', 'R_D1', 'R_D2', 'revenue', 'UI'
    close(2)

    open(2, file='BC moment1.txt', status='replace')
        FMT = '(7A12)'
        write(2,FMT) 'number',                                                  &
                     'u_m', 'LFPR_m', 'f_m', 'd_m', 'Mis_m', 'EE_m'
    close(2)
    
    open(2, file='BC moment2.txt', status='replace')
        FMT = '(5A12)'
        write(2,FMT) 'number',                                                  &
                     'u_st', 'LFPR_st', 'f_st', 'd_st'
    close(2)
    
    open(2, file='BC moment3.txt', status='replace')
        FMT = '(5A12)'
        write(2,FMT) 'number',                                                  &
                     'u_z', 'LFPR_z', 'f_z', 'd_z'
    close(2)
end subroutine File_Create

subroutine File_Write
    implicit none
    
    open(unit=2, file='Calibrate0.txt', status='old', position='append')
        FMT = '(I12,17F12.2)'
        write(2,FMT) iunit, b_UI, s,                                        &
                     psi, lambda_e, kappa, alpha_u, alpha_o,                &
                     lambda_a, lambda_b, a_shock, b_shock,                  &
                     phi1, phi2, phi3, phi4, mu_a0, mu_b0
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


end program main
