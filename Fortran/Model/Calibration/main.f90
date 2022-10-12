!
!   main.f90
!   Skill_Mismatch
!
!   Created by Rong Fan on 4/22/20.
!   Copyright 2020 FanRong. All rights reserved.
!
program main
use Control
use Global_Variable
use My_Function
use Value_Function
use Simulation
    implicit none

integer :: i, j
real(8) :: pguess(Nparam), param_range(2,Nparam)
    
    ! ======================================================================
    ! Input
    ! ======================================================================
    
    open(unit=2, file='Calibration', status='old')
        read(2,*) SMM_data
        read(2,*) SMM_weight
        SMM_weight = SMM_weight/sum(SMM_weight)
    close(2)

    ! Parameters
    pguess(1) = 0.6975d0; param_range(1,1) = 0.65d0; param_range(2,1) = 0.7d0           ! b_UI
    pguess(2) = 0.5469d0; param_range(1,2) = 0.5d0; param_range(2,2) = 0.6d0            ! e_min 
    pguess(3) = 1.0396d0; param_range(1,3) = 1d0; param_range(2,3) = 1.05d0             ! lambda_e	
    pguess(4) = 3.2221d0; param_range(1,4) = 3d0; param_range(2,4) = 3.5d0              ! psi
    pguess(5) = 0.7085d0; param_range(1,5) = 0.6d0; param_range(2,5) = 0.8d0            ! kappa	
    pguess(6) = 4.1982d0; param_range(1,6) = 3d0; param_range(2,6) = 5d0                ! alpha_u	
    pguess(7) = 4.7002d0; param_range(1,7) = 3d0; param_range(2,7) = 5d0                ! alpha_o	
    pguess(8) = 4.5142d0; param_range(1,8) = 3d0; param_range(2,8) = 5d0                ! lambda_a	
    pguess(9) = 9.4699d0; param_range(1,9) = 9d0; param_range(2,9) = 10d0               ! lambda_b	
    pguess(10) = 0.7759d0; param_range(1,10) = 0.1d0; param_range(2,10) = 0.8d0         ! a_shock	
    pguess(11) = 0.7428d0; param_range(1,11) = 0.1d0; param_range(2,11) = 0.8d0         ! b_shock	
    pguess(12) = 10.9125d0; param_range(1,12) = 10d0; param_range(2,12) = 20d0          ! phi1	
    pguess(13) = 0d0; param_range(1,13) = 0d0; param_range(2,13) = 0d0                  ! phi2	
    pguess(14) = 18.8252d0; param_range(1,14) = 10d0; param_range(2,14) = 20d0          ! phi3	
    pguess(15) = 0d0; param_range(1,15) = 0d0; param_range(2,15) = 0d0                  ! phi4
    pguess(16) = 1.6945d0; param_range(1,16) = 1.5d0; param_range(2,16) = 2d0           ! mu_b0

    call File_Create

    ! ======================================================================
    ! Nplot
    ! ======================================================================
    
    u0 = SMM_data(1); LFPR0 = SMM_data(2)
    d0 = SMM_data(3); EN0 = SMM_data(4); UN0 = SMM_data(5); EE0 = SMM_data(6)
    return_L0 = SMM_data(7); w_grow_in0 = SMM_data(8); w_grow_out0 = SMM_data(9)
    var_ab0 = SMM_data(10); corr_ab0 = SMM_data(11)
    train_revenues0 = SMM_data(12); R_D_revenues0 = SMM_data(13)

    iunit = 0
    call nlopt(6,pguess,param_range)

contains

subroutine File_Create
    implicit none
    open(2, file='Calibrate0.txt', status='replace')
        FMT = '(19A12)'
        write(2,FMT) 'number',                                                      &
                     'b_UI', 'e_min','lambda_e',                                    &
                     'psi', 'kappa', 'alpha_u', 'alpha_o',                          &
                     'lambda_a', 'lambda_b', 'a_shock', 'b_shock',                  &
                     'phi1', 'phi2', 'phi3', 'phi4', 'a', 'b',                      &
                     'L_N'
    close(2)
    
    
    open(2, file='Calibrate1.txt', status='replace')
        FMT = '(7A12)'
        write(2,FMT) 'number',                                                      &
                     'U', 'LFPR', 'd_m', 'EN', 'UN', 'EE'
    close(2)

    open(2, file='Calibrate2.txt', status='replace')
        FMT = '(8A12)'
        write(2,FMT) 'number',                                                      &
                     'return_L', 'w_grow_in', 'w_grow_out',                         &
                     'var_a_b', 'corr_ab', 'train_rev', 'R_D_rev'
    close(2)
    
    open(2, file='Calibrate3.txt', status='replace')
        FMT = '(8A12)'
        write(2,FMT) 'number',                                                      &
                     'a_var', 'b_var', 'train', 'R_D1', 'R_D2', 'revenues', 'UI'
    close(2)
        
    open(2, file='BC moment1.txt', status='replace')
        FMT = '(7A12)'
        write(2,FMT) 'number',                                                      &
                     'u_m', 'LFPR_m', 'f_m', 'd_m', 'Mis_m', 'EE_m'
    close(2)
    
    open(2, file='BC moment2.txt', status='replace')
        FMT = '(5A12)'
        write(2,FMT) 'number',                                                      &
                     'u_st', 'LFPR_st', 'f_st', 'd_st'
    close(2)
    
    open(2, file='BC moment3.txt', status='replace')
        FMT = '(6A12)'
        write(2,FMT) 'number',                                                      &
                     'u_z', 'LFPR_z', 'f_z', 'd_z'
    close(2)
end subroutine File_Create

end program main
