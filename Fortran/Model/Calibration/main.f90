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
real(8), allocatable :: param_range(:,:), pguess(:)
integer :: i, j

    
    ! ======================================================================
    ! Input
    ! ======================================================================
    
    open(unit=2, file='Calibration', status='old')
        read(2,*) SMM_data
        read(2,*) SMM_weight
        SMM_weight = SMM_weight/sum(SMM_weight)
        read(2,*) Param
        read(2,*) param_flag
    close(2)

    ! Parameters
    b_UI = 0.75d0
    psi = Param(1); lambda_e = Param(2); kappa = Param(3); alpha_u = Param(4); alpha_o = Param(5)
    lambda_a = Param(6); lambda_b = Param(7); a_shock = Param(8); b_shock = Param(9)
    phi1 = Param(10); phi2 = Param(11); phi3 = Param(12); phi4 = Param(13)
    mu_a0 = Param(14); mu_b0 = Param(15)
    Nparam = sum(param_flag)
    
    call File_Create

    ! ======================================================================
    ! Nplot
    ! ======================================================================
    
    u0 = SMM_data(1); LFPR0 = SMM_data(2)
    d0 = SMM_data(3); EN0 = SMM_data(4); UN0 = SMM_data(5); EE0 = SMM_data(6)
    return_L0 = SMM_data(7); w_grow_in0 = SMM_data(8); w_grow_out0 = SMM_data(9)
    var_ab0 = SMM_data(10); corr_ab0 = SMM_data(11)
    train_revenues0 = SMM_data(12); R_D_revenues0 = SMM_data(13)
   
    allocate(param_range(2,Nparam), pguess(Nparam))
    j = 1
    do i = 1,Nparam_total
        if (param_flag(i)==1) then
            pguess(j) = Param(i)
            if (i==3 .or. i==8 .or. i==9) then
                param_range(1,j) = 0d0; param_range(2,j) = 1d0
            else
                param_range(1,j) = 0.5d0*Param(i); param_range(2,j) = 2d0*Param(i)
            endif
            j = j+1
        endif
    end do

    iunit = 0
    call nlopt(5,pguess,param_range)

contains

subroutine File_Create
    implicit none
    open(2, file='Calibrate0.txt', status='replace')
        FMT = '(17A12)'
        write(2,FMT) 'number',                                                      &
                     'psi', 'theta_e', 'kappa', 'alpha_u', 'alpha_o',               &
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
