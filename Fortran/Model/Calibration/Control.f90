!
!   Control.f90
!   Skill_Mismatch
!
!   Created by Rong Fan on 4/22/20.
!   Copyright 2020 FanRong. All rights reserved.
!
module Control
    implicit none
integer, parameter :: TechEndo = 1
integer :: iunit
real(8) :: t1, t2, begin, final
character(100) :: Filename, FMT

integer, parameter :: Nparam_total = 15, Nmoment = 13
real(8), dimension(Nparam_total) :: Param
integer :: param_flag(Nparam_total), Nparam
real(8), dimension(Nmoment) :: SMM_data, SMM_model, SMM_weight
real(8) :: u0, LFPR0, d0, EN0, UN0, EE0, w_grow_in0, w_grow_out0, return_L0
real(8) :: var_ab0, corr_ab0, train_revenues0, R_D_revenues0

end module Control

