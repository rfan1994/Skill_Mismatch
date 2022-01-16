!
!   control.f90
!   Skill_Mismatch
!
!   Created by FanRong on 1/29/20.
!   Copyright 2020 FanRong. All rights reserved.
!
module Control
    implicit none
integer, parameter :: TechEndo = 1
integer :: iunit
real(8) :: t1, t2
character(100) :: Filename, FMT

integer, parameter :: Nparam_total = 17
real(8) :: Param(Nparam_total,100), UN0

end module Control

