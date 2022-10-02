!
!   Impulse_Response.f90
!   Skill_Mismatch
!
!   Created by FanRong on 1/28/20.
!   Copyright 2020 FanRong. All rights reserved.
!
module Simulation
use Control
use Global_Variable
use My_Function
use Value_Function
    implicit none

contains

! ============================================================
! Benchmark
! ============================================================

subroutine Simulate(iter_total,output)
    implicit none
integer, intent(in) :: iter_total, output
integer :: iter
    
    step = 0
    z_sequence = (Nz+1)/2; z_IR = 1d0
    call Productivity_Shocks(iter_total)
    
    if (output==1) allocate(IR_HH(2,3,iter_total), Value_IR_HH(4,3,iter_total))
    
    ! Simulation
    do iter = 1,iter_total
        ! Current TFP
        z_point = z_sequence(iter); z_value = z_IR(iter)

        ! Shocks
        step = 1; call Skill_Shocks(iter)
        step = 2; call Technology_Shocks(iter)
        step = 3; call Expected_Value(iter)

        ! Separation
        step = 4; call Leave_LF(iter)
        step = 5; call Separation(iter)
        step = 6; call Solve_Moment(iter,iter_total)
        step = 7; call IR_e(iter); if (output==1) call Store_Value(iter)

        ! Solve values
        step = 8; call Total_Revenue(iter)
        step = 9; call Hire_Value
        
        ! Solve new employment distribution
        step = 10; call Innovation(iter)
        step = 11; call Job_Finding_Rate(iter)
        step = 12; call Update_Distribution(iter)
        step = 13; call New_Entry(iter)
    end do
    
    ! Export_Result
    call BC_moment(iter_total)
    if (output==1) then
        call Export_Result(iter_total)
        call Export_HH(iter_total)
        deallocate(IR_HH, Value_IR_HH)
    endif

end subroutine Simulate


! ======================================================================
! Model Setup
! ======================================================================

subroutine Initialize_Distribution
    implicit none
real(8), dimension(Nhh) :: e_ind, y_ind
    mu_a = mu_a0; mu_b = mu_b0
    mu_a_IR = mu_a0; mu_b_IR = mu_b0
    call Random_Pareto(Nhh,0,mu_a,lambda_a,a_ind)
    call Random_Pareto(Nhh,0,mu_b,lambda_b,b_ind)
    u = 0.055d0+UN0
    call Random_Binomial(Nhh,0,1d0-u,e_ind)
    HH1(1,:) = a_ind; HH1(2,:) = b_ind*e_ind
    call get_wage

contains

subroutine get_wage
    implicit none
integer :: hhi
real(8) :: a, b, bp, w, y0
    
    Vab_tmp = Disutility(:,:,z_point); Vab2_tmp = Disutility2(:,:,z_point)
    do hhi = 1,Nhh
        if (.not. HH1(2,hhi)>0d0) cycle
        a = HH1(1,hhi); b = HH1(2,hhi)
        call splin2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp,a,b,y0)
        Wage_HH(hhi) = y0
    end do
    
end subroutine get_wage

end subroutine Initialize_Distribution


! ======================================================================

subroutine Productivity_Shocks(iter_total)
    implicit none
integer, intent(in) :: iter_total
integer :: iter, p_z

    call Random_Markov(iter_max,1994,Nz,z_grid,z_trans,z_sequence)
    do iter = 1,iter_total
        p_z = z_sequence(iter)
        z_IR(iter) = z_grid(p_z)
    enddo
    
end subroutine Productivity_Shocks

! ============================================================

subroutine Store_Value(iter)
    implicit none
integer, intent(in) :: iter

    IR_HH(:,:,iter) = HH1(:,1:3)
    Value_IR_HH(1,:,iter) = V_abz_HH(1:3)
    Value_IR_HH(2,:,iter) = EP_abz_HH(1:3)
    Value_IR_HH(3,:,iter) = Disutility_HH(1:3)
    Value_IR_HH(4,:,iter) = Wage_HH(1:3)
    
end subroutine Store_Value


! ======================================================================
! Model Pieces
! ======================================================================

subroutine Skill_Shocks(iter)
    implicit none
integer, intent(in) :: iter
integer :: hhi

    ! NLF -> U
    do hhi = 1,Nhh
        if (.not. HH1(2,hhi)==-1d0) cycle
        HH1(2,hhi) = 0d0
        V_abz_HH(hhi) = 0d0; EP_abz_HH(hhi) = 0d0; Prod_HH(hhi) = 0d0
        Disutility_HH(hhi) = 0d0; Wage_HH(hhi) = 0d0
    enddo
	! Depreciation and exit
    call depreciate
    ! Shocks
	call shock
    
contains

subroutine depreciate
    implicit none
integer :: hhi

    ! Depreciation
    if (phi1>0) then
        do hhi = 1,Nhh
            if (HH1(2,hhi)==0d0) HH1(1,hhi) = (1d0-depr_a)*HH1(1,hhi)
        enddo
    endif
    
    ! Exit
    if (phi2>0) then
        call Random_Binomial(Nhh,iter,delta2,d2_ind)
        do hhi = 1,Nhh
            if (d2_ind(hhi)==1d0) then
                HH1(2,hhi) = -2d0
                V_abz_HH(hhi) = 0d0; EP_abz_HH(hhi) = 0d0; Prod_HH(hhi) = 0d0
                Disutility_HH(hhi) = 0d0; Wage_HH(hhi) = 0d0
            endif
        enddo
    endif
    
end subroutine depreciate


subroutine shock
    implicit none
integer :: hhi

	! Skill shocks
    call Random_Pareto(Nhh,iter,mu_a0,lambda_a,a_ind)
    call Random_Binomial(Nhh,iter,a_shock,a_shock_ind)
    do hhi = 1,Nhh
        if (.not. a_shock_ind(hhi)==1d0) cycle
        HH1(1,hhi) = a_ind(hhi)
    enddo
    
end subroutine shock


end subroutine Skill_Shocks


! ======================================================================

subroutine Technology_Shocks(iter)
    implicit none
integer, intent(in) :: iter
integer :: hhi

    ! Depreciation
    if (phi3>0) then
        do hhi = 1,Nhh
            if (HH1(2,hhi)>0d0) HH1(2,hhi) = (1d0-depr_b)*HH1(2,hhi)
        enddo
    endif

    ! Technology shocks
    b_min = mu_b0*(lambda_b-1d0)/lambda_b
    call Random_Pareto(Nhh,iter,mu_b0,lambda_b,b_ind)
    call Random_Binomial(Nhh,iter,b_shock,b_shock_ind)
    do hhi = 1,Nhh
        if (.not.(HH1(2,hhi)>0d0 .and. b_shock_ind(hhi)==1d0)) cycle
        HH1(2,hhi) = b_ind(hhi)
    enddo
    
end subroutine Technology_Shocks


! ======================================================================

subroutine Expected_Value(iter)
    implicit none
integer, intent(in), optional :: iter

    z_value = z_IR(iter); z_point = z_sequence(iter)
    call value
    
contains

subroutine value
    implicit none
integer :: hhi
real(8) :: a, b, y0, w
    
    ! V_abz
    Vab_tmp = V_abz(:,:,z_point); Vab2_tmp = V_abz2(:,:,z_point)
    do hhi = 1,Nhh
        if (.not. HH1(2,hhi)>0d0) cycle
        a = HH1(1,hhi); b = HH1(2,hhi)
        call splin2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp,a,b,y0)
        V_abz_HH(hhi) = y0
    end do
    
    ! EP_abz
    Vab_tmp = EP_abz(:,:,z_point); Vab2_tmp = EP_abz2(:,:,z_point)
    do hhi = 1,Nhh
        if (.not. HH1(2,hhi)>0d0) cycle
        a = HH1(1,hhi); b = HH1(2,hhi)
        call splin2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp,a,b,y0)
        EP_abz_HH(hhi) = y0
    end do
    
    ! Disutility
    Vab_tmp = Disutility(:,:,z_point); Vab2_tmp = Disutility2(:,:,z_point)
    do hhi = 1,Nhh
        if (.not. HH1(2,hhi)>0d0) cycle
        a = HH1(1,hhi); b = HH1(2,hhi)
        call splin2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp,a,b,y0)
        Disutility_HH(hhi) = y0
    end do
    
    ! Prod
    do hhi = 1,Nhh
        if (.not. HH1(2,hhi)>0d0) cycle
        a = HH1(1,hhi); b = HH1(2,hhi)
        Prod_HH(hhi) = prod1(a,b,z_value)
    end do
    
    do hhi = 1,Nhh
        if (.not. HH1(2,hhi)>0d0) cycle
        y0 = EP_abz_HH(hhi); w = Wage_HH(hhi)
        if (w>y0) Wage_HH(hhi) = y0
    end do
    
end subroutine value

end subroutine Expected_Value


! ======================================================================

subroutine Leave_LF(iter)
    implicit none
integer, intent(in) :: iter

	LFPR = 0d0; EN = 0d0; UN = 0d0
    call leave
    LFPR_IR(iter) = LFPR; EN_IR(iter) = EN; UN_IR(iter) = UN
    
contains

subroutine leave
    implicit none
integer :: hhi
real(8) :: y0, e_n

    call Random_Pareto(Nhh,iter,mu_e,lambda_e,LF_ind)
    do hhi = 1,Nhh
        e_n = LF_ind(hhi)
        if (HH1(2,hhi)==0d0) then
            if (e_n>=b_UI) then
                HH1(2,hhi) = -1d0
                V_abz_HH(hhi) = 0d0; EP_abz_HH(hhi) = 0d0; Prod_HH(hhi) = 0d0
                Disutility_HH(hhi) = 0d0; Wage_HH(hhi) = 0d0
                UN = UN+1d0
            endif
        elseif (HH1(2,hhi)>0d0) then
            y0 = V_abz_HH(hhi)+b_UI
            if (e_n>=y0) then
                HH1(2,hhi) = -1d0
                V_abz_HH(hhi) = 0d0; EP_abz_HH(hhi) = 0d0; Prod_HH(hhi) = 0d0
                Disutility_HH(hhi) = 0d0; Wage_HH(hhi) = 0d0
                EN = EN+1d0
            endif
        endif
    end do
    y0 = dble(Nhh)
    LFPR = dble(count(HH1(2,:)>=0d0))/y0
    EN = EN/y0; UN = UN/y0
    
end subroutine leave

end subroutine Leave_LF


! ======================================================================

subroutine Total_Revenue(iter)
    implicit none
integer, intent(in) :: iter
integer :: hhi

    Revenue = 0d0
    do hhi = 1,Nhh
        if (.not. HH1(2,hhi)>0d0) cycle
        Revenue = Revenue+Prod_HH(hhi)
    enddo
    Revenue_IR(iter) = Revenue

end subroutine Total_Revenue


! ======================================================================

subroutine Separation(iter)
    implicit none
integer, intent(in) :: iter
real(8) :: y0, y1

    HH0 = HH1
    EU_exo = 0d0; EU_endo = 0d0; Mismatch = 0d0
    call separate
    u_IR(iter) = u; d_IR(iter) = d
    EU_endo_IR(iter) = EU_endo

contains

subroutine separate
    implicit none
integer :: hhi
    
    call Random_Binomial(Nhh,iter,delta_p,d_ind)
    do hhi = 1,Nhh
        if (.not. HH1(2,hhi)>0) cycle
        if (V_abz_HH(hhi)<0) then
            HH1(2,hhi) = 0d0
            V_abz_HH(hhi) = 0d0; EP_abz_HH(hhi) = 0d0; Prod_HH(hhi) = 0d0
            Disutility_HH(hhi) = 0d0; Wage_HH(hhi) = 0d0
            EU_endo = EU_endo+1d0
            Mismatch = Mismatch+1d0
        elseif (d_ind(hhi)==1d0) then
            HH1(2,hhi) = 0d0
            V_abz_HH(hhi) = 0d0; EP_abz_HH(hhi) = 0d0; Prod_HH(hhi) = 0d0
            Disutility_HH(hhi) = 0d0; Wage_HH(hhi) = 0d0
            EU_exo = EU_exo+1d0
        endif
    enddo
    y0 = dble(count(HH1(2,:)==0d0))
    y1 = dble(count(HH1(2,:)>0d0))
    u = y0/(y0+y1)
    y0 = dble(count(HH0(2,:)>0d0))
    d = (EU_exo+EU_endo)/y0
    EU_endo = EU_endo/y0
    
end subroutine separate

end subroutine Separation


! ======================================================================

subroutine Solve_Moment(iter,iter_total)
    implicit none
integer, intent(in) :: iter, iter_total
real(8), dimension(:), allocatable :: avec, bvec, wvec, vvec
real(8) :: a, b, w
integer :: N, hhi, i
integer :: p0, p1, p2

    a_pdf = 0d0; b_pdf = 0d0; pdf = 0d0; w_pdf = 0d0
    N = count(HH1(2,:)>0d0)
    allocate(avec(N),bvec(N),wvec(N))
    avec = pack(HH1(1,:), HH1(2,:)>0d0)
    bvec = pack(HH1(2,:), HH1(2,:)>0d0)
    wvec = pack(Wage_HH(:), HH1(2,:)>0d0)
    vvec = pack(EP_abz_HH(:), HH1(2,:)>0d0)

    ! Worker
	a_mean = mean(N,avec)
	a_var = variance(N,avec)/a_mean
    
    if (iter==iter_total) then
        do hhi = 1,Nhh
            a = HH1(1,hhi)
            p0 = 1; p1 = Na
            call Interpolate(Na,a_grid,a,p0,p1)
            a_pdf(p1) = a_pdf(p1)+1d0
        enddo
        a_pdf = a_pdf/dble(Nhh)
    endif
    
    ! Firm
	b_mean = mean(N,bvec)
	b_var = variance(N,bvec)/b_mean
    
    if (iter==iter_total) then
        do i = 1,N
            b = bvec(i)
            p0 = 1; p1 = Nb
            call Interpolate(Nb,b_grid,b,p0,p1)
            b_pdf(p1) = b_pdf(p1)+1d0
        enddo
        b_pdf = b_pdf/dble(N)
    endif
    
	! Worker and firm
 	corr_ab = correlation(N,N,avec,bvec)
     
    if (iter==iter_total) then
        do i = 1,N
            a = avec(i)
            p0 = 1; p1 = Na
            call Interpolate(Na,a_grid,a,p0,p1)
            b = bvec(i)
            p0 = 1; p2 = Nb
            call Interpolate(Nb,b_grid,b,p1,p2)
            pdf(p1,p2) = pdf(p1,p2)+1d0
        enddo
        pdf = pdf/dble(Nhh)
    endif

	! Wage
 	w_mean = mean(N,wvec)
    return_L = mean(N,wvec)/mean(N,vvec)
	w_var = variance(N,wvec)/w_mean
    
    if (iter==iter_total) then
        do i = 1,N
            w = wvec(i)
            p0 = 1; p1 = Nw
            call Interpolate(Nw,w_grid,w,p0,p1)
            w_pdf(p1) = w_pdf(p1)+1d0
        enddo
        w_pdf = w_pdf/dble(N)
    endif
    
    deallocate(avec,bvec,wvec)
    return_L_IR(iter) = return_L
    w_mean_IR(iter) = w_mean; w_var_IR(iter) = w_var
    b_mean_IR(iter) = b_mean; b_var_IR(iter) = b_var
    a_mean_IR(iter) = a_mean; a_var_IR(iter) = a_var; corr_ab_IR(iter) = corr_ab

end subroutine Solve_Moment


! ============================================================

subroutine IR_e(iter)
    implicit none
integer, intent(in) :: iter
real(8) :: y0, y1, y2, y3, y4, y5, y6
real(8) :: a_m

    a_m = 0.9*maxval(HH1(1,:))+0.1*maxval(HH1(1,:))
    y1 = dble(count(HH1(1,:)<=a_m))
    y2 = dble(count(HH1(1,:)>a_m))
    y3 = dble(count(HH1(1,:)<=a_m .and. HH1(2,:)>=0d0))
    y4 = dble(count(HH1(1,:)>a_m .and. HH1(2,:)>=0d0))
    y5 = dble(count(HH1(1,:)<=a_m .and. HH1(2,:)>0d0))
    y6 = dble(count(HH1(1,:)>a_m .and. HH1(2,:)>0d0))

    N1_IR(iter) = y1/dble(Nhh)
    N2_IR(iter) = y2/dble(Nhh)
    LF1_IR(iter) = y3/y1
    LF2_IR(iter) = y4/y2
    e1_IR(iter) = y5/y1
    e2_IR(iter) = y6/y2
    ur1_IR(iter) = 1d0-y5/y3
    ur2_IR(iter) = 1d0-y6/y4
    
end subroutine IR_e


! ======================================================================

subroutine Hire_Value
    implicit none
real(8) :: y1, y2
integer :: ai, bi

    UEa_HH = 0d0; UEb_HH = 0d0; EEb_HH = 0d0
    
    y2 = dble(count(HH1(2,:)>0d0))
    do ai = 1,Na
        Vacancy_a(ai) = beta_p*(1d0-delta_p)*sum(UEa_HH(ai,:))/y2
    enddo
    
    y1 = dble(count(HH0(2,:)==0d0))
    y2 = dble(count(HH1(2,:)>0d0))
    call value
    do bi = 1,Nb
        Vacancy_u_b(bi) = beta_p*(1d0-delta_p)*sum(UEb_HH(bi,:))/y1
        Vacancy_e_b(bi) = beta_p*(1d0-delta_p)*sum(EEb_HH(bi,:))/y2
        Vacancy_b(bi) = beta_p*(1d0-delta_p)*(sum(UEb_HH(bi,:))+s*sum(EEb_HH(bi,:)))/(y1+s*y2)
    enddo
    
contains

subroutine value
    implicit none
integer :: hhi
real(8) :: a, b
real(8) :: y0

    do ai = 1,Na
        Vb_tmp = EV_UEa(ai,:,z_point); Vb2_tmp = EV_UEa2(ai,:,z_point)
        do hhi = 1,Nhh
            if (.not. HH1(2,hhi)>0d0) cycle
            b = HH1(2,hhi)
            call splint(Nb,b_grid,Vb_tmp,Vb2_tmp,b,y0)
            UEa_HH(ai,hhi) = max(y0,0d0)
        end do
    enddo
       
    do bi = 1,Nb
        Va_tmp = EV_UEb(:,bi,z_point); Va2_tmp = EV_UEb2(:,bi,z_point)
        do hhi = 1,Nhh
            if (.not. HH0(2,hhi)==0d0) cycle
            a =  HH1(1,hhi)
            call splint(Na,a_grid,Va_tmp,Va2_tmp,a,y0)
            UEb_HH(bi,hhi) = max(y0,0d0)
        end do
        
        Vab_tmp = EV_EEb(:,:,bi,z_point); Vab2_tmp = EV_EEb2(:,:,bi,z_point)
        do hhi = 1,Nhh
            if (.not. HH0(2,hhi)>0d0) cycle
            a =  HH1(1,hhi); b = HH1(2,hhi)
            call splin2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp,a,b,y0)
            EEb_HH(bi,hhi) = max(y0,0d0)
        end do
    enddo

end subroutine value

end subroutine Hire_Value


! ======================================================================

subroutine Innovation(iter)
    implicit none
integer, intent(in) :: iter
real(8) :: y0, y1
    if (.not. phi4>0d0) return
    Vb_tmp = Vacancy_b
    call spline(Nb,b_grid,Vb_tmp,0d0,0d0,Vb2_tmp)
    b_star = mu_b; flag = 0
    y0 = Vb_func(mu_b); y1 = -EV_innov_func(mu_b+TOL_Brent)
    if (y1>y0) call Brent_Min(EV_innov_func,b_grid(1),b_grid(Nb/2),b_grid(Nb),TOL_Brent,b_star,flag)
    if (flag==1 .and. b_star>mu_b+TOL_Brent) mu_b = b_star
    R_D2 = Cost_innov_func(mu_b,b_star)
    
    mu_b_IR(iter) = mu_b
    R_D2_IR(iter) = R_D2
end subroutine

! ======================================================================

subroutine Job_Finding_Rate(iter)
    implicit none
integer, intent(in) :: iter
real(8) :: y0
    call value
    
    Vb_tmp = Vacancy_b
    call spline(Nb,b_grid,Vb_tmp,0d0,0d0,Vb2_tmp)
    call splint(Nb,b_grid,Vb_tmp,Vb2_tmp,mu_b,y0)
    Vacancy = y0
	f = (1d0/psi)*Vacancy
	f = min(f**(alpha/(1d0-alpha)),1d0)
    
    Vacancy_e_IR(iter) = Vacancy_e; Vacancy_u_IR(iter) = Vacancy_u
    f_IR(iter) = f

contains

subroutine value
    implicit none
real(8) :: y0
    Vb_tmp = Vacancy_u_b
    call spline(Nb,b_grid,Vb_tmp,0d0,0d0,Vb2_tmp)
    call splint(Nb,b_grid,Vb_tmp,Vb2_tmp,mu_b,y0)
    Vacancy_u = y0
    
    Vb_tmp = Vacancy_e_b
    call spline(Nb,b_grid,Vb_tmp,0d0,0d0,Vb2_tmp)
    call splint(Nb,b_grid,Vb_tmp,Vb2_tmp,mu_b,y0)
    Vacancy_e = y0
end subroutine value

end subroutine Job_Finding_Rate


! ======================================================================

subroutine Update_Distribution(iter)
    implicit none
integer, intent(in) :: iter
integer :: hhi
real(8) :: a, b, bp
real(8), dimension(Nhh) :: Wage_in1, Wage_in2, Wage_out1, Wage_out2

    EE = 0d0; train = 0d0; R_D1 = 0d0
    Wage_in1 = 0d0; Wage_in2 = 0d0
    Wage_out1 = 0d0; Wage_out2 = 0d0
    call update
    EE_IR(iter) = EE; train_IR(iter) = train; R_D1_IR(iter) = R_D1
    w_grow_out_IR(iter) = w_grow_out; w_grow_in_IR(iter) = w_grow_in

contains

subroutine update
	implicit none
real(8) :: V1, V2, V3, y0, w, d

    call Random_Pareto(Nhh,iter,mu_b,lambda_b,b_ind)
    call Random_Binomial(Nhh,iter,f,fu_ind)
    call Random_Binomial(Nhh,iter,s*f,fe_ind)
    do hhi = 1,Nhh
        if (.not.(HH0(2,hhi)==0d0 .and. fu_ind(hhi)==1d0)) cycle
        b = b_ind(hhi)
        Vab_tmp = EV_abz(:,:,z_point); Vab2_tmp = EV_abz2(:,:,z_point)
        call splin2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp,a,b,V3)
        if (V3>0) then
            HH1(2,hhi) = b_ind(hhi)
            if (phi1>0) call training
            if (phi3>0) call adoption
        else
            Mismatch = Mismatch+1d0
        endif
        Vab_tmp = Disutility(:,:,z_point); Vab2_tmp = Disutility2(:,:,z_point)
        a = HH1(1,hhi); b = HH1(2,hhi)
        call splin2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp,a,b,y0)
        Wage_HH(hhi) = y0
    
    enddo

    do hhi = 1,Nhh
        if (.not.(HH1(2,hhi)>0d0 .and. fe_ind(hhi)==1d0)) cycle
        a = HH1(1,hhi); b = HH1(2,hhi); bp = b_ind(hhi); d = Disutility_HH(hhi)
        V1 = EP_abz_HH(hhi)-d; V2 = Wage_HH(hhi)-d
        
        Vab_tmp = EV_abz(:,:,z_point); Vab2_tmp = EV_abz2(:,:,z_point)
        call splin2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp,a,bp,V3)
        
        if (V3>V1) then
            HH1(2,hhi) = bp
            if (phi1>0) call training
            if (phi3>0) call adoption
            Vab_tmp = Disutility(:,:,z_point); Vab2_tmp = Disutility2(:,:,z_point)
            a = HH1(1,hhi); b = HH1(2,hhi)
            call splin2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp,a,b,y0)
            Wage_HH(hhi) = V1+y0
            Wage_out1(hhi) = V2+d+EV_n1; Wage_out2(hhi) = V1+y0+EV_n1
            EE = EE+1d0
        elseif (V3>V2) then
            HH1(2,hhi) = b
            Wage_HH(hhi) = V3+d
            Wage_in1(hhi) = V2+d+EV_n1; Wage_in2(hhi) = V3+d+EV_n1
        endif
    enddo
    
    w_grow_out = (sum(Wage_out2)-sum(Wage_out1))/(sum(Wage_out1))
    w_grow_in = (sum(Wage_in2)-sum(Wage_in1))/(sum(Wage_in1))
    EE = EE/dble(Nhh)
    Mismatch = Mismatch/dble(Nhh)

end subroutine update

subroutine training
    implicit none
real(8) :: a_star
    a = HH1(1,hhi); b = HH1(2,hhi)
    Vab_tmp = a_opt1(:,:,z_point); Vab2_tmp = a_opt2(:,:,z_point)
    call splin2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp,a,b,a_star)
    if (.not. a_star>a+TOL_Brent) return
    HH1(1,hhi) = a_star
    train = train+Cost_train_func(a,a_star)
end subroutine training

subroutine adoption
    implicit none
real(8) :: b_star
    a = HH1(1,hhi); b = HH1(2,hhi)
    Vab_tmp = b_opt1(:,:,z_point); Vab2_tmp = b_opt2(:,:,z_point)
    call splin2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp,a,b,b_star)
    if (.not. b_star>b+TOL_Brent) return
    HH1(2,hhi) = b_star
    R_D1 = R_D1+Cost_tech_func(b,b_star)
end subroutine adoption

end subroutine Update_Distribution


! ======================================================================

subroutine New_Entry(iter)
    implicit none
integer, intent(in) :: iter
integer :: hhi, ai
real(8) :: b, y0, y1

    if (.not. phi2>0d0) return
    Va_tmp = Vacancy_a
    call spline(Na,a_grid,Va_tmp,0d0,0d0,Va2_tmp)
    a_star = mu_a; flag = 0
    y0 = Va_func(mu_a); y1 = -EV_school_func(mu_a+TOL_Brent)
    if (y1>y0) call Brent_Min(EV_school_func,a_grid(1),a_grid(Na/2),a_grid(Nb),TOL_Brent,a_star,flag)
    if (flag==1 .and. a_star>mu_a+TOL_Brent) mu_a = a_star
    schooling = Cost_school_func(mu_a,a_star)*dble(count(HH1(2,:)==-2d0))
    
    call Random_Pareto(Nhh,iter,mu_a,lambda_a,a_ind)
    do hhi = 1,Nhh
        if (.not. HH1(2,hhi)==-2d0) cycle
        HH1(1,hhi) = a_ind(hhi); HH1(2,hhi) = 0d0
        V_abz_HH(hhi) = 0d0; EP_abz_HH(hhi) = 0d0; Prod_HH(hhi) = 0d0
        Disutility_HH(hhi) = 0d0; Wage_HH(hhi) = 0d0
    enddo
    
    mu_a_IR(iter) = mu_a
    schooling_IR(iter) = schooling

end subroutine New_Entry



! ======================================================================

subroutine BC_moment(iter_total)
    implicit none
integer, intent(in) :: iter_total
    u_mean = mean(iter_total,u_IR(1:iter_total))
    LFPR_mean = mean(iter_total,LFPR_IR(1:iter_total))
    Mismatch_mean = mean(iter_total,Mismatch_IR(1:iter_total))
    f_mean = mean(iter_total,f_IR(1:iter_total))
    d_mean = mean(iter_total,d_IR(1:iter_total))
    EN_mean = mean(iter_total,EN_IR(1:iter_total))
    UN_mean = mean(iter_total,UN_IR(1:iter_total))
    EE_mean = mean(iter_total,EE_IR(1:iter_total))
    u_st = sqrt(variance(iter_total,u_IR(1:iter_total)))/u_mean
    LFPR_st = sqrt(variance(iter_total,LFPR_IR(1:iter_total)))/LFPR_mean
    f_st = sqrt(variance(iter_total,f_IR(1:iter_total)))/f_mean
    d_st = sqrt(variance(iter_total,d_IR(1:iter_total)))/d_mean
    corr_u_z = correlation(iter_total,iter_total,u_IR(1:iter_total),z_IR(1:iter_total))
    corr_LFPR_z = correlation(iter_total,iter_total,LFPR_IR(1:iter_total),z_IR(1:iter_total))
    corr_f_z = correlation(iter_total,iter_total,f_IR(1:iter_total),z_IR(1:iter_total))
    corr_d_z = correlation(iter_total,iter_total,d_IR(1:iter_total),z_IR(1:iter_total))
    a_var_mean = mean(iter_total,a_var_IR(1:iter_total))
    b_var_mean = mean(iter_total,b_var_IR(1:iter_total))
    corr_ab_mean = mean(iter_total,corr_ab_IR(1:iter_total))
    return_L_mean= mean(iter_total,return_L_IR(1:iter_total))
    w_grow_in_mean = mean(iter_total,w_grow_in_IR(1:iter_total))
    w_grow_out_mean = mean(iter_total,w_grow_out_IR(1:iter_total))
    Revenue_mean = mean(iter_total,Revenue_IR(1:iter_total))
    train_mean = mean(iter_total,train_IR(1:iter_total))
    R_D1_mean = mean(iter_total,R_D1_IR(1:iter_total))
    R_D2_mean = mean(iter_total,R_D2_IR(1:iter_total))
    schooling_mean = mean(iter_total,schooling_IR(1:iter_total))
    UI_mean = mean(iter_total,LFPR_IR(1:iter_total)*u_IR(1:iter_total))*(b_UI-0.75)*Nhh
end subroutine BC_moment


! ======================================================================

subroutine Export_Result(iter_total)
    implicit none
integer, intent(in) :: iter_total
integer :: ai, bi, wi, j

    write(Filename,'("pdf",I3,".txt")') iunit
    write(FMT,'("(",I3,"F10.3)")') Nb
    open(unit=2, file=Filename, status='replace')
        do ai = 1,Na
            write(2,FMT) pdf(ai,:)
        end do
    close(2)
    
    write(Filename,'("a_pdf",I3,".txt")') iunit
    open(unit=2, file=Filename, status='replace')
        write(2,'(A4,4A10)') 'ai', 'a_grid', 'a_pdf', 'low', 'high'
        do ai = 1,Na
            write(2,'(I4,4F10.3)') ai, a_grid(ai), a_pdf(ai), sum(pdf(ai,1:p_b0)), sum(pdf(ai,p_b1:Nb))
        end do
    close(2)
    
    write(Filename,'("b_pdf",I3,".txt")') iunit
    open(unit=2, file=Filename, status='replace')
        write(2,'(A4,4A10)') 'bi', 'b_grid', 'b_pdf', 'low', 'high'
        do bi = 1,Nb
            write(2,'(I4,4F10.3)') bi, b_grid(bi), b_pdf(bi), sum(pdf(1:p_a0,bi)), sum(pdf(p_a1:Na,bi))
        end do
    close(2)
    
    write(Filename,'("w_pdf",I3,".txt")') iunit
    open(unit=2, file=Filename, status='replace')
        do wi = 1,Nw
            write(2,'(2F10.3)') w_grid(wi), w_pdf(wi)
        end do
    close(2)
    
    write(Filename,'("IR",I3,".txt")') iunit
    open(unit=2, file=Filename, status='replace')
        write(2,'(A4,10A10)') 't', 'z', 'u', 'LFPR', 'Mismatch',                            &
                                        'f', 'd', 'EU_endo',                                &
                                        'EN', 'UN', 'EE'
                                   
        do j = 1,iter_total
            write(2,'(I4,10F10.4)') j, z_IR(j), u_IR(j), LFPR_IR(j), Mismatch_IR(j),        &
                                                f_IR(j), d_IR(j), EU_endo_IR(j),               &
                                                EN_IR(j), UN_IR(j), EE_IR(j)
        end do
    close(2)
    
    write(Filename,'("IR_value",I3,".txt")') iunit
    open(unit=2, file=Filename, status='replace')
        write(2,'(A4,8A14)') 't', 'z', 'Vacancy_e', 'Vacancy_u', 'Revenue',                         &
                                       'train', 'schooling', 'R_D1', 'R_D2'
                                   
        do j = 1,iter_total
            write(2,'(I4,8F14.6)') j, z_IR(j), Vacancy_e_IR(j), Vacancy_u_IR(j), Revenue_IR(j),     &
                                               train_IR(j), schooling_IR(j), R_D1_IR(j), R_D2_IR(j)
        end do
    close(2)

    write(Filename,'("IR_e",I3,".txt")') iunit
    open(unit=2, file=Filename, status='replace')
        write(2,'(A4,9A14)') 't', 'z', 'N1','N2', 'LF1', 'LF2', 'e1', 'e2', 'ur1', 'ur2'
        do j = 1,iter_total
            write(2,'(I4,9F14.6)') j, z_IR(j), N1_IR(j), N2_IR(j),LF1_IR(j), LF2_IR(j),     &
                                               e1_IR(j), e2_IR(j), ur1_IR(j), ur2_IR(j)
        end do
    close(2)

    write(Filename,'("IR_wage",I3,".txt")') iunit
    open(unit=2, file=Filename, status='replace')
        write(2,'(A4,11A14)') 't', 'z', 'return_L', 'w_mean', 'w_var',                         &
                                   'mu_a', 'mu_b', 'a_mean', 'a_var',                          &
                                   'b_mean', 'b_var', 'corr_ab'
        do j = 1,iter_total
            write(2,'(I4,11F14.6)') j, z_IR(j), return_L_IR(j), w_mean_IR(j), w_var_IR(j),               &
                                                mu_a_IR(j), mu_b_IR(j), a_mean_IR(j), a_var_IR(j),       &
                                                b_mean_IR(j), b_var_IR(j), corr_ab_IR(j)
        end do
    close(2)
    
end subroutine Export_Result


subroutine Export_HH(iter_total)
    implicit none
integer, intent(in) :: iter_total
integer :: hhi, i
    write(Filename,'("HH",I3,".txt")') iunit
    open(unit=2, file=Filename, status='replace')
        write(2,'(6A10)') 'a', 'b', 'V_abz', 'EP_abz', 'Disutility', 'Wage'
        do hhi = 1,Nhh
            write(2,'(6F10.3)') HH1(:,hhi), V_abz_HH(hhi), EP_abz_HH(hhi), Disutility_HH(hhi), Wage_HH(hhi)
        end do
    close(2)
        
    do hhi = 1,3
        write(Filename,'("IR_HH",I3,"_",I1".txt")') iunit, hhi
        open(unit=2, file=Filename, status='replace')
            write(2,'(6A10)') 'a', 'b', 'V_abz', 'EP_abz', 'Disu', 'Wage'
            do i = 1,iter_total
                write(2,'(6F10.3)') IR_HH(:,hhi,i), Value_IR_HH(:,hhi,i)
            end do
        close(2)
    enddo

end subroutine Export_HH

end module Simulation
