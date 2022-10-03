!
!   Value_Function.f90
!   Skill_Mismatch
!
!   Created by FanRong on 1/28/20.
!   Copyright 2020 FanRong. All rights reserved.
!
module Value_Function
use Control
use Global_Variable
use My_Function
    implicit none
! Value funcion
real(8), dimension(Na,Nb,Nz) :: V_e0, V_e1, EV_e
! Others
integer :: flag
real(8) :: sup, sup1, sup2
real(8) :: a0, b0, f0
real(8), parameter :: zeros_abz(Na,Nb,Nz) = 0d0, zeros_b(Nb) = 0d0
real(8), parameter :: ones_abz(Na,Nb,Nz) = 1d0

contains

subroutine VF(output)
    implicit none
integer, intent(in) :: output
integer :: iter

	beta_p = beta; delta_p = delta
	if (phi2>0d0)  then
        beta_p = beta*(1d0-delta2); delta_p = delta1
    endif
    
    ! Grid space
    call Grid_Space(output)
    ! Unemployment value
    call Solve_Vu
    
    ! Solve value functions V_e
    call Initial_Guess(1)
    do iter = 1,iter_max
    	call Continuation_Value(1)
        call Contraction_Mapping(1)
        
        ! Check convergence
        sup = maxval(abs(V_e1-V_e0))/maxval(abs(V_e0))
        if (sup<TOL_VF) then
            V_abz = V_e0
            exit
        endif
        ! Update
        V_e0 = V_e1; V_abz = V_e0
    enddo
    
    ! Solve disutility
    call Initial_Guess(2)
    do iter = 1,iter_max
    	call Continuation_Value(2)
        call Contraction_Mapping(2)
        
        ! Check convergence
        sup = maxval(abs(V_e1-V_e0))/maxval(abs(V_e0))
        if (sup<TOL_VF) then
            P_abz = V_e0+V_abz
            EP_abz = EV_abz+Disutility
            exit
        endif
        ! Update
        V_e0 = V_e1; P_abz = V_e0+V_abz
    enddo
    
    ! Export VF
    call Solve_PF
    call Solve_VF
    call Spline_Coefficient
    if (output==1) call Export_VF
end subroutine VF


! ======================================================================

subroutine Grid_Space(output)
    implicit none
integer, intent(in) :: output
    call Productivity_Space
    call Skill_Space
    call Technology_Space

contains

subroutine Productivity_Space
    implicit none
integer :: zi
    ! Productivity space
    call discrete_AR1(sigma_z,rho_z,Nz,z_grid,z_trans)
    
    if (output==1) then
        write(FMT,'("(I3,",I3,"F8.3)")') Nz+1
        open(unit=2, file='z_grid.txt', status='replace')
            do zi = 1,Nz
                write(2,FMT) zi, z_grid(zi), z_trans(:,zi)
            enddo
        close(2)
    endif
end subroutine Productivity_Space

subroutine Skill_Space
    implicit none
integer :: ai
    ! Skill space and distribution
    a_min = mu_a0*(lambda_a-1d0)/lambda_a
    a_max = (1d0-Pareto_rng)**(-1d0/lambda_a)*a_min
    call discrete_Pareto(a_min,a_max,lambda_a,Na,a_grid,a_pdf,a_cdf)
    p_a0 = 1; p_a1 = Na
    call Interpolate(Na,a_grid,mu_a0,p_a0,p_a1)
    p_a1 = p_a0+1
    if (output==1) then
        write(Filename,'("a_grid",I3,".txt")') iunit
        open(unit=2, file=Filename, status='replace')
            do ai = 1,Na
                write(2,'(I4,F8.3,F8.4)') ai, a_grid(ai), a_pdf(ai)
            enddo
        close(2)
    endif
end subroutine Skill_Space

subroutine Technology_Space
    implicit none
integer :: ai, bi, zi
    ! Technology space
    b_min = Brent_Root(prod_left,0.1d0*a_min,a_min,TOL_Brent,flag)
    b_max = Brent_Root(prod_right,a_max,10d0*a_max,TOL_Brent,flag)
    call discrete_Pareto(b_min,b_max,lambda_b,Nb,b_grid,b_pdf,b_cdf)
    p_b0 = 1; p_b1 = Nb
    call Interpolate(Nb,b_grid,mu_b0,p_b0,p_b1)
    p_b1 = p_b0+1
    if (output==1) then
        write(Filename,'("b_grid",I3,".txt")') iunit
        open(unit=2, file=Filename, status='replace')
            do bi = 1,Nb
                write(2,'(I4,F8.3,F8.4,2I4)') bi, b_grid(bi), b_pdf(bi)
            enddo
        close(2)
    endif
    ! Production
    do zi = 1,Nz
        do bi = 1,Nb
            do ai = 1,Na
                Prod(ai,bi,zi) = prod1(a_grid(ai),b_grid(bi),z_grid(zi))
            enddo
        enddo
    enddo
    if (output==1) then
        write(Filename,'("Production",I3,".txt")') iunit
        write(FMT,'("(",I3,"F12.4)")') Nb
        open(unit=2, file=Filename, status='replace')
            do ai = 1,Na
                write(2,FMT) Prod(ai,:,6)
            enddo
        close(2)
    endif
end subroutine Technology_Space

end subroutine Grid_Space


! ======================================================================

subroutine Solve_Vu
	implicit none
    mu_e = e_min*lambda_e/(lambda_e-1d0)
    EV_n1 = (b_UI+e_min**lambda_e*b_UI**(1-lambda_e)/lambda_e)/(1d0-beta_p)
    V_u = b_UI+beta_p*EV_n1
    
    call linspace(0d0,15d0,Nw,w_grid)
    V_e0 = 0d0; V_e1 = 0d0
end subroutine Solve_Vu


! ======================================================================

subroutine Initial_Guess(prod)
	implicit none
integer, intent(in) :: prod   
integer :: ai, bi, zi
    do zi = 1,Nz
        do bi = 1,Nb
            do ai = 1,Na
                select case (prod)
                case (1)
                    f0 = prod2(a_grid(ai),b_grid(bi),z_grid(zi))
                    V_e0(ai,bi,zi) = f0-b_UI
                case (2)
                    f0 = prod1(a_grid(ai),b_grid(bi),z_grid(zi)) &
                       - prod2(a_grid(ai),b_grid(bi),z_grid(zi))
                    V_e0(ai,bi,zi) = f0
                end select
            enddo
        enddo
    enddo
end subroutine Initial_Guess

! ======================================================================

subroutine Continuation_Value(prod)
	implicit none
integer, intent(in) :: prod
integer :: ai, bi, zi
real(8) :: y0, y1
real(8), dimension(Na,Nb,Nz) :: EV_ez, EV_ea, EV_eb
    ! Expectation on e
    do zi = 1,Nz
        do bi = 1,Nb
            do ai = 1,Na
                select case (prod)
                case (1)
                    y0 = V_e0(ai,bi,zi)
                    if (y0>0) then
                        V_max(ai,bi,zi) = y0+(e_min**lambda_e/(lambda_e-1d0))                   &
                                            *((y0+b_UI)**(1d0-lambda_e)-b_UI**(1d0-lambda_e))
                    else
                        V_max(ai,bi,zi) = 0
                    endif
                case (2)
                    y0 = V_e0(ai,bi,zi); y1 = V_abz(ai,bi,zi)
                    if (y1>0) then
                        V_max(ai,bi,zi) = y0*(1d0-(e_min/(y1+b_UI))**lambda_e)
                    else
                        V_max(ai,bi,zi) = 0
                    endif
                end select
			enddo
		enddo
	enddo

	! Expectation on z
    do zi = 1,Nz
        do bi = 1,Nb
            do ai = 1,Na
				y0 = sum(V_max(ai,bi,:)*z_trans(:,zi))
				EV_ez(ai,bi,zi) = y0
			enddo
		enddo
	enddo
    
    ! Expectation on a
    do zi = 1,Nz
        do bi = 1,Nb
            Va_tmp = EV_ez(:,bi,zi)
            call spline(Na,a_grid,Va_tmp,0d0,0d0,Va2_tmp)
            a_min = mu_a0*(lambda_a-1d0)/lambda_a
            call GauLeg_inf(V_Pareto_a_func,N_GauLeg,0d0,1d0/a_min,y1)
            EV_ea(:,bi,zi) = y1
		enddo
	enddo
    EV_ez = (1d0-a_shock)*EV_ez+a_shock*EV_ea
    
    ! Expectation on b
    do zi = 1,Nz
        do ai = 1,Na
            Vb_tmp = EV_ez(ai,:,zi)
            call spline(Nb,b_grid,Vb_tmp,0d0,0d0,Vb2_tmp)
            b_min = mu_b0*(lambda_b-1d0)/lambda_b
            call GauLeg_inf(V_Pareto_b_func,N_GauLeg,0d0,1d0/b_min,y1)
            EV_eb(ai,:,zi) = y1
		enddo
	enddo
    EV_ez = (1d0-b_shock)*EV_ez+b_shock*EV_eb
    EV_e = EV_ez
end subroutine Continuation_Value


! ======================================================================

subroutine Contraction_Mapping(prod)
	implicit none
integer, intent(in) :: prod
integer :: ai, bi, zi
real(8) :: y0
    do zi = 1,Nz
        do ai = 1,Na
            Vb_tmp = EV_e(ai,:,zi)
            call spline(Nb,b_grid,Vb_tmp,0d0,0d0,Vb2_tmp)
            do bi = 1,Nb
                select case (prod)
                case (1)
                    f0 = prod2(a_grid(ai),b_grid(bi),z_grid(zi))
                case (2)
                    f0 = prod1(a_grid(ai),b_grid(bi),z_grid(zi))    &
                       - prod2(a_grid(ai),b_grid(bi),z_grid(zi))
                end select
                a0 = a_grid(ai)
                b0 = (1d0-depr_b)*b_grid(bi)
                call splint(Nb,b_grid,Vb_tmp,Vb2_tmp,b0,y0)
                V_e1(ai,bi,zi) = f0+beta*(1d0-delta)*y0
                select case (prod)
                case (1)
                     EV_abz(ai,bi,zi) = y0
                case (2)
                     Disutility(ai,bi,zi) = y0
                end select
            enddo
        enddo
    enddo
end subroutine Contraction_Mapping


! ======================================================================
! Solve the rest
! ======================================================================

subroutine Solve_PF
    implicit none
    
    call LF_policy
    EV_abz0 = EV_abz
    if (phi1>0d0 .and. phi3==0d0) then
        call policy1
    elseif (phi1==0d0 .and. phi3>0d0) then
        call policy2
    elseif (phi1>0d0 .and. phi3>0d0) then
        call policy3
    endif

contains

subroutine LF_policy
    implicit none
    
    PF_LF_e1 = b_UI
    PF_LF_e2 = max(V_abz+b_UI,e_min*ones_abz)
    PF_LF1 = (e_min/PF_LF_e1)**lambda_e
    PF_LF2 = (e_min/PF_LF_e2)**lambda_e

end subroutine LF_policy


subroutine policy1
	implicit none
integer :: ai, bi, zi
real(8) :: a1, a2, a_star
real(8) :: y0, y1
    do zi = 1,Nz
        ! Training
        do bi = 1,Nb
            Va_tmp = beta_p*(1d0-delta_p)*EV_abz(:,bi,zi)
            call spline(Na,a_grid,Va_tmp,0d0,0d0,Va2_tmp)
            a_opt0(:,bi,zi) = a_grid; a_opt1(:,bi,zi) = a_grid
            a_star = a_grid(1)
            do ai = 1,Na-1
                a0 = a_grid(ai); a_star = max(a0,a_star); a1 = (a_star+a_max)/2d0; flag = 0
                y0 = Va_tmp(ai); y1 = Va_func(a0+TOL_Brent)
                if (y1>y0) call Brent_Min(EV_train_func,a_star,a1,a_max,TOL_Brent,a2,flag)
                if (flag==1 .and. a2>a0+TOL_Brent) then
                    a_star = a2
                    EV_abz0(ai,bi,zi) = -EV_train_func(a_star)
                endif
                a_opt1(ai,bi,zi) = a_star
            enddo
        enddo
    enddo
end subroutine policy1


subroutine policy2
	implicit none
integer :: ai, bi, zi
real(8) :: b1, b2, b_star
real(8) :: y0, y1
    y1 = EV_n1
    do zi = 1,Nz
        ! Technology adoption
        do ai = 1,Na
            Vb_tmp = beta_p*(1d0-delta_p)*EV_abz(ai,:,zi)
            call spline(Nb,b_grid,Vb_tmp,0d0,0d0,Vb2_tmp)
            b_opt0(ai,:,zi) = b_grid; b_opt1(ai,:,zi) = b_grid
            b_star = b_grid(1)
            do bi = 1,Nb-1
                b0 = b_grid(bi); b_star = max(b0,b_star); b1 = (b_star+b_max)/2d0; flag = 0
                y0 = Vb_tmp(bi); y1 = Vb_func(b0+TOL_Brent)
                if (y1>y0) call Brent_Min(EV_tech_func,b_star,b1,b_max,TOL_Brent,b2,flag)
                if (flag==1 .and. b2>b0+TOL_Brent) then
                    b_star = b2
                    EV_abz0(ai,bi,zi) = -EV_tech_func(b_star)
                endif
                b_opt1(ai,bi,zi) = b_star
            enddo
        enddo
    enddo
end subroutine policy2


subroutine policy3
	implicit none
integer :: ai, bi, zi, iter
real(8) :: a1, a2, a_star, b1, b2, b_star
real(8) :: y0, y1

    y1 = EV_n1
    do zi = 1,Nz
        Vab_tmp = beta_p*(1d0-delta_p)*EV_abz(:,:,zi)
        
        do iter = 1,iter_max
            ! Training
            do bi = 1,Nb
                ! Solve policy
                Va_tmp = Vab_tmp(:,bi)
                call spline(Na,a_grid,Va_tmp,0d0,0d0,Va2_tmp)
                a_opt0(:,bi,zi) = a_grid; a_opt1(:,bi,zi) = a_grid
                a_star = a_grid(1)
                do ai = 1,Na-1
                    a0 = a_grid(ai); a_star = max(a0,a_star); a1 = (a_star+a_max)/2d0; flag = 0
                    y0 = Va_tmp(ai)
                    y1 = Va_func(a0+TOL_Brent)
                    if (y1>y0) call Brent_Min(EV_train_func,a_star,a1,a_max,TOL_Brent,a2,flag)
                    if (flag==1 .and. a2>a0+TOL_Brent) a_star = a2
                    a_opt1(ai,bi,zi) = a_star
                enddo
                ! Solve value
                Va_tmp = EV_abz(:,bi,zi)
                call spline(Na,a_grid,Va_tmp,0d0,0d0,Va2_tmp)
                do ai = 1,Na-1
                    a0 = a_grid(ai)
                    a_star = a_opt1(ai,bi,zi)
                    if (a_star>a0+TOL_Brent) Vab_tmp(ai,bi) = Va_func(a_star)
                enddo
            enddo
            
            ! Technology adoption
            do ai = 1,Na
                ! Solve policy
                Vb_tmp = Vab_tmp(ai,:)
                call spline(Nb,b_grid,Vb_tmp,0d0,0d0,Vb2_tmp)
                b_opt0(ai,:,zi) = b_grid; b_opt1(ai,:,zi) = b_grid
                b_star = b_grid(1)
                do bi = 1,Nb-1
                    b0 = b_grid(bi); b_star = max(b0,b_star); b1 = (b_star+b_max)/2d0; flag = 0
                    y0 = Vb_tmp(bi)
                    y1 = Vb_func(b0+TOL_Brent)
                    if (y1>y0) call Brent_Min(EV_tech_func,b_star,b1,b_max,TOL_Brent,b2,flag)
                    if (flag==1 .and. b2>b0+TOL_Brent) b_star = b2
                    b_opt1(ai,bi,zi) = b_star
                enddo
                ! Solve value
                Vb_tmp = EV_abz(ai,:,zi)
                call spline(Nb,b_grid,Vb_tmp,0d0,0d0,Vb2_tmp)
                do bi = 1,Nb-1
                    b0 = b_grid(bi)
                    b_star = b_opt1(ai,bi,zi)
                    if (b_star>b0+TOL_Brent) Vab_tmp(ai,bi) = Vb_func(b_star)
                enddo
            enddo
            
            ! Check convergence
            sup1 = maxval(abs(a_opt1-a_opt0))/maxval(abs(a_opt0))
            sup2 = maxval(abs(b_opt1-b_opt0))/maxval(abs(b_opt0))
 
            if (max(sup1,sup2)<TOL_Brent) then
                Vab_tmp = EV_abz(:,:,zi)
                do bi = 1,Nb
                    do ai = 1,Na
                        a0 = a_grid(ai); b0 = b_grid(bi)
                        a_star = a_opt1(ai,bi,zi); b_star = b_opt1(ai,bi,zi)
                        if (a_star>a0+TOL_Brent .or. b_star>b0+TOL_Brent) EV_abz0(ai,bi,zi) = -EV_train_tech_func(a_star,b_star)
                    enddo
                enddo
                exit
            endif
            
            ! Update
            a_opt0 = a_opt1; b_opt0 = b_opt1
        enddo
    enddo
end subroutine policy3


end subroutine Solve_PF

! ======================================================================

subroutine Solve_VF
    implicit none
integer :: ai, bi, bip, zi
real(8) :: a1, a2, b1, b2, y0, y1
    
    ! Value function
    EV_UE = EV_abz0
    do zi = 1,Nz
        do bi = 1,Nb
            do ai = 1,Na
                y0 = EV_abz(ai,bi,zi)
                EV_EE(ai,bi,:,zi) = max(EV_abz(ai,:,zi)-y0,zeros_b)
            enddo
        enddo
    enddo
    EV_UEa = 0d0; EV_UEb = 0d0
    Match_a1a2 = 0d0; Match_b1b2 = 0d0; Trans_b1b2 = 0d0
        
    do zi = 1,Nz
        do bi = 1,Nb
            Va_tmp = EV_UE(:,bi,zi)
            call spline(Na,a_grid,Va_tmp,0d0,0d0,Va2_tmp)
            
            do ai = 1,Na
                a0 = a_grid(ai); a_min = a0*(lambda_a-1d0)/lambda_a
                call GauLeg_inf(V_Pareto_a_func,N_GauLeg,0d0,1d0/a_min,y1)
                EV_UEa(ai,bi,zi) = y1
            enddo
            
            Va_tmp = V_abz(:,bi,zi)
            call match(Na,a_grid,a1,a2)
            Match_a1a2(1,bi,zi) = a1; Match_a1a2(2,bi,zi) = a2
        enddo
    enddo
    
    do zi = 1,Nz
        do ai = 1,Na
            Vb_tmp = EV_UE(ai,:,zi)
            call spline(Nb,b_grid,Vb_tmp,0d0,0d0,Vb2_tmp)
            
            do bi = 1,Nb
                b0 = b_grid(bi); b_min = b0*(lambda_b-1d0)/lambda_b
                call GauLeg_inf(V_Pareto_b_func,N_GauLeg,0d0,1d0/b_min,y1)
                EV_UEb(ai,bi,zi) = y1
            enddo

            Vb_tmp = V_abz(ai,:,zi)
            call match(Nb,b_grid,b1,b2)
            Match_b1b2(ai,1,zi) = b1; Match_b1b2(ai,2,zi) = b2
        enddo
    enddo
    
    do zi = 1,Nz
        do bi = 1,Nb
            do ai = 1,Na
                Vb_tmp = EV_EE(ai,bi,:,zi)
                call spline(Nb,b_grid,Vb_tmp,0d0,0d0,Vb2_tmp)
                
                do bip = 1,Nb
                    b0 = b_grid(bip); b_min = b0*(lambda_b-1d0)/lambda_b
                    call GauLeg_inf(V_Pareto_b_func,N_GauLeg,0d0,1d0/b_min,y1)
                    EV_EEb(ai,bi,bip,zi) = y1
                enddo
                
                call match(Nb,b_grid,b1,b2)
                Trans_b1b2(ai,bi,1,zi) = b1; Trans_b1b2(ai,bi,1,zi) = b2
            enddo
        enddo
	enddo

contains

subroutine match(N,grid,PF1,PF2)
    implicit none
integer, intent(in) :: N
real(8), intent(in) :: grid(N)
real(8), intent(out) :: PF1, PF2
integer :: p1, p2
real(8) :: sup, x1, x2

    sup = maxval(abs(grid-a_grid))
    if (N==Na .and. sup==0) then
        call find_interval(N,Va_tmp,p1,p2)
        if (p1==0 .or. p2==0) then
            PF1 = 0d0; PF2 = 0d0
            return
        endif
        if (p1==1) then
            PF1 = grid(1)
        else
            x1 = grid(p1-1); x2 = grid(p1)
            PF1 = Brent_Root(Va_func,x1,x2,TOL_Brent,flag)
        endif
        if (p2==N) then
            PF2 = grid(N)
        else
            x1 = grid(p2); x2 = grid(p2+1)
            PF2 = Brent_Root(Va_func,x1,x2,TOL_Brent,flag)
        endif
    endif
    
    sup = maxval(abs(grid-b_grid))
    if (N==Nb .and. sup==0) then
        call find_interval(N,Vb_tmp,p1,p2)
        if (p1==0 .or. p2==0) then
            PF1 = 0d0; PF2 = 0d0
            return
        endif
        if (p1==1) then
            PF1 = grid(1)
        else
            x1 = grid(p1-1); x2 = grid(p1)
            PF1 = Brent_Root(Vb_func,x1,x2,TOL_Brent,flag)
        endif
        if (p2==N) then
            PF2 = grid(N)
        else
            x1 = grid(p2); x2 = grid(p2+1)
            PF2 = Brent_Root(Vb_func,x1,x2,TOL_Brent,flag)
        endif
    endif

end subroutine match

end subroutine Solve_VF


! ======================================================================

subroutine Spline_Coefficient
    implicit none
    
    call spline_VF
    if (phi1>0d0) call spline_PF_a
    if (phi3>0d0) call spline_PF_b

contains

subroutine spline_VF
    implicit none
integer :: ai, bi, zi
    do zi = 1,Nz
        Vab_tmp = V_abz(:,:,zi)
        call splie2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp)
        V_abz2(:,:,zi) = Vab2_tmp
        
        Vab_tmp = EV_abz0(:,:,zi)
        call splie2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp)
        EV_abz02(:,:,zi) = Vab2_tmp
        
        Vab_tmp = EV_abz(:,:,zi)
        call splie2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp)
        EV_abz2(:,:,zi) = Vab2_tmp
        
        Vab_tmp = EP_abz(:,:,zi)
        call splie2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp)
        EP_abz2(:,:,zi) = Vab2_tmp
        
        Vab_tmp = Disutility(:,:,zi)
        call splie2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp)
        Disutility2(:,:,zi) = Vab2_tmp
        
        do ai = 1,Na
            Vb_tmp = EV_UEa(ai,:,zi)
            call spline(Nb,b_grid,Vb_tmp,0d0,0d0,Vb2_tmp)
            EV_UEa2(ai,:,zi) = Vb2_tmp
        enddo
        
        do bi = 1,Nb
            Va_tmp = EV_UEb(:,bi,zi)
            call spline(Na,a_grid,Va_tmp,0d0,0d0,Va2_tmp)
            EV_UEb2(:,bi,zi) = Va2_tmp
            
            Vab_tmp = EV_EEb(:,:,bi,zi)
            call splie2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp)
            EV_EEb2(:,:,bi,zi) = Vab2_tmp
        enddo
    enddo

end subroutine spline_VF

subroutine spline_PF_a
    implicit none
integer :: zi
    do zi = 1,Nz
        Vab_tmp = a_opt1(:,:,zi)
        call splie2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp)
        a_opt2(:,:,zi) = Vab2_tmp
    enddo
end subroutine spline_PF_a


subroutine spline_PF_b
    implicit none
integer :: zi
    do zi = 1,Nz
        Vab_tmp = b_opt1(:,:,zi)
        call splie2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp)
        b_opt2(:,:,zi) = Vab2_tmp
    enddo
end subroutine spline_PF_b

end subroutine Spline_Coefficient


! ======================================================================
! Export Result
! ======================================================================

subroutine Export_VF
    implicit none
integer :: ai, bi

    write(Filename,'("Vu",I3,".txt")') iunit
    open(unit=2, file=Filename, status='replace')
        write(2,'(F12.4)') V_u
    close(2)

    write(Filename,'("V_abz",I3,".txt")') iunit
    write(FMT,'("(",I3,"F12.4)")') Nb
    open(unit=2, file=Filename, status='replace')
        do ai = 1,Na
            write(2,FMT) V_abz(ai,:,6)
        enddo
    close(2)

    write(Filename,'("EV_abz",I3,".txt")') iunit
    write(FMT,'("(",I3,"F12.4)")') Nb
    open(unit=2, file=Filename, status='replace')
        do ai = 1,Na
            write(2,FMT) EV_abz(ai,:,6)
        enddo
    close(2)

    write(Filename,'("EV_UE",I3,".txt")') iunit
    write(FMT,'("(",I3,"F12.4)")') Nb
    open(unit=2, file=Filename, status='replace')
        do ai = 1,Na
            write(2,FMT) EV_UE(ai,:,6)
        enddo
    close(2)

    write(Filename,'("Disutility",I3,".txt")') iunit
    write(FMT,'("(",I3,"F12.4)")') Nb
    open(unit=2, file=Filename, status='replace')
        do ai = 1,Na
            write(2,FMT) Disutility(ai,:,6)
        enddo
    close(2)

    write(Filename,'("Match_PF",I3,"_1.txt")') iunit
    open(unit=2, file=Filename, status='replace')
        write(2,'(A4,3A8)') 'ai', 'a_grid', 'b_low', 'b_high'
        do ai = 1,Na
            write(2,'(I4,3F8.3)') ai, a_grid(ai), match_b1b2(ai,1,6), match_b1b2(ai,2,6)
        enddo
    close(2)

    write(Filename,'("Match_PF",I3,"_2.txt")') iunit
    open(unit=2, file=Filename, status='replace')
        write(2,'(A4,3A8)') 'bi', 'b_grid', 'a_low', 'a_high'
        do bi = 1,Nb
            write(2,'(I4,3F8.3)') bi, b_grid(bi), match_a1a2(1,bi,6), match_a1a2(2,bi,6)
        enddo
    close(2)

    write(Filename,'("LF_PF",I3,"_1.txt")') iunit
    open(unit=2, file=Filename, status='replace')
        write(2,'(F12.4)') PF_LF1
    close(2)

    write(Filename,'("LF_PF",I3,"_2.txt")') iunit
    write(FMT,'("(",I3,"F12.4)")') Nb
    open(unit=2, file=Filename, status='replace')
        do ai = 1,Na
            write(2,FMT) PF_LF2(ai,:,6)
        enddo
    close(2)

    if (phi1>0d0) call export_PF_a
    if (phi3>0d0) call export_PF_b

contains

subroutine export_PF_a
    implicit none
integer :: ai

    write(Filename,'("PF_a",I3,".txt")') iunit
    write(FMT,'("(",I3,"F8.3)")') Nb
    open(unit=2, file=Filename, status='replace')
        do ai = 1,Na
            write(2,FMT) a_opt1(ai,:,6)
        enddo
    close(2)

end subroutine export_PF_a


subroutine export_PF_b
    implicit none
integer :: ai

    write(Filename,'("PF_b",I3,".txt")') iunit
    write(FMT,'("(",I3,"F8.3)")') Nb
    open(unit=2, file=Filename, status='replace')
        do ai = 1,Na
            write(2,FMT) b_opt1(ai,:,6)
        enddo
    close(2)
    
end subroutine export_PF_b

end subroutine Export_VF


! ======================================================================
! Functions
! ======================================================================

real(8) function prod1(a0,b0,z)
    implicit none
real(8), intent(in) :: a0, b0, z
real(8) :: f0

    f0 = kappa*a0+(1d0-kappa)*b0
    if (b0>a0) then
        prod1 = z*f0-alpha_u*(b0-a0)**2d0/a0
    else
        prod1 = z*f0
    endif
end function prod1


real(8) function prod2(a0,b0,z)
    implicit none
real(8), intent(in) :: a0, b0, z
real(8) :: f0

    f0 = kappa*a0+(1d0-kappa)*b0
    if (b0>a0) then
        prod2 = z*f0-alpha_u*(b0-a0)**2d0/a0
    else
        prod2 = z*f0-alpha_o*(a0-b0)**2d0/a0
    endif
end function prod2


real(8) function prod_left(b)
    implicit none
real(8), intent(in) :: b
    prod_left = prod2(a_min,b,1d0)-b_UI
end function prod_left


real(8) function prod_right(b)
    implicit none
real(8), intent(in) :: b
    prod_right = prod2(a_max,b,1d0)-b_UI
end function prod_right


real(8) function EV_train_func(a)
	implicit none
real(8), intent(in) :: a
real(8) :: EV
    EV = Va_func(a)
	EV_train_func = EV-Cost_train_func(a0,a)
	EV_train_func = -EV_train_func
end function EV_train_func


real(8) function Cost_train_func(a0,a)
	implicit none
real(8), intent(in) :: a0, a
	Cost_train_func = phi1*(a-a0)**2d0/a0
end function Cost_train_func


real(8) function EV_school_func(a)
	implicit none
real(8), intent(in) :: a
real(8) :: EV
    EV = Va_func(a)
	EV_school_func = EV-Cost_school_func(mu_a,a)
	EV_school_func = -EV_school_func
end function EV_school_func


real(8) function Cost_school_func(mu_a,a)
	implicit none
real(8), intent(in) :: mu_a, a
	Cost_school_func = phi2*(a-mu_a)**2d0/mu_a
end function Cost_school_func


real(8) function EV_tech_func(b)
    implicit none
real(8), intent(in) :: b
real(8) :: EV
    EV = Vb_func(b)
    EV_tech_func = EV-Cost_tech_func(b0,b)
    EV_tech_func = -EV_tech_func
end function EV_tech_func


real(8) function Cost_tech_func(b0,b)
    implicit none
real(8), intent(in) :: b0, b
real(8) :: EV
    Cost_tech_func = phi3*(b-b0)**2d0/b0
end function Cost_tech_func


real(8) function EV_innov_func(b)
    implicit none
real(8), intent(in) :: b
real(8) :: EV
    EV = Vb_func(b)
    EV_innov_func = EV-Cost_innov_func(mu_b,b)
    EV_innov_func = -EV_innov_func
end function EV_innov_func


real(8) function Cost_innov_func(mu_b,b)
    implicit none
real(8), intent(in) :: mu_b, b
real(8) :: EV
    Cost_innov_func = phi4*(b-mu_b)**2d0/mu_b
end function Cost_innov_func


real(8) function EV_train_tech_func(a,b)
	implicit none
real(8), intent(in) :: a, b
real(8) :: EV
    EV = Vab_func(a,b)
	EV_train_tech_func = EV-Cost_train_func(a0,a)-Cost_tech_func(b0,b)
	EV_train_tech_func = -EV_train_tech_func
end function EV_train_tech_func


real(8) function Va_func(a)
    implicit none
real(8), intent(in) :: a
    call splint(Na,a_grid,Va_tmp,Va2_tmp,a,Va_func)
end function Va_func


real(8) function Vb_func(b)
    implicit none
real(8), intent(in) :: b
    call splint(Nb,b_grid,Vb_tmp,Vb2_tmp,b,Vb_func)
end function Vb_func


real(8) function Vab_func(a,b)
    implicit none
real(8), intent(in) :: a, b
    call splin2(Na,Nb,a_grid,b_grid,Vab_tmp,Vab2_tmp,a,b,Vab_func)
end function Vab_func


real(8) function Pareto_a(a)
    implicit none
real(8), intent(in) :: a
    if (a<a_min) then
        Pareto_a = 0d0
    else
        Pareto_a = lambda_a*a_min**lambda_a/a**(lambda_a+1d0)
    endif
end function Pareto_a


real(8) function Pareto_b(b)
    implicit none
real(8), intent(in) :: b
    if (b<b_min) then
        Pareto_b = 0d0
    else
        Pareto_b = lambda_b*b_min**lambda_b/b**(lambda_b+1d0)
    endif
end function Pareto_b


real(8) function Pareto_a_cdf(a)
    implicit none
real(8), intent(in) :: a
    if (a<a_min) then
        Pareto_a_cdf = 0d0
    else
        Pareto_a_cdf = 1d0-(a_min/a)**lambda_a
    endif
end function Pareto_a_cdf


real(8) function Pareto_b_cdf(b)
    implicit none
real(8), intent(in) :: b
    if (b<b_min) then
        Pareto_b_cdf = 0d0
    else
        Pareto_b_cdf = 1d0-(b_min/b)**lambda_b
    endif
end function Pareto_b_cdf


real(8) function V_Pareto_a_func(a)
    implicit none
real(8), intent(in) :: a
real(8) :: Va
    call splint(Na,a_grid,Va_tmp,Va2_tmp,a,Va)
    V_Pareto_a_func = Va*Pareto_a(a)
end function V_Pareto_a_func


real(8) function V_Pareto_b_func(b)
    implicit none
real(8), intent(in) :: b
real(8) :: Vb
    call splint(Nb,b_grid,Vb_tmp,Vb2_tmp,b,Vb)
    V_Pareto_b_func = Vb*Pareto_b(b)
end function V_Pareto_b_func


end module Value_Function
