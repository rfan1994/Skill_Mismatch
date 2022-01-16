!
!   My_Function.f90
!   Skill_Mismatch
!
!   Created by FanRong on 1/28/20.
!   Copyright 2020 FanRong. All rights reserved.
!
module My_Function
use Control
use Global_Variable
    implicit none

public :: linspace, discrete_Pareto, discrete_Pareto2, discrete_AR1, discrete_Normal
public :: truncate_dist, cdf_normal, rouwenhorst_matrix
public :: find_interval
public :: cubicspline, spline, splint, splie2, splin2
public :: lagrange_interp_1d, lagrange_interp_2d
public :: Interpolate, Interpolate_1d, Interpolate_2d
public :: Bisection, Brent_Root, Brent_Min, Amoeba
public :: Random_Normal, Random_Pareto, Random_Binomial
public :: Random_Normal_Scalar, Random_Pareto_Scalar, Random_Binomial_Scalar
public :: mean, variance, correlation

contains

! ======================================================================
! Discretize
! ======================================================================

subroutine linspace(x_min,x_max,N,y)
    implicit none
real(8), intent(in) :: x_min, x_max
integer, intent(in) :: N
real(8), intent(out) :: y(N)
integer :: i
real(8) :: h
    h = (x_max-x_min)/dble(N-1)
    do i = 1,N
        y(i) = x_min+h*dble(i-1)
    enddo
end subroutine linspace


subroutine discrete_Pareto(x_min,x_max,lambda,N,grid,pdf,cdf)
    implicit none
real(8), intent(in) :: x_min, x_max, lambda                     ! Pareto distribution parameter
integer, intent(in) :: N                                        ! Number of points
real(8), dimension(N), intent(out) :: grid, pdf, cdf
real(8) :: h, x
integer :: i
    
    h = (1d0/x_min)/dble(N-2)
    do i = 1,N
        grid(N+1-i) = 1d0/x_max+dble(i-1)*h
    enddo
    grid = 1d0/grid
    do i = 1,N-1
        x = grid(i)
        if (x<x_min) then
            cdf(i) = 0d0
        else
            cdf(i) = 1d0-(x_min/x)**lambda
        endif 
    enddo
    cdf(N) = 1d0

    pdf(1) = cdf(1)
    do i = 2,N
        pdf(i) = cdf(i)-cdf(i-1)
    enddo
    pdf = pdf/sum(pdf)
    
    cdf(1) = pdf(1)
    do i = 2,N
        cdf(i) = cdf(i-1)+pdf(i)
    enddo
end subroutine discrete_Pareto


subroutine discrete_Pareto2(x_min,lambda,N,grid,pdf,cdf)
    implicit none
real(8), intent(in) :: x_min, lambda                            ! Pareto distribution parameter
integer, intent(in) :: N                                        ! Number of points
real(8), dimension(N), intent(in) :: grid
real(8), dimension(N), intent(out) :: pdf, cdf
real(8) :: x
integer :: i, p0, p1, p
    p0 = 1; p1 = N
    call Interpolate(N,grid,x_min,p0,p1)
    if (p1==N) then
        pdf = 0d0; cdf = 0d0
        pdf(N) = 1d0; cdf(N) = 1d0
        return
    endif
    if (p0<p1) then
        p = p1
    elseif (p0==p1) then
        p = p1+1
    endif
    
    do i = p,N-1
        x = grid(i)
        cdf(i) = 1d0-(x_min/x)**lambda
    enddo
    cdf(N) = 1d0

    pdf(1) = cdf(1)
    do i = 2,N
        pdf(i) = cdf(i)-cdf(i-1)
    enddo
    pdf = pdf/sum(pdf)
    
    cdf(1) = pdf(1)
    do i = 2,N
        cdf(i) = cdf(i-1)+pdf(i)
    enddo
end subroutine discrete_Pareto2


subroutine discrete_AR1(sigma,rho,N,grid,trans)
    implicit none
real(8), intent(in) :: rho, sigma                       ! Persistence and variacne
integer, intent(in) :: N                                ! Number of points
real(8), intent(out) :: grid(N),trans(N,N)
integer :: i
real(8) :: y0
    y0 = sigma*sqrt(dble(N-1)/(1d0-rho**2))
    do i = 1,N
        grid(i) = -y0+y0*2d0*dble(i-1)/dble(N-1)
    enddo
    call rouwenhorst_matrix(rho,trans)
    do i = 1,N
        trans(:,i) = trans(:,i)/sum(trans(:,i))
    enddo
    grid = exp(grid)
end subroutine discrete_AR1


subroutine discrete_Normal(mu,sigma,N,grid,pdf,cdf)
    implicit none
real(8), intent(in) :: mu, sigma                        ! Persistence and variacne
integer, intent(in) :: N                                ! Number of points
real(8), intent(in) :: grid(N)
real(8), dimension(N), intent(out) :: pdf, cdf
real(8) :: y0
integer :: i
    do i = 1,N-1
        y0 = (grid(i)+grid(i+1))/2d0
        cdf(i) = cdf_normal((y0-mu)/sigma)
    enddo
    cdf(N) = 1d0
    do i = 2,N
        pdf(i) = cdf(i)-cdf(i-1)
    enddo
    pdf(1) = cdf(1)
end subroutine discrete_Normal


subroutine truncate_dist(N,lim,pdf,cdf,N1,N2)
    implicit none
integer, intent(in) :: N
real(8), intent(in) :: lim
integer, intent(out) :: N1, N2
real(8), intent(inout) :: pdf(N), cdf(N)
integer :: i
    do i = 1,N
        if (pdf(i)<lim) then
            pdf(i) = 0d0
        else
            N1 = i
            exit
        endif
    enddo

    do i = 1,N
        if (pdf(N+1-i)<lim) then
            pdf(N+1-i) = 0d0
        else
            N2 = N+1-i
            exit
        endif
    enddo
    pdf = pdf/sum(pdf)

    cdf(1) = pdf(1)
    do i = 2,N
        cdf(i) = cdf(i-1)+pdf(i)
    enddo
end subroutine truncate_dist


function cdf_normal(x)
! David Hill,
! Algorithm AS 66: The Normal Integral,
! Applied Statistics,
! Volume 22, Number 3, 1973, pages 424-427.
    implicit none
real(8), intent(in) :: x
real(8) :: cdf_normal, xabs, y
real(8), parameter :: a0  = 0.5d0
real(8), parameter :: a1  = 0.398942280444d0
real(8), parameter :: a2  = 0.399903438504d0
real(8), parameter :: a3  = 5.75885480458d0
real(8), parameter :: a4  = 29.8213557808d0
real(8), parameter :: a5  = 2.62433121679d0
real(8), parameter :: a6  = 48.6959930692d0
real(8), parameter :: a7  = 5.92885724438d0
real(8), parameter :: b0  = 0.398942280385d0
real(8), parameter :: b1  = 3.8052d-8
real(8), parameter :: b2  = 1.00000615302d0
real(8), parameter :: b3  = 3.98064794d-4
real(8), parameter :: b4  = 1.98615381364d0
real(8), parameter :: b5  = 0.151679116635d0
real(8), parameter :: b6  = 5.29330324926d0
real(8), parameter :: b7  = 4.8385912808d0
real(8), parameter :: b8  = 15.1508972451d0
real(8), parameter :: b9  = 0.742380924027d0
real(8), parameter :: b10 = 30.789933034d0
real(8), parameter :: b11 = 3.99019417011d0

    ! calculate absolute value and quadratic
    xabs = abs(x)
    y = a0*x**2

    ! choose the right interval for calculation
    if (xabs <= 1.28d0)then
        cdf_normal = a0-xabs*(a1-a2*y/(y+a3-a4/(y+a5+a6/(y+a7))))
    elseif (xabs <= 12.7d0)then
        cdf_normal = b0*exp(-y)/(xabs-b1+b2/(xabs+b3+b4/(xabs-b5+b6/(xabs+b7-b8/ &
                    (xabs+b9+b10/(xabs+b11))))))
    else
        cdf_normal = 0d0
    endif

    ! transform if other side of the bell
    if (x>0d0) cdf_normal = 1d0-cdf_normal

end function cdf_normal


recursive subroutine rouwenhorst_matrix(rho, pi_new)
    implicit none
real(8), intent(in) :: rho
real(8), intent(out) :: pi_new(:, :)
integer :: n
real(8) :: p, pi_old(size(pi_new,1)-1, size(pi_new,2)-1)

    n = size(pi_new, 1)
    p = (1d0 + rho)/2d0

    if (n == 2) then
        pi_new(1, :) = [p, 1d0-p]
        pi_new(2, :) = [1d0-p, p]
    else
        call rouwenhorst_matrix(rho, pi_old)
        pi_new = 0d0

        pi_new(1:n-1, 1:n-1) = pi_new(1:n-1, 1:n-1) + p*pi_old
        pi_new(1:n-1, 2:n  ) = pi_new(1:n-1, 2:n  ) + (1d0-p)*pi_old
        pi_new(2:n  , 1:n-1) = pi_new(2:n  , 1:n-1) + (1d0-p)*pi_old
        pi_new(2:n  , 2:n  ) = pi_new(2:n  , 2:n  ) + p*pi_old

        pi_new(2:n-1, :) = pi_new(2:n-1, :)/2d0
    endif
end subroutine rouwenhorst_matrix


! ======================================================================
! Operations
! ======================================================================

subroutine find_interval(N,xvec,p0,p1)
    implicit none
real(8), intent(in) :: xvec(N)
integer, intent(in) :: N
integer, intent(out) :: p0, p1
integer :: i, j
    p0 = 0; p1 = 0
    do i = 1,N
        if (xvec(i)>0) then
            p0 = i; p1 = N
            exit
        endif
    enddo
    
    if (p0==0) then
        p1 = 0
        return
    endif
    
    do i = p0,N
        j = N-i+p0
        if (xvec(j)>0) then
            p1 = j
            exit
        endif
    enddo
end subroutine find_interval

! ======================================================================
! Function approximation
! ======================================================================

subroutine cubicspline(xa,ya,n,a,b,c,d)
    implicit none
! Data points
integer, intent(in) :: n
real(8), dimension(n), intent(in) :: xa, ya
! Cubic coefficient
real(8), dimension(n-1), intent(out) :: a, b, c, d
! Iterator
integer :: iter
! Others
real(8) :: alpha(2:n-1)
real(8), dimension(n-1) :: mu, z
real(8) :: dx, dxl, dy, dyl, ddx, l, mumu, zz, c0, c1

    ! Solve cubic coefficient
    a = ya(1:n-1)

    do iter = 2,n-1
        dx = xa(iter+1)-xa(iter)
        dxl = xa(iter)-xa(iter-1)
        dy = ya(iter+1)-ya(iter)
        dyl = ya(iter)-ya(iter-1)

        alpha(iter) = 3d0*dy/dx-3d0*dyl/dxl
    enddo

    mu(1) = 0d0
    z(1) = 0d0

    do iter = 2,n-1
        mumu = mu(iter-1)
        zz = z(iter-1)
        ddx = xa(iter+1)-xa(iter-1)
        dx = xa(iter+1)-xa(iter)
        dxl = xa(iter)-xa(iter-1)

        l = 2d0*ddx-dxl*mumu
        mu(iter) = dx/l
        z(iter) = (alpha(iter)-dxl*zz)/l
    enddo

    dy = ya(n)-ya(n-1)
    dx = xa(n)-xa(n-1)

    c(n-1) = z(n-1)
    b(n-1) = dy/dx-dx*2d0*c(n-1)/3d0
    d(n-1) = -c(n-1)/(3d0*dx)
    c1 = c(n-1)

    do iter = 2,n-1
        dy = ya(n-iter+1)-ya(n-iter)
        dx = xa(n-iter+1)-xa(n-iter)
        c0 = z(n-iter)-mu(n-iter)*c1
        c(n-iter) = c0
        b(n-iter) = dy/dx-dx*(c1+2d0*c0)/3d0
        d(n-iter) = (c1-c0)/(3d0*dx)
        c1 = c0
    enddo

end subroutine cubicspline


subroutine spline(N,xa,ya,yp1,ypn,y2a)
    implicit none
integer, intent(in) :: N                        ! Length of data
real(8), intent(in) :: xa(N), ya(N)             ! Data point y = f(x)
real(8), intent(in) :: yp1, ypn                 ! Derivative at point x1 and xn
real(8), intent(out) :: y2a(N)                  ! Second derivatives
integer, parameter :: Nmax = 500                ! Largest anticipated value of n
integer :: i, k                                 ! Iterator
real(8) :: p, qn, sig, un, u(Nmax)

    y2a(1) = -0.5d0
    u(1) = (3d0/(xa(2)-xa(1)))*((ya(2)-ya(1))/(xa(2)-xa(1))-yp1)
    do i = 2,N-1                                ! Decomposition loop
        sig = (xa(i)-xa(i-1))/(xa(i+1)-xa(i-1))
        p = sig*y2a(i-1)+2d0
        y2a(i) = (sig-1d0)/p
        u(i) = (6d0*((ya(i+1)-ya(i))/(xa(i+1)-xa(i))-(ya(i)-ya(i-1))/(xa(i)-xa(i-1)))/(xa(i+1)-xa(i-1))-sig*u(i-1))/p
    enddo

    qn = 0.5d0
    un = (3d0/(xa(N)-xa(N-1)))*(ypn-(ya(N)-ya(N-1))/(xa(N)-xa(N-1)))
    y2a(N) = (un-qn*u(N-1))/(qn*y2a(N-1)+1d0)
    do k = N-1,1,-1                             ! Backsubstitution loop
        y2a(k) = y2a(k)*y2a(k+1)+u(k)
    enddo
end subroutine spline


subroutine splint(N,xa,ya,y2a,x,y)
integer, intent(in) :: N
real(8), intent(in) :: xa(N),ya(N),y2a(N)   ! Data point yi = f(xi); y2a from spline
real(8), intent(in) :: x                    ! Point to Interpolate
real(8), intent(out) :: y                   ! Interpolated value
integer :: khi, klo
real(8) :: a, b, h, h1, h2

    klo = 1; khi = N
    call Interpolate(N,xa,x,klo,khi)
    h = xa(khi)-xa(klo)
    h1 = h; h2 = ya(khi)-ya(klo)
    if (h1==0d0 .or. h2==0d0) then
        y = ya(khi)
        return
    endif
    a = (xa(khi)-x)/h                       ! Cubic spline polynomial is now evaluated.
    b = (x-xa(klo))/h
    y = a*ya(klo)+b*ya(khi)+((a**3d0-a)*y2a(klo)+(b**3d0-b)*y2a(khi))*(h**2d0)/6d0
end subroutine splint


subroutine splie2(M,N,x1a,x2a,ya,y2a)
    implicit none
integer, intent(in) :: M, N                         ! Length of data
real(8), intent(in) :: x1a(M), x2a(N), ya(M,N)      ! Data point y = f(x1,x2)
real(8), intent(out) :: y2a(M,N)                    ! Second derivatives at (x1,x2)
integer :: j                                        ! Iterator
    do j = 1,M
        call spline(N,x2a,ya(j,:),0d0,0d0,y2a(j,:))
    enddo
end subroutine splie2


subroutine splin2(M,N,x1a,x2a,ya,y2a,x1,x2,y)
    implicit none
integer, intent(in) :: M, N                                 ! Length of data
real(8), intent(in) :: x1a(M),x2a(N),y2a(M,N),ya(M,N)       ! Data point y = f(x1,x2); y2a from splie2
real(8), intent(in) :: x1, x2                               ! Point to Interpolate
real(8), intent(out) :: y                                   ! Interpolated value
integer, parameter :: NN = 100                              ! Largest anticipated value of M and N
integer :: j
real(8) :: y2tmp(NN), yytmp(NN)
    do j = 1,M
        call splint(N,x2a,ya(j,:),y2a(j,:),x2,yytmp(j))
    enddo
    call spline(M,x1a,yytmp,0d0,0d0,y2tmp)
    call splint(M,x1a,yytmp,y2tmp,x1,y)
end subroutine splin2


subroutine lagrange_basis_1d(nd,xd,xi,lb)
    implicit none
integer, intent(in) :: nd                                           ! Number of data points
real(8), intent(in) :: xd(nd)                                       ! Data
real(8), intent(in) :: xi                                           ! Evaluation points
real(8), intent(out) :: lb(nd)
integer :: i
    do i = 1,nd
        lb(i) = product((xi-xd(1:i-1))/(xd(i)-xd(1:i-1)))*                    &
                product((xi-xd(i+1:nd))/(xd(i)-xd(i+1:nd)))
    enddo
end subroutine lagrange_basis_1d


subroutine lagrange_basis_2d(mx,xd,i,xi,yi)
    implicit none
integer, intent(in) :: mx                       ! Degree of the basis function
real(8), dimension(mx+1), intent(in) :: xd      ! Interpolation nodes
integer, intent(in) :: i                        ! Index of the basis function
real(8), intent(in) :: xi                       ! Evaluation point
real(8), intent(out) :: yi                      ! Evaluation value of the i-th Lagrange
integer :: j
    yi = 1d0
    if (xi .ne. xd(i)) then
        do j = 1,mx+1
            if (j .ne. i) then
                yi = yi*(xi-xd(j))/(xd(i)-xd(j))
            end if
        enddo
    end if
end subroutine lagrange_basis_2d


subroutine lagrange_interp_1d(nd,xd,yd,xi,yi)
    implicit none
integer, intent(in) :: nd                                           ! Number of data points
real(8), dimension(nd), intent(in) :: xd, yd                        ! Data
real(8), intent(in) :: xi                                           ! Interpolation points
real(8), intent(out) :: yi                                          ! Interpolated values
real(8) :: lb(nd)
    call lagrange_basis_1d(nd,xd,xi,lb)
    yi = sum(lb*yd)
end subroutine lagrange_interp_1d


subroutine lagrange_interp_2d(mx,my,xd_1d,yd_1d,zd,xi,yi,zi)
    implicit none
integer, intent(in) :: mx, my                                           ! Polynomial degree in x and y
real(8), intent(in) :: xd_1d(mx+1), yd_1d(my+1), zd(mx+1,my+1)          ! Data
real(8), intent(in) :: xi, yi                                           ! 2D interpolation points
real(8), intent(out) :: zi                                              ! Interpolated values
integer :: i, j
real(8) :: lx, ly
    zi = 0d0
    do i = 1,mx+1
        do j = 1,my+1
            call lagrange_basis_2d(mx,xd_1d,i,xi,lx)
            call lagrange_basis_2d(my,yd_1d,j,yi,ly)
            zi = zi+zd(i,j)*lx*ly
        enddo
    enddo
end subroutine lagrange_interp_2d


subroutine Interpolate(N,xvec,x,p0,p1)
    implicit none
! Function grid
integer, intent(in) :: N
real(8), dimension(N), intent(in) :: xvec
! Point to Interpolate
real(8), intent(in) :: x
! Interpolate interval
integer, intent(inout) :: p0, p1
! Iterator
integer :: iter, p
    if (x .ge. xvec(p1)) then
        p0 = p1; return
    elseif (x .le. xvec(p0)) then
        p1 = p0; return
    endif 
    
    do iter = 1,100
        if (p1-p0<2) exit
        p = (p0+p1)/2
        if (x==xvec(p)) then
            p0 = p; p1 = p
            return
        elseif (xvec(p)>x) then
            p1 = p
        else
            p0 = p
        endif
    enddo
end subroutine Interpolate


subroutine Interpolate_1d(Nx,x,y,m,xi,yi)
    implicit none
integer, intent(in) :: Nx                                       ! Data length
real(8) :: x(Nx), y(Nx)                                         ! Data
integer :: m                                                    ! Polynomial degree - 1
real(8), intent(in) :: xi                                       ! Point to evaluate
real(8), intent(out) :: yi                                      ! Evaluate value
integer :: p1_x, p2_x
real(8), allocatable :: xvec(:), yvec(:)

    p1_x = 1; p2_x = Nx
    call Interpolate(Nx,x,xi,p1_x,p2_x)
    if (mod(m,2) .ne. 0) m = m+1
    allocate(xvec(m), yvec(m))
    xvec = x(p2_x-m/2:p1_x+m/2)
    yvec = y(p2_x-m/2:p1_x+m/2)
    call lagrange_interp_1d(m-1,xvec,yvec,xi,yi)
end subroutine Interpolate_1d


subroutine Interpolate_2d(Nx,Ny,x,y,z,m,xi,yi,zi)
    implicit none
integer, intent(in) :: Nx, Ny                                   ! Data length
real(8) :: x(Nx), y(Ny), z(Nx,Ny)                               ! Data
integer :: m                                                    ! Polynomial degree - 1
real(8), intent(in) :: xi, yi                                   ! Point to evaluate
real(8), intent(out) :: zi                                      ! Evaluate value
integer :: p1_x, p2_x, p1_y, p2_y
real(8), allocatable :: xvec(:), yvec(:), zvec(:,:)

    p1_x = 1; p2_x = Nx
    call Interpolate(Nx,x,xi,p1_x,p2_x)
    p1_y = 1; p2_y = Ny
    call Interpolate(Ny,y,yi,p1_y,p2_y)
    if (mod(m,2) .ne. 0) m = m+1
    allocate(xvec(m), yvec(m), zvec(m,m))
    xvec = x(p2_x-m/2:p1_x+m/2)
    yvec = y(p2_y-m/2:p1_y+m/2)
    zvec = z(p2_x-m/2:p1_x+m/2,p2_y-m/2:p1_y+m/2)
    call lagrange_interp_2d(m-1,m-1,xvec,yvec,zvec,xi,yi,zi)
end subroutine Interpolate_2d


! ======================================================================
! Numerical Integral
! ======================================================================

subroutine qromo(func,a,b,ss,choose)
    implicit none
real(8), intent(in) :: a, b                     ! 2 ends of integral
real(8), intent(out) :: ss                      ! Integral of func from a to b
integer, parameter :: JMAX = 14, JMAXP = JMAX+1, K = 5, KM = K-1
real(8), parameter :: EPS=1d-6
integer :: j
real(8) ::  dss, h(JMAXP), s(JMAXP)

interface
    real(8) function func(x)
        implicit none
    real(8), intent(in) :: x
    end function func

    subroutine choose(funk,N,aa,bb,s)
        implicit none
    integer, intent(in) :: N
    real(8), intent(in) :: aa,bb
    real(8), intent(out) :: s
    
    interface
        real(8) function funk(x)
            implicit none
        real(8), intent(in) :: x
        end function funk
    end interface
    
    end subroutine choose
end interface

    h(1) = 1d0
    do j = 1,JMAX
        call choose(func,j,a,b,s(j))
        if (j .ge. K) then
            call polint(K,h(j-KM),s(j-KM),0d0,ss,dss)
            if (abs(dss) .le. EPS*abs(ss)) return
        endif
        s(j+1) = s(j)
        h(j+1) = h(j)/9d0
    enddo
end subroutine qromo


subroutine midpnt(func,N,a,b,s)
integer, intent(in) :: N                ! Nth stage of refinement of an extended midpoint rule
real(8), intent(in) :: a, b             ! 2 ends of integral
real(8), intent(out) :: s               ! Integral of func from a to b
integer :: it, j
real(8) :: ddel, del, sum, tnm, x

interface
    real(8) function func(x)
        implicit none
    real(8), intent(in) :: x
    end function func
end interface

    if (N==1) then
        s = (b-a)*func(0.5*(a+b))
    else
        it=3**(N-2)
        tnm = dble(it)
        del = (b-a)/(3d0*tnm)
        ddel = del+del
        x = a+0.5d0*del
        sum = 0d0
        do j = 1,it
            sum = sum+func(x)
            x = x+ddel
            sum = sum+func(x)
            x = x+del
        enddo
        s = (s+(b-a)*sum/tnm)/3d0
    endif
end subroutine midpnt


subroutine midinf(func,N,aa,bb,s)
integer, intent(in) :: N                ! Nth stage of refinement of an extended midpoint rule
real(8), intent(in) :: aa, bb           ! 2 ends of integral
real(8), intent(out) :: s               ! Integral of func from a to b
integer :: it, j
real(8) :: a, b, ddel, del, sum, tnm, x

interface
    real(8) function func(x)
        implicit none
    real(8), intent(in) :: x
    end function func
end interface

    a = 1d0/aa; b = 1d0/bb
    if (N==1) then
        x = 0.5d0*(a+b)
        s = (b-a)*func(1d0/x)/x**2d0
    else
        it = 3**(N-2)
        tnm = dble(it)
        del = (b-a)/(3d0*tnm)
        ddel = del+del
        x = a+0.5d0*del
        sum = 0d0
        do j = 1,it
            sum = sum+func(1d0/x)/x**2d0
            x = x+ddel
            sum = sum+func(1d0/x)/x**2d0
            x = x+del
        enddo
        s = (s+(b-a)*sum/tnm)/3d0
    endif
end subroutine midinf


subroutine polint(N,xa,ya,x,y,dy)
integer, intent(in) :: N                                ! Data length
real(8), dimension(N), intent(in) :: xa, ya             ! Data
real(8), intent(in) :: x                                ! Point to interpolate
real(8), intent(out) :: y, dy                           ! y = P(x); error estimate dy
integer, parameter :: NMAX = 10                         ! Largest anticipated value of n.
integer :: i, m, ns
real(8) :: den, dif, dift, ho, hp, w, c(NMAX), d(NMAX)
    ns = 1
    dif = abs(x-xa(1))
    do i = 1,N                                          ! Here we find the closest table entry
        dift = abs(x-xa(i))
        if (dift<dif) then
            ns = i
            dif = dift
        endif
        c(i) = ya(i); d(i) = ya(i)                      ! Initialize C and D
    enddo
    y = ya(ns)                                          ! Initial approximation of y
    ns = ns-1
    do m = 1,N-1                                        ! Update C and D
        do i = 1,N-m
            ho = xa(i)-x
            hp = xa(i+m)-x
            w = c(i+1)-d(i)
            den=ho-hp
            if (den==0d0) pause 'failure in polint'
            den=w/den
            d(i) = hp*den
            c(i) = ho*den
        enddo
        if (2*ns<n-m) then
            dy = c(ns+1)
        else
            dy = d(ns)
            ns = ns-1
        endif
        y = y+dy
    enddo
end subroutine polint


subroutine GaussLegendre(N,x1,x2,x,w)
integer, intent(in) :: N                         ! Number of grid points
real(8), intent(in) :: x1, x2                    ! Lower and upper bound
real(8), dimension(N) :: x, w                    ! Grid points and weight
integer, parameter :: iter_max = 100
real(8), parameter :: EPS = 3d-14                ! Relative precision
integer :: M, i, j, iter
real(8) :: p1, p2, p3, pp, xl, xm, z, z1
    M = (N+1)/2
    xm = 0.5d0*(x2+x1)
    xl = 0.5d0*(x2-x1)
    do i = 1,M
        z = cos(3.141592654d0*(i-0.25d0)/(N+0.5d0))
        do iter = 1,iter_max
            p1 = 1d0; p2 = 0d0
            do j = 1,N
                p3 = p2; p2 = p1
                p1 = ((2d0*j-1d0)*z*p2-(j-1d0)*p3)/j
            enddo
            pp = N*(z*p1-p2)/(z*z-1d0)
            z1 = z
            z = z1-p1/pp
            if (abs(z-z1)<EPS) exit
        enddo
        x(i) = xm-xl*z
        x(n+1-i) = xm+xl*z
        w(i) = 2d0*xl/((1d0-z*z)*pp*pp)
        w(n+1-i) = w(i)
    enddo
end subroutine GaussLegendre


subroutine GauLeg_int(func,N,x1,x2,s)
integer, intent(in) :: N                ! Nth stage of refinement of an extended midpoint rule
real(8), intent(in) :: x1, x2           ! 2 ends of integral
real(8), intent(out) :: s               ! Integral of func from a to b
integer :: i
real(8), dimension(N) :: x_grid, x_weight
real(8) :: x, w

interface
    real(8) function func(x)
        implicit none
    real(8), intent(in) :: x
    end function func
end interface

    call GaussLegendre(N,x1,x2,x_grid,x_weight)
    s = 0d0
    do i = 1,N
        x = x_grid(i); w = x_weight(i)
        s = s+w*func(x)
    enddo
    
end subroutine GauLeg_int


subroutine GauLeg_inf(func,N,x1,x2,s)
integer, intent(in) :: N                ! Nth stage of refinement of an extended midpoint rule
real(8), intent(in) :: x1, x2           ! 2 ends of integral
real(8), intent(out) :: s               ! Integral of func from a to b
integer :: i
real(8), dimension(N) :: x_grid, x_weight
real(8) :: x, w

interface
    real(8) function func(x)
        implicit none
    real(8), intent(in) :: x
    end function func
end interface

    call GaussLegendre(N,x1,x2,x_grid,x_weight)
    s = 0d0
    do i = 1,N
        x = x_grid(i); w = x_weight(i)
        s = s+w*func(1d0/x)/x**2d0
    enddo
    
end subroutine GauLeg_inf

! ======================================================================
! Root Finding and Optimization
! ======================================================================

real(8) function Bisection(func,c,d,TOL) result(x0)
    implicit none
! Input and output
real(8), intent(in) :: c, d, TOL
real(8) :: a, b, p, fa, fb, fp, t
integer :: iter

interface
    real(8) function func(x)
        implicit none
    real(8), intent(in) :: x
    end function func
end interface

    a = c
    b = d
    do iter = 1,1000
        fa = func(a)
        fb = func(b)
        p = (a+b)/2
        fp = func(p)

        t = 2d0*p*epsilon(p)+TOL
        if (fp == 0 .OR. abs((a-b)/2)<t) then
            x0 = p
            exit
        endif

        if (fa*fp>0) a=p
        if (fb*fp>0) b=p
    enddo

end function Bisection


real(8) function Brent_Root(func,x1,x2,TOL,flag) result(x0)
    implicit none
! Input of the function
real(8), intent(in) :: x1, x2, TOL          ! Root search interval
integer, intent(out) :: flag
! Others
real(8) :: a, b, c, d, e, fa, fb, fc, p, q, r, s, tol1, xm
! Iterator
integer, parameter :: iter_max = 100
integer :: iter
! Tolerance
real(8), parameter :: eps = 3d-8

interface
    real(8) function func(x)
        implicit none
    real(8), intent(in) :: x
    end function func
end interface

    ! Initialize values
    flag = 0
    a = x1; b = x2
    fa = func(a); fb = func(b)
    ! Check if the root is bracketed
    if (bracket(fa,fb) == .false.) return
    c = b; fc = fb
    do iter = 1,iter_max
        if (bracket(fb,fc) == .false.) then
            c = a; fc = fa; d = b-a; e = d
        endif
        ! b must be the best guess
        if (abs(fc)<abs(fb)) then
            a = b; b = c; c = a
            fa = fb; fb = fc; fc = fa
        endif

        ! Check convergence
        tol1 = 2d0*eps*abs(b)+0.5d0*tol
        xm = 0.5d0*(c-b)
        if (abs(xm)<=tol1 .or. fb==0d0) then
            x0 = b; flag = 1
            return
        endif

        if (abs(e)>=tol1 .and. abs(fa)>abs(fb)) then
            s = fb/fa
            if (a==c) then                          ! Secant
                p = 2d0*xm*s; q = 1d0-s
            else                                    ! Inverse quadradic
                q = fa/fc; r = fb/fc
                p = s*(2d0*xm*q*(q-r)-(b-a)*(r-1d0))
                q = (q-1d0)*(r-1d0)*(s-1d0)
            endif

            if (p>0d0) q = -q
            p = abs(p)
            if (2d0*p<min(3d0*xm*q-abs(tol1*q),abs(e*q))) then
                e = d; d = p/q                      ! Secant/Inverse quadradic
            else
                d = xm; e = d                       ! Bisection
            endif
        else
                d = xm; e = d                       ! Bisection
        endif
        a = b; fa = fb
        if (abs(d)>tol1) then
            b = b+d
        else
            b = b+sign(tol1,xm)
        endif
        fb = func(b)
    enddo
    x0 = b

contains

logical function bracket(a,b)
implicit none
real(8), intent(in) :: a, b
    bracket = .TRUE.
    if ((a>0 .and. b>0) .or. (a<0 .and. b<0)) bracket = .false.
end function bracket

end function Brent_Root


subroutine Brent_Min(func,ax,bx,cx,tol,xmin,flag)
    implicit none
! Input and output
real(8), intent(in) :: ax,bx,cx,tol
real(8), intent(out) :: xmin
integer, intent(out) :: flag
! Parameters
integer, parameter :: ITMAX=100
real(8), parameter :: CGOLD=0.3819660d0,ZEPS=1d-3*epsilon(ax)
! Others
integer :: iter
real(8) :: a,b,d,e,etemp,fx,fu,fv,fw,p,q,r,tol1,tol2,u,v,w,x,xm

interface
    real(8) function func(x)
        implicit none
    real(8), intent(in) :: x
    end function func
end interface

    flag = 0
    a=min(ax,cx)
    b=max(ax,cx)
    v=bx
    w=v
    x=v
    e=0d0
    fx=func(x)        ! here uses the function
    fv=fx
    fw=fx
    do iter=1,ITMAX
        xm=0.5d0*(a+b)
        tol1=tol*abs(x)+ZEPS
        tol2=2d0*tol1
        if (abs(x-xm) <= (tol2-0.5d0*(b-a))) then
            xmin=x
            flag = 1
            RETURN
        endif
        if (abs(e) > tol1) then
            r=(x-w)*(fx-fv)
            q=(x-v)*(fx-fw)
            p=(x-v)*q-(x-w)*r
            q=2d0*(q-r)
            if (q > 0.0d0) p=-p
            q=abs(q)
            etemp=e
            e=d
            if (abs(p) >= abs(0.5d0*q*etemp) .or. &
                p <= q*(a-x) .or. p >= q*(b-x)) then
                e=merge(a-x,b-x, x >= xm )
                d=CGOLD*e
            else
                d=p/q
                u=x+d
                if (u-a < tol2 .or. b-u < tol2) d=sign(tol1,xm-x)
            endif
        else
            e=merge(a-x,b-x, x >= xm )
            d=CGOLD*e
        endif
        u=merge(x+d,x+sign(tol1,d), abs(d) >= tol1 )
        fu=func(u)                        ! here uses the function
        if (fu <= fx) then
            if (u >= x) then
                a=x
            else
                b=x
            endif
            call shft(v,w,x,u)
            call shft(fv,fw,fx,fu)
        else
            if (u < x) then
                a=u
            else
                b=u
            endif
            if (fu <= fw .or. w == x) then
                v=w
                fv=fw
                w=u
                fw=fu
            else if (fu <= fv .or. v == x .or. v == w) then
                v=u
                fv=fu
            endif
        endif
    enddo

contains

!shift the variables
subroutine shft(a,b,c,d)
    implicit none
real(8), intent(out) :: a
real(8), intent(inout) :: b,c
real(8), intent(in) :: d
    a=b
    b=c
    c=d
end subroutine shft

end subroutine Brent_Min


subroutine Amoeba(p,ndim,func,TOL)
    implicit none
real(8), intent(in) :: TOL
integer, intent(in) :: ndim
real(8), intent(inout) :: p(ndim+1,ndim)                      ! Multidimensional minimization
integer, parameter :: nmax=20, itmax=500
real(8), parameter :: alpha=1d0, beta=0.5d0, gamma=2d0
real(8) :: y(ndim+1),pr(nmax),prr(nmax),pbar(nmax),ypr, yprr
integer :: i,j,ihi,inhi,ilo,mpts,iter
real(8) :: ftol, rtol

interface
    real(8) function func(x)
        implicit none
    real(8), dimension(:), intent(in) :: x
    end function func
end interface

    ftol = TOL
    do iter = 1,ndim+1
        y(iter) = func(p(iter,:))
    enddo
    mpts=ndim+1

    do iter = 0,itmax
        ilo=1
        if (y(1)>y(2)) then
            ihi=1; inhi=2
        else
            ihi=2; inhi=1
        endif

        do i=1,mpts
            if (y(i)<y(ilo)) ilo=i
            if (y(i)>y(ihi)) then
                inhi=ihi; ihi=i
            elseif (y(i)>y(inhi)) then
                if (i/=ihi) inhi=i
            endif
        enddo

        rtol=2d0*abs(y(ihi)-y(ilo))/(abs(y(ihi))+abs(y(ilo)))
        if (rtol<ftol)  return

        do j=1,ndim
            pbar(j)=0.
        enddo

        do i=1,mpts
            if (i/=ihi) then
                do j=1,ndim
                    pbar(j)=pbar(j)+p(i,j)
                enddo
            endif
        enddo

        do j=1,ndim
            pbar(j)=pbar(j)/ndim
            pr(j)=(1d0+alpha)*pbar(j)-alpha*p(ihi,j)
        enddo

        ypr=func(pr)

        if (ypr<=y(ilo)) then
            do j=1,ndim
                prr(j)=gamma*pr(j)+(1.-gamma)*pbar(j)
            enddo
            yprr=func(prr)
            if (yprr<y(ilo)) then
                do j=1,ndim
                    p(ihi,j)=prr(j)
                enddo
                y(ihi)=yprr
            else
                do j=1,ndim
                    p(ihi,j)=pr(j)
                enddo
                y(ihi)=ypr
            endif
        else if (ypr>=y(inhi)) then
            if (ypr<y(ihi)) then
                do j=1,ndim
                    p(ihi,j)=pr(j)
                enddo
                y(ihi)=ypr
            endif
            do j=1,ndim
                prr(j)=beta*p(ihi,j)+(1d0-beta)*pbar(j)
            enddo
            yprr=func(prr)
            if (yprr<y(ihi)) then
                do j=1,ndim
                    p(ihi,j)=prr(j)
                enddo
                y(ihi)=yprr
            else
                do i=1,mpts
                    if (i/=ilo) then
                        do j=1,ndim
                            pr(j)=0.5*(p(i,j)+p(ilo,j))
                            p(i,j)=pr(j)
                        enddo
                        y(i)=func(pr)
                    endif
                enddo
            endif
        else
            do j=1,ndim
                p(ihi,j)=pr(j)
            enddo
            y(ihi)=ypr
        endif
    enddo

end subroutine Amoeba


! ======================================================================
! Distribution
! ======================================================================

subroutine Random_Markov(N,iseed,Nx,x_grid,x_trans,x_sequence)
    implicit none
integer, intent(in) :: N, iseed, Nx
real(8), intent(in) :: x_grid(Nx), x_trans(Nx,Nx)
integer, intent(out) :: x_sequence(N)
integer :: seed(N)
real(8) :: random(N)
integer :: i, xi, p0, p1
real(8) :: x0, x1, trans_tmp(Nx)
    x0 = 1d0
    p0 = 1; p1 = Nx
    call Interpolate(Nx,x_grid,x0,p0,p1)
    seed = iseed
    call random_seed(put=seed)
    call random_number(random)
    do i = 1,N
        x_sequence(i) = p0
        x0 = random(i)
        trans_tmp = x_trans(:,p0)
        x1 = 0d0
        do xi = 1,Nx
            x1 = x1+trans_tmp(xi)
            if (x0<x1) then
                p1 = xi; exit
            endif
        enddo
        p0 = p1
    enddo
end subroutine Random_Markov


subroutine Random_Normal(N,iseed,sigma,x)
    implicit none
integer, intent(in) :: N, iseed
real(8), intent(in) :: sigma
real(8), intent(out) :: x(N)
real(8) :: random1(N), random2(N)
integer :: seed(N)
real(8), parameter :: pi = 4d0*atan(1d0)
    seed = iseed+step*1000
    call random_seed(put=seed)
    call random_number(random1)
    call random_number(random2)
    x = sigma*sqrt(-2d0*log(random1))*cos(2d0*pi*random2)
end subroutine Random_Normal


subroutine Random_Pareto(N,iseed,x_mean,lambda_x,x)
    implicit none
integer, intent(in) :: N, iseed
real(8), intent(in) :: x_mean, lambda_x
real(8), intent(out) :: x(N)
real(8) :: x_min, random(N)
integer :: seed(N)
    seed = iseed+step*1000
    call random_seed(put=seed)
    call random_number(random)
    x_min = x_mean*(lambda_x-1d0)/lambda_x
    x = x_min*(1d0-random)**(-1d0/lambda_x)
end subroutine Random_Pareto


subroutine Random_Binomial(N,iseed,p,x)
    implicit none
integer, intent(in) :: N, iseed
real(8), intent(in) :: p
real(8), intent(out) :: x(N)
real(8) :: random(N)
integer :: seed(N)
integer :: i
    seed = iseed+step*1000
    call random_seed(put=seed)
    call random_number(random)
    x = 0d0
    do i = 1,N
        if (random(i)<p) x(i) = 1d0
    enddo
end subroutine Random_Binomial


subroutine Random_Normal_Scalar(iseed,sigma,x)
    implicit none
integer, intent(in) :: iseed
real(8), intent(in) :: sigma
real(8), intent(out) :: x
real(8) :: random1, random2
integer, dimension(1) :: seed
real(8), parameter :: pi = 4d0*atan(1d0)
    seed = iseed+step*1000
    call random_seed(put=seed)
    call random_number(random1)
    call random_number(random2)
    x = sigma*sqrt(-2d0*log(random1))*cos(2d0*pi*random2)
end subroutine Random_Normal_Scalar


subroutine Random_Pareto_Scalar(iseed,x_mean,lambda_x,x)
    implicit none
integer, intent(in) :: iseed
real(8), intent(in) :: x_mean, lambda_x
real(8), intent(out) :: x
real(8) :: x_min, random
integer, dimension(1) :: seed
    seed = iseed+step*1000
    call random_seed(put=seed)
    call random_number(random)
    x = (1d0-random)**(-1d0/lambda_x)
    x_min = x_mean*(lambda_x-1d0)/lambda_x
    x = x_min*x
end subroutine Random_Pareto_Scalar


subroutine Random_Binomial_Scalar(iseed,p,x)
    implicit none
integer, intent(in) :: iseed
real(8), intent(in) :: p
real(8), intent(out) :: x
real(8) :: random
integer, dimension(1) :: seed
    seed = iseed+step*1000
    call random_seed(put=seed)
    call random_number(random)
    if (random<p) x = 1d0
end subroutine Random_Binomial_Scalar


! ======================================================================
! Statistics
! ======================================================================

real(8) function mean(N,x,pdf)
    implicit none
integer, intent(in) :: N
real(8), intent(in) :: x(N)
real(8), intent(in), optional :: pdf(N)
real(8) :: pdf1(N)
    pdf1 = 1d0/dble(N)
    if (present(pdf)) pdf1 = pdf
    mean = sum(x*pdf1)/sum(pdf1)
end function


real(8) function variance(N,x,pdf)
    implicit none
integer, intent(in) :: N
real(8), intent(in) :: x(N)
real(8), intent(in), optional :: pdf(N)
real(8) :: pdf1(N), mean_x
    pdf1 = 1d0/N
    if (present(pdf)) pdf1 = pdf
    mean_x = mean(N,x,pdf1)
    variance = sum((x-mean_x)**2d0*pdf1)/sum(pdf1)
end function


real(8) function correlation(Nx,Ny,x,y,pdf_x,pdf_y,pdf_xy)
    implicit none
integer, intent(in) :: Nx, Ny
real(8), intent(in) :: x(Nx), y(Ny)
real(8), intent(in), optional :: pdf_x(Nx), pdf_y(Ny), pdf_xy(Nx,Ny)
real(8) :: pdf_x1(Nx), pdf_y1(Ny)
real(8) :: mean_x, mean_y, var_x, var_y
integer :: xi, yi
    pdf_x1 = 1d0/dble(Nx); pdf_y1 = 1d0/dble(Ny)
    if (present(pdf_x)) pdf_x1 = pdf_x
    if (present(pdf_y)) pdf_y1 = pdf_y
    
    mean_x = mean(Nx,x,pdf_x1)
    mean_y = mean(Ny,y,pdf_y1)
    var_x = variance(Nx,x,pdf_x1)
    var_y = variance(Ny,y,pdf_y1)

    if (present(pdf_xy)) then
        correlation = 0d0
        do xi = 1,Nx
            do yi = 1,Ny
                correlation = correlation+(x(xi)-mean_x)*(y(yi)-mean_y)*pdf_xy(xi,yi)
            enddo
        enddo
        correlation = correlation/sum(pdf_xy)
        correlation = correlation/sqrt(var_x*var_y)
    else
        correlation = sum((x-mean_x)*(y-mean_y))/(Nx-1)
        correlation = correlation/sqrt(var_x*var_y)
    endif
end function

end module My_Function
