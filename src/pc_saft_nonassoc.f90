!> The module implementing the alpha^{hardchain} and alpha^{dispersion}
!> contributions in PC-SAFT. Parameters are stored in the module
!> pc_saft_parameters, while the association contribution is alpha^{assoc}
!> is implemented in the module saft.
!>
!> We have implemented the variant sPC-SAFT (simplified PC-SAFT),
!> which has the same (3 or 5) pure-component parameters as PC-SAFT
!> but is built upon a simpler and computationally faster mixing rule.
module pc_saft_nonassoc
  use thermopack_var, only: nce, base_eos_param
  use thermopack_constants, only: N_AVOGADRO
  use numconstants, only: PI
  implicit none
  save

  type, extends(base_eos_param) :: PCSAFT_eos
    ! All dimensions will be allocated to nce.
    real, allocatable :: m(:)                 !< [-]
    real, allocatable :: sigma(:,:)           !< [m]
    real, allocatable :: eps_depth_divk(:,:)  !< [K]
    real, allocatable :: sigma_cube(:,:)      !< [m^3]
  contains
    procedure, public :: dealloc => pcsaft_dealloc
    procedure, public :: allocate_and_init => pcsaft_allocate_and_init
    ! Assignment operator
    procedure, pass(This), public :: assign_eos => assign_pcsaft
  end type PCSAFT_eos

  ! Universal model constants
  real, parameter, dimension(0:2,0:6) :: a_mat = reshape( (/ &
       0.9105631445, -0.3084016918, -0.0906148351, &
       0.6361281449, 0.1860531159,  0.4527842806, &
       2.6861347891, -2.5030047259, 0.5962700728, &
       -26.547362491, 21.419793629, -1.7241829131, &
       97.759208784, -65.255885330, -4.1302112531, &
       -159.59154087, 83.318680481, 13.776631870, &
       91.297774084, -33.746922930, -8.6728470368 /), &
       (/3,7/) )
  real, parameter, dimension(0:2,0:6) :: b_mat = reshape( (/ &
       0.7240946941, -0.5755498075, 0.0976883116, &
       2.2382791861, 0.6995095521, -0.2557574982, &
       -4.0025849485, 3.8925673390, -9.1558561530, &
       -21.003576815, -17.215471648, 20.642075974, &
       26.855641363, 192.67226447, -38.804430052, &
       206.55133841, -161.82646165, 93.626774077, &
       -355.60235612, -165.20769346, -29.666905585 /), &
       (/3,7/) )

  ! SOME POSSIBLE OPTIMIZATIONS
  ! 1. I1 and I2 can probably be calculated simultaneously.
  ! 2. a and b can probably be calculated simultaneously.
  ! 3. A lot of routines should be able to optionally accept e.g. d_i(T).

contains

  !> Gives the contribution to the reduced, residual Helmholtz function F [mol]
  !> coming from PC-SAFT's hard-chain and dispersion contributions. All
  !> variables are in base SI units. F is defined by
  !> F(T,V,n) = sumn*alpha_PC(rho,T,n) = sumn*alpha_PC(sumn/V,T,n)
  subroutine F_PC_SAFT_TVn(eos,T,V,n,F,F_T,F_V,F_n,F_TT,F_TV,F_Tn,F_VV,F_Vn,F_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: T,V,n(nce) ! temp. [K], vol. [m^3], mole numbers [mol]
    ! Output.
    real, intent(out), optional :: F !< [mol]
    real, intent(out), optional :: F_T,F_V,F_n(nce)
    real, intent(out), optional :: F_TT,F_TV,F_Tn(nce),F_VV,F_Vn(nce),F_nn(nce,nce)
    ! Locals.
    real :: rho
    real :: alp
    real :: alp_rho, alp_T, alp_n(nce)
    real :: alp_rhorho, alp_rhoT, alp_rhon(nce), alp_TT
    real :: alp_Tn(nce), alp_nn(nce,nce)
    real :: sumn
    integer :: i,j
    logical :: fir_der_present, sec_der_present

    sumn = sum(n)
    rho = sumn/V

    fir_der_present = present(F_T) .or. present(F_V) .or. present(F_n)
    sec_der_present = present(F_TT) .or. present(F_TV) .or. present(F_Tn) .or. &
         present(F_VV) .or. present(F_Vn) .or. present(F_nn)

    if (sec_der_present) then
       call alpha_PC(eos,rho,T,n,alp=alp,alp_rho=alp_rho,alp_T=alp_T,alp_n=alp_n,&
            alp_rhorho=alp_rhorho,alp_rhoT=alp_rhoT,alp_rhon=alp_rhon,&
            alp_TT=alp_TT,alp_Tn=alp_Tn,alp_nn=alp_nn)
    else if (fir_der_present) then
       call alpha_PC(eos,rho,T,n,alp=alp,alp_rho=alp_rho,alp_T=alp_T,alp_n=alp_n)
    else
       call alpha_PC(eos,rho,T,n,alp=alp)
    end if

    if (present(F)) F = sumn*alp
    if (present(F_T)) F_T = sumn*alp_T
    if (present(F_V)) F_V = -(sumn/V)**2*alp_rho
    if (present(F_n)) F_n = alp + sumn*alp_rho/V + sumn*alp_n
    if (present(F_TT)) F_TT = sumn*alp_TT
    if (present(F_TV)) F_TV = -(sumn/V)**2*alp_rhoT
    if (present(F_Tn)) F_Tn = alp_T + sumn*alp_rhoT/V + sumn*alp_Tn
    if (present(F_VV)) F_VV = 2*sumn**2/V**3*alp_rho + sumn**3/V**4*alp_rhorho
    if (present(F_Vn)) F_Vn = -2*sumn*alp_rho/V**2-sumn**2*alp_rhorho/V**3-sumn**2*alp_rhon/V**2
    if (present(F_nn)) then
       do i=1,nce
          do j=1,nce
             F_nn(i,j) = sumn*alp_nn(i,j) + sumn*(alp_rhon(j)+alp_rhon(i))/V + alp_n(i) + alp_n(j) &
                  + sumn*alp_rhorho/V**2 + 2*alp_rho/V
          end do
       end do
    end if

  end subroutine F_PC_SAFT_TVn

  !> alpha_PC = alp^{hard_chain} + alpha^{dispersion}
  subroutine alpha_PC(eos,rho,T,n,alp,alp_rho,alp_T,alp_n, &
       alp_rhorho,alp_rhoT,alp_rhon,alp_TT,alp_Tn,alp_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: rho, T, n(nce)  !< [mol/m^3], [K], [mol]
    real, intent(out), optional :: alp !< [-]
    real, intent(out), optional :: alp_rho, alp_T, alp_n(nce)
    real, intent(out), optional :: alp_rhorho, alp_rhoT, alp_rhon(nce), alp_TT
    real, intent(out), optional :: alp_Tn(nce), alp_nn(nce,nce)
    ! Locals.
    real :: alp_hc, alp_hc_rho, alp_hc_T, alp_hc_n(nce)
    real :: alp_hc_rhorho, alp_hc_rhoT, alp_hc_rhon(nce)
    real :: alp_hc_TT, alp_hc_Tn(nce), alp_hc_nn(nce,nce)

    real :: alp_d, alp_d_rho, alp_d_T, alp_d_n(nce)
    real :: alp_d_rhorho, alp_d_rhoT, alp_d_rhon(nce)
    real :: alp_d_TT, alp_d_Tn(nce), alp_d_nn(nce,nce)

    logical :: fir_der_present, sec_der_present

    fir_der_present = present(alp_rho) .or. present(alp_T) .or. present(alp_n)
    sec_der_present = present(alp_rhorho) .or. present(alp_rhoT) .or. present(alp_rhon) .or. &
         present(alp_TT) .or. present(alp_Tn) .or. present(alp_nn)

    if (sec_der_present) then
       call alpha_spc_saft_hc(eos,rho,T,n,alp_hc,alp_hc_rho,alp_hc_T,alp_hc_n,&
            alp_hc_rhorho,alp_hc_rhoT,alp_hc_rhon,&
            alp_hc_TT,alp_hc_Tn,alp_hc_nn)
       call alpha_disp(eos,rho,T,n,alp_d,alp_d_rho,alp_d_T,alp_d_n,&
            alp_d_rhorho,alp_d_rhoT,alp_d_rhon,&
            alp_d_TT,alp_d_Tn,alp_d_nn)
    else if (fir_der_present) then
       call alpha_spc_saft_hc(eos,rho,T,n,alp_hc,alp_hc_rho,alp_hc_T,alp_hc_n)
       call alpha_disp(eos,rho,T,n,alp_d,alp_d_rho,alp_d_T,alp_d_n)
    else
       call alpha_spc_saft_hc(eos,rho,T,n,alp_hc)
       call alpha_disp(eos,rho,T,n,alp_d)
    end if

    if (present(alp)) then
       alp = alp_hc + alp_d
    end if

    if (present(alp_rho)) then
       alp_rho = alp_hc_rho + alp_d_rho
    end if

    if (present(alp_T)) then
       alp_T = alp_hc_T + alp_d_T
    end if

    if (present(alp_n)) then
       alp_n = alp_hc_n + alp_d_n
    end if

    if (present(alp_rhorho)) then
       alp_rhorho = alp_hc_rhorho + alp_d_rhorho
    end if

    if (present(alp_rhoT)) then
       alp_rhoT = alp_hc_rhoT + alp_d_rhoT
    end if

    if (present(alp_rhon)) then
       alp_rhon = alp_hc_rhon + alp_d_rhon
    end if

    if (present(alp_TT)) then
       alp_TT = alp_hc_TT + alp_d_TT
    end if

    if (present(alp_Tn)) then
       alp_Tn = alp_hc_Tn + alp_d_Tn
    end if

    if (present(alp_nn)) then
       alp_nn = alp_hc_nn + alp_d_nn
    end if

  end subroutine alpha_PC


  !> The reduced, molar Helmholtz energy contribution from dispersion.
  subroutine alpha_disp(eos,rho,T,n,alp,alp_rho,alp_T,alp_n, &
       alp_rhorho,alp_rhoT,alp_rhon,alp_TT,alp_Tn,alp_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: rho, T, n(nce)  !< [mol/m^3], [K], [mol]

    real, intent(out), optional :: alp ! [-]
    real, intent(out), optional :: alp_rho,alp_T,alp_n(nce)
    real, intent(out), optional :: alp_rhorho, alp_rhoT, alp_rhon(nce), alp_TT
    real, intent(out), optional :: alp_Tn(nce), alp_nn(nce,nce)

    real :: I1, I1_rho, I1_T, I1_n(nce)
    real :: I1_rhorho, I1_rhoT, I1_rhon(nce),I1_TT,I1_Tn(nce),I1_nn(nce,nce)
    real :: I2, I2_rho, I2_T, I2_n(nce)
    real :: I2_rhorho, I2_rhoT, I2_rhon(nce),I2_TT,I2_Tn(nce),I2_nn(nce,nce)
    real :: m2e1s3, m2e1s3_T, m2e1s3_n(nce), m2e2s3, m2e2s3_T, m2e2s3_n(nce)
    real :: m2e1s3_Tn(nce), m2e2s3_Tn(nce)
    real :: m2e1s3_TT, m2e1s3_nn(nce,nce), m2e2s3_TT, m2e2s3_nn(nce,nce)
    real :: mbar, mbar_n(nce), mbar_nn(nce,nce)
    real :: C1,C1_rho,C1_T,C1_n(nce)
    real :: C1_rhorho,C1_rhoT,C1_rhon(nce)
    real :: C1_TT,C1_Tn(nce),C1_nn(nce,nce)
    integer :: k,l

    logical :: fir_der_present, sec_der_present

    fir_der_present = present(alp_rho) .or. present(alp_T) .or. present(alp_n)
    sec_der_present = present(alp_rhorho) .or. present(alp_rhoT) .or. present(alp_rhon) .or. &
         present(alp_TT) .or. present(alp_Tn) .or. present(alp_nn)

    if (sec_der_present) then
       call I_1(eos,rho,T,n,I1,I1_rho,I1_T,I1_n,I1_rhorho,I1_rhoT,I1_rhon,I1_TT,I1_Tn,I1_nn)
       call I_2(eos,rho,T,n,I2,I2_rho,I2_T,I2_n,I2_rhorho,I2_rhoT,I2_rhon,I2_TT,I2_Tn,I2_nn)
       call m2e1s3_mean(eos,T,n,m2e1s3,m2e1s3_T,m2e1s3_n,m2e1s3_TT,m2e1s3_Tn,m2e1s3_nn)
       call m2e2s3_mean(eos,T,n,m2e2s3,m2e2s3_T,m2e2s3_n,m2e2s3_TT,m2e2s3_Tn,m2e2s3_nn)
       call C_1(eos,rho,T,n,C1,C1_rho,C1_T,C1_n,&
            C1_rhorho,C1_rhoT,C1_rhon,C1_TT,C1_Tn,C1_nn)
    else if (fir_der_present) then
       call I_1(eos,rho,T,n,I1,I1_rho,I1_T,I1_n)
       call I_2(eos,rho,T,n,I2,I2_rho,I2_T,I2_n)
       call m2e1s3_mean(eos,T,n,m2e1s3,m2e1s3_T,m2e1s3_n)
       call m2e2s3_mean(eos,T,n,m2e2s3,m2e2s3_T,m2e2s3_n)
       call C_1(eos,rho,T,n,C1,C1_rho,C1_T,C1_n)
    else
       call I_1(eos,rho,T,n,I1)
       call I_2(eos,rho,T,n,I2)
       call m2e1s3_mean(eos,T,n,m2e1s3)
       call m2e2s3_mean(eos,T,n,m2e2s3)
       call C_1(eos,rho,T,n,C1)
    end if

    call m_bar(eos,n,mbar,mbar_n,mbar_nn)

    if (present(alp)) then
       alp = -PI*rho*(2*I1*m2e1s3 + mbar*C1*I2*m2e2s3)
       alp = alp*N_AVOGADRO
    end if

    if (present(alp_rho)) then
       alp_rho = -2*PI*(I1+rho*I1_rho)*m2e1s3
       alp_rho = alp_rho - PI*mbar*(C1*I2+rho*C1_rho*I2+rho*C1*I2_rho)*m2e2s3
       alp_rho = alp_rho*N_AVOGADRO
    end if

    if (present(alp_T)) then
       alp_T = -2*PI*rho*(I1_T*m2e1s3 + I1*m2e1s3_T)
       alp_T = alp_T- PI*rho*mbar*(C1_T*I2*m2e2s3 + C1*I2_T*m2e2s3 + C1*I2*m2e2s3_T)
       alp_T = alp_T*N_AVOGADRO
    end if

    if (present(alp_n)) then
       alp_n = -2*PI*rho*(I1_n*m2e1s3 + I1*m2e1s3_n)
       alp_n = alp_n - PI*rho*(mbar_n*C1*I2 + mbar*C1_n*I2 + mbar*C1*I2_n)*m2e2s3 - &
            PI*rho*mbar*C1*I2*m2e2s3_n
       alp_n = alp_n*N_AVOGADRO
    end if

    if (present(alp_rhorho)) then
       alp_rhorho = -2*PI*(2*I1_rho+rho*I1_rhorho)*m2e1s3
       alp_rhorho = alp_rhorho - PI*mbar*(2*C1_rho*I2 + 2*C1*I2_rho + 2*rho*C1_rho*I2_rho &
            + rho*C1_rhorho*I2 +rho*C1*I2_rhorho)*m2e2s3
       alp_rhorho = alp_rhorho*N_AVOGADRO
    end if

    if (present(alp_rhoT)) then
       alp_rhoT = -2*PI*(I1_T+rho*I1_rhoT)*m2e1s3 -2*PI*(I1+rho*I1_rho)*m2e1s3_T
       alp_rhoT = alp_rhoT + 2*PI*mbar*(C1*I2 + rho*C1_rho*I2 + rho*C1*I2_rho)*m2e2s3/T - &
            PI*mbar*(C1_T*I2 + C1*I2_T + rho*C1_rhoT*I2 + &
            rho*C1_rho*I2_T + rho*C1_T*I2_rho + rho*C1*I2_rhoT)*m2e2s3
       alp_rhoT = alp_rhoT*N_AVOGADRO
    end if

    if (present(alp_rhon)) then
       alp_rhon = -2*PI*(I1_n*m2e1s3 + I1*m2e1s3_n) &
            - 2*PI*rho*(I1_rhon*m2e1s3 + I1_rho*m2e1s3_n)
       alp_rhon = alp_rhon - PI*(mbar_n*C1*I2 + mbar*C1_n*I2 + mbar*C1*I2_n)*m2e2s3 &
            - PI*mbar*C1*I2*m2e2s3_n &
            - PI*rho*(mbar_n*C1_rho*I2 + mbar*C1_rhon*I2 + mbar*C1_rho*I2_n &
            + mbar_n*C1*I2_rho + mbar*C1_n*I2_rho + mbar*C1*I2_rhon)*m2e2s3 &
            -pi*rho*mbar*(C1_rho*I2 + C1*I2_rho)*m2e2s3_n
       alp_rhon = alp_rhon*N_AVOGADRO
    end if

    if (present(alp_TT)) then
       alp_TT = -2*PI*rho*(I1_TT*m2e1s3 + 2*I1_T*m2e1s3_T + I1*m2e1s3_TT)
       alp_TT = alp_TT - PI*rho*mbar*(&
            C1_TT*I2*m2e2s3 + C1*I2_TT*m2e2s3 + C1*I2*m2e2s3_TT &
            + 2*(C1_T*I2_T*m2e2s3 + C1_T*I2*m2e2s3_T + C1*I2_T*m2e2s3_T))
       alp_TT = alp_TT*N_AVOGADRO
    end if

    if (present(alp_Tn)) then
       alp_Tn = -2*PI*rho*(I1_Tn-I1_n/T)*m2e1s3 - 2*PI*rho*(I1_T-I1/T)*m2e1s3_n
       alp_Tn = alp_Tn - PI*rho*(mbar_n*m2e2s3 + mbar*m2e2s3_n)*(C1_T*I2 + C1*I2_T - 2*C1*I2/T) &
            - PI*rho*mbar*(C1_Tn*I2 + C1_n*I2_T - 2*C1_n*I2/T &
            + C1_T*I2_n + C1*I2_Tn - 2*C1*I2_n/T)*m2e2s3
       alp_Tn = alp_Tn*N_AVOGADRO
    end if

    if (present(alp_nn)) then
       do k=1,nce
          do l=1,nce
             alp_nn(k,l) = -2*PI*rho*(I1_nn(k,l)*m2e1s3 &
                  + I1_n(l)*m2e1s3_n(k) + I1_n(k)*m2e1s3_n(l) + I1*m2e1s3_nn(l,k))
             alp_nn(k,l) = alp_nn(k,l) - PI*rho*mbar*C1*I2*m2e2s3_nn(k,l) &
                  - PI*rho*(mbar_n(k)*C1*I2 + mbar*(C1_n(k)*I2 + C1*I2_n(k)))*m2e2s3_n(l) &
                  - PI*rho*(mbar_n(l)*C1*I2 + mbar*(C1_n(l)*I2 + C1*I2_n(l)))*m2e2s3_n(k) &
                  - PI*rho*(mbar_nn(k,l)*C1*I2 + mbar_n(k)*(C1_n(l)*I2 + C1*I2_n(l)) &
                  + mbar_n(l)*(C1_n(k)*I2 + C1*I2_n(k)) &
                  + mbar*(C1_n(k)*I2_n(l) + C1_n(l)*I2_n(k)) &
                  + mbar*(C1_nn(k,l)*I2 + C1*I2_nn(l,k)))*m2e2s3
          end do
       end do
       alp_nn = alp_nn*N_AVOGADRO
    end if

  end subroutine alpha_disp

  ! The reduced, molar Helmholtz energy reference contribution from hard-chains.
  subroutine alpha_spc_saft_hc(eos,rho,T,n,alp,alp_rho,alp_T,alp_n, &
       alp_rhorho,alp_rhoT,alp_rhon,alp_TT,alp_Tn,alp_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: rho, T, n(nce)  !< [mol/m^3], [K], [mol]
    real, intent(out), optional :: alp !< [-]
    real, intent(out), optional :: alp_rho,alp_T,alp_n(nce)
    real, intent(out), optional :: alp_rhorho, alp_rhoT, alp_rhon(nce), alp_TT
    real, intent(out), optional :: alp_Tn(nce), alp_nn(nce,nce)
    ! Locals.
    real :: g    !< [-]
    real :: g_rho,g_T,g_n(nce)
    real :: g_rhorho,g_rhoT,g_rhon(nce)
    real :: g_TT,g_Tn(nce),g_nn(nce,nce)
    real :: alp_hs, alp_hs_rho, alp_hs_T, alp_hs_n(nce)
    real :: alp_hs_rhorho,alp_hs_rhoT,alp_hs_rhon(nce)
    real :: alp_hs_TT, alp_hs_Tn(nce), alp_hs_nn(nce,nce)
    real :: mbar,mbar_n(nce),mbar_nn(nce,nce)
    integer :: i,k,l
    real :: logg, sumn
    real :: bigG, bigG_T, bigG_rho
    real :: bigG_x(nce), bigG_n(nce), bigG_rhox(nce), bigG_Tx(nce), bigG_xx(nce,nce)
    logical :: fir_der_present, sec_der_present

    ! bigG is a helper function, equal to \sum_i x_i (m(i)-1)*ln(g)

    fir_der_present = present(alp_rho) .or. present(alp_T) .or. present(alp_n)
    sec_der_present = present(alp_rhorho) .or. present(alp_rhoT) .or. present(alp_rhon) .or. &
         present(alp_TT) .or. present(alp_Tn) .or. present(alp_nn)

    if (sec_der_present) then
       call g_ij_spc_saft(eos,rho,T,n,g,g_rho,g_T,g_n,g_rhorho,g_rhoT,g_rhon, &
            g_TT,g_Tn,g_nn)
       call alpha_spc_saft_hs(eos,rho,T,n,alp_hs,alp_hs_rho,alp_hs_T,alp_hs_n,&
            alp_hs_rhorho,alp_hs_rhoT,alp_hs_rhon,&
            alp_hs_TT,alp_hs_Tn,alp_hs_nn)
    else if (fir_der_present) then
       call g_ij_spc_saft(eos,rho,T,n,g,g_rho,g_T,g_n)
       call alpha_spc_saft_hs(eos,rho,T,n,alp_hs,alp_hs_rho,alp_hs_T,alp_hs_n)
    else
       call g_ij_spc_saft(eos,rho,T,n,g)
       call alpha_spc_saft_hs(eos,rho,T,n,alp_hs)
    end if
    call m_bar(eos,n,mbar,mbar_n,mbar_nn)

    logg = log(g)

    sumn = sum(n)
    if (present(alp)) then
       bigG = 0.0
       do i=1,nce
          bigG = bigG + n(i)*(eos%m(i)-1)
       end do
       bigG = bigG*logg/sumn
       alp = mbar*alp_hs - bigG
    end if

    if (present(alp_rho)) then
       bigG_rho = 0.0
       do i=1,nce
          bigG_rho = bigG_rho + n(i)*(eos%m(i)-1)
       end do
       bigG_rho = bigG_rho*g_rho/(sumn*g)
       alp_rho = mbar*alp_hs_rho - bigG_rho
    end if

    if (present(alp_T)) then
       bigG_T = 0.0
       do i=1,nce
          bigG_T = bigG_T + n(i)*(eos%m(i)-1)
       end do
       bigG_T = bigG_T*g_T/(sumn*g)
       alp_T = mbar*alp_hs_T - bigG_T
    end if

    if (present(alp_n)) then
       bigG_x = 0.0
       do i=1,nce
          bigG_x = bigG_x + n(i)*(eos%m(i)-1)*g_n/g
          bigG_x(i) = bigG_x(i) + (eos%m(i)-1)*logg
       end do
       bigG_n = (bigG_x-bigG)/sumn
       alp_n = mbar_n*alp_hs + mbar*alp_hs_n - bigG_n
    end if

    if (present(alp_rhorho)) then
       alp_rhorho = mbar*alp_hs_rhorho
       do i=1,nce
          alp_rhorho = alp_rhorho - n(i)*(eos%m(i)-1)*(-(g_rho/g)**2 + g_rhorho/g)/sumn
       end do
    end if

    if (present(alp_rhoT)) then
       alp_rhoT = mbar*alp_hs_rhoT
       do i=1,nce
          alp_rhoT = alp_rhoT - n(i)*(eos%m(i)-1)*(-g_rho*g_T/g**2 + g_rhoT/g)/sumn
       end do
    end if

    if (present(alp_rhon)) then
       bigG_rhox = 0.0
       do k=1,nce
          bigG_rhox(k) = bigG_rhox(k) + (eos%m(k)-1)*g_rho/g
          do i=1,nce
             bigG_rhox(k) = bigG_rhox(k) + n(i)*(eos%m(i)-1)*(-g_rho*g_n(k)/g**2 + g_rhon(k)/g)
          end do
       end do
       alp_rhon = mbar_n*alp_hs_rho + mbar*alp_hs_rhon - (bigG_rhox - bigG_rho)/sumn
    end if

    if (present(alp_Tn)) then
       bigG_Tx = 0.0
       do k=1,nce
          bigG_Tx(k) = bigG_Tx(k) + (eos%m(k)-1)*g_T/g
          do i=1,nce
             bigG_Tx(k) = bigG_Tx(k) + n(i)*(eos%m(i)-1)*(-g_T*g_n(k)/g**2 + g_Tn(k)/g)
          end do
       end do
       alp_Tn = mbar_n*alp_hs_T + mbar*alp_hs_Tn - (bigG_Tx - bigG_T)/sumn
    end if

    if (present(alp_TT)) then
       alp_TT = mbar*alp_hs_TT
       do i=1,nce
          alp_TT = alp_TT - n(i)*(eos%m(i)-1)*(-(g_T/g)**2 + g_TT/g)/sumn
       end do
    end if

    if (present(alp_nn)) then
       bigG_xx = 0.0
       do k=1,nce
          do l=1,nce
             alp_nn(k,l) = mbar_nn(k,l)*alp_hs + mbar_n(k)*alp_hs_n(l) &
                  + mbar_n(l)*alp_hs_n(k) + mbar*alp_hs_nn(k,l)
             bigG_xx(k,l) = bigG_xx(k,l) + (eos%m(k)-1)*g_n(l)/g + (eos%m(l)-1)*g_n(k)/g
             do i=1,nce
                bigG_xx(k,l) = bigG_xx(k,l) &
                     + n(i)*(eos%m(i)-1)*(-g_n(l)*g_n(k)/g**2 + g_nn(k,l)/g)
             end do
             alp_nn(k,l) = alp_nn(k,l) - (bigG_xx(k,l) - bigG_n(k) - bigG_n(l))/sumn
          end do
       end do
    end if

  end subroutine alpha_spc_saft_hc


  ! Reduced molar Helmholtz free energy contribution from a hard-sphere fluid.
  ! Should be pretty fast.
  subroutine alpha_spc_saft_hs(eos,rho,T,n,alp,alp_rho,alp_T,alp_n, &
       alp_rhorho,alp_rhoT,alp_rhon,alp_TT,alp_Tn,alp_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: rho, T, n(nce) !< [mol/m^3], [K], [mol]
    real, intent(out) :: alp          !< [-]
    real, intent(out), optional :: alp_rho,alp_T,alp_n(nce)
    real, intent(out), optional :: alp_rhorho, alp_rhoT, alp_rhon(nce), alp_TT
    real, intent(out), optional :: alp_Tn(nce), alp_nn(nce,nce)
    ! Locals.
    real :: e,e_rho,e_T,e_n(nce)
    real :: e_rhorho,e_rhoT,e_rhon(nce),e_TT,e_Tn(nce),e_nn(nce,nce)
    real :: alp_eta,alp_etaeta
    integer :: i,j
    logical :: fir_der_present, sec_der_present

    fir_der_present = present(alp_rho) .or. present(alp_T) .or. present(alp_n)
    sec_der_present = present(alp_rhorho) .or. present(alp_rhoT) .or. present(alp_rhon) .or. &
         present(alp_TT) .or. present(alp_Tn) .or. present(alp_nn)

    if (sec_der_present) then
       call eta(eos,rho,T,n,e,e_rho,e_T,e_n,e_rhorho,e_rhoT,e_rhon,e_TT,e_Tn,e_nn)
    else if (fir_der_present) then
       call eta(eos,rho,T,n,e,e_rho,e_T,e_n)
    else
       call eta(eos,rho,T,n,e)
    end if

    alp = (4*e-3*e**2)/(1-e)**2

    alp_eta = 2*(e-2)/(e-1)**3
    if (present(alp_rho)) alp_rho = alp_eta*e_rho
    if (present(alp_T)) alp_T = alp_eta*e_T
    if (present(alp_n)) alp_n = alp_eta*e_n

    alp_etaeta = 2*(5-2*e)/(e-1)**4
    if (present(alp_rhorho)) alp_rhorho = alp_eta*e_rhorho + alp_etaeta*e_rho**2
    if (present(alp_rhoT)) alp_rhoT = alp_eta*e_rhoT + alp_etaeta*e_rho*e_T
    if (present(alp_rhon)) alp_rhon = alp_eta*e_rhon + alp_etaeta*e_rho*e_n
    if (present(alp_TT)) alp_TT = alp_eta*e_TT + alp_etaeta*e_T**2
    if (present(alp_Tn)) alp_Tn = alp_eta*e_Tn + alp_etaeta*e_T*e_n
    if (present(alp_nn)) then
       do i=1,nce
          do j=i,nce
             alp_nn(i,j) = alp_eta*e_nn(i,j) + alp_etaeta*e_n(i)*e_n(j)
             alp_nn(j,i) = alp_nn(i,j)
          end do
       end do
    end if
  end subroutine alpha_spc_saft_hs


  subroutine g_spc_saft_TVn(eos,T,V,n,g,g_T,g_V,g_n,g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: V,T,n(nce)  !< [m^3], [K], [mol]
    real, intent(out) :: g  !< [-]
    real, intent(out), optional :: g_T,g_V,g_n(nce)
    real, intent(out), optional :: g_VV,g_TV,g_Vn(nce)
    real, intent(out), optional :: g_TT,g_Tn(nce),g_nn(nce,nce)
    ! Locals.
    real :: h, h_T, h_rho,h_n(nce)
    real :: h_rhorho,h_rhoT,h_rhon(nce)
    real :: h_TT,h_Tn(nce),h_nn(nce,nce)
    real :: sumn
    real :: rho
    real :: V_1, V_2, V_3
    integer :: i, j

    logical :: fir_der_present, sec_der_present

    sumn = sum(n)
    rho = sumn/V

    fir_der_present = present(g_V) .or. present(g_T) .or. present(g_n)
    sec_der_present = present(g_VV) .or. present(g_TV) .or. present(g_Vn) .or. &
         present(g_TT) .or. present(g_Tn) .or. present(g_nn)

    if (sec_der_present) then
       call g_ij_spc_saft(eos,rho=rho,T=T,n=n,g=h,g_rho=h_rho,g_T=h_T,g_n=h_n,&
            g_rhorho=h_rhorho,g_rhoT=h_rhoT,g_rhon=h_rhon,g_TT=h_TT,g_Tn=h_Tn,g_nn=h_nn)
    else if (fir_der_present) then
       call g_ij_spc_saft(eos,rho=rho,T=T,n=n,g=h,g_rho=h_rho,g_T=h_T,g_n=h_n)
    else
       call g_ij_spc_saft(eos,rho=rho,T=T,n=n,g=h)
    end if

    V_1 = 1/V
    V_2 = 1/V**2
    V_3 = 1/V**3

    g = h
    if (present(g_T)) g_T = h_T
    if (present(g_TT)) g_TT = h_TT
    if (present(g_TV)) g_TV = -sumn*V_2*h_rhoT
    if (present(g_Tn)) g_Tn = V_1*h_rhoT + h_Tn

    if (present(g_V)) g_V = -sumn*V_2*h_rho
    if (present(g_VV)) g_VV = 2*sumn*V_3*h_rho + (sumn*V_2)**2*h_rhorho
    if (present(g_Vn)) g_Vn = -sumn*V_3*h_rhorho - V_2*h_rho - sumn*V_2*h_rhon

    if (present(g_n)) g_n = V_1*h_rho + h_n
    if (present(g_nn)) then
       do i = 1,nce
          do j = i,nce
             g_nn(i,j) = V_2*h_rhorho + V_1*(h_rhon(i) + h_rhon(j)) + h_nn(i,j)
             g_nn(j,i) = g_nn(i,j)
          end do
       end do
    end if

  end subroutine g_spc_saft_TVn


  ! The radial pair distribution function between a segment on molecule i and a
  ! segment on molecule j for simplified PC-SAFT.
  subroutine g_ij_spc_saft(eos,rho,T,n,g,g_rho,g_T,g_n,g_rhorho,g_rhoT,g_rhon,g_TT,g_Tn,g_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: rho,T,n(nce)  !< [mol/m^3], [K], [mol]
    real, intent(out) :: g    !< [-]
    real, intent(out), optional :: g_rho,g_T,g_n(nce)
    real, intent(out), optional :: g_rhorho,g_rhoT,g_rhon(nce)
    real, intent(out), optional :: g_TT,g_Tn(nce),g_nn(nce,nce)
    ! Locals.
    integer :: i,j
    real :: e,e_rho,e_T,e_n(nce)
    real :: e_rhorho,e_rhoT,e_rhon(nce),e_TT,e_Tn(nce),e_nn(nce,nce)
    real :: g_eta, g_etaeta
    logical :: fir_der_present, sec_der_present

    fir_der_present = present(g_rho) .or. present(g_T) .or. present(g_n)
    sec_der_present = present(g_rhorho) .or. present(g_rhoT) .or. present(g_rhon) .or. &
         present(g_TT) .or. present(g_Tn) .or. present(g_nn)

    if (sec_der_present) then
       call eta(eos,rho,T,n,e,e_rho,e_T,e_n,e_rhorho,e_rhoT,e_rhon,e_TT,e_Tn,e_nn)
    else if (fir_der_present) then
       call eta(eos,rho,T,n,e,e_rho,e_T,e_n)
    else
       call eta(eos,rho,T,n,e)
    end if

    g = (1-e/2)/(1-e)**3

    g_eta = (5-2*e)/(2*(1-e)**4)
    if (present(g_rho)) g_rho = g_eta*e_rho
    if (present(g_T)) g_T = g_eta*e_T
    if (present(g_n)) g_n = g_eta*e_n

    g_etaeta = 3*(e-3)/(e-1)**5
    if (present(g_rhorho)) g_rhorho = g_eta*e_rhorho + g_etaeta*e_rho**2
    if (present(g_rhoT)) g_rhoT = g_eta*e_rhoT + g_etaeta*e_rho*e_T
    if (present(g_rhon)) g_rhon = g_eta*e_rhon + g_etaeta*e_rho*e_n
    if (present(g_TT)) g_TT = g_eta*e_TT + g_etaeta*e_T**2
    if (present(g_Tn)) g_Tn = g_eta*e_Tn + g_etaeta*e_T*e_n
    if (present(g_nn)) then
       do i=1,nce
          do j=1,nce
             g_nn(i,j) = g_eta*e_nn(i,j) + g_etaeta*e_n(i)*e_n(j)
          end do
       end do
    end if

  end subroutine g_ij_spc_saft

  subroutine m2e2s3_mean(eos,T,n,m2e2s3,m2e2s3_T,m2e2s3_n,m2e2s3_TT,m2e2s3_Tn,m2e2s3_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: T,n(nce) !< [K], [mol]
    real, intent(out) :: m2e2s3 !< [-]
    real, intent(out), optional :: m2e2s3_T,m2e2s3_n(nce)
    real, intent(out), optional :: m2e2s3_TT, m2e2s3_Tn(nce), m2e2s3_nn(nce,nce)
    ! Locals.
    integer :: k,l
    real :: E(nce,nce), sumn
    sumn = sum(n)

    do k=1,nce
      do l=1,nce
        E(k,l) = eos%m(k)*eos%m(l)*(eos%eps_depth_divk(k,l)**2)*eos%sigma(k,l)**3
      end do
    end do

    ! Value
    m2e2s3 = 0.0
    do k=1,nce
      m2e2s3 = m2e2s3 + n(k)*dot_product(n,E(:,k))
    end do
    m2e2s3 = m2e2s3/(T*sumn)**2

    ! T-derivative
    if (present(m2e2s3_T)) then
       m2e2s3_T = -2*m2e2s3/T
    end if

    ! n-derivative
    if (present(m2e2s3_n)) then
      do k=1,nce
        m2e2s3_n(k) = dot_product(n,E(:,k)+E(k,:))
      end do
      m2e2s3_n = m2e2s3_n/(T*sumn)**2 - 2*m2e2s3/sumn
    end if

    ! TT-derivative
    if (present(m2e2s3_TT)) then
      m2e2s3_TT = 6*m2e2s3/T**2
    end if

    ! Tn-derivative
    if (present(m2e2s3_Tn)) then
      if (present(m2e2s3_n)) then
        m2e2s3_Tn = -2*m2e2s3_n/T
      else
        do k=1,nce
          m2e2s3_Tn(k) = dot_product(n,E(:,k)+E(k,:))
        end do
        m2e2s3_Tn = -2*(m2e2s3_Tn/(T*sumn)**2 - 2*m2e2s3/sumn)/T
      end if
    end if

    ! nn-derivative
    if (present(m2e2s3_nn)) then
      do k=1,nce
        do l=k,nce
          m2e2s3_nn(k,l) = 2*m2e2s3/sumn**2 - 2*m2e2s3_n(l)/sumn &
               - (2/sumn**3)*dot_product(n,E(:,k)+E(k,:))/T**2 + (E(k,l)+E(l,k))/(T*sumn)**2
          m2e2s3_nn(l,k) = m2e2s3_nn(k,l)
        end do
      end do
    end if

  end subroutine m2e2s3_mean


  subroutine m2e1s3_mean(eos,T,n,m2e1s3,m2e1s3_T,m2e1s3_n,m2e1s3_TT,m2e1s3_Tn,m2e1s3_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: T,n(nce) !< [K], [mol]
    real, intent(out) :: m2e1s3 !< [-]
    real, intent(out), optional :: m2e1s3_T, m2e1s3_n(nce)
    real, intent(out), optional :: m2e1s3_TT, m2e1s3_Tn(nce), m2e1s3_nn(nce,nce)
    ! Locals.
    integer :: k,l
    real :: sumn
    real :: D(nce,nce)
    sumn = sum(n)

    do k=1,nce
      do l=1,nce
        D(k,l) = eos%m(k)*eos%m(l)*eos%eps_depth_divk(k,l)*eos%sigma(k,l)**3
      end do
    end do

    ! Value
    m2e1s3 = 0.0
    do k=1,nce
      m2e1s3 = m2e1s3 + n(k)*dot_product(n,D(:,k))
    end do
    m2e1s3 = m2e1s3/(T*sumn**2)

    ! T-derivative
    if (present(m2e1s3_T)) then
       m2e1s3_T = -m2e1s3/T
    end if

    ! n-derivative
    if (present(m2e1s3_n)) then
       do k=1,nce
         m2e1s3_n(k) = dot_product(n,D(:,k)+D(k,:))
       end do
       m2e1s3_n = m2e1s3_n/(T*sumn*sumn) - 2*m2e1s3/sumn
    end if

    ! TT-derivative
    if (present(m2e1s3_TT)) then
       m2e1s3_TT = 2*m2e1s3/T**2
    end if

    ! Tn-derivative
    if (present(m2e1s3_Tn)) then
      if (present(m2e1s3_n)) then
        m2e1s3_Tn = -m2e1s3_n/T
      else
        do k=1,nce
          m2e1s3_n(k) = dot_product(n,D(:,k)+D(k,:))
        end do
        m2e1s3_Tn = -m2e1s3_Tn/(T*T**sumn*sumn) + 2*m2e1s3/(T*sumn)
      end if
    end if

     ! nn-derivative
     if (present(m2e1s3_nn)) then
       do k=1,nce
         do l=k,nce
           m2e1s3_nn(k,l) = 2*m2e1s3/sumn**2 - 2*m2e1s3_n(l)/sumn &
                - (2/sumn**3)*dot_product(n,D(:,k)+D(k,:))/T + (D(k,l)+D(l,k))/(T*sumn**2)
           m2e1s3_nn(l,k) = m2e1s3_nn(k,l)
         end do
       end do
     end if

  end subroutine m2e1s3_mean


  ! A power series approximation of a perturbation theory integral.
  subroutine I_1(eos,rho,T,n,i1,i1_rho,i1_t,i1_n,&
       i1_rhorho,i1_rhoT,i1_rhon,i1_TT,i1_Tn,i1_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: rho, T, n(nce) !< [mol/m^3], [K], [mol]
    real, intent(out) :: i1           !< [-]
    real, intent(out), optional :: i1_rho,i1_T,i1_n(nce)
    real, intent(out), optional :: i1_rhorho,i1_rhoT,i1_rhon(nce)
    real, intent(out), optional :: i1_TT,i1_Tn(nce),i1_nn(nce,nce)
    ! Locals.
    integer :: i,k,l
    real :: a(0:6),a_n(0:6,nce),a_nn(0:6,nce,nce)
    real :: e,e_rho,e_T,e_n(nce)
    real :: e_rhorho,e_rhoT,e_rhon(nce),e_TT,e_Tn(nce),e_nn(nce,nce)
    real :: i1_eta, i1_etaeta

    call a_i(eos,n,a,a_n,a_nn)
    call eta(eos,rho,T,n,e=e,e_rho=e_rho,e_T=e_T,e_n=e_n,&
         e_rhorho=e_rhorho,e_rhoT=e_rhoT,e_rhon=e_rhon,e_TT=e_TT,e_Tn=e_Tn,e_nn=e_nn)

    i1 = 0.0
    do i=0,6
       i1 = i1 + a(i)*e**i
    end do

    i1_eta = 0.0
    i1_etaeta = 0.0
    do i=1,6
       i1_eta = i1_eta+a(i)*i*e**(i-1)
       i1_etaeta = i1_etaeta + i*(i-1)*a(i)*e**(i-2)
    end do

    if (present(i1_rho)) then
       i1_rho = i1_eta*e_rho
    end if

    if (present(i1_T)) then
       i1_T = i1_eta*e_T
    end if

    if (present(i1_n)) then
       i1_n = i1_eta*e_n
       do k=1,nce
          do i=0,6
             i1_n(k) = i1_n(k) + a_n(i,k)*e**i
          end do
       end do
    end if

    if (present(i1_rhorho)) then
       i1_rhorho = i1_etaeta*e_rho**2 + i1_eta*e_rhorho
    end if

    if (present(i1_rhoT)) then
       i1_rhoT = i1_etaeta*e_rho*e_T + i1_eta*e_rhoT
    end if

    if (present(i1_rhon)) then
       i1_rhon = i1_etaeta*e_rho*e_n + i1_eta*e_rhon
       do k=1,nce
          do i=1,6
             i1_rhon(k) = i1_rhon(k) + a_n(i,k)*i*e**(i-1)*e_rho
          end do
       end do
    end if

    if (present(i1_TT)) then
       i1_TT = i1_etaeta*e_T**2 + i1_eta*e_TT
    end if

    if (present(i1_Tn)) then
       i1_Tn = i1_etaeta*e_T*e_n + i1_eta*e_Tn
       do k=1,nce
          do i=1,6
             i1_Tn(k) = i1_Tn(k) + a_n(i,k)*i*e**(i-1)*e_T
          end do
       end do
    end if

    if (present(i1_nn)) then
       do k=1,nce
          do l=k,nce
             i1_nn(k,l) = i1_etaeta*e_n(k)*e_n(l) + i1_eta*e_nn(k,l)
             do i=0,6
                i1_nn(k,l) = i1_nn(k,l) + a_nn(i,l,k)*e**i +  &
                     i*e**(i-1)*(a_n(i,k)*e_n(l) + a_n(i,l)*e_n(k))
             end do
             i1_nn(l,k) = i1_nn(k,l)
          end do
       end do
    end if

  end subroutine I_1

  !> A power series approximation of a perturbation theory integral.
  subroutine I_2(eos,rho,T,n,i2,i2_rho,i2_t,i2_n,&
       i2_rhorho,i2_rhoT,i2_rhon,i2_TT,i2_Tn,i2_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: rho, T, n(nce) !< [mol/m^3], [K], [mol]
    real, intent(out) :: i2           !< [-]
    real, intent(out), optional :: i2_rho,i2_T,i2_n(nce)
    real, intent(out), optional :: i2_rhorho,i2_rhoT,i2_rhon(nce)
    real, intent(out), optional :: i2_TT,i2_Tn(nce),i2_nn(nce,nce)
    ! Locals.
    integer :: i,k,l
    real :: b(0:6),b_n(0:6,nce),b_nn(0:6,nce,nce)
    real :: e,e_rho,e_T,e_n(nce)
    real :: e_rhorho,e_rhoT,e_rhon(nce),e_TT,e_Tn(nce),e_nn(nce,nce)
    real :: i2_eta, i2_etaeta

    call b_i(eos,n,b,b_n,b_nn)
    call eta(eos,rho,T,n,e=e,e_rho=e_rho,e_T=e_T,e_n=e_n,&
         e_rhorho=e_rhorho,e_rhoT=e_rhoT,e_rhon=e_rhon,e_TT=e_TT,e_Tn=e_Tn,e_nn=e_nn)

    i2 = 0.0
    do i=0,6
       i2 = i2 + b(i)*e**i
    end do

    i2_eta = 0.0
    i2_etaeta = 0.0
    do i=1,6
       i2_eta = i2_eta+b(i)*i*e**(i-1)
       i2_etaeta = i2_etaeta + i*(i-1)*b(i)*e**(i-2)
    end do

    if (present(i2_rho)) then
       i2_rho = i2_eta*e_rho
    end if

    if (present(i2_T)) then
       i2_T = i2_eta*e_T
    end if

    if (present(i2_n)) then
       i2_n = i2_eta*e_n
       do k=1,nce
          do i=0,6
             i2_n(k) = i2_n(k) + b_n(i,k)*e**i
          end do
       end do
    end if

    if (present(i2_rhorho)) then
       i2_rhorho = i2_etaeta*e_rho**2 + i2_eta*e_rhorho
    end if

    if (present(i2_rhoT)) then
       i2_rhoT = i2_etaeta*e_rho*e_T + i2_eta*e_rhoT
    end if

    if (present(i2_rhon)) then
       i2_rhon = i2_etaeta*e_rho*e_n + i2_eta*e_rhon
       do k=1,nce
          do i=1,6
             i2_rhon(k) = i2_rhon(k) + b_n(i,k)*i*e**(i-1)*e_rho
          end do
       end do
    end if

    if (present(i2_TT)) then
       i2_TT = i2_etaeta*e_T**2 + i2_eta*e_TT
    end if

    if (present(i2_Tn)) then
       i2_Tn = i2_etaeta*e_T*e_n + i2_eta*e_Tn
       do k=1,nce
          do i=1,6
             i2_Tn(k) = i2_Tn(k) + b_n(i,k)*i*e**(i-1)*e_T
          end do
       end do
    end if

    if (present(i2_nn)) then
       do k=1,nce
          do l=k,nce
             i2_nn(k,l) = i2_etaeta*e_n(k)*e_n(l) + i2_eta*e_nn(k,l)
             do i=0,6
                i2_nn(k,l) = i2_nn(k,l) + b_nn(i,k,l)*e**i + &
                     i*e**(i-1)*(b_n(i,k)*e_n(l) + b_n(i,l)*e_n(k))
             end do
             i2_nn(l,k) = i2_nn(k,l)
          end do
       end do
    end if

  end subroutine I_2


  !> The quantities a_i(m_bar) and its derivatives.
  !> This is the only routine which accesses a_mat directly.
  subroutine a_i(eos,n,a,a_n,a_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real,intent(in):: n(nce)    !< [mol]
    real,intent(out)::a(0:6)   !< [-]
    real,intent(out),optional::a_n(0:6,nce), a_nn(0:6,nce,nce)
    ! Locals.
    integer :: i,k,l
    real :: mbar,mbar_n(nce),mbar_nn(nce,nce),mbar_inv
    real :: G,H

    call m_bar(eos,n,mbar,mbar_n,mbar_nn)
    mbar_inv = 1/mbar

    H = (mbar-1)*mbar_inv
    G = H*(mbar-2)*mbar_inv

    do i=0,6
       a(i) = a_mat(0,i) + H*a_mat(1,i) + G*a_mat(2,i)
    end do

    if (present(a_n)) then
       do k=1,nce
          H = mbar_n(k)*mbar_inv**2
          G = mbar_n(k)*(3*mbar-4)*mbar_inv**3
          do i=0,6
             a_n(i,k) = H*a_mat(1,i) + G*a_mat(2,i)
          end do
       end do
    end if

    if (present(a_nn)) then
       do k=1,nce
          do l=k,nce
             H = mbar_n(k)*mbar_n(l)
             G = mbar*mbar_nn(k,l)-2*H
             do i=0,6
                a_nn(i,k,l) = G*a_mat(1,i)*mbar_inv**3 &
                     + ((3*mbar-4)*G + 4*H)*a_mat(2,i)*mbar_inv**4
                a_nn(i,l,k) = a_nn(i,k,l)
             end do
          end do
       end do
    end if

  end subroutine a_i





  ! The quantities b_i(m_bar) and its derivatives.
  ! The only routine which accesses b_mat directly.
  subroutine b_i(eos,n,b,b_n,b_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real,intent(in):: n(nce)    !< [mol]
    real,intent(out)::b(0:6)   !< [-]
    real,intent(out),optional::b_n(0:6,nce), b_nn(0:6,nce,nce)
    ! Locals.
    integer :: i,k,l
    real :: mbar,mbar_n(nce),mbar_nn(nce,nce),mbar_inv
    real :: G,H

    call m_bar(eos,n,mbar,mbar_n,mbar_nn)
    mbar_inv = 1/mbar

    H = (mbar-1)*mbar_inv
    G = H*(mbar-2)*mbar_inv

    do i=0,6
       b(i) = b_mat(0,i) + H*b_mat(1,i) + G*b_mat(2,i)
    end do

    if (present(b_n)) then
       do k=1,nce
          H = mbar_n(k)*mbar_inv**2
          G = mbar_n(k)*(3*mbar-4)*mbar_inv**3
          do i=0,6
             b_n(i,k) = H*b_mat(1,i) + G*b_mat(2,i)
          end do
       end do
    end if

    if (present(b_nn)) then
       do k=1,nce
          do l=k,nce
             H = mbar_n(k)*mbar_n(l)
             G = mbar*mbar_nn(k,l)-2*H
             do i=0,6
                b_nn(i,k,l) = G*b_mat(1,i)*mbar_inv**3 &
                     + ((3*mbar-4)*G + 4*H)*b_mat(2,i)*mbar_inv**4
                b_nn(i,l,k) = b_nn(i,k,l)
             end do
          end do
       end do
    end if

  end subroutine b_i


  subroutine m_bar(eos,n,mbar,mbar_n,mbar_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in)  :: n(nce)          !< [mol]
    real, intent(out) :: mbar           !< [-]
    real, intent(out) :: mbar_n(nce)     !< [1/mol]
    real, intent(out) :: mbar_nn(nce,nce) !< [1/mol^2]
    real :: sumn
    integer :: i,j

    sumn = sum(n)
    mbar = dot_product(n,eos%m)/sumn
    mbar_n = (eos%m-mbar)/sumn
    do i=1,nce
       do j=1,nce
          mbar_nn(i,j) = -(mbar_n(i)+mbar_n(j))/sumn
       end do
    end do
  end subroutine m_bar


  ! The compressibility term, defined as
  ! (1 + Z^{hc} + rho*dZ^{hc}/drho)^{-1}
  subroutine C_1(eos,rho,T,n,c1,c1_rho,c1_t,c1_n,&
       c1_rhorho,c1_rhoT,c1_rhon,c1_TT,c1_Tn,c1_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: rho, T, n(nce) !< [mol/m^3], [K], [mol]
    real, intent(out) :: C1           !< [-]
    real, intent(out), optional :: c1_rho,c1_t,c1_n(nce)
    real, intent(out), optional :: c1_rhorho,c1_rhot,c1_rhon(nce)
    real, intent(out), optional :: c1_tt,c1_tn(nce),c1_nn(nce,nce)
    ! Locals.
    integer :: i,j
    real :: mbar, mbar_n(nce), mbar_nn(nce,nce)
    real :: e, e2, e3, e4
    real :: e_rho, e_T, e_n(nce)
    real :: e_rhorho, e_rhoT, e_rhon(nce), e_TT, e_Tn(nce), e_nn(nce,nce)
    real :: c1_eta, c1_etaeta
    real :: O1,O2
    real :: O1_eta,O2_eta
    real :: O1_etaeta,O2_etaeta
    logical :: fir_der_present, sec_der_present

    fir_der_present = present(c1_rho) .or. present(c1_T) .or. present(c1_n)
    sec_der_present = present(c1_rhorho) .or. present(c1_rhoT) .or. &
         present(c1_rhon) .or. present(c1_TT) .or. &
         present(c1_Tn) .or. present(c1_nn)

    if (sec_der_present) then
       call eta(eos,rho,T,n,e=e,e_rho=e_rho,e_T=e_T,e_n=e_n,&
            e_rhorho=e_rhorho,e_rhoT=e_rhoT,e_rhon=e_rhon,e_TT=e_TT,e_Tn=e_Tn,e_nn=e_nn)
    else if ( fir_der_present ) then
       call eta(eos,rho,T,n,e=e,e_rho=e_rho,e_T=e_T,e_n=e_n)
    else
       call eta(eos,rho,T,n,e=e)
    end if

    e2 = e*e
    e3 = e2*e
    e4 = e2*e2
    O1 = (8*e-2*e2)/(1-e)**4
    O2 = (20*e-27*e2+12*e3-2*e4)/((1-e)*(2-e))**2
    call m_bar(eos,n,mbar,mbar_n,mbar_nn)

    c1 = 1.0/(1 + mbar*O1 + (1-mbar)*O2)

    if (fir_der_present .or. sec_der_present) then
       O1_eta = (-4*e2+20*e+8)/(1-e)**5
       O2_eta = (2*e3+12*e2-48*e+40)/((1-e)*(2-e))**3
       c1_eta = -(mbar*O1_eta + (1-mbar)*O2_eta)*c1**2
       if (sec_der_present) then
          O1_etaeta = ((-8*e+20)*(1-e) + (-4*e2+20*e+8)*5)/(1-e)**6
          O2_etaeta = ((6*e2+24*e-48)*(1-e)*(2-e) + &
               (2*e3+12*e2-48*e+40)*(2-e + 1-e)*3)/((1-e)*(2-e))**4
          c1_etaeta = mbar*O1_etaeta + (1-mbar)*O2_etaeta
          c1_etaeta = 2*c1_eta**2/c1 - c1_etaeta*c1**2
       end if
    end if

    if (present(c1_rho)) c1_rho = c1_eta*e_rho

    if (present(c1_T)) c1_T = c1_eta*e_T

    if (present(c1_n)) then
       c1_n = c1_eta*e_n - c1**2*mbar_n*(O1-O2)
    end if

    if (present(c1_rhorho)) then
       c1_rhorho = c1_etaeta*e_rho**2 + c1_eta*e_rhorho
    end if

    if (present(c1_rhoT)) then
       c1_rhoT = c1_etaeta*e_rho*e_T + c1_eta*e_rhoT
    end if

    if (present(c1_TT)) then
       c1_TT = c1_etaeta*e_T**2 + c1_eta*e_TT
    end if

    if (present(c1_rhon)) then
       c1_rhon = c1_etaeta*e_rho*e_n + c1_eta*e_rhon &
            - 2*c1*c1_rho*mbar_n*(O1-O2) - c1**2*mbar_n*(O1_eta - O2_eta)*e_rho
    end if

    if (present(c1_Tn)) then
       c1_Tn = c1_etaeta*e_T*e_n + c1_eta*e_Tn &
            - 2*c1*c1_T*mbar_n*(O1-O2) - c1**2*mbar_n*(O1_eta - O2_eta)*e_T
    end if

    if (present(c1_nn)) then
       do i=1,nce
          do j=1,nce
             c1_nn(i,j) = c1_etaeta*e_n(i)*e_n(j) + c1_eta*e_nn(i,j)&
                  - 2*c1*c1_eta*(O1-O2)*(mbar_n(i)*e_n(j) + mbar_n(j)*e_n(i)) &
                  - c1**2*(O1_eta-O2_eta)*(mbar_n(i)*e_n(j) + mbar_n(j)*e_n(i)) &
                  + 2*c1**3*(O1-O2)**2*mbar_n(i)*mbar_n(j) - c1**2*mbar_nn(i,j)*(O1-O2)
          end do
       end do
    end if

  end subroutine C_1


  ! Calculates the functions zeta(0),...,zeta(3).
  ! zeta(3) equals eta, the packing fraction.
  subroutine zeta(eos,rho,T,n,z,z_rho,z_T,z_n,z_rhorho,z_rhoT,z_rhon,z_TT,z_Tn,z_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in)    :: rho, T, n(nce) !< [mol/m^3], [K], [mol]
    real, intent(out)   :: z(0:3)        !< [m^(i-3)]
    real, intent(out), optional :: z_rho(0:3), z_T(0:3), z_n(0:3,nce)
    real, intent(out), optional :: z_rhorho(0:3), z_rhoT(0:3), z_rhon(0:3,nce)
    real, intent(out), optional :: z_TT(0:3), z_Tn(0:3,nce), z_nn(0:3,nce,nce)
    ! Locals.
    real :: z_x(0:3,nce) ! Derivative with respect to molfraction.
    real :: z_rhox(0:3,nce)
    real :: z_Tx(0:3,nce)
    integer :: i,j,k
    real :: d(nce),d_T(nce),d_TT(nce),temp(4)
    real :: sumn
    real :: z_div_rho(0:3)
    sumn = sum(n)

    call calc_d(eos,T,d=d,d_T=d_T,d_TT=d_TT)

    z = 0.0
    do i=1,nce
       temp = (/ 1.0,d(i),d(i)**2,d(i)**3 /)
       z(0:3) = z(0:3) + n(i)*eos%m(i)*temp
    end do
    z_div_rho = (PI/6)*z*N_AVOGADRO/sumn
    z = z_div_rho*rho

    if (present(z_rho)) then
       z_rho = z_div_rho
    end if

    if (present(z_T)) then
       z_T = 0.0
       do i=1,nce
          temp = (/0.0,1.0,d(i),d(i)**2 /)
          z_T(1:3) = z_T(1:3) + n(i)*eos%m(i)*d_T(i)*temp(2:4)
       end do
       z_T(2) = 2*z_T(2)
       z_T(3) = 3*z_T(3)
       z_T = (PI/6)*rho*z_T*N_AVOGADRO/sumn
    end if

    if (present(z_n)) then
       do i=1,nce
          temp = (/ 1.0,d(i),d(i)**2,d(i)**3 /)
          z_x(0:3,i) = eos%m(i)*temp
       end do
       z_x = (PI/6)*rho*z_x*N_AVOGADRO
       do i=1,nce
          z_n(:,i) = (z_x(:,i)-z)/sumn
       end do
    end if

    if (present(z_rhorho)) then
       z_rhorho = 0.0
    end if

    if (present(z_rhoT)) then
       if (present(z_T)) then
          z_rhoT = z_T/rho
       else
          z_rhoT = 0.0
          do i=1,nce
             temp = (/0.0,1.0,d(i),d(i)**2 /)
             z_rhoT(1:3) = z_rhoT(1:3) + n(i)*eos%m(i)*d_T(i)*temp(2:4)
          end do
          z_rhoT(2) = 2*z_rhoT(2)
          z_rhoT(3) = 3*z_rhoT(3)
          z_rhoT = (PI/6)*z_rhoT*N_AVOGADRO/sumn
       end if
    end if

    if (present(z_rhon)) then
       do i=1,nce
          temp = (/ 1.0,d(i),d(i)**2,d(i)**3 /)
          z_rhox(0:3,i) = eos%m(i)*temp
       end do
       z_rhox = (PI/6)*z_rhox*N_AVOGADRO
       do i=1,nce
          z_rhon(:,i) = (z_rhox(:,i)-z_rho)/sumn
       end do
    end if

    if (present(z_TT)) then
       z_TT = 0.0
       do i=1,nce
          z_TT(1) = z_TT(1) + n(i)*eos%m(i)*d_TT(i)
          z_TT(2) = z_TT(2) + n(i)*eos%m(i)*(d_T(i)**2 + d(i)*d_TT(i))
          z_TT(3) = z_TT(3) + n(i)*eos%m(i)*(2*d(i)*d_T(i)**2 + d(i)**2*d_TT(i))
       end do
       z_TT(1) = z_TT(1)*(PI/6)*rho*N_AVOGADRO/sumn
       z_TT(2) = z_TT(2)*(PI/3)*rho*N_AVOGADRO/sumn
       z_TT(3) = z_TT(3)*(PI/2)*rho*N_AVOGADRO/sumn
    end if

    if (present(z_Tn)) then
       z_Tx = 0.0
       do k=1,nce
          z_Tx(1,k) = eos%m(k)*d_T(k)
          z_Tx(2,k) = 2*eos%m(k)*d(k)*d_T(k)
          z_Tx(3,k) = 3*eos%m(k)*d(k)**2*d_T(k)
       end do
       z_Tx = (PI/6)*rho*z_Tx*N_AVOGADRO
       do i=1,nce
          z_Tn(:,i) = (z_Tx(:,i)-z_T)/sumn
       end do
    end if

    if (present(z_nn)) then
       do i=1,nce
          do j=1,nce
             z_nn(:,i,j) = -(z_n(:,i)+z_n(:,j))
          end do
       end do
       z_nn = z_nn/sumn
    end if

  end subroutine zeta


  ! The packing fraction eta and its derivatives.
  subroutine eta(eos,rho,T,n,e,e_rho,e_T,e_n,e_rhorho,e_rhoT,e_rhon,e_TT,e_Tn,e_nn)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: rho, T, n(nce) !< [mol/m^3], [K], [mol]
    real, intent(out) :: e            !< [-]
    real, intent(out), optional :: e_rho, e_T, e_n(nce)
    real, intent(out), optional :: e_rhorho, e_rhoT, e_rhon(nce)
    real, intent(out), optional :: e_TT, e_Tn(nce), e_nn(nce,nce)
    ! Locals.
    integer :: i,k,j
    real :: d(nce),d_T(nce),d_TT(nce)
    real :: e_x(nce), e_rhox(nce), e_Tx(nce)
    real :: sumn
    real :: e_div_rho

    call calc_d(eos,T,d=d,d_T=d_T,d_TT=d_TT)
    sumn = sum(n)

    e = 0.0
    do i=1,nce
       e = e + n(i)*eos%m(i)*d(i)**3
    end do
    e_div_rho = (PI/6)*e*N_AVOGADRO/sumn
    e = e_div_rho*rho

    if (present(e_rho)) then
       e_rho = e_div_rho
    end if

    if (present(e_T)) then
       e_T = 0.0
       do i=1,nce
          e_T = e_T + n(i)*eos%m(i)*d(i)**2*d_T(i)
       end do
       e_T = (PI/2)*rho*e_T*N_AVOGADRO/sumn
    end if

    if (present(e_n)) then
       do k=1,nce
          e_x(k) = eos%m(k)*d(k)**3
       end do
       e_x = (PI/6)*rho*e_x*N_AVOGADRO
       e_n = (e_x-e)/sumn
    end if

    if (present(e_rhorho)) then
       e_rhorho = 0.0
    end if

    if (present(e_rhoT)) then
       if (present(e_T)) then
          e_rhoT = e_T/rho
       else
          e_rhoT = 0.0
          do i=1,nce
             e_rhoT = e_rhoT + n(i)*eos%m(i)*d_T(i)*d(i)**2
          end do
          e_rhoT = (PI/2)*e_rhoT*N_AVOGADRO/sumn
       end if
    end if

    if (present(e_rhon)) then
       do i=1,nce
          e_rhox(i) = eos%m(i)*d(i)**3
       end do
       e_rhox = (PI/6)*e_rhox*N_AVOGADRO
       e_rhon = (e_rhox-e_rho)/sumn
    end if

    if (present(e_TT)) then
       e_TT = 0.0
       do i=1,nce
          e_TT = e_TT + n(i)*eos%m(i)*(2*d(i)*d_T(i)**2 + d(i)**2*d_TT(i))
       end do
       e_TT = e_TT*(PI/2)*rho*N_AVOGADRO/sumn
    end if

    if (present(e_Tn)) then
       e_Tx = 0.0
       do k=1,nce
          e_Tx(k) = 3*eos%m(k)*d(k)**2*d_T(k)
       end do
       e_Tx = (PI/6)*rho*e_Tx*N_AVOGADRO
       e_Tn = (e_Tx-e_T)/sumn
    end if

    if (present(e_nn)) then
       do i=1,nce
          do j=1,nce
             e_nn(i,j) = -(e_n(i) + e_n(j))
          end do
       end do
       e_nn = e_nn/sumn
    end if
  end subroutine eta


  ! The segment diameter d_i and its derivatives.
  ! These diameters are often needed, but since they require computing an
  ! exponential, calc_d should not be called unecessary.
  subroutine calc_d(eos,T,d,d_T,d_TT)
    class(PCSAFT_eos), intent(in) :: eos
    real, intent(in) :: T            !< [mol/m^3], [K], [mol]
    real, intent(out) :: d(nce)       !< [m]
    real, intent(out), optional :: d_T(nce)
    real, intent(out), optional :: d_TT(nce)
    ! Locals.
    integer :: i
    real :: sig_expo(nce)
    real :: Tinv

    Tinv = 1/T

    do i=1,nce
       sig_expo(i) = eos%sigma(i,i)*exp(-3*eos%eps_depth_divk(i,i)*Tinv)
       d(i) = eos%sigma(i,i) - 0.12*sig_expo(i)
    end do


    if (present(d_T)) then
       do i = 1,nce
          d_T(i) = -0.36*eos%eps_depth_divk(i,i)*Tinv*Tinv*sig_expo(i)
       end do
    end if

    if (present(d_TT)) then
       do i=1,nce
          d_TT(i) = sig_expo(i)*(0.72*eos%eps_depth_divk(i,i) - &
               Tinv*1.08*eos%eps_depth_divk(i,i)**2)*Tinv**3
       end do
    end if

  end subroutine calc_d

  ! subroutine cleanup_pc_saft_nonassoc()
  !   if (allocated(m)) deallocate(m)
  !   if (allocated(sigma)) deallocate(sigma)
  !   if (allocated(sigma_cube)) deallocate(sigma_cube)
  !   if (allocated(eps_depth_divk)) deallocate(eps_depth_divk)
  ! end subroutine cleanup_pc_saft_nonassoc

  subroutine pcsaft_allocate_and_init(eos,nc,eos_label)
    use utilities, only: allocate_nc_x_nc, allocate_nc
    ! Passed object:
    class(pcsaft_eos), intent(inout) :: eos
    ! Input:
    integer, intent(in) :: nc !< Number of components
    character(len=*), intent(in) :: eos_label !< EOS label
    ! Locals
    call eos%dealloc()
    call allocate_nc(eos%m,nc,"eos%m")
    call allocate_nc_x_nc(eos%sigma,nc,"eos%sigma")
    call allocate_nc_x_nc(eos%sigma_cube,nc,"eos%sigma_cube")
    call allocate_nc_x_nc(eos%eps_depth_divk,nc,"eos%eps_depth_divk")
  end subroutine pcsaft_allocate_and_init

  subroutine assign_pcsaft(This, other)
    class(PCSAFT_eos), intent(inout) :: this
    class(*), intent(in)           :: other
    ! Locals
    integer :: nc
    select type (other)
    class is (PCSAFT_eos)
      call this%assign_base_eos_param(other)
      if (allocated(other%m)) then
        nc = size(other%m)
        call this%allocate_and_init(nc,other%eosid)
        this%m = other%m
        if (allocated(other%sigma)) this%sigma = other%sigma
        if (allocated(other%eps_depth_divk)) this%eps_depth_divk = other%eps_depth_divk
        if (allocated(other%sigma_cube)) this%sigma_cube = other%sigma_cube
      endif
    class default
      print *,"assign_pcsaft: Should not be here"
    end select
  end subroutine assign_pcsaft

  !! \author Morten H
  subroutine pcsaft_dealloc(eos)
    use utilities, only: deallocate_real, deallocate_real_2
    ! Passed object:
    class(pcsaft_eos), intent(inout) :: eos
    !
    call deallocate_real(eos%m,"eos%m")
    call deallocate_real_2(eos%sigma,"eos%sigma")
    call deallocate_real_2(eos%sigma_cube,"eos%sigma_cube")
    call deallocate_real_2(eos%eps_depth_divk,"eos%eps_depth_divk")
  end subroutine pcsaft_dealloc

end module pc_saft_nonassoc
