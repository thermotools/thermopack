!> Module responsible for radial distribution functions.
module saft_rdf
  use eosdata, only: eosPC_SAFT, cpaPR, cpaSRK, eosBH_pert
  implicit none
  save

  logical :: useSimplifiedCPA = .FALSE.

  public :: master_saft_rdf
  public :: useSimplifiedCPA
  private

contains

  subroutine master_saft_rdf(eos,nc,T,V,n,i,j,g,g_T,g_V,g_n,g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)
    !> Depends on component indices i,j only for eosBH_pert
    use pc_saft_nonassoc, only: g_spc_saft_tvn, PCSAFT_eos
    use saftvrmie_hardsphere, only: calc_gij_boublik
    use saftvrmie_containers, only: saftvrmie_var
    use thermopack_var, only: base_eos_param
    use utilities, only: get_thread_index
    class(base_eos_param), intent(in) :: eos
    integer, intent(in) :: nc
    real, intent(in) :: T,V,n(nc)  !< [K], [m^3], [mol]
    integer, intent(in) :: i,j     !< component indices [-]
    real, intent(out) :: g         !< The rdf gij [-]
    real, intent(out), optional :: g_T,g_V,g_n(nc)
    real, intent(out), optional :: g_VV,g_TV,g_Vn(nc)
    real, intent(out), optional :: g_TT,g_Tn(nc),g_nn(nc,nc)
    ! Locals
    integer :: iTh
    if (eos%assoc%saft_model == eosBH_pert) then
      iTh = get_thread_index()
      call calc_gij_boublik(nc,T,V,n,i,j,saftvrmie_var(iTh),g,g_T,g_V,g_n,&
           g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)
    else if (eos%assoc%saft_model == cpaPR .or. eos%assoc%saft_model == cpaSRK) then
      call g_rdf_cpa(nc,V,n,g,g_V,g_n,g_VV,g_Vn,g_nn)
      if (present(g_T)) g_T = 0.0
      if (present(g_TT)) g_TT = 0.0
      if (present(g_TV)) g_TV = 0.0
      if (present(g_Tn)) g_Tn = 0.0
    else
      select type ( p_eos => eos )
      class is(PCSAFT_eos)
        call g_spc_saft_TVn(p_eos,T,V,n,g,g_T,g_V,g_n,g_TT,g_TV,g_Tn,g_VV,g_Vn,g_nn)
      class default
        call stoperror("master_saft_rdf: Wrong eos...")
      end select
    end if

  end subroutine master_saft_rdf

  !> Radial distribution function from hard-sphere fluid.
  subroutine g_rdf_cpa(nc,V,n,g,g_V,g_n,g_VV,g_Vn,g_nn)
    use saft_globals, only: assoc_covol
    ! Input.
    integer, intent(in) :: nc
    real, intent(in) :: V                       !< [m^3]
    real, intent(in) :: n(nc)                   !< [mol]
    ! Output.
    real, intent(out) :: g                      !< [-]
    real, intent(out), optional :: g_V
    real, intent(out), optional :: g_n(nc)
    real, intent(out), optional :: g_VV
    real, intent(out), optional :: g_Vn(nc)
    real, intent(out), optional :: g_nn(nc,nc)
    ! Locals.
    real :: eta
    real :: bigB, D, D2, D3, V_1, V_2, V_3
    real :: eta_V, eta_n(nc), eta_VV, eta_Vn(nc)
    real :: g_eta, g_etaeta
    integer :: i,j
    real :: sumn, b_mix
    real :: bi(nc) ! Cubic b parameters; fetched directly from global var cbeos.

    do i=1,nc
       bi(i) = assoc_covol(i)
    end do

    ! Compute the radial distribution function g.
    sumn = sum(n)
    b_mix = dot_product(n,bi)/sumn
    eta = sumn*b_mix/(4*V)
    if (useSimplifiedCPA) then ! Simplified CPA.
      g = 1/(1-1.9*eta)
    else           ! Original formulation.
      g = (1-eta/2)/(1-eta)**3
    end if

    if (present(g_V) .or. present(g_n) .or. present(g_VV) .or. present(g_Vn) .or. present(g_nn)) then
      ! Prepare for computing derivatives.
      bigB = sumn*b_mix
      D = 1/(1-0.475*bigB/V)
      D2 = D*D
      D3 = D2*D
      V_1 = 1/V
      V_2 = V_1*V_1
      V_3 = V_2*V_1

      eta_V = -bigB*V_2/4
      eta_n = bi*V_1/4
      eta_VV = bigB*V_3/2
      eta_Vn = -bi*V_2/4

      if (useSimplifiedCPA) then ! Simplified CPA.
        g_eta = 1.9/(1-1.9*eta)**2
        g_etaeta = 2*1.9*1.9/(1-1.9*eta)**3
      else           ! Original formulation.
        g_eta = (2.5-eta)/(1-eta)**4
        g_etaeta = (9-3*eta)/(1-eta)**5
      end if

      if (present(g_V)) then
        g_V = g_eta*eta_V
      end if
      if (present(g_n)) then
        g_n = g_eta*eta_n
      end if
      if (present(g_VV)) then
        g_VV = g_etaeta*eta_V*eta_V + g_eta*eta_VV
      end if
      if (present(g_Vn)) then
        g_Vn = g_etaeta*eta_V*eta_n + g_eta*eta_Vn
      end if
      if (present(g_nn)) then
        do i=1,nc
          do j=1,nc
            g_nn(i,j) = g_etaeta*eta_n(i)*eta_n(j)
          end do
        end do
      end if
    end if

  end subroutine g_rdf_cpa

end module saft_rdf
