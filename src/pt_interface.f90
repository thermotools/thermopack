!---------------------------------------------------------------------
! Module and subroutines for the Quatum-SAFT-VR-Mie (QSAFT-VR-MIE)
! Equation of State implmented in Thermopack.
! Programmed by: M. Hammer, A. Aasen and Mr. Wilhelmsen
! Spring 2018, Imperial College London, UK
! © SINTEF Energy Research. All rights reserved.
!---------------------------------------------------------------------

module bh_interface
  use eosdata, only: eosSAFT_VR_MIE
  implicit none
  private
  save
  integer :: bh_model = - 1

  public :: init_BH_pert_model, calcFresBH
  public :: calc_BH_zeta

contains

  !> Initialize the Barker-Henderson perturbation model
  !!
  !! \author Morten Hammer, March 2019
  subroutine init_BH_pert_model(nc,comp,cbeos,setno,mixing)
    use saftvrmie_interface, only: init_saftvrmie
    use compdata, only: gendata
    use eosdata, only: eoscubic
    use saftvrmie_interface, only: init_saftvrmie
    integer, intent(in)           :: nc          !< Number of components.
    type(gendata), intent(inout)  :: comp(nc)    !< Component vector.
    type(eoscubic), intent(inout) :: cbeos       !< Underlying cubic equation of state.
    integer, intent(in), optional :: setno(nc)   !< Parameter sets to use for components
    integer, intent(in), optional :: mixing      !< Binary combination rule id
    !
    bh_model = cbeos%subeosidx
    if (cbeos%subeosidx == eosSAFT_VR_MIE) then
      call init_saftvrmie(nc,comp,cbeos,setno,mixing)
    endif
  end subroutine init_BH_pert_model

  !> Calculate hypotetical pure fluid packing fraction
  !!
  !! \author Morten Hammer, March 2019
  function calc_BH_zeta(nc,T,V,n) result(zeta)
    use saftvrmie_interface, only: calc_saftvrmie_zeta
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real :: zeta
    !
    if (bh_model == eosSAFT_VR_MIE) then
      zeta = calc_saftvrmie_zeta(nc,T,V,n)
    endif

  end function calc_BH_zeta

  !> Calculate residual reduced Helmholts free energy
  !!
  !! \author Morten Hammer, March 2019
  subroutine calcFresBH(nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
       F_VV,F_TV,F_Tn,F_Vn,F_nn)
    use saftvrmie_interface, only: calcFresSAFTVRMie
    ! Input
    integer, intent(in) :: nc !< Number of components
    real, intent(in) :: T !< Temperature [K]
    real, intent(in) :: V !< Volume [m3]
    real, intent(in) :: n(nc) !< Mol numbers [mol]
    ! Output
    real, intent(out) :: F
    real, optional, intent(out) :: F_T,F_V,F_TT,F_VV,F_TV
    real, optional, dimension(nc), intent(out) :: F_n,F_Tn,F_Vn
    real, optional, dimension(nc,nc), intent(out) :: F_nn
    !
    if (bh_model == eosSAFT_VR_MIE) then
      call calcFresSAFTVRMie(nc,T,V,n,F,F_T,F_V,F_n,F_TT,&
           F_VV,F_TV,F_Tn,F_Vn,F_nn)
    endif
  end subroutine calcFresBH

end module bh_interface
