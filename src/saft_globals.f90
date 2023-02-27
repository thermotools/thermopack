module saft_globals
  use eosdata, only: eosSAFT_VR_MIE, eosLJS_BH
  implicit none
  save
  public

contains

  function assoc_covol(ic)
    use pc_saft_nonassoc, only: sPCSAFT_eos
    use cubic_eos, only: cb_eos
    use thermopack_constants, only: N_AVOGADRO
    use thermopack_var, only: base_eos_param, get_active_eos
    real :: assoc_covol
    integer, intent(in) :: ic !< Component number
    ! Locals
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    if (eos%assoc%saft_model == eosSAFT_VR_MIE) then
      assoc_covol = N_AVOGADRO
    else
      select type ( p_eos => eos )
      class is ( cb_eos )
        assoc_covol = p_eos%single(ic)%b/1000
      class is ( sPCSAFT_eos )
        assoc_covol = N_AVOGADRO*p_eos%sigma_cube(ic,ic)
      class default
        assoc_covol = 0
        print *,"Not able to calculate assoc_covol. Not cubic."
      end select
    end if
  end function assoc_covol

  subroutine assoc_covol_binary(ic,jc, covol, covol_T, covol_TT)
    use pc_saft_nonassoc, only: sPCSAFT_eos, PCSAFT_eos
    use thermopack_constants, only: N_AVOGADRO
    use thermopack_var, only: base_eos_param, get_active_eos
    use cubic_eos, only: cb_eos
    integer, intent(in) :: ic, jc !< Component numbers
    real, intent(out) :: covol, covol_T, covol_TT
    ! Locals
    class(base_eos_param), pointer :: eos
    covol_T = 0
    covol_TT = 0
    eos => get_active_eos()
    if (eos%assoc%saft_model == eosSAFT_VR_MIE) then
      covol = N_AVOGADRO
    else
      select type ( p_eos => eos )
      class is ( cb_eos )
        covol = 0.5*(p_eos%single(ic)%b + p_eos%single(jc)%b)/1000.0
      class is (PCSAFT_eos)
        covol = N_AVOGADRO*(0.5*(p_eos%dhs%d(ic) + p_eos%dhs%d(jc)))**3
        covol_T = 3*N_AVOGADRO*0.5**3*((p_eos%dhs%d(ic) + p_eos%dhs%d(jc)))**2*&
             (p_eos%dhs%d_T(ic) + p_eos%dhs%d_T(jc))
        covol_TT = N_AVOGADRO*0.5**3 *(6*((p_eos%dhs%d(ic) + p_eos%dhs%d(jc)))*&
             (p_eos%dhs%d_T(ic) + p_eos%dhs%d_T(jc))**2 + &
             3*((p_eos%dhs%d(ic) + p_eos%dhs%d(jc)))**2*&
             (p_eos%dhs%d_TT(ic) + p_eos%dhs%d_TT(jc)))
      class is (sPCSAFT_eos)
        covol = N_AVOGADRO*p_eos%sigma_cube(ic,jc)
      class default
        covol = 0
        print *,"Not able to calculate assoc_covol. Not cubic."
      end select
    end if
  end subroutine assoc_covol_binary

end module saft_globals
