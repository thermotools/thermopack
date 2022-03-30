module saft_globals
  use eosdata, only: cpaPR, cpaSRK, eosPC_SAFT, eosPeTS, eosSAFT_VR_MIE, &
       eosLJS_BH
  implicit none
  save
  public

contains

  function assoc_covol(ic)
    use pc_saft_nonassoc, only: PCSAFT_eos
    use saftvrmie_containers, only: saftvrmie_param
    use cubic_eos, only: cb_eos
    use thermopack_constants, only: N_AVOGADRO
    use thermopack_var, only: base_eos_param, get_active_eos
    real :: assoc_covol
    integer, intent(in) :: ic !< Component number
    ! Locals
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    if (eos%assoc%saft_model == eosSAFT_VR_MIE .or. &
         eos%assoc%saft_model == eosLJS_BH) then
      assoc_covol = N_AVOGADRO*saftvrmie_param%sigma_ij_cube(ic,ic)
    else
      select type ( p_eos => eos )
      class is ( cb_eos )
        assoc_covol = p_eos%single(ic)%b/1000
      class is ( PCSAFT_eos )
        assoc_covol = N_AVOGADRO*p_eos%sigma_cube(ic,ic)
      class default
        assoc_covol = 0
        print *,"Not able to calculate assoc_covol. Not cubic."
      end select
    end if
  end function assoc_covol

  function assoc_covol_binary(ic,jc)
    use pc_saft_nonassoc, only: PCSAFT_eos
    use saftvrmie_containers, only: saftvrmie_param
    use thermopack_constants, only: N_AVOGADRO
    use thermopack_var, only: base_eos_param, get_active_eos
    use cubic_eos, only: cb_eos
    real :: assoc_covol_binary
    integer, intent(in) :: ic, jc !< Component numbers
    ! Locals
    class(base_eos_param), pointer :: eos
    eos => get_active_eos()
    if (eos%assoc%saft_model == eosSAFT_VR_MIE) then
      assoc_covol_binary = N_AVOGADRO*saftvrmie_param%sigma_ij_cube(ic,jc)
    else
      select type ( p_eos => eos )
      class is ( cb_eos )
        assoc_covol_binary = 0.5*(p_eos%single(ic)%b + p_eos%single(jc)%b)/1000.0
      class is (PCSAFT_eos)
        assoc_covol_binary = N_AVOGADRO*p_eos%sigma_cube(ic,jc)
      class default
        assoc_covol_binary = 0
        print *,"Not able to calculate assoc_covol. Not cubic."
      end select
    end if
  end function assoc_covol_binary

end module saft_globals
