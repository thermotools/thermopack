module saft_globals
  use eosdata, only: cpaPR, cpaSRK, eosPC_SAFT, eosPeTS, eosBH_pert, eosSAFT_VR_MIE
  implicit none
  save
  public

  !> Active SAFT model. Set to the correct EoS index stored in module eosdata.
  integer :: saft_model
contains

  function assoc_covol(ic)
    use pc_saft_nonassoc, only: sigma_cube
    use saftvrmie_containers, only: saftvrmie_param
    use cubic_eos, only: cb_eos
    use thermopack_constants, only: N_AVOGADRO
    use thermopack_var, only: get_active_eos
    real :: assoc_covol
    integer, intent(in) :: ic !< Component number
    ! Locals

    if (saft_model == eosPC_SAFT) then
      assoc_covol = N_AVOGADRO*sigma_cube(ic,ic)
    else if (saft_model == eosBH_pert) then
      assoc_covol = N_AVOGADRO*saftvrmie_param%sigma_ij_cube(ic,ic)
    else
      select type ( p_eos => get_active_eos() )
      class is ( cb_eos )
        assoc_covol = p_eos%single(ic)%b/1000
      class default
        assoc_covol = 0
        print *,"Not able to calculate assoc_covol. Not cubic."
      end select
    end if
  end function assoc_covol

  function assoc_covol_binary(ic,jc)
    use pc_saft_nonassoc, only: sigma_cube
    use saftvrmie_containers, only: saftvrmie_param
    use thermopack_constants, only: N_AVOGADRO
    use thermopack_var, only: get_active_eos
    use cubic_eos, only: cb_eos
    real :: assoc_covol_binary
    integer, intent(in) :: ic, jc !< Component numbers
    ! Locals
    if (saft_model == eosPC_SAFT) then
      assoc_covol_binary = N_AVOGADRO*sigma_cube(ic,jc)
    else if (saft_model == eosBH_pert) then
      assoc_covol_binary = N_AVOGADRO*saftvrmie_param%sigma_ij_cube(ic,jc)
    else
      select type ( p_eos => get_active_eos() )
      class is ( cb_eos )
        assoc_covol_binary = 0.5*(p_eos%single(ic)%b + p_eos%single(jc)%b)/1000.0
      class default
        assoc_covol_binary = 0
        print *,"Not able to calculate assoc_covol. Not cubic."
      end select
    end if
  end function assoc_covol_binary

end module saft_globals
