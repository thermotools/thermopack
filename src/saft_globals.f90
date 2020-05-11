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
    use tpconst, only: N_AVOGADRO
    use tpvar, only: cbeos
    real :: assoc_covol
    integer, intent(in) :: ic !< Component number
    if (saft_model == eosPC_SAFT) then
       assoc_covol = N_AVOGADRO*sigma_cube(ic,ic)
    else if (saft_model == eosBH_pert) then
       assoc_covol = N_AVOGADRO*saftvrmie_param%sigma_ij_cube(ic,ic)
    else
       assoc_covol = cbeos(1)%single(ic)%b/1000
    end if
  end function assoc_covol

  function assoc_covol_binary(ic,jc)
    use pc_saft_nonassoc, only: sigma_cube
    use saftvrmie_containers, only: saftvrmie_param
    use tpconst, only: N_AVOGADRO
    use tpvar, only: cbeos
    real :: assoc_covol_binary
    integer, intent(in) :: ic, jc !< Component numbers
    if (saft_model == eosPC_SAFT) then
       assoc_covol_binary = N_AVOGADRO*sigma_cube(ic,jc)
    else if (saft_model == eosBH_pert) then
       assoc_covol_binary = N_AVOGADRO*saftvrmie_param%sigma_ij_cube(ic,jc)
    else
       assoc_covol_binary = 0.5*(cbeos(1)%single(ic)%b + cbeos(1)%single(jc)%b)/1000.0
    end if
  end function assoc_covol_binary

end module saft_globals
