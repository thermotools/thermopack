!> Tuning of HV data
subroutine thermopack_getHVparam(i,j,alpha_ij,alpha_ji,aGE_ij,aGE_ji,bGE_ij,bGE_ji,cGE_ij,cGE_ji)
  use tpvar
  implicit none
  integer, intent(in) :: i,j
  real, intent(out) :: alpha_ij,alpha_ji,aGE_ij,aGE_ji,bGE_ij,bGE_ji,cGE_ij,cGE_ji
  !
  if (.not. allocated(cbeos)) then
    call stoperror('cbeos not allocated')
  endif
  if (.not. allocated(cbeos(1)%mixGE%alpha)) then
    call stoperror('cbeos(1)%mixGEalpha not allocated')
  endif
  if (.not. allocated(cbeos(1)%mixGE%aGE)) then
    call stoperror('cbeos(1)%mixGE%aGE not allocated')
  endif
  if (.not. allocated(cbeos(1)%mixGE%bGE)) then
    call stoperror('cbeos(1)%mixGE%bGE not allocated')
  endif
  if (.not. allocated(cbeos(1)%mixGE%cGE)) then
    call stoperror('cbeos(1)%mixGE%cGE not allocated')
  endif
  alpha_ij = cbeos(1)%mixGE%alpha(i,j)
  alpha_ji = cbeos(1)%mixGE%alpha(j,i)
  aGE_ij = cbeos(1)%mixGE%aGE(i,j)
  aGE_ji = cbeos(1)%mixGE%aGE(j,i)
  bGE_ij = cbeos(1)%mixGE%bGE(i,j)
  bGE_ji = cbeos(1)%mixGE%bGE(j,i)
  cGE_ij = cbeos(1)%mixGE%cGE(i,j)
  cGE_ji = cbeos(1)%mixGE%cGE(j,i)
end subroutine thermopack_getHVparam

!> Tuning of HV data
subroutine thermopack_setHVparam(i,j,alpha_ij,alpha_ji,aGE_ij,aGE_ji,bGE_ij,bGE_ji,cGE_ij,cGE_ji)
  use tpvar
  implicit none
  integer, intent(in) :: i,j
  real, intent(in) :: alpha_ij,alpha_ji,aGE_ij,aGE_ji,bGE_ij,bGE_ji,cGE_ij,cGE_ji
  !
  if (.not. allocated(cbeos)) then
    call stoperror('cbeos not allocated')
  endif
  if (.not. allocated(cbeos(1)%mixGE%alpha)) then
    call stoperror('cbeos(1)%mixGE%alpha not allocated')
  endif
  if (.not. allocated(cbeos(1)%mixGE%aGE)) then
    call stoperror('cbeos(1)%mixGE%aGE not allocated')
  endif
  if (.not. allocated(cbeos(1)%mixGE%bGE)) then
    call stoperror('cbeos(1)%mixGE%bGE not allocated')
  endif
  if (.not. allocated(cbeos(1)%mixGE%cGE)) then
    call stoperror('cbeos(1)%mixGE%cGE not allocated')
  endif
  cbeos(1)%mixGE%alpha(i,j) = alpha_ij
  cbeos(1)%mixGE%alpha(j,i) = alpha_ji
  cbeos(1)%mixGE%aGE(i,j) = aGE_ij
  cbeos(1)%mixGE%aGE(j,i) = aGE_ji
  cbeos(1)%mixGE%bGE(i,j) = bGE_ij
  cbeos(1)%mixGE%bGE(j,i) = bGE_ji
  cbeos(1)%mixGE%cGE(i,j) = cGE_ij
  cbeos(1)%mixGE%cGE(j,i) = cGE_ji
  cbeos(1)%mixGE%correlation(i,j) = 1
  cbeos(1)%mixGE%correlation(j,i) = 1
end subroutine thermopack_setHVparam

!> Tuning of TWU alpha correlation
subroutine thermopack_getTWUparam(i,c_1,c_2,c_3)
  use tpvar
  implicit none
  integer, intent(in) :: i
  real, intent(out) :: c_1,c_2,c_3
  !
  c_1 = cbeos(1)%single(i)%alphaParams(1)
  c_2 = cbeos(1)%single(i)%alphaParams(2)
  c_3 = cbeos(1)%single(i)%alphaParams(3)
end subroutine thermopack_getTWUparam

!> Tuning of TWU alpha correlation. If eoslibinit wasn't initilized with
!> TWU, TWU becomes the new alpha correlation.
subroutine thermopack_setTWUparam(i,c_1,c_2,c_3)
  use cbAlpha, only: setSingleAlphaCorr
  use tpvar, only: cbeos
  implicit none
  integer, intent(in) :: i
  real, intent(in) :: c_1,c_2,c_3
  !
  call setSingleAlphaCorr(i, cbeos(1), corrName="TWU", alphaParams=(/c_1,c_2,c_3/))
end subroutine thermopack_setTWUparam

!> Retrieve TWU parameters
subroutine thermopack_getMCparam(i,c_1,c_2,c_3)
  use tpvar
  implicit none
  integer, intent(in) :: i
  real, intent(out) :: c_1,c_2,c_3
  !
  c_1 = cbeos(1)%single(i)%alphaParams(1)
  c_2 = cbeos(1)%single(i)%alphaParams(2)
  c_3 = cbeos(1)%single(i)%alphaParams(3)
end subroutine thermopack_getMCparam

!> Tuning of MC alpha correlation. If eoslibinit wasn't initilized with
!> MC, MC becomes the new alpha correlation.
subroutine thermopack_setMCparam(i,c_1,c_2,c_3)
  use cbAlpha, only: setSingleAlphaCorr
  use tpvar, only: cbeos
  implicit none
  integer, intent(in) :: i
  real, intent(in) :: c_1,c_2,c_3
  !
  call setSingleAlphaCorr(i, cbeos(1), corrName="MC", alphaParams=(/c_1,c_2,c_3/))
end subroutine thermopack_setMCparam

subroutine thermopack_setClassicFitparam(i,c_1)
  use cbAlpha, only: setSingleAlphaCorr
  use tpvar, only: cbeos
  implicit none
  integer, intent(in) :: i
  real, intent(in) :: c_1
  !
  call setSingleAlphaCorr(i, cbeos(1), corrName="CLASSIC", alphaParams=(/c_1/))
end subroutine thermopack_setClassicFitparam

!> Tuning of vdW kij interaction parameters
subroutine thermopack_getkij(i,j,kij)
  use tpvar
  implicit none
  integer, intent(in) :: i,j
  real, intent(out) :: kij
  !
  kij = cbeos(1)%kij(i,j)
end subroutine thermopack_getkij

!> Tuning of vdW kij interaction parameters
subroutine thermopack_setkij(i,j,kij)
  use tpvar
  implicit none
  integer, intent(in) :: i,j
  real, intent(in) :: kij
  !
  cbeos(1)%kij(i,j) = kij
end subroutine thermopack_setkij

!> Tuning of kij interaction parameters
subroutine thermopack_setkijandji(i,j,kij)
  use tpvar, only: cbeos
  use eosdata, only: cbSRK, cbSRKGB, cbPR, cbVdW, cbRK, &
       cbSW, cbPT, eosLK, cspSRK, cspSRKGB, cspPR, &
       cpaSRK, cpaPR, eosPC_SAFT, eos_single, eosSAFT_VR_MIE, &
       eosBH_pert
  use saftvrmie_containers, only: set_saftvrmie_eps_kij
  use saft_interface, only: pc_saft_set_kij
  implicit none
  integer, intent(in) :: i,j
  real, intent(in) :: kij
  !
  select case (cbeos(1)%eosidx)
  case(cbSRK, cbSRKGB, cbPR, cbVdW, cbRK, cbSW, cbPT, eosLK, cspSRK, cspSRKGB, cspPR)
    cbeos(1)%kij(i,j) = kij
    cbeos(1)%kij(j,i) = kij
  case(cpaSRK, cpaPR)
    call stoperror("Not able to set binary kij for CPA eos.")
  case(eosPC_SAFT)
    call pc_saft_set_kij(i,j,kij)
  case(eos_single)
    call stoperror("Not able to set binary kij for single comp. eos.")
  case(eosBH_pert)
    if (cbeos(1)%subeosidx == eosSAFT_VR_MIE) then
      call set_saftvrmie_eps_kij(i,j,kij)
    endif
  case default
    call stoperror("Not able to set binary kij for selected model")
  end select

end subroutine thermopack_setkijandji

!> Tuning of lij interaction parameters
subroutine thermopack_setlijandji(i,j,lij)
  use tpvar, only: cbeos
  use eosdata, only: cbSRK, cbSRKGB, cbPR, cbVdW, cbRK, &
       cbSW, cbPT, eosLK, cspSRK, cspSRKGB, cspPR, &
       cpaSRK, cpaPR, eosPC_SAFT, eos_single, eosSAFT_VR_MIE, &
       eosBH_pert
  use saftvrmie_containers, only: set_saftvrmie_sigma_lij
  implicit none
  integer, intent(in) :: i,j
  real, intent(in) :: lij
  !
  select case (cbeos(1)%eosidx)
  case(cbSRK, cbSRKGB, cbPR, cbVdW, cbRK, cbSW, cbPT, eosLK, cspSRK, cspSRKGB, cspPR)
    call stoperror("Not able to set binary lij for Cubic eos.")
  case(cpaSRK, cpaPR)
    call stoperror("Not able to set binary lij for CPA eos.")
  case(eosPC_SAFT)
    call stoperror("Not able to set binary lij for PC-SAFT eos.")
  case(eos_single)
    call stoperror("Not able to set binary lij for single comp. eos.")
  case(eosBH_pert)
    if (cbeos(1)%subeosidx == eosSAFT_VR_MIE) then
      call set_saftvrmie_sigma_lij(i,j,lij)
    endif
  case default
    call stoperror("Not able to set binary lij for selected model")
  end select

end subroutine thermopack_setlijandji



