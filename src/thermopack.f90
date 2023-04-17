program run_thermopack
  ! Program to call the thermopack code
  ! Morten Hammer, 2020-05.
  !----------------------------------------------------------------------
  use hyperdual_mod
  use pair_potentials, only: calc_bh_diameter
  use uv_theory!, only: calcFres_uv, uv_theory_eos, dhs_BH_Mie, preCalcUVTheory
  use eoslibinit, only: init_thermo, init_saftvrmie, init_uv, init_ljs
  use single_phase, only: TV_CalcFres
  use thermopack_constants
  use critical, only: calcCriticalTV
  use eosTV, only: pressure, free_energy_tv
  use saft_interface, only: calcSaftFder_res_nonassoc
  use thermopack_var, only: nc, nce, get_active_eos, base_eos_param, &
       thermo_model, get_active_thermo_model, apparent_to_real_mole_numbers
  use saftvrmie_containers, only: saftvrmie_eos
  use saftvrmie_hardsphere, only: calc_hardsphere_helmholtzenergy
  use saftvrmie_dispersion, only: calcA1, calcA2, calcA3
  use saftvrmie_interface, only: preCalcSAFTVRMie
  implicit none
  real, dimension(1) :: z
  real :: P, T, V
  integer :: ierr
  real :: F, y
  real :: a0, a1, a2, a3
  real :: epsdivk, sigma, lamr, dhs
  type(hyperdual) :: T_hd, rho_hd, z_hd(1), dhs_hd, delta_b2u, Delta_B2, delta_a0, a_hs_res
  type(hyperdual) :: delta_a1u, delta_a2u, delta_a3u
  class(base_eos_param), pointer :: act_eos_ptr
  type(thermo_model), pointer :: act_mod_ptr

  ! Thermodynamic state
  T = 100
  V = 1.0e-3
  z = (/1.0/)

  print *, "SAFTVRMIE"
  call init_saftvrmie("AR","DEFAULT")
  act_eos_ptr => get_active_eos()
  select type(p_eos => act_eos_ptr)
  type is (saftvrmie_eos)
     call preCalcSAFTVRMie(nc,T,V,z,3,p_eos%saftvrmie_var)
     call calc_hardsphere_helmholtzenergy(nc,T,V,z,s_vc=p_eos%saftvrmie_var,a=a0)
     call calcA1(nc,T,V,z, saftvrmie_vc=p_eos%saftvrmie_var,a1=a1)
     call calcA2(nc,T,V,z, saftvrmie_vc=p_eos%saftvrmie_var,a2=a2)
     call calcA3(nc,T,V,z, s_vc=p_eos%saftvrmie_var,a3=a3)
     dhs = p_eos%saftvrmie_var%dhs%d(1,1)
     sigma = p_eos%saftvrmie_param%sigma_ij(1,1)
     epsdivk = p_eos%saftvrmie_param%eps_divk_ij(1,1)
     lamr = p_eos%saftvrmie_param%lambda_r_ij(1,1)

     ! I think these are correct, but you may want to double-check
     ! with saftvrmie_dispersion code
     print *, "Khs", p_eos%saftvrmie_var%Khs%zx
     print *, "chi", a2/p_eos%saftvrmie_var%a2chij%am(1,1)-1

  end select
  print *, "dhs      ", dhs
  print *, "a0       ", a0
  print *, "a1       ", a1
  print *, "a2       ", a2
  print *, "a3       ", a3
  print *, ""

  print *, "UV-THEORY"
  call init_uv(comps="AR",model="uv-mie-bh",parameter_reference="SVRMIE")
  act_eos_ptr => get_active_eos()
  select type(p_eos => act_eos_ptr)
  type is (uv_theory_eos)
     epsdivk = p_eos%mie(1,1)%epsdivk%f0
     sigma = p_eos%mie(1,1)%sigma%f0
     lamr = p_eos%mie(1,1)%lamr%f0
     T_hd = T
     z_hd = z
     rho_hd = z_hd(1)/V*N_AVOGADRO
     LAFITTE = .true.
     print *, "LAFITTE"
     call preCalcUVTheory(p_eos, nc, T_hd, z_hd)
     call DeltaB2_quadrature(p_eos,nc,T_hd,z_hd, Delta_B2)
     call calc_ares_hardsphere_bmcsl(nc, rho_hd*z_hd, p_eos%dhs(1,1), a_hs_res)
     call delta_a1u_b2u_lafitte(p_eos, nc, T_hd, rho_hd, z_hd, Delta_a1u, Delta_B2u)
     print *, "ahs      ", a_hs_res%f0
     print *, "a0       ", delta_a0%f0 + a_hs_res%f0
     print *, "a1       ", Delta_a1u%f0*T

     Delta_a2u = 0.0
     Delta_a3u = 0.0
     ! TODO for Tage: implement these routines.
     call delta_a2u_lafitte(p_eos, nc, T_hd, rho_hd, z_hd, Delta_a2u)
     call delta_a3u_lafitte(p_eos, nc, T_hd, rho_hd, z_hd, Delta_a3u)
     print *, "a2       ", Delta_a2u%f0*T**2
     print *, "a3       ", Delta_a3u%f0*T**3
     print *, ""


     ! LAFITTE = .False.
     ! print *, "VANWESTEN"
     ! call preCalcUVTheory(p_eos, nc, T_hd, z_hd)
     ! call calc_ares_hardsphere_bmcsl(nc, rho_hd*z_hd, p_eos%dhs(1,1), a_hs_res)
     ! call Delta_a0_Mie(p_eos, nc, T_hd, rho_hd, z_hd, delta_a0)
     ! call delta_a1u_b2u_Mie(p_eos, nc, T_hd, rho_hd, z_hd, Delta_a1u, Delta_B2u)
     ! call DeltaB2_Mie(p_eos,nc,T_hd,z_hd, Delta_B2)
     ! print *, "ahs      ", a_hs_res%f0
     ! print *, "Deltaa0  ", delta_a0%f0
     ! print *, "a0       ", delta_a0%f0 + a_hs_res%f0
     ! print *, "a1       ", Delta_a1u%f0*T
     ! print *, "DeltaB2  ", Delta_B2%f0
     ! print *, ""
     

  end select
end program run_thermopack
