subroutine set_verbose()

use parameters, only: verbose

verbose = .true.

end subroutine set_verbose

subroutine chemical_potential(nc, t, v, z, mu, dmudv, dmudt, dmudz)
  use eostv, only: eos => chemical_potential
  implicit none
  integer,                intent(in)  :: nc
  real,                   intent(in)  :: t !< K - Temperature
  real,                   intent(in)  :: v !< m3/mol - Molar volume
  real, dimension(nc),    intent(in)  :: z !< 1 - Composition
  real, dimension(nc),    intent(out) :: mu !< J/mol
  real, dimension(nc),    intent(out) :: dmudv !< J/m^3
  real, dimension(nc),    intent(out) :: dmudt !< J/mol K
  real, dimension(nc,nc), intent(out) :: dmudz !< J/mol^2)
  !
  call eos(t, v, z, mu, dmudv=dmudv, dmudt=dmudt, dmudz=dmudz)
  !
end subroutine chemical_potential

subroutine specific_internal_energy_Tv(nc,T,v,x,u,Cv,dudv)
  ! Get the specific internal energy (joule per mole) of a phase of composition
  ! x at conditions T,v.
  use eosTV, only: internal_energy
  ! Input:
  implicit none
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: v      !< Volume [m3/mol]
  real,     intent(in)      :: x(nc)  !< Molar composition [mol/mol]
  ! Output:
  real,     intent(out)     :: u      !< Specific internal energy [J/mol]
  real,     intent(out)     :: Cv     !< Isochoric heat capacity [J/mol/K]
  real,     intent(out)     :: dudv   !< Isochoric heat capacity [J/m3]

  call internal_energy(T,v,x,u,Cv,dudv)

end subroutine specific_internal_energy_Tv

subroutine specific_entropy_Tv(nc,t,v,n,s)
  ! Get entropy of a phase with mol numbers, n, at
  ! specified T and V.
  !
  use eosTV, only: entropyTV
  implicit none
  !
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: v      !< Specific volume [m3/mol]
  real,     intent(in)      :: n(nc)  !< Mol numbers [-]
  ! Output:
  real,     intent(out)     :: s        !< Entropy [J/(K)]

  call entropyTV(t,v,n,s)
end subroutine specific_entropy_Tv

subroutine enthalpy_Tv(nc,t,v,n,h)
  ! Get enthalpy at specified mol numbers, n, and
  ! specified T and V.
  !
  use eosTV, only: enthalpyTV
  implicit none
  !
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: v      !< Volume [m3]
  real,     intent(in)      :: n(nc)  !< Mol numbers [-]
  ! Output:
  real,     intent(out)     :: h        !< Enthalpy [J]

  call enthalpyTV(t,v,n,h)
end subroutine enthalpy_Tv

subroutine residual_specific_entropy_Tv(nc,t,v,n,s_r)
  ! Get specific entropy of a phase with mol numbers n at
  ! specified T and V.
  !
  use eosTV, only: entropyTV
  implicit none
  !
  integer,  intent(in)      :: nc     !< Number of components
  real,     intent(in)      :: T      !< Temperature [K]
  real,     intent(in)      :: v      !< Volume [m3]
  real,     intent(in)      :: n(nc)  !< Mol numbers [mol]
  ! Output:
  real,     intent(out)     :: s_r    !< Residual entropy [J/K]
  !
  call entropyTV(t,v,n,s_r,residual=.true.)
end subroutine residual_specific_entropy_Tv

subroutine pressure_v_no_derivs(nc, T, V, x, P)
  !Get the pressure at given volume from given EOS.
  !Gives wrong results inside phase envelope
  use eosTV, only : pressure
  implicit none

  integer, intent(in) :: nc !< Number of components
  real,    intent(in) :: T  !< Temperature(K)
  real,    intent(in) :: V  !< Volume (m³/mol)
  real,    intent(in) :: x(nc) !< Composition
  real,    intent(out):: P    !< Pressure (Pa)

  P = pressure(T,V,x)

end subroutine pressure_v_no_derivs


subroutine pressure_v(nc, T, V, x, P, dpdv, dpdt)
  !Get the pressure at given volume from given EOS.
  !Gives wrong results inside phase envelope
  use eosTV, only : pressure
  implicit none

  integer, intent(in) :: nc !< Number of components
  real,    intent(in) :: T  !< Temperature(K)
  real,    intent(in) :: V  !< Volume (m³/mol)
  real,    intent(in) :: x(nc) !< Composition
  real,    intent(out):: P    !< Pressure (Pa)
  real,    intent(out):: dpdv !< dp/dV_T (Pa*mol/m³)
  real,    intent(out):: dpdt !< dp/dT_V (Pa/K)

  P = pressure(T,V,x, dpdv, dpdt)

end subroutine pressure_v

subroutine lnphi_tv(nc, T, v, z, lnphi)
  !> Calculate lnphi given composition, temperature and specific volume
  use eosTV , only: thermoTV
  integer, intent(in) :: nc
  real, intent(in)                                  :: t !< K - Temperature
  real, intent(in)                                  :: v !< m3/mol - Volume
  real, dimension(1:nc), intent(in)                 :: z !< Compozition
  real, dimension(1:nc), intent(out)                :: lnphi !< Logarithm of fugacity coeff.

  call thermoTV(T, v, z, lnphi)

end subroutine lnphi_tv

subroutine lnphi_tv_derivs(nc, T, v, z, lnphi, lnphit, lnphiv, lnphin)
  !> Calculate lnphi given composition, temperature and specific volume
  use eosTV , only: thermoTV
  integer, intent(in) :: nc
  real, intent(in)                                  :: t !< K - Temperature
  real, intent(in)                                  :: v !< m3/mol - Volume
  real, dimension(1:nc), intent(in)                 :: z !< Compozition
  real, dimension(1:nc), intent(out) :: lnphi !< Logarithm of fugacity coeff.
  real, dimension(1:nc), intent(out) :: lnphit !< 1/K - dlnphi/dt
  real, dimension(1:nc), intent(out) :: lnphiv !< mol/m3 - dlnphi/dv
  real, dimension(1:nc,1:nc), intent(out) :: lnphin !< (dlnphi_i/dn_j)_ij

  call thermoTV(T, v, z, lnphi, lnphit, lnphiv, lnphin)

end subroutine lnphi_tv_derivs

subroutine helmholtz_energy(nc, T, v, z, a)
  !> Calculate total molar helmholtz energy.
  use eosTV , only: Fres
  use ideal, only: TV_Yideal_mix
  use tpvar, only: comp
  use tpconst, only: RGas
  integer, intent(in)                               :: nc
  real, intent(in)                                  :: t !< K - Temperature
  real, intent(in)                                  :: v !< m3/mol - Molar volume
  real, dimension(1:nc), intent(in)                 :: z !< - Composition
  real, intent(out)                                 :: a !< J/mol
  real :: f, Y_ideal_mix

  call Fres(t,v,z,F=f)
  call TV_Yideal_mix(nc, comp, T, v*1e3, z, Y_ideal_mix)

  a = Rgas*T*f + y_ideal_mix/1e3

end subroutine helmholtz_energy

subroutine chempottvz(nc, T, v, z, mu, mu_v, mu_z)
  !> Calculate total chemical potential and its volume derivative.
  use eosTV , only: Fres, Fideal
  use tpvar, only: comp
  use tpconst, only: RGas
  use parameters, only: EoSlib, TREND
  integer, intent(in)                               :: nc
  real, intent(in)                                  :: t !< K - Temperature
  real, intent(in)                                  :: v !< m3/mol - Molar volume
  real, dimension(1:nc), intent(in)                 :: z !< - Composition
  real, dimension(1:nc), intent(out)                :: mu !< J/mol
  real, dimension(1:nc), intent(out)                :: mu_v !< J/m^3
  real, dimension(1:nc,1:nc), intent(out)           :: mu_z !< J/mol^2
  real :: f, f_n(nc), f_vn(nc), f_nn(nc,nc)
  real :: fid, fid_n(nc), fid_vn(nc)

  call Fres(t,v*1e3,z,F=f, F_n=f_n, F_VN=f_vn, F_NN=f_nn)

  if (EoSlib == TREND) then
    mu = Rgas*T*(f_n-log(v)) ! (wrong T-dependence in this ideal mu; usually inconsequential)
  else
    call Fideal(t,v*1e3,z,F=f, F_n=fid_n)
    mu = Rgas*T*(f_n+fid_n)
  end if

  mu_v = Rgas*T*(f_vn*1e3 - 1.0/v)
  mu_z = f_nn
  do i=1,nc
    mu_z(i,i) = mu_z(i,i) + 1.0/z(i)
  end do
  mu_z = mu_z*Rgas*T

end subroutine chempottvz

! If t_in<0, it is estimated. Same for v_in.
subroutine calculatecriticaltvp(nc,t_in,v_in,z,t_c,v_c,p_c,ierr)
  use critical, only: calcCriticalTV
  use eosTV, only : pressure
  implicit none
  ! Input
  integer, intent(in) :: nc !< Number of components
  real, intent(in) :: Z(nc) !< Trial composition (Overall compozition)
  real, intent(in) :: t_in !< Temperature guess [K]
  real, intent(in) :: v_in !< Volume guess [m3/mol]
  ! Output
  real, intent(out) :: t_c !< Critical temperature [K]
  real, intent(out) :: v_c !< Critical volume [m3/mol]
  real, intent(out) :: p_c !< Critical pressure [Pa]
  integer, intent(out) :: ierr !< Error flag
  !real, intent(in) :: tol !< Tolerance (absolute tolerance on solver)
  ! Locals
  real :: t_loc, v_loc

  t_loc = t_in
  v_loc = v_in
  call calcCriticalTV(t_loc,v_loc,Z,ierr)!,tol)

  t_c = t_loc
  v_c = v_loc
  p_c = pressure(t=t_c,v=v_c,n=z)

end subroutine calculatecriticaltvp

! Calculate Helmholtz free energy
subroutine helmholtz_1stderivs(nc,t,v,z,A,A_T,A_V)
  use eosTV, only : free_energy
  implicit none
  ! Input
  integer, intent(in) :: nc !< Number of components
  real, intent(in) :: Z(nc) !< Phase compozition
  real, intent(in) :: t !< Temperature  [K]
  real, intent(in) :: v !< Volume [m3/mol]
  ! Output
  real, intent(out) :: A !< Helmholtz energy [J]
  real, intent(out) :: A_T !< Helmholtz energy [J/K]
  real, intent(out) :: A_V !< Helmholtz energy [J mol/m3]

  call free_energy(t,v,z,A,A_T,A_V)
end subroutine helmholtz_1stderivs


! Calculate Helmholtz free energy
subroutine helmholtz_free_energy(nc,t,v,z,A,A_T,A_V,A_TT,A_VV,A_VT)
  use eosTV, only : free_energy
  implicit none
  ! Input
  integer, intent(in) :: nc !< Number of components
  real, intent(in) :: Z(nc) !< Phase compozition
  real, intent(in) :: t !< Temperature  [K]
  real, intent(in) :: v !< Volume [m3/mol]
  ! Output
  real, intent(out) :: A !< Helmholtz energy [J]
  real, intent(out) :: A_T !< Helmholtz energy [J/K]
  real, intent(out) :: A_V !< Helmholtz energy [J mol/m3]
  real, intent(out) :: A_TT !< J/(mol K2) - Helmholtz differential wrpt. temperature
  real, intent(out) :: A_VV !< J/m6 - Helmholtz second differential wrpt. specific volume
  real, intent(out) :: A_VT !< J/(m3 K) - Helmholtz second differential wrpt. specific volume and temperature

  ! Locals

  call free_energy(t,v,z,A,A_T,A_V,A_TT,A_VV,A_VT)

end subroutine helmholtz_free_energy

! Calculate residual reduced Helmholtz free energy
subroutine residual_reduced_helmholtz(nc,T,V,n,F,F_T,F_V,F_TT,F_VV,F_VT,F_n,F_Vn,F_Tn,F_nn)
  use eosTV, only : Fres
  implicit none
  ! Input
  integer, intent(in) :: nc !< Number of components
  real, intent(in) :: n(nc) !< Phase mole numbers
  real, intent(in) :: T !< Temperature  [K]
  real, intent(in) :: V !< Volume [m3]
  ! Output
  real, intent(out) :: F !< Residual reduced (RR) Helmholtz energy [-]
  real, intent(out) :: F_T !< RR Helmholtz energy temperature differential [1/K]
  real, intent(out) :: F_V !< RR Helmholtz energy volume differential [mol/m3]
  real, intent(out) :: F_TT !< 1/(mol K2) - Helmholtz differential wrpt. temperature
  real, intent(out) :: F_VV !< 1/m6 - Helmholtz second differential wrpt. specific volume
  real, intent(out) :: F_VT !< 1/(m3 K) - Helmholtz second differential wrpt. specific volume and temperature
  real, intent(out) :: F_n(nc) !< 1/(mol K2) - RR Helmholtz differential wrt. mole numbers
  real, intent(out) :: F_Vn(nc) !< 1/m6 - RR Helmholtz second differential wrt. specific volume and mole numbers
  real, intent(out) :: F_Tn(nc) !< 1/(m3 K) - RR Helmholtz second differential wrt. tempresture and mole numbers
  real, intent(out) :: F_nn(nc,nc) !< 1/(mol K2) - RR Helmholtz second differential wrt. mole numbers
  call Fres(T,V,n,F,F_T,F_V,F_n,F_TT,F_VT,F_VV,F_Tn,F_Vn,F_nn)
end subroutine residual_reduced_helmholtz

! Calculate reduced ideal Helmholtz free energy
subroutine ideal_reduced_helmholtz(nc,T,V,n,F,F_T,F_V,F_TT,F_VV,F_VT,F_n,F_Vn,F_Tn,F_nn)
  use eosTV, only : Fideal
  implicit none
  ! Input
  integer, intent(in) :: nc !< Number of components
  real, intent(in) :: n(nc) !< Phase mole numbers
  real, intent(in) :: T !< Temperature  [K]
  real, intent(in) :: V !< Volume [m3]
  ! Output
  real, intent(out) :: F !< Ideal reduced (IR) Helmholtz energy [-]
  real, intent(out) :: F_T !< IR Helmholtz energy temperature differential [1/K]
  real, intent(out) :: F_V !< IR Helmholtz energy volume differential [mol/m3]
  real, intent(out) :: F_TT !< 1/(mol K2) - IR Helmholtz second differential wrt. temperature
  real, intent(out) :: F_VV !< 1/m6 - IR Helmholtz second differential wrt. specific volume
  real, intent(out) :: F_VT !< 1/(m3 K) - IR Helmholtz second differential wrt. specific volume and temperature
  real, intent(out) :: F_n(nc) !< 1/(mol K2) - IR Helmholtz differential wrt. mole numbers
  real, intent(out) :: F_Vn(nc) !< 1/m6 - IR Helmholtz second differential wrt. specific volume and mole numbers
  real, intent(out) :: F_Tn(nc) !< 1/(m3 K) - IR Helmholtz second differential wrt. tempresture and mole numbers
  real, intent(out) :: F_nn(nc,nc) !< 1/(mol K2) - IR Helmholtz second differential wrt. mole numbers
  call Fideal(T,V,n,F,F_T,F_V,F_n,F_TT,F_VT,F_VV,F_Tn,F_Vn,F_nn)
end subroutine ideal_reduced_helmholtz
