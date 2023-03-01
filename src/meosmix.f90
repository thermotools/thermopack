module meosmix
  !> Mixture of multiparamater EoSs
  use gergmix, only: meos_gergmix
  use thermopack_constants, only: N_Avogadro, VAPPH, LIQPH, Rgas_default
  use thermopack_var, only: Rgas, nce, complist, base_eos_param, &
       get_active_alt_eos
  use meosmixdb, only: max_meos_mix_reducing, meos_mix_reducingdb, &
       max_meos_mix_data, meos_mix_datadb
  use iso_fortran_env, only: dp => REAL64
  implicit none
  save
  private

  !> Multiparameter equations of state.
  type, extends(meos_gergmix) :: meos_mix

  contains
    procedure, public :: alpha0_hd
    procedure, public :: alphaRes_hd
    !
    procedure, public :: rgas_mix
    procedure, public :: calc_del_alpha_r

  end type meos_mix

  public :: meos_mix, constructor_meos

contains

  function constructor_meos(nc) result(mmix)
    use stringmod, only: str_eq
    use thermopack_var, only: get_active_thermo_model, thermo_model
    !character(len=*), intent(in) :: comp_name
    integer, intent(in) :: nc
    type(meos_mix) :: mmix
    ! Locals
    type(thermo_model), pointer :: p_thermo
    integer :: i, j, k
    real :: rhoc_i, rhoc_j, Tc_i, Tc_j

    call mmix%allocate_and_init(nc, "MEOS")
    call mmix%allocate_param(nc)

    mmix%mix_data_index = -1
    do i=1,nc
      rhoc_i = mmix%nist(i)%meos%rc
      Tc_i = mmix%nist(i)%meos%tc
      do j=i,nc
        rhoc_j = mmix%nist(j)%meos%rc
        Tc_j = mmix%nist(j)%meos%tc
        mmix%inv_rho_pow(i,j) = (1.0_dp/rhoc_i**(1.0_dp/3.0_dp) + 1.0_dp/rhoc_j**(1.0_dp/3.0_dp))**3
        mmix%tc_prod_sqrt(i,j) = (Tc_i*Tc_j)**(0.5_dp)
        !
        if (j > i) then
          do k=1,max_meos_mix_reducing
            if (str_eq(meos_mix_reducingdb(k)%ident1, complist(i)) .and. &
                 str_eq(meos_mix_reducingdb(k)%ident2, complist(j))) then
              mmix%beta_T(i,j) = meos_mix_reducingdb(k)%beta_T
              mmix%beta_v(i,j) = meos_mix_reducingdb(k)%beta_v
              mmix%gamma_T(i,j) = meos_mix_reducingdb(k)%gamma_T
              mmix%gamma_v(i,j) = meos_mix_reducingdb(k)%gamma_v
              exit
            else if (str_eq(meos_mix_reducingdb(k)%ident1, complist(j)) .and. &
                 str_eq(meos_mix_reducingdb(k)%ident2, complist(i))) then
              mmix%beta_T(i,j) = 1.0/meos_mix_reducingdb(k)%beta_T
              mmix%beta_v(i,j) = 1.0/meos_mix_reducingdb(k)%beta_v
              mmix%gamma_T(i,j) = meos_mix_reducingdb(k)%gamma_T
              mmix%gamma_v(i,j) = meos_mix_reducingdb(k)%gamma_v
              exit
            endif
          enddo
          do k=1,max_meos_mix_data
            if ((str_eq(meos_mix_datadb(k)%ident1, complist(i)) .and. &
                 str_eq(meos_mix_datadb(k)%ident2, complist(j))) .or. &
                 (str_eq(meos_mix_datadb(k)%ident1, complist(j)) .and. &
                 str_eq(meos_mix_datadb(k)%ident2, complist(i)))) then
              mmix%mix_data_index(i,j) = k
              exit
            endif
          enddo
        endif
      enddo
    enddo

    ! Set consistent Rgas
    p_thermo => get_active_thermo_model()
    if (nc == 1) then
      p_thermo%Rgas = mmix%nist(1)%meos%Rgas_meos
    else
      p_thermo%Rgas = Rgas_default
    endif
    p_thermo%kRgas = 1000.0*p_thermo%Rgas !< J/kmol/K
  end function constructor_meos

  ! The functional form of the ideal gas function varies among multiparameter EoS,
  ! which explains why this routine may seem a bit hard-coded.
  function rgas_mix(this, x) result(rmix)
    use hyperdual_mod
    class(meos_mix) :: this
    type(hyperdual), intent(in) :: x(nce)
    type(hyperdual) :: rmix
    ! Internals
    integer :: i
    rmix = 0.0_dp
    do i=1,nce
      rmix = rmix + x(i)*this%nist(i)%meos%Rgas_meos
    enddo
  end function rgas_mix

  ! The functional form of the ideal gas function varies among multiparameter EoS,
  ! which explains why this routine may seem a bit hard-coded.
  function alpha0_hd(this, x, rho, T) result(alp0)
    use hyperdual_mod
    class(meos_mix) :: this
    type(hyperdual), intent(in) :: rho, T, x(nce)
    type(hyperdual) :: alp0 !< alp0
    ! Internals
    !type(hyperdual) :: rmix
    !rmix = this%rgas_mix(x)
    !alp0 = rmix*this%meos_gergmix%alpha0_hd(x, rho, T)/Rgas
    alp0 = this%meos_gergmix%alpha0_hd(x, rho, T)
  end function alpha0_hd

  function alphaRes_hd(this, x, delta, tau) result(alpr)
    use hyperdual_mod
    class(meos_mix) :: this
    type(hyperdual), intent(in) :: delta, tau, x(nce)
    type(hyperdual) :: alpr !< alpr
    ! Internal
    type(hyperdual) :: rmix
    !rmix = this%rgas_mix(x)
    !print *,rmix%f0/Rgas-1.0_dp
    !stop
    !alpr = rmix*this%meos_gergmix%alphaRes_hd(x, delta, tau)/Rgas
    alpr = this%meos_gergmix%alphaRes_hd(x, delta, tau)
  end function alphaRes_hd

  function calc_del_alpha_r(this, x, tau, delta) result(del_alpha_r)
    use hyperdual_mod
    class(meos_mix) :: this
    type(hyperdual), intent(in) :: x(nce), tau, delta
    type(hyperdual) :: del_alpha_r
    ! Internals
    integer :: i, j, k, idb, n_poly, n_exp, n_gauss
    type(hyperdual) :: del_alpha_r_ij
    del_alpha_r = 0.0_dp

    do i=1,nce-1
      do j=i+1,nce
        del_alpha_r_ij = 0.0_dp
        if (this%mix_data_index(i,j) > 0) then
          idb = this%mix_data_index(i,j)
          n_gauss = meos_mix_datadb(idb)%num_mix
          n_exp = n_gauss - meos_mix_datadb(idb)%num_gauss
          n_poly = n_exp - meos_mix_datadb(idb)%num_exp
          !print *,meos_mix_datadb(idb)%num_mix,meos_mix_datadb(idb)%num_exp
          do k=1,n_poly
            del_alpha_r_ij = del_alpha_r_ij + meos_mix_datadb(idb)%n_mix(k)*delta**meos_mix_datadb(idb)%d_mix(k)*&
                 tau**meos_mix_datadb(idb)%t_mix(k)
            !print *,"k",k,meos_mix_datadb(idb)%n_mix(k),meos_mix_datadb(idb)%d_mix(k),meos_mix_datadb(idb)%t_mix(k)
          enddo
          do k=n_poly+1,n_exp
            del_alpha_r_ij = del_alpha_r_ij + meos_mix_datadb(idb)%n_mix(k)*delta**meos_mix_datadb(idb)%d_mix(k)*&
                 tau**meos_mix_datadb(idb)%t_mix(k)* &
                 exp(-delta**meos_mix_datadb(idb)%l_mix(k))
          enddo
          do k=n_exp+1,n_gauss
            del_alpha_r_ij = del_alpha_r_ij + meos_mix_datadb(idb)%n_mix(k)*delta**meos_mix_datadb(idb)%d_mix(k)*&
                 tau**meos_mix_datadb(idb)%t_mix(k)* &
                 exp(-meos_mix_datadb(idb)%eta_mix(k)*(delta-meos_mix_datadb(idb)%epsilon_mix(k))**2 &
                 - meos_mix_datadb(idb)%beta_mix(k)*(delta-meos_mix_datadb(idb)%gamma_mix(k)))
            !print *,"k",k,meos_mix_datadb(idb)%n_mix(k),meos_mix_datadb(idb)%d_mix(k),meos_mix_datadb(idb)%t_mix(k),&
            !     meos_mix_datadb(idb)%eta_mix(k),meos_mix_datadb(idb)%epsilon_mix(k),&
            !     meos_mix_datadb(idb)%beta_mix(k),meos_mix_datadb(idb)%gamma_mix(k)
          enddo
          !print *,"Fij",meos_mix_datadb(idb)%Fij
          !print *,"del_alpha_r_ij",del_alpha_r_ij%f0
          del_alpha_r = del_alpha_r + x(i)*x(j)*meos_mix_datadb(idb)%Fij*del_alpha_r_ij
        endif
      enddo
    enddo
  end function calc_del_alpha_r

end module meosmix
