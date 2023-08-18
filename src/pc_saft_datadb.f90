!> Automatically generated to file pc_saft_datadb.f90
!! using utility python code pyUtils
!! Time stamp: 2023-08-18T11:18:19.140638

module pc_saft_datadb
  use thermopack_constants, only: uid_len, ref_len, bibref_len
  use AssocSchemeUtils
  use eosdata, only: eosPC_SAFT, eosOPC_SAFT, eosSPC_SAFT
  implicit none
  public

  !> PURE COMPONENT PARAMETERS.
  !> This data structure stores pure component parameters for the
  !> PC-SAFT equation of state.
  ! ---------------------------------------------------------------------------
  type :: pc_saft_data
    integer :: eosidx
    character(len=uid_len) :: compName
    ! Pure component fitted parameters.
    real :: m        !< [-]. Mean number of segments.
    real :: sigma    !< [m]. Temperature-independent segment diameter.
    real :: eps_depth_divk !< [K]. Well depth divided by Boltzmann's c.
    ! Association parameters.
    real :: eps  !< [J/mol].
    real :: beta !< [-]. Also known as kappa in SAFT literature.
    integer :: assoc_scheme !< Association scheme.
    ! Electical moment parameters.
    real :: mu  !< Dipole-moment [D]
    real :: Q !< Quadrupol-moment [ÅD]
    ! Bibliograpic reference
    character(len=bibref_len) :: bib_ref
    ! Parameter set
    character(len=ref_len) :: ref
  end type pc_saft_data

  !> TEMPERATURE-INDEPENDENT INTERACTION PARAMETERS FOR PC-SAFT DISPERSION TERM.
  ! ----------------------------------------------------------------------------
  type :: PCkijdata
    integer:: eosidx
    character(len=uid_len) :: uid1, uid2
    character(len=ref_len) :: ref ! Parameter set
    character(len=bibref_len) :: bib_ref ! Bibliographic reference
    real :: kijvalue
  end type PCkijdata

  type(pc_saft_data), parameter :: PCcx1 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "BUT1OL", &
      m = 2.9614, &
      sigma = 3.5065e-10, &
      eps_depth_divk = 253.29, &
      eps = 21625.917269816575, &
      beta = 0.003874630939, &
      assoc_scheme = assoc_scheme_2C, &
      mu = 0., &
      Q = 0., &
      bib_ref = "de Villiers et al. (2011). Doi: 10.1021/ie200521k", &
      ref = "Default/deVilliers2011" &
      )

  type(pc_saft_data), parameter :: PCcx2 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "BUT1OL", &
      m = 2.7515, &
      sigma = 3.6189e-10, &
      eps_depth_divk = 259.59, &
      eps = 21156.981578152732, &
      beta = 0.006692, &
      assoc_scheme = assoc_scheme_2B, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      ref = "Tang_Gross2010" &
      )

  type(pc_saft_data), parameter :: PCcx3 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "HEX1OL", &
      m = 3.1542, &
      sigma = 3.8188e-10, &
      eps_depth_divk = 277.24, &
      eps = 24495.238319341257, &
      beta = 0.001308996939, &
      assoc_scheme = assoc_scheme_2C, &
      mu = 0., &
      Q = 0., &
      bib_ref = "de Villiers et al. (2011). Doi: 10.1021/ie200521k", &
      ref = "Default/deVilliers2011" &
      )

  type(pc_saft_data), parameter :: PCcx4 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "HEX1OL", &
      m = 3.5146, &
      sigma = 3.6735e-10, &
      eps_depth_divk = 262.32, &
      eps = 21109.589141229262, &
      beta = 0.005747, &
      assoc_scheme = assoc_scheme_2B, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      ref = "Tang_Gross2010" &
      )

  type(pc_saft_data), parameter :: PCcx5 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "PENT1OL", &
      m = 3.1488, &
      sigma = 3.635e-10, &
      eps_depth_divk = 261.96, &
      eps = 21246.2789066717, &
      beta = 0.003926990817, &
      assoc_scheme = assoc_scheme_2C, &
      mu = 0., &
      Q = 0., &
      bib_ref = "de Villiers et al. (2011). Doi: 10.1021/ie200521k", &
      ref = "Default/deVilliers2011" &
      )

  type(pc_saft_data), parameter :: PCcx6 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "PENT1OL", &
      m = 3.626, &
      sigma = 3.4508e-10, &
      eps_depth_divk = 247.28, &
      eps = 18725.00126234291, &
      beta = 0.010319, &
      assoc_scheme = assoc_scheme_2B, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      ref = "Tang_Gross2010" &
      )

  type(pc_saft_data), parameter :: PCcx7 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "PROP1OL", &
      m = 2.9537, &
      sigma = 3.2473e-10, &
      eps_depth_divk = 226.36, &
      eps = 20353.970778491494, &
      beta = 0.011938052084, &
      assoc_scheme = assoc_scheme_2C, &
      mu = 0., &
      Q = 0., &
      bib_ref = "de Villiers et al. (2011). Doi: 10.1021/ie200521k", &
      ref = "Default/deVilliers2011" &
      )

  type(pc_saft_data), parameter :: PCcx8 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "PROP1OL", &
      m = 2.9997, &
      sigma = 3.2522e-10, &
      eps_depth_divk = 233.4, &
      eps = 18930.368489011296, &
      beta = 0.015268, &
      assoc_scheme = assoc_scheme_2B, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      ref = "Tang_Gross2010" &
      )

  type(pc_saft_data), parameter :: PCcx9 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "ACETONE", &
      m = 2.7447, &
      sigma = 3.2742e-10, &
      eps_depth_divk = 232.99, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Gross and Vrabec 2006. doi: 10.1002/aic.10683", &
      ref = "Gross2006" &
      )

  type(pc_saft_data), parameter :: PCcx10 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "ACETYLENE", &
      m = 1.5477, &
      sigma = 3.3428e-10, &
      eps_depth_divk = 174.48, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Gross 2005, doi: 10.1002/aic.10502", &
      ref = "Gross2005" &
      )

  type(pc_saft_data), parameter :: PCcx11 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "ACETYLENE", &
      m = 1.5587, &
      sigma = 3.3325e-10, &
      eps_depth_divk = 174.68, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 4.5415, &
      bib_ref = "Gross 2005, doi: 10.1002/aic.10502", &
      ref = "Gross2005ADJQ" &
      )

  type(pc_saft_data), parameter :: PCcx12 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NH3", &
      m = 2.25807, &
      sigma = 2.37802e-10, &
      eps_depth_divk = 126.868, &
      eps = 11032.6, &
      beta = 0.20479, &
      assoc_scheme = assoc_scheme_3B, &
      mu = 0., &
      Q = 0., &
      bib_ref = "SINTEF Energy Research", &
      ref = "Default/InHouse" &
      )

  type(pc_saft_data), parameter :: PCcx13 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NH3", &
      m = 1.4302, &
      sigma = 2.7927e-10, &
      eps_depth_divk = 145.0059, &
      eps = 13303.140189, &
      beta = 0.221193, &
      assoc_scheme = assoc_scheme_2B, &
      mu = 0., &
      Q = 0., &
      bib_ref = "10.1016/j.fluid.2020.112689", &
      ref = "NguyenHuynh2020" &
      )

  type(pc_saft_data), parameter :: PCcx14 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "AR", &
      m = 0.9285, &
      sigma = 3.4784e-10, &
      eps_depth_divk = 122.23, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx15 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "BENZENE", &
      m = 2.4653, &
      sigma = 3.6478e-10, &
      eps_depth_divk = 287.35, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx16 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "BENZENE", &
      m = 2.2463, &
      sigma = 3.7852e-10, &
      eps_depth_divk = 296.24, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 5.5907, &
      bib_ref = "Gross 2005, doi: 10.1002/aic.10502", &
      ref = "Gross2005ADJQ" &
      )

  type(pc_saft_data), parameter :: PCcx17 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "BUTANAL", &
      m = 2.8825, &
      sigma = 3.4698e-10, &
      eps_depth_divk = 247.09, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Gross and Vrabec (2006). Doi: 10.1002/aic.10683", &
      ref = "Default/Gross2006" &
      )

  type(pc_saft_data), parameter :: PCcx18 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "CO2", &
      m = 2.0729, &
      sigma = 2.7852e-10, &
      eps_depth_divk = 169.21, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx19 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "CO2", &
      m = 2.58239, &
      sigma = 2.56106e-10, &
      eps_depth_divk = 151.691, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      ref = "Tang_Gross2010" &
      )

  type(pc_saft_data), parameter :: PCcx20 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "CO2", &
      m = 1.72897, &
      sigma = 2.76102e-10, &
      eps_depth_divk = 83.7215, &
      eps = 9256.5, &
      beta = 0.171605, &
      assoc_scheme = assoc_scheme_3B, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Smith (2017) MSc thesis, Doi: 10019.1/101071", &
      ref = "Smith2017" &
      )

  type(pc_saft_data), parameter :: PCcx21 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "CO2", &
      m = 1.5131, &
      sigma = 3.1869e-10, &
      eps_depth_divk = 163.33, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Gross 2005, doi: 10.1002/aic.10502", &
      ref = "Gross2005" &
      )

  type(pc_saft_data), parameter :: PCcx22 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "CO2", &
      m = 1.6298, &
      sigma = 3.0867e-10, &
      eps_depth_divk = 163.34, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 3.9546, &
      bib_ref = "Gross 2005, doi: 10.1002/aic.10502", &
      ref = "Gross2005ADJQ" &
      )

  type(pc_saft_data), parameter :: PCcx23 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "CL2", &
      m = 1.3934, &
      sigma = 3.5339e-10, &
      eps_depth_divk = 270.49, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Gross (2005): doi: 10.1002/aic.10502", &
      ref = "Default/Gross2005" &
      )

  type(pc_saft_data), parameter :: PCcx24 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "CL2", &
      m = 1.4682, &
      sigma = 3.448e-10, &
      eps_depth_divk = 269.67, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 3.0724, &
      bib_ref = "Gross (2005): doi: 10.1002/aic.10502", &
      ref = "Gross2005ADJQ" &
      )

  type(pc_saft_data), parameter :: PCcx25 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "CYCLOHEX", &
      m = 2.5303, &
      sigma = 3.8499e-10, &
      eps_depth_divk = 278.11, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx26 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "DME", &
      m = 2.2634, &
      sigma = 3.2723e-10, &
      eps_depth_divk = 210.29, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Gross and Vrabeck (2006). Doi: 10.1002/aic.10683", &
      ref = "Default/Gross2006" &
      )

  type(pc_saft_data), parameter :: PCcx27 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "C2", &
      m = 1.6069, &
      sigma = 3.5206e-10, &
      eps_depth_divk = 191.42, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx28 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "ETOH", &
      m = 2.3609, &
      sigma = 3.1895e-10, &
      eps_depth_divk = 207.56, &
      eps = 22413.213735129506, &
      beta = 0.017121679962, &
      assoc_scheme = assoc_scheme_2C, &
      mu = 0., &
      Q = 0., &
      bib_ref = "de Villiers et al. (2011). Doi: 10.1021/ie200521k", &
      ref = "Default/deVilliers2011" &
      )

  type(pc_saft_data), parameter :: PCcx29 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "ETOH", &
      m = 1.2309, &
      sigma = 4.1057e-10, &
      eps_depth_divk = 316.91, &
      eps = 23372.12070888112, &
      beta = 0.00331438025, &
      assoc_scheme = assoc_scheme_2B, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      ref = "Tang_Gross2010" &
      )

  type(pc_saft_data), parameter :: PCcx30 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "ETOH", &
      m = 2.3827, &
      sigma = 3.1771e-10, &
      eps_depth_divk = 198.24, &
      eps = 22061.595111007806, &
      beta = 0.032384, &
      assoc_scheme = assoc_scheme_2B, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Gross & Sadowski (2002). Doi: 10.1021/ie010954d", &
      ref = "Gross_Sadowski2002" &
      )

  type(pc_saft_data), parameter :: PCcx31 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "C2_1", &
      m = 1.5425, &
      sigma = 3.4523e-10, &
      eps_depth_divk = 179.37, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Gross (2005), doi: 10.1002/aic.10502", &
      ref = "Gross2005" &
      )

  type(pc_saft_data), parameter :: PCcx32 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "C2_1", &
      m = 1.5477, &
      sigma = 3.4475e-10, &
      eps_depth_divk = 179.19, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 1.9155, &
      bib_ref = "Gross (2005), doi: 10.1002/aic.10502", &
      ref = "Gross2005ADJQ" &
      )

  type(pc_saft_data), parameter :: PCcx33 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "H2S", &
      m = 1.6941, &
      sigma = 3.0214e-10, &
      eps_depth_divk = 226.79, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx34 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "H2S", &
      m = 1.355, &
      sigma = 3.309e-10, &
      eps_depth_divk = 234.25, &
      eps = 6491.932412254049, &
      beta = 0.001, &
      assoc_scheme = assoc_scheme_2B, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      ref = "Default/Tang_Gross2010" &
      )

  type(pc_saft_data), parameter :: PCcx35 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "IC4", &
      m = 2.2616, &
      sigma = 3.7574e-10, &
      eps_depth_divk = 216.53, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx36 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "IC5", &
      m = 2.562, &
      sigma = 3.8296e-10, &
      eps_depth_divk = 230.75, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx37 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "C1", &
      m = 1., &
      sigma = 3.7039e-10, &
      eps_depth_divk = 150.03, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx38 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "MEOH", &
      m = 2.1, &
      sigma = 2.7998e-10, &
      eps_depth_divk = 197.23, &
      eps = 21077.16273701846, &
      beta = 0.430921792317, &
      assoc_scheme = assoc_scheme_2C, &
      mu = 0., &
      Q = 0., &
      bib_ref = "de Villiers et al. (2011). Doi: 10.1021/ie200521k", &
      ref = "Default/deVilliers2011" &
      )

  type(pc_saft_data), parameter :: PCcx39 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "N2", &
      m = 1.2053, &
      sigma = 3.313e-10, &
      eps_depth_divk = 90.96, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx40 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "N2", &
      m = 1.1504, &
      sigma = 3.3848e-10, &
      eps_depth_divk = 91.4, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Gross 2005, doi: 10.1002/aic.10502", &
      ref = "Gross2005" &
      )

  type(pc_saft_data), parameter :: PCcx41 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "N2", &
      m = 1.1879, &
      sigma = 3.3353e-10, &
      eps_depth_divk = 90.99, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 1.1151, &
      bib_ref = "Gross 2005, doi: 10.1002/aic.10502", &
      ref = "Gross2005ADJQ" &
      )

  type(pc_saft_data), parameter :: PCcx42 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "O2", &
      m = 1.1217, &
      sigma = 3.2098e-10, &
      eps_depth_divk = 114.96, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx43 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "C3", &
      m = 2.002, &
      sigma = 3.6184e-10, &
      eps_depth_divk = 208.11, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx44 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "TOLU", &
      m = 2.8149, &
      sigma = 3.7169e-10, &
      eps_depth_divk = 285.69, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx45 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "H2O", &
      m = 1.5, &
      sigma = 2.6273e-10, &
      eps_depth_divk = 180.3, &
      eps = 15001.119744924437, &
      beta = 0.0942, &
      assoc_scheme = assoc_scheme_4C, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Grenner et al. (2006). Doi: 10.1021/ie0605332", &
      ref = "Default/Grenner2006" &
      )

  type(pc_saft_data), parameter :: PCcx46 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "H2O", &
      m = 1.0656, &
      sigma = 3.0007e-10, &
      eps_depth_divk = 366.51, &
      eps = 20791.976669215805, &
      beta = 0.034868, &
      assoc_scheme = assoc_scheme_2B, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx47 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "H2O", &
      m = 1.18381, &
      sigma = 2.87756e-10, &
      eps_depth_divk = 201.82186, &
      eps = 15074.120726711822, &
      beta = 0.07002, &
      assoc_scheme = assoc_scheme_4C, &
      mu = 0., &
      Q = 0., &
      bib_ref = "10.1016/j.fluid.2018.06.019", &
      ref = "NguyenHuynh2020" &
      )

  type(pc_saft_data), parameter :: PCcx48 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC4", &
      m = 2.3316, &
      sigma = 3.7086e-10, &
      eps_depth_divk = 222.88, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx49 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC10", &
      m = 4.6627, &
      sigma = 3.8384e-10, &
      eps_depth_divk = 243.87, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx50 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC22", &
      m = 8.7068, &
      sigma = 3.982e-10, &
      eps_depth_divk = 253.955, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx51 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC12", &
      m = 5.2133, &
      sigma = 3.9115e-10, &
      eps_depth_divk = 248.0042, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx52 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC20", &
      m = 8.0081, &
      sigma = 3.973e-10, &
      eps_depth_divk = 253.1802, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx53 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC21", &
      m = 8.3574, &
      sigma = 3.9777e-10, &
      eps_depth_divk = 253.5838, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx54 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC17", &
      m = 6.96, &
      sigma = 3.9559e-10, &
      eps_depth_divk = 251.7263, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx55 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC7", &
      m = 3.4831, &
      sigma = 3.8049e-10, &
      eps_depth_divk = 238.4, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx56 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC16", &
      m = 6.6107, &
      sigma = 3.949e-10, &
      eps_depth_divk = 251.1392, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx57 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC6", &
      m = 3.0576, &
      sigma = 3.7983e-10, &
      eps_depth_divk = 236.77, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx58 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC19", &
      m = 7.6587, &
      sigma = 3.9678e-10, &
      eps_depth_divk = 252.7398, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx59 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC9", &
      m = 4.2079, &
      sigma = 3.8448e-10, &
      eps_depth_divk = 244.51, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx60 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC18", &
      m = 7.3094, &
      sigma = 3.9622e-10, &
      eps_depth_divk = 252.2573, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx61 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC8", &
      m = 3.8176, &
      sigma = 3.8373e-10, &
      eps_depth_divk = 242.78, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx62 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC25", &
      m = 9.7548, &
      sigma = 3.9931e-10, &
      eps_depth_divk = 254.9091, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx63 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC15", &
      m = 6.2614, &
      sigma = 3.9412e-10, &
      eps_depth_divk = 250.4867, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx64 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC5", &
      m = 2.6896, &
      sigma = 3.7729e-10, &
      eps_depth_divk = 231.2, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx65 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC14", &
      m = 5.912, &
      sigma = 3.9326e-10, &
      eps_depth_divk = 249.757, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx66 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC24", &
      m = 9.4055, &
      sigma = 3.9897e-10, &
      eps_depth_divk = 254.6147, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx67 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC23", &
      m = 9.0561, &
      sigma = 3.986e-10, &
      eps_depth_divk = 254.2975, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx68 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC13", &
      m = 5.5627, &
      sigma = 3.9227e-10, &
      eps_depth_divk = 248.9356, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  type(pc_saft_data), parameter :: PCcx69 = &
      pc_saft_data(eosidx = eosPC_SAFT, &
      compName = "NC11", &
      m = 4.864, &
      sigma = 3.8986e-10, &
      eps_depth_divk = 246.939, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      mu = 0., &
      Q = 0., &
      bib_ref = "Kontogeorgis & Folas (2010). Doi: 10.1002/9780470747537", &
      ref = "Default/Kontogeorgis_Folas2001" &
      )

  integer, parameter :: nPCmodels = 69
  type(pc_saft_data), dimension(nPCmodels), parameter :: PCarray = (/&
      PCcx1,PCcx2,PCcx3,PCcx4,PCcx5, &
      PCcx6,PCcx7,PCcx8,PCcx9,PCcx10, &
      PCcx11,PCcx12,PCcx13,PCcx14,PCcx15, &
      PCcx16,PCcx17,PCcx18,PCcx19,PCcx20, &
      PCcx21,PCcx22,PCcx23,PCcx24,PCcx25, &
      PCcx26,PCcx27,PCcx28,PCcx29,PCcx30, &
      PCcx31,PCcx32,PCcx33,PCcx34,PCcx35, &
      PCcx36,PCcx37,PCcx38,PCcx39,PCcx40, &
      PCcx41,PCcx42,PCcx43,PCcx44,PCcx45, &
      PCcx46,PCcx47,PCcx48,PCcx49,PCcx50, &
      PCcx51,PCcx52,PCcx53,PCcx54,PCcx55, &
      PCcx56,PCcx57,PCcx58,PCcx59,PCcx60, &
      PCcx61,PCcx62,PCcx63,PCcx64,PCcx65, &
      PCcx66,PCcx67,PCcx68,PCcx69 &
  /)

  type (PCkijdata), parameter :: PCSAFT_KIJ_1 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "C1", &
      kijvalue = 0.0425  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_2 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "C2", &
      kijvalue = 0.072  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_3 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "C3", &
      kijvalue = 0.069  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_4 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "NC4", &
      kijvalue = 0.067  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_5 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "NC5", &
      kijvalue = 0.073  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_6 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "NC6", &
      kijvalue = 0.073  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_7 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "NC7", &
      kijvalue = 0.078  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_8 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "NC9", &
      kijvalue = 0.086  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_9 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "NC10", &
      kijvalue = 0.077  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_10 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "IC4", &
      kijvalue = 0.06  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_11 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "IC5", &
      kijvalue = 0.076  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_12 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "CYCLOHEX", &
      kijvalue = 0.082  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_13 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "H2S", &
      uid2 = "NC8", &
      kijvalue = 0.  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_14 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "H2S", &
      kijvalue = 0.0223  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_15 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "BENZENE", &
      kijvalue = 0.025  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_16 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "TOLU", &
      kijvalue = 0.026  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_17 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Gross_Sadowski2001", &
      bib_ref = "Gross & Sadowski (2001). Doi: 10.1021/ie0003887", &
      uid1 = "CO2", &
      uid2 = "C1", &
      kijvalue = 0.065  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_18 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "C2", &
      kijvalue = 0.102  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_19 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "C3", &
      kijvalue = 0.0107  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_20 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "NC4", &
      kijvalue = 0.109  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_21 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "NC5", &
      kijvalue = 0.12  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_22 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "NC6", &
      kijvalue = 0.123  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_23 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "NC7", &
      kijvalue = 0.115  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_24 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "NC8", &
      kijvalue = 0.132  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_25 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "NC9", &
      kijvalue = 0.122  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_26 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "NC10", &
      kijvalue = 0.133  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_27 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "IC4", &
      kijvalue = 0.112  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_28 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "IC5", &
      kijvalue = 0.116  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_29 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "CYCLOHEX", &
      kijvalue = 0.125  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_30 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "BENZENE", &
      kijvalue = 0.087  &
      )

  type (PCkijdata), parameter :: PCSAFT_KIJ_31 = &
      PCkijdata(eosidx = eosPC_SAFT, &
      ref = "Default/Tang_Gross2010", &
      bib_ref = "Tang & Gross (2010). Doi: 10.1016/j.fluid.2010.02.004", &
      uid1 = "CO2", &
      uid2 = "TOLU", &
      kijvalue = 0.108  &
      )

  integer, parameter :: PCmaxkij = 31
  type (PCkijdata), dimension(PCmaxkij), parameter :: PCkijdb = (/&
      PCSAFT_KIJ_1,PCSAFT_KIJ_2,PCSAFT_KIJ_3,PCSAFT_KIJ_4,PCSAFT_KIJ_5, &
      PCSAFT_KIJ_6,PCSAFT_KIJ_7,PCSAFT_KIJ_8,PCSAFT_KIJ_9,PCSAFT_KIJ_10, &
      PCSAFT_KIJ_11,PCSAFT_KIJ_12,PCSAFT_KIJ_13,PCSAFT_KIJ_14,PCSAFT_KIJ_15, &
      PCSAFT_KIJ_16,PCSAFT_KIJ_17,PCSAFT_KIJ_18,PCSAFT_KIJ_19,PCSAFT_KIJ_20, &
      PCSAFT_KIJ_21,PCSAFT_KIJ_22,PCSAFT_KIJ_23,PCSAFT_KIJ_24,PCSAFT_KIJ_25, &
      PCSAFT_KIJ_26,PCSAFT_KIJ_27,PCSAFT_KIJ_28,PCSAFT_KIJ_29,PCSAFT_KIJ_30, &
      PCSAFT_KIJ_31 &
  /)

end module pc_saft_datadb
