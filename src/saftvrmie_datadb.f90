!> Automatically generated to file saftvrmie_datadb.f90
!! using utility python code pyUtils
!! Time stamp: 2022-05-15T19:17:32.065393

module saftvrmie_datadb
  use thermopack_constants, only: uid_len, ref_len, bibref_len
  use AssocSchemeUtils
  use eosdata, only: eosSAFT_VR_MIE
  implicit none
  public

  !> PURE COMPONENT PARAMETERS.
  !> This data structure stores pure component parameters for the
  !> SAFT-VRQ Mie EoS
  ! ---------------------------------------------------------------------------
  type :: saftvrmie_data
    integer :: eosidx
    character (len=uid_len) :: compName
    ! Pure component fitted parameters.
    real :: m        !< [-]. Mean number of segments.
    real :: sigma    !< [m]. Temperature-independent segment diameter.
    real :: eps_depth_divk !< [K]. Well depth divided by Boltzmann's c.
    real :: lambda_a !< [] attractive exponent of the Mie potential
    real :: lambda_r !< [] repulsive exponent of the Mie potential
    real :: mass     !< Segment mass, i.e. molecule mass dividided by number of segments [kg]
    ! Association parameters.
    real :: eps  !< [J/mol].
    real :: beta !< [-]. Also known as kappa in SAFT literature.
    !real :: rc  !< [m]. range of association

    ! Association scheme.
    integer :: assoc_scheme
    ! Order of Feynman--Hibbs correction (nonzero only for quantum fluids).
    integer :: fh_order
    ! Bibliograpic reference
    character(len=bibref_len) :: bib_ref
    ! Parameter set
    character(len=ref_len) :: ref
  end type saftvrmie_data

  !> INTERACTION PARAMETERS FOR THE SAFT-VR-MIE DISPERSION TERM.
  ! ----------------------------------------------------------------------------
  type :: Miekijdata
    integer:: eosidx
    character (len=uid_len) :: uid1, uid2
    character(len=ref_len) :: ref ! Parameter set
    character(len=bibref_len) :: bib_ref ! Bibliographic reference
    real :: kijvalue
  end type Miekijdata

  type(saftvrmie_data), parameter :: Miecx1 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "AR", &
      m = 1., &
      sigma = 3.41e-10, &
      eps_depth_divk = 118.7, &
      lambda_a = 6., &
      lambda_r = 12.26, &
      mass = 6.6335e-26, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Unpublished (as of 08/2018) parameters from G. Jackon's group", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx2 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "CO2", &
      m = 1.5, &
      sigma = 3.1916e-10, &
      eps_depth_divk = 231.88, &
      lambda_a = 5.1646, &
      lambda_r = 27.557, &
      mass = 0.e+00, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx3 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "D2", &
      m = 1., &
      sigma = 3.1538e-10, &
      eps_depth_divk = 21.2, &
      lambda_a = 6., &
      lambda_r = 8., &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "DEFAULT/AASEN2019-FH0" &
      )

  type(saftvrmie_data), parameter :: Miecx4 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "D2", &
      m = 1., &
      sigma = 3.0203e-10, &
      eps_depth_divk = 30.273, &
      lambda_a = 6., &
      lambda_r = 10., &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH1" &
      )

  type(saftvrmie_data), parameter :: Miecx5 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "D2", &
      m = 1., &
      sigma = 2.9897e-10, &
      eps_depth_divk = 36.913, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH2/AASEN2019-FH2-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx6 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "D2", &
      m = 1., &
      sigma = 3.1561e-10, &
      eps_depth_divk = 28.222, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH0-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx7 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "D2", &
      m = 1., &
      sigma = 3.0193e-10, &
      eps_depth_divk = 34.389, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH1-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx8 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "C2", &
      m = 1.4373, &
      sigma = 3.7257e-10, &
      eps_depth_divk = 206.12, &
      lambda_a = 6., &
      lambda_r = 12.4, &
      mass = 0.e+00, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx9 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "HE", &
      m = 1., &
      sigma = 3.353e-10, &
      eps_depth_divk = 4.44, &
      lambda_a = 6., &
      lambda_r = 14.84, &
      mass = 6.6464764e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Herdes 2015, doi: 10.1016/j.fluid.2015.07.014", &
      ref = "DEFAULT/AASEN2019-FH0" &
      )

  type(saftvrmie_data), parameter :: Miecx10 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "HE", &
      m = 1., &
      sigma = 2.7443e-10, &
      eps_depth_divk = 5.4195, &
      lambda_a = 6., &
      lambda_r = 9., &
      mass = 6.6464764e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH1" &
      )

  type(saftvrmie_data), parameter :: Miecx11 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "HE", &
      m = 1., &
      sigma = 2.549e-10, &
      eps_depth_divk = 10.952, &
      lambda_a = 6., &
      lambda_r = 13., &
      mass = 6.6464764e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH2" &
      )

  type(saftvrmie_data), parameter :: Miecx12 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "HE", &
      m = 1., &
      sigma = 3.1273e-10, &
      eps_depth_divk = 4.1463, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 6.6464764e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH0-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx13 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "HE", &
      m = 1., &
      sigma = 2.8107e-10, &
      eps_depth_divk = 6.6893, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 6.6464764e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH1-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx14 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "HE", &
      m = 1., &
      sigma = 2.5442e-10, &
      eps_depth_divk = 10.393, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 6.6464764e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH2-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx15 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "HE", &
      m = 1., &
      sigma = 2.8525e-10, &
      eps_depth_divk = 4.5764, &
      lambda_a = 6., &
      lambda_r = 8.3471, &
      mass = 6.6464764e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "HYVA parameters with non-integer repulsive exponent", &
      ref = "HYVA-FH0" &
      )

  type(saftvrmie_data), parameter :: Miecx16 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "HE", &
      m = 1., &
      sigma = 2.6615e-10, &
      eps_depth_divk = 7.9191, &
      lambda_a = 6., &
      lambda_r = 10.455, &
      mass = 6.6464764e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "HYVA parameters with non-integer repulsive exponent", &
      ref = "HYVA-FH1" &
      )

  type(saftvrmie_data), parameter :: Miecx17 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "HE", &
      m = 1., &
      sigma = 2.5682e-10, &
      eps_depth_divk = 11.942, &
      lambda_a = 6., &
      lambda_r = 13.239, &
      mass = 6.6464764e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "HYVA parameters with non-integer repulsive exponent", &
      ref = "HYVA-FH2" &
      )

  type(saftvrmie_data), parameter :: Miecx18 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "H2", &
      m = 1., &
      sigma = 3.2574e-10, &
      eps_depth_divk = 17.931, &
      lambda_a = 6., &
      lambda_r = 8., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "DEFAULT/AASEN2019-FH0" &
      )

  type(saftvrmie_data), parameter :: Miecx19 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "H2", &
      m = 1., &
      sigma = 3.0243e-10, &
      eps_depth_divk = 26.706, &
      lambda_a = 6., &
      lambda_r = 9., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH1" &
      )

  type(saftvrmie_data), parameter :: Miecx20 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "H2", &
      m = 1., &
      sigma = 2.9195e-10, &
      eps_depth_divk = 55.729, &
      lambda_a = 6., &
      lambda_r = 20., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH2" &
      )

  type(saftvrmie_data), parameter :: Miecx21 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "H2", &
      m = 1., &
      sigma = 3.5333e-10, &
      eps_depth_divk = 14.312, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH0-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx22 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "H2", &
      m = 1., &
      sigma = 3.0225e-10, &
      eps_depth_divk = 33.096, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH1-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx23 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "H2", &
      m = 1., &
      sigma = 2.9324e-10, &
      eps_depth_divk = 40.321, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH2-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx24 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "H2", &
      m = 1., &
      sigma = 3.198e-10, &
      eps_depth_divk = 14.492, &
      lambda_a = 6., &
      lambda_r = 6.4709, &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "HYVA parameters with non-integer repulsive exponent", &
      ref = "HYVA-FH0" &
      )

  type(saftvrmie_data), parameter :: Miecx25 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "H2", &
      m = 1., &
      sigma = 3.0022e-10, &
      eps_depth_divk = 28.349, &
      lambda_a = 6., &
      lambda_r = 9.5413, &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "HYVA parameters with non-integer repulsive exponent", &
      ref = "HYVA-FH1" &
      )

  type(saftvrmie_data), parameter :: Miecx26 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "H2", &
      m = 1., &
      sigma = 2.9209e-10, &
      eps_depth_divk = 53.07, &
      lambda_a = 6., &
      lambda_r = 18.033, &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "HYVA parameters with non-integer repulsive exponent", &
      ref = "HYVA-FH2" &
      )

  type(saftvrmie_data), parameter :: Miecx27 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "H2", &
      m = 1., &
      sigma = 3.0459e-10, &
      eps_depth_divk = 33.434, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Trejos et al. 2013, doi: 10.1063/1.4829769", &
      ref = "TREJOS2013" &
      )

  type(saftvrmie_data), parameter :: Miecx28 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "KR", &
      m = 1., &
      sigma = 3.64e-10, &
      eps_depth_divk = 166.66, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 1.3915e-22, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Irma et. al (1999) ISSN : 1029-0435/0892-7022", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx29 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "LJF", &
      m = 1., &
      sigma = 3.e-10, &
      eps_depth_divk = 30., &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 0.e+00, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx30 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "C1", &
      m = 1., &
      sigma = 3.7412e-10, &
      eps_depth_divk = 153.36, &
      lambda_a = 6., &
      lambda_r = 12.65, &
      mass = 0.e+00, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx31 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "C1", &
      m = 1., &
      sigma = 3.752e-10, &
      eps_depth_divk = 170.75, &
      lambda_a = 6., &
      lambda_r = 16.39, &
      mass = 0.e+00, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Müller's mystery parameters for methane and decane where SAFT-VR Mie fails", &
      ref = "Muller" &
      )

  type(saftvrmie_data), parameter :: Miecx32 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NE", &
      m = 1., &
      sigma = 2.8019e-10, &
      eps_depth_divk = 29.875, &
      lambda_a = 6., &
      lambda_r = 9.6977, &
      mass = 3.3509177e-26, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "DEFAULT/AASEN2019-FH0" &
      )

  type(saftvrmie_data), parameter :: Miecx33 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NE", &
      m = 1., &
      sigma = 2.7778e-10, &
      eps_depth_divk = 37.501, &
      lambda_a = 6., &
      lambda_r = 13., &
      mass = 3.3509177e-26, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH1" &
      )

  type(saftvrmie_data), parameter :: Miecx34 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NE", &
      m = 1., &
      sigma = 2.776e-10, &
      eps_depth_divk = 37.716, &
      lambda_a = 6., &
      lambda_r = 13., &
      mass = 3.3509177e-26, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH2" &
      )

  type(saftvrmie_data), parameter :: Miecx35 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NE", &
      m = 1., &
      sigma = 2.8066e-10, &
      eps_depth_divk = 33.977, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3509177e-26, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH0-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx36 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NE", &
      m = 1., &
      sigma = 2.776e-10, &
      eps_depth_divk = 35.851, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3509177e-26, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH1-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx37 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NE", &
      m = 1., &
      sigma = 2.7745e-10, &
      eps_depth_divk = 36.024, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3509177e-26, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH2-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx38 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NE", &
      m = 1., &
      sigma = 2.8e-10, &
      eps_depth_divk = 31.297, &
      lambda_a = 6., &
      lambda_r = 10.396, &
      mass = 3.3509177e-26, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "HYVA parameters with non-integer repulsive exponent", &
      ref = "HYVA-FH0" &
      )

  type(saftvrmie_data), parameter :: Miecx39 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NE", &
      m = 1., &
      sigma = 2.7765e-10, &
      eps_depth_divk = 36.648, &
      lambda_a = 6., &
      lambda_r = 12.451, &
      mass = 3.3509177e-26, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "HYVA parameters with non-integer repulsive exponent", &
      ref = "HYVA-FH1" &
      )

  type(saftvrmie_data), parameter :: Miecx40 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NE", &
      m = 1., &
      sigma = 2.7765e-10, &
      eps_depth_divk = 38.039, &
      lambda_a = 6., &
      lambda_r = 13.187, &
      mass = 3.3509177e-26, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "HYVA parameters with non-integer repulsive exponent", &
      ref = "HYVA-FH2" &
      )

  type(saftvrmie_data), parameter :: Miecx41 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "N2", &
      m = 1., &
      sigma = 3.656e-10, &
      eps_depth_divk = 98.94, &
      lambda_a = 6., &
      lambda_r = 12.26, &
      mass = 4.65173451e-26, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Ronald A. Aziz, J. Chem. Phys. 99, 4518 (1993), DOI : https://doi.org/10.1063/1.466051", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx42 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "O-H2", &
      m = 1., &
      sigma = 3.2571e-10, &
      eps_depth_divk = 17.935, &
      lambda_a = 6., &
      lambda_r = 8., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "DEFAULT/AASEN2019-FH0" &
      )

  type(saftvrmie_data), parameter :: Miecx43 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "O-H2", &
      m = 1., &
      sigma = 3.0239e-10, &
      eps_depth_divk = 26.716, &
      lambda_a = 6., &
      lambda_r = 9., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH1" &
      )

  type(saftvrmie_data), parameter :: Miecx44 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "O-H2", &
      m = 1., &
      sigma = 2.9191e-10, &
      eps_depth_divk = 55.749, &
      lambda_a = 6., &
      lambda_r = 20., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH2" &
      )

  type(saftvrmie_data), parameter :: Miecx45 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "O-H2", &
      m = 1., &
      sigma = 3.3604e-10, &
      eps_depth_divk = 14.084, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH0-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx46 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "O-H2", &
      m = 1., &
      sigma = 3.0222e-10, &
      eps_depth_divk = 33.107, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH1-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx47 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "O-H2", &
      m = 1., &
      sigma = 2.9319e-10, &
      eps_depth_divk = 40.342, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH2-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx48 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "O2", &
      m = 1., &
      sigma = 3.433e-10, &
      eps_depth_divk = 113., &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 5.31339291e-26, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Chen, Rex and Yuen, W., Oxidation of Metals, 73 (2009), DOI : 10.1007/s11085-009-9180-z", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx49 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "P-H2", &
      m = 1., &
      sigma = 3.2557e-10, &
      eps_depth_divk = 17.849, &
      lambda_a = 6., &
      lambda_r = 8., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "DEFAULT/AASEN2019-FH0" &
      )

  type(saftvrmie_data), parameter :: Miecx50 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "P-H2", &
      m = 1., &
      sigma = 3.0235e-10, &
      eps_depth_divk = 26.586, &
      lambda_a = 6., &
      lambda_r = 9., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH1" &
      )

  type(saftvrmie_data), parameter :: Miecx51 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "P-H2", &
      m = 1., &
      sigma = 2.9185e-10, &
      eps_depth_divk = 55.519, &
      lambda_a = 6., &
      lambda_r = 20., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH2" &
      )

  type(saftvrmie_data), parameter :: Miecx52 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "P-H2", &
      m = 1., &
      sigma = 3.8131e-10, &
      eps_depth_divk = 13.197, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH0-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx53 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "P-H2", &
      m = 1., &
      sigma = 3.0217e-10, &
      eps_depth_divk = 32.955, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 1, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH1-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx54 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "P-H2", &
      m = 1., &
      sigma = 2.9318e-10, &
      eps_depth_divk = 40.152, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 3.3472e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 2, &
      bib_ref = "Aasen 2019, doi: 10.1063/1.5111364", &
      ref = "AASEN2019-FH2-LJ" &
      )

  type(saftvrmie_data), parameter :: Miecx55 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "C3", &
      m = 1.6845, &
      sigma = 3.9056e-10, &
      eps_depth_divk = 239.89, &
      lambda_a = 6., &
      lambda_r = 13.006, &
      mass = 0.e+00, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx56 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "XE", &
      m = 1., &
      sigma = 3.9011e-10, &
      eps_depth_divk = 227.55, &
      lambda_a = 6., &
      lambda_r = 12., &
      mass = 2.1801716e-22, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Jadran Vrabec, Jurgen Stoll, Hans Hasse, J. Phys. Chem. B 2001, 105, DOI : 10.1021/jp012542o", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx57 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NC4", &
      m = 1.8514, &
      sigma = 4.0887e-10, &
      eps_depth_divk = 273.64, &
      lambda_a = 6., &
      lambda_r = 13.65, &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx58 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NC10", &
      m = 2.9976, &
      sigma = 4.589e-10, &
      eps_depth_divk = 400.79, &
      lambda_a = 6., &
      lambda_r = 18.885, &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx59 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NC10", &
      m = 3., &
      sigma = 4.584e-10, &
      eps_depth_divk = 415.19, &
      lambda_a = 6., &
      lambda_r = 20.92, &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Müller's mystery parameters for methane and decane where SAFT-VR Mie fails", &
      ref = "Muller" &
      )

  type(saftvrmie_data), parameter :: Miecx60 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NC22", &
      m = 3.2519, &
      sigma = 4.7484e-10, &
      eps_depth_divk = 437.72, &
      lambda_a = 6., &
      lambda_r = 20.862, &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx61 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NC20", &
      m = 4.8794, &
      sigma = 4.8788e-10, &
      eps_depth_divk = 475.76, &
      lambda_a = 6., &
      lambda_r = 22.926, &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx62 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NC7", &
      m = 2.3949, &
      sigma = 4.4282e-10, &
      eps_depth_divk = 358.51, &
      lambda_a = 6., &
      lambda_r = 17.092, &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx63 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NC6", &
      m = 2.1097, &
      sigma = 4.423e-10, &
      eps_depth_divk = 354.38, &
      lambda_a = 6., &
      lambda_r = 17.203, &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx64 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NC9", &
      m = 2.8099, &
      sigma = 4.5334e-10, &
      eps_depth_divk = 387.55, &
      lambda_a = 6., &
      lambda_r = 18.324, &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx65 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NC8", &
      m = 2.6253, &
      sigma = 4.4696e-10, &
      eps_depth_divk = 369.18, &
      lambda_a = 6., &
      lambda_r = 17.378, &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx66 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NC15", &
      m = 3.9325, &
      sigma = 4.7738e-10, &
      eps_depth_divk = 444.51, &
      lambda_a = 6., &
      lambda_r = 20.822, &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  type(saftvrmie_data), parameter :: Miecx67 = &
      saftvrmie_data(eosidx = eosSAFT_VR_MIE, &
      compName = "NC5", &
      m = 1.9606, &
      sigma = 4.2928e-10, &
      eps_depth_divk = 321.94, &
      lambda_a = 6., &
      lambda_r = 15.847, &
      mass = 6.689e-27, &
      eps = 0., &
      beta = 0., &
      assoc_scheme = no_assoc, &
      fh_order = 0, &
      bib_ref = "Lafitte et al. 2013, doi: 10.1063/1.4819786", &
      ref = "DEFAULT" &
      )

  integer, parameter :: nMiemodels = 67
  type(saftvrmie_data), dimension(nMiemodels), parameter :: Miearray = (/&
      Miecx1,Miecx2,Miecx3,Miecx4,Miecx5, &
      Miecx6,Miecx7,Miecx8,Miecx9,Miecx10, &
      Miecx11,Miecx12,Miecx13,Miecx14,Miecx15, &
      Miecx16,Miecx17,Miecx18,Miecx19,Miecx20, &
      Miecx21,Miecx22,Miecx23,Miecx24,Miecx25, &
      Miecx26,Miecx27,Miecx28,Miecx29,Miecx30, &
      Miecx31,Miecx32,Miecx33,Miecx34,Miecx35, &
      Miecx36,Miecx37,Miecx38,Miecx39,Miecx40, &
      Miecx41,Miecx42,Miecx43,Miecx44,Miecx45, &
      Miecx46,Miecx47,Miecx48,Miecx49,Miecx50, &
      Miecx51,Miecx52,Miecx53,Miecx54,Miecx55, &
      Miecx56,Miecx57,Miecx58,Miecx59,Miecx60, &
      Miecx61,Miecx62,Miecx63,Miecx64,Miecx65, &
      Miecx66,Miecx67 &
  /)

  type (Miekijdata), parameter :: SVRM_KIJ_1 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "DEFAULT", &
      bib_ref = "", &
      uid1 = "He", &
      uid2 = "Ne", &
      kijvalue = 0.0425  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_2 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "DEFAULT", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC10", &
      kijvalue = -0.0222  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_3 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "DEFAULT", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "C2", &
      kijvalue = 0.072  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_4 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH1", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "He", &
      uid2 = "Ne", &
      kijvalue = -0.22  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_5 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH1", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "H2", &
      uid2 = "Ne", &
      kijvalue = 0.105  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_6 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH1", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "He", &
      uid2 = "H2", &
      kijvalue = 0.08  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_7 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH1", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "D2", &
      uid2 = "Ne", &
      kijvalue = 0.13  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_8 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH1", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "H2", &
      uid2 = "D2", &
      kijvalue = 0.  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_9 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH1", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "He", &
      uid2 = "D2", &
      kijvalue = 0.  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_10 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH2", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "He", &
      uid2 = "Ne", &
      kijvalue = -0.06  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_11 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH2", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "H2", &
      uid2 = "Ne", &
      kijvalue = 0.105  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_12 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH2", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "He", &
      uid2 = "H2", &
      kijvalue = 0.15  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_13 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH2", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "D2", &
      uid2 = "Ne", &
      kijvalue = 0.14  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_14 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH2", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "H2", &
      uid2 = "D2", &
      kijvalue = -0.04  &
      )

  type (Miekijdata), parameter :: SVRM_KIJ_15 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH2", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "He", &
      uid2 = "D2", &
      kijvalue = 0.12  &
      )

  type (Miekijdata), parameter :: SVRM_LIJ_1 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH1", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "He", &
      uid2 = "Ne", &
      kijvalue = 0.  &
      )

  type (Miekijdata), parameter :: SVRM_LIJ_2 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH1", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "H2", &
      uid2 = "Ne", &
      kijvalue = 0.  &
      )

  type (Miekijdata), parameter :: SVRM_LIJ_3 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH1", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "He", &
      uid2 = "H2", &
      kijvalue = -0.05  &
      )

  type (Miekijdata), parameter :: SVRM_LIJ_4 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH1", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "D2", &
      uid2 = "Ne", &
      kijvalue = 0.  &
      )

  type (Miekijdata), parameter :: SVRM_LIJ_5 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH1", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "H2", &
      uid2 = "D2", &
      kijvalue = 0.  &
      )

  type (Miekijdata), parameter :: SVRM_LIJ_6 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH1", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "He", &
      uid2 = "D2", &
      kijvalue = -0.05  &
      )

  type (Miekijdata), parameter :: SVRM_LIJ_7 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH2", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "He", &
      uid2 = "Ne", &
      kijvalue = 0.  &
      )

  type (Miekijdata), parameter :: SVRM_LIJ_8 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH2", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "H2", &
      uid2 = "Ne", &
      kijvalue = 0.  &
      )

  type (Miekijdata), parameter :: SVRM_LIJ_9 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH2", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "He", &
      uid2 = "H2", &
      kijvalue = -0.025  &
      )

  type (Miekijdata), parameter :: SVRM_LIJ_10 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH2", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "D2", &
      uid2 = "Ne", &
      kijvalue = 0.  &
      )

  type (Miekijdata), parameter :: SVRM_LIJ_11 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH2", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "H2", &
      uid2 = "D2", &
      kijvalue = 0.  &
      )

  type (Miekijdata), parameter :: SVRM_LIJ_12 = &
      Miekijdata(eosidx = eosSAFT_VR_MIE, &
      ref = "AASEN2019-FH2", &
      bib_ref = "10.1063/1.5136079", &
      uid1 = "He", &
      uid2 = "D2", &
      kijvalue = -0.05  &
      )

  integer, parameter :: Miemaxkij = 15
  type (Miekijdata), dimension(Miemaxkij), parameter :: Miekijdb = (/&
      SVRM_KIJ_1,SVRM_KIJ_2,SVRM_KIJ_3,SVRM_KIJ_4,SVRM_KIJ_5, &
      SVRM_KIJ_6,SVRM_KIJ_7,SVRM_KIJ_8,SVRM_KIJ_9,SVRM_KIJ_10, &
      SVRM_KIJ_11,SVRM_KIJ_12,SVRM_KIJ_13,SVRM_KIJ_14,SVRM_KIJ_15 &
  /)

  integer, parameter :: Miemaxlij = 12
  type (Miekijdata), dimension(Miemaxlij), parameter :: Mielijdb = (/&
      SVRM_LIJ_1,SVRM_LIJ_2,SVRM_LIJ_3,SVRM_LIJ_4,SVRM_LIJ_5, &
      SVRM_LIJ_6,SVRM_LIJ_7,SVRM_LIJ_8,SVRM_LIJ_9,SVRM_LIJ_10, &
      SVRM_LIJ_11,SVRM_LIJ_12 &
  /)

end module saftvrmie_datadb
