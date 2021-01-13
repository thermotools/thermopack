!---------------------------------------------------------------------
! Module containing the parameters for SAFT-VRQ Mie
! Programmed by: M. Hammer, A. Aasen and Mr. Wilhelmsen
! Spring 2018, Imperial College London, UK
!---------------------------------------------------------------------
module saftvrmie_parameters
  use AssocSchemeUtils
  use eosdata, only: eosSAFT_VR_MIE
  use thermopack_constants, only: uid_len, ref_len, Rgas => Rgas_default
  implicit none
  save

  !> PURE COMPONENT PARAMETERS.
  !> This data structure stores pure component parameters for the
  !> SAFT-VRQ Mie EoS
  ! ---------------------------------------------------------------------------
  type :: saftvrmie_data
     integer :: eosidx
     character(len=uid_len) :: compName
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
     ! Parameter set
     character(len=ref_len) :: ref
  end type saftvrmie_data

  ! Lafitte et al. 2013: 10.1063/1.4819786
  type(saftvrmie_data), parameter :: Miecx1 = saftvrmie_data(eosSAFT_VR_MIE,"CO2", &
       1.5000, 3.1916E-10, 231.88, 5.1646, 27.557, 0.0, 0.0, 0.0, no_assoc, 0,"DEFAULT")
  ! Lafitte et al. 2013: 10.1063/1.4819786
  type(saftvrmie_data), parameter :: Miecx2 = saftvrmie_data(eosSAFT_VR_MIE,"C2", &
       1.4373, 3.7257E-10, 206.12, 6, 12.400, 0.0, 0.0, 0.0, no_assoc, 0,"DEFAULT")
  ! Lafitte et al. 2013: 10.1063/1.4819786
  type(saftvrmie_data), parameter :: Miecx3 = saftvrmie_data(eosSAFT_VR_MIE,"C1", &
       1.0, 3.7412E-10, 153.36, 6, 12.650, 0.0, 0.0, 0.0, no_assoc, 0, "DEFAULT")
  ! Lafitte et al. 2013: 10.1063/1.4819786
  type(saftvrmie_data), parameter :: Miecx4 = saftvrmie_data(eosSAFT_VR_MIE,"C2", &
       1.4373, 3.7257E-10, 206.12, 6, 12.400, 0.0, 0.0, 0.0, no_assoc, 0,"DEFAULT")
  ! Lafitte et al. 2013: 10.1063/1.4819786
  type(saftvrmie_data), parameter :: Miecx5 = saftvrmie_data(eosSAFT_VR_MIE,"C3", &
       1.6845, 3.9056E-10, 239.89, 6, 13.006, 0.0, 0.0, 0.0, no_assoc, 0, "DEFAULT")
  ! Unpublished (as of 08/2018) parameters from G. Jackon's group
  type(saftvrmie_data), parameter :: Miecx6 = saftvrmie_data(eosSAFT_VR_MIE,"AR", &
       1.000, 3.41E-10, 118.7, 6, 12.26, 6.6335E-26, 0.0, 0.0, no_assoc, 0,"DEFAULT")



  ! Hydrocarbon parameters from Lafitte et al. 2013: 10.1063/1.4819786
  type(saftvrmie_data), parameter :: Miecx7 = saftvrmie_data(eosSAFT_VR_MIE,"NC4", &
       1.8514, 4.0887E-10, 273.64, 6, 13.650, 6.6890E-27, 0.0, 0.0, no_assoc, 0,"DEFAULT")
  type(saftvrmie_data), parameter :: Miecx8 = saftvrmie_data(eosSAFT_VR_MIE,"NC5", &
       1.9606, 4.2928E-10, 321.94, 6, 15.847, 6.6890E-27, 0.0, 0.0, no_assoc, 0,"DEFAULT")
  type(saftvrmie_data), parameter :: Miecx9 = saftvrmie_data(eosSAFT_VR_MIE,"NC6", &
       2.1097, 4.4230E-10, 354.38, 6, 17.203, 6.6890E-27, 0.0, 0.0, no_assoc, 0,"DEFAULT")
  type(saftvrmie_data), parameter :: Miecx10 = saftvrmie_data(eosSAFT_VR_MIE,"NC7", &
       2.3949, 4.4282E-10, 358.51, 6, 17.092, 6.6890E-27, 0.0, 0.0, no_assoc, 0,"DEFAULT")
  type(saftvrmie_data), parameter :: Miecx11 = saftvrmie_data(eosSAFT_VR_MIE,"NC8", &
       2.6253, 4.4696E-10, 369.18, 6, 17.378, 6.6890E-27, 0.0, 0.0, no_assoc, 0,"DEFAULT")
  type(saftvrmie_data), parameter :: Miecx12 = saftvrmie_data(eosSAFT_VR_MIE,"NC9", &
       2.8099, 4.5334E-10, 387.55, 6, 18.324, 6.6890E-27, 0.0, 0.0, no_assoc, 0,"DEFAULT")
  type(saftvrmie_data), parameter :: Miecx13 = saftvrmie_data(eosSAFT_VR_MIE,"NC10", &
       2.9976, 4.5890E-10, 400.79, 6, 18.885, 6.6890E-27, 0.0, 0.0, no_assoc, 0,"DEFAULT")
  type(saftvrmie_data), parameter :: Miecx14 = saftvrmie_data(eosSAFT_VR_MIE,"NC12", &
       3.2519, 4.7484E-10, 437.72, 6, 20.862, 6.6890E-27, 0.0, 0.0, no_assoc, 0,"DEFAULT")
  type(saftvrmie_data), parameter :: Miecx15 = saftvrmie_data(eosSAFT_VR_MIE,"NC15", &
       3.9325, 4.7738E-10, 444.51, 6, 20.822, 6.6890E-27, 0.0, 0.0, no_assoc, 0,"DEFAULT")
  type(saftvrmie_data), parameter :: Miecx16 = saftvrmie_data(eosSAFT_VR_MIE,"NC20", &
       4.8794, 4.8788E-10, 475.76, 6, 22.926, 6.6890E-27, 0.0, 0.0, no_assoc, 0,"DEFAULT")

  ! Parameters for quantum fluids are from Aasen 2019 unless otherwise stated
  type(saftvrmie_data), parameter :: Miecx18 = saftvrmie_data(eosSAFT_VR_MIE, "D2" , &
       1.00, 3.1538e-10,  21.2, 6,     8, 6.6890E-27, 0.0, 0.0, no_assoc, 0, "DEFAULT/AASEN2019-FH0")
  type(saftvrmie_data), parameter :: Miecx19 = saftvrmie_data(eosSAFT_VR_MIE, "D2" , &
       1.00, 3.0203e-10, 30.273, 6,    10, 6.6890E-27, 0.0, 0.0, no_assoc, 1, "AASEN2019-FH1")
  type(saftvrmie_data), parameter :: Miecx21 = saftvrmie_data(eosSAFT_VR_MIE, "D2" , &
       1.00, 2.9897e-10, 36.913, 6,    12, 6.6890E-27, 0.0, 0.0, no_assoc, 2, "AASEN2019-FH2/AASEN2019-FH2-LJ")
  type(saftvrmie_data), parameter :: Miecx17 = saftvrmie_data(eosSAFT_VR_MIE, "D2" , &
       1.00, 3.1561e-10, 28.222, 6,    12, 6.6890E-27, 0.0, 0.0, no_assoc, 0, "AASEN2019-FH0-LJ")
  type(saftvrmie_data), parameter :: Miecx20 = saftvrmie_data(eosSAFT_VR_MIE, "D2" , &
       1.00, 3.0193e-10, 34.389, 6,    12, 6.6890E-27, 0.0, 0.0, no_assoc, 1, "AASEN2019-FH1-LJ")

  type(saftvrmie_data), parameter :: Miecx23 = saftvrmie_data(eosSAFT_VR_MIE, "H2" , &
       1.00, 3.2574e-10, 17.931, 6,     8, 3.3472E-27, 0.0, 0.0, no_assoc, 0, "DEFAULT/AASEN2019-FH0")
  type(saftvrmie_data), parameter :: Miecx25 = saftvrmie_data(eosSAFT_VR_MIE, "H2" , &
       1.00, 3.0243e-10, 26.706, 6,     9, 3.3472E-27, 0.0, 0.0, no_assoc, 1, "AASEN2019-FH1")
  type(saftvrmie_data), parameter :: Miecx27 = saftvrmie_data(eosSAFT_VR_MIE, "H2" , &
       1.00, 2.9195e-10, 55.729, 6,    20, 3.3472E-27, 0.0, 0.0, no_assoc, 2, "AASEN2019-FH2")
  type(saftvrmie_data), parameter :: Miecx22 = saftvrmie_data(eosSAFT_VR_MIE, "H2" , &
       1.00, 3.5333e-10, 14.312, 6,    12, 3.3472E-27, 0.0, 0.0, no_assoc, 0, "AASEN2019-FH0-LJ")
  type(saftvrmie_data), parameter :: Miecx24 = saftvrmie_data(eosSAFT_VR_MIE, "H2" , &
       1.00, 3.0225e-10, 33.096, 6,    12, 3.3472E-27, 0.0, 0.0, no_assoc, 1, "AASEN2019-FH1-LJ")
  type(saftvrmie_data), parameter :: Miecx26 = saftvrmie_data(eosSAFT_VR_MIE, "H2" , &
       1.00, 2.9324e-10, 40.321, 6,    12, 3.3472E-27, 0.0, 0.0, no_assoc, 2, "AASEN2019-FH2-LJ")

  type(saftvrmie_data), parameter :: Miecx28 = saftvrmie_data(eosSAFT_VR_MIE, "HE" , &
       1.00, 3.353e-10,  4.44, 6, 14.84, 6.6464764E-27, 0.0, 0.0, no_assoc, 0, "DEFAULT/AASEN2019-FH0") ! Herdes 2015
  type(saftvrmie_data), parameter :: Miecx31 = saftvrmie_data(eosSAFT_VR_MIE, "HE" , &
       1.00, 2.7443e-10, 5.4195, 6,     9, 6.6464764E-27, 0.0, 0.0, no_assoc, 1, "AASEN2019-FH1")
  type(saftvrmie_data), parameter :: Miecx33 = saftvrmie_data(eosSAFT_VR_MIE, "HE" , &
       1.00, 2.549e-10, 10.952, 6,    13, 6.6464764E-27, 0.0, 0.0, no_assoc, 2, "AASEN2019-FH2")
  type(saftvrmie_data), parameter :: Miecx29 = saftvrmie_data(eosSAFT_VR_MIE, "HE" , &
       1.00, 3.1273e-10, 4.1463, 6,    12, 6.6464764E-27, 0.0, 0.0, no_assoc, 0, "AASEN2019-FH0-LJ")
  type(saftvrmie_data), parameter :: Miecx30 = saftvrmie_data(eosSAFT_VR_MIE, "HE" , &
       1.00, 2.8107e-10, 6.6893, 6,    12, 6.6464764E-27, 0.0, 0.0, no_assoc, 1, "AASEN2019-FH1-LJ")
  type(saftvrmie_data), parameter :: Miecx32 = saftvrmie_data(eosSAFT_VR_MIE, "HE" , &
       1.00, 2.5442e-10, 10.393, 6,    12, 6.6464764E-27, 0.0, 0.0, no_assoc, 2, "AASEN2019-FH2-LJ")


  type(saftvrmie_data), parameter :: Miecx34 = saftvrmie_data(eosSAFT_VR_MIE, "NE" , &
       1.00, 2.8019e-10, 29.875, 6, 9.6977, 3.3509177E-26, 0.0, 0.0, no_assoc, 0, "DEFAULT/AASEN2019-FH0")
  type(saftvrmie_data), parameter :: Miecx37 = saftvrmie_data(eosSAFT_VR_MIE, "NE" , &
       1.00, 2.7778e-10, 37.501, 6,    13, 3.3509177E-26, 0.0, 0.0, no_assoc, 1, "AASEN2019-FH1")
  type(saftvrmie_data), parameter :: Miecx39 = saftvrmie_data(eosSAFT_VR_MIE, "NE" , &
       1.00, 2.776e-10, 37.716, 6,    13, 3.3509177E-26, 0.0, 0.0, no_assoc, 2, "AASEN2019-FH2")
  type(saftvrmie_data), parameter :: Miecx35 = saftvrmie_data(eosSAFT_VR_MIE, "NE" , &
       1.00, 2.8066e-10, 33.977, 6,    12, 3.3509177E-26, 0.0, 0.0, no_assoc, 0, "AASEN2019-FH0-LJ")
  type(saftvrmie_data), parameter :: Miecx36 = saftvrmie_data(eosSAFT_VR_MIE, "NE" , &
       1.00, 2.776e-10, 35.851, 6,    12, 3.3509177E-26, 0.0, 0.0, no_assoc, 1, "AASEN2019-FH1-LJ")
  type(saftvrmie_data), parameter :: Miecx38 = saftvrmie_data(eosSAFT_VR_MIE, "NE" , &
       1.00, 2.7745e-10, 36.024, 6,    12, 3.3509177E-26, 0.0, 0.0, no_assoc, 2, "AASEN2019-FH2-LJ")

  type(saftvrmie_data), parameter :: Miecx41 = saftvrmie_data(eosSAFT_VR_MIE, "O-H2" , &
       1.00, 3.2571e-10, 17.935, 6,     8, 3.3472E-27, 0.0, 0.0, no_assoc, 0, "DEFAULT/AASEN2019-FH0")
  type(saftvrmie_data), parameter :: Miecx43 = saftvrmie_data(eosSAFT_VR_MIE, "O-H2" , &
       1.00, 3.0239e-10, 26.716, 6,     9, 3.3472E-27, 0.0, 0.0, no_assoc, 1, "AASEN2019-FH1")
  type(saftvrmie_data), parameter :: Miecx45 = saftvrmie_data(eosSAFT_VR_MIE, "O-H2" , &
       1.00, 2.9191e-10, 55.749, 6,    20, 3.3472E-27, 0.0, 0.0, no_assoc, 2, "AASEN2019-FH2")
  type(saftvrmie_data), parameter :: Miecx40 = saftvrmie_data(eosSAFT_VR_MIE, "O-H2" , &
       1.00, 3.3604e-10, 14.084, 6,    12, 3.3472E-27, 0.0, 0.0, no_assoc, 0, "AASEN2019-FH0-LJ")
  type(saftvrmie_data), parameter :: Miecx42 = saftvrmie_data(eosSAFT_VR_MIE, "O-H2" , &
       1.00, 3.0222e-10, 33.107, 6,    12, 3.3472E-27, 0.0, 0.0, no_assoc, 1, "AASEN2019-FH1-LJ")
  type(saftvrmie_data), parameter :: Miecx44 = saftvrmie_data(eosSAFT_VR_MIE, "O-H2" , &
       1.00, 2.9319e-10, 40.342, 6,    12, 3.3472E-27, 0.0, 0.0, no_assoc, 2, "AASEN2019-FH2-LJ")

  type(saftvrmie_data), parameter :: Miecx47 = saftvrmie_data(eosSAFT_VR_MIE, "P-H2" , &
       1.00, 3.2557e-10, 17.849, 6,     8, 3.3472E-27, 0.0, 0.0, no_assoc, 0, "DEFAULT/AASEN2019-FH0")
  type(saftvrmie_data), parameter :: Miecx49 = saftvrmie_data(eosSAFT_VR_MIE, "P-H2" , &
       1.00, 3.0235e-10, 26.586, 6,     9, 3.3472E-27, 0.0, 0.0, no_assoc, 1, "AASEN2019-FH1")
  type(saftvrmie_data), parameter :: Miecx51 = saftvrmie_data(eosSAFT_VR_MIE, "P-H2" , &
       1.00, 2.9185e-10, 55.519, 6,    20, 3.3472E-27, 0.0, 0.0, no_assoc, 2, "AASEN2019-FH2")
  type(saftvrmie_data), parameter :: Miecx46 = saftvrmie_data(eosSAFT_VR_MIE, "P-H2" , &
       1.00, 3.8131e-10, 13.197, 6,    12, 3.3472E-27, 0.0, 0.0, no_assoc, 0, "AASEN2019-FH0-LJ")
  type(saftvrmie_data), parameter :: Miecx48 = saftvrmie_data(eosSAFT_VR_MIE, "P-H2" , &
       1.00, 3.0217e-10, 32.955, 6,    12, 3.3472E-27, 0.0, 0.0, no_assoc, 1, "AASEN2019-FH1-LJ")
  type(saftvrmie_data), parameter :: Miecx50 = saftvrmie_data(eosSAFT_VR_MIE, "P-H2" , &
       1.00, 2.9318e-10, 40.152, 6,    12, 3.3472E-27, 0.0, 0.0, no_assoc, 2, "AASEN2019-FH2-LJ")

  ! HYVA parameters with non-integer repulsive exponent (not recommended for general use)
  type(saftvrmie_data), parameter :: Miecx52 = saftvrmie_data(eosSAFT_VR_MIE,"HE", &
       1.0000e+00, 2.8525e-10, 4.5764e+00, 6.0000e+00, 8.3471e+00, 6.6464764E-27, 0.0, 0.0, no_assoc, 0, "HYVA-FH0")
  type(saftvrmie_data), parameter :: Miecx53 = saftvrmie_data(eosSAFT_VR_MIE,"HE", &
       1.0000e+00, 2.6615e-10, 7.9191e+00, 6.0000e+00, 1.0455e+01, 6.6464764E-27, 0.0, 0.0, no_assoc, 1, "HYVA-FH1")
  type(saftvrmie_data), parameter :: Miecx54 = saftvrmie_data(eosSAFT_VR_MIE,"HE", &
       1.0000e+00, 2.5682e-10, 1.1942e+01, 6.0000e+00, 1.3239e+01, 6.6464764E-27, 0.0, 0.0, no_assoc, 2, "HYVA-FH2")
  type(saftvrmie_data), parameter :: Miecx55 = saftvrmie_data(eosSAFT_VR_MIE,"NE", &
       1.0000e+00, 2.8000e-10, 3.1297e+01, 6.0000e+00, 1.0396e+01, 3.3509177E-26, 0.0, 0.0, no_assoc, 0, "HYVA-FH0")
  type(saftvrmie_data), parameter :: Miecx56 = saftvrmie_data(eosSAFT_VR_MIE,"NE", &
       1.0000e+00, 2.7765e-10, 3.6648e+01, 6.0000e+00, 1.2451e+01, 3.3509177E-26, 0.0, 0.0, no_assoc, 1, "HYVA-FH1")
  type(saftvrmie_data), parameter :: Miecx57 = saftvrmie_data(eosSAFT_VR_MIE,"NE", &
       1.0000e+00, 2.7765e-10, 3.8039e+01, 6.0000e+00, 1.3187e+01, 3.3509177E-26, 0.0, 0.0, no_assoc, 2, "HYVA-FH2")
  type(saftvrmie_data), parameter :: Miecx58 = saftvrmie_data(eosSAFT_VR_MIE,"H2", &
       1.0000e+00, 3.1980e-10, 1.4492e+01, 6.0000e+00, 6.4709e+00 , 3.3472E-27, 0.0, 0.0, no_assoc, 0, "HYVA-FH0")
  type(saftvrmie_data), parameter :: Miecx59 = saftvrmie_data(eosSAFT_VR_MIE,"H2", &
       1.0000e+00, 3.0022e-10, 2.8349e+01, 6.0000e+00, 9.5413e+00 , 3.3472E-27, 0.0, 0.0, no_assoc, 1, "HYVA-FH1")
  type(saftvrmie_data), parameter :: Miecx60 = saftvrmie_data(eosSAFT_VR_MIE,"H2", &
       1.0000e+00, 2.9209e-10, 5.3070e+01, 6.0000e+00, 1.8033e+01 , 3.3472E-27, 0.0, 0.0, no_assoc, 2, "HYVA-FH2")
  type(saftvrmie_data), parameter :: Miecx61 = saftvrmie_data(eosSAFT_VR_MIE,"H2", &
       1.0000, 3.0459E-10, 33.434, 6, 12.0, 3.3472E-27, 0.0, 0.0, no_assoc, 0, "TREJOS2013") ! Trejos et al. 2013, doi: 10.1063/1.4829769

  ! Müller's mystery parameters for methane and decane where SAFT-VR Mie fails
  type(saftvrmie_data), parameter :: Miecx62 = saftvrmie_data(eosSAFT_VR_MIE,"C1", &
       1.0, 3.752E-10, 170.75, 6, 16.39, 0.0, 0.0, 0.0, no_assoc, 0, "Muller")
  type(saftvrmie_data), parameter :: Miecx63 = saftvrmie_data(eosSAFT_VR_MIE,"NC10", &
       3.0, 4.5840E-10, 415.19, 6, 20.92, 6.6890E-27, 0.0, 0.0, no_assoc, 0, "Muller")

  !Unpublished water parameters (courtesy of Edward Graham)
  type(saftvrmie_data), parameter :: Miecx64 = saftvrmie_data(eosSAFT_VR_MIE,"H2O", &
       1*1.25656, 2.802422E-10, 3.512321E+02, 6, 2.512615E+01, 0E-27, &
       1.630570E+03*Rgas, 177.6236E-30, assoc_scheme_4C, 0, "DEFAULT")

  ! ! Table 1 in Dufal 2015
  ! type(saftvrmie_data), parameter :: Miecx64 = saftvrmie_data(eosSAFT_VR_MIE,"H2O", &
  !      1.0, 3.1610E-10, 488.75, 6, 52.367, 0E-27, &
  !      1210.0*Rgas, 3309.2E-30/(3.0661E-10)**3, assoc_scheme_4C, 0, 1)

  ! Table 2 in Dufal 2015
  ! type(saftvrmie_data), parameter :: Miecx64 = saftvrmie_data(eosSAFT_VR_MIE,"H2O", &
  !      1.0, 3.0661E-10, 170.00, 6, 19.697, 0E-27, &
  !      2660.0*Rgas, 3309.2E-30/(3.0661E-10)**3, assoc_scheme_4C, 0, 1)


  type(saftvrmie_data), parameter :: Miecx65 = saftvrmie_data(eosSAFT_VR_MIE,"ETOH", &
       3*0.75216313, 3.2903E-10, 238.97, 6, 12.282, 0E-27, &
       2247.3*Rgas, 4.2794E-29, assoc_scheme_3B, 0, "DEFAULT")

  !-----------------------------------------------------------
  type(saftvrmie_data), parameter :: Miecx66 = saftvrmie_data(eosSAFT_VR_MIE,"LJF", &
       1, 3.0E-10, 30.0, 6, 12, 0.0, &
       0.0, 0.0, no_assoc, 0, "DEFAULT")
  !-----------------------------------------------------------

  integer, parameter :: nMiemodels = 66
  type(saftvrmie_data), dimension(nMiemodels), parameter :: Miearray = (/ &
       Miecx1, &
       Miecx2, &
       Miecx3, &
       Miecx4, &
       Miecx5, &
       Miecx6, &
       Miecx7, &
       Miecx8, &
       Miecx9, &
       Miecx10, &
       Miecx11, &
       Miecx12, &
       Miecx13, &
       Miecx14, &
       Miecx15, &
       Miecx16, &
       Miecx17, &
       Miecx18, &
       Miecx19, &
       Miecx20, &
       Miecx21, &
       Miecx22, &
       Miecx23, &
       Miecx24, &
       Miecx25, &
       Miecx26, &
       Miecx27, &
       Miecx28, &
       Miecx29, &
       Miecx30, &
       Miecx31, &
       Miecx32, &
       Miecx33, &
       Miecx34, &
       Miecx35, &
       Miecx36, &
       Miecx37, &
       Miecx38, &
       Miecx39, &
       Miecx40, &
       Miecx41, &
       Miecx42, &
       Miecx43, &
       Miecx44, &
       Miecx45, &
       Miecx46, &
       Miecx47, &
       Miecx48, &
       Miecx49, &
       Miecx50, &
       Miecx51, &
       Miecx52, &
       Miecx53, &
       Miecx54, &
       Miecx55, &
       Miecx56, &
       Miecx57, &
       Miecx58, &
       Miecx59, &
       Miecx60, &
       Miecx61, &
       Miecx62, &
       Miecx63, &
       Miecx64, &
       Miecx65, &
       Miecx66/)

  !> INTERACTION PARAMETERS FOR THE SAFT-VR-MIE DISPERSION TERM.
  ! ----------------------------------------------------------------------------
  type :: Miekijdata
     sequence
     integer:: eosidx
     character (len=8) :: uid1, uid2
     integer :: setno
     real :: kijvalue
  end type Miekijdata

  integer, parameter :: Miemaxkij = 3
  type(Miekijdata), dimension(Miemaxkij), parameter :: Miekijdb = (/ &
       Miekijdata(eosSAFT_VR_MIE,"He","Ne",1,0.0425), &
       Miekijdata(eosSAFT_VR_MIE,"C2","NC10",1,-0.0222), &
       Miekijdata(eosSAFT_VR_MIE,"H2S","C2",1,0.072)/)

contains

  !> Get the index in the PCarray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getMiedataIdx(eosidx,compName,ref) result(idx)
    use stringmod, only: str_eq, string_match
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: compName, ref
    integer :: idx, idx_default
    logical :: found

    found = .false.
    idx = 1
    idx_default = -1
    do while (idx <= nMiemodels)
      if ((eosidx==Miearray(idx)%eosidx) .and. &
           str_eq(compName, Miearray(idx)%compName)) then
        if (string_match(ref,MieArray(idx)%ref)) then
          found = .true.
          exit
        else if (string_match("DEFAULT",MieArray(idx)%ref)) then
          idx_default = idx
        endif
      endif
      idx = idx + 1
    enddo

    if (.not. found .and. idx_default > 0) then
      idx = idx_default
      found = .true.
    endif
    if (.not. found) then
       print *, "ERROR FOR COMPONENT ", compname
       call stoperror("The SAFT-VR-MIE parameters don't exist.")
    end if

  end function getMiedataIdx

  !> Retrieve binary interaction parameter for components uid1 and uid2.
  !> If no kij is stored in the database PCkijdb, it returns 0.0.
  function getMiekij (eosidx,uid1,uid2,setno) result(kijvalue)
    use stringmod, only: str_eq
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: uid1, uid2
    integer, intent(in), optional :: setno
    real :: kijvalue
    integer :: idx, setno_loc
    logical :: match_11_22, match_12_21

    setno_loc = 1
    if (present(setno)) setno_loc = setno

    kijvalue = 0.0 ! default value if the binary is not in Miekijdb.
    idx = 1
    do idx = 1,Miemaxkij
       match_11_22 = str_eq(uid1,Miekijdb(idx)%uid1) .and. str_eq(uid2,Miekijdb(idx)%uid2)
       match_12_21 = str_eq(uid1,Miekijdb(idx)%uid2) .and. str_eq(uid2,Miekijdb(idx)%uid1)

       if ( eosidx==Miekijdb(idx)%eosidx .and. (match_11_22 .or. match_12_21) &
            .and. Miekijdb(idx)%setno==setno_loc) then
          kijvalue = Miekijdb(idx)%kijvalue
          exit ! Exit means "break" in Fortran.
       endif
    end do
  end function getMiekij

  subroutine getMieKij_allComps(nc,comp,eosidx,kij)
    use thermopack_var, only: gendata_pointer
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eosidx
    ! Output
    real, intent(out) :: kij(nc,nc)
    ! Locals
    integer :: ic,jc

    kij = 0.0
    do ic=1,nc
       do jc=ic+1,nc
          kij(ic,jc) = getMiekij(eosidx,comp(ic)%p_comp%ident,comp(jc)%p_comp%ident,setno=1)
          kij(jc,ic) = kij(ic,jc)
       end do
    end do

  end subroutine getMieKij_allComps

  !> Map SAFT-VR Mie paramaters to active component
  subroutine getSaftVrMiePureFluidParams(compName,eosidx,ref,saftvrmie_comp,fh_order,found)
    ! Input
    character(len=*), intent(in) :: compName, ref
    integer, intent(in) :: eosidx
    ! Output
    type(saftvrmie_data), intent(out) :: saftvrmie_comp
    integer, intent(out) :: fh_order
    logical, optional, intent(out) :: found
    ! Locals
    integer :: idx
    character(len=100) :: message

    idx = getMiedataIdx(eosidx,compName,ref)
    if ( idx == 0 ) then
       if (present(found)) then
          found = .false.
       else
          write(message,*) "saftvrmie_interface::getSaftVrMiePureFluidParams"//&
               "No SAFT-VR Mie paramaters for "//trim(compName)
          call stoperror(trim(message))
       endif
       return
    else
       if (present(found)) then
          found = .true.
       endif
    end if

    saftvrmie_comp%m = Miearray(idx)%m
    saftvrmie_comp%sigma = Miearray(idx)%sigma
    saftvrmie_comp%eps_depth_divk = Miearray(idx)%eps_depth_divk
    saftvrmie_comp%lambda_r = Miearray(idx)%lambda_r
    saftvrmie_comp%lambda_a = Miearray(idx)%lambda_a
    saftvrmie_comp%eps = Miearray(idx)%eps
    saftvrmie_comp%beta = Miearray(idx)%beta
    saftvrmie_comp%assoc_scheme = Miearray(idx)%assoc_scheme
    saftvrmie_comp%mass = Miearray(idx)%mass
    saftvrmie_comp%eps_depth_divk = Miearray(idx)%eps_depth_divk
    fh_order = Miearray(idx)%fh_order
  end subroutine getSaftVrMiePureFluidParams

  !> Map SAFT-VR Mie paramaters to active components
  subroutine getSaftVrMieParams(nc,comp,eosidx,ref,saftvrmie_comp,fh_orders,found)
    use thermopack_var, only: gendata_pointer
    ! Input
    type(gendata_pointer), intent(inout)  :: comp(nc)    !< Component vector.
    character(len=*), intent(in) :: ref   !< Parameter sets to use for components
    integer, intent(in) :: eosidx,nc
    ! Output
    type(saftvrmie_data), intent(out) :: saftvrmie_comp(nc)
    integer, intent(out) :: fh_orders(nc)
    logical, optional, intent(out) :: found
    ! Locals
    integer :: i

    do i=1,nc
       call getSaftVrMiePureFluidParams(trim(comp(i)%p_comp%ident),eosidx,ref,&
            saftvrmie_comp(i),fh_orders(i), found)
       if (present(found)) then
          if (.not. found) then
             return
          endif
       endif
    enddo
  end subroutine getSaftVrMieParams


  subroutine getSaftVrMieAssocParams_allComps(nc,comp,eosidx,ref,found,&
       eps,beta,scheme)
    use compdata, only: gendata_pointer
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: ref
    ! Output
    logical, intent(out) :: found(nc)
    real, intent(out) :: eps(nc), beta(nc)
    integer, intent(out) :: scheme(nc)
    ! Locals
    integer :: ic

    do ic=1,nc
       call getSaftVrMieAssocParams_singleComp(comp(ic)%p_comp%ident,eosidx,&
            ref,found(ic), eps(ic),beta(ic),scheme(ic))
    end do

  end subroutine getSaftVrMieAssocParams_allComps

  subroutine getSaftVrMieAssocParams_singleComp(compName,eosidx,ref,found,&
       eps,beta,scheme)
    ! Input
    character(len=*), intent(in) :: compName, ref
    integer, intent(in) :: eosidx
    ! Output
    logical, intent(out) :: found
    real, intent(out) :: eps, beta
    integer, intent(out) :: scheme
    ! Locals
    integer :: idx

    idx = getMiedataIdx(eosidx,compName,ref)
    if ( idx == 0 ) then
       found = .false.
       return
    end if

    found = .true.
    eps = Miearray(idx)%eps
    beta = Miearray(idx)%beta
    scheme = Miearray(idx)%assoc_scheme
  end subroutine getSaftVrMieAssocParams_singleComp

end module saftvrmie_parameters
