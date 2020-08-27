!> Module for PC-SAFT pure-component parameters and binary interaction
!> parameters. Also contains parameters for the PeTS equation of state.

!> NB: If you want to add new parameters here, beware that different authors are
!> inconsistent wrt. whether beta should be multiplied with pi/6 or not. For
!> this reason you should validate your results before using these parameters.
!> If you get strange results, try multiplying beta with pi/6=0.5236.
module pc_saft_parameters
  use AssocSchemeUtils
  use eosdata, only: eosPC_SAFT, eosPeTS
  use thermopack_constants, only: Rgas => Rgas_default, uid_len, ref_len
  implicit none
  save

  real, parameter :: PI_6 = 4.0*atan(1.0)/6.0

  !> PURE COMPONENT PARAMETERS.
  !> This data structure stores pure component parameters for the PC-SAFT
  !> equation of state.
  ! ---------------------------------------------------------------------------
  type :: pc_saft_data
     sequence
     integer :: eosidx
     character (len=uid_len) :: compName
     ! Pure component fitted parameters.
     real :: m        !< [-]. Mean number of segments.
     real :: sigma    !< [m]. Temperature-independent segment diameter.
     real :: eps_depth_divk !< [K]. Well depth divided by Boltzmann's constant k.
     ! Association parameters.
     real :: eps  !< [J/mol].
     real :: beta !< [-]. Also known as kappa in SAFT literature.
     ! Association scheme.
     integer :: assoc_scheme
     ! Set number
     character (len=ref_len) :: ref
  end type pc_saft_data

  ! SOURCES:
  ! [0] Regressed by in-house.
  ! [1] Kontogeorgis & Folas, "Thermodynamic Models for Industrial Applications", Wiley 2010 (appendix A).
  ! [2] Tang & Gross. (2010) "Modeling the phase equilibria of hydrogen sulfide
  !     and carbon dioxide in mixtures with hydrocarbons an water using the
  !     PCP-SAFT equation of state". Fluid Phase Equilibria 293.
  ! [3] Smith (2017) MSc thesis. "Measurement and modelling of the vapour--liquid equilibria of binary mixtures of water and alkanols"
  type(pc_saft_data), parameter :: PCcx1 = pc_saft_data(eosPC_SAFT,"C1", &
       1.0000, 3.7039E-10, 150.03, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx2 = pc_saft_data(eosPC_SAFT,"C2", &
       1.6069, 3.5206E-10, 191.42, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx3 = pc_saft_data(eosPC_SAFT,"C3", &
       2.0020, 3.6184E-10, 208.11, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx4 = pc_saft_data(eosPC_SAFT,"NC4", &
       2.3316, 3.7086E-10, 222.88, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx5 = pc_saft_data(eosPC_SAFT,"NC5", &
       2.6896, 3.7729E-10, 231.2, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx6 = pc_saft_data(eosPC_SAFT,"NC6", &
       3.0576, 3.7983E-10, 236.77, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx7 = pc_saft_data(eosPC_SAFT,"NC7", &
       3.4831, 3.8049E-10, 238.4, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx8 = pc_saft_data(eosPC_SAFT,"NC8", &
       3.8176, 3.8373E-10, 242.78, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx9 = pc_saft_data(eosPC_SAFT,"NC9", &
       4.2079, 3.8448E-10, 244.51, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx10 = pc_saft_data(eosPC_SAFT,"NC10", &
       4.6627, 3.8384E-10, 243.87, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx11 = pc_saft_data(eosPC_SAFT,"BENZENE", &
       2.4653, 3.6478E-10, 287.35, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx12 = pc_saft_data(eosPC_SAFT,"TOLU", &
       2.8149, 3.7169E-10, 285.69, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx13 = pc_saft_data(eosPC_SAFT,"N2", &
       1.2053, 3.313E-10,  90.96,  0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx14 = pc_saft_data(eosPC_SAFT,"O2", &
       1.1217, 3.2098E-10, 114.96,  0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx15 = pc_saft_data(eosPC_SAFT,"CYCLOHEX", &
       2.5303, 3.8499E-10, 278.11, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]

  type(pc_saft_data), parameter :: PCcx16 = pc_saft_data(eosPC_SAFT,"H2S", &
       1.6941, 3.0214E-10, 226.79, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1] H2S without association.
  type(pc_saft_data), parameter :: PCcx17 = pc_saft_data(eosPC_SAFT,"H2S", &
       1.355, 3.309E-10, 234.25, 780.8*Rgas, 0.001, assoc_scheme_2B,"Default/Tang_Gross2010") ! [2]

  ! Grenner et al. 2006. doi 10.1021/ie0605332. DeltaP 0.93, DeltaRho=2.62 for T>323K.
  type(pc_saft_data), parameter :: PCcx18 = pc_saft_data(eosPC_SAFT,"H2O", &
       1.50, 2.6273E-10, 180.3, 1804.22*Rgas, 0.0942, assoc_scheme_4C,"Default/Grenner2006")

  type(pc_saft_data), parameter :: PCcx19 = pc_saft_data(eosPC_SAFT,"H2O", &
       1.0656, 3.0007E-10, 366.51, 2500.7*Rgas, 0.034868,  assoc_scheme_2B,"Kontogeorgis_Folas2001") ! [1]

  type(pc_saft_data), parameter :: PCcx20 = pc_saft_data(eosPC_SAFT,"CO2", &
       2.0729, 2.7852E-10, 169.21, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  ! [10]
  type(pc_saft_data), parameter :: PCcx21 = pc_saft_data(eosPC_SAFT,"CO2", &
       2.58239e+00 ,2.56106e-10 ,1.51691e+02 ,0.0 ,0.0,no_assoc,"Tang_Gross2010")

  ! [0] CO2 3B regressed to Tc, Pc and Psat. AAD_P 0.22%, AAD_liqv 3.08%
  type(pc_saft_data), parameter :: PCcx22 = pc_saft_data(eosPC_SAFT,"CO2", &
       1.72897e+00 ,2.76102e-10 ,8.37215e+01 ,9.25650e+03 ,1.71605e-01, assoc_scheme_3B,"Smith2017")

  ! [1]
  type(pc_saft_data), parameter :: PCcx23 = pc_saft_data(eosPC_SAFT,"AR", &
       0.9285, 3.4784E-10, 122.23, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")

  type(pc_saft_data), parameter :: PCcx24 = pc_saft_data(eosPC_SAFT, "NC11", &
       4.8640, 3.8986E-10, 246.9390, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx25 = pc_saft_data(eosPC_SAFT, "NC12", &
       5.2133, 3.9115E-10, 248.0042, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx26 = pc_saft_data(eosPC_SAFT, "NC13", &
       5.5627, 3.9227E-10, 248.9356, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx27 = pc_saft_data(eosPC_SAFT, "NC14", &
       5.9120, 3.9326E-10, 249.7570, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx28 = pc_saft_data(eosPC_SAFT, "NC15", &
       6.2614, 3.9412E-10, 250.4867, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx29 = pc_saft_data(eosPC_SAFT, "NC16", &
       6.6107, 3.9490E-10, 251.1392, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx30 = pc_saft_data(eosPC_SAFT, "NC17", &
       6.9600, 3.9559E-10, 251.7263, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx31 = pc_saft_data(eosPC_SAFT, "NC18", &
       7.3094, 3.9622E-10, 252.2573, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx32 = pc_saft_data(eosPC_SAFT, "NC19", &
       7.6587, 3.9678E-10, 252.7398, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx33 = pc_saft_data(eosPC_SAFT, "NC20", &
       8.0081, 3.9730E-10, 253.1802, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx34 = pc_saft_data(eosPC_SAFT, "NC21", &
       8.3574, 3.9777E-10, 253.5838, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx35 = pc_saft_data(eosPC_SAFT, "NC22", &
       8.7068, 3.9820E-10, 253.9550, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx36 = pc_saft_data(eosPC_SAFT, "NC23", &
       9.0561, 3.9860E-10, 254.2975, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx37 = pc_saft_data(eosPC_SAFT, "NC24", &
       9.4055, 3.9897E-10, 254.6147, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")
  type(pc_saft_data), parameter :: PCcx38 = pc_saft_data(eosPC_SAFT, "NC25", &
       9.7548, 3.9931E-10, 254.9091, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001")

  type(pc_saft_data), parameter :: PCcx39 = pc_saft_data(eosPC_SAFT,"NH3", & ! [0] ammonia (AAD(P,liqv)=(2.0,0.61))
       2.25807 , 2.37802e-10 ,1.26868e+02 , 1.10326e+04, 2.04790e-01, assoc_scheme_3B,"Default/InHouse")

  type(pc_saft_data), parameter :: PCcx40 = pc_saft_data(eosPC_SAFT,"IC4", &
       2.2616, 3.7574E-10, 216.53, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]
  type(pc_saft_data), parameter :: PCcx41 = pc_saft_data(eosPC_SAFT,"IC5", &
       2.562, 3.8296E-10, 230.75, 0.0, 0.0, no_assoc,"Default/Kontogeorgis_Folas2001") ! [1]

  ! de Villiers 2011. doi 10.1021/ie200521k. 0.5<Tr<0.9, (DeltaP,DeltaRho)=(0.52,0.37)
  type(pc_saft_data), parameter :: PCcx42 = pc_saft_data(eosPC_SAFT,"MEOH", &
       2.1, 2.7998E-10, 197.23, 2535.0*Rgas, 0.8230*PI_6, assoc_scheme_2C,"Default/deVilliers2011")

  ! de Villiers 2011. doi 10.1021/ie200521k. 0.5<Tr<0.9, (DeltaP,DeltaRho)=(0.18,0.21)
  type(pc_saft_data), parameter :: PCcx43 = pc_saft_data(eosPC_SAFT,"ETOH", &
       2.3609, 3.1895E-10, 207.56, 2695.69*Rgas, 0.03270*PI_6, assoc_scheme_2C,"Default/deVilliers2011")

  type(pc_saft_data), parameter :: PCcx44 = pc_saft_data(eosPC_SAFT,"ETOH", &
       1.2309, 4.1057E-10, 316.91, 2811.02*Rgas, 0.00633*PI_6, assoc_scheme_2B,"Tang_Gross2010")

  ! Gross and Sadowski 2002, Ind. Eng. Chem. Res. 41 (DeltaP,Deltav)=(0.58,0.71)
  type(pc_saft_data), parameter :: PCcx45 = pc_saft_data(eosPC_SAFT,"ETOH", &
       2.3827, 3.1771E-10, 198.24, 2653.4*Rgas, 0.032384, assoc_scheme_2B,"Gross_Sadowski2002")

  ! de Villiers 2011. doi 10.1021/ie200521k. 0.5<Tr<0.9, (DeltaP,DeltaRho)=(0.27,0.25)
  type(pc_saft_data), parameter :: PCcx46 = pc_saft_data(eosPC_SAFT,"PROP1OL", &
       2.9537, 3.2473E-10, 226.36, 2448.02*Rgas, 0.02280*PI_6, assoc_scheme_2C,"Default/deVilliers2011")
  ! de Villiers 2011. doi 10.1021/ie200521k. 0.5<Tr<0.9, (DeltaP,DeltaRho)=(0.19, 0.33)
  type(pc_saft_data), parameter :: PCcx47 = pc_saft_data(eosPC_SAFT,"BUT1OL", &
       2.9614, 3.5065E-10, 253.29, 2601.00*Rgas, 0.00740*PI_6, assoc_scheme_2C,"Default/deVilliers2011")
  ! de Villiers 2011. doi 10.1021/ie200521k. 0.5<Tr<0.9, (DeltaP,DeltaRho)=(0.09,0.10)
  type(pc_saft_data), parameter :: PCcx48 = pc_saft_data(eosPC_SAFT,"PENT1OL", &
       3.1488, 3.6350E-10, 261.96, 2555.34*Rgas, 0.00750*PI_6, assoc_scheme_2C,"Default/deVilliers2011")
  ! de Villiers 2011. doi 10.1021/ie200521k. 0.5<Tr<0.9, (DeltaP,DeltaRho)=(0.09,0.42)
  type(pc_saft_data), parameter :: PCcx49 = pc_saft_data(eosPC_SAFT,"HEX1OL", &
       3.1542, 3.8188E-10, 277.24, 2946.10*Rgas, 0.00250*PI_6, assoc_scheme_2C,"Default/deVilliers2011")

  ! Gross and Sadowski 2002, Ind. Eng. Chem. Res. 41 (DeltaP,Deltav)=(0.85,1.71)
  type(pc_saft_data), parameter :: PCcx50 = pc_saft_data(eosPC_SAFT,"PROP1OL", &
       2.9997, 3.2522E-10, 233.40, 2276.8*Rgas, 0.015268, assoc_scheme_2B,"Tang_Gross2010")
  ! Gross and Sadowski 2002, Ind. Eng. Chem. Res. 41 (DeltaP,Deltav)=(1.78,1.63)
  type(pc_saft_data), parameter :: PCcx51 = pc_saft_data(eosPC_SAFT,"BUT1OL", &
       2.7515, 3.6189E-10, 259.59, 2544.6*Rgas, 0.006692, assoc_scheme_2B,"Tang_Gross2010")
  ! Gross and Sadowski 2002, Ind. Eng. Chem. Res. 41 (DeltaP,Deltav)=(0.5,0.52)
  type(pc_saft_data), parameter :: PCcx52 = pc_saft_data(eosPC_SAFT,"PENT1OL", &
       3.6260, 3.4508E-10, 247.28, 2252.1*Rgas, 0.010319, assoc_scheme_2B,"Tang_Gross2010")
  ! Gross and Sadowski 2002, Ind. Eng. Chem. Res. 41 (DeltaP,Deltav)=(0.58,0.71)
  type(pc_saft_data), parameter :: PCcx53 = pc_saft_data(eosPC_SAFT,"HEX1OL", &
       3.5146, 3.6735E-10, 262.32, 2538.9*Rgas, 0.005747, assoc_scheme_2B,"Tang_Gross2010")

  type(pc_saft_data), parameter :: PCcx54 = pc_saft_data(eosPeTS,"AR", &
       1.000, 3.42E-10, 136.0, 0.0, 0.0, no_assoc,"Default")

  integer, parameter :: nPCmodels = 54
  type(pc_saft_data), dimension(nPCmodels), parameter :: PCarray = (/ &
       PCcx1, &
       PCcx2, &
       PCcx3, &
       PCcx4, &
       PCcx5, &
       PCcx6, &
       PCcx7, &
       PCcx8, &
       PCcx9, &
       PCcx10, &
       PCcx11, &
       PCcx12, &
       PCcx13, &
       PCcx14, &
       PCcx15, &
       PCcx16, &
       PCcx17, &
       PCcx18, &
       PCcx19, &
       PCcx20, &
       PCcx21, &
       PCcx22, &
       PCcx23, &
       PCcx24, &
       PCcx25, &
       PCcx26, &
       PCcx27, &
       PCcx28, &
       PCcx29, &
       PCcx30, &
       PCcx31, &
       PCcx32, &
       PCcx33, &
       PCcx34, &
       PCcx35, &
       PCcx36, &
       PCcx37, &
       PCcx38, &
       PCcx39, &
       PCcx40, &
       PCcx41, &
       PCcx42, &
       PCcx43, &
       PCcx44, &
       PCcx45, &
       PCcx46, &
       PCcx47, &
       PCcx48, &
       PCcx49, &
       PCcx50, &
       PCcx51, &
       PCcx52, &
       PCcx53, &
       PCcx54/)

  !> TEMPERATURE-INDEPENDENT INTERACTION PARAMETERS FOR PC-SAFT DISPERSION TERM.
  ! ----------------------------------------------------------------------------
  type :: PCkijdata
     integer:: eosidx
     character (len=uid_len) :: uid1, uid2
     character(len=ref_len) :: ref
     real :: kijvalue
  end type PCkijdata

  ! All interaction parameters from the following reference:
  ! [2] Tang & Gross. (2010) "Modeling the phase equilibria of hydrogen sulfide
  !     and carbon dioxide in mixtures with hydrocarbons and water using the
  !     PCP-SAFT equation of state". Fluid Phase Equilibria 293.
  integer, parameter :: PCmaxkij = 31
  type(PCkijdata), dimension(PCmaxkij), parameter :: PCkijdb = (/ &
       PCkijdata(eosPC_SAFT,"H2S","C1","Default/Tang_Gross2010",0.0425), &
       PCkijdata(eosPC_SAFT,"H2S","C2","Default/Tang_Gross2010",0.072), &
       PCkijdata(eosPC_SAFT,"H2S","C3","Default/Tang_Gross2010",0.069), &
       PCkijdata(eosPC_SAFT,"H2S","NC4","Default/Tang_Gross2010",0.067), &
       PCkijdata(eosPC_SAFT,"H2S","NC5","Default/Tang_Gross2010",0.073), &
       PCkijdata(eosPC_SAFT,"H2S","NC6","Default/Tang_Gross2010",0.073), &
       PCkijdata(eosPC_SAFT,"H2S","NC7","Default/Tang_Gross2010",0.078), &
       PCkijdata(eosPC_SAFT,"H2S","NC9","Default/Tang_Gross2010",0.086), &
       PCkijdata(eosPC_SAFT,"H2S","NC10","Default/Tang_Gross2010",0.077), &
       PCkijdata(eosPC_SAFT,"H2S","IC4","Default/Tang_Gross2010",0.060), &
       PCkijdata(eosPC_SAFT,"H2S","IC5","Default/Tang_Gross2010",0.076), &
       PCkijdata(eosPC_SAFT,"H2S","CYCLOHEX","Default/Tang_Gross2010",0.082), &
       PCkijdata(eosPC_SAFT,"H2S","NC8","Default/Tang_Gross2010",0.0), &
       PCkijdata(eosPC_SAFT,"CO2","H2S","Default/Tang_Gross2010",0.0223), &
       PCkijdata(eosPC_SAFT,"CO2","BENZENE","Default/Tang_Gross2010",0.025), &
       PCkijdata(eosPC_SAFT,"CO2","TOLU","Default/Tang_Gross2010",0.026), &
       PCkijdata(eosPC_SAFT,"CO2","C1","Default/Gross_Sadowski2001",0.0650), & ! Value from Gross and Sadowski 2001.
       PCkijdata(eosPC_SAFT,"CO2","C2","Default/Tang_Gross2010",0.102), &
       PCkijdata(eosPC_SAFT,"CO2","C3","Default/Tang_Gross2010",0.0107), &
       PCkijdata(eosPC_SAFT,"CO2","NC4","Default/Tang_Gross2010",0.109), &
       PCkijdata(eosPC_SAFT,"CO2","NC5","Default/Tang_Gross2010",0.120), &
       PCkijdata(eosPC_SAFT,"CO2","NC6","Default/Tang_Gross2010",0.123), &
       PCkijdata(eosPC_SAFT,"CO2","NC7","Default/Tang_Gross2010",0.115), &
       PCkijdata(eosPC_SAFT,"CO2","NC8","Default/Tang_Gross2010",0.132), &
       PCkijdata(eosPC_SAFT,"CO2","NC9","Default/Tang_Gross2010",0.122), &
       PCkijdata(eosPC_SAFT,"CO2","NC10","Default/Tang_Gross2010",0.133), &
       PCkijdata(eosPC_SAFT,"CO2","IC4","Default/Tang_Gross2010",0.112), &
       PCkijdata(eosPC_SAFT,"CO2","IC5","Default/Tang_Gross2010",0.116), &
       PCkijdata(eosPC_SAFT,"CO2","CYCLOHEX","Default/Tang_Gross2010",0.125), &
       PCkijdata(eosPC_SAFT,"CO2","BENZENE","Default/Tang_Gross2010",0.087), &
       PCkijdata(eosPC_SAFT,"CO2","TOLU","Default/Tang_Gross2010",0.108) &
       /)

contains

  !> Checks that we use the correct gas constant when initializing the
  !> pc_saft_data parameters.
  logical function Rgas_is_correct()
    use thermopack_constants, only: Rgas_default
    Rgas_is_correct = (Rgas == Rgas_default)
  end function Rgas_is_correct

  !> Get the index in the PCarray of the component having uid given by
  !> compName. idx=0 if component isn't in database.
  function getPCdataIdx(eosidx,compName,param_ref) result(idx)
    use stringmod, only: str_eq, string_match
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: compName, param_ref
    integer :: idx, idx_default
    logical :: found

    if (.not. Rgas_is_correct()) call stoperror("Rgas_default must be default Rgas parameter.")

    found = .false.
    idx = 1
    idx_default = -1
    do while (idx <= nPCmodels)
      if ((eosidx==PCarray(idx)%eosidx) .and. &
           str_eq(compName, PCarray(idx)%compName)) then
        if (string_match(param_ref,PCarray(idx)%ref)) then
          found = .true.
          exit
        elseif (string_match("DEFAULT", PCarray(idx)%ref)) then
          idx_default = idx
        endif
      endif
      idx = idx + 1
    enddo

    if (.not. found) then
      if (verbose) then
        print *, "No CPA parameters for compName, ref ", compName, trim(param_ref)
      endif
      if (idx_default > 0) then
        idx = idx_default
        print *, "Using default parameter set for "//trim(compName)
      endif
    end if

    if (.not. found) then
      if (idx_default > 0) then
        idx = idx_default
        print *, "Using default parameter set for "//trim(compName)
      else
        print *, "ERROR FOR COMPONENT ", compname
        call stoperror("The PC-SAFT parameters don't exist.")
      endif
    end if

  end function getPCdataIdx

  !> Retrieve binary interaction parameter for components uid1 and uid2.
  !> If no kij is stored in the database PCkijdb, it returns 0.0.
  function getPCkij (eosidx,uid1,uid2,param_ref) result(kijvalue)
    use stringmod, only: str_eq, string_match
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: uid1, uid2, param_ref
    real :: kijvalue
    integer :: idx, idx_default
    logical :: match_11_22, match_12_21, found

    kijvalue = 0.0 ! default value if the binary is not in PCkijdb.
    idx = 1
    idx_default = -1
    found = .false.
    do idx = 1,PCmaxkij
       match_11_22 = str_eq(uid1,PCkijdb(idx)%uid1) .and. str_eq(uid2,PCkijdb(idx)%uid2)
       match_12_21 = str_eq(uid1,PCkijdb(idx)%uid2) .and. str_eq(uid2,PCkijdb(idx)%uid1)

       if ( eosidx==PCkijdb(idx)%eosidx .and. (match_11_22 .or. match_12_21)) then
         if (string_match(param_ref,PCkijdb(idx)%ref)) then
           kijvalue = PCkijdb(idx)%kijvalue
           found = .true.
           exit
         elseif (string_match("DEFAULT", PCkijdb(idx)%ref)) then
           idx_default = idx
         endif
       endif
     end do

     if (.not. found .and. idx_default > 0) then
       kijvalue = PCkijdb(idx_default)%kijvalue
     endif
  end function getPCkij

  subroutine getPcSaftKij_allComps(nc,comp,eosidx,kij)
    use compdata, only: gendata_pointer
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
          kij(ic,jc) = getPCkij(eosidx,comp(ic)%p_comp%ident,comp(jc)%p_comp%ident,"DEFAULT")
          kij(jc,ic) = kij(ic,jc)
       end do
    end do

  end subroutine getPcSaftKij_allComps

  subroutine getPcSaftPureParams_allComps(nc,comp,eosidx,param_ref,found,m,sigma,eps_depth_divk,eps,beta,scheme)
    use compdata, only: gendata_pointer
    ! Input
    integer, intent(in) :: nc
    type(gendata_pointer), intent(in) :: comp(nc)
    integer, intent(in) :: eosidx
    character(len=*), intent(in) :: param_ref
    ! Output
    logical, intent(out) :: found(nc)
    real, intent(out) :: m(nc), sigma(nc), eps_depth_divk(nc), eps(nc), beta(nc)
    integer, intent(out) :: scheme(nc)
    ! Locals
    integer :: ic

    do ic=1,nc
       call getPcSaftpureParams_singleComp(comp(ic)%p_comp%ident,eosidx,param_ref,found(ic),m(ic),&
            sigma(ic),eps_depth_divk(ic),eps(ic),beta(ic),scheme(ic))
    end do

  end subroutine getPcSaftPureParams_allComps

  subroutine getPcSaftpureParams_singleComp(compName,eosidx,param_ref,found,m,sigma,eps_depth_divk,eps,beta,scheme)
    ! Input
    character(len=*), intent(in) :: compName, param_ref
    integer, intent(in) :: eosidx
    ! Output
    logical, intent(out) :: found
    real, intent(out) :: m, sigma, eps_depth_divk, eps, beta
    integer, intent(out) :: scheme
    ! Locals
    integer :: idx

    idx = getPCdataIdx(eosidx,compName,param_ref)
    if ( idx == 0 ) then
       found = .false.
       return
    end if

    found = .true.
    m = PCarray(idx)%m
    scheme = PCarray(idx)%assoc_scheme
    sigma = PCarray(idx)%sigma
    eps_depth_divk = PCarray(idx)%eps_depth_divk
    eps = PCarray(idx)%eps
    beta = PCarray(idx)%beta
    scheme = PCarray(idx)%assoc_scheme
  end subroutine getPcSaftpureParams_singleComp


end module pc_saft_parameters
