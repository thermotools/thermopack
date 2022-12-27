!> Automatically generated file gergmixdb.f90
!! Time stamp: 2022-10-03T21:42:11.071422

module gergmixdb
  implicit none
  public

  type :: gerg_mix_reducing
    character(len=8) :: ident1 !< The component ID
    character(len=8) :: ident2 !< The component ID
    real :: beta_v !< Reducing density parameter
    real :: gamma_v !< Reducing density parameter
    real :: beta_T !< Reducing temperature parameter
    real :: gamma_T !< Reducing temperature parameter
  end type gerg_mix_reducing

  type :: gerg_mix_data
    character(len=8) :: ident1 !< The component ID
    character(len=8) :: ident2 !< The component ID
    real :: Fij !< Departure function parameter
    integer :: num_mix !< Number of parameters
    real :: n_mix(12) !<
    real :: t_mix(12) !<
    integer :: d_mix(12) !<
    real :: eta_mix(12) !<
    real :: gamma_mix(12) !<
    real :: epsilon_mix(12) !<
    real :: beta_mix(12) !<
    integer :: num_exp !< Number of exponential terms
  end type gerg_mix_data

  type(gerg_mix_reducing), parameter :: gerg_red_1 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "H2", &
      beta_v = 0.904142159, &
      gamma_v = 1.15279255, &
      beta_T = 0.942320195, &
      gamma_T = 1.782924792)

  type(gerg_mix_reducing), parameter :: gerg_red_2 = &
      gerg_mix_reducing(ident1 = "NC10", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_3 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "H2S", &
      beta_v = 1.012599087, &
      gamma_v = 1.040161207, &
      beta_T = 1.011090031, &
      gamma_T = 0.961155729)

  type(gerg_mix_reducing), parameter :: gerg_red_4 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.195952177, &
      beta_T = 1., &
      gamma_T = 1.472607971)

  type(gerg_mix_reducing), parameter :: gerg_red_5 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "C3", &
      beta_v = 0.997607277, &
      gamma_v = 1.00303472, &
      beta_T = 0.996199694, &
      gamma_T = 1.01473019)

  type(gerg_mix_reducing), parameter :: gerg_red_6 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 0.973386152, &
      beta_T = 1.00768862, &
      gamma_T = 1.140671202)

  type(gerg_mix_reducing), parameter :: gerg_red_7 = &
      gerg_mix_reducing(ident1 = "NC7", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.006767176, &
      beta_T = 1., &
      gamma_T = 0.998793111)

  type(gerg_mix_reducing), parameter :: gerg_red_8 = &
      gerg_mix_reducing(ident1 = "NC6", &
      ident2 = "NC10", &
      beta_v = 1.001516371, &
      gamma_v = 1.013511439, &
      beta_T = 0.99764101, &
      gamma_T = 1.028939539)

  type(gerg_mix_reducing), parameter :: gerg_red_9 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.019174227, &
      beta_T = 1., &
      gamma_T = 1.021283378)

  type(gerg_mix_reducing), parameter :: gerg_red_10 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "NC10", &
      beta_v = 1.000151132, &
      gamma_v = 1.183394668, &
      beta_T = 1.02002879, &
      gamma_T = 1.145512213)

  type(gerg_mix_reducing), parameter :: gerg_red_11 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "IC4", &
      beta_v = 1., &
      gamma_v = 1.006616886, &
      beta_T = 1., &
      gamma_T = 1.033283811)

  type(gerg_mix_reducing), parameter :: gerg_red_12 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.169701102, &
      beta_T = 1., &
      gamma_T = 1.092177796)

  type(gerg_mix_reducing), parameter :: gerg_red_13 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1.002284353, &
      beta_T = 1., &
      gamma_T = 1.001835788)

  type(gerg_mix_reducing), parameter :: gerg_red_14 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "NC10", &
      beta_v = 0.984104227, &
      gamma_v = 1.053040574, &
      beta_T = 0.985331233, &
      gamma_T = 1.140905252)

  type(gerg_mix_reducing), parameter :: gerg_red_15 = &
      gerg_mix_reducing(ident1 = "H2", &
      ident2 = "H2S", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_16 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.95)

  type(gerg_mix_reducing), parameter :: gerg_red_17 = &
      gerg_mix_reducing(ident1 = "H2", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.121416201, &
      beta_T = 1., &
      gamma_T = 1.377504607)

  type(gerg_mix_reducing), parameter :: gerg_red_18 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "NC4", &
      beta_v = 0.979105972, &
      gamma_v = 1.045375122, &
      beta_T = 0.99417491, &
      gamma_T = 1.171607691)

  type(gerg_mix_reducing), parameter :: gerg_red_19 = &
      gerg_mix_reducing(ident1 = "CO", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_20 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.060243344, &
      beta_T = 1., &
      gamma_T = 1.021624748)

  type(gerg_mix_reducing), parameter :: gerg_red_21 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "N2", &
      beta_v = 0.998721377, &
      gamma_v = 1.013950311, &
      beta_T = 0.99809883, &
      gamma_T = 0.979273013)

  type(gerg_mix_reducing), parameter :: gerg_red_22 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 0.851343711, &
      beta_T = 1., &
      gamma_T = 1.038675574)

  type(gerg_mix_reducing), parameter :: gerg_red_23 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "IC4", &
      beta_v = 1.000880464, &
      gamma_v = 1.00041444, &
      beta_T = 1.000077547, &
      gamma_T = 1.001432824)

  type(gerg_mix_reducing), parameter :: gerg_red_24 = &
      gerg_mix_reducing(ident1 = "NC6", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.006268954, &
      beta_T = 1., &
      gamma_T = 1.001633952)

  type(gerg_mix_reducing), parameter :: gerg_red_25 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.057666085, &
      beta_T = 1., &
      gamma_T = 1.134532014)

  type(gerg_mix_reducing), parameter :: gerg_red_26 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_27 = &
      gerg_mix_reducing(ident1 = "NC10", &
      ident2 = "H2S", &
      beta_v = 0.975187766, &
      gamma_v = 1.171714677, &
      beta_T = 0.973091413, &
      gamma_T = 1.103693489)

  type(gerg_mix_reducing), parameter :: gerg_red_28 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.002480637, &
      beta_T = 1., &
      gamma_T = 1.000761237)

  type(gerg_mix_reducing), parameter :: gerg_red_29 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1.011759763, &
      beta_T = 1., &
      gamma_T = 0.600340961)

  type(gerg_mix_reducing), parameter :: gerg_red_30 = &
      gerg_mix_reducing(ident1 = "NC6", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1.170217596, &
      beta_T = 1., &
      gamma_T = 0.569681333)

  type(gerg_mix_reducing), parameter :: gerg_red_31 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1.002728434, &
      beta_T = 1., &
      gamma_T = 1.000792201)

  type(gerg_mix_reducing), parameter :: gerg_red_32 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1.223638763, &
      beta_T = 1., &
      gamma_T = 0.615512682)

  type(gerg_mix_reducing), parameter :: gerg_red_33 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "C2", &
      beta_v = 0.978880168, &
      gamma_v = 1.042352891, &
      beta_T = 1.007671428, &
      gamma_T = 1.098650964)

  type(gerg_mix_reducing), parameter :: gerg_red_34 = &
      gerg_mix_reducing(ident1 = "IC5", &
      ident2 = "H2S", &
      beta_v = 1., &
      gamma_v = 0.835763343, &
      beta_T = 1., &
      gamma_T = 0.982651529)

  type(gerg_mix_reducing), parameter :: gerg_red_35 = &
      gerg_mix_reducing(ident1 = "NC10", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 0.551405318, &
      beta_T = 0.897162268, &
      gamma_T = 0.740416402)

  type(gerg_mix_reducing), parameter :: gerg_red_36 = &
      gerg_mix_reducing(ident1 = "NC6", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.001508227, &
      beta_T = 1., &
      gamma_T = 0.999762786)

  type(gerg_mix_reducing), parameter :: gerg_red_37 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.021668316, &
      beta_T = 1., &
      gamma_T = 1.00988576)

  type(gerg_mix_reducing), parameter :: gerg_red_38 = &
      gerg_mix_reducing(ident1 = "NC9", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_39 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "NC8", &
      beta_v = 0.994740603, &
      gamma_v = 1.116549372, &
      beta_T = 0.957473785, &
      gamma_T = 1.449245409)

  type(gerg_mix_reducing), parameter :: gerg_red_40 = &
      gerg_mix_reducing(ident1 = "NC9", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_41 = &
      gerg_mix_reducing(ident1 = "NC9", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.342647661, &
      beta_T = 1., &
      gamma_T = 2.23435404)

  type(gerg_mix_reducing), parameter :: gerg_red_42 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.084740904, &
      beta_T = 1., &
      gamma_T = 1.173916162)

  type(gerg_mix_reducing), parameter :: gerg_red_43 = &
      gerg_mix_reducing(ident1 = "NC9", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_44 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.046905515, &
      beta_T = 1., &
      gamma_T = 1.033180106)

  type(gerg_mix_reducing), parameter :: gerg_red_45 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "NC4", &
      beta_v = 0.999157205, &
      gamma_v = 1.006179146, &
      beta_T = 0.999130554, &
      gamma_T = 1.034832749)

  type(gerg_mix_reducing), parameter :: gerg_red_46 = &
      gerg_mix_reducing(ident1 = "NC10", &
      ident2 = "H2", &
      beta_v = 1.695358382, &
      gamma_v = 1.120233729, &
      beta_T = 1.064818089, &
      gamma_T = 3.786003724)

  type(gerg_mix_reducing), parameter :: gerg_red_47 = &
      gerg_mix_reducing(ident1 = "NC7", &
      ident2 = "H2S", &
      beta_v = 0.828967164, &
      gamma_v = 1.087956749, &
      beta_T = 0.988937417, &
      gamma_T = 1.013453092)

  type(gerg_mix_reducing), parameter :: gerg_red_48 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "H2S", &
      beta_v = 1.012994431, &
      gamma_v = 0.988591117, &
      beta_T = 0.974550548, &
      gamma_T = 0.937130844)

  type(gerg_mix_reducing), parameter :: gerg_red_49 = &
      gerg_mix_reducing(ident1 = "NC7", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_50 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "NC5", &
      beta_v = 1., &
      gamma_v = 1.078877166, &
      beta_T = 1., &
      gamma_T = 1.419029041)

  type(gerg_mix_reducing), parameter :: gerg_red_51 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.008972412, &
      beta_T = 1., &
      gamma_T = 1.002441051)

  type(gerg_mix_reducing), parameter :: gerg_red_52 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "H2S", &
      beta_v = 0.906630564, &
      gamma_v = 1.024085837, &
      beta_T = 1.016034583, &
      gamma_T = 0.92601888)

  type(gerg_mix_reducing), parameter :: gerg_red_53 = &
      gerg_mix_reducing(ident1 = "NC6", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.243461678, &
      beta_T = 1., &
      gamma_T = 3.021197546)

  type(gerg_mix_reducing), parameter :: gerg_red_54 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_55 = &
      gerg_mix_reducing(ident1 = "NC8", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.001357085, &
      beta_T = 1., &
      gamma_T = 1.000235044)

  type(gerg_mix_reducing), parameter :: gerg_red_56 = &
      gerg_mix_reducing(ident1 = "HE", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_57 = &
      gerg_mix_reducing(ident1 = "CO", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_58 = &
      gerg_mix_reducing(ident1 = "NC9", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.252151449, &
      beta_T = 1., &
      gamma_T = 1.294070556)

  type(gerg_mix_reducing), parameter :: gerg_red_59 = &
      gerg_mix_reducing(ident1 = "O2", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_60 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "AR", &
      beta_v = 1.008392428, &
      gamma_v = 1.029205465, &
      beta_T = 0.996512863, &
      gamma_T = 1.050971635)

  type(gerg_mix_reducing), parameter :: gerg_red_61 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.957934447, &
      gamma_T = 1.822157123)

  type(gerg_mix_reducing), parameter :: gerg_red_62 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "CO2", &
      beta_v = 0.977794634, &
      gamma_v = 1.047578256, &
      beta_T = 1.005894529, &
      gamma_T = 1.107654104)

  type(gerg_mix_reducing), parameter :: gerg_red_63 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "NC5", &
      beta_v = 0.993851009, &
      gamma_v = 1.026085655, &
      beta_T = 0.998688946, &
      gamma_T = 1.066665676)

  type(gerg_mix_reducing), parameter :: gerg_red_64 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "NC7", &
      beta_v = 0.962050831, &
      gamma_v = 1.156655935, &
      beta_T = 0.977431529, &
      gamma_T = 1.379850328)

  type(gerg_mix_reducing), parameter :: gerg_red_65 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "AR", &
      beta_v = 1.034630259, &
      gamma_v = 1.014678542, &
      beta_T = 0.990954281, &
      gamma_T = 0.989843388)

  type(gerg_mix_reducing), parameter :: gerg_red_66 = &
      gerg_mix_reducing(ident1 = "H2O", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1.038993495, &
      beta_T = 1., &
      gamma_T = 1.070941866)

  type(gerg_mix_reducing), parameter :: gerg_red_67 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.07400611, &
      beta_T = 1., &
      gamma_T = 2.308215191)

  type(gerg_mix_reducing), parameter :: gerg_red_68 = &
      gerg_mix_reducing(ident1 = "NC8", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.219206702, &
      beta_T = 1., &
      gamma_T = 1.276565536)

  type(gerg_mix_reducing), parameter :: gerg_red_69 = &
      gerg_mix_reducing(ident1 = "NC10", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_70 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "NC5", &
      beta_v = 1.024311498, &
      gamma_v = 1.068406078, &
      beta_T = 1.027000795, &
      gamma_T = 0.979217302)

  type(gerg_mix_reducing), parameter :: gerg_red_71 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.188334783, &
      beta_T = 1., &
      gamma_T = 2.013859174)

  type(gerg_mix_reducing), parameter :: gerg_red_72 = &
      gerg_mix_reducing(ident1 = "IC5", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.028994325, &
      beta_T = 1., &
      gamma_T = 1.008191499)

  type(gerg_mix_reducing), parameter :: gerg_red_73 = &
      gerg_mix_reducing(ident1 = "NC7", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.002972346, &
      beta_T = 1., &
      gamma_T = 1.002229938)

  type(gerg_mix_reducing), parameter :: gerg_red_74 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_75 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "H2S", &
      beta_v = 0.936811219, &
      gamma_v = 1.010593999, &
      beta_T = 0.992573556, &
      gamma_T = 0.905829247)

  type(gerg_mix_reducing), parameter :: gerg_red_76 = &
      gerg_mix_reducing(ident1 = "NC7", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.190354273, &
      beta_T = 1., &
      gamma_T = 1.256123503)

  type(gerg_mix_reducing), parameter :: gerg_red_77 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "NC5", &
      beta_v = 1., &
      gamma_v = 1.01815965, &
      beta_T = 1., &
      gamma_T = 1.00214364)

  type(gerg_mix_reducing), parameter :: gerg_red_78 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "O2", &
      beta_v = 0.99952177, &
      gamma_v = 0.997082328, &
      beta_T = 0.997190589, &
      gamma_T = 0.995157044)

  type(gerg_mix_reducing), parameter :: gerg_red_79 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.199769134, &
      beta_T = 1., &
      gamma_T = 1.109973833)

  type(gerg_mix_reducing), parameter :: gerg_red_80 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "IC4", &
      beta_v = 0.98641583, &
      gamma_v = 1.100576129, &
      beta_T = 0.99286813, &
      gamma_T = 1.284462634)

  type(gerg_mix_reducing), parameter :: gerg_red_81 = &
      gerg_mix_reducing(ident1 = "H2O", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_82 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.232939523, &
      beta_T = 1., &
      gamma_T = 2.509259945)

  type(gerg_mix_reducing), parameter :: gerg_red_83 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "H2S", &
      beta_v = 0.910394249, &
      gamma_v = 1.256844157, &
      beta_T = 1.004692366, &
      gamma_T = 0.9601742)

  type(gerg_mix_reducing), parameter :: gerg_red_84 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1.045439935, &
      beta_T = 1., &
      gamma_T = 1.021150247)

  type(gerg_mix_reducing), parameter :: gerg_red_85 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "H2O", &
      beta_v = 1.012783169, &
      gamma_v = 1.585018334, &
      beta_T = 1.063333913, &
      gamma_T = 0.775810513)

  type(gerg_mix_reducing), parameter :: gerg_red_86 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.016370338, &
      beta_T = 1., &
      gamma_T = 1.049035838)

  type(gerg_mix_reducing), parameter :: gerg_red_87 = &
      gerg_mix_reducing(ident1 = "O2", &
      ident2 = "AR", &
      beta_v = 0.999746847, &
      gamma_v = 0.993907223, &
      beta_T = 1.000023103, &
      gamma_T = 0.990430423)

  type(gerg_mix_reducing), parameter :: gerg_red_88 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "NC5", &
      beta_v = 1.044919431, &
      gamma_v = 1.019921513, &
      beta_T = 0.996484021, &
      gamma_T = 1.008344412)

  type(gerg_mix_reducing), parameter :: gerg_red_89 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.40455409, &
      beta_T = 1., &
      gamma_T = 1.520975334)

  type(gerg_mix_reducing), parameter :: gerg_red_90 = &
      gerg_mix_reducing(ident1 = "IC5", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.184340443, &
      beta_T = 1., &
      gamma_T = 1.996386669)

  type(gerg_mix_reducing), parameter :: gerg_red_91 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.186067025, &
      beta_T = 1., &
      gamma_T = 1.733280051)

  type(gerg_mix_reducing), parameter :: gerg_red_92 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1.343685343, &
      beta_T = 1., &
      gamma_T = 1.188899743)

  type(gerg_mix_reducing), parameter :: gerg_red_93 = &
      gerg_mix_reducing(ident1 = "NC10", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 0.87018496, &
      beta_T = 1.049594632, &
      gamma_T = 1.803567587)

  type(gerg_mix_reducing), parameter :: gerg_red_94 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1.000024335, &
      beta_T = 1., &
      gamma_T = 1.000050537)

  type(gerg_mix_reducing), parameter :: gerg_red_95 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "C3", &
      beta_v = 1.00482707, &
      gamma_v = 1.038470657, &
      beta_T = 0.989680305, &
      gamma_T = 1.098655531)

  type(gerg_mix_reducing), parameter :: gerg_red_96 = &
      gerg_mix_reducing(ident1 = "NC7", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.159131722, &
      beta_T = 1., &
      gamma_T = 3.169143057)

  type(gerg_mix_reducing), parameter :: gerg_red_97 = &
      gerg_mix_reducing(ident1 = "NC6", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_98 = &
      gerg_mix_reducing(ident1 = "NC6", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_99 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "NC8", &
      beta_v = 1.026169373, &
      gamma_v = 1.104043935, &
      beta_T = 1.02969078, &
      gamma_T = 1.074455386)

  type(gerg_mix_reducing), parameter :: gerg_red_100 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "NC7", &
      beta_v = 1.205469976, &
      gamma_v = 1.164585914, &
      beta_T = 1.011806317, &
      gamma_T = 1.046169823)

  type(gerg_mix_reducing), parameter :: gerg_red_101 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "AR", &
      beta_v = 1.004166412, &
      gamma_v = 1.002212182, &
      beta_T = 0.999069843, &
      gamma_T = 0.990034831)

  type(gerg_mix_reducing), parameter :: gerg_red_102 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "H2", &
      beta_v = 0.925367171, &
      gamma_v = 1.10607204, &
      beta_T = 0.932969831, &
      gamma_T = 1.902008495)

  type(gerg_mix_reducing), parameter :: gerg_red_103 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "NC5", &
      beta_v = 0.94833012, &
      gamma_v = 1.124508039, &
      beta_T = 0.992127525, &
      gamma_T = 1.249173968)

  type(gerg_mix_reducing), parameter :: gerg_red_104 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.032807063, &
      beta_T = 1., &
      gamma_T = 1.013945424)

  type(gerg_mix_reducing), parameter :: gerg_red_105 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.047298475, &
      beta_T = 1., &
      gamma_T = 1.017817492)

  type(gerg_mix_reducing), parameter :: gerg_red_106 = &
      gerg_mix_reducing(ident1 = "H2O", &
      ident2 = "H2S", &
      beta_v = 1., &
      gamma_v = 1.014832832, &
      beta_T = 1., &
      gamma_T = 0.940587083)

  type(gerg_mix_reducing), parameter :: gerg_red_107 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.008690943, &
      beta_T = 1., &
      gamma_T = 0.993425388)

  type(gerg_mix_reducing), parameter :: gerg_red_108 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_109 = &
      gerg_mix_reducing(ident1 = "IC5", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_110 = &
      gerg_mix_reducing(ident1 = "IC5", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_111 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1.214638734, &
      beta_T = 1., &
      gamma_T = 1.245039498)

  type(gerg_mix_reducing), parameter :: gerg_red_112 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "NC10", &
      beta_v = 1.033086292, &
      gamma_v = 1.146089637, &
      beta_T = 0.937777823, &
      gamma_T = 1.568231489)

  type(gerg_mix_reducing), parameter :: gerg_red_113 = &
      gerg_mix_reducing(ident1 = "IC5", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_114 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 0.881405683, &
      beta_T = 1., &
      gamma_T = 3.159776855)

  type(gerg_mix_reducing), parameter :: gerg_red_115 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_116 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.147595688, &
      beta_T = 1., &
      gamma_T = 1.895305393)

  type(gerg_mix_reducing), parameter :: gerg_red_117 = &
      gerg_mix_reducing(ident1 = "IC5", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.039372957, &
      beta_T = 1., &
      gamma_T = 1.010825138)

  type(gerg_mix_reducing), parameter :: gerg_red_118 = &
      gerg_mix_reducing(ident1 = "O2", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1.143174289, &
      beta_T = 1., &
      gamma_T = 0.964767932)

  type(gerg_mix_reducing), parameter :: gerg_red_119 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.14353473, &
      beta_T = 1., &
      gamma_T = 1.05603303)

  type(gerg_mix_reducing), parameter :: gerg_red_120 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "NC10", &
      beta_v = 0.976951968, &
      gamma_v = 1.027845529, &
      beta_T = 0.993688386, &
      gamma_T = 1.076466918)

  type(gerg_mix_reducing), parameter :: gerg_red_121 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 0.95667731, &
      beta_T = 1., &
      gamma_T = 0.447666011)

  type(gerg_mix_reducing), parameter :: gerg_red_122 = &
      gerg_mix_reducing(ident1 = "O2", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_123 = &
      gerg_mix_reducing(ident1 = "H2", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_124 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_125 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_126 = &
      gerg_mix_reducing(ident1 = "NC9", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_127 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.119954454, &
      beta_T = 1., &
      gamma_T = 1.206043295)

  type(gerg_mix_reducing), parameter :: gerg_red_128 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.102764612, &
      beta_T = 1., &
      gamma_T = 1.063694129)

  type(gerg_mix_reducing), parameter :: gerg_red_129 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.034910633, &
      beta_T = 1., &
      gamma_T = 1.103421755)

  type(gerg_mix_reducing), parameter :: gerg_red_130 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.034995284, &
      beta_T = 1., &
      gamma_T = 1.00915706)

  type(gerg_mix_reducing), parameter :: gerg_red_131 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_132 = &
      gerg_mix_reducing(ident1 = "NC10", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_133 = &
      gerg_mix_reducing(ident1 = "H2", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_134 = &
      gerg_mix_reducing(ident1 = "IC5", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.017880545, &
      beta_T = 1., &
      gamma_T = 1.00564748)

  type(gerg_mix_reducing), parameter :: gerg_red_135 = &
      gerg_mix_reducing(ident1 = "H2", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_136 = &
      gerg_mix_reducing(ident1 = "H2S", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_137 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "H2S", &
      beta_v = 1.010817909, &
      gamma_v = 1.030988277, &
      beta_T = 0.990197354, &
      gamma_T = 0.90273666)

  type(gerg_mix_reducing), parameter :: gerg_red_138 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_139 = &
      gerg_mix_reducing(ident1 = "NC6", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.155145836, &
      beta_T = 1., &
      gamma_T = 1.233272781)

  type(gerg_mix_reducing), parameter :: gerg_red_140 = &
      gerg_mix_reducing(ident1 = "NC6", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.02076168, &
      beta_T = 1., &
      gamma_T = 1.055369591)

  type(gerg_mix_reducing), parameter :: gerg_red_141 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "C2", &
      beta_v = 0.997547866, &
      gamma_v = 1.006617867, &
      beta_T = 0.996336508, &
      gamma_T = 1.049707697)

  type(gerg_mix_reducing), parameter :: gerg_red_142 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.049219137, &
      beta_T = 1., &
      gamma_T = 1.014096448)

  type(gerg_mix_reducing), parameter :: gerg_red_143 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "NC9", &
      beta_v = 1.002852287, &
      gamma_v = 1.141895355, &
      beta_T = 0.947716769, &
      gamma_T = 1.528532478)

  type(gerg_mix_reducing), parameter :: gerg_red_144 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "NC6", &
      beta_v = 0.958015294, &
      gamma_v = 1.052643846, &
      beta_T = 0.981844797, &
      gamma_T = 1.330570181)

  type(gerg_mix_reducing), parameter :: gerg_red_145 = &
      gerg_mix_reducing(ident1 = "NC9", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.00081052, &
      beta_T = 1., &
      gamma_T = 1.000182392)

  type(gerg_mix_reducing), parameter :: gerg_red_146 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "H2S", &
      beta_v = 0.908113163, &
      gamma_v = 1.033366041, &
      beta_T = 0.985962886, &
      gamma_T = 0.926156602)

  type(gerg_mix_reducing), parameter :: gerg_red_147 = &
      gerg_mix_reducing(ident1 = "O2", &
      ident2 = "H2S", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_148 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_149 = &
      gerg_mix_reducing(ident1 = "NC8", &
      ident2 = "H2S", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_150 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_151 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_152 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1.094749685, &
      beta_T = 1., &
      gamma_T = 0.968808467)

  type(gerg_mix_reducing), parameter :: gerg_red_153 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "NC10", &
      beta_v = 0.995676258, &
      gamma_v = 1.098361281, &
      beta_T = 0.970918061, &
      gamma_T = 1.237191558)

  type(gerg_mix_reducing), parameter :: gerg_red_154 = &
      gerg_mix_reducing(ident1 = "NC6", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_155 = &
      gerg_mix_reducing(ident1 = "NC9", &
      ident2 = "H2S", &
      beta_v = 1., &
      gamma_v = 1.082905109, &
      beta_T = 1., &
      gamma_T = 1.086557826)

  type(gerg_mix_reducing), parameter :: gerg_red_156 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "IC5", &
      beta_v = 1.040459289, &
      gamma_v = 0.999432118, &
      beta_T = 0.994364425, &
      gamma_T = 1.0032695)

  type(gerg_mix_reducing), parameter :: gerg_red_157 = &
      gerg_mix_reducing(ident1 = "CO", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1.159720623, &
      beta_T = 1., &
      gamma_T = 0.954215746)

  type(gerg_mix_reducing), parameter :: gerg_red_158 = &
      gerg_mix_reducing(ident1 = "NC6", &
      ident2 = "H2S", &
      beta_v = 0.754473958, &
      gamma_v = 1.339283552, &
      beta_T = 0.985891113, &
      gamma_T = 0.956075596)

  type(gerg_mix_reducing), parameter :: gerg_red_159 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "NC8", &
      beta_v = 1.007469726, &
      gamma_v = 1.071917985, &
      beta_T = 0.984068272, &
      gamma_T = 1.168636194)

  type(gerg_mix_reducing), parameter :: gerg_red_160 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.079648053, &
      beta_T = 1., &
      gamma_T = 1.050044169)

  type(gerg_mix_reducing), parameter :: gerg_red_161 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.010493989, &
      beta_T = 1., &
      gamma_T = 1.006018054)

  type(gerg_mix_reducing), parameter :: gerg_red_162 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "HE", &
      beta_v = 0.846647561, &
      gamma_v = 0.864141549, &
      beta_T = 0.76837763, &
      gamma_T = 3.207456948)

  type(gerg_mix_reducing), parameter :: gerg_red_163 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.087272232, &
      beta_T = 1., &
      gamma_T = 1.161390082)

  type(gerg_mix_reducing), parameter :: gerg_red_164 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.100405929, &
      beta_T = 0.95637945, &
      gamma_T = 1.749119996)

  type(gerg_mix_reducing), parameter :: gerg_red_165 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "IC4", &
      beta_v = 0.999243146, &
      gamma_v = 1.001156119, &
      beta_T = 0.998012298, &
      gamma_T = 1.005250774)

  type(gerg_mix_reducing), parameter :: gerg_red_166 = &
      gerg_mix_reducing(ident1 = "IC5", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.116694577, &
      beta_T = 1., &
      gamma_T = 1.199326059)

  type(gerg_mix_reducing), parameter :: gerg_red_167 = &
      gerg_mix_reducing(ident1 = "NC7", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_168 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_169 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.057872566, &
      beta_T = 1., &
      gamma_T = 1.025657518)

  type(gerg_mix_reducing), parameter :: gerg_red_170 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "HE", &
      beta_v = 0.969501055, &
      gamma_v = 0.932629867, &
      beta_T = 0.692868765, &
      gamma_T = 1.47183158)

  type(gerg_mix_reducing), parameter :: gerg_red_171 = &
      gerg_mix_reducing(ident1 = "NC8", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 0.599484191, &
      beta_T = 1., &
      gamma_T = 0.662072469)

  type(gerg_mix_reducing), parameter :: gerg_red_172 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "NC5", &
      beta_v = 1., &
      gamma_v = 1.002779804, &
      beta_T = 1., &
      gamma_T = 1.002495889)

  type(gerg_mix_reducing), parameter :: gerg_red_173 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.018702573, &
      beta_T = 1., &
      gamma_T = 1.352643115)

  type(gerg_mix_reducing), parameter :: gerg_red_174 = &
      gerg_mix_reducing(ident1 = "NC4", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_175 = &
      gerg_mix_reducing(ident1 = "H2", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_176 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "C3", &
      beta_v = 0.974424681, &
      gamma_v = 1.081025408, &
      beta_T = 1.002677329, &
      gamma_T = 1.201264026)

  type(gerg_mix_reducing), parameter :: gerg_red_177 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "IC4", &
      beta_v = 1.076551882, &
      gamma_v = 1.081909003, &
      beta_T = 1.023339824, &
      gamma_T = 0.929982936)

  type(gerg_mix_reducing), parameter :: gerg_red_178 = &
      gerg_mix_reducing(ident1 = "NC8", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_179 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.108143673, &
      beta_T = 1., &
      gamma_T = 1.197564208)

  type(gerg_mix_reducing), parameter :: gerg_red_180 = &
      gerg_mix_reducing(ident1 = "NC7", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.001370076, &
      beta_T = 1., &
      gamma_T = 1.001150096)

  type(gerg_mix_reducing), parameter :: gerg_red_181 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_182 = &
      gerg_mix_reducing(ident1 = "IC5", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.002995876, &
      beta_T = 1., &
      gamma_T = 1.001204174)

  type(gerg_mix_reducing), parameter :: gerg_red_183 = &
      gerg_mix_reducing(ident1 = "C3", &
      ident2 = "NC4", &
      beta_v = 0.999795868, &
      gamma_v = 1.003264179, &
      beta_T = 1.000310289, &
      gamma_T = 1.007392782)

  type(gerg_mix_reducing), parameter :: gerg_red_184 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "NC4", &
      beta_v = 0.99608261, &
      gamma_v = 1.146949309, &
      beta_T = 0.994515234, &
      gamma_T = 1.304886838)

  type(gerg_mix_reducing), parameter :: gerg_red_185 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "H2", &
      beta_v = 0.972532065, &
      gamma_v = 0.970115357, &
      beta_T = 0.946134337, &
      gamma_T = 1.175696583)

  type(gerg_mix_reducing), parameter :: gerg_red_186 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "IC4", &
      beta_v = 1.011240388, &
      gamma_v = 1.054319053, &
      beta_T = 0.980315756, &
      gamma_T = 1.161117729)

  type(gerg_mix_reducing), parameter :: gerg_red_187 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "C3", &
      beta_v = 0.996898004, &
      gamma_v = 1.047596298, &
      beta_T = 1.033620538, &
      gamma_T = 0.908772477)

  type(gerg_mix_reducing), parameter :: gerg_red_188 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_189 = &
      gerg_mix_reducing(ident1 = "NC8", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_190 = &
      gerg_mix_reducing(ident1 = "NC7", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_191 = &
      gerg_mix_reducing(ident1 = "IC5", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_192 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "CO2", &
      beta_v = 0.999518072, &
      gamma_v = 1.002806594, &
      beta_T = 1.02262449, &
      gamma_T = 0.975665369)

  type(gerg_mix_reducing), parameter :: gerg_red_193 = &
      gerg_mix_reducing(ident1 = "NC7", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_194 = &
      gerg_mix_reducing(ident1 = "NC8", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.002553544, &
      beta_T = 1., &
      gamma_T = 1.007186267)

  type(gerg_mix_reducing), parameter :: gerg_red_195 = &
      gerg_mix_reducing(ident1 = "H2S", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_196 = &
      gerg_mix_reducing(ident1 = "NC8", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.305249405, &
      beta_T = 1., &
      gamma_T = 2.191555216)

  type(gerg_mix_reducing), parameter :: gerg_red_197 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "C2", &
      beta_v = 1.002525718, &
      gamma_v = 1.032876701, &
      beta_T = 1.013871147, &
      gamma_T = 0.90094953)

  type(gerg_mix_reducing), parameter :: gerg_red_198 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_199 = &
      gerg_mix_reducing(ident1 = "IC5", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.009928206, &
      beta_T = 1., &
      gamma_T = 1.003194615)

  type(gerg_mix_reducing), parameter :: gerg_red_200 = &
      gerg_mix_reducing(ident1 = "N2", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1.154135439, &
      beta_T = 1., &
      gamma_T = 1.38177077)

  type(gerg_mix_reducing), parameter :: gerg_red_201 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.069223964, &
      beta_T = 1., &
      gamma_T = 1.016422347)

  type(gerg_mix_reducing), parameter :: gerg_red_202 = &
      gerg_mix_reducing(ident1 = "C1", &
      ident2 = "CO", &
      beta_v = 0.997340772, &
      gamma_v = 1.006102927, &
      beta_T = 0.987411732, &
      gamma_T = 0.987473033)

  type(gerg_mix_reducing), parameter :: gerg_red_203 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "H2O", &
      beta_v = 0.949055959, &
      gamma_v = 1.542328793, &
      beta_T = 0.997372205, &
      gamma_T = 0.775453996)

  type(gerg_mix_reducing), parameter :: gerg_red_204 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "IC5", &
      beta_v = 1.060793104, &
      gamma_v = 1.116793198, &
      beta_T = 1.019180957, &
      gamma_T = 0.961218039)

  type(gerg_mix_reducing), parameter :: gerg_red_205 = &
      gerg_mix_reducing(ident1 = "NC5", &
      ident2 = "H2S", &
      beta_v = 0.984613203, &
      gamma_v = 1.076539234, &
      beta_T = 0.962006651, &
      gamma_T = 0.959065662)

  type(gerg_mix_reducing), parameter :: gerg_red_206 = &
      gerg_mix_reducing(ident1 = "CO", &
      ident2 = "H2S", &
      beta_v = 0.795660392, &
      gamma_v = 1.101731308, &
      beta_T = 1.025536736, &
      gamma_T = 1.022749748)

  type(gerg_mix_reducing), parameter :: gerg_red_207 = &
      gerg_mix_reducing(ident1 = "CO2", &
      ident2 = "NC4", &
      beta_v = 1.174760923, &
      gamma_v = 1.222437324, &
      beta_T = 1.018171004, &
      gamma_T = 0.911498231)

  type(gerg_mix_reducing), parameter :: gerg_red_208 = &
      gerg_mix_reducing(ident1 = "IC4", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(gerg_mix_reducing), parameter :: gerg_red_209 = &
      gerg_mix_reducing(ident1 = "C2", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.201417898, &
      beta_T = 1., &
      gamma_T = 1.069224728)

  type(gerg_mix_reducing), parameter :: gerg_red_210 = &
      gerg_mix_reducing(ident1 = "NC8", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  integer, parameter :: max_gerg_mix_reducing = 210
  type (gerg_mix_reducing), dimension(max_gerg_mix_reducing), parameter :: gerg_mix_reducingdb = (/&
      gerg_red_1,gerg_red_2,gerg_red_3,gerg_red_4,gerg_red_5,gerg_red_6, &
      gerg_red_7,gerg_red_8,gerg_red_9,gerg_red_10,gerg_red_11,gerg_red_12, &
      gerg_red_13,gerg_red_14,gerg_red_15,gerg_red_16,gerg_red_17,gerg_red_18, &
      gerg_red_19,gerg_red_20,gerg_red_21,gerg_red_22,gerg_red_23,gerg_red_24, &
      gerg_red_25,gerg_red_26,gerg_red_27,gerg_red_28,gerg_red_29,gerg_red_30, &
      gerg_red_31,gerg_red_32,gerg_red_33,gerg_red_34,gerg_red_35,gerg_red_36, &
      gerg_red_37,gerg_red_38,gerg_red_39,gerg_red_40,gerg_red_41,gerg_red_42, &
      gerg_red_43,gerg_red_44,gerg_red_45,gerg_red_46,gerg_red_47,gerg_red_48, &
      gerg_red_49,gerg_red_50,gerg_red_51,gerg_red_52,gerg_red_53,gerg_red_54, &
      gerg_red_55,gerg_red_56,gerg_red_57,gerg_red_58,gerg_red_59,gerg_red_60, &
      gerg_red_61,gerg_red_62,gerg_red_63,gerg_red_64,gerg_red_65,gerg_red_66, &
      gerg_red_67,gerg_red_68,gerg_red_69,gerg_red_70,gerg_red_71,gerg_red_72, &
      gerg_red_73,gerg_red_74,gerg_red_75,gerg_red_76,gerg_red_77,gerg_red_78, &
      gerg_red_79,gerg_red_80,gerg_red_81,gerg_red_82,gerg_red_83,gerg_red_84, &
      gerg_red_85,gerg_red_86,gerg_red_87,gerg_red_88,gerg_red_89,gerg_red_90, &
      gerg_red_91,gerg_red_92,gerg_red_93,gerg_red_94,gerg_red_95,gerg_red_96, &
      gerg_red_97,gerg_red_98,gerg_red_99,gerg_red_100,gerg_red_101,gerg_red_102, &
      gerg_red_103,gerg_red_104,gerg_red_105,gerg_red_106,gerg_red_107,gerg_red_108, &
      gerg_red_109,gerg_red_110,gerg_red_111,gerg_red_112,gerg_red_113,gerg_red_114, &
      gerg_red_115,gerg_red_116,gerg_red_117,gerg_red_118,gerg_red_119,gerg_red_120, &
      gerg_red_121,gerg_red_122,gerg_red_123,gerg_red_124,gerg_red_125,gerg_red_126, &
      gerg_red_127,gerg_red_128,gerg_red_129,gerg_red_130,gerg_red_131,gerg_red_132, &
      gerg_red_133,gerg_red_134,gerg_red_135,gerg_red_136,gerg_red_137,gerg_red_138, &
      gerg_red_139,gerg_red_140,gerg_red_141,gerg_red_142,gerg_red_143,gerg_red_144, &
      gerg_red_145,gerg_red_146,gerg_red_147,gerg_red_148,gerg_red_149,gerg_red_150, &
      gerg_red_151,gerg_red_152,gerg_red_153,gerg_red_154,gerg_red_155,gerg_red_156, &
      gerg_red_157,gerg_red_158,gerg_red_159,gerg_red_160,gerg_red_161,gerg_red_162, &
      gerg_red_163,gerg_red_164,gerg_red_165,gerg_red_166,gerg_red_167,gerg_red_168, &
      gerg_red_169,gerg_red_170,gerg_red_171,gerg_red_172,gerg_red_173,gerg_red_174, &
      gerg_red_175,gerg_red_176,gerg_red_177,gerg_red_178,gerg_red_179,gerg_red_180, &
      gerg_red_181,gerg_red_182,gerg_red_183,gerg_red_184,gerg_red_185,gerg_red_186, &
      gerg_red_187,gerg_red_188,gerg_red_189,gerg_red_190,gerg_red_191,gerg_red_192, &
      gerg_red_193,gerg_red_194,gerg_red_195,gerg_red_196,gerg_red_197,gerg_red_198, &
      gerg_red_199,gerg_red_200,gerg_red_201,gerg_red_202,gerg_red_203,gerg_red_204, &
      gerg_red_205,gerg_red_206,gerg_red_207,gerg_red_208,gerg_red_209,gerg_red_210 &
      /)

  type(gerg_mix_data), parameter :: gerg_mix1 = &
      gerg_mix_data(ident1 = "C2", &
      ident2 = "C3", &
      Fij = 0.13042476515, &
      num_mix = 10, &
      n_mix = (/ &
      0.25574776844118d1,-0.79846357136353d1,0.47859131465806d1, &
      -0.73265392369587,0.13805471345312d1,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.55527385721943d-4,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.0,1.55,1.7, &
      0.25,1.35,0.0, &
      1.25,0.0,0.7, &
      5.4,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,2,2,3, &
      3,4,4,4,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      num_exp = 0)

  type(gerg_mix_data), parameter :: gerg_mix2 = &
      gerg_mix_data(ident1 = "C2", &
      ident2 = "IC4", &
      Fij = 0.260632376098, &
      num_mix = 10, &
      n_mix = (/ &
      0.25574776844118d1,-0.79846357136353d1,0.47859131465806d1, &
      -0.73265392369587,0.13805471345312d1,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.55527385721943d-4,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.0,1.55,1.7, &
      0.25,1.35,0.0, &
      1.25,0.0,0.7, &
      5.4,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,2,2,3, &
      3,4,4,4,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      num_exp = 0)

  type(gerg_mix_data), parameter :: gerg_mix3 = &
      gerg_mix_data(ident1 = "C1", &
      ident2 = "NC4", &
      Fij = 1., &
      num_mix = 10, &
      n_mix = (/ &
      0.25574776844118d1,-0.79846357136353d1,0.47859131465806d1, &
      -0.73265392369587,0.13805471345312d1,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.55527385721943d-4,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.0,1.55,1.7, &
      0.25,1.35,0.0, &
      1.25,0.0,0.7, &
      5.4,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,2,2,3, &
      3,4,4,4,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      num_exp = 0)

  type(gerg_mix_data), parameter :: gerg_mix4 = &
      gerg_mix_data(ident1 = "C1", &
      ident2 = "N2", &
      Fij = 1., &
      num_mix = 9, &
      n_mix = (/ &
      -0.98038985517335d-2,0.42487270143005d-3,-0.34800214576142d-1, &
      -0.13333813013896,-0.11993694974627d-1,0.69243379775168d-1, &
      -0.31022508148249,0.24495491753226,0.22369816716981, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      0.0,1.85,7.85, &
      5.4,0.0,0.75, &
      2.8,4.45,4.25, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,4,1,2,2,2, &
      2,2,3,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,1., &
      1.,0.25,0., &
      0.,0.,0., &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,1., &
      1.,2.5,3., &
      3.,3.,3., &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 7)

  type(gerg_mix_data), parameter :: gerg_mix5 = &
      gerg_mix_data(ident1 = "NC4", &
      ident2 = "IC4", &
      Fij = -0.0551240293009, &
      num_mix = 10, &
      n_mix = (/ &
      0.25574776844118d1,-0.79846357136353d1,0.47859131465806d1, &
      -0.73265392369587,0.13805471345312d1,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.55527385721943d-4,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.0,1.55,1.7, &
      0.25,1.35,0.0, &
      1.25,0.0,0.7, &
      5.4,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,2,2,3, &
      3,4,4,4,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      num_exp = 0)

  type(gerg_mix_data), parameter :: gerg_mix6 = &
      gerg_mix_data(ident1 = "N2", &
      ident2 = "C2", &
      Fij = 1., &
      num_mix = 6, &
      n_mix = (/ &
      -0.47376518126608,0.48961193461001,-0.57011062090535d-2, &
      -0.19966820041320,-0.69411103101723,0.69226192739021, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      0.0,0.05,0.0, &
      3.65,4.9,4.45, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      2,2,3,1,2,2, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.0, &
      1.,1.,0.875, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.0, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.0, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.0, &
      1.,1.,1.25, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 3)

  type(gerg_mix_data), parameter :: gerg_mix7 = &
      gerg_mix_data(ident1 = "C2", &
      ident2 = "NC4", &
      Fij = 0.281570073085, &
      num_mix = 10, &
      n_mix = (/ &
      0.25574776844118d1,-0.79846357136353d1,0.47859131465806d1, &
      -0.73265392369587,0.13805471345312d1,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.55527385721943d-4,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.0,1.55,1.7, &
      0.25,1.35,0.0, &
      1.25,0.0,0.7, &
      5.4,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,2,2,3, &
      3,4,4,4,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      num_exp = 0)

  type(gerg_mix_data), parameter :: gerg_mix8 = &
      gerg_mix_data(ident1 = "N2", &
      ident2 = "CO2", &
      Fij = 1., &
      num_mix = 6, &
      n_mix = (/ &
      0.28661625028399,-0.10919833861247,-0.11374032082270d1, &
      0.76580544237358,0.42638000926819d-2,0.17673538204534, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.85,1.4,3.2, &
      2.5,8.0,3.75, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      2,3,1,1,1,2, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.25, &
      0.25,0.,0., &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.75, &
      1.,2.,3., &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 4)

  type(gerg_mix_data), parameter :: gerg_mix9 = &
      gerg_mix_data(ident1 = "C1", &
      ident2 = "C3", &
      Fij = 1., &
      num_mix = 9, &
      n_mix = (/ &
      0.13746429958576d-1,-0.74425012129552d-2,-0.45516600213685d-2, &
      -0.54546603350237d-2,0.23682016824471d-2,0.18007763721438, &
      -0.44773942932486,0.19327374888200d-1,-0.30632197804624, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.85,3.95,0.0, &
      1.85,3.85,5.25, &
      3.85,0.2,6.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      3,3,4,4,4,1, &
      1,1,2,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.25, &
      0.25,0.,0., &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.75, &
      1.,2.,3., &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 4)

  type(gerg_mix_data), parameter :: gerg_mix10 = &
      gerg_mix_data(ident1 = "C1", &
      ident2 = "C2", &
      Fij = 1., &
      num_mix = 12, &
      n_mix = (/ &
      -0.80926050298746d-3,-0.75381925080059d-3,-0.41618768891219d-1, &
      -0.23452173681569,0.14003840584586,0.63281744807738d-1, &
      -0.34660425848809d-1,-0.23918747334251,0.19855255066891d-2, &
      0.61777746171555d1,-0.69575358271105d1,0.10630185306388d1 &
      /), &
      t_mix = (/ &
      0.65,1.55,3.1, &
      5.9,7.05,3.35, &
      1.2,5.8,2.7, &
      0.45,0.55,1.95 &
      /), &
      d_mix = (/ &
      3,4,1,2,2,2, &
      2,2,2,3,3,3 &
      /), &
      eta_mix = (/ &
      0.0,0.0,1., &
      1.,1.,0.875, &
      0.75,0.5,0., &
      0.,0.,0. &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5 &
      /), &
      beta_mix = (/ &
      0.0,0.0,1., &
      1.,1.,1.25, &
      1.5,2.,3., &
      3.,3.,3. &
      /), &
      num_exp = 10)

  type(gerg_mix_data), parameter :: gerg_mix11 = &
      gerg_mix_data(ident1 = "C3", &
      ident2 = "IC4", &
      Fij = -0.0551609771024, &
      num_mix = 10, &
      n_mix = (/ &
      0.25574776844118d1,-0.79846357136353d1,0.47859131465806d1, &
      -0.73265392369587,0.13805471345312d1,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.55527385721943d-4,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.0,1.55,1.7, &
      0.25,1.35,0.0, &
      1.25,0.0,0.7, &
      5.4,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,2,2,3, &
      3,4,4,4,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      num_exp = 0)

  type(gerg_mix_data), parameter :: gerg_mix12 = &
      gerg_mix_data(ident1 = "C1", &
      ident2 = "H2", &
      Fij = 1., &
      num_mix = 4, &
      n_mix = (/ &
      -0.25157134971934,-0.62203841111983d-2,0.88850315184396d-1, &
      -0.35592212573239d-1,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      2.0,-1.0,1.75, &
      1.4,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,3,3,4,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0)

  type(gerg_mix_data), parameter :: gerg_mix13 = &
      gerg_mix_data(ident1 = "C3", &
      ident2 = "NC4", &
      Fij = 0.0312572600489, &
      num_mix = 10, &
      n_mix = (/ &
      0.25574776844118d1,-0.79846357136353d1,0.47859131465806d1, &
      -0.73265392369587,0.13805471345312d1,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.55527385721943d-4,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.0,1.55,1.7, &
      0.25,1.35,0.0, &
      1.25,0.0,0.7, &
      5.4,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,2,2,3, &
      3,4,4,4,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      num_exp = 0)

  type(gerg_mix_data), parameter :: gerg_mix14 = &
      gerg_mix_data(ident1 = "C1", &
      ident2 = "IC4", &
      Fij = 0.771035405688, &
      num_mix = 10, &
      n_mix = (/ &
      0.25574776844118d1,-0.79846357136353d1,0.47859131465806d1, &
      -0.73265392369587,0.13805471345312d1,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.55527385721943d-4,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.0,1.55,1.7, &
      0.25,1.35,0.0, &
      1.25,0.0,0.7, &
      5.4,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,2,2,3, &
      3,4,4,4,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0,0.0, &
      0.0,0.0d0,0.0d0 &
      /), &
      num_exp = 0)

  type(gerg_mix_data), parameter :: gerg_mix15 = &
      gerg_mix_data(ident1 = "C1", &
      ident2 = "CO2", &
      Fij = 1., &
      num_mix = 6, &
      n_mix = (/ &
      -0.10859387354942,0.80228576727389d-1,-0.93303985115717d-2, &
      0.40989274005848d-1,-0.24338019772494,0.23855347281124, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      2.6,1.95,0.0, &
      3.95,7.95,8.0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,2,3,1,2,3, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0,0.0,0.0, &
      1.,0.5,0., &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0,0.0,0.0, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0,0.0,0.0, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0,0.0,0.0, &
      1.,2.,3., &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 3)

  integer, parameter :: max_gerg_mix_data = 15
  type (gerg_mix_data), dimension(max_gerg_mix_data), parameter :: gerg_mix_datadb = (/&
      gerg_mix1,gerg_mix2,gerg_mix3,gerg_mix4,gerg_mix5,gerg_mix6, &
      gerg_mix7,gerg_mix8,gerg_mix9,gerg_mix10,gerg_mix11,gerg_mix12, &
      gerg_mix13,gerg_mix14,gerg_mix15 &
      /)

end module gergmixdb
