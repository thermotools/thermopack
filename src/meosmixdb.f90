!> Automatically generated file meosmixdb.f90
!! Time stamp: 2023-02-23T14:11:36.036900

module meosmixdb
  use thermopack_constants, only: uid_len
  implicit none
  public

  type :: meos_mix_reducing
    character(len=uid_len) :: ident1 !< The component ID
    character(len=uid_len) :: ident2 !< The component ID
    real :: beta_v !< Reducing density parameter
    real :: gamma_v !< Reducing density parameter
    real :: beta_T !< Reducing temperature parameter
    real :: gamma_T !< Reducing temperature parameter
  end type meos_mix_reducing

  type :: meos_mix_data
    character(len=uid_len) :: ident1 !< The component ID
    character(len=uid_len) :: ident2 !< The component ID
    real :: Fij !< Departure function parameter
    integer :: num_mix !< Number of parameters
    real :: n_mix(12) !<
    real :: t_mix(12) !<
    integer :: d_mix(12) !<
    integer :: l_mix(12) !<
    real :: eta_mix(12) !<
    real :: gamma_mix(12) !<
    real :: epsilon_mix(12) !<
    real :: beta_mix(12) !<
    integer :: num_exp !< Number of exponential terms
    integer :: num_gauss !< Number of Gaussian terms
  end type meos_mix_data

  type(meos_mix_reducing), parameter :: meos_red_1 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "NH3", &
      beta_v = 0.739937, &
      gamma_v = 1.447261, &
      beta_T = 1.057512, &
      gamma_T = 0.952705)

  type(meos_mix_reducing), parameter :: meos_red_2 = &
      meos_mix_reducing(ident1 = "H2", &
      ident2 = "NH3", &
      beta_v = 1.0103, &
      gamma_v = 0.7298, &
      beta_T = 0.98824, &
      gamma_T = 1.1266)

  type(meos_mix_reducing), parameter :: meos_red_3 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.435)

  type(meos_mix_reducing), parameter :: meos_red_4 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "H2", &
      beta_v = 1.198000, &
      gamma_v = 0.842000, &
      beta_T = 0.979000, &
      gamma_T = 1.961000)

  type(meos_mix_reducing), parameter :: meos_red_5 = &
      meos_mix_reducing(ident1 = "NC10", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_6 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.954, &
      gamma_T = 1.366)

  type(meos_mix_reducing), parameter :: meos_red_7 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "H2S", &
      beta_v = 1.012599087, &
      gamma_v = 1.040161207, &
      beta_T = 1.011090031, &
      gamma_T = 0.961155729)

  type(meos_mix_reducing), parameter :: meos_red_8 = &
      meos_mix_reducing(ident1 = "AR", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.01, &
      gamma_T = 1.397)

  type(meos_mix_reducing), parameter :: meos_red_9 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "PRLN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.002)

  type(meos_mix_reducing), parameter :: meos_red_10 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.02)

  type(meos_mix_reducing), parameter :: meos_red_11 = &
      meos_mix_reducing(ident1 = "AR", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.957934447, &
      gamma_T = 1.822157123)

  type(meos_mix_reducing), parameter :: meos_red_12 = &
      meos_mix_reducing(ident1 = "BENZENE", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.99896, &
      gamma_T = 1.00464)

  type(meos_mix_reducing), parameter :: meos_red_13 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.195952177, &
      beta_T = 1., &
      gamma_T = 1.472607971)

  type(meos_mix_reducing), parameter :: meos_red_14 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "C3", &
      beta_v = 0.997607277, &
      gamma_v = 1.00303472, &
      beta_T = 0.996199694, &
      gamma_T = 1.01473019)

  type(meos_mix_reducing), parameter :: meos_red_15 = &
      meos_mix_reducing(ident1 = "TOLUENE", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_16 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 0.973386152, &
      beta_T = 1.00768862, &
      gamma_T = 1.140671202)

  type(meos_mix_reducing), parameter :: meos_red_17 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.006767176, &
      beta_T = 1., &
      gamma_T = 0.998793111)

  type(meos_mix_reducing), parameter :: meos_red_18 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9996901, &
      gamma_T = 0.99296)

  type(meos_mix_reducing), parameter :: meos_red_19 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "NC10", &
      beta_v = 1.001516371, &
      gamma_v = 1.013511439, &
      beta_T = 0.99764101, &
      gamma_T = 1.028939539)

  type(meos_mix_reducing), parameter :: meos_red_20 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "NC5", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.161980013944, &
      gamma_T = 0.8702)

  type(meos_mix_reducing), parameter :: meos_red_21 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.15)

  type(meos_mix_reducing), parameter :: meos_red_22 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "PRLN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.99, &
      gamma_T = 1.197)

  type(meos_mix_reducing), parameter :: meos_red_23 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1.3341, &
      beta_T = 1.107297087809, &
      gamma_T = 0.8395)

  type(meos_mix_reducing), parameter :: meos_red_24 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.019174227, &
      beta_T = 1., &
      gamma_T = 1.021283378)

  type(meos_mix_reducing), parameter :: meos_red_25 = &
      meos_mix_reducing(ident1 = "NC10", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1.005, &
      beta_T = 1., &
      gamma_T = 0.998)

  type(meos_mix_reducing), parameter :: meos_red_26 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "NC10", &
      beta_v = 1.000151132, &
      gamma_v = 1.183394668, &
      beta_T = 1.02002879, &
      gamma_T = 1.145512213)

  type(meos_mix_reducing), parameter :: meos_red_27 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.19)

  type(meos_mix_reducing), parameter :: meos_red_28 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "IC4", &
      beta_v = 1., &
      gamma_v = 1.006616886, &
      beta_T = 1., &
      gamma_T = 1.033283811)

  type(meos_mix_reducing), parameter :: meos_red_29 = &
      meos_mix_reducing(ident1 = "NE", &
      ident2 = "XE", &
      beta_v = 0.894, &
      gamma_v = 1.213, &
      beta_T = 1., &
      gamma_T = 1.283)

  type(meos_mix_reducing), parameter :: meos_red_30 = &
      meos_mix_reducing(ident1 = "OXYL", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.002908434460, &
      gamma_T = 0.98501)

  type(meos_mix_reducing), parameter :: meos_red_31 = &
      meos_mix_reducing(ident1 = "ETOH", &
      ident2 = "CYCLOHEX", &
      beta_v = 0.994035785288, &
      gamma_v = 1.2, &
      beta_T = 0.989119683482, &
      gamma_T = 0.961)

  type(meos_mix_reducing), parameter :: meos_red_32 = &
      meos_mix_reducing(ident1 = "ETOH", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.048, &
      beta_T = 1.021450459653, &
      gamma_T = 0.9)

  type(meos_mix_reducing), parameter :: meos_red_33 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "H2", &
      beta_v = 1.037000, &
      gamma_v = 1.040000, &
      beta_T = 1.078000, &
      gamma_T = 1.105000)

  type(meos_mix_reducing), parameter :: meos_red_34 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.98799, &
      gamma_T = 1.17616)

  type(meos_mix_reducing), parameter :: meos_red_35 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "CO", &
      beta_v = 1.000000000, &
      gamma_v = 1.007241430, &
      beta_T = 1.000000000, &
      gamma_T = 1.177903242)

  type(meos_mix_reducing), parameter :: meos_red_36 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "ACETONE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.016363451570, &
      gamma_T = 0.9358)

  type(meos_mix_reducing), parameter :: meos_red_37 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.169701102, &
      beta_T = 1., &
      gamma_T = 1.092177796)

  type(meos_mix_reducing), parameter :: meos_red_38 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.927, &
      gamma_T = 1.666)

  type(meos_mix_reducing), parameter :: meos_red_39 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.0267)

  type(meos_mix_reducing), parameter :: meos_red_40 = &
      meos_mix_reducing(ident1 = "AR", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.492)

  type(meos_mix_reducing), parameter :: meos_red_41 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.01308, &
      gamma_T = 0.99672)

  type(meos_mix_reducing), parameter :: meos_red_42 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1.002284353, &
      beta_T = 1., &
      gamma_T = 1.001835788)

  type(meos_mix_reducing), parameter :: meos_red_43 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "NC10", &
      beta_v = 0.984104227, &
      gamma_v = 1.053040574, &
      beta_T = 0.985331233, &
      gamma_T = 1.140905252)

  type(meos_mix_reducing), parameter :: meos_red_44 = &
      meos_mix_reducing(ident1 = "XE", &
      ident2 = "C3_1", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.934273835661, &
      gamma_T = 0.94711)

  type(meos_mix_reducing), parameter :: meos_red_45 = &
      meos_mix_reducing(ident1 = "H2", &
      ident2 = "H2S", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_46 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.018, &
      gamma_T = 0.965)

  type(meos_mix_reducing), parameter :: meos_red_47 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.95)

  type(meos_mix_reducing), parameter :: meos_red_48 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "O2", &
      beta_v = 1.000000, &
      gamma_v = 1.000002, &
      beta_T = 1.000000, &
      gamma_T = 1.118566)

  type(meos_mix_reducing), parameter :: meos_red_49 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "MEG", &
      beta_v = 1., &
      gamma_v = 0.9108, &
      beta_T = 0.9975, &
      gamma_T = 1.0468)

  type(meos_mix_reducing), parameter :: meos_red_50 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "BENZENE", &
      beta_v = 1.044, &
      gamma_v = 1.152, &
      beta_T = 0.99, &
      gamma_T = 0.966)

  type(meos_mix_reducing), parameter :: meos_red_51 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.16)

  type(meos_mix_reducing), parameter :: meos_red_52 = &
      meos_mix_reducing(ident1 = "KR", &
      ident2 = "C2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.992161920825, &
      gamma_T = 1.0437)

  type(meos_mix_reducing), parameter :: meos_red_53 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 0.9903, &
      beta_T = 1., &
      gamma_T = 0.9335)

  type(meos_mix_reducing), parameter :: meos_red_54 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "NC4", &
      beta_v = 0.979105972, &
      gamma_v = 1.045375122, &
      beta_T = 0.99417491, &
      gamma_T = 1.171607691)

  type(meos_mix_reducing), parameter :: meos_red_55 = &
      meos_mix_reducing(ident1 = "ETOH", &
      ident2 = "NC8", &
      beta_v = 0.986193293886, &
      gamma_v = 1.036, &
      beta_T = 0.985221674877, &
      gamma_T = 0.887)

  type(meos_mix_reducing), parameter :: meos_red_56 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_57 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "ACETONE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.990197049213, &
      gamma_T = 1.0959)

  type(meos_mix_reducing), parameter :: meos_red_58 = &
      meos_mix_reducing(ident1 = "XE", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.980786394531, &
      gamma_T = 1.16656)

  type(meos_mix_reducing), parameter :: meos_red_59 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.931, &
      gamma_T = 0.8775)

  type(meos_mix_reducing), parameter :: meos_red_60 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "PRLN", &
      beta_v = 1.096, &
      gamma_v = 1.014, &
      beta_T = 0.997, &
      gamma_T = 0.945)

  type(meos_mix_reducing), parameter :: meos_red_61 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.060243344, &
      beta_T = 1., &
      gamma_T = 1.021624748)

  type(meos_mix_reducing), parameter :: meos_red_62 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "SO2", &
      beta_v = 1.000000, &
      gamma_v = 1.000000, &
      beta_T = 1.000000, &
      gamma_T = 1.000000)

  type(meos_mix_reducing), parameter :: meos_red_63 = &
      meos_mix_reducing(ident1 = "F6S", &
      ident2 = "C3", &
      beta_v = 1.007559212995, &
      gamma_v = 1.0150842, &
      beta_T = 0.990884850262, &
      gamma_T = 0.89338144)

  type(meos_mix_reducing), parameter :: meos_red_64 = &
      meos_mix_reducing(ident1 = "KR", &
      ident2 = "PRLN", &
      beta_v = 1., &
      gamma_v = 1.0390195, &
      beta_T = 1., &
      gamma_T = 1.0773977)

  type(meos_mix_reducing), parameter :: meos_red_65 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "N2", &
      beta_v = 0.998721377, &
      gamma_v = 1.013950311, &
      beta_T = 0.99809883, &
      gamma_T = 0.979273013)

  type(meos_mix_reducing), parameter :: meos_red_66 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 0.851343711, &
      beta_T = 1., &
      gamma_T = 1.038675574)

  type(meos_mix_reducing), parameter :: meos_red_67 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "IC4", &
      beta_v = 1.000880464, &
      gamma_v = 1.00041444, &
      beta_T = 1.000077547, &
      gamma_T = 1.001432824)

  type(meos_mix_reducing), parameter :: meos_red_68 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.006268954, &
      beta_T = 1., &
      gamma_T = 1.001633952)

  type(meos_mix_reducing), parameter :: meos_red_69 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.057666085, &
      beta_T = 1., &
      gamma_T = 1.134532014)

  type(meos_mix_reducing), parameter :: meos_red_70 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_71 = &
      meos_mix_reducing(ident1 = "NC10", &
      ident2 = "H2S", &
      beta_v = 0.975187766, &
      gamma_v = 1.171714677, &
      beta_T = 0.973091413, &
      gamma_T = 1.103693489)

  type(meos_mix_reducing), parameter :: meos_red_72 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.03)

  type(meos_mix_reducing), parameter :: meos_red_73 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.002480637, &
      beta_T = 1., &
      gamma_T = 1.000761237)

  type(meos_mix_reducing), parameter :: meos_red_74 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "NC4", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.02)

  type(meos_mix_reducing), parameter :: meos_red_75 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "OXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.99933, &
      gamma_T = 0.99245)

  type(meos_mix_reducing), parameter :: meos_red_76 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1.011759763, &
      beta_T = 1., &
      gamma_T = 0.600340961)

  type(meos_mix_reducing), parameter :: meos_red_77 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9934926, &
      gamma_T = 1.09811)

  type(meos_mix_reducing), parameter :: meos_red_78 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1.170217596, &
      beta_T = 1., &
      gamma_T = 0.569681333)

  type(meos_mix_reducing), parameter :: meos_red_79 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1.002728434, &
      beta_T = 1., &
      gamma_T = 1.000792201)

  type(meos_mix_reducing), parameter :: meos_red_80 = &
      meos_mix_reducing(ident1 = "MXYL", &
      ident2 = "NC11", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.999100809272, &
      gamma_T = 1.00198)

  type(meos_mix_reducing), parameter :: meos_red_81 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "PRLN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.184)

  type(meos_mix_reducing), parameter :: meos_red_82 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.0034, &
      gamma_T = 1.1043)

  type(meos_mix_reducing), parameter :: meos_red_83 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1.223638763, &
      beta_T = 1., &
      gamma_T = 0.615512682)

  type(meos_mix_reducing), parameter :: meos_red_84 = &
      meos_mix_reducing(ident1 = "CYCLOHEX", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1.0167, &
      beta_T = 1., &
      gamma_T = 1.0548)

  type(meos_mix_reducing), parameter :: meos_red_85 = &
      meos_mix_reducing(ident1 = "AR", &
      ident2 = "NH3", &
      beta_v = 0.756526, &
      gamma_v = 1.041113, &
      beta_T = 1.146326, &
      gamma_T = 0.998353)

  type(meos_mix_reducing), parameter :: meos_red_86 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.9194)

  type(meos_mix_reducing), parameter :: meos_red_87 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.99)

  type(meos_mix_reducing), parameter :: meos_red_88 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "C2", &
      beta_v = 0.978880168, &
      gamma_v = 1.042352891, &
      beta_T = 1.007671428, &
      gamma_T = 1.098650964)

  type(meos_mix_reducing), parameter :: meos_red_89 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "H2S", &
      beta_v = 1., &
      gamma_v = 0.835763343, &
      beta_T = 1., &
      gamma_T = 0.982651529)

  type(meos_mix_reducing), parameter :: meos_red_90 = &
      meos_mix_reducing(ident1 = "NC10", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 0.551405318, &
      beta_T = 0.897162268, &
      gamma_T = 0.740416402)

  type(meos_mix_reducing), parameter :: meos_red_91 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.001508227, &
      beta_T = 1., &
      gamma_T = 0.999762786)

  type(meos_mix_reducing), parameter :: meos_red_92 = &
      meos_mix_reducing(ident1 = "ETOH", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.97813, &
      gamma_T = 0.9059)

  type(meos_mix_reducing), parameter :: meos_red_93 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.021668316, &
      beta_T = 1., &
      gamma_T = 1.00988576)

  type(meos_mix_reducing), parameter :: meos_red_94 = &
      meos_mix_reducing(ident1 = "CYCLOHEX", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.009, &
      beta_T = 1., &
      gamma_T = 1.015)

  type(meos_mix_reducing), parameter :: meos_red_95 = &
      meos_mix_reducing(ident1 = "BENZENE", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1.004, &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_96 = &
      meos_mix_reducing(ident1 = "NC9", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_97 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "NC8", &
      beta_v = 0.994740603, &
      gamma_v = 1.116549372, &
      beta_T = 0.957473785, &
      gamma_T = 1.449245409)

  type(meos_mix_reducing), parameter :: meos_red_98 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.956937799043, &
      gamma_T = 1.191)

  type(meos_mix_reducing), parameter :: meos_red_99 = &
      meos_mix_reducing(ident1 = "NC9", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_100 = &
      meos_mix_reducing(ident1 = "NC9", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.342647661, &
      beta_T = 1., &
      gamma_T = 2.23435404)

  type(meos_mix_reducing), parameter :: meos_red_101 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.084740904, &
      beta_T = 1., &
      gamma_T = 1.173916162)

  type(meos_mix_reducing), parameter :: meos_red_102 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.01, &
      gamma_T = 1.397)

  type(meos_mix_reducing), parameter :: meos_red_103 = &
      meos_mix_reducing(ident1 = "NC9", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_104 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "TOLUENE", &
      beta_v = 1.0019167, &
      gamma_v = 1.0026789, &
      beta_T = 0.998329, &
      gamma_T = 0.9832346)

  type(meos_mix_reducing), parameter :: meos_red_105 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.046905515, &
      beta_T = 1., &
      gamma_T = 1.033180106)

  type(meos_mix_reducing), parameter :: meos_red_106 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.45)

  type(meos_mix_reducing), parameter :: meos_red_107 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "NC4", &
      beta_v = 0.999157205, &
      gamma_v = 1.006179146, &
      beta_T = 0.999130554, &
      gamma_T = 1.034832749)

  type(meos_mix_reducing), parameter :: meos_red_108 = &
      meos_mix_reducing(ident1 = "CYCLOHEX", &
      ident2 = "EBZN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.0043, &
      gamma_T = 0.99414)

  type(meos_mix_reducing), parameter :: meos_red_109 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.927, &
      gamma_T = 1.949)

  type(meos_mix_reducing), parameter :: meos_red_110 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "NC16", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9833, &
      gamma_T = 1.1486)

  type(meos_mix_reducing), parameter :: meos_red_111 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "NH3", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9599, &
      gamma_T = 0.7905)

  type(meos_mix_reducing), parameter :: meos_red_112 = &
      meos_mix_reducing(ident1 = "NC10", &
      ident2 = "H2", &
      beta_v = 1.695358382, &
      gamma_v = 1.120233729, &
      beta_T = 1.064818089, &
      gamma_T = 3.786003724)

  type(meos_mix_reducing), parameter :: meos_red_113 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.976)

  type(meos_mix_reducing), parameter :: meos_red_114 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "H2S", &
      beta_v = 0.828967164, &
      gamma_v = 1.087956749, &
      beta_T = 0.988937417, &
      gamma_T = 1.013453092)

  type(meos_mix_reducing), parameter :: meos_red_115 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "H2S", &
      beta_v = 1.012994431, &
      gamma_v = 0.988591117, &
      beta_T = 0.974550548, &
      gamma_T = 0.937130844)

  type(meos_mix_reducing), parameter :: meos_red_116 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "TOLUENE", &
      beta_v = 1.047, &
      gamma_v = 1.134, &
      beta_T = 1., &
      gamma_T = 1.024)

  type(meos_mix_reducing), parameter :: meos_red_117 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.016, &
      gamma_T = 0.934)

  type(meos_mix_reducing), parameter :: meos_red_118 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.0198, &
      beta_T = 1., &
      gamma_T = 0.9146)

  type(meos_mix_reducing), parameter :: meos_red_119 = &
      meos_mix_reducing(ident1 = "H2O", &
      ident2 = "MEG", &
      beta_v = 1.009003234965, &
      gamma_v = 1.066166, &
      beta_T = 1.006062026742, &
      gamma_T = 0.9986637)

  type(meos_mix_reducing), parameter :: meos_red_120 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_121 = &
      meos_mix_reducing(ident1 = "TOLUENE", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.996)

  type(meos_mix_reducing), parameter :: meos_red_122 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "NC5", &
      beta_v = 1., &
      gamma_v = 1.078877166, &
      beta_T = 1., &
      gamma_T = 1.419029041)

  type(meos_mix_reducing), parameter :: meos_red_123 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.042500015116, &
      gamma_T = 0.95719)

  type(meos_mix_reducing), parameter :: meos_red_124 = &
      meos_mix_reducing(ident1 = "N2O", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.965763677628, &
      gamma_T = 1.2736)

  type(meos_mix_reducing), parameter :: meos_red_125 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.008972412, &
      beta_T = 1., &
      gamma_T = 1.002441051)

  type(meos_mix_reducing), parameter :: meos_red_126 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "H2S", &
      beta_v = 0.906630564, &
      gamma_v = 1.024085837, &
      beta_T = 1.016034583, &
      gamma_T = 0.92601888)

  type(meos_mix_reducing), parameter :: meos_red_127 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.56)

  type(meos_mix_reducing), parameter :: meos_red_128 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.243461678, &
      beta_T = 1., &
      gamma_T = 3.021197546)

  type(meos_mix_reducing), parameter :: meos_red_129 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_130 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.499)

  type(meos_mix_reducing), parameter :: meos_red_131 = &
      meos_mix_reducing(ident1 = "HE", &
      ident2 = "NE", &
      beta_v = 1.0097, &
      gamma_v = 0.899, &
      beta_T = 1.168, &
      gamma_T = 1.371)

  type(meos_mix_reducing), parameter :: meos_red_132 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.001357085, &
      beta_T = 1., &
      gamma_T = 1.000235044)

  type(meos_mix_reducing), parameter :: meos_red_133 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.01)

  type(meos_mix_reducing), parameter :: meos_red_134 = &
      meos_mix_reducing(ident1 = "CYCLOHEX", &
      ident2 = "NC11", &
      beta_v = 1., &
      gamma_v = 1.014, &
      beta_T = 1., &
      gamma_T = 1.04)

  type(meos_mix_reducing), parameter :: meos_red_135 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.99471, &
      gamma_T = 1.63462)

  type(meos_mix_reducing), parameter :: meos_red_136 = &
      meos_mix_reducing(ident1 = "HE", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_137 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "H2O", &
      beta_v = 0.9404260, &
      gamma_v = 0.7667560, &
      beta_T = 0.956090, &
      gamma_T = 0.8239840)

  type(meos_mix_reducing), parameter :: meos_red_138 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "ACETONE", &
      beta_v = 1., &
      gamma_v = 1.1177, &
      beta_T = 0.978377849525, &
      gamma_T = 1.0433)

  type(meos_mix_reducing), parameter :: meos_red_139 = &
      meos_mix_reducing(ident1 = "NC9", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.252151449, &
      beta_T = 1., &
      gamma_T = 1.294070556)

  type(meos_mix_reducing), parameter :: meos_red_140 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_141 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "AR", &
      beta_v = 1.0037659, &
      gamma_v = 1.0138330, &
      beta_T = 0.998705, &
      gamma_T = 1.0396748)

  type(meos_mix_reducing), parameter :: meos_red_142 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "EBZN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9992206, &
      gamma_T = 0.98584)

  type(meos_mix_reducing), parameter :: meos_red_143 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.957934447, &
      gamma_T = 1.822157123)

  type(meos_mix_reducing), parameter :: meos_red_144 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "CO2", &
      beta_v = 0.977794634, &
      gamma_v = 1.047578256, &
      beta_T = 1.005894529, &
      gamma_T = 1.107654104)

  type(meos_mix_reducing), parameter :: meos_red_145 = &
      meos_mix_reducing(ident1 = "CYCLOHEX", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.00315, &
      gamma_T = 0.99328)

  type(meos_mix_reducing), parameter :: meos_red_146 = &
      meos_mix_reducing(ident1 = "BENZENE", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.007170043540, &
      gamma_T = 1.02547)

  type(meos_mix_reducing), parameter :: meos_red_147 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "NC5", &
      beta_v = 0.993851009, &
      gamma_v = 1.026085655, &
      beta_T = 0.998688946, &
      gamma_T = 1.066665676)

  type(meos_mix_reducing), parameter :: meos_red_148 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "NC7", &
      beta_v = 0.962050831, &
      gamma_v = 1.156655935, &
      beta_T = 0.977431529, &
      gamma_T = 1.379850328)

  type(meos_mix_reducing), parameter :: meos_red_149 = &
      meos_mix_reducing(ident1 = "BENZENE", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.00112, &
      gamma_T = 1.00639)

  type(meos_mix_reducing), parameter :: meos_red_150 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "EBZN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9996002, &
      gamma_T = 0.98665)

  type(meos_mix_reducing), parameter :: meos_red_151 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "AR", &
      beta_v = 1.034630259, &
      gamma_v = 1.014678542, &
      beta_T = 0.990954281, &
      gamma_T = 0.989843388)

  type(meos_mix_reducing), parameter :: meos_red_152 = &
      meos_mix_reducing(ident1 = "H2O", &
      ident2 = "AR", &
      beta_v = 0.9403980, &
      gamma_v = 1.0509520, &
      beta_T = 0.679104, &
      gamma_T = 0.9210000)

  type(meos_mix_reducing), parameter :: meos_red_153 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.982, &
      gamma_T = 1.266)

  type(meos_mix_reducing), parameter :: meos_red_154 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "MEG", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.148)

  type(meos_mix_reducing), parameter :: meos_red_155 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.035)

  type(meos_mix_reducing), parameter :: meos_red_156 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.081)

  type(meos_mix_reducing), parameter :: meos_red_157 = &
      meos_mix_reducing(ident1 = "BENZENE", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.007, &
      beta_T = 1., &
      gamma_T = 0.987)

  type(meos_mix_reducing), parameter :: meos_red_158 = &
      meos_mix_reducing(ident1 = "AR", &
      ident2 = "KR", &
      beta_v = 1., &
      gamma_v = 1.0037297, &
      beta_T = 1., &
      gamma_T = 1.0025306)

  type(meos_mix_reducing), parameter :: meos_red_159 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "NH3", &
      beta_v = 0.739937224, &
      gamma_v = 1.707, &
      beta_T = 1.057511717, &
      gamma_T = 0.952705)

  type(meos_mix_reducing), parameter :: meos_red_160 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.07400611, &
      beta_T = 1., &
      gamma_T = 2.308215191)

  type(meos_mix_reducing), parameter :: meos_red_161 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1.0177, &
      beta_T = 1., &
      gamma_T = 1.442)

  type(meos_mix_reducing), parameter :: meos_red_162 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.219206702, &
      beta_T = 1., &
      gamma_T = 1.276565536)

  type(meos_mix_reducing), parameter :: meos_red_163 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9965519, &
      gamma_T = 1.08363)

  type(meos_mix_reducing), parameter :: meos_red_164 = &
      meos_mix_reducing(ident1 = "NC10", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_165 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "PRLN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.155)

  type(meos_mix_reducing), parameter :: meos_red_166 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "NC5", &
      beta_v = 1.024311498, &
      gamma_v = 1.068406078, &
      beta_T = 1.027000795, &
      gamma_T = 0.979217302)

  type(meos_mix_reducing), parameter :: meos_red_167 = &
      meos_mix_reducing(ident1 = "H2S", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.972370008958, &
      gamma_T = 1.10582)

  type(meos_mix_reducing), parameter :: meos_red_168 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.99082, &
      gamma_T = 1.09426)

  type(meos_mix_reducing), parameter :: meos_red_169 = &
      meos_mix_reducing(ident1 = "ETOH", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.980959959569, &
      gamma_T = 0.8981)

  type(meos_mix_reducing), parameter :: meos_red_170 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 0.9962, &
      beta_T = 1., &
      gamma_T = 0.98)

  type(meos_mix_reducing), parameter :: meos_red_171 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1.24, &
      beta_T = 0.97, &
      gamma_T = 1.3)

  type(meos_mix_reducing), parameter :: meos_red_172 = &
      meos_mix_reducing(ident1 = "EBZN", &
      ident2 = "OXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.001530037438, &
      gamma_T = 0.99924)

  type(meos_mix_reducing), parameter :: meos_red_173 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.188334783, &
      beta_T = 1., &
      gamma_T = 2.013859174)

  type(meos_mix_reducing), parameter :: meos_red_174 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.028994325, &
      beta_T = 1., &
      gamma_T = 1.008191499)

  type(meos_mix_reducing), parameter :: meos_red_175 = &
      meos_mix_reducing(ident1 = "PXYL", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.001011021131, &
      gamma_T = 0.99262)

  type(meos_mix_reducing), parameter :: meos_red_176 = &
      meos_mix_reducing(ident1 = "TOLUENE", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.0035123, &
      gamma_T = 0.99601)

  type(meos_mix_reducing), parameter :: meos_red_177 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.002972346, &
      beta_T = 1., &
      gamma_T = 1.002229938)

  type(meos_mix_reducing), parameter :: meos_red_178 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_179 = &
      meos_mix_reducing(ident1 = "ETOH", &
      ident2 = "EBZN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9898638, &
      gamma_T = 0.90463)

  type(meos_mix_reducing), parameter :: meos_red_180 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "ACETONE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.004722194313, &
      gamma_T = 0.9769)

  type(meos_mix_reducing), parameter :: meos_red_181 = &
      meos_mix_reducing(ident1 = "D2", &
      ident2 = "N2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.015610027687, &
      gamma_T = 1.22898)

  type(meos_mix_reducing), parameter :: meos_red_182 = &
      meos_mix_reducing(ident1 = "AR", &
      ident2 = "PRLN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.158)

  type(meos_mix_reducing), parameter :: meos_red_183 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "H2S", &
      beta_v = 0.936811219, &
      gamma_v = 1.010593999, &
      beta_T = 0.992573556, &
      gamma_T = 0.905829247)

  type(meos_mix_reducing), parameter :: meos_red_184 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.190354273, &
      beta_T = 1., &
      gamma_T = 1.256123503)

  type(meos_mix_reducing), parameter :: meos_red_185 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "NC5", &
      beta_v = 1., &
      gamma_v = 1.01815965, &
      beta_T = 1., &
      gamma_T = 1.00214364)

  type(meos_mix_reducing), parameter :: meos_red_186 = &
      meos_mix_reducing(ident1 = "MXYL", &
      ident2 = "OXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.00073, &
      gamma_T = 0.99997)

  type(meos_mix_reducing), parameter :: meos_red_187 = &
      meos_mix_reducing(ident1 = "H2O", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 0.5514, &
      beta_T = 1.080146899978, &
      gamma_T = 0.7908)

  type(meos_mix_reducing), parameter :: meos_red_188 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "O2", &
      beta_v = 0.999521770, &
      gamma_v = 0.997082328, &
      beta_T = 0.997190589, &
      gamma_T = 0.995157044)

  type(meos_mix_reducing), parameter :: meos_red_189 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "ETOH", &
      beta_v = 1.022, &
      gamma_v = 1.026, &
      beta_T = 1.013, &
      gamma_T = 0.901)

  type(meos_mix_reducing), parameter :: meos_red_190 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.199769134, &
      beta_T = 1., &
      gamma_T = 1.109973833)

  type(meos_mix_reducing), parameter :: meos_red_191 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "IC4", &
      beta_v = 0.98641583, &
      gamma_v = 1.100576129, &
      beta_T = 0.99286813, &
      gamma_T = 1.284462634)

  type(meos_mix_reducing), parameter :: meos_red_192 = &
      meos_mix_reducing(ident1 = "H2O", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_193 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.232939523, &
      beta_T = 1., &
      gamma_T = 2.509259945)

  type(meos_mix_reducing), parameter :: meos_red_194 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "H2S", &
      beta_v = 0.910394249, &
      gamma_v = 1.256844157, &
      beta_T = 1.004692366, &
      gamma_T = 0.9601742)

  type(meos_mix_reducing), parameter :: meos_red_195 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1.045439935, &
      beta_T = 1., &
      gamma_T = 1.021150247)

  type(meos_mix_reducing), parameter :: meos_red_196 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "H2O", &
      beta_v = 1.176, &
      gamma_v = 1.038, &
      beta_T = 1.263, &
      gamma_T = 0.748)

  type(meos_mix_reducing), parameter :: meos_red_197 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.016370338, &
      beta_T = 1., &
      gamma_T = 1.049035838)

  type(meos_mix_reducing), parameter :: meos_red_198 = &
      meos_mix_reducing(ident1 = "KR", &
      ident2 = "CO2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.948047023132, &
      gamma_T = 1.0466)

  type(meos_mix_reducing), parameter :: meos_red_199 = &
      meos_mix_reducing(ident1 = "XE", &
      ident2 = "PRLN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.009784814856, &
      gamma_T = 1.01223)

  type(meos_mix_reducing), parameter :: meos_red_200 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "AR", &
      beta_v = 1.0065020, &
      gamma_v = 1.0013410, &
      beta_T = 0.999039, &
      gamma_T = 0.9888220)

  type(meos_mix_reducing), parameter :: meos_red_201 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.03, &
      gamma_T = 0.95)

  type(meos_mix_reducing), parameter :: meos_red_202 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "NC5", &
      beta_v = 1.044919431, &
      gamma_v = 1.019921513, &
      beta_T = 0.996484021, &
      gamma_T = 1.008344412)

  type(meos_mix_reducing), parameter :: meos_red_203 = &
      meos_mix_reducing(ident1 = "OXYL", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.001351824964, &
      gamma_T = 0.99471)

  type(meos_mix_reducing), parameter :: meos_red_204 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "ETOH", &
      beta_v = 1.064, &
      gamma_v = 0.964, &
      beta_T = 1.016, &
      gamma_T = 0.998)

  type(meos_mix_reducing), parameter :: meos_red_205 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.40455409, &
      beta_T = 1., &
      gamma_T = 1.520975334)

  type(meos_mix_reducing), parameter :: meos_red_206 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.184340443, &
      beta_T = 1., &
      gamma_T = 1.996386669)

  type(meos_mix_reducing), parameter :: meos_red_207 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.186067025, &
      beta_T = 1., &
      gamma_T = 1.733280051)

  type(meos_mix_reducing), parameter :: meos_red_208 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1.343685343, &
      beta_T = 1., &
      gamma_T = 1.188899743)

  type(meos_mix_reducing), parameter :: meos_red_209 = &
      meos_mix_reducing(ident1 = "NC10", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 0.87018496, &
      beta_T = 1.049594632, &
      gamma_T = 1.803567587)

  type(meos_mix_reducing), parameter :: meos_red_210 = &
      meos_mix_reducing(ident1 = "BENZENE", &
      ident2 = "EBZN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.99807, &
      gamma_T = 1.01425)

  type(meos_mix_reducing), parameter :: meos_red_211 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1.000024335, &
      beta_T = 1., &
      gamma_T = 1.000050537)

  type(meos_mix_reducing), parameter :: meos_red_212 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "PRLN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.998, &
      gamma_T = 1.117)

  type(meos_mix_reducing), parameter :: meos_red_213 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.999011, &
      gamma_T = 1.04089)

  type(meos_mix_reducing), parameter :: meos_red_214 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "C3", &
      beta_v = 1.00482707, &
      gamma_v = 1.038470657, &
      beta_T = 0.989680305, &
      gamma_T = 1.098655531)

  type(meos_mix_reducing), parameter :: meos_red_215 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "N2", &
      beta_v = 0.903624500, &
      gamma_v = 1.215580800, &
      beta_T = 1.045874000, &
      gamma_T = 1.194658800)

  type(meos_mix_reducing), parameter :: meos_red_216 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.159131722, &
      beta_T = 1., &
      gamma_T = 3.169143057)

  type(meos_mix_reducing), parameter :: meos_red_217 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "ACETONE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.003109639884, &
      gamma_T = 1.0191)

  type(meos_mix_reducing), parameter :: meos_red_218 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "O2", &
      beta_v = 1.219246300, &
      gamma_v = 1.660631700, &
      beta_T = 0.927961000, &
      gamma_T = 1.035878200)

  type(meos_mix_reducing), parameter :: meos_red_219 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_220 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_221 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1.179, &
      beta_T = 0.994, &
      gamma_T = 1.086)

  type(meos_mix_reducing), parameter :: meos_red_222 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "C3", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.001001001001, &
      gamma_T = 0.995)

  type(meos_mix_reducing), parameter :: meos_red_223 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "TOLUENE", &
      beta_v = 0.961, &
      gamma_v = 1.085, &
      beta_T = 0.967, &
      gamma_T = 1.34)

  type(meos_mix_reducing), parameter :: meos_red_224 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "NC8", &
      beta_v = 1.026169373, &
      gamma_v = 1.104043935, &
      beta_T = 1.02969078, &
      gamma_T = 1.074455386)

  type(meos_mix_reducing), parameter :: meos_red_225 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "NC7", &
      beta_v = 1.205469976, &
      gamma_v = 1.164585914, &
      beta_T = 1.011806317, &
      gamma_T = 1.046169823)

  type(meos_mix_reducing), parameter :: meos_red_226 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "AR", &
      beta_v = 1.0066970, &
      gamma_v = 1.0015490, &
      beta_T = 0.999442, &
      gamma_T = 0.9893110)

  type(meos_mix_reducing), parameter :: meos_red_227 = &
      meos_mix_reducing(ident1 = "NE", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1.051, &
      beta_T = 1.004, &
      gamma_T = 1.0994)

  type(meos_mix_reducing), parameter :: meos_red_228 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 0.87, &
      beta_T = 0.933, &
      gamma_T = 1.941)

  type(meos_mix_reducing), parameter :: meos_red_229 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.99788, &
      gamma_T = 0.98651)

  type(meos_mix_reducing), parameter :: meos_red_230 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "H2", &
      beta_v = 0.925367171, &
      gamma_v = 1.10607204, &
      beta_T = 0.932969831, &
      gamma_T = 1.902008495)

  type(meos_mix_reducing), parameter :: meos_red_231 = &
      meos_mix_reducing(ident1 = "H2S", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.994, &
      gamma_T = 1.06)

  type(meos_mix_reducing), parameter :: meos_red_232 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "NC5", &
      beta_v = 0.94833012, &
      gamma_v = 1.124508039, &
      beta_T = 0.992127525, &
      gamma_T = 1.249173968)

  type(meos_mix_reducing), parameter :: meos_red_233 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.032807063, &
      beta_T = 1., &
      gamma_T = 1.013945424)

  type(meos_mix_reducing), parameter :: meos_red_234 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.0167)

  type(meos_mix_reducing), parameter :: meos_red_235 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "NH3", &
      beta_v = 1.006058, &
      gamma_v = 1.069834, &
      beta_T = 1.022371, &
      gamma_T = 0.940156)

  type(meos_mix_reducing), parameter :: meos_red_236 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.047298475, &
      beta_T = 1., &
      gamma_T = 1.017817492)

  type(meos_mix_reducing), parameter :: meos_red_237 = &
      meos_mix_reducing(ident1 = "CYCLOHEX", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 0.9979, &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_238 = &
      meos_mix_reducing(ident1 = "TOLUENE", &
      ident2 = "EBZN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9971283, &
      gamma_T = 1.00252)

  type(meos_mix_reducing), parameter :: meos_red_239 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "CO", &
      beta_v = 1.0000000, &
      gamma_v = 1.0013170, &
      beta_T = 1.002409, &
      gamma_T = 0.9941000)

  type(meos_mix_reducing), parameter :: meos_red_240 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_241 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_242 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_243 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.435)

  type(meos_mix_reducing), parameter :: meos_red_244 = &
      meos_mix_reducing(ident1 = "NE", &
      ident2 = "KR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.02385, &
      gamma_T = 1.1957)

  type(meos_mix_reducing), parameter :: meos_red_245 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1.214638734, &
      beta_T = 1., &
      gamma_T = 1.245039498)

  type(meos_mix_reducing), parameter :: meos_red_246 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "NC10", &
      beta_v = 1.033086292, &
      gamma_v = 1.146089637, &
      beta_T = 0.937777823, &
      gamma_T = 1.568231489)

  type(meos_mix_reducing), parameter :: meos_red_247 = &
      meos_mix_reducing(ident1 = "H2S", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.214)

  type(meos_mix_reducing), parameter :: meos_red_248 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.970873786408, &
      gamma_T = 1.117)

  type(meos_mix_reducing), parameter :: meos_red_249 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.492)

  type(meos_mix_reducing), parameter :: meos_red_250 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1.024, &
      beta_T = 1.021450459653, &
      gamma_T = 1.039)

  type(meos_mix_reducing), parameter :: meos_red_251 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9958, &
      gamma_T = 1.1004)

  type(meos_mix_reducing), parameter :: meos_red_252 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_253 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 0.881405683, &
      beta_T = 1., &
      gamma_T = 3.159776855)

  type(meos_mix_reducing), parameter :: meos_red_254 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.0007, &
      gamma_T = 0.9752)

  type(meos_mix_reducing), parameter :: meos_red_255 = &
      meos_mix_reducing(ident1 = "BENZENE", &
      ident2 = "OXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.01414, &
      gamma_T = 1.00329)

  type(meos_mix_reducing), parameter :: meos_red_256 = &
      meos_mix_reducing(ident1 = "OXYL", &
      ident2 = "NC11", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.001903616872, &
      gamma_T = 0.99825)

  type(meos_mix_reducing), parameter :: meos_red_257 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_258 = &
      meos_mix_reducing(ident1 = "TOLUENE", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.026)

  type(meos_mix_reducing), parameter :: meos_red_259 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "KR", &
      beta_v = 1., &
      gamma_v = 1.0043319, &
      beta_T = 1., &
      gamma_T = 0.9729000)

  type(meos_mix_reducing), parameter :: meos_red_260 = &
      meos_mix_reducing(ident1 = "CYCLOHEX", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.0015, &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_261 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "ACETONE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.005833836250, &
      gamma_T = 0.9221)

  type(meos_mix_reducing), parameter :: meos_red_262 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.147595688, &
      beta_T = 1., &
      gamma_T = 1.895305393)

  type(meos_mix_reducing), parameter :: meos_red_263 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.039372957, &
      beta_T = 1., &
      gamma_T = 1.010825138)

  type(meos_mix_reducing), parameter :: meos_red_264 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "H2O", &
      beta_v = 1.0281970, &
      gamma_v = 0.8734600, &
      beta_T = 1.253060, &
      gamma_T = 0.8078420)

  type(meos_mix_reducing), parameter :: meos_red_265 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.14353473, &
      beta_T = 1., &
      gamma_T = 1.05603303)

  type(meos_mix_reducing), parameter :: meos_red_266 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "NC10", &
      beta_v = 0.976951968, &
      gamma_v = 1.027845529, &
      beta_T = 0.993688386, &
      gamma_T = 1.076466918)

  type(meos_mix_reducing), parameter :: meos_red_267 = &
      meos_mix_reducing(ident1 = "MXYL", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.002270041417, &
      gamma_T = 0.98031)

  type(meos_mix_reducing), parameter :: meos_red_268 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.0023, &
      gamma_T = 1.035)

  type(meos_mix_reducing), parameter :: meos_red_269 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 0.95667731, &
      beta_T = 1., &
      gamma_T = 0.447666011)

  type(meos_mix_reducing), parameter :: meos_red_270 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_271 = &
      meos_mix_reducing(ident1 = "H2", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_272 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_273 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "C1", &
      beta_v = 1.3155340, &
      gamma_v = 1.1195433, &
      beta_T = 0.999432, &
      gamma_T = 1.1157135)

  type(meos_mix_reducing), parameter :: meos_red_274 = &
      meos_mix_reducing(ident1 = "CYCLOHEX", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.0069, &
      beta_T = 1., &
      gamma_T = 1.008)

  type(meos_mix_reducing), parameter :: meos_red_275 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 0.999, &
      beta_T = 1., &
      gamma_T = 0.991)

  type(meos_mix_reducing), parameter :: meos_red_276 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_277 = &
      meos_mix_reducing(ident1 = "NC9", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_278 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "OXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.00096, &
      gamma_T = 1.00154)

  type(meos_mix_reducing), parameter :: meos_red_279 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.119954454, &
      beta_T = 1., &
      gamma_T = 1.206043295)

  type(meos_mix_reducing), parameter :: meos_red_280 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.102764612, &
      beta_T = 1., &
      gamma_T = 1.063694129)

  type(meos_mix_reducing), parameter :: meos_red_281 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.034910633, &
      beta_T = 1., &
      gamma_T = 1.103421755)

  type(meos_mix_reducing), parameter :: meos_red_282 = &
      meos_mix_reducing(ident1 = "D2", &
      ident2 = "NE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.98882, &
      gamma_T = 0.83393)

  type(meos_mix_reducing), parameter :: meos_red_283 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "C3_1", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.0024359, &
      gamma_T = 0.98041)

  type(meos_mix_reducing), parameter :: meos_red_284 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "NH3", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9721, &
      gamma_T = 0.8483)

  type(meos_mix_reducing), parameter :: meos_red_285 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.99846, &
      gamma_T = 0.9896)

  type(meos_mix_reducing), parameter :: meos_red_286 = &
      meos_mix_reducing(ident1 = "H2S", &
      ident2 = "ACETONE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.042122595302, &
      gamma_T = 1.04871)

  type(meos_mix_reducing), parameter :: meos_red_287 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.034995284, &
      beta_T = 1., &
      gamma_T = 1.00915706)

  type(meos_mix_reducing), parameter :: meos_red_288 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.101)

  type(meos_mix_reducing), parameter :: meos_red_289 = &
      meos_mix_reducing(ident1 = "H2S", &
      ident2 = "H2O", &
      beta_v = 0.945818, &
      gamma_v = 1.189702, &
      beta_T = 0.960526, &
      gamma_T = 0.924026)

  type(meos_mix_reducing), parameter :: meos_red_290 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_291 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.99884, &
      gamma_T = 1.025)

  type(meos_mix_reducing), parameter :: meos_red_292 = &
      meos_mix_reducing(ident1 = "H2", &
      ident2 = "D2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.0087153, &
      gamma_T = 0.99511)

  type(meos_mix_reducing), parameter :: meos_red_293 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "IC4", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.048657718121, &
      gamma_T = 0.7986)

  type(meos_mix_reducing), parameter :: meos_red_294 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "N2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9525, &
      gamma_T = 1.1068)

  type(meos_mix_reducing), parameter :: meos_red_295 = &
      meos_mix_reducing(ident1 = "NC10", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_296 = &
      meos_mix_reducing(ident1 = "AR", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.45)

  type(meos_mix_reducing), parameter :: meos_red_297 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "OXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.00114, &
      gamma_T = 1.10292)

  type(meos_mix_reducing), parameter :: meos_red_298 = &
      meos_mix_reducing(ident1 = "H2", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_299 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.017880545, &
      beta_T = 1., &
      gamma_T = 1.00564748)

  type(meos_mix_reducing), parameter :: meos_red_300 = &
      meos_mix_reducing(ident1 = "H2", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_301 = &
      meos_mix_reducing(ident1 = "H2S", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_302 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 0.9826, &
      beta_T = 1., &
      gamma_T = 0.984)

  type(meos_mix_reducing), parameter :: meos_red_303 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "H2S", &
      beta_v = 1.010817909, &
      gamma_v = 1.030988277, &
      beta_T = 0.990197354, &
      gamma_T = 0.90273666)

  type(meos_mix_reducing), parameter :: meos_red_304 = &
      meos_mix_reducing(ident1 = "ETOH", &
      ident2 = "MEG", &
      beta_v = 1., &
      gamma_v = 0.9095, &
      beta_T = 1., &
      gamma_T = 1.0174)

  type(meos_mix_reducing), parameter :: meos_red_305 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "H2", &
      beta_v = 1.000000000, &
      gamma_v = 1.035170959, &
      beta_T = 1.000000000, &
      gamma_T = 1.940852069)

  type(meos_mix_reducing), parameter :: meos_red_306 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.988)

  type(meos_mix_reducing), parameter :: meos_red_307 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_308 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.155145836, &
      beta_T = 1., &
      gamma_T = 1.233272781)

  type(meos_mix_reducing), parameter :: meos_red_309 = &
      meos_mix_reducing(ident1 = "ETOH", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.059, &
      beta_T = 1.004016064257, &
      gamma_T = 0.894)

  type(meos_mix_reducing), parameter :: meos_red_310 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.00071, &
      gamma_T = 0.99711)

  type(meos_mix_reducing), parameter :: meos_red_311 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.02076168, &
      beta_T = 1., &
      gamma_T = 1.055369591)

  type(meos_mix_reducing), parameter :: meos_red_312 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 0.9979, &
      beta_T = 1., &
      gamma_T = 1.002)

  type(meos_mix_reducing), parameter :: meos_red_313 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "C2", &
      beta_v = 0.997547866, &
      gamma_v = 1.006617867, &
      beta_T = 0.996336508, &
      gamma_T = 1.049707697)

  type(meos_mix_reducing), parameter :: meos_red_314 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.130454442686, &
      gamma_T = 0.8776)

  type(meos_mix_reducing), parameter :: meos_red_315 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.008064516129, &
      gamma_T = 1.082)

  type(meos_mix_reducing), parameter :: meos_red_316 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.049219137, &
      beta_T = 1., &
      gamma_T = 1.014096448)

  type(meos_mix_reducing), parameter :: meos_red_317 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "NC9", &
      beta_v = 1.002852287, &
      gamma_v = 1.141895355, &
      beta_T = 0.947716769, &
      gamma_T = 1.528532478)

  type(meos_mix_reducing), parameter :: meos_red_318 = &
      meos_mix_reducing(ident1 = "PXYL", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.999390371873, &
      gamma_T = 0.99387)

  type(meos_mix_reducing), parameter :: meos_red_319 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "NC6", &
      beta_v = 0.958015294, &
      gamma_v = 1.052643846, &
      beta_T = 0.981844797, &
      gamma_T = 1.330570181)

  type(meos_mix_reducing), parameter :: meos_red_320 = &
      meos_mix_reducing(ident1 = "NC9", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.00081052, &
      beta_T = 1., &
      gamma_T = 1.000182392)

  type(meos_mix_reducing), parameter :: meos_red_321 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "H2S", &
      beta_v = 0.908113163, &
      gamma_v = 1.033366041, &
      beta_T = 0.985962886, &
      gamma_T = 0.926156602)

  type(meos_mix_reducing), parameter :: meos_red_322 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "H2S", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_323 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_324 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "H2S", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_325 = &
      meos_mix_reducing(ident1 = "MXYL", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.999650122457, &
      gamma_T = 0.99369)

  type(meos_mix_reducing), parameter :: meos_red_326 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "ACETONE", &
      beta_v = 1., &
      gamma_v = 0.8906, &
      beta_T = 1.016156894625, &
      gamma_T = 0.9452)

  type(meos_mix_reducing), parameter :: meos_red_327 = &
      meos_mix_reducing(ident1 = "NC9", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1.005, &
      beta_T = 0.999, &
      gamma_T = 1.01)

  type(meos_mix_reducing), parameter :: meos_red_328 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "KR", &
      beta_v = 1., &
      gamma_v = 1.0003826, &
      beta_T = 1., &
      gamma_T = 0.9930853)

  type(meos_mix_reducing), parameter :: meos_red_329 = &
      meos_mix_reducing(ident1 = "NE", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1.0260610, &
      beta_T = 1., &
      gamma_T = 1.1554168)

  type(meos_mix_reducing), parameter :: meos_red_330 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.979431929481, &
      gamma_T = 1.097)

  type(meos_mix_reducing), parameter :: meos_red_331 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "CO", &
      beta_v = 1.0338017, &
      gamma_v = 1.0001623, &
      beta_T = 0.989782, &
      gamma_T = 1.1621298)

  type(meos_mix_reducing), parameter :: meos_red_332 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "NC11", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9430937, &
      gamma_T = 1.27938)

  type(meos_mix_reducing), parameter :: meos_red_333 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_334 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "H2O", &
      beta_v = 0.9262450, &
      gamma_v = 0.7334430, &
      beta_T = 1.048054, &
      gamma_T = 0.8051470)

  type(meos_mix_reducing), parameter :: meos_red_335 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1.0266, &
      beta_T = 1.015847216579, &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_336 = &
      meos_mix_reducing(ident1 = "TOLUENE", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.0008507, &
      gamma_T = 0.99999)

  type(meos_mix_reducing), parameter :: meos_red_337 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.178828244725, &
      gamma_T = 0.9726)

  type(meos_mix_reducing), parameter :: meos_red_338 = &
      meos_mix_reducing(ident1 = "EBZN", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.003750010038, &
      gamma_T = 0.98564)

  type(meos_mix_reducing), parameter :: meos_red_339 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1.0915, &
      beta_T = 1.044, &
      gamma_T = 0.995)

  type(meos_mix_reducing), parameter :: meos_red_340 = &
      meos_mix_reducing(ident1 = "BENZENE", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.008064516129, &
      gamma_T = 1.052)

  type(meos_mix_reducing), parameter :: meos_red_341 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.992)

  type(meos_mix_reducing), parameter :: meos_red_342 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.029654036244, &
      gamma_T = 0.9299)

  type(meos_mix_reducing), parameter :: meos_red_343 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.992)

  type(meos_mix_reducing), parameter :: meos_red_344 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "NC4", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.055631795630, &
      gamma_T = 0.8067)

  type(meos_mix_reducing), parameter :: meos_red_345 = &
      meos_mix_reducing(ident1 = "H2S", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.075)

  type(meos_mix_reducing), parameter :: meos_red_346 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "NC10", &
      beta_v = 0.995676258, &
      gamma_v = 1.098361281, &
      beta_T = 0.970918061, &
      gamma_T = 1.237191558)

  type(meos_mix_reducing), parameter :: meos_red_347 = &
      meos_mix_reducing(ident1 = "ETOH", &
      ident2 = "OXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.98697, &
      gamma_T = 0.90681)

  type(meos_mix_reducing), parameter :: meos_red_348 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.021450459653, &
      gamma_T = 1.068)

  type(meos_mix_reducing), parameter :: meos_red_349 = &
      meos_mix_reducing(ident1 = "H2S", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.011, &
      gamma_T = 1.0138)

  type(meos_mix_reducing), parameter :: meos_red_350 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_351 = &
      meos_mix_reducing(ident1 = "NC9", &
      ident2 = "H2S", &
      beta_v = 1., &
      gamma_v = 1.082905109, &
      beta_T = 1., &
      gamma_T = 1.086557826)

  type(meos_mix_reducing), parameter :: meos_red_352 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "SO2", &
      beta_v = 0.8898650, &
      gamma_v = 1.0057783, &
      beta_T = 1.020063, &
      gamma_T = 1.0079753)

  type(meos_mix_reducing), parameter :: meos_red_353 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "SO2", &
      beta_v = 1.000000000, &
      gamma_v = 1.000000000, &
      beta_T = 1.004300000, &
      gamma_T = 1.220360000)

  type(meos_mix_reducing), parameter :: meos_red_354 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.106194690265, &
      gamma_T = 0.8385)

  type(meos_mix_reducing), parameter :: meos_red_355 = &
      meos_mix_reducing(ident1 = "ETOH", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.982318271120, &
      gamma_T = 0.904)

  type(meos_mix_reducing), parameter :: meos_red_356 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "IC5", &
      beta_v = 1.040459289, &
      gamma_v = 0.999432118, &
      beta_T = 0.994364425, &
      gamma_T = 1.0032695)

  type(meos_mix_reducing), parameter :: meos_red_357 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "AR", &
      beta_v = 1.000000000, &
      gamma_v = 1.159720623, &
      beta_T = 1.000000000, &
      gamma_T = 0.954215746)

  type(meos_mix_reducing), parameter :: meos_red_358 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_359 = &
      meos_mix_reducing(ident1 = "BENZENE", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.016, &
      beta_T = 1., &
      gamma_T = 0.994)

  type(meos_mix_reducing), parameter :: meos_red_360 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "H2S", &
      beta_v = 0.754473958, &
      gamma_v = 1.339283552, &
      beta_T = 0.985891113, &
      gamma_T = 0.956075596)

  type(meos_mix_reducing), parameter :: meos_red_361 = &
      meos_mix_reducing(ident1 = "BENZENE", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.9933)

  type(meos_mix_reducing), parameter :: meos_red_362 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "NC8", &
      beta_v = 1.007469726, &
      gamma_v = 1.071917985, &
      beta_T = 0.984068272, &
      gamma_T = 1.168636194)

  type(meos_mix_reducing), parameter :: meos_red_363 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.079648053, &
      beta_T = 1., &
      gamma_T = 1.050044169)

  type(meos_mix_reducing), parameter :: meos_red_364 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.011, &
      gamma_T = 1.022)

  type(meos_mix_reducing), parameter :: meos_red_365 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1.038, &
      beta_T = 0.991, &
      gamma_T = 1.027)

  type(meos_mix_reducing), parameter :: meos_red_366 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.258)

  type(meos_mix_reducing), parameter :: meos_red_367 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.010493989, &
      beta_T = 1., &
      gamma_T = 1.006018054)

  type(meos_mix_reducing), parameter :: meos_red_368 = &
      meos_mix_reducing(ident1 = "CYCLOHEX", &
      ident2 = "OXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.00392, &
      gamma_T = 0.99006)

  type(meos_mix_reducing), parameter :: meos_red_369 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9917, &
      gamma_T = 1.4608)

  type(meos_mix_reducing), parameter :: meos_red_370 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "AR", &
      beta_v = 1.000000000, &
      gamma_v = 1.021291140, &
      beta_T = 1.000000000, &
      gamma_T = 1.141020219)

  type(meos_mix_reducing), parameter :: meos_red_371 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.524)

  type(meos_mix_reducing), parameter :: meos_red_372 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "HE", &
      beta_v = 0.846647561, &
      gamma_v = 0.864141549, &
      beta_T = 0.76837763, &
      gamma_T = 3.207456948)

  type(meos_mix_reducing), parameter :: meos_red_373 = &
      meos_mix_reducing(ident1 = "BENZENE", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.005025125628, &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_374 = &
      meos_mix_reducing(ident1 = "XE", &
      ident2 = "C2", &
      beta_v = 1., &
      gamma_v = 1.0034307, &
      beta_T = 1., &
      gamma_T = 1.0112641)

  type(meos_mix_reducing), parameter :: meos_red_375 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "EBZN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.00091, &
      gamma_T = 1.09145)

  type(meos_mix_reducing), parameter :: meos_red_376 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.087272232, &
      beta_T = 1., &
      gamma_T = 1.161390082)

  type(meos_mix_reducing), parameter :: meos_red_377 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.100405929, &
      beta_T = 0.95637945, &
      gamma_T = 1.749119996)

  type(meos_mix_reducing), parameter :: meos_red_378 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.9961)

  type(meos_mix_reducing), parameter :: meos_red_379 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "IC4", &
      beta_v = 0.999243146, &
      gamma_v = 1.001156119, &
      beta_T = 0.998012298, &
      gamma_T = 1.005250774)

  type(meos_mix_reducing), parameter :: meos_red_380 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.018)

  type(meos_mix_reducing), parameter :: meos_red_381 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.116694577, &
      beta_T = 1., &
      gamma_T = 1.199326059)

  type(meos_mix_reducing), parameter :: meos_red_382 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_383 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_384 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.981810006018, &
      gamma_T = 1.62153)

  type(meos_mix_reducing), parameter :: meos_red_385 = &
      meos_mix_reducing(ident1 = "EBZN", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.000120014402, &
      gamma_T = 0.99939)

  type(meos_mix_reducing), parameter :: meos_red_386 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.0563, &
      beta_T = 1.044, &
      gamma_T = 0.9779)

  type(meos_mix_reducing), parameter :: meos_red_387 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.057872566, &
      beta_T = 1., &
      gamma_T = 1.025657518)

  type(meos_mix_reducing), parameter :: meos_red_388 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.011330031476, &
      gamma_T = 1.02114)

  type(meos_mix_reducing), parameter :: meos_red_389 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "HE", &
      beta_v = 0.969501055, &
      gamma_v = 0.932629867, &
      beta_T = 0.692868765, &
      gamma_T = 1.47183158)

  type(meos_mix_reducing), parameter :: meos_red_390 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 0.599484191, &
      beta_T = 1., &
      gamma_T = 0.662072469)

  type(meos_mix_reducing), parameter :: meos_red_391 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "IC4", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.971817298348, &
      gamma_T = 1.014)

  type(meos_mix_reducing), parameter :: meos_red_392 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "NC5", &
      beta_v = 1., &
      gamma_v = 1.002779804, &
      beta_T = 1., &
      gamma_T = 1.002495889)

  type(meos_mix_reducing), parameter :: meos_red_393 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "H2", &
      beta_v = 1.086000, &
      gamma_v = 0.804000, &
      beta_T = 1.010000, &
      gamma_T = 1.440000)

  type(meos_mix_reducing), parameter :: meos_red_394 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "SO2", &
      beta_v = 1.000000000, &
      gamma_v = 1.000000000, &
      beta_T = 1.015760000, &
      gamma_T = 1.084560000)

  type(meos_mix_reducing), parameter :: meos_red_395 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_396 = &
      meos_mix_reducing(ident1 = "H2", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_397 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "C3", &
      beta_v = 0.974424681, &
      gamma_v = 1.081025408, &
      beta_T = 1.002677329, &
      gamma_T = 1.201264026)

  type(meos_mix_reducing), parameter :: meos_red_398 = &
      meos_mix_reducing(ident1 = "ETOH", &
      ident2 = "BENZENE", &
      beta_v = 0.997008973081, &
      gamma_v = 0.97, &
      beta_T = 0.981354268891, &
      gamma_T = 0.92)

  type(meos_mix_reducing), parameter :: meos_red_399 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "IC4", &
      beta_v = 1.076551882, &
      gamma_v = 1.081909003, &
      beta_T = 1.023339824, &
      gamma_T = 0.929982936)

  type(meos_mix_reducing), parameter :: meos_red_400 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1.0835, &
      beta_T = 1., &
      gamma_T = 1.3145)

  type(meos_mix_reducing), parameter :: meos_red_401 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_402 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.108143673, &
      beta_T = 1., &
      gamma_T = 1.197564208)

  type(meos_mix_reducing), parameter :: meos_red_403 = &
      meos_mix_reducing(ident1 = "TOLUENE", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.004944326084, &
      gamma_T = 1.01727)

  type(meos_mix_reducing), parameter :: meos_red_404 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "H2O", &
      beta_v = 1.008091, &
      gamma_v = 0.923029, &
      beta_T = 0.98882, &
      gamma_T = 0.925278)

  type(meos_mix_reducing), parameter :: meos_red_405 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.0466, &
      beta_T = 1.0246, &
      gamma_T = 0.9409)

  type(meos_mix_reducing), parameter :: meos_red_406 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "NC9", &
      beta_v = 1., &
      gamma_v = 1.001370076, &
      beta_T = 1., &
      gamma_T = 1.001150096)

  type(meos_mix_reducing), parameter :: meos_red_407 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "O2", &
      beta_v = 1.0000000, &
      gamma_v = 1.0844600, &
      beta_T = 1.000000, &
      gamma_T = 1.0319860)

  type(meos_mix_reducing), parameter :: meos_red_408 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "H2O", &
      beta_v = 1.094032, &
      gamma_v = 0.962547, &
      beta_T = 1.019562, &
      gamma_T = 0.916311)

  type(meos_mix_reducing), parameter :: meos_red_409 = &
      meos_mix_reducing(ident1 = "HE", &
      ident2 = "KR", &
      beta_v = 1.052, &
      gamma_v = 2.016, &
      beta_T = 0.790, &
      gamma_T = 2.013)

  type(meos_mix_reducing), parameter :: meos_red_410 = &
      meos_mix_reducing(ident1 = "F6S", &
      ident2 = "NC5", &
      beta_v = 1., &
      gamma_v = 1.0605, &
      beta_T = 1.041666666667, &
      gamma_T = 0.9293)

  type(meos_mix_reducing), parameter :: meos_red_411 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.3883)

  type(meos_mix_reducing), parameter :: meos_red_412 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "NC6", &
      beta_v = 1., &
      gamma_v = 1.002995876, &
      beta_T = 1., &
      gamma_T = 1.001204174)

  type(meos_mix_reducing), parameter :: meos_red_413 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.015537727227, &
      gamma_T = 1.0224)

  type(meos_mix_reducing), parameter :: meos_red_414 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "NC11", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.0061476, &
      gamma_T = 1.19119)

  type(meos_mix_reducing), parameter :: meos_red_415 = &
      meos_mix_reducing(ident1 = "ETOH", &
      ident2 = "H2O", &
      beta_v = 1.0124, &
      gamma_v = 0.9558, &
      beta_T = 0.9866, &
      gamma_T = 0.9971)

  type(meos_mix_reducing), parameter :: meos_red_416 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "OXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.99904, &
      gamma_T = 0.98672)

  type(meos_mix_reducing), parameter :: meos_red_417 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "NC4", &
      beta_v = 0.999795868, &
      gamma_v = 1.003264179, &
      beta_T = 1.000310289, &
      gamma_T = 1.007392782)

  type(meos_mix_reducing), parameter :: meos_red_418 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "NC11", &
      beta_v = 1., &
      gamma_v = 1.075, &
      beta_T = 1.044, &
      gamma_T = 0.98)

  type(meos_mix_reducing), parameter :: meos_red_419 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "NC4", &
      beta_v = 0.99608261, &
      gamma_v = 1.146949309, &
      beta_T = 0.994515234, &
      gamma_T = 1.304886838)

  type(meos_mix_reducing), parameter :: meos_red_420 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "N2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.98876, &
      gamma_T = 1.02354)

  type(meos_mix_reducing), parameter :: meos_red_421 = &
      meos_mix_reducing(ident1 = "NE", &
      ident2 = "N2", &
      beta_v = 0.915750915751, &
      gamma_v = 0.988, &
      beta_T = 1.025641025641, &
      gamma_T = 1.148)

  type(meos_mix_reducing), parameter :: meos_red_422 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "H2", &
      beta_v = 0.993000, &
      gamma_v = 0.773000, &
      beta_T = 1.027000, &
      gamma_T = 1.240000)

  type(meos_mix_reducing), parameter :: meos_red_423 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.231678778175, &
      gamma_T = 1.0676)

  type(meos_mix_reducing), parameter :: meos_red_424 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "IC4", &
      beta_v = 1.011240388, &
      gamma_v = 1.054319053, &
      beta_T = 0.980315756, &
      gamma_T = 1.161117729)

  type(meos_mix_reducing), parameter :: meos_red_425 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "C3", &
      beta_v = 0.996898004, &
      gamma_v = 1.047596298, &
      beta_T = 1.033620538, &
      gamma_T = 0.908772477)

  type(meos_mix_reducing), parameter :: meos_red_426 = &
      meos_mix_reducing(ident1 = "CYCLOHEX", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.00184, &
      gamma_T = 0.99198)

  type(meos_mix_reducing), parameter :: meos_red_427 = &
      meos_mix_reducing(ident1 = "KR", &
      ident2 = "NH3", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.011818, &
      gamma_T = 0.92419)

  type(meos_mix_reducing), parameter :: meos_red_428 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_429 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "AR", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_430 = &
      meos_mix_reducing(ident1 = "EBZN", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.999479970571, &
      gamma_T = 0.99905)

  type(meos_mix_reducing), parameter :: meos_red_431 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1.047, &
      beta_T = 1., &
      gamma_T = 1.114)

  type(meos_mix_reducing), parameter :: meos_red_432 = &
      meos_mix_reducing(ident1 = "P-H2", &
      ident2 = "O-H2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_433 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "ACETONE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.9177)

  type(meos_mix_reducing), parameter :: meos_red_434 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.0352, &
      beta_T = 1.0196, &
      gamma_T = 0.9277)

  type(meos_mix_reducing), parameter :: meos_red_435 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "H2O", &
      beta_v = 1.044759, &
      gamma_v = 1.189754, &
      beta_T = 0.933585, &
      gamma_T = 1.015826)

  type(meos_mix_reducing), parameter :: meos_red_436 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_437 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "O2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_438 = &
      meos_mix_reducing(ident1 = "H2", &
      ident2 = "NE", &
      beta_v = 0.8285, &
      gamma_v = 1.2007, &
      beta_T = 1.00705, &
      gamma_T = 0.7819)

  type(meos_mix_reducing), parameter :: meos_red_439 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.129)

  type(meos_mix_reducing), parameter :: meos_red_440 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "MXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.984390035020, &
      gamma_T = 1.5975)

  type(meos_mix_reducing), parameter :: meos_red_441 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1.02, &
      beta_T = 1., &
      gamma_T = 1.019)

  type(meos_mix_reducing), parameter :: meos_red_442 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "CO2", &
      beta_v = 0.999518072, &
      gamma_v = 1.002806594, &
      beta_T = 1.02262449, &
      gamma_T = 0.975665369)

  type(meos_mix_reducing), parameter :: meos_red_443 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_444 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.002553544, &
      beta_T = 1., &
      gamma_T = 1.007186267)

  type(meos_mix_reducing), parameter :: meos_red_445 = &
      meos_mix_reducing(ident1 = "H2S", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_446 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "H2", &
      beta_v = 1., &
      gamma_v = 1.305249405, &
      beta_T = 1., &
      gamma_T = 2.191555216)

  type(meos_mix_reducing), parameter :: meos_red_447 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 0.978, &
      beta_T = 0.904, &
      gamma_T = 1.716)

  type(meos_mix_reducing), parameter :: meos_red_448 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "C2", &
      beta_v = 1.002525718, &
      gamma_v = 1.032876701, &
      beta_T = 1.013871147, &
      gamma_T = 0.90094953)

  type(meos_mix_reducing), parameter :: meos_red_449 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_450 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "NC7", &
      beta_v = 1., &
      gamma_v = 1.009928206, &
      beta_T = 1., &
      gamma_T = 1.003194615)

  type(meos_mix_reducing), parameter :: meos_red_451 = &
      meos_mix_reducing(ident1 = "H2S", &
      ident2 = "MEG", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.9877, &
      gamma_T = 1.1981)

  type(meos_mix_reducing), parameter :: meos_red_452 = &
      meos_mix_reducing(ident1 = "N2", &
      ident2 = "IC5", &
      beta_v = 1., &
      gamma_v = 1.154135439, &
      beta_T = 1., &
      gamma_T = 1.38177077)

  type(meos_mix_reducing), parameter :: meos_red_453 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "CYCLOHEX", &
      beta_v = 1.19, &
      gamma_v = 1.0106, &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_454 = &
      meos_mix_reducing(ident1 = "TOLUENE", &
      ident2 = "OXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.001743, &
      gamma_T = 1.00209)

  type(meos_mix_reducing), parameter :: meos_red_455 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "NC8", &
      beta_v = 1., &
      gamma_v = 1.069223964, &
      beta_T = 1., &
      gamma_T = 1.016422347)

  type(meos_mix_reducing), parameter :: meos_red_456 = &
      meos_mix_reducing(ident1 = "PXYL", &
      ident2 = "NC11", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.000190036107, &
      gamma_T = 1.0018)

  type(meos_mix_reducing), parameter :: meos_red_457 = &
      meos_mix_reducing(ident1 = "ACETONE", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 0.997, &
      beta_T = 1.005, &
      gamma_T = 0.975)

  type(meos_mix_reducing), parameter :: meos_red_458 = &
      meos_mix_reducing(ident1 = "NC4", &
      ident2 = "BENZENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_459 = &
      meos_mix_reducing(ident1 = "C1", &
      ident2 = "CO", &
      beta_v = 0.997340772, &
      gamma_v = 1.006102927, &
      beta_T = 0.987411732, &
      gamma_T = 0.987473033)

  type(meos_mix_reducing), parameter :: meos_red_460 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "H2O", &
      beta_v = 1.0213920, &
      gamma_v = 0.8951560, &
      beta_T = 1.030538, &
      gamma_T = 0.8284720)

  type(meos_mix_reducing), parameter :: meos_red_461 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "IC5", &
      beta_v = 1.060793104, &
      gamma_v = 1.116793198, &
      beta_T = 1.019180957, &
      gamma_T = 0.961218039)

  type(meos_mix_reducing), parameter :: meos_red_462 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "ETOH", &
      beta_v = 1., &
      gamma_v = 1.025, &
      beta_T = 1.028, &
      gamma_T = 0.923)

  type(meos_mix_reducing), parameter :: meos_red_463 = &
      meos_mix_reducing(ident1 = "XE", &
      ident2 = "F6S", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.0366024, &
      gamma_T = 0.92846)

  type(meos_mix_reducing), parameter :: meos_red_464 = &
      meos_mix_reducing(ident1 = "XE", &
      ident2 = "C3", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.995113990308, &
      gamma_T = 1.04133)

  type(meos_mix_reducing), parameter :: meos_red_465 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "H2S", &
      beta_v = 0.984613203, &
      gamma_v = 1.076539234, &
      beta_T = 0.962006651, &
      gamma_T = 0.959065662)

  type(meos_mix_reducing), parameter :: meos_red_466 = &
      meos_mix_reducing(ident1 = "SO2", &
      ident2 = "H2S", &
      beta_v = 1.000000000, &
      gamma_v = 1.000000000, &
      beta_T = 1.000000000, &
      gamma_T = 1.000000000)

  type(meos_mix_reducing), parameter :: meos_red_467 = &
      meos_mix_reducing(ident1 = "KR", &
      ident2 = "XE", &
      beta_v = 1., &
      gamma_v = 1.0054519, &
      beta_T = 1., &
      gamma_T = 1.0196370)

  type(meos_mix_reducing), parameter :: meos_red_468 = &
      meos_mix_reducing(ident1 = "CO", &
      ident2 = "H2S", &
      beta_v = 0.795660392, &
      gamma_v = 1.101731308, &
      beta_T = 1.025536736, &
      gamma_T = 1.022749748)

  type(meos_mix_reducing), parameter :: meos_red_469 = &
      meos_mix_reducing(ident1 = "NH3", &
      ident2 = "H2S", &
      beta_v = 1.000000, &
      gamma_v = 1.000000, &
      beta_T = 1.000000, &
      gamma_T = 1.000000)

  type(meos_mix_reducing), parameter :: meos_red_470 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "ACETONE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.019919, &
      gamma_T = 0.99014)

  type(meos_mix_reducing), parameter :: meos_red_471 = &
      meos_mix_reducing(ident1 = "O2", &
      ident2 = "SO2", &
      beta_v = 1.000000000, &
      gamma_v = 1.000000000, &
      beta_T = 0.991148000, &
      gamma_T = 1.173450000)

  type(meos_mix_reducing), parameter :: meos_red_472 = &
      meos_mix_reducing(ident1 = "CO2", &
      ident2 = "NC4", &
      beta_v = 1.174760923, &
      gamma_v = 1.222437324, &
      beta_T = 1.018171004, &
      gamma_T = 0.911498231)

  type(meos_mix_reducing), parameter :: meos_red_473 = &
      meos_mix_reducing(ident1 = "NC5", &
      ident2 = "CYCLOHEX", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.9961)

  type(meos_mix_reducing), parameter :: meos_red_474 = &
      meos_mix_reducing(ident1 = "NC7", &
      ident2 = "PXYL", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 0.99936, &
      gamma_T = 0.98957)

  type(meos_mix_reducing), parameter :: meos_red_475 = &
      meos_mix_reducing(ident1 = "IC4", &
      ident2 = "H2O", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_476 = &
      meos_mix_reducing(ident1 = "IC5", &
      ident2 = "NC12", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.13)

  type(meos_mix_reducing), parameter :: meos_red_477 = &
      meos_mix_reducing(ident1 = "C2", &
      ident2 = "CO", &
      beta_v = 1., &
      gamma_v = 1.201417898, &
      beta_T = 1., &
      gamma_T = 1.069224728)

  type(meos_mix_reducing), parameter :: meos_red_478 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "SO2", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 0.8738)

  type(meos_mix_reducing), parameter :: meos_red_479 = &
      meos_mix_reducing(ident1 = "PRLN", &
      ident2 = "NC5", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.04)

  type(meos_mix_reducing), parameter :: meos_red_480 = &
      meos_mix_reducing(ident1 = "C3", &
      ident2 = "TOLUENE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.049)

  type(meos_mix_reducing), parameter :: meos_red_481 = &
      meos_mix_reducing(ident1 = "NC8", &
      ident2 = "HE", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1., &
      gamma_T = 1.)

  type(meos_mix_reducing), parameter :: meos_red_482 = &
      meos_mix_reducing(ident1 = "NC6", &
      ident2 = "EBZN", &
      beta_v = 1., &
      gamma_v = 1., &
      beta_T = 1.0002601, &
      gamma_T = 0.99383)

  type(meos_mix_reducing), parameter :: meos_red_483 = &
      meos_mix_reducing(ident1 = "CYCLOHEX", &
      ident2 = "NC10", &
      beta_v = 1., &
      gamma_v = 1.012, &
      beta_T = 1., &
      gamma_T = 1.025)

  integer, parameter :: max_meos_mix_reducing = 483
  type (meos_mix_reducing), dimension(max_meos_mix_reducing), parameter :: meos_mix_reducingdb = (/&
      meos_red_1,meos_red_2,meos_red_3,meos_red_4,meos_red_5,meos_red_6, &
      meos_red_7,meos_red_8,meos_red_9,meos_red_10,meos_red_11,meos_red_12, &
      meos_red_13,meos_red_14,meos_red_15,meos_red_16,meos_red_17,meos_red_18, &
      meos_red_19,meos_red_20,meos_red_21,meos_red_22,meos_red_23,meos_red_24, &
      meos_red_25,meos_red_26,meos_red_27,meos_red_28,meos_red_29,meos_red_30, &
      meos_red_31,meos_red_32,meos_red_33,meos_red_34,meos_red_35,meos_red_36, &
      meos_red_37,meos_red_38,meos_red_39,meos_red_40,meos_red_41,meos_red_42, &
      meos_red_43,meos_red_44,meos_red_45,meos_red_46,meos_red_47,meos_red_48, &
      meos_red_49,meos_red_50,meos_red_51,meos_red_52,meos_red_53,meos_red_54, &
      meos_red_55,meos_red_56,meos_red_57,meos_red_58,meos_red_59,meos_red_60, &
      meos_red_61,meos_red_62,meos_red_63,meos_red_64,meos_red_65,meos_red_66, &
      meos_red_67,meos_red_68,meos_red_69,meos_red_70,meos_red_71,meos_red_72, &
      meos_red_73,meos_red_74,meos_red_75,meos_red_76,meos_red_77,meos_red_78, &
      meos_red_79,meos_red_80,meos_red_81,meos_red_82,meos_red_83,meos_red_84, &
      meos_red_85,meos_red_86,meos_red_87,meos_red_88,meos_red_89,meos_red_90, &
      meos_red_91,meos_red_92,meos_red_93,meos_red_94,meos_red_95,meos_red_96, &
      meos_red_97,meos_red_98,meos_red_99,meos_red_100,meos_red_101,meos_red_102, &
      meos_red_103,meos_red_104,meos_red_105,meos_red_106,meos_red_107,meos_red_108, &
      meos_red_109,meos_red_110,meos_red_111,meos_red_112,meos_red_113,meos_red_114, &
      meos_red_115,meos_red_116,meos_red_117,meos_red_118,meos_red_119,meos_red_120, &
      meos_red_121,meos_red_122,meos_red_123,meos_red_124,meos_red_125,meos_red_126, &
      meos_red_127,meos_red_128,meos_red_129,meos_red_130,meos_red_131,meos_red_132, &
      meos_red_133,meos_red_134,meos_red_135,meos_red_136,meos_red_137,meos_red_138, &
      meos_red_139,meos_red_140,meos_red_141,meos_red_142,meos_red_143,meos_red_144, &
      meos_red_145,meos_red_146,meos_red_147,meos_red_148,meos_red_149,meos_red_150, &
      meos_red_151,meos_red_152,meos_red_153,meos_red_154,meos_red_155,meos_red_156, &
      meos_red_157,meos_red_158,meos_red_159,meos_red_160,meos_red_161,meos_red_162, &
      meos_red_163,meos_red_164,meos_red_165,meos_red_166,meos_red_167,meos_red_168, &
      meos_red_169,meos_red_170,meos_red_171,meos_red_172,meos_red_173,meos_red_174, &
      meos_red_175,meos_red_176,meos_red_177,meos_red_178,meos_red_179,meos_red_180, &
      meos_red_181,meos_red_182,meos_red_183,meos_red_184,meos_red_185,meos_red_186, &
      meos_red_187,meos_red_188,meos_red_189,meos_red_190,meos_red_191,meos_red_192, &
      meos_red_193,meos_red_194,meos_red_195,meos_red_196,meos_red_197,meos_red_198, &
      meos_red_199,meos_red_200,meos_red_201,meos_red_202,meos_red_203,meos_red_204, &
      meos_red_205,meos_red_206,meos_red_207,meos_red_208,meos_red_209,meos_red_210, &
      meos_red_211,meos_red_212,meos_red_213,meos_red_214,meos_red_215,meos_red_216, &
      meos_red_217,meos_red_218,meos_red_219,meos_red_220,meos_red_221,meos_red_222, &
      meos_red_223,meos_red_224,meos_red_225,meos_red_226,meos_red_227,meos_red_228, &
      meos_red_229,meos_red_230,meos_red_231,meos_red_232,meos_red_233,meos_red_234, &
      meos_red_235,meos_red_236,meos_red_237,meos_red_238,meos_red_239,meos_red_240, &
      meos_red_241,meos_red_242,meos_red_243,meos_red_244,meos_red_245,meos_red_246, &
      meos_red_247,meos_red_248,meos_red_249,meos_red_250,meos_red_251,meos_red_252, &
      meos_red_253,meos_red_254,meos_red_255,meos_red_256,meos_red_257,meos_red_258, &
      meos_red_259,meos_red_260,meos_red_261,meos_red_262,meos_red_263,meos_red_264, &
      meos_red_265,meos_red_266,meos_red_267,meos_red_268,meos_red_269,meos_red_270, &
      meos_red_271,meos_red_272,meos_red_273,meos_red_274,meos_red_275,meos_red_276, &
      meos_red_277,meos_red_278,meos_red_279,meos_red_280,meos_red_281,meos_red_282, &
      meos_red_283,meos_red_284,meos_red_285,meos_red_286,meos_red_287,meos_red_288, &
      meos_red_289,meos_red_290,meos_red_291,meos_red_292,meos_red_293,meos_red_294, &
      meos_red_295,meos_red_296,meos_red_297,meos_red_298,meos_red_299,meos_red_300, &
      meos_red_301,meos_red_302,meos_red_303,meos_red_304,meos_red_305,meos_red_306, &
      meos_red_307,meos_red_308,meos_red_309,meos_red_310,meos_red_311,meos_red_312, &
      meos_red_313,meos_red_314,meos_red_315,meos_red_316,meos_red_317,meos_red_318, &
      meos_red_319,meos_red_320,meos_red_321,meos_red_322,meos_red_323,meos_red_324, &
      meos_red_325,meos_red_326,meos_red_327,meos_red_328,meos_red_329,meos_red_330, &
      meos_red_331,meos_red_332,meos_red_333,meos_red_334,meos_red_335,meos_red_336, &
      meos_red_337,meos_red_338,meos_red_339,meos_red_340,meos_red_341,meos_red_342, &
      meos_red_343,meos_red_344,meos_red_345,meos_red_346,meos_red_347,meos_red_348, &
      meos_red_349,meos_red_350,meos_red_351,meos_red_352,meos_red_353,meos_red_354, &
      meos_red_355,meos_red_356,meos_red_357,meos_red_358,meos_red_359,meos_red_360, &
      meos_red_361,meos_red_362,meos_red_363,meos_red_364,meos_red_365,meos_red_366, &
      meos_red_367,meos_red_368,meos_red_369,meos_red_370,meos_red_371,meos_red_372, &
      meos_red_373,meos_red_374,meos_red_375,meos_red_376,meos_red_377,meos_red_378, &
      meos_red_379,meos_red_380,meos_red_381,meos_red_382,meos_red_383,meos_red_384, &
      meos_red_385,meos_red_386,meos_red_387,meos_red_388,meos_red_389,meos_red_390, &
      meos_red_391,meos_red_392,meos_red_393,meos_red_394,meos_red_395,meos_red_396, &
      meos_red_397,meos_red_398,meos_red_399,meos_red_400,meos_red_401,meos_red_402, &
      meos_red_403,meos_red_404,meos_red_405,meos_red_406,meos_red_407,meos_red_408, &
      meos_red_409,meos_red_410,meos_red_411,meos_red_412,meos_red_413,meos_red_414, &
      meos_red_415,meos_red_416,meos_red_417,meos_red_418,meos_red_419,meos_red_420, &
      meos_red_421,meos_red_422,meos_red_423,meos_red_424,meos_red_425,meos_red_426, &
      meos_red_427,meos_red_428,meos_red_429,meos_red_430,meos_red_431,meos_red_432, &
      meos_red_433,meos_red_434,meos_red_435,meos_red_436,meos_red_437,meos_red_438, &
      meos_red_439,meos_red_440,meos_red_441,meos_red_442,meos_red_443,meos_red_444, &
      meos_red_445,meos_red_446,meos_red_447,meos_red_448,meos_red_449,meos_red_450, &
      meos_red_451,meos_red_452,meos_red_453,meos_red_454,meos_red_455,meos_red_456, &
      meos_red_457,meos_red_458,meos_red_459,meos_red_460,meos_red_461,meos_red_462, &
      meos_red_463,meos_red_464,meos_red_465,meos_red_466,meos_red_467,meos_red_468, &
      meos_red_469,meos_red_470,meos_red_471,meos_red_472,meos_red_473,meos_red_474, &
      meos_red_475,meos_red_476,meos_red_477,meos_red_478,meos_red_479,meos_red_480, &
      meos_red_481,meos_red_482,meos_red_483 &
      /)

  type(meos_mix_data), parameter :: meos_mix1 = &
      meos_mix_data(ident1 = "H2", &
      ident2 = "NH3", &
      Fij = 1.000000, &
      num_mix = 4, &
      n_mix = (/ &
      -3.73558,-7.47092,1.98413, &
      1.87191,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.2800000000,2.0500000000,2.6000000000, &
      3.1300000000,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,2,1,2,0,0, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      1,1,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,-0.6100000000, &
      -1.6000000000,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.8000000000, &
      1.6200000000,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,-2.0600000000, &
      -1.7400000000,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.7900000000, &
      2.1000000000,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 2, &
      num_gauss = 2)

  type(meos_mix_data), parameter :: meos_mix2 = &
      meos_mix_data(ident1 = "N2", &
      ident2 = "BENZENE", &
      Fij = 0.257, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix3 = &
      meos_mix_data(ident1 = "CO2", &
      ident2 = "H2", &
      Fij = 1.000000, &
      num_mix = 6, &
      n_mix = (/ &
      0.356000000000D+01,-0.103600000000D+01,-0.483500000000D+01, &
      0.123800000000D+02,-0.265000000000D+01,-0.330000000000D+01, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.4700000000,1.1700000000,1.9500000000, &
      0.3100000000,1.4120000000,2.2800000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,2,1,2,3,1, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,-0.5800000000, &
      -0.2000000000,-0.2920000000,-0.1200000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.5200000000, &
      0.1500000000,0.2400000000,0.1500000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,-0.4650000000, &
      -0.8200000000,-0.5200000000,-1.0000000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.1700000000, &
      2.1100000000,1.4900000000,1.7300000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 4)

  type(meos_mix_data), parameter :: meos_mix4 = &
      meos_mix_data(ident1 = "C1", &
      ident2 = "ETOH", &
      Fij = 1.486, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix5 = &
      meos_mix_data(ident1 = "C2", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix6 = &
      meos_mix_data(ident1 = "TOLUENE", &
      ident2 = "NC9", &
      Fij = -1.06, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix7 = &
      meos_mix_data(ident1 = "NH3", &
      ident2 = "CYCLOHEX", &
      Fij = -0.08997, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix8 = &
      meos_mix_data(ident1 = "NC10", &
      ident2 = "NC12", &
      Fij = 0.35, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix9 = &
      meos_mix_data(ident1 = "C2", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix10 = &
      meos_mix_data(ident1 = "NE", &
      ident2 = "XE", &
      Fij = 1., &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix11 = &
      meos_mix_data(ident1 = "ETOH", &
      ident2 = "CYCLOHEX", &
      Fij = 1., &
      num_mix = 7, &
      n_mix = (/ &
      -2.05560,1.94512,-0.196687, &
      0.425412,-1.01883,-0.786737, &
      -0.229943,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      4.5,4.7,4.68, &
      2.5,4.68,5.5, &
      8.2,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,2,3,4,4, &
      5,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,0,0,1,1,2, &
      2,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 5, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix12 = &
      meos_mix_data(ident1 = "CO", &
      ident2 = "H2", &
      Fij = 1.000000, &
      num_mix = 4, &
      n_mix = (/ &
      -0.521000000000D+00,-0.387000000000D+00,-0.259000000000D+01, &
      0.435000000000D+01,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      2.2500000000,0.4730000000,0.5850000000, &
      0.0910000000,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,2,1,2,0,0, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,-0.6470000000, &
      -0.3440000000,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,1.3800000000, &
      0.7730000000,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,-0.7510000000, &
      -0.6600000000,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,1.8600000000, &
      2.2300000000,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 2)

  type(meos_mix_data), parameter :: meos_mix13 = &
      meos_mix_data(ident1 = "N2", &
      ident2 = "ETOH", &
      Fij = 0.778, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix14 = &
      meos_mix_data(ident1 = "CO2", &
      ident2 = "BENZENE", &
      Fij = 2.894, &
      num_mix = 9, &
      n_mix = (/ &
      0.013746429958576,-0.0074425012129552,-0.0045516600213685, &
      -0.0054546603350237,0.0023682016824471,0.18007763721438, &
      -0.44773942932486,0.019327374888200,-0.30632197804624, &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.25, &
      0.25,0.0,0.0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.75, &
      1.0,2.0,3.0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 4)

  type(meos_mix_data), parameter :: meos_mix15 = &
      meos_mix_data(ident1 = "ACETONE", &
      ident2 = "CYCLOHEX", &
      Fij = -0.5274, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix16 = &
      meos_mix_data(ident1 = "C1", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix17 = &
      meos_mix_data(ident1 = "PRLN", &
      ident2 = "H2O", &
      Fij = 0.7604, &
      num_mix = 6, &
      n_mix = (/ &
      1.09765,1.94679,-2.16809, &
      -0.137077,0.0486690,1.04024, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      0.26,7.3,5.3, &
      2.3,0.7,3.3, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      2,3,5,5,7,6, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      1,2,2,1,1,2, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 6, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix18 = &
      meos_mix_data(ident1 = "CO2", &
      ident2 = "PRLN", &
      Fij = -0.362, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix19 = &
      meos_mix_data(ident1 = "C1", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,1., &
      1.,0.25,0., &
      0.,0.,0., &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,1., &
      1.,2.5,3., &
      3.,3.,3., &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 7)

  type(meos_mix_data), parameter :: meos_mix20 = &
      meos_mix_data(ident1 = "NC4", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix21 = &
      meos_mix_data(ident1 = "CYCLOHEX", &
      ident2 = "NC12", &
      Fij = -0.358, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix22 = &
      meos_mix_data(ident1 = "AR", &
      ident2 = "NH3", &
      Fij = 1., &
      num_mix = 3, &
      n_mix = (/ &
      0.02350785,-1.913776,1.624062, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      2.3,1.65,0.42, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      3,1,1,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      1,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,-1.3,-1.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.31,0.39, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,-0.6,-0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.9,1.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 1, &
      num_gauss = 2)

  type(meos_mix_data), parameter :: meos_mix23 = &
      meos_mix_data(ident1 = "N2", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      1.,1.,0.875, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      1.,1.,1.25, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 3)

  type(meos_mix_data), parameter :: meos_mix24 = &
      meos_mix_data(ident1 = "CYCLOHEX", &
      ident2 = "NC9", &
      Fij = -0.358, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix25 = &
      meos_mix_data(ident1 = "BENZENE", &
      ident2 = "TOLUENE", &
      Fij = 0.068, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix26 = &
      meos_mix_data(ident1 = "NC7", &
      ident2 = "TOLUENE", &
      Fij = -0.6281548, &
      num_mix = 12, &
      n_mix = (/ &
      -0.00080926050298746,-0.00075381925080059,-0.041618768891219, &
      -0.23452173681569,0.14003840584586,0.063281744807738, &
      -0.034660425848809,-0.23918747334251,0.0019855255066891, &
      6.1777746171555,-6.9575358271105,1.0630185306388 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,1.0, &
      1.0,1.0,0.875, &
      0.75,0.5,0.0, &
      0.0,0.0,0.0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,1.0, &
      1.0,1.0,1.25, &
      1.5,2.0,3.0, &
      3.0,3.0,3.0 &
      /), &
      num_exp = 0, &
      num_gauss = 10)

  type(meos_mix_data), parameter :: meos_mix27 = &
      meos_mix_data(ident1 = "C2", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix28 = &
      meos_mix_data(ident1 = "N2", &
      ident2 = "NC12", &
      Fij = 0.898, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix29 = &
      meos_mix_data(ident1 = "CO2", &
      ident2 = "TOLUENE", &
      Fij = 1.257, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix30 = &
      meos_mix_data(ident1 = "TOLUENE", &
      ident2 = "NC8", &
      Fij = -0.704, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix31 = &
      meos_mix_data(ident1 = "HE", &
      ident2 = "NE", &
      Fij = -3.25, &
      num_mix = 6, &
      n_mix = (/ &
      -0.47376518126608,0.48961193461001,-0.0057011062090535, &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      1.0,1.0,0.875, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      1.0,1.0,1.25, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 3)

  type(meos_mix_data), parameter :: meos_mix32 = &
      meos_mix_data(ident1 = "CYCLOHEX", &
      ident2 = "NC11", &
      Fij = -0.358, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix33 = &
      meos_mix_data(ident1 = "CO", &
      ident2 = "H2O", &
      Fij = 0.9897000, &
      num_mix = 5, &
      n_mix = (/ &
      0.401420790000D+01,-0.115739390000D+01,-0.721024250000D+01, &
      -0.532512230000D+01,-0.221558670000D+01,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      0.5470000000,0.0550000000,1.9250000000, &
      0.5520000000,1.0000000000,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,2,4,0, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,1,1,1,1,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 4, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix34 = &
      meos_mix_data(ident1 = "CO2", &
      ident2 = "ACETONE", &
      Fij = 1.6093, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix35 = &
      meos_mix_data(ident1 = "CO2", &
      ident2 = "AR", &
      Fij = 1.0000000, &
      num_mix = 6, &
      n_mix = (/ &
      -0.656000000000D-01,0.237000000000D-01,0.352170000000D+01, &
      -0.283100000000D+01,-0.140600000000D+01,0.864000000000D+00, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      3.2200000000,2.9000000000,1.9000000000, &
      1.5700000000,2.7300000000,1.0800000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      2,3,1,1,1,2, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,1.2430000000, &
      1.0720000000,1.4650000000,0.9460000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.5000000000, &
      0.5000000000,0.5000000000,0.5000000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.6500000000, &
      0.7270000000,0.6480000000,0.7060000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,1.2080000000, &
      0.8200000000,1.5270000000,0.8600000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 4)

  type(meos_mix_data), parameter :: meos_mix36 = &
      meos_mix_data(ident1 = "N2", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.25, &
      0.25,0.,0., &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.75, &
      1.,2.,3., &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 4)

  type(meos_mix_data), parameter :: meos_mix37 = &
      meos_mix_data(ident1 = "C1", &
      ident2 = "BENZENE", &
      Fij = 0.955, &
      num_mix = 9, &
      n_mix = (/ &
      0.013746429958576,-0.0074425012129552,-0.0045516600213685, &
      -0.0054546603350237,0.0023682016824471,0.18007763721438, &
      -0.44773942932486,0.019327374888200,-0.30632197804624, &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.25, &
      0.25,0.0,0.0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.75, &
      1.0,2.0,3.0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 4)

  type(meos_mix_data), parameter :: meos_mix38 = &
      meos_mix_data(ident1 = "BENZENE", &
      ident2 = "NC7", &
      Fij = -0.675, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix39 = &
      meos_mix_data(ident1 = "N2", &
      ident2 = "CYCLOHEX", &
      Fij = 0.735, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix40 = &
      meos_mix_data(ident1 = "C2", &
      ident2 = "ACETONE", &
      Fij = 2.1862, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix41 = &
      meos_mix_data(ident1 = "C1", &
      ident2 = "H2O", &
      Fij = 1.0000000, &
      num_mix = 6, &
      n_mix = (/ &
      0.330000000000D+01,0.960000000000D+01,-0.117000000000D+02, &
      0.213000000000D+01,-0.530000000000D+00,-0.288000000000D+01, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.1000000000,0.8000000000,1.0000000000, &
      4.0000000000,3.4000000000,0.8000000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,2,4,1, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,0,1,1,1,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 4, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix42 = &
      meos_mix_data(ident1 = "KR", &
      ident2 = "CO2", &
      Fij = 0.6362, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix43 = &
      meos_mix_data(ident1 = "C3", &
      ident2 = "ETOH", &
      Fij = 1.15, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix44 = &
      meos_mix_data(ident1 = "C1", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.25, &
      0.25,0.,0., &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.75, &
      1.,2.,3., &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 4)

  type(meos_mix_data), parameter :: meos_mix45 = &
      meos_mix_data(ident1 = "C1", &
      ident2 = "TOLUENE", &
      Fij = 1.313, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix46 = &
      meos_mix_data(ident1 = "CYCLOHEX", &
      ident2 = "TOLUENE", &
      Fij = 0.5865, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix47 = &
      meos_mix_data(ident1 = "H2S", &
      ident2 = "NC12", &
      Fij = -1.808, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix48 = &
      meos_mix_data(ident1 = "TOLUENE", &
      ident2 = "NC10", &
      Fij = -1.395, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix49 = &
      meos_mix_data(ident1 = "CYCLOHEX", &
      ident2 = "NC7", &
      Fij = -0.2539, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix50 = &
      meos_mix_data(ident1 = "O2", &
      ident2 = "H2O", &
      Fij = 0.6017000, &
      num_mix = 5, &
      n_mix = (/ &
      0.401420790000D+01,-0.115739390000D+01,-0.721024250000D+01, &
      -0.532512230000D+01,-0.221558670000D+01,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      0.5470000000,0.0550000000,1.9250000000, &
      0.5520000000,1.0000000000,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,2,4,0, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,1,1,1,1,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 4, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix51 = &
      meos_mix_data(ident1 = "CYCLOHEX", &
      ident2 = "NC8", &
      Fij = -0.358, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix52 = &
      meos_mix_data(ident1 = "NC6", &
      ident2 = "BENZENE", &
      Fij = -0.92, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix53 = &
      meos_mix_data(ident1 = "H2S", &
      ident2 = "H2O", &
      Fij = 1.000000, &
      num_mix = 6, &
      n_mix = (/ &
      0.170000000000D+00,-0.111600000000D+00,0.121000000000D+00, &
      -0.235200000000D-02,-0.431000000000D-01,0.776400000000D+00, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      0.9000000000,4.0400000000,6.8800000000, &
      8.1500000000,5.3500000000,2.7000000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,2,4,8,1, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,0,1,1,2,1, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 4, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix54 = &
      meos_mix_data(ident1 = "ETOH", &
      ident2 = "MEG", &
      Fij = 0.3312, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix55 = &
      meos_mix_data(ident1 = "NC6", &
      ident2 = "TOLUENE", &
      Fij = -0.213, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix56 = &
      meos_mix_data(ident1 = "NC6", &
      ident2 = "CYCLOHEX", &
      Fij = -0.3672, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix57 = &
      meos_mix_data(ident1 = "C1", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,1., &
      1.,1.,0.875, &
      0.75,0.5,0., &
      0.,0.,0. &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5, &
      0.5,0.5,0.5 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,1., &
      1.,1.,1.25, &
      1.5,2.,3., &
      3.,3.,3. &
      /), &
      num_exp = 0, &
      num_gauss = 10)

  type(meos_mix_data), parameter :: meos_mix58 = &
      meos_mix_data(ident1 = "NC9", &
      ident2 = "NC12", &
      Fij = 0.15, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix59 = &
      meos_mix_data(ident1 = "CO2", &
      ident2 = "CO", &
      Fij = 1.0000000, &
      num_mix = 6, &
      n_mix = (/ &
      0.186100000000D+01,-0.401700000000D+01,0.273400000000D+00, &
      0.239300000000D+01,0.264600000000D+02,-0.121300000000D+01, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      2.8200000000,3.2600000000,0.9400000000, &
      3.9440000000,2.5300000000,4.3800000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,2,4,1,1, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,0,1,1,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,-0.3850000000,-0.2950000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.1090000000,2.5960000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,-0.1440000000,-0.3100000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,5.1000000000,1.6610000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 2, &
      num_gauss = 2)

  type(meos_mix_data), parameter :: meos_mix60 = &
      meos_mix_data(ident1 = "N2", &
      ident2 = "H2O", &
      Fij = 1.0000000, &
      num_mix = 5, &
      n_mix = (/ &
      0.401420790000D+01,-0.115739390000D+01,-0.721024250000D+01, &
      -0.532512230000D+01,-0.221558670000D+01,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      0.5470000000,0.0550000000,1.9250000000, &
      0.5520000000,1.0000000000,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,2,4,0, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,1,1,1,1,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 4, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix61 = &
      meos_mix_data(ident1 = "BENZENE", &
      ident2 = "NC10", &
      Fij = -2.444, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix62 = &
      meos_mix_data(ident1 = "ETOH", &
      ident2 = "TOLUENE", &
      Fij = 0.282, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix63 = &
      meos_mix_data(ident1 = "BENZENE", &
      ident2 = "NC8", &
      Fij = -0.924, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix64 = &
      meos_mix_data(ident1 = "BENZENE", &
      ident2 = "CYCLOHEX", &
      Fij = -0.6475, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix65 = &
      meos_mix_data(ident1 = "NC6", &
      ident2 = "NC12", &
      Fij = 1.14, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix66 = &
      meos_mix_data(ident1 = "CO2", &
      ident2 = "NC12", &
      Fij = -1.539, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix67 = &
      meos_mix_data(ident1 = "BENZENE", &
      ident2 = "NC9", &
      Fij = -1.193, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix68 = &
      meos_mix_data(ident1 = "C3", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix69 = &
      meos_mix_data(ident1 = "C1", &
      ident2 = "H2", &
      Fij = 1.000000, &
      num_mix = 8, &
      n_mix = (/ &
      0.128000000000D+01,-0.774000000000D+00,-0.914000000000D+00, &
      0.360000000000D+00,-0.245000000000D+01,0.446200000000D+01, &
      -0.972000000000D+00,-0.207000000000D+01,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      0.3400000000,0.4200000000,0.8000000000, &
      3.2900000000,0.0500000000,0.2540000000, &
      0.4100000000,2.6300000000,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,2,1,2,1,2, &
      3,1,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,-0.8300000000,-0.3400000000, &
      -0.5700000000,-0.4400000000,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,1.3600000000,1.4400000000, &
      1.6900000000,1.5300000000,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,-0.9700000000,-0.2000000000, &
      -0.2600000000,-0.1800000000,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.7700000000,1.7400000000, &
      0.7900000000,0.4000000000,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 4)

  type(meos_mix_data), parameter :: meos_mix70 = &
      meos_mix_data(ident1 = "ETOH", &
      ident2 = "BENZENE", &
      Fij = -0.162, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix71 = &
      meos_mix_data(ident1 = "C1", &
      ident2 = "CYCLOHEX", &
      Fij = -0.3986, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix72 = &
      meos_mix_data(ident1 = "HE", &
      ident2 = "KR", &
      Fij = 1.41, &
      num_mix = 4, &
      n_mix = (/ &
      -0.25157134971934,-0.0062203841111983,0.088850315184396, &
      -0.035592212573239,0.0d0,0.0d0, &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix73 = &
      meos_mix_data(ident1 = "ETOH", &
      ident2 = "H2O", &
      Fij = 1.000000, &
      num_mix = 6, &
      n_mix = (/ &
      -0.272600000000D+00,0.270000000000D-01,-0.148300000000D-01, &
      0.177300000000D+01,0.690000000000D+01,-0.642000000000D+01, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      1.6800000000,0.7300000000,4.5500000000, &
      1.1700000000,0.1500000000,0.4300000000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,4,3,2,1,1, &
      0,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,0,1,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      -0.58500,-0.51000,-0.70000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      1.0800,0.7500,1.3400, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      -0.1900,-2.1200,-1.2200, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      1.11000,1.64000,1.64000, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 1, &
      num_gauss = 3)

  type(meos_mix_data), parameter :: meos_mix74 = &
      meos_mix_data(ident1 = "C3", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix75 = &
      meos_mix_data(ident1 = "NE", &
      ident2 = "N2", &
      Fij = 1.7, &
      num_mix = 6, &
      n_mix = (/ &
      -0.47376518126608,0.48961193461001,-0.0057011062090535, &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      1.0,1.0,0.875, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      1.0,1.0,1.25, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 3)

  type(meos_mix_data), parameter :: meos_mix76 = &
      meos_mix_data(ident1 = "N2", &
      ident2 = "H2", &
      Fij = 1.000000, &
      num_mix = 8, &
      n_mix = (/ &
      -0.165900000000D+01,-0.268000000000D+00,-0.502000000000D+00, &
      0.323000000000D+00,0.297600000000D+01,0.508400000000D+01, &
      -0.386600000000D+01,-0.547400000000D+01,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      0.7240000000,0.0970000000,2.9530000000, &
      3.5000000000,2.9390000000,0.6940000000, &
      2.3290000000,1.0660000000,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,2,1,2,1,1, &
      1,1,0,0,0,0 &
      /), &
      l_mix = (/ &
      1,1,2,2,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,-1.8600000000,-0.0300000000, &
      -1.8500000000,-0.1300000000,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,2.5100000000,0.3900000000, &
      2.5100000000,1.2200000000,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,-0.9300000000,-0.3600000000, &
      -0.9900000000,-0.2400000000,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,1.3000000000,0.8000000000, &
      1.5000000000,0.2300000000,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 4, &
      num_gauss = 4)

  type(meos_mix_data), parameter :: meos_mix77 = &
      meos_mix_data(ident1 = "C1", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix78 = &
      meos_mix_data(ident1 = "NH3", &
      ident2 = "H2O", &
      Fij = 1., &
      num_mix = 7, &
      n_mix = (/ &
      -2.00211,3.08130,-1.75352, &
      2.98160,-3.82588,-1.73850, &
      0.42008,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      0.25,2.0,0.5, &
      2.0,1.0,4.0, &
      1.0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,1,1,1,1, &
      3,0,0,0,0,0 &
      /), &
      l_mix = (/ &
      2,1,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0, &
      -0.746,-4.25,-0.7, &
      -3.0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0, &
      2.0,-0.25,1.85, &
      0.3,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,-0.27, &
      -0.86,-3.0,-0.5, &
      -4.0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,2.8, &
      1.8,1.5,0.8, &
      1.3,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 2, &
      num_gauss = 5)

  type(meos_mix_data), parameter :: meos_mix79 = &
      meos_mix_data(ident1 = "H2", &
      ident2 = "NE", &
      Fij = 0.1617, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix80 = &
      meos_mix_data(ident1 = "C1", &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      1.,0.5,0., &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.5,0.5,0.5, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      1.,2.,3., &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 3)

  type(meos_mix_data), parameter :: meos_mix81 = &
      meos_mix_data(ident1 = "CO2", &
      ident2 = "CYCLOHEX", &
      Fij = 0.0184, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix82 = &
      meos_mix_data(ident1 = "CO2", &
      ident2 = "H2O", &
      Fij = 1.0000000, &
      num_mix = 8, &
      n_mix = (/ &
      0.394404670000D+00,-0.176347320000D+01,0.146207550000D+00, &
      0.875223200000D-02,0.203493980000D+01,-0.903502500000D-01, &
      -0.216388540000D+00,0.396121700000D-01,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t_mix = (/ &
      0.8800000000,2.9320000000,2.4330000000, &
      1.3300000000,4.4160000000,5.5140000000, &
      5.2030000000,1.0000000000,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      d_mix = (/ &
      1,1,3,0,2,3, &
      1,5,0,0,0,0 &
      /), &
      l_mix = (/ &
      0,0,0,1,1,1, &
      2,2,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 5, &
      num_gauss = 0)

  type(meos_mix_data), parameter :: meos_mix83 = &
      meos_mix_data(ident1 = "CYCLOHEX", &
      ident2 = "NC10", &
      Fij = -0.358, &
      num_mix = 10, &
      n_mix = (/ &
      2.5574776844118,-7.9846357136353,4.7859131465806, &
      -0.73265392369587,1.3805471345312,0.28349603476365, &
      -0.49087385940425,-0.10291888921447,0.11836314681968, &
      0.000055527385721943,0.0d0,0.0d0 &
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
      l_mix = (/ &
      0,0,0,0,0,0, &
      0,0,0,0,0,0 &
      /), &
      eta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      gamma_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      epsilon_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      beta_mix = (/ &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      num_exp = 0, &
      num_gauss = 0)

  integer, parameter :: max_meos_mix_data = 83
  type (meos_mix_data), dimension(max_meos_mix_data), parameter :: meos_mix_datadb = (/&
      meos_mix1,meos_mix2,meos_mix3,meos_mix4,meos_mix5,meos_mix6, &
      meos_mix7,meos_mix8,meos_mix9,meos_mix10,meos_mix11,meos_mix12, &
      meos_mix13,meos_mix14,meos_mix15,meos_mix16,meos_mix17,meos_mix18, &
      meos_mix19,meos_mix20,meos_mix21,meos_mix22,meos_mix23,meos_mix24, &
      meos_mix25,meos_mix26,meos_mix27,meos_mix28,meos_mix29,meos_mix30, &
      meos_mix31,meos_mix32,meos_mix33,meos_mix34,meos_mix35,meos_mix36, &
      meos_mix37,meos_mix38,meos_mix39,meos_mix40,meos_mix41,meos_mix42, &
      meos_mix43,meos_mix44,meos_mix45,meos_mix46,meos_mix47,meos_mix48, &
      meos_mix49,meos_mix50,meos_mix51,meos_mix52,meos_mix53,meos_mix54, &
      meos_mix55,meos_mix56,meos_mix57,meos_mix58,meos_mix59,meos_mix60, &
      meos_mix61,meos_mix62,meos_mix63,meos_mix64,meos_mix65,meos_mix66, &
      meos_mix67,meos_mix68,meos_mix69,meos_mix70,meos_mix71,meos_mix72, &
      meos_mix73,meos_mix74,meos_mix75,meos_mix76,meos_mix77,meos_mix78, &
      meos_mix79,meos_mix80,meos_mix81,meos_mix82,meos_mix83 &
      /)

end module meosmixdb
