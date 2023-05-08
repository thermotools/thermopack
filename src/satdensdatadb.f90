!> Automatically generated file satdensdatadb.f90
!! Time stamp: 2021-07-25T15:05:48.939509

module satdensdatadb
  implicit none
  public

  type :: satdensdata
    character(len=8) :: ident !< The component ID
    character(len=40) :: name !< The component name
    character(len=1) :: phase !< Phase (G/L)
    real :: tr !< Reducing temperature (K)
    real :: rhor !< Reducing density (mol/l)
    integer :: correlation_id !<
    integer :: n_param !<
    real :: n(6) !<
    real :: t(6) !<
  end type satdensdata

  type (satdensdata), parameter :: satdensgas_1 = &
      satdensdata(ident = "AR", &
      name = "ARGON", &
      phase = "G", &
      tr = 150.687, &
      rhor = 13.40742965, &
      correlation_id = 5, &
      n_param = 4, &
      n = (/ &
      -0.29182D+01,0.97930D-01,-0.13721D+01, &
      -0.22898D+01,0.0d0,0.0d0 &
      /), &
      t = (/ &
      0.72,1.25,0.32, &
      4.34,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensliq_1 = &
      satdensdata(ident = "AR", &
      name = "ARGON", &
      phase = "L", &
      tr = 150.687, &
      rhor = 13.40742965, &
      correlation_id = 3, &
      n_param = 4, &
      n = (/ &
      1.5004264,-0.3138129,0.086461622, &
      -0.041477525,0.0d0,0.0d0 &
      /), &
      t = (/ &
      0.334,0.6666666666666,2.3333333333333, &
      4.0,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_2 = &
      satdensdata(ident = "CO", &
      name = "CO", &
      phase = "G", &
      tr = 132.86, &
      rhor = 10.85, &
      correlation_id = 3, &
      n_param = 6, &
      n = (/ &
      -0.25439D+01,-0.55601D+01,-0.85276D+01, &
      -0.51163D+01,-0.17701D+02,-0.29858D+02 &
      /), &
      t = (/ &
      0.395,1.21,3.0, &
      3.5,6.0,8.0 &
      /) )

  type (satdensdata), parameter :: satdensliq_2 = &
      satdensdata(ident = "CO", &
      name = "CO", &
      phase = "L", &
      tr = 132.86, &
      rhor = 10.85, &
      correlation_id = 1, &
      n_param = 5, &
      n = (/ &
      0.29570D+01,-0.42880D+01,0.87643D+01, &
      -0.84001D+01,0.36372D+01,0.0d0 &
      /), &
      t = (/ &
      0.398,0.735,1.08, &
      1.5,1.9,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_3 = &
      satdensdata(ident = "H2O", &
      name = "WATER", &
      phase = "G", &
      tr = 647.096, &
      rhor = 17.8737279956, &
      correlation_id = 4, &
      n_param = 6, &
      n = (/ &
      -2.03150240,-2.68302940,-5.38626492, &
      -17.2991605,-44.7586581,-63.9201063 &
      /), &
      t = (/ &
      1.0,2.0,4.0, &
      9.0,18.5,35.5 &
      /) )

  type (satdensdata), parameter :: satdensliq_3 = &
      satdensdata(ident = "H2O", &
      name = "WATER", &
      phase = "L", &
      tr = 647.096, &
      rhor = 17.8737279956, &
      correlation_id = 2, &
      n_param = 6, &
      n = (/ &
      1.99274064,1.09965342,-0.510839303, &
      -1.75493479,-45.5170352,-6.74694450d5 &
      /), &
      t = (/ &
      1.,2.,5., &
      16.,43.,110. &
      /) )

  type (satdensdata), parameter :: satdensgas_4 = &
      satdensdata(ident = "CO2", &
      name = "CO2", &
      phase = "G", &
      tr = 304.1282, &
      rhor = 10.6249, &
      correlation_id = 4, &
      n_param = 5, &
      n = (/ &
      -1.7074879,-0.8227467,-4.6008549, &
      -10.111178,-29.742252,0.0d0 &
      /), &
      t = (/ &
      1.02,1.5,3.0, &
      7.0,14.0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensliq_4 = &
      satdensdata(ident = "CO2", &
      name = "CO2", &
      phase = "L", &
      tr = 304.1282, &
      rhor = 10.6249, &
      correlation_id = 4, &
      n_param = 4, &
      n = (/ &
      1.92451080,-0.62385555,-0.32731127, &
      0.39245142,0.0d0,0.0d0 &
      /), &
      t = (/ &
      1.02,1.5,5.0, &
      5.5,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_5 = &
      satdensdata(ident = "NC10", &
      name = "DECANE", &
      phase = "G", &
      tr = 617.7, &
      rhor = 1.64, &
      correlation_id = 3, &
      n_param = 6, &
      n = (/ &
      -0.50378D+01,-0.34694D+01,-0.15906D+02, &
      -0.82894D+02,0.29336D+02,-0.10985D+03 &
      /), &
      t = (/ &
      0.4985,1.33,2.43, &
      5.44,5.8,11.0 &
      /) )

  type (satdensdata), parameter :: satdensliq_5 = &
      satdensdata(ident = "NC10", &
      name = "DECANE", &
      phase = "L", &
      tr = 617.7, &
      rhor = 1.64, &
      correlation_id = 1, &
      n_param = 5, &
      n = (/ &
      0.92435D+01,-0.16288D+02,0.20445D+02, &
      -0.17624D+02,0.73796D+01,0.0d0 &
      /), &
      t = (/ &
      0.535,0.74,1.0, &
      1.28,1.57,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_6 = &
      satdensdata(ident = "O2", &
      name = "OXYGEN", &
      phase = "G", &
      tr = 154.581, &
      rhor = 13.63, &
      correlation_id = 3, &
      n_param = 6, &
      n = (/ &
      -0.22695D+01,-0.46578D+01,-0.99480D+01, &
      -0.22845D+02,-0.45190D+02,-0.25101D+02 &
      /), &
      t = (/ &
      0.3785,1.07,2.7, &
      5.5,10.0,20.0 &
      /) )

  type (satdensdata), parameter :: satdensliq_6 = &
      satdensdata(ident = "O2", &
      name = "OXYGEN", &
      phase = "L", &
      tr = 154.581, &
      rhor = 13.63, &
      correlation_id = 1, &
      n_param = 5, &
      n = (/ &
      0.16622D+01,0.76846D+00,-0.10041D+00, &
      0.20480D+00,0.11551D-01,0.0d0 &
      /), &
      t = (/ &
      0.345,0.74,1.2, &
      2.6,7.2,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_7 = &
      satdensdata(ident = "NC8", &
      name = "OCTANE", &
      phase = "G", &
      tr = 569.32, &
      rhor = 2.056404, &
      correlation_id = 3, &
      n_param = 6, &
      n = (/ &
      -0.16556D+00,-0.59337D+01,-0.18915D+02, &
      -0.36484D+03,0.72686D+03,-0.50392D+03 &
      /), &
      t = (/ &
      0.09,0.59,2.4, &
      7.0,8.0,9.0 &
      /) )

  type (satdensdata), parameter :: satdensliq_7 = &
      satdensdata(ident = "NC8", &
      name = "OCTANE", &
      phase = "L", &
      tr = 569.32, &
      rhor = 2.056404, &
      correlation_id = 1, &
      n_param = 5, &
      n = (/ &
      0.56814D-01,0.38908D+02,-0.75923D+02, &
      0.59548D+02,-0.19651D+02,0.0d0 &
      /), &
      t = (/ &
      0.10,0.75,0.90, &
      1.10,1.25,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_8 = &
      satdensdata(ident = "IC4", &
      name = "ISOBUTAN", &
      phase = "G", &
      tr = 407.81, &
      rhor = 3.879756788, &
      correlation_id = 6, &
      n_param = 4, &
      n = (/ &
      -2.12933323,-2.93790085,-0.89441086, &
      -3.46343707,0.0d0,0.0d0 &
      /), &
      t = (/ &
      1.065,2.5,9.5, &
      13.0,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensliq_8 = &
      satdensdata(ident = "IC4", &
      name = "ISOBUTAN", &
      phase = "L", &
      tr = 407.81, &
      rhor = 3.879756788, &
      correlation_id = 2, &
      n_param = 4, &
      n = (/ &
      2.04025104,0.850874089,-0.479052281, &
      0.348201252,0.0d0,0.0d0 &
      /), &
      t = (/ &
      1.065,3.0,4.0, &
      7.0,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_9 = &
      satdensdata(ident = "HE", &
      name = "HELIUM", &
      phase = "G", &
      tr = 5.1953, &
      rhor = 17.3837, &
      correlation_id = 3, &
      n_param = 5, &
      n = (/ &
      -1.5789,-10.749,17.711, &
      -15.413,-14.352,0.0d0 &
      /), &
      t = (/ &
      0.333,1.5,2.1, &
      2.7,9.0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensliq_9 = &
      satdensdata(ident = "HE", &
      name = "HELIUM", &
      phase = "L", &
      tr = 5.1953, &
      rhor = 17.3837, &
      correlation_id = 1, &
      n_param = 5, &
      n = (/ &
      1.0929,1.6584,-3.6477, &
      2.7440,-2.3859,0.0d0 &
      /), &
      t = (/ &
      0.286,1.2,2.0, &
      2.8,6.5,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_10 = &
      satdensdata(ident = "H2", &
      name = "HYDROGEN", &
      phase = "G", &
      tr = 33.145, &
      rhor = 15.508, &
      correlation_id = 3, &
      n_param = 6, &
      n = (/ &
      -0.29962D+01,-0.16724D+02,0.15819D+02, &
      -0.16852D+02,0.34586D+02,-0.53754D+02 &
      /), &
      t = (/ &
      0.466,2.,2.4, &
      4.,7.,8. &
      /) )

  type (satdensdata), parameter :: satdensliq_10 = &
      satdensdata(ident = "H2", &
      name = "HYDROGEN", &
      phase = "L", &
      tr = 33.145, &
      rhor = 15.508, &
      correlation_id = 1, &
      n_param = 5, &
      n = (/ &
      0.15456D+02,-0.41720D+02,0.50276D+02, &
      -0.27947D+02,0.56718D+01,0.0d0 &
      /), &
      t = (/ &
      0.62,0.83,1.05, &
      1.3,1.6,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_11 = &
      satdensdata(ident = "NC5", &
      name = "PENTANE", &
      phase = "G", &
      tr = 469.7, &
      rhor = 3.2155776, &
      correlation_id = 3, &
      n_param = 6, &
      n = (/ &
      -0.29389D+01,-0.62784D+01,-0.19941D+02, &
      -0.16709D+02,-0.36543D+02,-0.12799D+03 &
      /), &
      t = (/ &
      0.4,1.18,3.2, &
      6.6,7.0,15.0 &
      /) )

  type (satdensdata), parameter :: satdensliq_11 = &
      satdensdata(ident = "NC5", &
      name = "PENTANE", &
      phase = "L", &
      tr = 469.7, &
      rhor = 3.2155776, &
      correlation_id = 1, &
      n_param = 5, &
      n = (/ &
      0.10178D+01,0.42703D+00,0.11334D+01, &
      0.41518D+00,-0.47950D-01,0.0d0 &
      /), &
      t = (/ &
      0.27,0.44,0.6, &
      4.0,5.0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_12 = &
      satdensdata(ident = "NC4", &
      name = "BUTANE", &
      phase = "G", &
      tr = 425.125, &
      rhor = 3.922769613, &
      correlation_id = 3, &
      n_param = 5, &
      n = (/ &
      -0.27390D+01,-0.57347D+01,-0.16408D+02, &
      -0.46986D+02,-0.10090D+03,0.0d0 &
      /), &
      t = (/ &
      0.391,1.14,3.0, &
      6.5,14.0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensliq_12 = &
      satdensdata(ident = "NC4", &
      name = "BUTANE", &
      phase = "L", &
      tr = 425.125, &
      rhor = 3.922769613, &
      correlation_id = 1, &
      n_param = 4, &
      n = (/ &
      0.52341D+01,-0.62011D+01,0.36063D+01, &
      0.22137D+00,0.0d0,0.0d0 &
      /), &
      t = (/ &
      0.44,0.60,0.76, &
      5.00,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_13 = &
      satdensdata(ident = "N2", &
      name = "NITROGEN", &
      phase = "G", &
      tr = 126.192, &
      rhor = 11.1839, &
      correlation_id = 6, &
      n_param = 5, &
      n = (/ &
      -0.170127164E+1,-0.370402649E+1,0.129859383E+1, &
      -0.561424977E+0,-0.268505381E+1,0.0d0 &
      /), &
      t = (/ &
      1.02,2.5,3.5, &
      6.5,14.0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensliq_13 = &
      satdensdata(ident = "N2", &
      name = "NITROGEN", &
      phase = "L", &
      tr = 126.192, &
      rhor = 11.1839, &
      correlation_id = 4, &
      n_param = 4, &
      n = (/ &
      0.148654237E+1,-0.280476066E+0,0.894143085E-1, &
      -0.119879866E+0,0.0d0,0.0d0 &
      /), &
      t = (/ &
      0.9882,2.,8., &
      17.5,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_14 = &
      satdensdata(ident = "NC7", &
      name = "HEPTANE", &
      phase = "G", &
      tr = 540.13, &
      rhor = 2.315323, &
      correlation_id = 3, &
      n_param = 6, &
      n = (/ &
      -0.24571D+00,-0.63004D+01,-0.19144D+02, &
      -0.96970D+02,0.21643D+03,-0.27953D+03 &
      /), &
      t = (/ &
      0.097,0.646,2.56, &
      6.6,9.3,10.7 &
      /) )

  type (satdensdata), parameter :: satdensliq_14 = &
      satdensdata(ident = "NC7", &
      name = "HEPTANE", &
      phase = "L", &
      tr = 540.13, &
      rhor = 2.315323, &
      correlation_id = 1, &
      n_param = 5, &
      n = (/ &
      -0.26395D+01,0.21806D+02,-0.28896D+02, &
      0.12609D+02,0.40749D+00,0.0d0 &
      /), &
      t = (/ &
      0.322,0.504,0.651, &
      0.816,6.4,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_15 = &
      satdensdata(ident = "C2", &
      name = "ETHANE", &
      phase = "G", &
      tr = 305.322, &
      rhor = 6.856886685, &
      correlation_id = 6, &
      n_param = 6, &
      n = (/ &
      -1.89879145,-3.65459262,0.850562745, &
      0.363965487,-1.50005943,-2.26690389 &
      /), &
      t = (/ &
      1.038,2.5,3.0, &
      6.0,9.0,15.0 &
      /) )

  type (satdensdata), parameter :: satdensliq_15 = &
      satdensdata(ident = "C2", &
      name = "ETHANE", &
      phase = "L", &
      tr = 305.322, &
      rhor = 6.856886685, &
      correlation_id = 4, &
      n_param = 4, &
      n = (/ &
      1.56138026,-0.381552776,0.0785372040, &
      0.0370315089,0.0d0,0.0d0 &
      /), &
      t = (/ &
      0.987,2.0,4.0, &
      9.5,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_16 = &
      satdensdata(ident = "H2S", &
      name = "H2S", &
      phase = "G", &
      tr = 373.1, &
      rhor = 10.19, &
      correlation_id = 3, &
      n_param = 4, &
      n = (/ &
      -3.9156,-7.7093,-19.543, &
      -49.418,0.0d0,0.0d0 &
      /), &
      t = (/ &
      0.49,1.69,4.00, &
      8.00,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensliq_16 = &
      satdensdata(ident = "H2S", &
      name = "H2S", &
      phase = "L", &
      tr = 373.1, &
      rhor = 10.19, &
      correlation_id = 2, &
      n_param = 3, &
      n = (/ &
      11.833,-17.019,7.8047, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t = (/ &
      1.63,1.95,2.30, &
      0.0d0,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_17 = &
      satdensdata(ident = "IC5", &
      name = "IPENTANE", &
      phase = "G", &
      tr = 460.356, &
      rhor = 3.271, &
      correlation_id = 3, &
      n_param = 6, &
      n = (/ &
      -0.38825D+02,0.79040D+02,-0.48791D+02, &
      -0.21603D+02,-0.57218D+02,-0.15164D+03 &
      /), &
      t = (/ &
      0.565,0.66,0.77, &
      3.25,7.3,16.6 &
      /) )

  type (satdensdata), parameter :: satdensliq_17 = &
      satdensdata(ident = "IC5", &
      name = "IPENTANE", &
      phase = "L", &
      tr = 460.356, &
      rhor = 3.271, &
      correlation_id = 1, &
      n_param = 5, &
      n = (/ &
      0.18367D+02,-0.30283D+02,0.13557D+02, &
      -0.90533D+00,0.20927D+01,0.0d0 &
      /), &
      t = (/ &
      1.21,1.41,1.65, &
      0.09,0.164,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_18 = &
      satdensdata(ident = "C1", &
      name = "METHANE", &
      phase = "G", &
      tr = 190.564, &
      rhor = 10.139128, &
      correlation_id = 4, &
      n_param = 6, &
      n = (/ &
      -1.8802840,-2.8526531,-3.0006480, &
      -5.2511690,-13.191859,-37.553961 &
      /), &
      t = (/ &
      1.062,2.5,4.5, &
      7.5,12.5,23.5 &
      /) )

  type (satdensdata), parameter :: satdensliq_18 = &
      satdensdata(ident = "C1", &
      name = "METHANE", &
      phase = "L", &
      tr = 190.564, &
      rhor = 10.139128, &
      correlation_id = 3, &
      n_param = 3, &
      n = (/ &
      1.9906389,-0.78756197,0.036976723, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t = (/ &
      0.354,0.5,2.5, &
      0.0d0,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_19 = &
      satdensdata(ident = "NC6", &
      name = "HEXANE", &
      phase = "G", &
      tr = 507.82, &
      rhor = 2.7058779, &
      correlation_id = 3, &
      n_param = 6, &
      n = (/ &
      -0.13309D+00,-0.50653D+01,-0.11602D+02, &
      -0.28530D+02,-0.51731D+02,-0.13482D+03 &
      /), &
      t = (/ &
      0.107,0.553,2.006, &
      4.46,8.0,16.0 &
      /) )

  type (satdensdata), parameter :: satdensliq_19 = &
      satdensdata(ident = "NC6", &
      name = "HEXANE", &
      phase = "L", &
      tr = 507.82, &
      rhor = 2.7058779, &
      correlation_id = 1, &
      n_param = 3, &
      n = (/ &
      0.14686D+03,-0.26585D+03,0.12200D+03, &
      0.0d0,0.0d0,0.0d0 &
      /), &
      t = (/ &
      0.75,0.81,0.88, &
      0.0d0,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_20 = &
      satdensdata(ident = "C3", &
      name = "PROPANE", &
      phase = "G", &
      tr = 369.89, &
      rhor = 5.0, &
      correlation_id = 3, &
      n_param = 6, &
      n = (/ &
      -2.4887,-5.1069,-12.174, &
      -30.495,-52.192,-134.89 &
      /), &
      t = (/ &
      0.3785,1.07,2.7, &
      5.5,10.0,20.0 &
      /) )

  type (satdensdata), parameter :: satdensliq_20 = &
      satdensdata(ident = "C3", &
      name = "PROPANE", &
      phase = "L", &
      tr = 369.89, &
      rhor = 5.0, &
      correlation_id = 1, &
      n_param = 4, &
      n = (/ &
      1.82205,0.65802,0.21109, &
      0.083973,0.0d0,0.0d0 &
      /), &
      t = (/ &
      0.345,0.74,2.6, &
      7.2,0.0d0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensgas_21 = &
      satdensdata(ident = "NC9", &
      name = "NONANE", &
      phase = "G", &
      tr = 594.55, &
      rhor = 1.81, &
      correlation_id = 3, &
      n_param = 5, &
      n = (/ &
      -0.33199D+01,-0.23900D+01,-0.15307D+02, &
      -0.51788D+02,-0.11133D+03,0.0d0 &
      /), &
      t = (/ &
      0.461,0.666,2.12, &
      5.1,11.0,0.0d0 &
      /) )

  type (satdensdata), parameter :: satdensliq_21 = &
      satdensdata(ident = "NC9", &
      name = "NONANE", &
      phase = "L", &
      tr = 594.55, &
      rhor = 1.81, &
      correlation_id = 1, &
      n_param = 5, &
      n = (/ &
      -0.43785D+00,0.37240D+01,-0.23029D+01, &
      0.18270D+01,0.38664D+00,0.0d0 &
      /), &
      t = (/ &
      0.116,0.32,0.54, &
      0.8,3.5,0.0d0 &
      /) )

  integer, parameter :: maxsatdens = 42
  type (satdensdata), dimension(maxsatdens), parameter :: satdensdb = (/&
      satdensgas_1,satdensliq_1,satdensgas_2,satdensliq_2,satdensgas_3,satdensliq_3, &
      satdensgas_4,satdensliq_4,satdensgas_5,satdensliq_5,satdensgas_6,satdensliq_6, &
      satdensgas_7,satdensliq_7,satdensgas_8,satdensliq_8,satdensgas_9,satdensliq_9, &
      satdensgas_10,satdensliq_10,satdensgas_11,satdensliq_11,satdensgas_12,satdensliq_12, &
      satdensgas_13,satdensliq_13,satdensgas_14,satdensliq_14,satdensgas_15,satdensliq_15, &
      satdensgas_16,satdensliq_16,satdensgas_17,satdensliq_17,satdensgas_18,satdensliq_18, &
      satdensgas_19,satdensliq_19,satdensgas_20,satdensliq_20,satdensgas_21,satdensliq_21 &
      /)

end module satdensdatadb
