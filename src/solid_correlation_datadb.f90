!> Automatically generated to file solid_correlation_datadb.f90
!! using utility python code pyUtils
!! Time stamp: 2023-04-28T20:02:12.169534

module solid_correlation_datadb
  use thermopack_constants, only: uid_len, ref_len, bibref_len
  implicit none
  public

  !> This data structure stores parameters for
  !> sublimation and melting line correlations.
  ! ---------------------------------------------------------------------------
  type :: solid_correlation_data
    character(len=uid_len) :: compName
    character(len=4) :: correlation !< Correlations type
    real :: triple_temperature  !< [K]. Triple point temperature.
    real :: minimum_temperature  !< [K]. Minimum temperature for sublimation line.
    real :: maximum_temperature  !< [K]. Maximum temperature for melting line.
    real :: reducing_pressure !< [Pa]. Pressure scaling parameter.
    real :: reducing_temperature !< [K]. Temperature reducing parameter.
    integer :: n_coeff !< Number of coefficients
    integer :: n_coeff_1 !< Number of coefficients for type one terms
    integer :: n_coeff_2 !< Number of coefficients for type two terms
    integer :: n_coeff_3 !< Number of coefficients for type three terms
    real :: coeff(6)  !< Correlation coefficients
    real :: exponents(6) !< Correlation exponents.
    character(len=bibref_len) :: bib_ref !< Bibliograpic reference.
    character(len=ref_len) :: ref !< Parameter set
  end type solid_correlation_data

  type(solid_correlation_data), parameter :: MELT1 = &
      solid_correlation_data( &
      compName = "NH3", &
      correlation = "ML-1", &
      triple_temperature = 195.495, &
      minimum_temperature = 0., &
      maximum_temperature = 10000., &
      reducing_pressure = 1000000., &
      reducing_temperature = 195.49453, &
      n_coeff = 1, &
      n_coeff_1 = 0, &
      n_coeff_2 = 0, &
      n_coeff_3 = 1, &
      coeff = (/2.533125000000e+03,0.000000000000e+00,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/1.000000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.555579", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT2 = &
      solid_correlation_data( &
      compName = "AR", &
      correlation = "ML-1", &
      triple_temperature = 83.8058, &
      minimum_temperature = 0., &
      maximum_temperature = 700., &
      reducing_pressure = 68891., &
      reducing_temperature = 83.8058, &
      n_coeff = 3, &
      n_coeff_1 = 1, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/1.000000000000e+00,-7.476266510000e+03,9.959061250000e+03, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.050000,1.275000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.556037", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT3 = &
      solid_correlation_data( &
      compName = "CO2", &
      correlation = "ML-1", &
      triple_temperature = 216.592, &
      minimum_temperature = 0., &
      maximum_temperature = 1100., &
      reducing_pressure = 517950., &
      reducing_temperature = 216.592, &
      n_coeff = 3, &
      n_coeff_1 = 0, &
      n_coeff_2 = 3, &
      n_coeff_3 = 0, &
      coeff = (/1.000000000000e+00,1.955539000000e+03,2.055459300000e+03, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.000000,2.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.555991", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT4 = &
      solid_correlation_data( &
      compName = "CO", &
      correlation = "ML-1", &
      triple_temperature = 68.16, &
      minimum_temperature = 0., &
      maximum_temperature = 1000., &
      reducing_pressure = 1000000., &
      reducing_temperature = 1., &
      n_coeff = 2, &
      n_coeff_1 = 2, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/-1.429410000000e+02,1.956080000000e-02,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,2.107470,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/0021-9614(82)90044-1", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT5 = &
      solid_correlation_data( &
      compName = "CYCLOHEX", &
      correlation = "ML-1", &
      triple_temperature = 279.86, &
      minimum_temperature = 0., &
      maximum_temperature = 700., &
      reducing_pressure = 5348.7, &
      reducing_temperature = 279.86, &
      n_coeff = 3, &
      n_coeff_1 = 2, &
      n_coeff_2 = 1, &
      n_coeff_3 = 0, &
      coeff = (/1.000000000000e+00,7.500000000000e+01,1.020000000000e+05, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,2.000000,1.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.4900538", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT6 = &
      solid_correlation_data( &
      compName = "C2", &
      correlation = "ML-1", &
      triple_temperature = 90.368, &
      minimum_temperature = 0., &
      maximum_temperature = 2000., &
      reducing_pressure = 1.1421, &
      reducing_temperature = 90.368, &
      n_coeff = 3, &
      n_coeff_1 = 1, &
      n_coeff_2 = 1, &
      n_coeff_3 = 0, &
      coeff = (/1.000000000000e+00,2.236263150000e+08,1.052623740000e+08, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.000000,2.550000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1859286", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT7 = &
      solid_correlation_data( &
      compName = "HE", &
      correlation = "ML-1", &
      triple_temperature = 2.1768, &
      minimum_temperature = 0., &
      maximum_temperature = 1500., &
      reducing_pressure = 1000000., &
      reducing_temperature = 1., &
      n_coeff = 2, &
      n_coeff_1 = 2, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/-1.745583700000e+00,1.697979300000e+00,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.555414,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1007/978-1-4613-0639-9_174", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT8 = &
      solid_correlation_data( &
      compName = "H2", &
      correlation = "ML-1", &
      triple_temperature = 13.957, &
      minimum_temperature = 0., &
      maximum_temperature = 400., &
      reducing_pressure = 7357.8, &
      reducing_temperature = 13.957, &
      n_coeff = 3, &
      n_coeff_1 = 1, &
      n_coeff_2 = 2, &
      n_coeff_3 = 0, &
      coeff = (/1.000000000000e+00,5.626300000000e+03,2.717200000000e+03, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.000000,1.830000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT9 = &
      solid_correlation_data( &
      compName = "IC4", &
      correlation = "ML-1", &
      triple_temperature = 113.73, &
      minimum_temperature = 0., &
      maximum_temperature = 575., &
      reducing_pressure = 0.022891, &
      reducing_temperature = 113.73, &
      n_coeff = 2, &
      n_coeff_1 = 2, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/-1.953637129000e+09,1.953637130000e+09,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,6.120000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1901687", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT10 = &
      solid_correlation_data( &
      compName = "IC5", &
      correlation = "ML-1", &
      triple_temperature = 112.65, &
      minimum_temperature = 0., &
      maximum_temperature = 2000., &
      reducing_pressure = 0.00008952, &
      reducing_temperature = 112.65, &
      n_coeff = 2, &
      n_coeff_1 = 2, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/-7.127700000000e+12,7.127700000001e+12,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.563000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1725068", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT11 = &
      solid_correlation_data( &
      compName = "KR", &
      correlation = "ML-1", &
      triple_temperature = 115.775, &
      minimum_temperature = 0., &
      maximum_temperature = 800., &
      reducing_pressure = 101325., &
      reducing_temperature = 1., &
      n_coeff = 2, &
      n_coeff_1 = 2, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/-2.345921000000e+03,1.080476685000e+00,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.616984,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/0031-8914(62)90096-4", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT12 = &
      solid_correlation_data( &
      compName = "C1", &
      correlation = "ML-1", &
      triple_temperature = 90.6941, &
      minimum_temperature = 0., &
      maximum_temperature = 625., &
      reducing_pressure = 11696., &
      reducing_temperature = 90.6941, &
      n_coeff = 3, &
      n_coeff_1 = 1, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/1.000000000000e+00,2.475680000000e+04,-7.366020000000e+03, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.850000,2.100000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.555898", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT13 = &
      solid_correlation_data( &
      compName = "MEOH", &
      correlation = "ML-1", &
      triple_temperature = 175.61, &
      minimum_temperature = 0., &
      maximum_temperature = 620., &
      reducing_pressure = 0.187, &
      reducing_temperature = 175.61, &
      n_coeff = 4, &
      n_coeff_1 = 1, &
      n_coeff_2 = 3, &
      n_coeff_3 = 0, &
      coeff = (/1.000000000000e+00,5.320770000000e+09,4.524780000000e+09, &
      3.888861000000e+10,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.000000,1.500000, &
      4.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/j.fluid.2013.03.024", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT14 = &
      solid_correlation_data( &
      compName = "NE", &
      correlation = "ML-1", &
      triple_temperature = 24.556, &
      minimum_temperature = 0., &
      maximum_temperature = 10000., &
      reducing_pressure = 43368.14, &
      reducing_temperature = 24.556, &
      n_coeff = 2, &
      n_coeff_1 = 1, &
      n_coeff_2 = 1, &
      n_coeff_3 = 0, &
      coeff = (/1.000000000000e+00,4.437000000000e+03,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.330000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2010)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT15 = &
      solid_correlation_data( &
      compName = "N2", &
      correlation = "ML-1", &
      triple_temperature = 63.151, &
      minimum_temperature = 0., &
      maximum_temperature = 2000., &
      reducing_pressure = 12519.8, &
      reducing_temperature = 63.151, &
      n_coeff = 2, &
      n_coeff_1 = 1, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/1.000000000000e+00,1.279861000000e+04,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.789630,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1349047", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT16 = &
      solid_correlation_data( &
      compName = "O2", &
      correlation = "ML-2", &
      triple_temperature = 54.361, &
      minimum_temperature = 0., &
      maximum_temperature = 300., &
      reducing_pressure = 146.277, &
      reducing_temperature = 54.361, &
      n_coeff = 4, &
      n_coeff_1 = 0, &
      n_coeff_2 = 4, &
      n_coeff_3 = 0, &
      coeff = (/-3.246353900000e+01,1.427801100000e+02,-1.470234100000e+02, &
      5.200120000000e+01,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.062500,0.125000,0.187500, &
      0.250000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/0378-3812(85)87016-3", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT17 = &
      solid_correlation_data( &
      compName = "P-H2", &
      correlation = "MP-1", &
      triple_temperature = 13.8, &
      minimum_temperature = 0., &
      maximum_temperature = 400., &
      reducing_pressure = 1000000., &
      reducing_temperature = 1., &
      n_coeff = 4, &
      n_coeff_1 = 4, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/-2.652891150000e+01,2.485785960000e-01,-2.128233930000e+01, &
      1.257466430000e-01,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.764739,0.000000, &
      1.955000,0.000000,0.000000/), &
      bib_ref = "ISBN: 088318415X, https://srd.nist.gov/JPCRD/jpcrdS1Vol11.pdf", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT18 = &
      solid_correlation_data( &
      compName = "C3", &
      correlation = "ML-1", &
      triple_temperature = 85.525, &
      minimum_temperature = 0., &
      maximum_temperature = 2000., &
      reducing_pressure = 0.00017205, &
      reducing_temperature = 85.525, &
      n_coeff = 2, &
      n_coeff_1 = 2, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/-4.230000000000e+12,4.230000000001e+12,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.283000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1725068", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT19 = &
      solid_correlation_data( &
      compName = "PRLN", &
      correlation = "ML-1", &
      triple_temperature = 87.953, &
      minimum_temperature = 0., &
      maximum_temperature = 2000., &
      reducing_pressure = 0.0007471, &
      reducing_temperature = 87.953, &
      n_coeff = 2, &
      n_coeff_1 = 2, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/-6.593000000000e+09,6.593000001000e+09,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,2.821000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1725068", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT20 = &
      solid_correlation_data( &
      compName = "F6S", &
      correlation = "ML-1", &
      triple_temperature = 223.555, &
      minimum_temperature = 0., &
      maximum_temperature = 650., &
      reducing_pressure = 231429., &
      reducing_temperature = 223.555, &
      n_coeff = 2, &
      n_coeff_1 = 1, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/1.000000000000e+00,9.666031480000e+02,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.555000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.5005537", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT21 = &
      solid_correlation_data( &
      compName = "XE", &
      correlation = "ML-1", &
      triple_temperature = 161.405, &
      minimum_temperature = 0., &
      maximum_temperature = 1300., &
      reducing_pressure = 101325., &
      reducing_temperature = 1., &
      n_coeff = 2, &
      n_coeff_1 = 2, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/-2.575072800000e+03,7.983277028000e-01,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.589165,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/0031-8914(62)90096-4", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT22 = &
      solid_correlation_data( &
      compName = "NC4", &
      correlation = "ML-1", &
      triple_temperature = 134.895, &
      minimum_temperature = 0., &
      maximum_temperature = 575., &
      reducing_pressure = 0.66566, &
      reducing_temperature = 134.895, &
      n_coeff = 2, &
      n_coeff_1 = 2, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/-5.585582354000e+08,5.585582364000e+08,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,2.206000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1901687", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT23 = &
      solid_correlation_data( &
      compName = "NC5", &
      correlation = "ML-1", &
      triple_temperature = 143.47, &
      minimum_temperature = 0., &
      maximum_temperature = 2000., &
      reducing_pressure = 0.076322, &
      reducing_temperature = 143.47, &
      n_coeff = 2, &
      n_coeff_1 = 2, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/-8.647500000000e+09,8.647500001000e+09,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,1.649000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1725068", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL1 = &
      solid_correlation_data( &
      compName = "NH3", &
      correlation = "SL-2", &
      triple_temperature = 195.49, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      reducing_temperature = 1., &
      n_coeff = 5, &
      n_coeff_1 = 5, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/1.363932000000e+01,-3.537000000000e+03,-3.310000000000e+04, &
      1.742000000000e+06,-2.995000000000e+07,0.000000000000e+00/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,-4.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/5.0128269", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL2 = &
      solid_correlation_data( &
      compName = "AR", &
      correlation = "SL-3", &
      triple_temperature = 83.8058, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 68891., &
      reducing_temperature = 83.8058, &
      n_coeff = 1, &
      n_coeff_1 = 0, &
      n_coeff_2 = 1, &
      n_coeff_3 = 0, &
      coeff = (/-1.113070000000e+01,0.000000000000e+00,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/1.000000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2002)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL3 = &
      solid_correlation_data( &
      compName = "CO2", &
      correlation = "SL-3", &
      triple_temperature = 216.592, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 517950., &
      reducing_temperature = 216.592, &
      n_coeff = 3, &
      n_coeff_1 = 0, &
      n_coeff_2 = 3, &
      n_coeff_3 = 0, &
      coeff = (/-1.474084600000e+01,2.432701500000e+00,-5.306177800000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/1.000000,1.900000,2.900000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.555991", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL4 = &
      solid_correlation_data( &
      compName = "CO", &
      correlation = "SL-2", &
      triple_temperature = 68.16, &
      minimum_temperature = 61.55, &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      reducing_temperature = 1., &
      n_coeff = 4, &
      n_coeff_1 = 4, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/7.945240000000e+00,-7.481510000000e+02,-5.843300000000e+03, &
      3.938500000000e+04,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1007/978-1-4613-9856-1_76", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL5 = &
      solid_correlation_data( &
      compName = "C2", &
      correlation = "SL-2", &
      triple_temperature = 90.368, &
      minimum_temperature = 20., &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      reducing_temperature = 1., &
      n_coeff = 6, &
      n_coeff_1 = 6, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/1.281110000000e+01,-2.207460000000e+03,-2.411300000000e+04, &
      7.744400000000e+05,-1.161500000000e+07,6.763300000000e+07/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,-4.000000,-5.000000/), &
      bib_ref = "DOI: 10.1007/978-1-4613-9856-1_76", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL6 = &
      solid_correlation_data( &
      compName = "H2", &
      correlation = "SL-3", &
      triple_temperature = 13.957, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 7700., &
      reducing_temperature = 13.957, &
      n_coeff = 1, &
      n_coeff_1 = 0, &
      n_coeff_2 = 1, &
      n_coeff_3 = 0, &
      coeff = (/-8.065000000000e+00,0.000000000000e+00,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.930000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2003)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL7 = &
      solid_correlation_data( &
      compName = "H2S", &
      correlation = "S2", &
      triple_temperature = 187.7, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      reducing_temperature = 187.7, &
      n_coeff = 5, &
      n_coeff_1 = 5, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/6.624700000000e+00,-7.260000000000e+02,-3.504000000000e+05, &
      2.724000000000e+07,-8.582000000000e+08,0.000000000000e+00/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,-4.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/j.pss.2009.09.011", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL8 = &
      solid_correlation_data( &
      compName = "KR", &
      correlation = "SL-3", &
      triple_temperature = 115.775, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 73197., &
      reducing_temperature = 115.775, &
      n_coeff = 1, &
      n_coeff_1 = 0, &
      n_coeff_2 = 1, &
      n_coeff_3 = 0, &
      coeff = (/-1.156160000000e+01,0.000000000000e+00,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/1.000000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2002)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL9 = &
      solid_correlation_data( &
      compName = "C1", &
      correlation = "SL-3", &
      triple_temperature = 90.6941, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 11696., &
      reducing_temperature = 90.6941, &
      n_coeff = 1, &
      n_coeff_1 = 0, &
      n_coeff_2 = 1, &
      n_coeff_3 = 0, &
      coeff = (/-1.284000000000e+01,0.000000000000e+00,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/1.000000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2002)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL10 = &
      solid_correlation_data( &
      compName = "NE", &
      correlation = "SL-2", &
      triple_temperature = 24.556, &
      minimum_temperature = 7., &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      reducing_temperature = 1., &
      n_coeff = 5, &
      n_coeff_1 = 5, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/8.307100000000e+00,-3.085550000000e+02,9.860200000000e+02, &
      -9.069300000000e+03,3.514200000000e+04,0.000000000000e+00/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,-4.000000,0.000000/), &
      bib_ref = "DOI: 10.1007/978-1-4613-9856-1_76", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL11 = &
      solid_correlation_data( &
      compName = "N2", &
      correlation = "SL-3", &
      triple_temperature = 63.151, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 12523., &
      reducing_temperature = 63.151, &
      n_coeff = 1, &
      n_coeff_1 = 0, &
      n_coeff_2 = 1, &
      n_coeff_3 = 0, &
      coeff = (/-1.308869200000e+01,0.000000000000e+00,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/1.000000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (1999)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL12 = &
      solid_correlation_data( &
      compName = "N2O", &
      correlation = "SL-2", &
      triple_temperature = 182.33, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      reducing_temperature = 1., &
      n_coeff = 4, &
      n_coeff_1 = 4, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/1.379787540000e+01,-3.181209790000e+03,6.345151470000e+04, &
      -4.189965370000e+06,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,0.000000,0.000000/), &
      bib_ref = "Bell, I.H. (2018)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL13 = &
      solid_correlation_data( &
      compName = "O2", &
      correlation = "SL-3", &
      triple_temperature = 54.361, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 146.28, &
      reducing_temperature = 54.361, &
      n_coeff = 1, &
      n_coeff_1 = 0, &
      n_coeff_2 = 1, &
      n_coeff_3 = 0, &
      coeff = (/-2.071400000000e+01,0.000000000000e+00,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/1.060000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2003)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL14 = &
      solid_correlation_data( &
      compName = "P-H2", &
      correlation = "SL-2", &
      triple_temperature = 13.8, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      reducing_temperature = 1., &
      n_coeff = 6, &
      n_coeff_1 = 6, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/4.782880000000e+00,-1.485636000000e+02,2.323210000000e+02, &
      -5.602070000000e+02,6.641260000000e+02,-2.890600000000e+02/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,-4.000000,-5.000000/), &
      bib_ref = "DOI: 10.1007/978-1-4613-9856-1_76", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL15 = &
      solid_correlation_data( &
      compName = "F6S", &
      correlation = "SL-2", &
      triple_temperature = 223.555, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 231429., &
      reducing_temperature = 223.555, &
      n_coeff = 2, &
      n_coeff_1 = 2, &
      n_coeff_2 = 0, &
      n_coeff_3 = 0, &
      coeff = (/-1.169421410000e+01,1.169421410000e+01,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/-1.070000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.3037344", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL16 = &
      solid_correlation_data( &
      compName = "XE", &
      correlation = "SL-3", &
      triple_temperature = 161.405, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 81750., &
      reducing_temperature = 161.405, &
      n_coeff = 2, &
      n_coeff_1 = 0, &
      n_coeff_2 = 2, &
      n_coeff_3 = 0, &
      coeff = (/-1.390000000000e+01,1.400000000000e+01,0.000000000000e+00, &
      0.000000000000e+00,0.000000000000e+00,0.000000000000e+00/), &
      exponents = (/1.060000,3.100000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2003)", &
      ref = "DEFAULT" &
      )

  integer, parameter :: n_melting_curves = 23
  type(solid_correlation_data), dimension(n_melting_curves), parameter :: melting_corr_array = (/&
      MELT1,MELT2,MELT3,MELT4,MELT5, &
      MELT6,MELT7,MELT8,MELT9,MELT10, &
      MELT11,MELT12,MELT13,MELT14,MELT15, &
      MELT16,MELT17,MELT18,MELT19,MELT20, &
      MELT21,MELT22,MELT23 &
  /)

  integer, parameter :: n_sublimation_curves = 16
  type(solid_correlation_data), dimension(n_sublimation_curves), parameter :: sublimation_corr_array = (/&
      SUBL1,SUBL2,SUBL3,SUBL4,SUBL5, &
      SUBL6,SUBL7,SUBL8,SUBL9,SUBL10, &
      SUBL11,SUBL12,SUBL13,SUBL14,SUBL15, &
      SUBL16 &
  /)

end module solid_correlation_datadb
