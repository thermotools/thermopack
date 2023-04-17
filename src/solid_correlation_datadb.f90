!> Automatically generated to file solid_correlation_datadb.f90
!! using utility python code pyUtils
!! Time stamp: 2023-04-13T14:56:53.128093

module solid_correlation_datadb
  use thermopack_constants, only: uid_len, ref_len, bibref_len
  implicit none
  public

  !> This data structure stores parameters for
  !> sublimation and melting line correlations.
  ! ---------------------------------------------------------------------------
  type :: solid_correlation_data
    character(len=uid_len) :: compName
    character(len=2) :: correlation !< Correlations type
    real :: triple_temperature  !< [K]. Triple point temperature.
    real :: minimum_temperature  !< [K]. Minimum temperature for sublimation line.
    real :: maximum_temperature  !< [K]. Maximum temperature for melting line.
    real :: reducing_pressure !< [Pa]. Pressure scaling parameter.
    integer :: n_coeff !< Number of coefficients
    real :: coeff(6)  !< Correlation coefficients
    real :: exponents(6) !< Correlation exponents.
    character(len=bibref_len) :: bib_ref !< Bibliograpic reference.
    character(len=ref_len) :: ref !< Parameter set
  end type solid_correlation_data

  type(solid_correlation_data), parameter :: MELT1 = &
      solid_correlation_data( &
      compName = "NH3", &
      correlation = "M1", &
      triple_temperature = 195.49, &
      minimum_temperature = 0., &
      maximum_temperature = 10000., &
      reducing_pressure = 1000000., &
      n_coeff = 1, &
      coeff = (/6.06150000e-03,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.555579", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT2 = &
      solid_correlation_data( &
      compName = "AR", &
      correlation = "M1", &
      triple_temperature = 83.8058, &
      minimum_temperature = 0., &
      maximum_temperature = 700., &
      reducing_pressure = 68891., &
      n_coeff = 5, &
      coeff = (/1.00000000e+00,-7.47626651e+03,9.95906125e+03, &
      7.47626651e+03,-9.95906125e+03,0.00000000e+00/), &
      exponents = (/0.000000,1.050000,1.275000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.556037", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT3 = &
      solid_correlation_data( &
      compName = "CO2", &
      correlation = "M1", &
      triple_temperature = 216.592, &
      minimum_temperature = 0., &
      maximum_temperature = 1100., &
      reducing_pressure = 517950., &
      n_coeff = 3, &
      coeff = (/1.00000000e+00,1.95553900e+03,2.05545930e+03, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.000000,2.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.555991", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT4 = &
      solid_correlation_data( &
      compName = "CO", &
      correlation = "M1", &
      triple_temperature = 68.16, &
      minimum_temperature = 0., &
      maximum_temperature = 1000., &
      reducing_pressure = 1000000., &
      n_coeff = 2, &
      coeff = (/-1.43036880e+02,1.95608000e-02,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,2.107470,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/0021-9614(82)90044-1", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT5 = &
      solid_correlation_data( &
      compName = "CYCLOHEX", &
      correlation = "M1", &
      triple_temperature = 279.86, &
      minimum_temperature = 0., &
      maximum_temperature = 700., &
      reducing_pressure = 5348.7, &
      n_coeff = 3, &
      coeff = (/1.00000000e+00,7.50000000e+01,1.02000000e+05, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,2.000000,1.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.4900538", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT6 = &
      solid_correlation_data( &
      compName = "C2", &
      correlation = "M1", &
      triple_temperature = 90.368, &
      minimum_temperature = 0., &
      maximum_temperature = 2000., &
      reducing_pressure = 1.1421, &
      n_coeff = 4, &
      coeff = (/1.00000000e+00,1.05262374e+08,-1.05262374e+08, &
      2.23626315e+08,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,2.550000,0.000000, &
      1.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1859286", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT7 = &
      solid_correlation_data( &
      compName = "HE", &
      correlation = "M1", &
      triple_temperature = 2.1768, &
      minimum_temperature = 0., &
      maximum_temperature = 1500., &
      reducing_pressure = 1000000., &
      n_coeff = 2, &
      coeff = (/-1.74558370e+00,1.69797930e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.555414,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1007/978-1-4613-0639-9_174", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT8 = &
      solid_correlation_data( &
      compName = "H2", &
      correlation = "M1", &
      triple_temperature = 13.957, &
      minimum_temperature = 0., &
      maximum_temperature = 400., &
      reducing_pressure = 7357.8, &
      n_coeff = 3, &
      coeff = (/1.00000000e+00,5.62630000e+03,2.71720000e+03, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.000000,1.830000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT9 = &
      solid_correlation_data( &
      compName = "IC4", &
      correlation = "M1", &
      triple_temperature = 113.73, &
      minimum_temperature = 0., &
      maximum_temperature = 575., &
      reducing_pressure = 0.022891, &
      n_coeff = 2, &
      coeff = (/-1.95363713e+09,1.95363713e+09,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,6.120000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1901687", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT10 = &
      solid_correlation_data( &
      compName = "IC5", &
      correlation = "M1", &
      triple_temperature = 112.65, &
      minimum_temperature = 0., &
      maximum_temperature = 2000., &
      reducing_pressure = 0.00008952, &
      n_coeff = 2, &
      coeff = (/-7.12770000e+12,7.12770000e+12,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.563000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1725068", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT11 = &
      solid_correlation_data( &
      compName = "KR", &
      correlation = "M1", &
      triple_temperature = 115.775, &
      minimum_temperature = 0., &
      maximum_temperature = 800., &
      reducing_pressure = 101325., &
      n_coeff = 2, &
      coeff = (/-2.34592100e+03,1.08047669e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.616984,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/0031-8914(62)90096-4", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT12 = &
      solid_correlation_data( &
      compName = "C1", &
      correlation = "M1", &
      triple_temperature = 90.6941, &
      minimum_temperature = 0., &
      maximum_temperature = 625., &
      reducing_pressure = 11696., &
      n_coeff = 5, &
      coeff = (/1.00000000e+00,2.47568000e+04,-7.36602000e+03, &
      -2.47568000e+04,7.36602000e+03,0.00000000e+00/), &
      exponents = (/0.000000,1.850000,2.100000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.555898", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT13 = &
      solid_correlation_data( &
      compName = "MEOH", &
      correlation = "M1", &
      triple_temperature = 175.61, &
      minimum_temperature = 0., &
      maximum_temperature = 620., &
      reducing_pressure = 0.187, &
      n_coeff = 4, &
      coeff = (/1.00000000e+00,5.32077000e+09,4.52478000e+09, &
      3.88886100e+10,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.000000,1.500000, &
      4.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/j.fluid.2013.03.024", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT14 = &
      solid_correlation_data( &
      compName = "NE", &
      correlation = "M1", &
      triple_temperature = 24.556, &
      minimum_temperature = 0., &
      maximum_temperature = 10000., &
      reducing_pressure = 43368.14, &
      n_coeff = 2, &
      coeff = (/1.00000000e+00,4.43700000e+03,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.330000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2010)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT15 = &
      solid_correlation_data( &
      compName = "N2", &
      correlation = "M1", &
      triple_temperature = 63.151, &
      minimum_temperature = 0., &
      maximum_temperature = 2000., &
      reducing_pressure = 12519.8, &
      n_coeff = 3, &
      coeff = (/1.00000000e+00,1.27986100e+04,-1.27986100e+04, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.789630,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1349047", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT16 = &
      solid_correlation_data( &
      compName = "O2", &
      correlation = "M2", &
      triple_temperature = 54.361, &
      minimum_temperature = 0., &
      maximum_temperature = 300., &
      reducing_pressure = 146.277, &
      n_coeff = 4, &
      coeff = (/-3.24635390e+01,1.42780110e+02,-1.47023410e+02, &
      5.20012000e+01,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.062500,0.125000,0.187500, &
      0.250000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/0378-3812(85)87016-3", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT17 = &
      solid_correlation_data( &
      compName = "P-H2", &
      correlation = "MP", &
      triple_temperature = 13.8, &
      minimum_temperature = 0., &
      maximum_temperature = 400., &
      reducing_pressure = 1000000., &
      n_coeff = 4, &
      coeff = (/-2.65289115e+01,2.48578596e-01,-2.12823393e+01, &
      1.25746643e-01,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.764739,0.000000, &
      1.955000,0.000000,0.000000/), &
      bib_ref = "ISBN: 088318415X, https://srd.nist.gov/JPCRD/jpcrdS1Vol11.pdf", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT18 = &
      solid_correlation_data( &
      compName = "C3", &
      correlation = "M1", &
      triple_temperature = 85.525, &
      minimum_temperature = 0., &
      maximum_temperature = 2000., &
      reducing_pressure = 0.00017205, &
      n_coeff = 2, &
      coeff = (/-4.23000000e+12,4.23000000e+12,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.283000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1725068", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT19 = &
      solid_correlation_data( &
      compName = "PRLN", &
      correlation = "M1", &
      triple_temperature = 87.953, &
      minimum_temperature = 0., &
      maximum_temperature = 2000., &
      reducing_pressure = 0.0007471, &
      n_coeff = 2, &
      coeff = (/-6.59300000e+09,6.59300000e+09,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,2.821000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1725068", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT20 = &
      solid_correlation_data( &
      compName = "F6S", &
      correlation = "M1", &
      triple_temperature = 223.555, &
      minimum_temperature = 0., &
      maximum_temperature = 650., &
      reducing_pressure = 231429., &
      n_coeff = 3, &
      coeff = (/1.00000000e+00,9.66603148e+02,-9.66603148e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.555000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.5005537", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT21 = &
      solid_correlation_data( &
      compName = "XE", &
      correlation = "M1", &
      triple_temperature = 161.405, &
      minimum_temperature = 0., &
      maximum_temperature = 1300., &
      reducing_pressure = 101325., &
      n_coeff = 2, &
      coeff = (/-2.57507280e+03,7.98327703e-01,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.589165,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/0031-8914(62)90096-4", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT22 = &
      solid_correlation_data( &
      compName = "NC4", &
      correlation = "M1", &
      triple_temperature = 134.895, &
      minimum_temperature = 0., &
      maximum_temperature = 575., &
      reducing_pressure = 0.66566, &
      n_coeff = 2, &
      coeff = (/-5.58558235e+08,5.58558236e+08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,2.206000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1901687", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: MELT23 = &
      solid_correlation_data( &
      compName = "NC5", &
      correlation = "M1", &
      triple_temperature = 143.47, &
      minimum_temperature = 0., &
      maximum_temperature = 2000., &
      reducing_pressure = 0.076322, &
      n_coeff = 2, &
      coeff = (/-8.64750000e+09,8.64750000e+09,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,1.649000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.1725068", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL0 = &
      solid_correlation_data( &
      compName = "NH3", &
      correlation = "S2", &
      triple_temperature = 195.49, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      n_coeff = 5, &
      coeff = (/1.36395000e+01,-3.53700000e+03,-3.31000000e+04, &
      1.74200000e+06,-2.99500000e+07,0.00000000e+00/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,-4.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/j.pss.2009.09.011", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL1 = &
      solid_correlation_data( &
      compName = "AR", &
      correlation = "S3", &
      triple_temperature = 83.8058, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 68891., &
      n_coeff = 1, &
      coeff = (/-1.11307000e+01,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/1.000000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2002)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL2 = &
      solid_correlation_data( &
      compName = "CO2", &
      correlation = "S3", &
      triple_temperature = 216.592, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 517950., &
      n_coeff = 3, &
      coeff = (/-1.47408460e+01,2.43270150e+00,-5.30617780e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/1.000000,1.900000,2.900000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.555991", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL3 = &
      solid_correlation_data( &
      compName = "CO", &
      correlation = "S2", &
      triple_temperature = 68.16, &
      minimum_temperature = 61.55, &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      n_coeff = 4, &
      coeff = (/7.94524000e+00,-7.48151000e+02,-5.84330000e+03, &
      3.93850000e+04,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1007/978-1-4613-9856-1_76", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL4 = &
      solid_correlation_data( &
      compName = "C2", &
      correlation = "S2", &
      triple_temperature = 90.368, &
      minimum_temperature = 20., &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      n_coeff = 6, &
      coeff = (/1.28111000e+01,-2.20746000e+03,-2.41130000e+04, &
      7.74440000e+05,-1.16150000e+07,6.76330000e+07/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,-4.000000,-5.000000/), &
      bib_ref = "DOI: 10.1007/978-1-4613-9856-1_76", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL5 = &
      solid_correlation_data( &
      compName = "H2", &
      correlation = "S3", &
      triple_temperature = 13.957, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 7700., &
      n_coeff = 1, &
      coeff = (/-8.06500000e+00,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.930000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2003)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL6 = &
      solid_correlation_data( &
      compName = "H2S", &
      correlation = "S2", &
      triple_temperature = 187.7, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      n_coeff = 5, &
      coeff = (/6.62470000e+00,-7.26000000e+02,-3.50400000e+05, &
      2.72400000e+07,-8.58200000e+08,0.00000000e+00/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,-4.000000,0.000000/), &
      bib_ref = "DOI: 10.1016/j.pss.2009.09.011", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL7 = &
      solid_correlation_data( &
      compName = "KR", &
      correlation = "S3", &
      triple_temperature = 115.775, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 73197., &
      n_coeff = 1, &
      coeff = (/-1.15616000e+01,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/1.000000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2002)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL8 = &
      solid_correlation_data( &
      compName = "C1", &
      correlation = "S3", &
      triple_temperature = 90.6941, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 11696., &
      n_coeff = 1, &
      coeff = (/-1.28400000e+01,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/1.000000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2002)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL9 = &
      solid_correlation_data( &
      compName = "NE", &
      correlation = "S2", &
      triple_temperature = 10000., &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      n_coeff = 5, &
      coeff = (/8.30710000e+00,-3.08555000e+02,9.86020000e+02, &
      -9.06930000e+03,3.51420000e+04,0.00000000e+00/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,-4.000000,0.000000/), &
      bib_ref = "DOI: 10.1007/978-1-4613-9856-1_76", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL10 = &
      solid_correlation_data( &
      compName = "N2", &
      correlation = "S3", &
      triple_temperature = 63.151, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 12523., &
      n_coeff = 1, &
      coeff = (/-1.30886920e+01,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/1.000000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (1999)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL11 = &
      solid_correlation_data( &
      compName = "N2O", &
      correlation = "S2", &
      triple_temperature = 182.33, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      n_coeff = 4, &
      coeff = (/1.37978754e+01,-3.18120979e+03,6.34515147e+04, &
      -4.18996537e+06,0.00000000e+00,0.00000000e+00/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,0.000000,0.000000/), &
      bib_ref = "Bell, I.H. (2018)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL12 = &
      solid_correlation_data( &
      compName = "O2", &
      correlation = "S3", &
      triple_temperature = 54.361, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 146.28, &
      n_coeff = 1, &
      coeff = (/-2.07140000e+01,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/1.060000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "Lemmon, E.W. (2003)", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL13 = &
      solid_correlation_data( &
      compName = "P-H2", &
      correlation = "S2", &
      triple_temperature = 13.8, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 1000000., &
      n_coeff = 6, &
      coeff = (/4.78288000e+00,-1.48563600e+02,2.32321000e+02, &
      -5.60207000e+02,6.64126000e+02,-2.89060000e+02/), &
      exponents = (/0.000000,-1.000000,-2.000000, &
      -3.000000,-4.000000,-5.000000/), &
      bib_ref = "DOI: 10.1007/978-1-4613-9856-1_76", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL14 = &
      solid_correlation_data( &
      compName = "F6S", &
      correlation = "S2", &
      triple_temperature = 223.555, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 231429., &
      n_coeff = 2, &
      coeff = (/-1.16942141e+01,1.16942141e+01,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      exponents = (/-1.070000,0.000000,0.000000, &
      0.000000,0.000000,0.000000/), &
      bib_ref = "DOI: 10.1063/1.3037344", &
      ref = "DEFAULT" &
      )

  type(solid_correlation_data), parameter :: SUBL15 = &
      solid_correlation_data( &
      compName = "XE", &
      correlation = "S3", &
      triple_temperature = 161.405, &
      minimum_temperature = 0., &
      maximum_temperature = 0., &
      reducing_pressure = 81750., &
      n_coeff = 2, &
      coeff = (/-1.39000000e+01,1.40000000e+01,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
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

  integer, parameter :: n_sublimation_curves = 15
  type(solid_correlation_data), dimension(n_sublimation_curves), parameter :: sublimation_corr_array = (/&
      SUBL1,SUBL2,SUBL3,SUBL4,SUBL5, &
      SUBL6,SUBL7,SUBL8,SUBL9,SUBL10, &
      SUBL11,SUBL12,SUBL13,SUBL14,SUBL15 &
  /)

end module solid_correlation_datadb
