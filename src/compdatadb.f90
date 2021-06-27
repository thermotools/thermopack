!> Automatically generated to file compdatadb.f90
!! using utility python code pyUtils
!! Time stamp: 2021-06-27T20:40:30.227534

module compdatadb
  use compdata, only: gendatadb, cpdata, alphadatadb, cidatadb
  implicit none
  public

  type (gendatadb), parameter :: cx1 = &
      gendatadb(ident = "BUT1OL", &
      formula = "C4H10O", &
      name = "1-BUTANOL", &
      mw = 74.1216, &
      Tc = 562.2000, &
      Pc = 4500000.00, &
      Zc = 0.263800, &
      acf = 0.592980, &
      Tb = 390.6000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.254900 &
      )

  type (cpdata), parameter :: cp1 = &
      cpdata(cid = "BUT1OL", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu1 = &
      alphadatadb(eosid="PR", &
      cid="BUT1OL", &
      ref="tcPR", &
      coeff=(/1.62670000e+00, 1.00000000e+00, 6.99800000e-01/) &
      )

  type (cidatadb), parameter :: c1 = &
      cidatadb(eosid="PR", &
      cid="BUT1OL", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.91930000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu2 = &
      alphadatadb(eosid="SRK", &
      cid="BUT1OL", &
      ref="tcRK", &
      coeff=(/1.51560000e+00, 1.00000000e+00, 8.55400000e-01/) &
      )

  type (cidatadb), parameter :: c2 = &
      cidatadb(eosid="SRK", &
      cid="BUT1OL", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.74514000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx2 = &
      gendatadb(ident = "HEX1OL", &
      formula = "C6H14O", &
      name = "1-HEXANOL", &
      mw = 102.1748, &
      Tc = 610.5000, &
      Pc = 3413000.00, &
      Zc = 0.260200, &
      acf = 0.570000, &
      Tb = 430.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.254400 &
      )

  type (cpdata), parameter :: cp2 = &
      cpdata(cid = "HEX1OL", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu3 = &
      alphadatadb(eosid="PR", &
      cid="HEX1OL", &
      ref="tcPR", &
      coeff=(/2.35820000e+00, 1.00000000e+00, 4.48600000e-01/) &
      )

  type (cidatadb), parameter :: c3 = &
      cidatadb(eosid="PR", &
      cid="HEX1OL", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=4.15340000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu4 = &
      alphadatadb(eosid="SRK", &
      cid="HEX1OL", &
      ref="tcRK", &
      coeff=(/2.08320000e+00, 1.00000000e+00, 5.78300000e-01/) &
      )

  type (cidatadb), parameter :: c4 = &
      cidatadb(eosid="SRK", &
      cid="HEX1OL", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.59066000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx3 = &
      gendatadb(ident = "PENT1OL", &
      formula = "C5H12O", &
      name = "1-PENTANOL", &
      mw = 88.1482, &
      Tc = 580.0000, &
      Pc = 3900000.00, &
      Zc = 0.264300, &
      acf = 0.578980, &
      Tb = 411.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.256100 &
      )

  type (cpdata), parameter :: cp3 = &
      cpdata(cid = "PENT1OL", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu5 = &
      alphadatadb(eosid="PR", &
      cid="PENT1OL", &
      ref="tcPR", &
      coeff=(/1.68580000e+00, 1.00000000e+00, 6.61800000e-01/) &
      )

  type (cidatadb), parameter :: c5 = &
      cidatadb(eosid="PR", &
      cid="PENT1OL", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.47820000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu6 = &
      alphadatadb(eosid="SRK", &
      cid="PENT1OL", &
      ref="tcRK", &
      coeff=(/1.63720000e+00, 1.00000000e+00, 7.71700000e-01/) &
      )

  type (cidatadb), parameter :: c6 = &
      cidatadb(eosid="SRK", &
      cid="PENT1OL", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.99375000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx4 = &
      gendatadb(ident = "PROP1OL", &
      formula = "C3H8O", &
      name = "1-PROPANOL", &
      mw = 60.0950, &
      Tc = 536.9000, &
      Pc = 5200000.00, &
      Zc = 0.254300, &
      acf = 0.623000, &
      Tb = 370.3000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.251100 &
      )

  type (cpdata), parameter :: cp4 = &
      cpdata(cid = "PROP1OL", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu7 = &
      alphadatadb(eosid="PR", &
      cid="PROP1OL", &
      ref="tcPR", &
      coeff=(/1.37950000e+00, 1.00000000e+00, 8.77200000e-01/) &
      )

  type (cidatadb), parameter :: c7 = &
      cidatadb(eosid="PR", &
      cid="PROP1OL", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=3.27770000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu8 = &
      alphadatadb(eosid="SRK", &
      cid="PROP1OL", &
      ref="tcRK", &
      coeff=(/1.37350000e+00, 1.00000000e+00, 9.94100000e-01/) &
      )

  type (cidatadb), parameter :: c8 = &
      cidatadb(eosid="SRK", &
      cid="PROP1OL", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.58803000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx5 = &
      gendatadb(ident = "13BD", &
      formula = "C4H6", &
      name = "1,3-BUTADIENE", &
      mw = 54.0920, &
      Tc = 425.0000, &
      Pc = 4330000.00, &
      Zc = 0.270000, &
      acf = 0.195000, &
      Tb = 268.7000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.267500 &
      )

  type (cpdata), parameter :: cp5 = &
      cpdata(cid = "13BD", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/-1.68700000e+00,3.41900000e-01,-2.34000000e-04,6.33500000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu9 = &
      alphadatadb(eosid="PR", &
      cid="13BD", &
      ref="tcPR", &
      coeff=(/1.61800000e-01, 8.59200000e-01, 2.40880000e+00/) &
      )

  type (cidatadb), parameter :: c9 = &
      cidatadb(eosid="PR", &
      cid="13BD", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.61400000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu10 = &
      alphadatadb(eosid="SRK", &
      cid="13BD", &
      ref="tcRK", &
      coeff=(/2.22300000e-01, 8.58400000e-01, 2.41930000e+00/) &
      )

  type (cidatadb), parameter :: c10 = &
      cidatadb(eosid="SRK", &
      cid="13BD", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.12082000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx6 = &
      gendatadb(ident = "2MHX", &
      formula = "C7H16", &
      name = "2-METHYLHEXANE", &
      mw = 100.2050, &
      Tc = 530.4000, &
      Pc = 2730000.00, &
      Zc = 0.261000, &
      acf = 0.329000, &
      Tb = 363.2000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.263800 &
      )

  type (cpdata), parameter :: cp6 = &
      cpdata(cid = "2MHX", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/1.78937090e+01,4.04849000e-01,1.33465300e-03,2.87769800e-06,-3.51181800e-09, &
      1.25400550e-12,1.82345600e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -75.0000, &
      Tcpmax = 700.0000  &
      )

  type (alphadatadb), parameter :: twu11 = &
      alphadatadb(eosid="PR", &
      cid="2MHX", &
      ref="tcPR", &
      coeff=(/3.79600000e-01, 8.10300000e-01, 1.68300000e+00/) &
      )

  type (cidatadb), parameter :: c11 = &
      cidatadb(eosid="PR", &
      cid="2MHX", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=7.27700000e-07, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu12 = &
      alphadatadb(eosid="SRK", &
      cid="2MHX", &
      ref="tcRK", &
      coeff=(/3.42600000e-01, 8.32200000e-01, 2.20210000e+00/) &
      )

  type (cidatadb), parameter :: c12 = &
      cidatadb(eosid="SRK", &
      cid="2MHX", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.51160000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx7 = &
      gendatadb(ident = "3MP", &
      formula = "C6H14", &
      name = "3-METHYLPENTANE", &
      mw = 86.1780, &
      Tc = 504.5000, &
      Pc = 3120000.00, &
      Zc = 0.273000, &
      acf = 0.272000, &
      Tb = 336.4000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.269000 &
      )

  type (cpdata), parameter :: cp7 = &
      cpdata(cid = "3MP", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/1.79647680e+01,3.97799000e-01,1.20987000e-03,3.25455600e-06,-3.94266100e-09, &
      1.43841480e-12,2.14954100e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -75.0000, &
      Tcpmax = 700.0000  &
      )

  type (alphadatadb), parameter :: twu13 = &
      alphadatadb(eosid="PR", &
      cid="3MP", &
      ref="tcPR", &
      coeff=(/2.52500000e-01, 8.32200000e-01, 2.04680000e+00/) &
      )

  type (cidatadb), parameter :: c13 = &
      cidatadb(eosid="PR", &
      cid="3MP", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.74790000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu14 = &
      alphadatadb(eosid="SRK", &
      cid="3MP", &
      ref="tcRK", &
      coeff=(/2.76300000e-01, 8.45100000e-01, 2.37500000e+00/) &
      )

  type (cidatadb), parameter :: c14 = &
      cidatadb(eosid="SRK", &
      cid="3MP", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.88966000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx8 = &
      gendatadb(ident = "ACETONE", &
      formula = "C3H6O", &
      name = "ACETONE", &
      mw = 58.0800, &
      Tc = 508.1000, &
      Pc = 4700000.00, &
      Zc = 0.233000, &
      acf = 0.307000, &
      Tb = 329.2200, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.244200 &
      )

  type (cpdata), parameter :: cp8 = &
      cpdata(cid = "ACETONE", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 6, &
      cp = (/7.33799960e-01,2.16303500e-04,8.20407250e-06,-1.02740600e-08,3.90520150e-12, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 50.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu15 = &
      alphadatadb(eosid="PR", &
      cid="ACETONE", &
      ref="tcPR", &
      coeff=(/6.04100000e-01, 8.40200000e-01, 1.19840000e+00/) &
      )

  type (cidatadb), parameter :: c15 = &
      cidatadb(eosid="PR", &
      cid="ACETONE", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.26537000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu16 = &
      alphadatadb(eosid="SRK", &
      cid="ACETONE", &
      ref="tcRK", &
      coeff=(/3.20700000e-01, 8.54700000e-01, 2.36300000e+00/) &
      )

  type (cidatadb), parameter :: c16 = &
      cidatadb(eosid="SRK", &
      cid="ACETONE", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.61512000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx9 = &
      gendatadb(ident = "ACETYLEN", &
      formula = "C2H2", &
      name = "ACETYLEN", &
      mw = 26.0380, &
      Tc = 308.3000, &
      Pc = 6140000.00, &
      Zc = 0.270000, &
      acf = 0.190000, &
      Tb = 188.4000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = -1.000000 &
      )

  type (cpdata), parameter :: cp9 = &
      cpdata(cid = "ACETYLEN", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/2.68200000e+01,7.57800000e-02,-5.00700000e-05,1.41200000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (gendatadb), parameter :: cx10 = &
      gendatadb(ident = "ALLENE", &
      formula = "C3H4", &
      name = "PROPADIENE", &
      mw = 40.0650, &
      Tc = 393.8500, &
      Pc = 5248600.00, &
      Zc = 0.259700, &
      acf = 0.120000, &
      Tb = 238.6500, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/6.53610000e+00, 1.05472000e+03, -7.70800000e+01/), &
      Tantmin = 174.0000, &
      Tantmax = 257.0000, &
      Zra = 0.267700 &
      )

  type (cpdata), parameter :: cp10 = &
      cpdata(cid = "ALLENE", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/9.90600000e+00,1.97700000e-01,-1.18200000e-04,2.78200000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu17 = &
      alphadatadb(eosid="PR", &
      cid="ALLENE", &
      ref="tcPR", &
      coeff=(/1.08530000e+00, 4.78800000e-01, 4.59300000e-01/) &
      )

  type (cidatadb), parameter :: c17 = &
      cidatadb(eosid="PR", &
      cid="ALLENE", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.92300000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu18 = &
      alphadatadb(eosid="SRK", &
      cid="ALLENE", &
      ref="tcRK", &
      coeff=(/4.19800000e-01, 7.25600000e-01, 1.03070000e+00/) &
      )

  type (cidatadb), parameter :: c18 = &
      cidatadb(eosid="SRK", &
      cid="ALLENE", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.18997000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx11 = &
      gendatadb(ident = "NH3", &
      formula = "NH3", &
      name = "AMMONIA", &
      mw = 17.0310, &
      Tc = 405.6000, &
      Pc = 11470000.00, &
      Zc = 0.242000, &
      acf = 0.250000, &
      Tb = 239.7000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.69481000e+01, 2.13250000e+03, -3.29800000e+01/), &
      Tantmin = 179.0000, &
      Tantmax = 261.0000, &
      Zra = 0.246500 &
      )

  type (cpdata), parameter :: cp11 = &
      cpdata(cid = "NH3", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/-2.20260600e+00,2.01031700e+00,-6.50061000e-04,2.37326400e-06,-1.59759500e-09, &
      3.76173900e-13,9.90447000e-01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu19 = &
      alphadatadb(eosid="PR", &
      cid="NH3", &
      ref="tcPR", &
      coeff=(/2.27400000e-01, 8.64500000e-01, 2.33200000e+00/) &
      )

  type (alphadatadb), parameter :: mc1 = &
      alphadatadb(eosid="PR", &
      cid="NH3", &
      ref="Chapoy2005", &
      coeff=(/7.48000000e-01, -2.50000000e-02, 1.00000000e-03/) &
      )

  type (cidatadb), parameter :: c19 = &
      cidatadb(eosid="PR", &
      cid="NH3", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=4.03030000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu20 = &
      alphadatadb(eosid="SRK", &
      cid="NH3", &
      ref="tcRK", &
      coeff=(/2.98100000e-01, 8.65100000e-01, 2.32450000e+00/) &
      )

  type (alphadatadb), parameter :: mc2 = &
      alphadatadb(eosid="SRK", &
      cid="NH3", &
      ref="Chapoy2005", &
      coeff=(/9.16000000e-01, -3.69000000e-01, 4.17000000e-01/) &
      )

  type (cidatadb), parameter :: c20 = &
      cidatadb(eosid="SRK", &
      cid="NH3", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=8.63980000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx12 = &
      gendatadb(ident = "AR", &
      formula = "AR", &
      name = "ARGON", &
      mw = 39.9480, &
      Tc = 150.8000, &
      Pc = 4873700.00, &
      Zc = 0.291000, &
      acf = -0.004000, &
      Tb = 87.3000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.52330000e+01, 7.00510000e+02, -5.84000000e+00/), &
      Tantmin = 81.0000, &
      Tantmax = 94.0000, &
      Zra = 0.308500 &
      )

  type (cpdata), parameter :: cp12 = &
      cpdata(cid = "AR", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/4.96900000e+00,-7.67000000e-06,1.23400000e-08,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu21 = &
      alphadatadb(eosid="PR", &
      cid="AR", &
      ref="tcPR", &
      coeff=(/1.22800000e-01, 9.04500000e-01, 1.85390000e+00/) &
      )

  type (alphadatadb), parameter :: mc3 = &
      alphadatadb(eosid="PR", &
      cid="AR", &
      ref="Default", &
      coeff=(/3.97483000e-01, -2.82393000e-01, 7.96288000e-01/) &
      )

  type (cidatadb), parameter :: c21 = &
      cidatadb(eosid="PR", &
      cid="AR", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.29390000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu22 = &
      alphadatadb(eosid="SRK", &
      cid="AR", &
      ref="tcRK", &
      coeff=(/2.02300000e-01, 9.08600000e-01, 1.81290000e+00/) &
      )

  type (cidatadb), parameter :: c22 = &
      cidatadb(eosid="SRK", &
      cid="AR", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=8.72800000e-07, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx13 = &
      gendatadb(ident = "BENZENE", &
      formula = "C6H6", &
      name = "BENZENE", &
      mw = 78.1140, &
      Tc = 562.1000, &
      Pc = 4894000.00, &
      Zc = 0.271000, &
      acf = 0.212000, &
      Tb = 353.3000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.59008000e+01, 2.78851000e+03, -5.23600000e+01/), &
      Tantmin = 280.0000, &
      Tantmax = 377.0000, &
      Zra = 0.269800 &
      )

  type (cpdata), parameter :: cp13 = &
      cpdata(cid = "BENZENE", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/8.44670620e+01,-5.13560000e-01,3.24874000e-03,-1.54391300e-06,3.65037000e-10, &
      -2.48222000e-14,5.63104100e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: mc4 = &
      alphadatadb(eosid="PR", &
      cid="BENZENE", &
      ref="Chapoy2005", &
      coeff=(/7.01000000e-01, -2.52000000e-01, 9.76000000e-01/) &
      )

  type (alphadatadb), parameter :: twu23 = &
      alphadatadb(eosid="PR", &
      cid="BENZENE", &
      ref="tcPR", &
      coeff=(/1.26100000e-01, 8.46000000e-01, 2.61370000e+00/) &
      )

  type (cidatadb), parameter :: c23 = &
      cidatadb(eosid="PR", &
      cid="BENZENE", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.39140000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: mc5 = &
      alphadatadb(eosid="SRK", &
      cid="BENZENE", &
      ref="Chapoy2005", &
      coeff=(/8.40000000e-01, -3.89000000e-01, 9.17000000e-01/) &
      )

  type (alphadatadb), parameter :: twu24 = &
      alphadatadb(eosid="SRK", &
      cid="BENZENE", &
      ref="tcRK", &
      coeff=(/1.82300000e-01, 8.44800000e-01, 2.63540000e+00/) &
      )

  type (cidatadb), parameter :: c24 = &
      cidatadb(eosid="SRK", &
      cid="BENZENE", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.35115000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx14 = &
      gendatadb(ident = "CO2", &
      formula = "CO2", &
      name = "CARBON DIOXIDE", &
      mw = 44.0100, &
      Tc = 304.2000, &
      Pc = 7376500.00, &
      Zc = 0.274000, &
      acf = 0.225000, &
      Tb = 194.7000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/2.25898000e+01, 3.10339000e+03, -1.60000000e-01/), &
      Tantmin = 154.0000, &
      Tantmax = 204.0000, &
      Zra = 0.272200 &
      )

  type (cpdata), parameter :: cp14 = &
      cpdata(cid = "CO2", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/1.11137440e+01,4.79107000e-01,7.62159000e-04,-3.59392000e-07,8.47440000e-11, &
      -5.77520000e-15,2.71918000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (cpdata), parameter :: cp15 = &
      cpdata(cid = "CO2", &
      ref = "", &
      bib_ref = "", &
      cptype = 5, &
      cp = (/5.67998955e-01,1.25397835e-03,-7.65547666e-07,1.80606165e-10,-3.10473875e+03, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 50.0000, &
      Tcpmax = 5000.0000  &
      )

  type (alphadatadb), parameter :: twu25 = &
      alphadatadb(eosid="PR", &
      cid="CO2", &
      ref="tcPR", &
      coeff=(/1.78300000e-01, 8.59000000e-01, 2.41070000e+00/) &
      )

  type (alphadatadb), parameter :: mc6 = &
      alphadatadb(eosid="PR", &
      cid="CO2", &
      ref="Default", &
      coeff=(/7.04606000e-01, -3.14862000e-01, 1.89083000e+00/) &
      )

  type (alphadatadb), parameter :: mc7 = &
      alphadatadb(eosid="PR", &
      cid="CO2", &
      ref="Chapoy2005", &
      coeff=(/7.05000000e-01, -3.15000000e-01, 1.89000000e+00/) &
      )

  type (cidatadb), parameter :: c25 = &
      cidatadb(eosid="PR", &
      cid="CO2", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.13680000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu26 = &
      alphadatadb(eosid="SRK", &
      cid="CO2", &
      ref="tcRK", &
      coeff=(/2.80700000e-01, 8.68500000e-01, 2.27780000e+00/) &
      )

  type (alphadatadb), parameter :: mc8 = &
      alphadatadb(eosid="SRK", &
      cid="CO2", &
      ref="Chapoy2005", &
      coeff=(/8.67000000e-01, -6.74000000e-01, 2.47100000e+00/) &
      )

  type (cidatadb), parameter :: c26 = &
      cidatadb(eosid="SRK", &
      cid="CO2", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=4.15820000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx15 = &
      gendatadb(ident = "CO", &
      formula = "CO", &
      name = "CARBON MONOXIDE", &
      mw = 28.0100, &
      Tc = 132.8500, &
      Pc = 3494000.00, &
      Zc = 0.292000, &
      acf = 0.045000, &
      Tb = 81.6600, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.54140000e+01, 6.71763100e+02, -5.15400000e+00/), &
      Tantmin = 69.7300, &
      Tantmax = 88.0800, &
      Zra = 0.289600 &
      )

  type (cpdata), parameter :: cp16 = &
      cpdata(cid = "CO", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 6, &
      cp = (/1.16120000e+00,-1.16150000e-03,3.50860000e-06,-3.86480000e-09,1.52870000e-12, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 50.0000, &
      Tcpmax = 1000.0000  &
      )

  type (alphadatadb), parameter :: twu27 = &
      alphadatadb(eosid="PR", &
      cid="CO", &
      ref="tcPR", &
      coeff=(/9.83000000e-02, 8.77700000e-01, 2.15680000e+00/) &
      )

  type (alphadatadb), parameter :: mc9 = &
      alphadatadb(eosid="PR", &
      cid="CO", &
      ref="Default", &
      coeff=(/7.05000000e-01, -3.18500000e-01, 1.90120000e+00/) &
      )

  type (cidatadb), parameter :: c27 = &
      cidatadb(eosid="PR", &
      cid="CO", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.67610000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu28 = &
      alphadatadb(eosid="SRK", &
      cid="CO", &
      ref="tcRK", &
      coeff=(/1.62500000e-01, 8.77800000e-01, 2.15620000e+00/) &
      )

  type (cidatadb), parameter :: c28 = &
      cidatadb(eosid="SRK", &
      cid="CO", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.40680000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx16 = &
      gendatadb(ident = "ClF3Si", &
      formula = "ClF3Si", &
      name = "CHLOROTRIFLUOROSILANE", &
      mw = 120.5000, &
      Tc = 307.7000, &
      Pc = 3470000.00, &
      Zc = 0.000000, &
      acf = 0.270730, &
      Tb = 203.2000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = -1.000000 &
      )

  type (cpdata), parameter :: cp17 = &
      cpdata(cid = "ClF3Si", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/9.59700000e+01,-6.08380000e+00,1.20890000e-02,-6.90760000e-06,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (gendatadb), parameter :: cx17 = &
      gendatadb(ident = "CYCLOHEX", &
      formula = "C6H12", &
      name = "CYCLOHEXANE", &
      mw = 84.1610, &
      Tc = 553.5000, &
      Pc = 4073000.00, &
      Zc = 0.273000, &
      acf = 0.211000, &
      Tb = 353.9300, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.273000 &
      )

  type (cpdata), parameter :: cp18 = &
      cpdata(cid = "CYCLOHEX", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 8, &
      cp = (/4.03500000e+00,-4.43300000e-03,1.68340000e-04,-2.07750000e-07,7.74600000e-11, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -100.0000, &
      Tcpmax = 1000.0000  &
      )

  type (alphadatadb), parameter :: twu29 = &
      alphadatadb(eosid="PR", &
      cid="CYCLOHEX", &
      ref="tcPR", &
      coeff=(/3.69100000e-01, 8.12000000e-01, 1.36170000e+00/) &
      )

  type (cidatadb), parameter :: c29 = &
      cidatadb(eosid="PR", &
      cid="CYCLOHEX", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-4.23270000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu30 = &
      alphadatadb(eosid="SRK", &
      cid="CYCLOHEX", &
      ref="tcRK", &
      coeff=(/3.16900000e-01, 8.33200000e-01, 1.86290000e+00/) &
      )

  type (cidatadb), parameter :: c30 = &
      cidatadb(eosid="SRK", &
      cid="CYCLOHEX", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.33377000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx18 = &
      gendatadb(ident = "C3_1", &
      formula = "C3H6", &
      name = "CYCLOPROPANE", &
      mw = 42.0810, &
      Tc = 397.8000, &
      Pc = 5490000.00, &
      Zc = 0.274000, &
      acf = 0.130000, &
      Tb = 240.3000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.271600 &
      )

  type (cpdata), parameter :: cp19 = &
      cpdata(cid = "C3_1", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/-3.52400000e+01,3.81300000e-01,-2.88100000e-04,9.03500000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu31 = &
      alphadatadb(eosid="PR", &
      cid="C3_1", &
      ref="tcPR", &
      coeff=(/2.16300000e-01, 8.56300000e-01, 1.73750000e+00/) &
      )

  type (cidatadb), parameter :: c31 = &
      cidatadb(eosid="PR", &
      cid="C3_1", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.05740000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu32 = &
      alphadatadb(eosid="SRK", &
      cid="C3_1", &
      ref="tcRK", &
      coeff=(/2.28700000e-01, 8.71400000e-01, 2.15940000e+00/) &
      )

  type (cidatadb), parameter :: c32 = &
      cidatadb(eosid="SRK", &
      cid="C3_1", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=6.36470000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx19 = &
      gendatadb(ident = "D2", &
      formula = "D2", &
      name = "DEUTERIUM", &
      mw = 4.0282, &
      Tc = 38.3400, &
      Pc = 1679600.00, &
      Zc = 0.304200, &
      acf = -0.136290, &
      Tb = 23.6610, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.315000 &
      )

  type (cpdata), parameter :: cp20 = &
      cpdata(cid = "D2", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/2.07860000e+01,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu33 = &
      alphadatadb(eosid="PR", &
      cid="D2", &
      ref="tcPR", &
      coeff=(/1.48600000e-01, 9.96800000e-01, 1.05870000e+00/) &
      )

  type (alphadatadb), parameter :: twu34 = &
      alphadatadb(eosid="PR", &
      cid="D2", &
      ref="QuantumCubic", &
      coeff=(/5.50070000e+01, -1.69810000e-02, 3.16210000e+00/) &
      )

  type (cidatadb), parameter :: c33 = &
      cidatadb(eosid="PR", &
      cid="D2", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-4.55550000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (cidatadb), parameter :: c34 = &
      cidatadb(eosid="PR", &
      cid="D2", &
      ref="QuantumCubic", &
      bib_ref="10.1016/j.fluid.2020.112790", &
      ciA=-3.87180000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu35 = &
      alphadatadb(eosid="SRK", &
      cid="D2", &
      ref="tcRK", &
      coeff=(/2.15000000e-01, 9.92100000e-01, 1.10790000e+00/) &
      )

  type (cidatadb), parameter :: c35 = &
      cidatadb(eosid="SRK", &
      cid="D2", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.25430000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx20 = &
      gendatadb(ident = "S434", &
      formula = "C12H26O", &
      name = "DI-n-HEXYL ETHER", &
      mw = 186.3390, &
      Tc = 657.0000, &
      Pc = 1823900.00, &
      Zc = 0.240000, &
      acf = 0.700000, &
      Tb = 499.6000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.63372000e+01, 3.98278000e+03, -8.91500000e+01/), &
      Tantmin = 373.0000, &
      Tantmax = 545.0000, &
      Zra = -1.000000 &
      )

  type (cpdata), parameter :: cp21 = &
      cpdata(cid = "S434", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/8.01000000e+00,2.56400000e-01,-1.32200000e-04,4.00700000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (gendatadb), parameter :: cx21 = &
      gendatadb(ident = "N2O4", &
      formula = "N2O4", &
      name = "DINITROGEN TETROXIDE", &
      mw = 92.0110, &
      Tc = 431.0100, &
      Pc = 10100000.00, &
      Zc = 0.470700, &
      acf = 1.007000, &
      Tb = 302.2200, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.70046000e+01, 2.73020000e+03, -3.89700000e+01/), &
      Tantmin = 254.1700, &
      Tantmax = 320.6900, &
      Zra = 0.366500 &
      )

  type (cpdata), parameter :: cp22 = &
      cpdata(cid = "N2O4", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 6, &
      cp = (/3.04890000e-01,2.46305000e-03,-1.73230000e-06,-5.56640000e-10,7.76230000e-13, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 50.0000, &
      Tcpmax = 1000.0000  &
      )

  type (gendatadb), parameter :: cx22 = &
      gendatadb(ident = "E-H2", &
      formula = "H2", &
      name = "EQUILIBRIUM-HYDROGEN", &
      mw = 2.0159, &
      Tc = 32.9380, &
      Pc = 1285800.00, &
      Zc = 0.302000, &
      acf = -0.219000, &
      Tb = 20.2710, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 10, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 15.0000, &
      Tantmax = 32.5000, &
      Zra = 0.306000 &
      )

  type (cpdata), parameter :: cp23 = &
      cpdata(cid = "E-H2", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 10, &
      cp = (/2.86719970e+01,1.33961560e+01,2.96013100e-03,-3.98074400e-06,2.66166700e-09, &
      -6.09986300e-13,-1.18013710e+01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 5.0000, &
      Tcpmax = 5.0000  &
      )

  type (gendatadb), parameter :: cx23 = &
      gendatadb(ident = "C2", &
      formula = "C2H6", &
      name = "ETHANE", &
      mw = 30.0700, &
      Tc = 305.4000, &
      Pc = 4883900.00, &
      Zc = 0.285000, &
      acf = 0.098000, &
      Tb = 184.5000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.56637000e+01, 1.51142000e+03, -1.71600000e+01/), &
      Tantmin = 130.0000, &
      Tantmax = 199.0000, &
      Zra = 0.280800 &
      )

  type (cpdata), parameter :: cp24 = &
      cpdata(cid = "C2", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/-4.93340000e-02,1.10899200e+00,-1.88512000e-04,3.96558000e-06,-3.14020900e-09, &
      8.00818700e-13,1.99588900e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu36 = &
      alphadatadb(eosid="PR", &
      cid="C2", &
      ref="tcPR", &
      coeff=(/1.45900000e-01, 8.78000000e-01, 2.15360000e+00/) &
      )

  type (alphadatadb), parameter :: mc10 = &
      alphadatadb(eosid="PR", &
      cid="C2", &
      ref="Default", &
      coeff=(/7.17800000e-01, -7.64400000e-01, 1.63960000e+00/) &
      )

  type (alphadatadb), parameter :: mc11 = &
      alphadatadb(eosid="PR", &
      cid="C2", &
      ref="Chapoy2005", &
      coeff=(/5.31000000e-01, -6.20000000e-02, 2.14000000e-01/) &
      )

  type (cidatadb), parameter :: c36 = &
      cidatadb(eosid="PR", &
      cid="C2", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.70740000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu37 = &
      alphadatadb(eosid="SRK", &
      cid="C2", &
      ref="tcRK", &
      coeff=(/2.24000000e-01, 8.81600000e-01, 2.10900000e+00/) &
      )

  type (alphadatadb), parameter :: mc12 = &
      alphadatadb(eosid="SRK", &
      cid="C2", &
      ref="Default", &
      coeff=(/7.17800000e-01, -7.64400000e-01, 1.63960000e+00/) &
      )

  type (alphadatadb), parameter :: mc13 = &
      alphadatadb(eosid="SRK", &
      cid="C2", &
      ref="Chapoy2005", &
      coeff=(/7.11000000e-01, -5.73000000e-01, 8.94000000e-01/) &
      )

  type (cidatadb), parameter :: c37 = &
      cidatadb(eosid="SRK", &
      cid="C2", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=4.57390000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx24 = &
      gendatadb(ident = "ETOH", &
      formula = "ETOH", &
      name = "ETHANOL", &
      mw = 46.0684, &
      Tc = 514.7100, &
      Pc = 6268000.00, &
      Zc = 0.247000, &
      acf = 0.646000, &
      Tb = 351.5700, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.243000 &
      )

  type (cpdata), parameter :: cp25 = &
      cpdata(cid = "ETOH", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu38 = &
      alphadatadb(eosid="PR", &
      cid="ETOH", &
      ref="tcPR", &
      coeff=(/9.86100000e-01, 9.64200000e-01, 1.33380000e+00/) &
      )

  type (cidatadb), parameter :: c38 = &
      cidatadb(eosid="PR", &
      cid="ETOH", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=5.95100000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu39 = &
      alphadatadb(eosid="SRK", &
      cid="ETOH", &
      ref="tcRK", &
      coeff=(/9.43700000e-01, 9.36300000e-01, 1.55900000e+00/) &
      )

  type (cidatadb), parameter :: c39 = &
      cidatadb(eosid="SRK", &
      cid="ETOH", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.60736000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx25 = &
      gendatadb(ident = "EBZN", &
      formula = "C8H10", &
      name = "ETHYLBENZENE", &
      mw = 106.1670, &
      Tc = 617.1600, &
      Pc = 3608000.00, &
      Zc = 0.263000, &
      acf = 0.302000, &
      Tb = 409.3600, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.262000 &
      )

  type (cpdata), parameter :: cp26 = &
      cpdata(cid = "EBZN", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/7.84400000e+04,3.39900000e+05,1.55900000e+03,2.42600000e+05,-7.02000000e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 200.0000, &
      Tcpmax = 1500.0000  &
      )

  type (alphadatadb), parameter :: twu40 = &
      alphadatadb(eosid="PR", &
      cid="EBZN", &
      ref="tcPR", &
      coeff=(/4.99100000e-01, 8.22900000e-01, 1.35530000e+00/) &
      )

  type (cidatadb), parameter :: c40 = &
      cidatadb(eosid="PR", &
      cid="EBZN", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.70300000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu41 = &
      alphadatadb(eosid="SRK", &
      cid="EBZN", &
      ref="tcRK", &
      coeff=(/5.11300000e-01, 8.33400000e-01, 1.56830000e+00/) &
      )

  type (cidatadb), parameter :: c41 = &
      cidatadb(eosid="SRK", &
      cid="EBZN", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.46150000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx26 = &
      gendatadb(ident = "C2_1", &
      formula = "C2H4", &
      name = "ETYLENE", &
      mw = 28.0540, &
      Tc = 282.4000, &
      Pc = 5035900.00, &
      Zc = 0.276000, &
      acf = 0.085000, &
      Tb = 169.4000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.55368000e+01, 1.34701000e+03, -1.81500000e+01/), &
      Tantmin = 120.0000, &
      Tantmax = 182.0000, &
      Zra = 0.281500 &
      )

  type (cpdata), parameter :: cp27 = &
      cpdata(cid = "C2_1", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/6.00935360e+01,6.06930000e-01,1.28878800e-03,1.03363600e-06,-1.09953700e-09, &
      2.92932600e-13,4.48985300e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: mc14 = &
      alphadatadb(eosid="PR", &
      cid="C2_1", &
      ref="Chapoy2005", &
      coeff=(/5.12000000e-01, -8.70000000e-02, 3.49000000e-01/) &
      )

  type (alphadatadb), parameter :: mc15 = &
      alphadatadb(eosid="SRK", &
      cid="C2_1", &
      ref="Chapoy2005", &
      coeff=(/6.52000000e-01, -3.15000000e-01, 5.63000000e-01/) &
      )

  type (gendatadb), parameter :: cx27 = &
      gendatadb(ident = "HE", &
      formula = "HE", &
      name = "HELIUM-4", &
      mw = 4.0030, &
      Tc = 5.1953, &
      Pc = 227600.00, &
      Zc = 0.301000, &
      acf = -0.385000, &
      Tb = 4.2100, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.22514000e+01, 3.37329000e+01, 1.79000000e+00/), &
      Tantmin = 3.7000, &
      Tantmax = 4.3000, &
      Zra = 0.335500 &
      )

  type (cpdata), parameter :: cp28 = &
      cpdata(cid = "HE", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/2.07860000e+01,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu42 = &
      alphadatadb(eosid="PR", &
      cid="HE", &
      ref="tcPR", &
      coeff=(/6.30000000e-03, 1.21750000e+00, 1.09090000e+00/) &
      )

  type (alphadatadb), parameter :: twu43 = &
      alphadatadb(eosid="PR", &
      cid="HE", &
      ref="QuantumCubic", &
      coeff=(/4.85580000e-01, 1.71730000e+00, 3.02710000e-01/) &
      )

  type (cidatadb), parameter :: c42 = &
      cidatadb(eosid="PR", &
      cid="HE", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-4.89150000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (cidatadb), parameter :: c43 = &
      cidatadb(eosid="PR", &
      cid="HE", &
      ref="QuantumCubic", &
      bib_ref="10.1016/j.fluid.2020.112790", &
      ciA=-3.17910000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu44 = &
      alphadatadb(eosid="SRK", &
      cid="HE", &
      ref="tcRK", &
      coeff=(/-4.66000000e-02, 1.24730000e+00, 5.40100000e-01/) &
      )

  type (cidatadb), parameter :: c44 = &
      cidatadb(eosid="SRK", &
      cid="HE", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.46080000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx28 = &
      gendatadb(ident = "H2", &
      formula = "H2", &
      name = "HYDROGEN", &
      mw = 2.0160, &
      Tc = 33.1450, &
      Pc = 1296400.00, &
      Zc = 0.305000, &
      acf = -0.220000, &
      Tb = 20.4000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.36333000e+01, 1.64900000e+02, 3.19000000e+00/), &
      Tantmin = 14.0000, &
      Tantmax = 25.0000, &
      Zra = 0.306000 &
      )

  type (cpdata), parameter :: cp29 = &
      cpdata(cid = "H2", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/2.86719970e+01,1.33961560e+01,2.96013100e-03,-3.98074400e-06,2.66166700e-09, &
      -6.09986300e-13,-1.18013710e+01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu45 = &
      alphadatadb(eosid="PR", &
      cid="H2", &
      ref="tcPR", &
      coeff=(/1.51470000e+00, -3.79590000e+00, -1.37700000e-01/) &
      )

  type (alphadatadb), parameter :: twu46 = &
      alphadatadb(eosid="PR", &
      cid="H2", &
      ref="QuantumCubic", &
      coeff=(/1.56210000e+02, -6.20720000e-03, 5.04700000e+00/) &
      )

  type (alphadatadb), parameter :: mc16 = &
      alphadatadb(eosid="PR", &
      cid="H2", &
      ref="Chapoy2005", &
      coeff=(/9.50000000e-02, -2.75000000e-01, -2.90000000e-02/) &
      )

  type (cidatadb), parameter :: c45 = &
      cidatadb(eosid="PR", &
      cid="H2", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-5.33860000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (cidatadb), parameter :: c46 = &
      cidatadb(eosid="PR", &
      cid="H2", &
      ref="QuantumCubic", &
      bib_ref="10.1016/j.fluid.2020.112790", &
      ciA=-3.81390000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu47 = &
      alphadatadb(eosid="SRK", &
      cid="H2", &
      ref="tcRK", &
      coeff=(/9.44400000e-01, 3.00870000e+00, 1.76200000e-01/) &
      )

  type (alphadatadb), parameter :: mc17 = &
      alphadatadb(eosid="SRK", &
      cid="H2", &
      ref="Chapoy2005", &
      coeff=(/1.61000000e-01, -2.25000000e-01, -2.32000000e-01/) &
      )

  type (cidatadb), parameter :: c47 = &
      cidatadb(eosid="SRK", &
      cid="H2", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.34490000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx29 = &
      gendatadb(ident = "H2O2", &
      formula = "H2O2", &
      name = "HYDROGENPEROXIDE", &
      mw = 34.0147, &
      Tc = 730.1500, &
      Pc = 21700000.00, &
      Zc = 0.274800, &
      acf = 0.358200, &
      Tb = 424.5500, &
      Ttr = 272.7403, &
      Ptr = 0.0000, &
      sref = 232.9500, &
      href = -136106.4000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.267000 &
      )

  type (cpdata), parameter :: cp30 = &
      cpdata(cid = "H2O2", &
      ref = "Default", &
      bib_ref = "https://webbook.nist.gov", &
      cptype = 12, &
      cp = (/3.42566700e+01,5.51844500e+01,-3.51544300e+01,9.08744000e+00,-4.22157000e-01, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 2000.0000  &
      )

  type (alphadatadb), parameter :: twu48 = &
      alphadatadb(eosid="PR", &
      cid="H2O2", &
      ref="tcPR", &
      coeff=(/3.19100000e-01, 8.64900000e-01, 2.28830000e+00/) &
      )

  type (cidatadb), parameter :: c48 = &
      cidatadb(eosid="PR", &
      cid="H2O2", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-8.74100000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu49 = &
      alphadatadb(eosid="SRK", &
      cid="H2O2", &
      ref="tcRK", &
      coeff=(/4.35100000e-01, 8.77500000e-01, 2.16040000e+00/) &
      )

  type (cidatadb), parameter :: c49 = &
      cidatadb(eosid="SRK", &
      cid="H2O2", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=3.35320000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx30 = &
      gendatadb(ident = "H2S", &
      formula = "H2S", &
      name = "HYDROGEN SULFIDE", &
      mw = 34.0800, &
      Tc = 373.2000, &
      Pc = 8936900.00, &
      Zc = 0.284000, &
      acf = 0.100000, &
      Tb = 212.8000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.61040000e+01, 1.76869000e+03, -2.60600000e+01/), &
      Tantmin = 190.0000, &
      Tantmax = 230.0000, &
      Zra = 0.285500 &
      )

  type (cpdata), parameter :: cp31 = &
      cpdata(cid = "H2S", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/-1.43704900e+00,9.98865000e-01,-1.84315000e-04,5.57087000e-07,-7.86320000e-11, &
      6.98500000e-15,1.80540900e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu50 = &
      alphadatadb(eosid="PR", &
      cid="H2S", &
      ref="tcPR", &
      coeff=(/1.12000000e-01, 8.68800000e-01, 2.27350000e+00/) &
      )

  type (alphadatadb), parameter :: mc18 = &
      alphadatadb(eosid="PR", &
      cid="H2S", &
      ref="Chapoy2005", &
      coeff=(/5.07000000e-01, 8.00000000e-03, 3.42000000e-01/) &
      )

  type (cidatadb), parameter :: c50 = &
      cidatadb(eosid="PR", &
      cid="H2S", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-2.50560000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu51 = &
      alphadatadb(eosid="SRK", &
      cid="H2S", &
      ref="tcRK", &
      coeff=(/1.74900000e-01, 8.68600000e-01, 2.27610000e+00/) &
      )

  type (alphadatadb), parameter :: mc19 = &
      alphadatadb(eosid="SRK", &
      cid="H2S", &
      ref="Chapoy2005", &
      coeff=(/6.41000000e-01, -1.83000000e-01, 5.13000000e-01/) &
      )

  type (cidatadb), parameter :: c51 = &
      cidatadb(eosid="SRK", &
      cid="H2S", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=3.01750000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx31 = &
      gendatadb(ident = "IC4", &
      formula = "C4H10", &
      name = "ISOBUTANE", &
      mw = 58.1240, &
      Tc = 408.1000, &
      Pc = 3647700.00, &
      Zc = 0.283000, &
      acf = 0.176000, &
      Tb = 261.3000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.55381000e+01, 2.03273000e+03, -3.31500000e+01/), &
      Tantmin = 187.0000, &
      Tantmax = 280.0000, &
      Zra = 0.275400 &
      )

  type (cpdata), parameter :: cp32 = &
      cpdata(cid = "IC4", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/2.67442080e+01,1.95448000e-01,2.52314300e-03,1.95651000e-07,-7.72615000e-10, &
      2.38608700e-13,3.46659500e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -75.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: mc20 = &
      alphadatadb(eosid="PR", &
      cid="IC4", &
      ref="Default", &
      coeff=(/8.28800000e-01, -8.28500000e-01, 2.32010000e+00/) &
      )

  type (alphadatadb), parameter :: mc21 = &
      alphadatadb(eosid="PR", &
      cid="IC4", &
      ref="Chapoy2005", &
      coeff=(/6.52000000e-01, -1.49000000e-01, 5.99000000e-01/) &
      )

  type (alphadatadb), parameter :: twu52 = &
      alphadatadb(eosid="PR", &
      cid="IC4", &
      ref="tcPR", &
      coeff=(/1.57500000e-01, 8.60100000e-01, 2.39510000e+00/) &
      )

  type (cidatadb), parameter :: c52 = &
      cidatadb(eosid="PR", &
      cid="IC4", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-4.07050000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: mc22 = &
      alphadatadb(eosid="SRK", &
      cid="IC4", &
      ref="Default", &
      coeff=(/8.28800000e-01, -8.28500000e-01, 2.32010000e+00/) &
      )

  type (alphadatadb), parameter :: mc23 = &
      alphadatadb(eosid="SRK", &
      cid="IC4", &
      ref="Chapoy2005", &
      coeff=(/8.07000000e-01, -4.32000000e-01, 9.10000000e-01/) &
      )

  type (alphadatadb), parameter :: twu53 = &
      alphadatadb(eosid="SRK", &
      cid="IC4", &
      ref="tcRK", &
      coeff=(/2.31300000e-01, 8.62500000e-01, 2.35980000e+00/) &
      )

  type (cidatadb), parameter :: c53 = &
      cidatadb(eosid="SRK", &
      cid="IC4", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.04875000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx32 = &
      gendatadb(ident = "IC5", &
      formula = "C5H12", &
      name = "ISOPENTANE", &
      mw = 72.1510, &
      Tc = 460.4000, &
      Pc = 3384300.00, &
      Zc = 0.271000, &
      acf = 0.227000, &
      Tb = 301.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.56338000e+01, 2.34867000e+03, -4.00500000e+01/), &
      Tantmin = 216.0000, &
      Tantmax = 322.0000, &
      Zra = 0.271700 &
      )

  type (cpdata), parameter :: cp33 = &
      cpdata(cid = "IC5", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/6.42520750e+01,-1.31900000e-01,3.54115600e-03,-1.33322500e-06,2.51463000e-10, &
      -1.29589000e-14,4.57297600e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: mc24 = &
      alphadatadb(eosid="PR", &
      cid="IC5", &
      ref="Default", &
      coeff=(/8.76700000e-01, -6.04300000e-01, 1.40250000e+00/) &
      )

  type (alphadatadb), parameter :: mc25 = &
      alphadatadb(eosid="PR", &
      cid="IC5", &
      ref="Chapoy2005", &
      coeff=(/7.24000000e-01, -1.66000000e-01, 5.15000000e-01/) &
      )

  type (alphadatadb), parameter :: twu54 = &
      alphadatadb(eosid="PR", &
      cid="IC5", &
      ref="tcPR", &
      coeff=(/2.08400000e-01, 8.41800000e-01, 2.13820000e+00/) &
      )

  type (cidatadb), parameter :: c54 = &
      cidatadb(eosid="PR", &
      cid="IC5", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.62110000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: mc26 = &
      alphadatadb(eosid="SRK", &
      cid="IC5", &
      ref="Default", &
      coeff=(/8.76700000e-01, -6.04300000e-01, 1.40250000e+00/) &
      )

  type (alphadatadb), parameter :: mc27 = &
      alphadatadb(eosid="SRK", &
      cid="IC5", &
      ref="Chapoy2005", &
      coeff=(/8.76000000e-01, -3.86000000e-01, 6.60000000e-01/) &
      )

  type (alphadatadb), parameter :: twu55 = &
      alphadatadb(eosid="SRK", &
      cid="IC5", &
      ref="tcRK", &
      coeff=(/2.37400000e-01, 8.54800000e-01, 2.47360000e+00/) &
      )

  type (cidatadb), parameter :: c55 = &
      cidatadb(eosid="SRK", &
      cid="IC5", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.38881000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx33 = &
      gendatadb(ident = "LJF", &
      formula = "LJF", &
      name = "LENNARD-JONES_FLUID", &
      mw = 1.0000, &
      Tc = 1.3200, &
      Pc = 0.13, &
      Zc = 0.310000, &
      acf = 0.317700, &
      Tb = 0.8000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = -1.000000 &
      )

  type (cpdata), parameter :: cp34 = &
      cpdata(cid = "LJF", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 300.0000, &
      Tcpmax = 1500.0000  &
      )

  type (gendatadb), parameter :: cx34 = &
      gendatadb(ident = "MXYL", &
      formula = "C8H10", &
      name = "M-XYLENE", &
      mw = 106.1670, &
      Tc = 617.0500, &
      Pc = 3536000.00, &
      Zc = 0.259000, &
      acf = 0.326000, &
      Tb = 412.2700, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.258700 &
      )

  type (cpdata), parameter :: cp35 = &
      cpdata(cid = "MXYL", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/7.56800000e+04,3.39240000e+05,1.49600000e+03,2.24700000e+05,-6.75900000e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 200.0000, &
      Tcpmax = 1500.0000  &
      )

  type (alphadatadb), parameter :: twu56 = &
      alphadatadb(eosid="PR", &
      cid="MXYL", &
      ref="tcPR", &
      coeff=(/3.50600000e-01, 8.33200000e-01, 1.85940000e+00/) &
      )

  type (cidatadb), parameter :: c56 = &
      cidatadb(eosid="PR", &
      cid="MXYL", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=5.14480000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu57 = &
      alphadatadb(eosid="SRK", &
      cid="MXYL", &
      ref="tcRK", &
      coeff=(/3.49200000e-01, 8.48100000e-01, 2.26170000e+00/) &
      )

  type (cidatadb), parameter :: c57 = &
      cidatadb(eosid="SRK", &
      cid="MXYL", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.70949000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx35 = &
      gendatadb(ident = "C1", &
      formula = "CH4", &
      name = "METHANE", &
      mw = 16.0425, &
      Tc = 190.5550, &
      Pc = 4598837.00, &
      Zc = 0.283742, &
      acf = 0.011310, &
      Tb = 111.7000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.52243000e+01, 8.97840000e+02, -7.16000000e+00/), &
      Tantmin = 93.0000, &
      Tantmax = 120.0000, &
      Zra = 0.289200 &
      )

  type (cpdata), parameter :: cp36 = &
      cpdata(cid = "C1", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/-1.62285490e+01,2.39359400e+00,-2.21800700e-03,5.74022000e-06,-3.72790500e-09, &
      8.54968500e-13,-3.39779000e-01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu58 = &
      alphadatadb(eosid="PR", &
      cid="C1", &
      ref="tcPR", &
      coeff=(/1.47100000e-01, 9.07400000e-01, 1.82530000e+00/) &
      )

  type (alphadatadb), parameter :: mc28 = &
      alphadatadb(eosid="PR", &
      cid="C1", &
      ref="Default", &
      coeff=(/5.85700000e-01, -7.20600000e-01, 1.28990000e+00/) &
      )

  type (alphadatadb), parameter :: mc29 = &
      alphadatadb(eosid="PR", &
      cid="C1", &
      ref="Chapoy2005", &
      coeff=(/4.16000000e-01, -1.73000000e-01, 3.48000000e-01/) &
      )

  type (cidatadb), parameter :: c58 = &
      cidatadb(eosid="PR", &
      cid="C1", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.56060000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu59 = &
      alphadatadb(eosid="SRK", &
      cid="C1", &
      ref="tcRK", &
      coeff=(/2.17100000e-01, 9.08200000e-01, 1.81720000e+00/) &
      )

  type (alphadatadb), parameter :: mc30 = &
      alphadatadb(eosid="SRK", &
      cid="C1", &
      ref="Default", &
      coeff=(/5.85700000e-01, -7.20600000e-01, 1.28990000e+00/) &
      )

  type (alphadatadb), parameter :: mc31 = &
      alphadatadb(eosid="SRK", &
      cid="C1", &
      ref="Chapoy2005", &
      coeff=(/5.49000000e-01, -4.09000000e-01, 6.03000000e-01/) &
      )

  type (cidatadb), parameter :: c59 = &
      cidatadb(eosid="SRK", &
      cid="C1", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.05030000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx36 = &
      gendatadb(ident = "MEOH", &
      formula = "CH4O", &
      name = "METHANOL", &
      mw = 32.0420, &
      Tc = 512.6000, &
      Pc = 8095900.00, &
      Zc = 0.224000, &
      acf = 0.559000, &
      Tb = 337.8000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.85875000e+01, 3.62655000e+03, -3.42900000e+01/), &
      Tantmin = 257.0000, &
      Tantmax = 364.0000, &
      Zra = 0.233400 &
      )

  type (cpdata), parameter :: cp37 = &
      cpdata(cid = "MEOH", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/5.05200000e+00,1.69400000e-02,6.17900000e-06,-6.81100000e-09,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu60 = &
      alphadatadb(eosid="PR", &
      cid="MEOH", &
      ref="tcPR", &
      coeff=(/6.75500000e-01, 9.14100000e-01, 1.75860000e+00/) &
      )

  type (cidatadb), parameter :: c60 = &
      cidatadb(eosid="PR", &
      cid="MEOH", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=9.18650000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu61 = &
      alphadatadb(eosid="SRK", &
      cid="MEOH", &
      ref="tcRK", &
      coeff=(/7.08200000e-01, 9.02200000e-01, 1.87800000e+00/) &
      )

  type (cidatadb), parameter :: c61 = &
      cidatadb(eosid="SRK", &
      cid="MEOH", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.69543000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx37 = &
      gendatadb(ident = "MTC5", &
      formula = "C6H12", &
      name = "METHYLCYCLOPENTANE", &
      mw = 84.1620, &
      Tc = 532.7000, &
      Pc = 3789600.00, &
      Zc = 0.273000, &
      acf = 0.239000, &
      Tb = 345.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.58023000e+01, 2.73100000e+03, -4.71100000e+01/), &
      Tantmin = 250.0000, &
      Tantmax = 375.0000, &
      Zra = 0.271100 &
      )

  type (cpdata), parameter :: cp38 = &
      cpdata(cid = "MTC5", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/-1.19680000e+01,1.52400000e-01,-8.69900000e-05,1.91400000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu62 = &
      alphadatadb(eosid="PR", &
      cid="MTC5", &
      ref="tcPR", &
      coeff=(/3.83900000e-01, 8.08200000e-01, 1.37410000e+00/) &
      )

  type (cidatadb), parameter :: c62 = &
      cidatadb(eosid="PR", &
      cid="MTC5", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.43780000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu63 = &
      alphadatadb(eosid="SRK", &
      cid="MTC5", &
      ref="tcRK", &
      coeff=(/3.05400000e-01, 8.29300000e-01, 1.96110000e+00/) &
      )

  type (cidatadb), parameter :: c63 = &
      cidatadb(eosid="SRK", &
      cid="MTC5", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.46862000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx38 = &
      gendatadb(ident = "MEG", &
      formula = "C2H6O2", &
      name = "ETHYLENE GLYCOL", &
      mw = 62.0700, &
      Tc = 720.0000, &
      Pc = 8200000.00, &
      Zc = 0.261600, &
      acf = 0.534700, &
      Tb = 470.2500, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.89090800e+01, 4.97797500e+03, -6.47210000e+01/), &
      Tantmin = 200.0000, &
      Tantmax = 720.0000, &
      Zra = 0.242400 &
      )

  type (cpdata), parameter :: cp39 = &
      cpdata(cid = "MEG", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/5.44670000e-01,1.18540000e-03,0.00000000e+00,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -22.0000, &
      Tcpmax = 150.0000  &
      )

  type (alphadatadb), parameter :: twu64 = &
      alphadatadb(eosid="PR", &
      cid="MEG", &
      ref="tcPR", &
      coeff=(/1.57530000e+00, 1.00000000e+00, 6.61400000e-01/) &
      )

  type (cidatadb), parameter :: c64 = &
      cidatadb(eosid="PR", &
      cid="MEG", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=8.38700000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu65 = &
      alphadatadb(eosid="SRK", &
      cid="MEG", &
      ref="tcRK", &
      coeff=(/1.54540000e+00, 1.00000000e+00, 7.62500000e-01/) &
      )

  type (cidatadb), parameter :: c65 = &
      cidatadb(eosid="SRK", &
      cid="MEG", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.92954000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx39 = &
      gendatadb(ident = "NC19", &
      formula = "C19H40", &
      name = "N-NONADECANE", &
      mw = 268.5300, &
      Tc = 755.0000, &
      Pc = 1160000.00, &
      Zc = 0.242000, &
      acf = 0.845000, &
      Tb = 602.3400, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.233370 &
      )

  type (cpdata), parameter :: cp40 = &
      cpdata(cid = "NC19", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 8, &
      cp = (/2.64470000e+01,-9.99800000e-03,5.06970000e-04,-6.79120000e-07,2.74280000e-10, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu66 = &
      alphadatadb(eosid="PR", &
      cid="NC19", &
      ref="tcPR", &
      coeff=(/5.94600000e-01, 7.93400000e-01, 2.26540000e+00/) &
      )

  type (cidatadb), parameter :: c66 = &
      cidatadb(eosid="PR", &
      cid="NC19", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=6.49064000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu67 = &
      alphadatadb(eosid="SRK", &
      cid="NC19", &
      ref="tcRK", &
      coeff=(/6.08600000e-01, 8.02400000e-01, 2.52710000e+00/) &
      )

  type (cidatadb), parameter :: c67 = &
      cidatadb(eosid="SRK", &
      cid="NC19", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.37989500e-04, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx40 = &
      gendatadb(ident = "NE", &
      formula = "NE", &
      name = "NEON", &
      mw = 20.1830, &
      Tc = 44.4000, &
      Pc = 2661630.00, &
      Zc = 0.311000, &
      acf = -0.038450, &
      Tb = 27.1000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/2.56553000e+01, -2.95285000e+02, 4.97548000e+00/), &
      Tantmin = 30.0000, &
      Tantmax = 40.0000, &
      Zra = 0.308500 &
      )

  type (cpdata), parameter :: cp41 = &
      cpdata(cid = "NE", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/2.07860000e+01,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu68 = &
      alphadatadb(eosid="PR", &
      cid="NE", &
      ref="tcPR", &
      coeff=(/1.88700000e-01, 9.47000000e-01, 1.46980000e+00/) &
      )

  type (alphadatadb), parameter :: twu69 = &
      alphadatadb(eosid="PR", &
      cid="NE", &
      ref="QuantumCubic", &
      coeff=(/4.04530000e-01, 9.58610000e-01, 8.39600000e-01/) &
      )

  type (cidatadb), parameter :: c68 = &
      cidatadb(eosid="PR", &
      cid="NE", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-2.35730000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (cidatadb), parameter :: c69 = &
      cidatadb(eosid="PR", &
      cid="NE", &
      ref="QuantumCubic", &
      bib_ref="10.1016/j.fluid.2020.112790", &
      ciA=-2.46650000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu70 = &
      alphadatadb(eosid="SRK", &
      cid="NE", &
      ref="tcRK", &
      coeff=(/3.27500000e-01, 9.69900000e-01, 1.28930000e+00/) &
      )

  type (cidatadb), parameter :: c70 = &
      cidatadb(eosid="SRK", &
      cid="NE", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-7.12000000e-08, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx41 = &
      gendatadb(ident = "NO", &
      formula = "NO", &
      name = "NITRIC OXIDE", &
      mw = 30.0061, &
      Tc = 180.0000, &
      Pc = 6480000.00, &
      Zc = 0.251127, &
      acf = 0.582000, &
      Tb = 121.3800, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/2.01315000e+01, 1.57250000e+03, -4.88000000e+00/), &
      Tantmin = 106.9000, &
      Tantmax = 127.5600, &
      Zra = 0.266800 &
      )

  type (cpdata), parameter :: cp42 = &
      cpdata(cid = "NO", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 6, &
      cp = (/8.56500000e-01,-1.44390000e-03,3.90260000e-06,-4.07270000e-09,1.52250000e-12, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 50.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu71 = &
      alphadatadb(eosid="PR", &
      cid="NO", &
      ref="tcPR", &
      coeff=(/8.81500000e-01, 9.55200000e-01, 1.40470000e+00/) &
      )

  type (cidatadb), parameter :: c71 = &
      cidatadb(eosid="PR", &
      cid="NO", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-7.54000000e-07, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu72 = &
      alphadatadb(eosid="SRK", &
      cid="NO", &
      ref="tcRK", &
      coeff=(/8.68100000e-01, 9.32000000e-01, 1.59540000e+00/) &
      )

  type (cidatadb), parameter :: c72 = &
      cidatadb(eosid="SRK", &
      cid="NO", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.63650000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx42 = &
      gendatadb(ident = "N2", &
      formula = "N2", &
      name = "NITROGEN", &
      mw = 28.0130, &
      Tc = 126.1610, &
      Pc = 3394400.00, &
      Zc = 0.290000, &
      acf = 0.040000, &
      Tb = 77.4000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.49542000e+01, 5.88720000e+02, -6.60000000e+00/), &
      Tantmin = 54.0000, &
      Tantmax = 90.0000, &
      Zra = 0.290000 &
      )

  type (cpdata), parameter :: cp43 = &
      cpdata(cid = "N2", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/-2.17250700e+00,1.06849000e+00,-1.34096000e-04,2.15569000e-07,-7.86320000e-11, &
      6.98500000e-15,1.80540900e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu73 = &
      alphadatadb(eosid="PR", &
      cid="N2", &
      ref="tcPR", &
      coeff=(/1.24000000e-01, 8.89700000e-01, 2.01380000e+00/) &
      )

  type (alphadatadb), parameter :: mc32 = &
      alphadatadb(eosid="PR", &
      cid="N2", &
      ref="Default", &
      coeff=(/4.04606000e-01, 3.91057000e-01, -9.63495000e-01/) &
      )

  type (alphadatadb), parameter :: mc33 = &
      alphadatadb(eosid="PR", &
      cid="N2", &
      ref="Chapoy2005", &
      coeff=(/4.48000000e-01, -1.57000000e-01, 4.69000000e-01/) &
      )

  type (cidatadb), parameter :: c73 = &
      cidatadb(eosid="PR", &
      cid="N2", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.64220000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu74 = &
      alphadatadb(eosid="SRK", &
      cid="N2", &
      ref="tcRK", &
      coeff=(/1.90200000e-01, 8.90000000e-01, 2.01060000e+00/) &
      )

  type (alphadatadb), parameter :: mc34 = &
      alphadatadb(eosid="SRK", &
      cid="N2", &
      ref="Default", &
      coeff=(/5.86700000e-01, -4.45900000e-01, 8.92600000e-01/) &
      )

  type (alphadatadb), parameter :: mc35 = &
      alphadatadb(eosid="SRK", &
      cid="N2", &
      ref="Chapoy2005", &
      coeff=(/5.84000000e-01, -3.96000000e-01, 7.36000000e-01/) &
      )

  type (cidatadb), parameter :: c74 = &
      cidatadb(eosid="SRK", &
      cid="N2", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.34700000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx43 = &
      gendatadb(ident = "N2O", &
      formula = "N2O", &
      name = "NITROUS OXIDE", &
      mw = 44.0130, &
      Tc = 309.6000, &
      Pc = 7240000.00, &
      Zc = 0.274000, &
      acf = 0.165000, &
      Tb = 184.7000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.275800 &
      )

  type (cpdata), parameter :: cp44 = &
      cpdata(cid = "N2O", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/2.16200000e+01,7.28100000e-02,-5.77800000e-05,1.83000000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu75 = &
      alphadatadb(eosid="PR", &
      cid="N2O", &
      ref="tcPR", &
      coeff=(/6.24800000e-01, 7.93300000e-01, 7.97600000e-01/) &
      )

  type (cidatadb), parameter :: c75 = &
      cidatadb(eosid="PR", &
      cid="N2O", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.23720000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu76 = &
      alphadatadb(eosid="SRK", &
      cid="N2O", &
      ref="tcRK", &
      coeff=(/3.08500000e-01, 8.13400000e-01, 1.56750000e+00/) &
      )

  type (cidatadb), parameter :: c76 = &
      cidatadb(eosid="SRK", &
      cid="N2O", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=4.39740000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx44 = &
      gendatadb(ident = "N-H2", &
      formula = "H2", &
      name = "N-HYDROGEN", &
      mw = 2.0159, &
      Tc = 33.1450, &
      Pc = 1296400.00, &
      Zc = 0.303000, &
      acf = -0.219000, &
      Tb = 20.3690, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 10, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 15.0989, &
      Tantmax = 32.6952, &
      Zra = 0.306000 &
      )

  type (cpdata), parameter :: cp45 = &
      cpdata(cid = "N-H2", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 10, &
      cp = (/2.86719970e+01,1.33961560e+01,2.96013100e-03,-3.98074400e-06,2.66166700e-09, &
      -6.09986300e-13,-1.18013710e+01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 5.0000, &
      Tcpmax = 5.0000  &
      )

  type (gendatadb), parameter :: cx45 = &
      gendatadb(ident = "O-H2", &
      formula = "H2", &
      name = "ORTHO-HYDROGEN", &
      mw = 2.0159, &
      Tc = 33.2200, &
      Pc = 1310650.00, &
      Zc = 0.307000, &
      acf = -0.218000, &
      Tb = 20.3800, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 10, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 15.0989, &
      Tantmax = 32.6952, &
      Zra = 0.306000 &
      )

  type (cpdata), parameter :: cp46 = &
      cpdata(cid = "O-H2", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 10, &
      cp = (/2.86719970e+01,1.33961560e+01,2.96013100e-03,-3.98074400e-06,2.66166700e-09, &
      -6.09986300e-13,-1.18013710e+01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 5.0000, &
      Tcpmax = 5.0000  &
      )

  type (gendatadb), parameter :: cx46 = &
      gendatadb(ident = "OXYL", &
      formula = "C8H10", &
      name = "O-XYLENE", &
      mw = 106.1670, &
      Tc = 630.3300, &
      Pc = 3734000.00, &
      Zc = 0.263000, &
      acf = 0.310400, &
      Tb = 417.5800, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.261600 &
      )

  type (cpdata), parameter :: cp47 = &
      cpdata(cid = "OXYL", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/8.52100000e+04,3.29540000e+05,1.49440000e+03,2.11500000e+05,-6.75800000e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 200.0000, &
      Tcpmax = 1500.0000  &
      )

  type (alphadatadb), parameter :: twu77 = &
      alphadatadb(eosid="PR", &
      cid="OXYL", &
      ref="tcPR", &
      coeff=(/3.10800000e-01, 8.46300000e-01, 2.02890000e+00/) &
      )

  type (cidatadb), parameter :: c77 = &
      cidatadb(eosid="PR", &
      cid="OXYL", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.52880000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu78 = &
      alphadatadb(eosid="SRK", &
      cid="OXYL", &
      ref="tcRK", &
      coeff=(/3.20600000e-01, 8.58600000e-01, 2.41770000e+00/) &
      )

  type (cidatadb), parameter :: c78 = &
      cidatadb(eosid="SRK", &
      cid="OXYL", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.38251000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx47 = &
      gendatadb(ident = "O2", &
      formula = "O2", &
      name = "OXYGEN", &
      mw = 31.9990, &
      Tc = 154.6000, &
      Pc = 5045990.00, &
      Zc = 0.288000, &
      acf = 0.021000, &
      Tb = 90.2000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.54075000e+01, 7.34550000e+02, -6.45000000e+00/), &
      Tantmin = 63.0000, &
      Tantmax = 100.0000, &
      Zra = 0.290500 &
      )

  type (cpdata), parameter :: cp48 = &
      cpdata(cid = "O2", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/-2.28357400e+00,9.52440000e-01,-2.81140000e-04,6.55223000e-07,-4.52316000e-10, &
      1.08774400e-13,2.08031000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu79 = &
      alphadatadb(eosid="PR", &
      cid="O2", &
      ref="tcPR", &
      coeff=(/2.12900000e-01, 8.91300000e-01, 1.40050000e+00/) &
      )

  type (alphadatadb), parameter :: mc36 = &
      alphadatadb(eosid="PR", &
      cid="O2", &
      ref="Chapoy2005", &
      coeff=(/4.13000000e-01, -1.70000000e-02, 9.20000000e-02/) &
      )

  type (cidatadb), parameter :: c79 = &
      cidatadb(eosid="PR", &
      cid="O2", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-2.76670000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu80 = &
      alphadatadb(eosid="SRK", &
      cid="O2", &
      ref="tcRK", &
      coeff=(/2.11800000e-01, 9.02000000e-01, 1.87980000e+00/) &
      )

  type (alphadatadb), parameter :: mc37 = &
      alphadatadb(eosid="SRK", &
      cid="O2", &
      ref="Chapoy2005", &
      coeff=(/5.45000000e-01, -2.35000000e-01, 2.92000000e-01/) &
      )

  type (cidatadb), parameter :: c80 = &
      cidatadb(eosid="SRK", &
      cid="O2", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.34570000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx48 = &
      gendatadb(ident = "P-H2", &
      formula = "H2", &
      name = "PARA-HYDROGEN", &
      mw = 2.0159, &
      Tc = 32.9380, &
      Pc = 1285800.00, &
      Zc = 0.302000, &
      acf = -0.219000, &
      Tb = 20.2710, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 10, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 15.0000, &
      Tantmax = 32.5000, &
      Zra = 0.306000 &
      )

  type (cpdata), parameter :: cp49 = &
      cpdata(cid = "P-H2", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 10, &
      cp = (/2.86719970e+01,1.33961560e+01,2.96013100e-03,-3.98074400e-06,2.66166700e-09, &
      -6.09986300e-13,-1.18013710e+01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 5.0000, &
      Tcpmax = 5.0000  &
      )

  type (gendatadb), parameter :: cx49 = &
      gendatadb(ident = "PXYL", &
      formula = "C8H10", &
      name = "P-XYLENE", &
      mw = 106.1670, &
      Tc = 616.2300, &
      Pc = 3511000.00, &
      Zc = 0.260000, &
      acf = 0.321500, &
      Tb = 411.5100, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.258500 &
      )

  type (cpdata), parameter :: cp50 = &
      cpdata(cid = "PXYL", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/7.51200000e+04,3.39700000e+05,1.49280000e+03,2.24700000e+05,-6.75100000e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 200.0000, &
      Tcpmax = 1500.0000  &
      )

  type (alphadatadb), parameter :: twu81 = &
      alphadatadb(eosid="PR", &
      cid="PXYL", &
      ref="tcPR", &
      coeff=(/2.26200000e-01, 8.50100000e-01, 2.54710000e+00/) &
      )

  type (cidatadb), parameter :: c81 = &
      cidatadb(eosid="PR", &
      cid="PXYL", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=5.37320000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu82 = &
      alphadatadb(eosid="SRK", &
      cid="PXYL", &
      ref="tcRK", &
      coeff=(/2.97900000e-01, 8.51400000e-01, 2.52780000e+00/) &
      )

  type (cidatadb), parameter :: c82 = &
      cidatadb(eosid="SRK", &
      cid="PXYL", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.76019000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx50 = &
      gendatadb(ident = "C3", &
      formula = "C3H8", &
      name = "PROPANE", &
      mw = 44.0970, &
      Tc = 369.8000, &
      Pc = 4245500.00, &
      Zc = 0.281000, &
      acf = 0.152000, &
      Tb = 231.1000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.57260000e+01, 1.87246000e+03, -2.51600000e+01/), &
      Tantmin = 164.0000, &
      Tantmax = 249.0000, &
      Zra = 0.276600 &
      )

  type (cpdata), parameter :: cp51 = &
      cpdata(cid = "C3", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/5.19200000e+04,1.92450000e+05,1.62650000e+03,1.16800000e+05,7.23600000e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 200.0000, &
      Tcpmax = 1500.0000  &
      )

  type (alphadatadb), parameter :: twu83 = &
      alphadatadb(eosid="PR", &
      cid="C3", &
      ref="tcPR", &
      coeff=(/1.59600000e-01, 8.68100000e-01, 2.28200000e+00/) &
      )

  type (alphadatadb), parameter :: mc38 = &
      alphadatadb(eosid="PR", &
      cid="C3", &
      ref="Default", &
      coeff=(/7.86300000e-01, -7.45900000e-01, 1.84540000e+00/) &
      )

  type (alphadatadb), parameter :: mc39 = &
      alphadatadb(eosid="PR", &
      cid="C3", &
      ref="Chapoy2005", &
      coeff=(/6.00000000e-01, -6.00000000e-03, 1.74000000e-01/) &
      )

  type (cidatadb), parameter :: c83 = &
      cidatadb(eosid="PR", &
      cid="C3", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.89270000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu84 = &
      alphadatadb(eosid="SRK", &
      cid="C3", &
      ref="tcRK", &
      coeff=(/2.45300000e-01, 8.73700000e-01, 2.20880000e+00/) &
      )

  type (alphadatadb), parameter :: mc40 = &
      alphadatadb(eosid="SRK", &
      cid="C3", &
      ref="Default", &
      coeff=(/7.86300000e-01, -7.45900000e-01, 1.84540000e+00/) &
      )

  type (alphadatadb), parameter :: mc41 = &
      alphadatadb(eosid="SRK", &
      cid="C3", &
      ref="Chapoy2005", &
      coeff=(/7.75000000e-01, -4.76000000e-01, 8.15000000e-01/) &
      )

  type (cidatadb), parameter :: c84 = &
      cidatadb(eosid="SRK", &
      cid="C3", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=7.46600000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx51 = &
      gendatadb(ident = "PRLN", &
      formula = "C3H6", &
      name = "PROPYLENE", &
      mw = 42.0810, &
      Tc = 364.9000, &
      Pc = 4600000.00, &
      Zc = 0.274000, &
      acf = 0.144000, &
      Tb = 225.5000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.57027000e+01, 1.80753000e+03, -2.61500000e+01/), &
      Tantmin = 160.0000, &
      Tantmax = 240.0000, &
      Zra = 0.277900 &
      )

  type (cpdata), parameter :: cp52 = &
      cpdata(cid = "PRLN", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/6.63699910e+01,1.28994000e-01,2.64691000e-03,-6.71019000e-07,-5.52250000e-11, &
      4.94690000e-14,5.11755300e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu85 = &
      alphadatadb(eosid="PR", &
      cid="PRLN", &
      ref="tcPR", &
      coeff=(/4.64100000e-01, 8.41900000e-01, 1.04550000e+00/) &
      )

  type (cidatadb), parameter :: c85 = &
      cidatadb(eosid="PR", &
      cid="PRLN", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.56200000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu86 = &
      alphadatadb(eosid="SRK", &
      cid="PRLN", &
      ref="tcRK", &
      coeff=(/3.84900000e-01, 8.51300000e-01, 1.46570000e+00/) &
      )

  type (cidatadb), parameter :: c86 = &
      cidatadb(eosid="SRK", &
      cid="PRLN", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=6.89870000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx52 = &
      gendatadb(ident = "R11", &
      formula = "CCL3F", &
      name = "TRICHLOROFLUOROMETHANE", &
      mw = 137.3680, &
      Tc = 471.2000, &
      Pc = 4407600.00, &
      Zc = 0.279000, &
      acf = 0.188000, &
      Tb = 297.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.58516000e+01, 2.40161000e+03, -3.63000000e+01/), &
      Tantmin = 240.0000, &
      Tantmax = 300.0000, &
      Zra = 0.274500 &
      )

  type (cpdata), parameter :: cp53 = &
      cpdata(cid = "R11", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/9.78900000e+00,3.89300000e-02,-3.38300000e-05,9.90300000e-09,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu87 = &
      alphadatadb(eosid="PR", &
      cid="R11", &
      ref="tcPR", &
      coeff=(/3.33800000e-01, 8.31800000e-01, 1.44220000e+00/) &
      )

  type (cidatadb), parameter :: c87 = &
      cidatadb(eosid="PR", &
      cid="R11", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-4.75660000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu88 = &
      alphadatadb(eosid="SRK", &
      cid="R11", &
      ref="tcRK", &
      coeff=(/3.24500000e-01, 8.48400000e-01, 1.82270000e+00/) &
      )

  type (cidatadb), parameter :: c88 = &
      cidatadb(eosid="SRK", &
      cid="R11", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=9.15060000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx53 = &
      gendatadb(ident = "R1114", &
      formula = "C2F4", &
      name = "TETRAFLUOROETHYLENE", &
      mw = 100.0160, &
      Tc = 306.5000, &
      Pc = 3940000.00, &
      Zc = 0.267000, &
      acf = 0.223000, &
      Tb = 197.2000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.270100 &
      )

  type (cpdata), parameter :: cp54 = &
      cpdata(cid = "R1114", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/2.90100000e+01,2.27700000e-01,-2.03600000e-04,6.77800000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu89 = &
      alphadatadb(eosid="PR", &
      cid="R1114", &
      ref="tcPR", &
      coeff=(/2.84800000e-01, 8.17800000e-01, 1.67930000e+00/) &
      )

  type (cidatadb), parameter :: c89 = &
      cidatadb(eosid="PR", &
      cid="R1114", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.39410000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu90 = &
      alphadatadb(eosid="SRK", &
      cid="R1114", &
      ref="tcRK", &
      coeff=(/2.82900000e-01, 8.36700000e-01, 2.09660000e+00/) &
      )

  type (cidatadb), parameter :: c90 = &
      cidatadb(eosid="SRK", &
      cid="R1114", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=8.62210000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx54 = &
      gendatadb(ident = "R1132a", &
      formula = "C2H2F2", &
      name = "1,1-DIFLUOROETHYLENE", &
      mw = 64.0350, &
      Tc = 302.9000, &
      Pc = 4460000.00, &
      Zc = 0.273000, &
      acf = 0.140000, &
      Tb = 187.5000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.271300 &
      )

  type (cpdata), parameter :: cp55 = &
      cpdata(cid = "R1132a", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/3.07300000e+00,2.44500000e-01,-2.09900000e-04,7.02100000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu91 = &
      alphadatadb(eosid="PR", &
      cid="R1132a", &
      ref="tcPR", &
      coeff=(/6.79000000e-02, 8.44700000e-01, 2.63710000e+00/) &
      )

  type (cidatadb), parameter :: c91 = &
      cidatadb(eosid="PR", &
      cid="R1132a", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-4.08900000e-07, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu92 = &
      alphadatadb(eosid="SRK", &
      cid="R1132a", &
      ref="tcRK", &
      coeff=(/1.35000000e-01, 8.46200000e-01, 2.61180000e+00/) &
      )

  type (cidatadb), parameter :: c92 = &
      cidatadb(eosid="SRK", &
      cid="R1132a", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=8.45540000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx55 = &
      gendatadb(ident = "R114", &
      formula = "C2CL2F4", &
      name = "1,2-DICHLOROTETRAFLUOROETHANE", &
      mw = 170.9220, &
      Tc = 418.9000, &
      Pc = 3262700.00, &
      Zc = 0.275000, &
      acf = 0.255000, &
      Tb = 277.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.273700 &
      )

  type (cpdata), parameter :: cp56 = &
      cpdata(cid = "R114", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/9.26200000e+00,8.21600000e-02,-7.04700000e-05,2.03200000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu93 = &
      alphadatadb(eosid="PR", &
      cid="R114", &
      ref="tcPR", &
      coeff=(/1.49200000e-01, 8.43900000e-01, 2.65050000e+00/) &
      )

  type (cidatadb), parameter :: c93 = &
      cidatadb(eosid="PR", &
      cid="R114", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-5.03720000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu94 = &
      alphadatadb(eosid="SRK", &
      cid="R114", &
      ref="tcRK", &
      coeff=(/2.21100000e-01, 8.45900000e-01, 2.61660000e+00/) &
      )

  type (cidatadb), parameter :: c94 = &
      cidatadb(eosid="SRK", &
      cid="R114", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.14150000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx56 = &
      gendatadb(ident = "R115", &
      formula = "C2CLF5", &
      name = "CHLOROPENTAFLUOROETHANE", &
      mw = 154.4670, &
      Tc = 353.2000, &
      Pc = 3161300.00, &
      Zc = 0.271000, &
      acf = 0.253000, &
      Tb = 234.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.57343000e+01, 1.84890000e+03, -3.08800000e+01/), &
      Tantmin = 175.0000, &
      Tantmax = 230.0000, &
      Zra = 0.275700 &
      )

  type (cpdata), parameter :: cp57 = &
      cpdata(cid = "R115", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/6.64800000e+00,8.34000000e-02,-6.90400000e-05,1.94400000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu95 = &
      alphadatadb(eosid="PR", &
      cid="R115", &
      ref="tcPR", &
      coeff=(/7.21200000e-01, 8.70300000e-01, 9.54000000e-01/) &
      )

  type (cidatadb), parameter :: c95 = &
      cidatadb(eosid="PR", &
      cid="R115", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-6.21940000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu96 = &
      alphadatadb(eosid="SRK", &
      cid="R115", &
      ref="tcRK", &
      coeff=(/3.45600000e-01, 8.39400000e-01, 1.94490000e+00/) &
      )

  type (cidatadb), parameter :: c96 = &
      cidatadb(eosid="SRK", &
      cid="R115", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=8.07540000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx57 = &
      gendatadb(ident = "R116", &
      formula = "C2F6", &
      name = "HEXAFLUOROETHANE", &
      mw = 138.0120, &
      Tc = 293.0000, &
      Pc = 3060000.00, &
      Zc = 0.279000, &
      acf = 0.253680, &
      Tb = 194.9000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.277800 &
      )

  type (cpdata), parameter :: cp58 = &
      cpdata(cid = "R116", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/2.68200000e+01,3.45800000e-01,-2.86900000e-04,8.13500000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu97 = &
      alphadatadb(eosid="PR", &
      cid="R116", &
      ref="tcPR", &
      coeff=(/2.20000000e-01, 8.32600000e-01, 2.13210000e+00/) &
      )

  type (cidatadb), parameter :: c97 = &
      cidatadb(eosid="PR", &
      cid="R116", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-6.70800000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu98 = &
      alphadatadb(eosid="SRK", &
      cid="R116", &
      ref="tcRK", &
      coeff=(/2.27400000e-01, 8.46900000e-01, 2.59910000e+00/) &
      )

  type (cidatadb), parameter :: c98 = &
      cidatadb(eosid="SRK", &
      cid="R116", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=5.60370000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx58 = &
      gendatadb(ident = "R12", &
      formula = "CCL2F2", &
      name = "DICHLORODIFLUOROMETHANE", &
      mw = 120.9140, &
      Tc = 385.0000, &
      Pc = 4123900.00, &
      Zc = 0.280000, &
      acf = 0.176000, &
      Tb = 243.4000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.275700 &
      )

  type (cpdata), parameter :: cp59 = &
      cpdata(cid = "R12", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/7.54700000e+00,4.25700000e-02,-3.60300000e-05,1.03700000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu99 = &
      alphadatadb(eosid="PR", &
      cid="R12", &
      ref="tcPR", &
      coeff=(/1.55200000e-01, 8.60100000e-01, 2.39040000e+00/) &
      )

  type (cidatadb), parameter :: c99 = &
      cidatadb(eosid="PR", &
      cid="R12", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-4.23300000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu100 = &
      alphadatadb(eosid="SRK", &
      cid="R12", &
      ref="tcRK", &
      coeff=(/2.18200000e-01, 8.60000000e-01, 2.39650000e+00/) &
      )

  type (cidatadb), parameter :: c100 = &
      cidatadb(eosid="SRK", &
      cid="R12", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=7.93010000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx59 = &
      gendatadb(ident = "R1234yf", &
      formula = "CF3CF=CH2", &
      name = "2,3,3,3-TETRAFLUOROPROPENE", &
      mw = 114.0416, &
      Tc = 367.8500, &
      Pc = 3382200.00, &
      Zc = 0.265190, &
      acf = 0.276000, &
      Tb = 243.7000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.67996000e+01, 2.41112000e+03, -6.30281000e+00/), &
      Tantmin = 243.7000, &
      Tantmax = 366.0700, &
      Zra = 0.264500 &
      )

  type (cpdata), parameter :: cp60 = &
      cpdata(cid = "R1234yf", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 6, &
      cp = (/2.13968000e-01,2.10720000e-03,1.89670000e-06,-6.68177000e-09,4.25854000e-12, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 170.0000, &
      Tcpmax = 1000.0000  &
      )

  type (alphadatadb), parameter :: twu101 = &
      alphadatadb(eosid="PR", &
      cid="R1234yf", &
      ref="tcPR", &
      coeff=(/1.71200000e-01, 8.37400000e-01, 2.58130000e+00/) &
      )

  type (cidatadb), parameter :: c101 = &
      cidatadb(eosid="PR", &
      cid="R1234yf", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=4.91000000e-07, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu102 = &
      alphadatadb(eosid="SRK", &
      cid="R1234yf", &
      ref="tcRK", &
      coeff=(/2.42900000e-01, 8.45500000e-01, 2.62360000e+00/) &
      )

  type (cidatadb), parameter :: c102 = &
      cidatadb(eosid="SRK", &
      cid="R1234yf", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.42961000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx60 = &
      gendatadb(ident = "R1234ze", &
      formula = "CHF=CHCF3_(t", &
      name = "TRANS-1,3,3,3-TETRAFLUOROPROPENE", &
      mw = 114.0416, &
      Tc = 382.5130, &
      Pc = 3634900.00, &
      Zc = 0.266412, &
      acf = 0.313000, &
      Tb = 254.1770, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.69543000e+01, 2.50917000e+03, -1.08418000e+01/), &
      Tantmin = 253.8000, &
      Tantmax = 383.0200, &
      Zra = 0.266400 &
      )

  type (cpdata), parameter :: cp61 = &
      cpdata(cid = "R1234ze", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 6, &
      cp = (/-1.75392000e-01,7.96320000e-03,-2.60047000e-05,4.67071000e-08,-3.17226000e-11, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 170.0000, &
      Tcpmax = 500.0000  &
      )

  type (alphadatadb), parameter :: twu103 = &
      alphadatadb(eosid="PR", &
      cid="R1234ze", &
      ref="tcPR", &
      coeff=(/1.47200000e-01, 8.30400000e-01, 2.88900000e+00/) &
      )

  type (cidatadb), parameter :: c103 = &
      cidatadb(eosid="PR", &
      cid="R1234ze", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.47890000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu104 = &
      alphadatadb(eosid="SRK", &
      cid="R1234ze", &
      ref="tcRK", &
      coeff=(/2.23900000e-01, 8.33600000e-01, 2.83390000e+00/) &
      )

  type (cidatadb), parameter :: c104 = &
      cidatadb(eosid="SRK", &
      cid="R1234ze", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.16861000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx61 = &
      gendatadb(ident = "R124", &
      formula = "C2HCLF4", &
      name = "2-CHLORO-1,1,1,2-TETRAFLUOROETHANE", &
      mw = 136.4750, &
      Tc = 395.4000, &
      Pc = 3620000.00, &
      Zc = 0.266000, &
      acf = 0.288000, &
      Tb = 261.1000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.269700 &
      )

  type (cpdata), parameter :: cp62 = &
      cpdata(cid = "R124", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 5, &
      cp = (/4.56447000e-01,1.78778000e-03,3.17361700e-08,-2.23347000e-11,-3.56841800e+01, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu105 = &
      alphadatadb(eosid="PR", &
      cid="R124", &
      ref="tcPR", &
      coeff=(/2.00900000e-01, 8.50200000e-01, 2.54620000e+00/) &
      )

  type (cidatadb), parameter :: c105 = &
      cidatadb(eosid="PR", &
      cid="R124", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.07770000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu106 = &
      alphadatadb(eosid="SRK", &
      cid="R124", &
      ref="tcRK", &
      coeff=(/2.64900000e-01, 8.49600000e-01, 2.55500000e+00/) &
      )

  type (cidatadb), parameter :: c106 = &
      cidatadb(eosid="SRK", &
      cid="R124", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.06893000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx62 = &
      gendatadb(ident = "R124a", &
      formula = "C2HCLF4", &
      name = "1-CHLORO-1,1,2,2-TETRAFLUOROETHANE", &
      mw = 136.4750, &
      Tc = 399.9000, &
      Pc = 3720000.00, &
      Zc = 0.273000, &
      acf = 0.281000, &
      Tb = 263.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = -1.000000 &
      )

  type (cpdata), parameter :: cp63 = &
      cpdata(cid = "R124a", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/1.27000000e+01,3.79500000e-01,-3.46000000e-04,1.15400000e-07,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (gendatadb), parameter :: cx63 = &
      gendatadb(ident = "R125", &
      formula = "C2HF5", &
      name = "PENTAFLUOROETHANE", &
      mw = 120.0300, &
      Tc = 343.7000, &
      Pc = 3870000.00, &
      Zc = 0.298600, &
      acf = 0.269000, &
      Tb = 224.6500, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.267100 &
      )

  type (cpdata), parameter :: cp64 = &
      cpdata(cid = "R125", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/5.37000000e+00,3.84500000e-01,-3.42000000e-04,1.12000000e-07,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu107 = &
      alphadatadb(eosid="PR", &
      cid="R125", &
      ref="tcPR", &
      coeff=(/1.78000000e-01, 8.41400000e-01, 2.69360000e+00/) &
      )

  type (cidatadb), parameter :: c107 = &
      cidatadb(eosid="PR", &
      cid="R125", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.45020000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu108 = &
      alphadatadb(eosid="SRK", &
      cid="R125", &
      ref="tcRK", &
      coeff=(/2.64000000e-01, 8.46300000e-01, 2.60960000e+00/) &
      )

  type (cidatadb), parameter :: c108 = &
      cidatadb(eosid="SRK", &
      cid="R125", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.04040000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx64 = &
      gendatadb(ident = "R13", &
      formula = "CCLF3", &
      name = "CHLOROTRIFLUOROMETHANE", &
      mw = 104.4590, &
      Tc = 302.0000, &
      Pc = 3921300.00, &
      Zc = 0.282000, &
      acf = 0.180000, &
      Tb = 191.7000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.277100 &
      )

  type (cpdata), parameter :: cp65 = &
      cpdata(cid = "R13", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/5.44900000e+00,4.56500000e-02,-3.76500000e-05,1.06500000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu109 = &
      alphadatadb(eosid="PR", &
      cid="R13", &
      ref="tcPR", &
      coeff=(/1.40000000e-01, 8.58100000e-01, 2.42490000e+00/) &
      )

  type (cidatadb), parameter :: c109 = &
      cidatadb(eosid="PR", &
      cid="R13", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.88590000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu110 = &
      alphadatadb(eosid="SRK", &
      cid="R13", &
      ref="tcRK", &
      coeff=(/2.15200000e-01, 8.60900000e-01, 2.38380000e+00/) &
      )

  type (cidatadb), parameter :: c110 = &
      cidatadb(eosid="SRK", &
      cid="R13", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=6.25440000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx65 = &
      gendatadb(ident = "R134a", &
      formula = "C2H2F4", &
      name = "1,1,1,2-TETRAFLUOROETHANE", &
      mw = 102.0300, &
      Tc = 374.1790, &
      Pc = 4056000.00, &
      Zc = 0.259100, &
      acf = 0.326680, &
      Tb = 246.7000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.259600 &
      )

  type (cpdata), parameter :: cp66 = &
      cpdata(cid = "R134a", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 5, &
      cp = (/1.31419000e-01,3.00600000e-03,-2.23892000e-06,5.97826000e-10,4.30007700e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu111 = &
      alphadatadb(eosid="PR", &
      cid="R134a", &
      ref="tcPR", &
      coeff=(/2.29200000e-01, 8.50000000e-01, 2.54990000e+00/) &
      )

  type (cidatadb), parameter :: c111 = &
      cidatadb(eosid="PR", &
      cid="R134a", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.19800000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu112 = &
      alphadatadb(eosid="SRK", &
      cid="R134a", &
      ref="tcRK", &
      coeff=(/3.22600000e-01, 8.56200000e-01, 2.45240000e+00/) &
      )

  type (cidatadb), parameter :: c112 = &
      cidatadb(eosid="SRK", &
      cid="R134a", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.38434000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx66 = &
      gendatadb(ident = "R14", &
      formula = "CF4", &
      name = "CARBON TETRAFLUORIDE", &
      mw = 88.0050, &
      Tc = 227.6000, &
      Pc = 3738900.00, &
      Zc = 0.277000, &
      acf = 0.191000, &
      Tb = 145.2000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.60543000e+01, 1.24455000e+03, -1.30600000e+01/), &
      Tantmin = 93.0000, &
      Tantmax = 148.0000, &
      Zra = 0.281000 &
      )

  type (cpdata), parameter :: cp67 = &
      cpdata(cid = "R14", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/3.33900000e+00,4.83800000e-02,-3.88300000e-05,1.07800000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu113 = &
      alphadatadb(eosid="PR", &
      cid="R14", &
      ref="tcPR", &
      coeff=(/1.65300000e-01, 8.58400000e-01, 2.29530000e+00/) &
      )

  type (cidatadb), parameter :: c113 = &
      cidatadb(eosid="PR", &
      cid="R14", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-4.52580000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu114 = &
      alphadatadb(eosid="SRK", &
      cid="R14", &
      ref="tcRK", &
      coeff=(/2.40400000e-01, 8.65900000e-01, 2.31260000e+00/) &
      )

  type (cidatadb), parameter :: c114 = &
      cidatadb(eosid="SRK", &
      cid="R14", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=3.34870000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx67 = &
      gendatadb(ident = "R142b", &
      formula = "C2H3ClF2", &
      name = "1-CHLORO-1,1-DIFLUOROETHANE", &
      mw = 100.4960, &
      Tc = 409.6000, &
      Pc = 4218000.00, &
      Zc = 0.286100, &
      acf = 0.236000, &
      Tb = 262.9300, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.266800 &
      )

  type (cpdata), parameter :: cp68 = &
      cpdata(cid = "R142b", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/4.01700000e+00,6.58400000e-02,-4.75800000e-05,1.26700000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu115 = &
      alphadatadb(eosid="PR", &
      cid="R142b", &
      ref="tcPR", &
      coeff=(/1.78700000e-01, 8.55600000e-01, 2.46250000e+00/) &
      )

  type (cidatadb), parameter :: c115 = &
      cidatadb(eosid="PR", &
      cid="R142b", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-2.19000000e-08, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu116 = &
      alphadatadb(eosid="SRK", &
      cid="R142b", &
      ref="tcRK", &
      coeff=(/2.56100000e-01, 8.58500000e-01, 2.41880000e+00/) &
      )

  type (cidatadb), parameter :: c116 = &
      cidatadb(eosid="SRK", &
      cid="R142b", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.30216000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx68 = &
      gendatadb(ident = "R143a", &
      formula = "C2H3F3", &
      name = "1,1,1-TRIFLUOROETHANE", &
      mw = 84.0410, &
      Tc = 346.3000, &
      Pc = 3760000.00, &
      Zc = 0.253000, &
      acf = 0.251000, &
      Tb = 225.6000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.256700 &
      )

  type (cpdata), parameter :: cp69 = &
      cpdata(cid = "R143a", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/5.74400000e+00,3.14100000e-01,-2.59700000e-04,8.41500000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu117 = &
      alphadatadb(eosid="PR", &
      cid="R143a", &
      ref="tcPR", &
      coeff=(/2.03700000e-01, 8.56000000e-01, 2.45580000e+00/) &
      )

  type (cidatadb), parameter :: c117 = &
      cidatadb(eosid="PR", &
      cid="R143a", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=4.78330000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu118 = &
      alphadatadb(eosid="SRK", &
      cid="R143a", &
      ref="tcRK", &
      coeff=(/2.95700000e-01, 8.62300000e-01, 2.36330000e+00/) &
      )

  type (cidatadb), parameter :: c118 = &
      cidatadb(eosid="SRK", &
      cid="R143a", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.65099000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx69 = &
      gendatadb(ident = "R152a", &
      formula = "C2H4F2", &
      name = "1,1-DIFLUOROETHANE", &
      mw = 66.0510, &
      Tc = 386.6000, &
      Pc = 4498800.00, &
      Zc = 0.253000, &
      acf = 0.266000, &
      Tb = 248.4000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.61871000e+01, 2.09535000e+03, -2.91600000e+01/), &
      Tantmin = 238.0000, &
      Tantmax = 273.0000, &
      Zra = 0.253800 &
      )

  type (cpdata), parameter :: cp70 = &
      cpdata(cid = "R152a", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/2.07200000e+00,5.72200000e-02,-3.48000000e-05,8.10700000e-09,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu119 = &
      alphadatadb(eosid="PR", &
      cid="R152a", &
      ref="tcPR", &
      coeff=(/2.77700000e-01, 8.73100000e-01, 2.21630000e+00/) &
      )

  type (cidatadb), parameter :: c119 = &
      cidatadb(eosid="PR", &
      cid="R152a", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=5.59180000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu120 = &
      alphadatadb(eosid="SRK", &
      cid="R152a", &
      ref="tcRK", &
      coeff=(/3.87500000e-01, 8.83100000e-01, 2.09120000e+00/) &
      )

  type (cidatadb), parameter :: c120 = &
      cidatadb(eosid="SRK", &
      cid="R152a", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.64932000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx70 = &
      gendatadb(ident = "R21", &
      formula = "CHCL2F", &
      name = "DICHLOROFLUOROMETHANE", &
      mw = 102.9230, &
      Tc = 451.6000, &
      Pc = 5167600.00, &
      Zc = 0.272000, &
      acf = 0.202000, &
      Tb = 282.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.270500 &
      )

  type (cpdata), parameter :: cp71 = &
      cpdata(cid = "R21", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/5.65200000e+00,3.77700000e-02,-2.86600000e-05,7.79500000e-09,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu121 = &
      alphadatadb(eosid="PR", &
      cid="R21", &
      ref="tcPR", &
      coeff=(/1.44100000e-01, 8.51500000e-01, 2.52500000e+00/) &
      )

  type (cidatadb), parameter :: c121 = &
      cidatadb(eosid="PR", &
      cid="R21", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.78500000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu122 = &
      alphadatadb(eosid="SRK", &
      cid="R21", &
      ref="tcRK", &
      coeff=(/2.04900000e-01, 8.50900000e-01, 2.53470000e+00/) &
      )

  type (cidatadb), parameter :: c122 = &
      cidatadb(eosid="SRK", &
      cid="R21", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=9.50530000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx71 = &
      gendatadb(ident = "R218", &
      formula = "C3F8", &
      name = "OCTAFLUOROPROPANE", &
      mw = 188.0170, &
      Tc = 345.1000, &
      Pc = 2680000.00, &
      Zc = 0.280000, &
      acf = 0.325000, &
      Tb = 236.5000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.277800 &
      )

  type (cpdata), parameter :: cp72 = &
      cpdata(cid = "R218", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/1.29400000e+01,6.22000000e-01,-6.40800000e-04,2.39800000e-07,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu123 = &
      alphadatadb(eosid="PR", &
      cid="R218", &
      ref="tcPR", &
      coeff=(/1.03840000e+00, 1.00000000e+00, 8.04600000e-01/) &
      )

  type (cidatadb), parameter :: c123 = &
      cidatadb(eosid="PR", &
      cid="R218", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.06166000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu124 = &
      alphadatadb(eosid="SRK", &
      cid="R218", &
      ref="tcRK", &
      coeff=(/8.84000000e-01, 9.71000000e-01, 1.11890000e+00/) &
      )

  type (cidatadb), parameter :: c124 = &
      cidatadb(eosid="SRK", &
      cid="R218", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=5.58080000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx72 = &
      gendatadb(ident = "R22", &
      formula = "CHCLF2", &
      name = "CHLORODIFLUOROMETHANE", &
      mw = 86.4690, &
      Tc = 369.2000, &
      Pc = 4975100.00, &
      Zc = 0.267000, &
      acf = 0.215000, &
      Tb = 232.4000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.55602000e+01, 1.70480000e+03, -4.13000000e+01/), &
      Tantmin = 225.0000, &
      Tantmax = 240.0000, &
      Zra = 0.266300 &
      )

  type (cpdata), parameter :: cp73 = &
      cpdata(cid = "R22", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 1, &
      cp = (/4.13200000e+00,3.86500000e-02,-2.79400000e-05,7.30500000e-09,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu125 = &
      alphadatadb(eosid="PR", &
      cid="R22", &
      ref="tcPR", &
      coeff=(/4.51300000e-01, 8.26700000e-01, 1.24430000e+00/) &
      )

  type (cidatadb), parameter :: c125 = &
      cidatadb(eosid="PR", &
      cid="R22", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-1.18800000e-07, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu126 = &
      alphadatadb(eosid="SRK", &
      cid="R22", &
      ref="tcRK", &
      coeff=(/4.03800000e-01, 8.40800000e-01, 1.63470000e+00/) &
      )

  type (cidatadb), parameter :: c126 = &
      cidatadb(eosid="SRK", &
      cid="R22", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=9.50070000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx73 = &
      gendatadb(ident = "R23", &
      formula = "CHF3", &
      name = "TRIFLUOROMETHANE", &
      mw = 70.0130, &
      Tc = 299.3000, &
      Pc = 4860000.00, &
      Zc = 0.259000, &
      acf = 0.260000, &
      Tb = 191.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.257600 &
      )

  type (cpdata), parameter :: cp74 = &
      cpdata(cid = "R23", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/8.15600000e+00,1.81300000e-01,-1.37900000e-04,3.93800000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu127 = &
      alphadatadb(eosid="PR", &
      cid="R23", &
      ref="tcPR", &
      coeff=(/3.86400000e-01, 8.42000000e-01, 1.57440000e+00/) &
      )

  type (cidatadb), parameter :: c127 = &
      cidatadb(eosid="PR", &
      cid="R23", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.94380000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu128 = &
      alphadatadb(eosid="SRK", &
      cid="R23", &
      ref="tcRK", &
      coeff=(/3.60900000e-01, 8.53700000e-01, 2.00220000e+00/) &
      )

  type (cidatadb), parameter :: c128 = &
      cidatadb(eosid="SRK", &
      cid="R23", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.08742000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx74 = &
      gendatadb(ident = "R32", &
      formula = "CH2F2", &
      name = "DIFLUOROMETHANE", &
      mw = 52.0230, &
      Tc = 351.6000, &
      Pc = 5830000.00, &
      Zc = 0.241000, &
      acf = 0.271000, &
      Tb = 221.5000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.244400 &
      )

  type (cpdata), parameter :: cp75 = &
      cpdata(cid = "R32", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/1.17900000e+01,1.18100000e-01,-4.84300000e-05,2.12500000e-09,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu129 = &
      alphadatadb(eosid="PR", &
      cid="R32", &
      ref="tcPR", &
      coeff=(/2.48300000e-01, 8.64400000e-01, 2.33320000e+00/) &
      )

  type (cidatadb), parameter :: c129 = &
      cidatadb(eosid="PR", &
      cid="R32", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=7.19530000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu130 = &
      alphadatadb(eosid="SRK", &
      cid="R32", &
      ref="tcRK", &
      coeff=(/3.48300000e-01, 8.72400000e-01, 2.22590000e+00/) &
      )

  type (cidatadb), parameter :: c130 = &
      cidatadb(eosid="SRK", &
      cid="R32", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.49356000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx75 = &
      gendatadb(ident = "R41", &
      formula = "CH3F", &
      name = "METHYL FLUORIDE", &
      mw = 34.0330, &
      Tc = 315.0000, &
      Pc = 5600000.00, &
      Zc = 0.240000, &
      acf = 0.187000, &
      Tb = 194.7000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.248100 &
      )

  type (cpdata), parameter :: cp76 = &
      cpdata(cid = "R41", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/1.38200000e+01,8.61600000e-02,-2.07100000e-05,-1.98500000e-09,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu131 = &
      alphadatadb(eosid="PR", &
      cid="R41", &
      ref="tcPR", &
      coeff=(/2.56600000e-01, 8.73000000e-01, 1.96820000e+00/) &
      )

  type (cidatadb), parameter :: c131 = &
      cidatadb(eosid="PR", &
      cid="R41", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=6.15490000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu132 = &
      alphadatadb(eosid="SRK", &
      cid="R41", &
      ref="tcRK", &
      coeff=(/2.95400000e-01, 8.77000000e-01, 2.16570000e+00/) &
      )

  type (cidatadb), parameter :: c132 = &
      cidatadb(eosid="SRK", &
      cid="R41", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.31619000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx76 = &
      gendatadb(ident = "F6S", &
      formula = "F6S", &
      name = "SULFUR HEXAFLUORIDE", &
      mw = 146.0540, &
      Tc = 318.7000, &
      Pc = 3760000.00, &
      Zc = 0.282000, &
      acf = 0.286000, &
      Tb = 209.6000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.278800 &
      )

  type (cpdata), parameter :: cp77 = &
      cpdata(cid = "F6S", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/-6.59900000e-01,4.63900000e-01,-5.08900000e-04,1.95300000e-07,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu133 = &
      alphadatadb(eosid="PR", &
      cid="F6S", &
      ref="tcPR", &
      coeff=(/4.93500000e-01, 4.81600000e-01, 8.17500000e-01/) &
      )

  type (cidatadb), parameter :: c133 = &
      cidatadb(eosid="PR", &
      cid="F6S", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-5.49410000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu134 = &
      alphadatadb(eosid="SRK", &
      cid="F6S", &
      ref="tcRK", &
      coeff=(/1.05890000e+00, 8.35400000e-01, 7.30700000e-01/) &
      )

  type (cidatadb), parameter :: c134 = &
      cidatadb(eosid="SRK", &
      cid="F6S", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=5.39720000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx77 = &
      gendatadb(ident = "SO2", &
      formula = "SO2", &
      name = "SULFUR DIOXIDE", &
      mw = 64.0650, &
      Tc = 430.8000, &
      Pc = 7885000.00, &
      Zc = 0.269000, &
      acf = 0.251000, &
      Tb = 263.1300, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.67681000e+01, 2.30240000e+03, -3.59600000e+01/), &
      Tantmin = 199.7100, &
      Tantmax = 279.4700, &
      Zra = 0.266100 &
      )

  type (cpdata), parameter :: cp78 = &
      cpdata(cid = "SO2", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 6, &
      cp = (/5.73200000e-01,-2.89930000e-04,3.04210000e-06,-4.24520000e-09,1.80790000e-12, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 50.0000, &
      Tcpmax = 1000.0000  &
      )

  type (alphadatadb), parameter :: twu135 = &
      alphadatadb(eosid="PR", &
      cid="SO2", &
      ref="tcPR", &
      coeff=(/4.18400000e-01, 8.23800000e-01, 1.40680000e+00/) &
      )

  type (cidatadb), parameter :: c135 = &
      cidatadb(eosid="PR", &
      cid="SO2", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-4.93000000e-08, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu136 = &
      alphadatadb(eosid="SRK", &
      cid="SO2", &
      ref="tcRK", &
      coeff=(/4.01400000e-01, 8.35800000e-01, 1.73550000e+00/) &
      )

  type (cidatadb), parameter :: c136 = &
      cidatadb(eosid="SRK", &
      cid="SO2", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=6.99930000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx78 = &
      gendatadb(ident = "F4N2", &
      formula = "F4N2", &
      name = "TETRAFLUOROHYDRAZINE", &
      mw = 104.0160, &
      Tc = 309.3000, &
      Pc = 3750000.00, &
      Zc = 0.000000, &
      acf = 0.206000, &
      Tb = 199.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = -1.000000 &
      )

  type (cpdata), parameter :: cp79 = &
      cpdata(cid = "F4N2", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/3.55300000e+00,3.50900000e-01,-3.63700000e-04,1.33800000e-07,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (alphadatadb), parameter :: twu137 = &
      alphadatadb(eosid="PR", &
      cid="F4N2", &
      ref="tcPR", &
      coeff=(/4.61700000e-01, 9.45500000e-01, 1.48230000e+00/) &
      )

  type (cidatadb), parameter :: c137 = &
      cidatadb(eosid="PR", &
      cid="F4N2", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=0.00000000e+00, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu138 = &
      alphadatadb(eosid="SRK", &
      cid="F4N2", &
      ref="tcRK", &
      coeff=(/5.05000000e-01, 9.34500000e-01, 1.57380000e+00/) &
      )

  type (cidatadb), parameter :: c138 = &
      cidatadb(eosid="SRK", &
      cid="F4N2", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=0.00000000e+00, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx79 = &
      gendatadb(ident = "TOLU", &
      formula = "C7H8", &
      name = "TOLUENE", &
      mw = 92.1410, &
      Tc = 591.7900, &
      Pc = 4108600.00, &
      Zc = 0.264000, &
      acf = 0.264100, &
      Tb = 383.7800, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.264300 &
      )

  type (cpdata), parameter :: cp80 = &
      cpdata(cid = "TOLU", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/5.81400000e+04,2.86300000e+05,1.44060000e+03,1.89800000e+05,-6.50430000e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 200.0000, &
      Tcpmax = 1500.0000  &
      )

  type (alphadatadb), parameter :: mc42 = &
      alphadatadb(eosid="PR", &
      cid="TOLU", &
      ref="Chapoy2005", &
      coeff=(/7.62000000e-01, -4.20000000e-02, 2.71000000e-01/) &
      )

  type (alphadatadb), parameter :: twu139 = &
      alphadatadb(eosid="PR", &
      cid="TOLU", &
      ref="tcPR", &
      coeff=(/3.09400000e-01, 8.30500000e-01, 1.78080000e+00/) &
      )

  type (cidatadb), parameter :: c139 = &
      cidatadb(eosid="PR", &
      cid="TOLU", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.23690000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: mc43 = &
      alphadatadb(eosid="SRK", &
      cid="TOLU", &
      ref="Chapoy2005", &
      coeff=(/9.23000000e-01, -3.01000000e-01, 4.94000000e-01/) &
      )

  type (alphadatadb), parameter :: twu140 = &
      alphadatadb(eosid="SRK", &
      cid="TOLU", &
      ref="tcRK", &
      coeff=(/3.25200000e-01, 8.43800000e-01, 2.10000000e+00/) &
      )

  type (cidatadb), parameter :: c140 = &
      cidatadb(eosid="SRK", &
      cid="TOLU", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.96684000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx80 = &
      gendatadb(ident = "F3NO", &
      formula = "F3N0", &
      name = "TRIFLUOROAMINEOXIDE", &
      mw = 87.0010, &
      Tc = 303.0000, &
      Pc = 6430000.00, &
      Zc = 0.375000, &
      acf = 0.212000, &
      Tb = 186.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = -1.000000 &
      )

  type (cpdata), parameter :: cp81 = &
      cpdata(cid = "F3NO", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/1.51300000e+01,2.44600000e-01,-2.52800000e-04,9.37500000e-08,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (gendatadb), parameter :: cx81 = &
      gendatadb(ident = "H2O", &
      formula = "H2O", &
      name = "WATER", &
      mw = 18.0150, &
      Tc = 647.3000, &
      Pc = 22048300.00, &
      Zc = 0.229000, &
      acf = 0.344000, &
      Tb = 373.2000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.83036000e+01, 3.81644000e+03, -4.61300000e+01/), &
      Tantmin = 284.0000, &
      Tantmax = 441.0000, &
      Zra = 0.233800 &
      )

  type (cpdata), parameter :: cp82 = &
      cpdata(cid = "H2O", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/-5.72991500e+00,1.91500700e+00,-3.95741000e-04,8.76232000e-07,-4.95086000e-10, &
      1.03861300e-13,7.02815000e-01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -175.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu141 = &
      alphadatadb(eosid="PR", &
      cid="H2O", &
      ref="tcPR", &
      coeff=(/3.86500000e-01, 8.72000000e-01, 1.96930000e+00/) &
      )

  type (alphadatadb), parameter :: mc44 = &
      alphadatadb(eosid="PR", &
      cid="H2O", &
      ref="Chapoy2005", &
      coeff=(/9.19000000e-01, -3.32000000e-01, 3.17000000e-01/) &
      )

  type (cidatadb), parameter :: c141 = &
      cidatadb(eosid="PR", &
      cid="H2O", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=5.30410000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu142 = &
      alphadatadb(eosid="SRK", &
      cid="H2O", &
      ref="tcRK", &
      coeff=(/4.16300000e-01, 8.75600000e-01, 2.18420000e+00/) &
      )

  type (alphadatadb), parameter :: mc45 = &
      alphadatadb(eosid="SRK", &
      cid="H2O", &
      ref="Chapoy2005", &
      coeff=(/1.09500000e+00, -6.78000000e-01, 7.00000000e-01/) &
      )

  type (cidatadb), parameter :: c142 = &
      cidatadb(eosid="SRK", &
      cid="H2O", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=8.99950000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx82 = &
      gendatadb(ident = "XE", &
      formula = "XE", &
      name = "XENON", &
      mw = 131.3000, &
      Tc = 289.7000, &
      Pc = 5840000.00, &
      Zc = 0.287000, &
      acf = 0.008000, &
      Tb = 165.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 2, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.282900 &
      )

  type (cpdata), parameter :: cp83 = &
      cpdata(cid = "XE", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 4, &
      cp = (/2.07860000e+01,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 0.0000, &
      Tcpmax = 0.0000  &
      )

  type (gendatadb), parameter :: cx83 = &
      gendatadb(ident = "NC4", &
      formula = "C4H10", &
      name = "N-BUTANE", &
      mw = 58.1240, &
      Tc = 425.2000, &
      Pc = 3799700.00, &
      Zc = 0.274000, &
      acf = 0.193000, &
      Tb = 272.7000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.56782000e+01, 2.15490000e+03, -3.44200000e+01/), &
      Tantmin = 195.0000, &
      Tantmax = 290.0000, &
      Zra = 0.273000 &
      )

  type (cpdata), parameter :: cp84 = &
      cpdata(cid = "NC4", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/1.72831340e+01,4.12696000e-01,2.02860100e-03,7.02953000e-07,-1.02587100e-09, &
      2.88339400e-13,2.71486100e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -75.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: mc46 = &
      alphadatadb(eosid="PR", &
      cid="NC4", &
      ref="Default", &
      coeff=(/8.78700000e-01, -9.39900000e-01, 2.26660000e+00/) &
      )

  type (alphadatadb), parameter :: mc47 = &
      alphadatadb(eosid="PR", &
      cid="NC4", &
      ref="Chapoy2005", &
      coeff=(/6.77000000e-01, -8.10000000e-02, 2.99000000e-01/) &
      )

  type (alphadatadb), parameter :: twu143 = &
      alphadatadb(eosid="PR", &
      cid="NC4", &
      ref="tcPR", &
      coeff=(/1.86700000e-01, 8.64500000e-01, 2.33270000e+00/) &
      )

  type (cidatadb), parameter :: c143 = &
      cidatadb(eosid="PR", &
      cid="NC4", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=-3.58180000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: mc48 = &
      alphadatadb(eosid="SRK", &
      cid="NC4", &
      ref="Default", &
      coeff=(/8.78700000e-01, -9.39900000e-01, 2.26660000e+00/) &
      )

  type (alphadatadb), parameter :: mc49 = &
      alphadatadb(eosid="SRK", &
      cid="NC4", &
      ref="Chapoy2005", &
      coeff=(/8.23000000e-01, -2.67000000e-01, 4.02000000e-01/) &
      )

  type (alphadatadb), parameter :: twu144 = &
      alphadatadb(eosid="SRK", &
      cid="NC4", &
      ref="tcRK", &
      coeff=(/2.62100000e-01, 8.66900000e-01, 2.29960000e+00/) &
      )

  type (cidatadb), parameter :: c144 = &
      cidatadb(eosid="SRK", &
      cid="NC4", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.09178000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx84 = &
      gendatadb(ident = "NC10", &
      formula = "C10H22", &
      name = "N-DECANE", &
      mw = 142.2860, &
      Tc = 617.6000, &
      Pc = 2107600.00, &
      Zc = 0.247000, &
      acf = 0.490000, &
      Tb = 447.3000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.60114000e+01, 3.45680000e+03, -7.86700000e+01/), &
      Tantmin = 330.0000, &
      Tantmax = 476.0000, &
      Zra = 0.250700 &
      )

  type (cpdata), parameter :: cp85 = &
      cpdata(cid = "NC10", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/-6.96202000e+00,8.51375000e-01,-2.63041000e-04,5.52181600e-06,-5.63173300e-09, &
      1.88854430e-12,-4.12446000e-01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -75.0000, &
      Tcpmax = 700.0000  &
      )

  type (alphadatadb), parameter :: twu145 = &
      alphadatadb(eosid="PR", &
      cid="NC10", &
      ref="tcPR", &
      coeff=(/3.67700000e-01, 8.11900000e-01, 2.21880000e+00/) &
      )

  type (cidatadb), parameter :: c145 = &
      cidatadb(eosid="PR", &
      cid="NC10", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.28105000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu146 = &
      alphadatadb(eosid="SRK", &
      cid="NC10", &
      ref="tcRK", &
      coeff=(/3.55300000e-01, 8.31000000e-01, 2.72810000e+00/) &
      )

  type (cidatadb), parameter :: c146 = &
      cidatadb(eosid="SRK", &
      cid="NC10", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=4.85857000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx85 = &
      gendatadb(ident = "NC22", &
      formula = "C22H46", &
      name = "N-DOCOSANE", &
      mw = 310.6100, &
      Tc = 787.0000, &
      Pc = 1060000.00, &
      Zc = 0.240000, &
      acf = 0.972200, &
      Tb = 641.7500, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.229950 &
      )

  type (cpdata), parameter :: cp86 = &
      cpdata(cid = "NC22", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/3.92560000e+05,1.18200000e+06,1.72340000e+03,8.15780000e+05,7.85130000e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 300.0000, &
      Tcpmax = 1500.0000  &
      )

  type (alphadatadb), parameter :: twu147 = &
      alphadatadb(eosid="PR", &
      cid="NC22", &
      ref="tcPR", &
      coeff=(/4.78800000e-01, 7.99000000e-01, 2.84990000e+00/) &
      )

  type (cidatadb), parameter :: c147 = &
      cidatadb(eosid="PR", &
      cid="NC22", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=8.51555000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu148 = &
      alphadatadb(eosid="SRK", &
      cid="NC22", &
      ref="tcRK", &
      coeff=(/5.01600000e-01, 8.08200000e-01, 3.12430000e+00/) &
      )

  type (cidatadb), parameter :: c148 = &
      cidatadb(eosid="SRK", &
      cid="NC22", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.71456100e-04, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx86 = &
      gendatadb(ident = "NC20", &
      formula = "C20H42", &
      name = "N-EICOSANE", &
      mw = 282.5500, &
      Tc = 768.0000, &
      Pc = 1070000.00, &
      Zc = 0.243000, &
      acf = 0.865000, &
      Tb = 616.8400, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.232780 &
      )

  type (cpdata), parameter :: cp87 = &
      cpdata(cid = "NC20", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/3.24810000e+05,1.10900000e+06,1.63600000e+03,7.45000000e+05,7.26270000e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 200.0000, &
      Tcpmax = 1500.0000  &
      )

  type (alphadatadb), parameter :: twu149 = &
      alphadatadb(eosid="PR", &
      cid="NC20", &
      ref="tcPR", &
      coeff=(/4.77100000e-01, 8.16000000e-01, 2.92090000e+00/) &
      )

  type (cidatadb), parameter :: c149 = &
      cidatadb(eosid="PR", &
      cid="NC20", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=6.37019000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu150 = &
      alphadatadb(eosid="SRK", &
      cid="NC20", &
      ref="tcRK", &
      coeff=(/5.21900000e-01, 8.21000000e-01, 3.08880000e+00/) &
      )

  type (cidatadb), parameter :: c150 = &
      cidatadb(eosid="SRK", &
      cid="NC20", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.40431300e-04, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx87 = &
      gendatadb(ident = "NC21", &
      formula = "C21H44", &
      name = "N-HENEICOSANE", &
      mw = 296.5800, &
      Tc = 778.0000, &
      Pc = 1110000.00, &
      Zc = 0.242000, &
      acf = 0.942000, &
      Tb = 629.6500, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.231530 &
      )

  type (cpdata), parameter :: cp88 = &
      cpdata(cid = "NC21", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/3.82820000e+05,7.71070000e+05,8.01080000e+02,4.99080000e+05,2.36160000e+03, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 300.0000, &
      Tcpmax = 1500.0000  &
      )

  type (alphadatadb), parameter :: twu151 = &
      alphadatadb(eosid="PR", &
      cid="NC21", &
      ref="tcPR", &
      coeff=(/4.54600000e-01, 8.18600000e-01, 3.14140000e+00/) &
      )

  type (cidatadb), parameter :: c151 = &
      cidatadb(eosid="PR", &
      cid="NC21", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=7.24116000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu152 = &
      alphadatadb(eosid="SRK", &
      cid="NC21", &
      ref="tcRK", &
      coeff=(/5.30200000e-01, 8.18900000e-01, 3.13490000e+00/) &
      )

  type (cidatadb), parameter :: c152 = &
      cidatadb(eosid="SRK", &
      cid="NC21", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.53342200e-04, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx88 = &
      gendatadb(ident = "NC17", &
      formula = "C17H36", &
      name = "N-HEPTADECANE", &
      mw = 240.4700, &
      Tc = 736.0000, &
      Pc = 1340000.00, &
      Zc = 0.242000, &
      acf = 0.753000, &
      Tb = 574.5600, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.236420 &
      )

  type (cpdata), parameter :: cp89 = &
      cpdata(cid = "NC17", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 8, &
      cp = (/2.38130000e+01,-9.21000000e-03,4.53330000e-04,-6.06010000e-07,2.44550000e-10, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu153 = &
      alphadatadb(eosid="PR", &
      cid="NC17", &
      ref="tcPR", &
      coeff=(/5.25700000e-01, 7.96900000e-01, 2.30920000e+00/) &
      )

  type (cidatadb), parameter :: c153 = &
      cidatadb(eosid="PR", &
      cid="NC17", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=5.24579000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu154 = &
      alphadatadb(eosid="SRK", &
      cid="NC17", &
      ref="tcRK", &
      coeff=(/5.16100000e-01, 8.09800000e-01, 2.69190000e+00/) &
      )

  type (cidatadb), parameter :: c154 = &
      cidatadb(eosid="SRK", &
      cid="NC17", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.17184700e-04, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx89 = &
      gendatadb(ident = "NC7", &
      formula = "C7H16", &
      name = "N-HEPTANE", &
      mw = 100.2050, &
      Tc = 540.2000, &
      Pc = 2735800.00, &
      Zc = 0.263000, &
      acf = 0.351000, &
      Tb = 371.6000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.58737000e+01, 2.91132000e+03, -5.65100000e+01/), &
      Tantmin = 270.0000, &
      Tantmax = 400.0000, &
      Zra = 0.260400 &
      )

  type (cpdata), parameter :: cp90 = &
      cpdata(cid = "NC7", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/-1.53725000e-01,7.54499000e-01,2.61728000e-04,4.36635800e-06,-4.48451000e-09, &
      1.48420990e-12,3.80048000e-01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -75.0000, &
      Tcpmax = 700.0000  &
      )

  type (alphadatadb), parameter :: mc50 = &
      alphadatadb(eosid="PR", &
      cid="NC7", &
      ref="Default", &
      coeff=(/1.22780000e+00, -1.55580000e+00, 3.93610000e+00/) &
      )

  type (alphadatadb), parameter :: mc51 = &
      alphadatadb(eosid="PR", &
      cid="NC7", &
      ref="Chapoy2005", &
      coeff=(/8.78000000e-01, -3.10000000e-02, 3.02000000e-01/) &
      )

  type (alphadatadb), parameter :: twu155 = &
      alphadatadb(eosid="PR", &
      cid="NC7", &
      ref="tcPR", &
      coeff=(/3.29700000e-01, 8.22200000e-01, 1.96150000e+00/) &
      )

  type (cidatadb), parameter :: c155 = &
      cidatadb(eosid="PR", &
      cid="NC7", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=3.09080000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: mc52 = &
      alphadatadb(eosid="SRK", &
      cid="NC7", &
      ref="Default", &
      coeff=(/1.22780000e+00, -1.55580000e+00, 3.93610000e+00/) &
      )

  type (alphadatadb), parameter :: mc53 = &
      alphadatadb(eosid="SRK", &
      cid="NC7", &
      ref="Chapoy2005", &
      coeff=(/1.03600000e+00, -2.58000000e-01, 4.88000000e-01/) &
      )

  type (alphadatadb), parameter :: twu156 = &
      alphadatadb(eosid="SRK", &
      cid="NC7", &
      ref="tcRK", &
      coeff=(/3.26900000e-01, 8.38700000e-01, 2.39600000e+00/) &
      )

  type (cidatadb), parameter :: c156 = &
      cidatadb(eosid="SRK", &
      cid="NC7", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.78263000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx90 = &
      gendatadb(ident = "NC16", &
      formula = "C16H34", &
      name = "N-HEXADECANE", &
      mw = 226.4460, &
      Tc = 717.0000, &
      Pc = 1418600.00, &
      Zc = 0.230000, &
      acf = 0.742000, &
      Tb = 560.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.61841000e+01, 4.21491000e+03, -1.18700000e+02/), &
      Tantmin = 423.0000, &
      Tantmax = 594.0000, &
      Zra = 0.238800 &
      )

  type (cpdata), parameter :: cp91 = &
      cpdata(cid = "NC16", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/6.09270110e+01,-9.55630000e-02,3.45931300e-03,-1.35680700e-06,2.65935000e-10, &
      -1.46753000e-14,3.09512800e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (cpdata), parameter :: cp92 = &
      cpdata(cid = "NC16", &
      ref = "", &
      bib_ref = "", &
      cptype = 8, &
      cp = (/3.97470000e+01,-2.06152000e-01,1.14814000e-03,-1.55548000e-06,6.75340000e-10, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu157 = &
      alphadatadb(eosid="PR", &
      cid="NC16", &
      ref="tcPR", &
      coeff=(/5.37200000e-01, 7.92900000e-01, 2.14580000e+00/) &
      )

  type (cidatadb), parameter :: c157 = &
      cidatadb(eosid="PR", &
      cid="NC16", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=5.19037000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu158 = &
      alphadatadb(eosid="SRK", &
      cid="NC16", &
      ref="tcRK", &
      coeff=(/5.26200000e-01, 8.06800000e-01, 2.50800000e+00/) &
      )

  type (cidatadb), parameter :: c158 = &
      cidatadb(eosid="SRK", &
      cid="NC16", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.13300800e-04, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx91 = &
      gendatadb(ident = "NC6", &
      formula = "C6H14", &
      name = "N-HEXANE", &
      mw = 86.1780, &
      Tc = 507.4000, &
      Pc = 2968800.00, &
      Zc = 0.260000, &
      acf = 0.296000, &
      Tb = 341.9000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.58366000e+01, 2.69755000e+03, -4.87800000e+01/), &
      Tantmin = 245.0000, &
      Tantmax = 370.0000, &
      Zra = 0.263500 &
      )

  type (cpdata), parameter :: cp93 = &
      cpdata(cid = "NC6", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/-1.71910710e+01,9.59226000e-01,-6.14725000e-04,6.14210100e-06,-6.16095200e-09, &
      2.08681900e-12,-2.07040000e-01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -75.0000, &
      Tcpmax = 700.0000  &
      )

  type (alphadatadb), parameter :: mc54 = &
      alphadatadb(eosid="PR", &
      cid="NC6", &
      ref="Default", &
      coeff=(/1.04300000e+00, -1.15530000e+00, 2.92350000e+00/) &
      )

  type (alphadatadb), parameter :: mc55 = &
      alphadatadb(eosid="PR", &
      cid="NC6", &
      ref="Chapoy2005", &
      coeff=(/8.70000000e-01, -5.88000000e-01, 1.50400000e+00/) &
      )

  type (alphadatadb), parameter :: twu159 = &
      alphadatadb(eosid="PR", &
      cid="NC6", &
      ref="tcPR", &
      coeff=(/2.55700000e-01, 8.37700000e-01, 2.18710000e+00/) &
      )

  type (cidatadb), parameter :: c159 = &
      cidatadb(eosid="PR", &
      cid="NC6", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=7.92500000e-07, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: mc56 = &
      alphadatadb(eosid="SRK", &
      cid="NC6", &
      ref="Default", &
      coeff=(/1.04300000e+00, -1.15530000e+00, 2.92350000e+00/) &
      )

  type (alphadatadb), parameter :: mc57 = &
      alphadatadb(eosid="SRK", &
      cid="NC6", &
      ref="Chapoy2005", &
      coeff=(/1.00500000e+00, -5.91000000e-01, 1.20300000e+00/) &
      )

  type (alphadatadb), parameter :: twu160 = &
      alphadatadb(eosid="SRK", &
      cid="NC6", &
      ref="tcRK", &
      coeff=(/2.77300000e-01, 8.50300000e-01, 2.54390000e+00/) &
      )

  type (cidatadb), parameter :: c160 = &
      cidatadb(eosid="SRK", &
      cid="NC6", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.20445000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx92 = &
      gendatadb(ident = "NC9", &
      formula = "C9H20", &
      name = "N-NONANE", &
      mw = 128.2590, &
      Tc = 594.6000, &
      Pc = 2310200.00, &
      Zc = 0.260000, &
      acf = 0.444000, &
      Tb = 424.0000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.59671000e+01, 3.29145000e+03, -7.13300000e+01/), &
      Tantmin = 312.0000, &
      Tantmax = 452.0000, &
      Zra = 0.254300 &
      )

  type (cpdata), parameter :: cp94 = &
      cpdata(cid = "NC9", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/4.00027800e+00,7.07805000e-01,4.38048000e-04,3.96934200e-06,-4.04315800e-09, &
      1.28760280e-12,2.57265000e-01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -75.0000, &
      Tcpmax = 700.0000  &
      )

  type (alphadatadb), parameter :: twu161 = &
      alphadatadb(eosid="PR", &
      cid="NC9", &
      ref="tcPR", &
      coeff=(/4.05400000e-01, 8.09700000e-01, 1.93430000e+00/) &
      )

  type (cidatadb), parameter :: c161 = &
      cidatadb(eosid="PR", &
      cid="NC9", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=9.31890000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu162 = &
      alphadatadb(eosid="SRK", &
      cid="NC9", &
      ref="tcRK", &
      coeff=(/3.85800000e-01, 8.29400000e-01, 2.40410000e+00/) &
      )

  type (cidatadb), parameter :: c162 = &
      cidatadb(eosid="SRK", &
      cid="NC9", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=4.13357000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx93 = &
      gendatadb(ident = "NC18", &
      formula = "C18H38", &
      name = "N-OCTADECANE", &
      mw = 254.5000, &
      Tc = 747.0000, &
      Pc = 1290000.00, &
      Zc = 0.247000, &
      acf = 0.800000, &
      Tb = 588.3000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.234730 &
      )

  type (cpdata), parameter :: cp95 = &
      cpdata(cid = "NC18", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 8, &
      cp = (/2.51300000e+01,-9.60300000e-03,4.80150000e-04,-6.42560000e-07,2.59420000e-10, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu163 = &
      alphadatadb(eosid="PR", &
      cid="NC18", &
      ref="tcPR", &
      coeff=(/5.53300000e-01, 7.95500000e-01, 2.30670000e+00/) &
      )

  type (cidatadb), parameter :: c163 = &
      cidatadb(eosid="PR", &
      cid="NC18", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=5.90709000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu164 = &
      alphadatadb(eosid="SRK", &
      cid="NC18", &
      ref="tcRK", &
      coeff=(/5.50000000e-01, 8.07600000e-01, 2.65400000e+00/) &
      )

  type (cidatadb), parameter :: c164 = &
      cidatadb(eosid="SRK", &
      cid="NC18", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.27994400e-04, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx94 = &
      gendatadb(ident = "NC8", &
      formula = "C8H18", &
      name = "N-OCTANE", &
      mw = 114.2320, &
      Tc = 568.8000, &
      Pc = 2482500.00, &
      Zc = 0.259000, &
      acf = 0.394000, &
      Tb = 398.8000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.59426000e+01, 3.12029000e+03, -6.36300000e+01/), &
      Tantmin = 292.0000, &
      Tantmax = 425.0000, &
      Zra = 0.257100 &
      )

  type (cpdata), parameter :: cp96 = &
      cpdata(cid = "NC8", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/2.60472500e+00,7.24670000e-01,3.67845000e-04,4.14283300e-06,-4.24019900e-09, &
      1.37340550e-12,3.27588000e-01,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -75.0000, &
      Tcpmax = 700.0000  &
      )

  type (alphadatadb), parameter :: mc58 = &
      alphadatadb(eosid="PR", &
      cid="NC8", &
      ref="Default", &
      coeff=(/1.27980000e+00, -1.38220000e+00, 3.39330000e+00/) &
      )

  type (alphadatadb), parameter :: mc59 = &
      alphadatadb(eosid="PR", &
      cid="NC8", &
      ref="Chapoy2005", &
      coeff=(/9.58000000e-01, -1.34000000e-01, 4.87000000e-01/) &
      )

  type (alphadatadb), parameter :: twu165 = &
      alphadatadb(eosid="PR", &
      cid="NC8", &
      ref="tcPR", &
      coeff=(/3.38500000e-01, 8.18500000e-01, 2.07470000e+00/) &
      )

  type (cidatadb), parameter :: c165 = &
      cidatadb(eosid="PR", &
      cid="NC8", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=6.41340000e-06, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: mc60 = &
      alphadatadb(eosid="SRK", &
      cid="NC8", &
      ref="Default", &
      coeff=(/1.27980000e+00, -1.38220000e+00, 3.39330000e+00/) &
      )

  type (alphadatadb), parameter :: mc61 = &
      alphadatadb(eosid="SRK", &
      cid="NC8", &
      ref="Chapoy2005", &
      coeff=(/1.15000000e+00, -5.87000000e-01, 1.09600000e+00/) &
      )

  type (alphadatadb), parameter :: twu166 = &
      alphadatadb(eosid="SRK", &
      cid="NC8", &
      ref="tcRK", &
      coeff=(/3.44900000e-01, 8.34100000e-01, 2.46600000e+00/) &
      )

  type (cidatadb), parameter :: c166 = &
      cidatadb(eosid="SRK", &
      cid="NC8", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=3.48304000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx95 = &
      gendatadb(ident = "NC25", &
      formula = "C25H52", &
      name = "N-PENTACOSANE", &
      mw = 352.6900, &
      Tc = 812.0000, &
      Pc = 950000.00, &
      Zc = 0.240000, &
      acf = 1.105300, &
      Tb = 674.1500, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.228110 &
      )

  type (cpdata), parameter :: cp97 = &
      cpdata(cid = "NC25", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/4.45150000e+05,1.33890000e+06,1.72150000e+03,9.24780000e+05,7.84280000e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 300.0000, &
      Tcpmax = 1500.0000  &
      )

  type (alphadatadb), parameter :: twu167 = &
      alphadatadb(eosid="PR", &
      cid="NC25", &
      ref="tcPR", &
      coeff=(/5.71700000e-01, 7.80300000e-01, 2.46660000e+00/) &
      )

  type (cidatadb), parameter :: c167 = &
      cidatadb(eosid="PR", &
      cid="NC25", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.05664500e-04, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu168 = &
      alphadatadb(eosid="SRK", &
      cid="NC25", &
      ref="tcRK", &
      coeff=(/6.03400000e-01, 7.79000000e-01, 2.50350000e+00/) &
      )

  type (cidatadb), parameter :: c168 = &
      cidatadb(eosid="SRK", &
      cid="NC25", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.11001600e-04, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx96 = &
      gendatadb(ident = "NC15", &
      formula = "C15H32", &
      name = "N-PENTADECANE", &
      mw = 212.4200, &
      Tc = 708.0000, &
      Pc = 1480000.00, &
      Zc = 0.243000, &
      acf = 0.685000, &
      Tb = 543.8300, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.238360 &
      )

  type (cpdata), parameter :: cp98 = &
      cpdata(cid = "NC15", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 8, &
      cp = (/2.11800000e+01,-8.42400000e-03,3.99690000e-04,-5.30000029e+01,2.14820000e-10, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu169 = &
      alphadatadb(eosid="PR", &
      cid="NC15", &
      ref="tcPR", &
      coeff=(/4.77000000e-01, 7.97000000e-01, 2.26360000e+00/) &
      )

  type (cidatadb), parameter :: c169 = &
      cidatadb(eosid="PR", &
      cid="NC15", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=4.53108000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu170 = &
      alphadatadb(eosid="SRK", &
      cid="NC15", &
      ref="tcRK", &
      coeff=(/4.93500000e-01, 8.08700000e-01, 2.55440000e+00/) &
      )

  type (cidatadb), parameter :: c170 = &
      cidatadb(eosid="SRK", &
      cid="NC15", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.02313400e-04, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx97 = &
      gendatadb(ident = "NC5", &
      formula = "C5H12", &
      name = "N-PENTAN", &
      mw = 72.1510, &
      Tc = 469.6000, &
      Pc = 3374100.00, &
      Zc = 0.262000, &
      acf = 0.251000, &
      Tb = 309.2000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.58333000e+01, 2.47707000e+03, -3.99400000e+01/), &
      Tantmin = 220.0000, &
      Tantmax = 330.0000, &
      Zra = 0.268400 &
      )

  type (cpdata), parameter :: cp99 = &
      cpdata(cid = "NC5", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/6.32016770e+01,-1.17010000e-02,3.31649800e-03,-1.17051000e-06,1.99648000e-10, &
      -8.66520000e-15,4.07527500e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: mc62 = &
      alphadatadb(eosid="PR", &
      cid="NC5", &
      ref="Default", &
      coeff=(/9.82000000e-01, -1.16950000e+00, 2.75230000e+00/) &
      )

  type (alphadatadb), parameter :: mc63 = &
      alphadatadb(eosid="PR", &
      cid="NC5", &
      ref="Chapoy2005", &
      coeff=(/7.63000000e-01, -2.24000000e-01, 6.69000000e-01/) &
      )

  type (alphadatadb), parameter :: mc64 = &
      alphadatadb(eosid="SRK", &
      cid="NC5", &
      ref="Default", &
      coeff=(/9.82000000e-01, -1.16950000e+00, 2.75230000e+00/) &
      )

  type (alphadatadb), parameter :: mc65 = &
      alphadatadb(eosid="SRK", &
      cid="NC5", &
      ref="Chapoy2005", &
      coeff=(/9.01000000e-01, -3.05000000e-01, 5.42000000e-01/) &
      )

  type (gendatadb), parameter :: cx98 = &
      gendatadb(ident = "NC14", &
      formula = "C14H30", &
      name = "N-TETRADECANE", &
      mw = 198.3900, &
      Tc = 693.0000, &
      Pc = 1570000.00, &
      Zc = 0.244000, &
      acf = 0.644000, &
      Tb = 526.7600, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.240060 &
      )

  type (cpdata), parameter :: cp100 = &
      cpdata(cid = "NC14", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 8, &
      cp = (/1.83750000e+01,6.58500000e-03,3.23070000e-04,-4.26630000e-07,1.65900000e-10, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu171 = &
      alphadatadb(eosid="PR", &
      cid="NC14", &
      ref="tcPR", &
      coeff=(/4.90200000e-01, 7.97400000e-01, 2.13530000e+00/) &
      )

  type (cidatadb), parameter :: c171 = &
      cidatadb(eosid="PR", &
      cid="NC14", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=3.96525000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu172 = &
      alphadatadb(eosid="SRK", &
      cid="NC14", &
      ref="tcRK", &
      coeff=(/4.84100000e-01, 8.11800000e-01, 2.49950000e+00/) &
      )

  type (cidatadb), parameter :: c172 = &
      cidatadb(eosid="SRK", &
      cid="NC14", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=9.26542000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx99 = &
      gendatadb(ident = "NC24", &
      formula = "C24H50", &
      name = "N-TETRACOSANE", &
      mw = 338.6600, &
      Tc = 804.0000, &
      Pc = 980000.00, &
      Zc = 0.239000, &
      acf = 1.071000, &
      Tb = 664.4500, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.228390 &
      )

  type (cpdata), parameter :: cp101 = &
      cpdata(cid = "NC24", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/4.27180000e+05,1.28910000e+06,8.15290000e+02,-5.04180000e+05,9.44980000e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 300.0000, &
      Tcpmax = 1500.0000  &
      )

  type (alphadatadb), parameter :: twu173 = &
      alphadatadb(eosid="PR", &
      cid="NC24", &
      ref="tcPR", &
      coeff=(/4.65600000e-01, 8.09700000e-01, 3.35130000e+00/) &
      )

  type (cidatadb), parameter :: c173 = &
      cidatadb(eosid="PR", &
      cid="NC24", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=8.96547000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu174 = &
      alphadatadb(eosid="SRK", &
      cid="NC24", &
      ref="tcRK", &
      coeff=(/5.37300000e-01, 8.09500000e-01, 3.35700000e+00/) &
      )

  type (cidatadb), parameter :: c174 = &
      cidatadb(eosid="SRK", &
      cid="NC24", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.83043000e-04, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx100 = &
      gendatadb(ident = "NC23", &
      formula = "C23H48", &
      name = "N-TRICOSANE", &
      mw = 324.6300, &
      Tc = 796.0000, &
      Pc = 1020000.00, &
      Zc = 0.240000, &
      acf = 1.026200, &
      Tb = 653.3500, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.229280 &
      )

  type (cpdata), parameter :: cp102 = &
      cpdata(cid = "NC23", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 7, &
      cp = (/4.10150000e+05,1.23420000e+06,1.72310000e+03,8.52280000e+05,7.84970000e+02, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = 300.0000, &
      Tcpmax = 1500.0000  &
      )

  type (alphadatadb), parameter :: twu175 = &
      alphadatadb(eosid="PR", &
      cid="NC23", &
      ref="tcPR", &
      coeff=(/4.69700000e-01, 8.14200000e-01, 3.24260000e+00/) &
      )

  type (cidatadb), parameter :: c175 = &
      cidatadb(eosid="PR", &
      cid="NC23", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=8.45878000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu176 = &
      alphadatadb(eosid="SRK", &
      cid="NC23", &
      ref="tcRK", &
      coeff=(/5.44000000e-01, 8.14300000e-01, 3.24000000e+00/) &
      )

  type (cidatadb), parameter :: c176 = &
      cidatadb(eosid="SRK", &
      cid="NC23", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.73915900e-04, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx101 = &
      gendatadb(ident = "NC13", &
      formula = "C13H28", &
      name = "N-TRIDECANE", &
      mw = 184.3700, &
      Tc = 675.0000, &
      Pc = 1680000.00, &
      Zc = 0.246000, &
      acf = 0.618000, &
      Tb = 508.6300, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.243240 &
      )

  type (cpdata), parameter :: cp103 = &
      cpdata(cid = "NC13", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 8, &
      cp = (/1.85460000e+01,-7.63600000e-03,3.46040000e-04,-4.59780000e-07,1.85090000e-10, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu177 = &
      alphadatadb(eosid="PR", &
      cid="NC13", &
      ref="tcPR", &
      coeff=(/4.48200000e-01, 8.03900000e-01, 2.23430000e+00/) &
      )

  type (cidatadb), parameter :: c177 = &
      cidatadb(eosid="PR", &
      cid="NC13", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=3.00534000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu178 = &
      alphadatadb(eosid="SRK", &
      cid="NC13", &
      ref="tcRK", &
      coeff=(/4.47100000e-01, 8.17500000e-01, 2.60970000e+00/) &
      )

  type (cidatadb), parameter :: c178 = &
      cidatadb(eosid="SRK", &
      cid="NC13", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=7.83925000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx102 = &
      gendatadb(ident = "NC11", &
      formula = "C11H24", &
      name = "N-UNDECANE", &
      mw = 156.3120, &
      Tc = 639.0000, &
      Pc = 1980000.00, &
      Zc = 0.257000, &
      acf = 0.537000, &
      Tb = 469.1000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/1.60541000e+01, 3.61407000e+03, -8.54500000e+01/), &
      Tantmin = 348.0000, &
      Tantmax = 498.0000, &
      Zra = 0.249900 &
      )

  type (cpdata), parameter :: cp104 = &
      cpdata(cid = "NC11", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 2, &
      cp = (/6.52905640e+01,-9.98270000e-02,3.47249500e-03,-1.35433600e-06,2.64721000e-10, &
      -1.45574000e-14,3.40795900e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu179 = &
      alphadatadb(eosid="PR", &
      cid="NC11", &
      ref="tcPR", &
      coeff=(/4.18500000e-01, 8.07100000e-01, 2.12240000e+00/) &
      )

  type (cidatadb), parameter :: c179 = &
      cidatadb(eosid="PR", &
      cid="NC11", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=1.81817000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu180 = &
      alphadatadb(eosid="SRK", &
      cid="NC11", &
      ref="tcRK", &
      coeff=(/4.01800000e-01, 8.24500000e-01, 2.58800000e+00/) &
      )

  type (cidatadb), parameter :: c180 = &
      cidatadb(eosid="SRK", &
      cid="NC11", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=5.80492000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (gendatadb), parameter :: cx103 = &
      gendatadb(ident = "NC12", &
      formula = "C12H26", &
      name = "N-DODECANE", &
      mw = 170.3400, &
      Tc = 658.1000, &
      Pc = 1817000.00, &
      Zc = 0.249700, &
      acf = 0.574000, &
      Tb = 489.3000, &
      Ttr = 0.0000, &
      Ptr = 0.0000, &
      sref = 0.0000, &
      href = 0.0000, &
      DfH = 0.0000, &
      DfG = 0.0000, &
      psatcode = 1, &
      ant = (/0.00000000e+00, 0.00000000e+00, 0.00000000e+00/), &
      Tantmin = 0.0000, &
      Tantmax = 0.0000, &
      Zra = 0.246680 &
      )

  type (cpdata), parameter :: cp105 = &
      cpdata(cid = "NC12", &
      ref = "Default", &
      bib_ref = "", &
      cptype = 8, &
      cp = (/1.72290000e+01,-7.24200000e-03,3.19220000e-04,-4.23220000e-07,1.70220000e-10, &
      0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00,0.00000000e+00/), &
      Tcpmin = -20.0000, &
      Tcpmax = 1200.0000  &
      )

  type (alphadatadb), parameter :: twu181 = &
      alphadatadb(eosid="PR", &
      cid="NC12", &
      ref="tcPR", &
      coeff=(/3.95600000e-01, 8.11200000e-01, 2.35490000e+00/) &
      )

  type (cidatadb), parameter :: c181 = &
      cidatadb(eosid="PR", &
      cid="NC12", &
      ref="tcPR", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=2.20285000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )

  type (alphadatadb), parameter :: twu182 = &
      alphadatadb(eosid="SRK", &
      cid="NC12", &
      ref="tcRK", &
      coeff=(/3.88700000e-01, 8.27500000e-01, 2.82130000e+00/) &
      )

  type (cidatadb), parameter :: c182 = &
      cidatadb(eosid="SRK", &
      cid="NC12", &
      ref="tcRK", &
      bib_ref="10.1016/j.fluid.2016.09.003", &
      ciA=6.56626000e-05, &
      ciB=0.00000000e+00, &
      ciC=0.00000000e+00, &
      c_type=1 &
      )


  integer, parameter :: maxncdb =103
  type (gendatadb), dimension(maxncdb), parameter :: compdb = (/&
      cx1,cx2,cx3,cx4,cx5, &
      cx6,cx7,cx8,cx9,cx10, &
      cx11,cx12,cx13,cx14,cx15, &
      cx16,cx17,cx18,cx19,cx20, &
      cx21,cx22,cx23,cx24,cx25, &
      cx26,cx27,cx28,cx29,cx30, &
      cx31,cx32,cx33,cx34,cx35, &
      cx36,cx37,cx38,cx39,cx40, &
      cx41,cx42,cx43,cx44,cx45, &
      cx46,cx47,cx48,cx49,cx50, &
      cx51,cx52,cx53,cx54,cx55, &
      cx56,cx57,cx58,cx59,cx60, &
      cx61,cx62,cx63,cx64,cx65, &
      cx66,cx67,cx68,cx69,cx70, &
      cx71,cx72,cx73,cx74,cx75, &
      cx76,cx77,cx78,cx79,cx80, &
      cx81,cx82,cx83,cx84,cx85, &
      cx86,cx87,cx88,cx89,cx90, &
      cx91,cx92,cx93,cx94,cx95, &
      cx96,cx97,cx98,cx99,cx100, &
      cx101,cx102,cx103 &
  /)

  integer, parameter :: maxcpdb =105
  type (cpdata), dimension(maxcpdb), parameter :: cpdb = (/&
      cp1,cp2,cp3,cp4,cp5, &
      cp6,cp7,cp8,cp9,cp10, &
      cp11,cp12,cp13,cp14,cp15, &
      cp16,cp17,cp18,cp19,cp20, &
      cp21,cp22,cp23,cp24,cp25, &
      cp26,cp27,cp28,cp29,cp30, &
      cp31,cp32,cp33,cp34,cp35, &
      cp36,cp37,cp38,cp39,cp40, &
      cp41,cp42,cp43,cp44,cp45, &
      cp46,cp47,cp48,cp49,cp50, &
      cp51,cp52,cp53,cp54,cp55, &
      cp56,cp57,cp58,cp59,cp60, &
      cp61,cp62,cp63,cp64,cp65, &
      cp66,cp67,cp68,cp69,cp70, &
      cp71,cp72,cp73,cp74,cp75, &
      cp76,cp77,cp78,cp79,cp80, &
      cp81,cp82,cp83,cp84,cp85, &
      cp86,cp87,cp88,cp89,cp90, &
      cp91,cp92,cp93,cp94,cp95, &
      cp96,cp97,cp98,cp99,cp100, &
      cp101,cp102,cp103,cp104,cp105 &
  /)

  integer, parameter :: maxTWUdb =182
  type (alphadatadb), dimension(maxTWUdb), parameter :: alphaTWUdb = (/&
      twu1,twu2,twu3,twu4,twu5, &
      twu6,twu7,twu8,twu9,twu10, &
      twu11,twu12,twu13,twu14,twu15, &
      twu16,twu17,twu18,twu19,twu20, &
      twu21,twu22,twu23,twu24,twu25, &
      twu26,twu27,twu28,twu29,twu30, &
      twu31,twu32,twu33,twu34,twu35, &
      twu36,twu37,twu38,twu39,twu40, &
      twu41,twu42,twu43,twu44,twu45, &
      twu46,twu47,twu48,twu49,twu50, &
      twu51,twu52,twu53,twu54,twu55, &
      twu56,twu57,twu58,twu59,twu60, &
      twu61,twu62,twu63,twu64,twu65, &
      twu66,twu67,twu68,twu69,twu70, &
      twu71,twu72,twu73,twu74,twu75, &
      twu76,twu77,twu78,twu79,twu80, &
      twu81,twu82,twu83,twu84,twu85, &
      twu86,twu87,twu88,twu89,twu90, &
      twu91,twu92,twu93,twu94,twu95, &
      twu96,twu97,twu98,twu99,twu100, &
      twu101,twu102,twu103,twu104,twu105, &
      twu106,twu107,twu108,twu109,twu110, &
      twu111,twu112,twu113,twu114,twu115, &
      twu116,twu117,twu118,twu119,twu120, &
      twu121,twu122,twu123,twu124,twu125, &
      twu126,twu127,twu128,twu129,twu130, &
      twu131,twu132,twu133,twu134,twu135, &
      twu136,twu137,twu138,twu139,twu140, &
      twu141,twu142,twu143,twu144,twu145, &
      twu146,twu147,twu148,twu149,twu150, &
      twu151,twu152,twu153,twu154,twu155, &
      twu156,twu157,twu158,twu159,twu160, &
      twu161,twu162,twu163,twu164,twu165, &
      twu166,twu167,twu168,twu169,twu170, &
      twu171,twu172,twu173,twu174,twu175, &
      twu176,twu177,twu178,twu179,twu180, &
      twu181,twu182 &
  /)

  integer, parameter :: maxMCdb =65
  type (alphadatadb), dimension(maxMCdb), parameter :: alphaMCdb = (/&
      mc1,mc2,mc3,mc4,mc5, &
      mc6,mc7,mc8,mc9,mc10, &
      mc11,mc12,mc13,mc14,mc15, &
      mc16,mc17,mc18,mc19,mc20, &
      mc21,mc22,mc23,mc24,mc25, &
      mc26,mc27,mc28,mc29,mc30, &
      mc31,mc32,mc33,mc34,mc35, &
      mc36,mc37,mc38,mc39,mc40, &
      mc41,mc42,mc43,mc44,mc45, &
      mc46,mc47,mc48,mc49,mc50, &
      mc51,mc52,mc53,mc54,mc55, &
      mc56,mc57,mc58,mc59,mc60, &
      mc61,mc62,mc63,mc64,mc65 &
  /)

  integer, parameter :: maxcidb =182
  type (cidatadb), dimension(maxcidb), parameter :: cidb = (/&
      c1,c2,c3,c4,c5, &
      c6,c7,c8,c9,c10, &
      c11,c12,c13,c14,c15, &
      c16,c17,c18,c19,c20, &
      c21,c22,c23,c24,c25, &
      c26,c27,c28,c29,c30, &
      c31,c32,c33,c34,c35, &
      c36,c37,c38,c39,c40, &
      c41,c42,c43,c44,c45, &
      c46,c47,c48,c49,c50, &
      c51,c52,c53,c54,c55, &
      c56,c57,c58,c59,c60, &
      c61,c62,c63,c64,c65, &
      c66,c67,c68,c69,c70, &
      c71,c72,c73,c74,c75, &
      c76,c77,c78,c79,c80, &
      c81,c82,c83,c84,c85, &
      c86,c87,c88,c89,c90, &
      c91,c92,c93,c94,c95, &
      c96,c97,c98,c99,c100, &
      c101,c102,c103,c104,c105, &
      c106,c107,c108,c109,c110, &
      c111,c112,c113,c114,c115, &
      c116,c117,c118,c119,c120, &
      c121,c122,c123,c124,c125, &
      c126,c127,c128,c129,c130, &
      c131,c132,c133,c134,c135, &
      c136,c137,c138,c139,c140, &
      c141,c142,c143,c144,c145, &
      c146,c147,c148,c149,c150, &
      c151,c152,c153,c154,c155, &
      c156,c157,c158,c159,c160, &
      c161,c162,c163,c164,c165, &
      c166,c167,c168,c169,c170, &
      c171,c172,c173,c174,c175, &
      c176,c177,c178,c179,c180, &
      c181,c182 &
  /)

end module compdatadb
