!> Automatically generated to file mixdatadb.f90
!! using utility python code pyUtils
!! Time stamp: 2021-06-30T11:07:26.573810

module mixdatadb
  use cubic_eos, only: kijdatadb, interGEdatadb, lijdatadb
  implicit none
  public

  type (kijdatadb), parameter :: vdw1 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C1", &
      uid2 = "C2", &
      kijvalue = 0.00500000  &
      )

  type (kijdatadb), parameter :: vdw2 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C1", &
      uid2 = "IC4", &
      kijvalue = -0.00800000  &
      )

  type (kijdatadb), parameter :: vdw3 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C1", &
      uid2 = "NC4", &
      kijvalue = 0.01500000  &
      )

  type (kijdatadb), parameter :: vdw4 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C1", &
      uid2 = "NC5", &
      kijvalue = 0.02000000  &
      )

  type (kijdatadb), parameter :: vdw5 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C1", &
      uid2 = "NC6", &
      kijvalue = 0.01000000  &
      )

  type (kijdatadb), parameter :: vdw6 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C2", &
      uid2 = "C3", &
      kijvalue = 0.00600000  &
      )

  type (kijdatadb), parameter :: vdw7 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C2", &
      uid2 = "PRLN", &
      kijvalue = 0.00400000  &
      )

  type (kijdatadb), parameter :: vdw8 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C2", &
      uid2 = "NC4", &
      kijvalue = 0.00100000  &
      )

  type (kijdatadb), parameter :: vdw9 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C2", &
      uid2 = "NC5", &
      kijvalue = -0.00100000  &
      )

  type (kijdatadb), parameter :: vdw10 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C2", &
      uid2 = "NC7", &
      kijvalue = -0.01500000  &
      )

  type (kijdatadb), parameter :: vdw11 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C2_1", &
      uid2 = "C1", &
      kijvalue = 0.02600000  &
      )

  type (kijdatadb), parameter :: vdw12 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C2_1", &
      uid2 = "C2", &
      kijvalue = 0.01800000  &
      )

  type (kijdatadb), parameter :: vdw13 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C2_1", &
      uid2 = "C3", &
      kijvalue = 0.01900000  &
      )

  type (kijdatadb), parameter :: vdw14 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C2_1", &
      uid2 = "NC4", &
      kijvalue = 0.06200000  &
      )

  type (kijdatadb), parameter :: vdw15 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C3", &
      uid2 = "NC4", &
      kijvalue = 0.01300000  &
      )

  type (kijdatadb), parameter :: vdw16 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C3", &
      uid2 = "NC5", &
      kijvalue = 0.01300000  &
      )

  type (kijdatadb), parameter :: vdw17 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "C3", &
      uid2 = "IC5", &
      kijvalue = 0.02100000  &
      )

  type (kijdatadb), parameter :: vdw18 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "PRLN", &
      uid2 = "C3", &
      kijvalue = 0.03000000  &
      )

  type (kijdatadb), parameter :: vdw19 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "NC4", &
      uid2 = "NC10", &
      kijvalue = 0.00500000  &
      )

  type (kijdatadb), parameter :: vdw20 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "IC4", &
      uid2 = "NC4", &
      kijvalue = -0.00300000  &
      )

  type (kijdatadb), parameter :: vdw21 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "CO2", &
      uid2 = "C1", &
      kijvalue = 0.09300000  &
      )

  type (kijdatadb), parameter :: vdw22 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "CO2", &
      uid2 = "C2", &
      kijvalue = 0.12800000  &
      )

  type (kijdatadb), parameter :: vdw23 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "CO2", &
      uid2 = "C2_1", &
      kijvalue = 0.05700000  &
      )

  type (kijdatadb), parameter :: vdw24 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "CO2", &
      uid2 = "C3", &
      kijvalue = 0.13100000  &
      )

  type (kijdatadb), parameter :: vdw25 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "CO2", &
      uid2 = "NC4", &
      kijvalue = 0.10900000  &
      )

  type (kijdatadb), parameter :: vdw26 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "CO2", &
      uid2 = "IC4", &
      kijvalue = 0.12700000  &
      )

  type (kijdatadb), parameter :: vdw27 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "CO2", &
      uid2 = "NC5", &
      kijvalue = 0.13500000  &
      )

  type (kijdatadb), parameter :: vdw28 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "H2S", &
      uid2 = "C1", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw29 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "H2S", &
      uid2 = "C2", &
      kijvalue = 0.08900000  &
      )

  type (kijdatadb), parameter :: vdw30 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "H2S", &
      uid2 = "IC4", &
      kijvalue = 0.04600000  &
      )

  type (kijdatadb), parameter :: vdw31 = &
      kijdatadb(eosid = "PT", &
      mruleid = "vdW", &
      ref = "Patel1982", &
      bib_ref = "10.1016/0009-2509(82)80099-7", &
      uid1 = "H2S", &
      uid2 = "NC7", &
      kijvalue = 0.05300000  &
      )

  type (kijdatadb), parameter :: vdw32 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "H2S", &
      kijvalue = 0.10100000  &
      )

  type (kijdatadb), parameter :: vdw33 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "C2", &
      kijvalue = -0.00780000  &
      )

  type (kijdatadb), parameter :: vdw34 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "C2_1", &
      kijvalue = 0.02000000  &
      )

  type (kijdatadb), parameter :: vdw35 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "C3", &
      kijvalue = 0.00900000  &
      )

  type (kijdatadb), parameter :: vdw36 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "IC4", &
      kijvalue = 0.02410000  &
      )

  type (kijdatadb), parameter :: vdw37 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC4", &
      kijvalue = 0.00560000  &
      )

  type (kijdatadb), parameter :: vdw38 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC5", &
      kijvalue = 0.01900000  &
      )

  type (kijdatadb), parameter :: vdw39 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "H2S", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw40 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "C2_1", &
      kijvalue = 0.01120000  &
      )

  type (kijdatadb), parameter :: vdw41 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "C3", &
      kijvalue = -0.00220000  &
      )

  type (kijdatadb), parameter :: vdw42 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "IC4", &
      kijvalue = -0.01000000  &
      )

  type (kijdatadb), parameter :: vdw43 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC4", &
      kijvalue = 0.00670000  &
      )

  type (kijdatadb), parameter :: vdw44 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC5", &
      kijvalue = 0.00560000  &
      )

  type (kijdatadb), parameter :: vdw45 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "C2_1", &
      kijvalue = 0.10000000  &
      )

  type (kijdatadb), parameter :: vdw46 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "IC4", &
      kijvalue = -0.01000000  &
      )

  type (kijdatadb), parameter :: vdw47 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC5", &
      kijvalue = 0.02300000  &
      )

  type (kijdatadb), parameter :: vdw48 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "NC4", &
      kijvalue = 0.00110000  &
      )

  type (kijdatadb), parameter :: vdw49 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "NC5", &
      kijvalue = 0.02040000  &
      )

  type (kijdatadb), parameter :: vdw50 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "C1", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw51 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "O2", &
      kijvalue = 0.11800000  &
      )

  type (kijdatadb), parameter :: vdw52 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NH3", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw53 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "N2O", &
      kijvalue = 0.01000000  &
      )

  type (kijdatadb), parameter :: vdw54 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "N2O4", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw55 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "CO", &
      kijvalue = -0.06800000  &
      )

  type (kijdatadb), parameter :: vdw56 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C1", &
      kijvalue = 0.10600000  &
      )

  type (kijdatadb), parameter :: vdw57 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C2", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw58 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C2_1", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw59 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C3", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw60 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "H2O", &
      kijvalue = 0.07400000  &
      )

  type (kijdatadb), parameter :: vdw61 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "IC4", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw62 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "IC5", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw63 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC10", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw64 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC11", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw65 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC4", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw66 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC5", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw67 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "H2S", &
      kijvalue = 0.09900000  &
      )

  type (kijdatadb), parameter :: vdw68 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "N2", &
      kijvalue = -0.04200000  &
      )

  type (kijdatadb), parameter :: vdw69 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "AR", &
      kijvalue = 0.09400000  &
      )

  type (kijdatadb), parameter :: vdw70 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "SO2", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw71 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "H2", &
      kijvalue = 0.10800000  &
      )

  type (kijdatadb), parameter :: vdw72 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC4", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw73 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC5", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw74 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC6", &
      kijvalue = 0.05000000  &
      )

  type (kijdatadb), parameter :: vdw75 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC7", &
      kijvalue = 0.04000000  &
      )

  type (kijdatadb), parameter :: vdw76 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC8", &
      kijvalue = 0.04000000  &
      )

  type (kijdatadb), parameter :: vdw77 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC9", &
      kijvalue = 0.03000000  &
      )

  type (kijdatadb), parameter :: vdw78 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MEG", &
      uid2 = "CO2", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw79 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MEG", &
      uid2 = "H2O", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw80 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MEG", &
      uid2 = "C1", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw81 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MEOH", &
      uid2 = "CO2", &
      kijvalue = 0.01700000  &
      )

  type (kijdatadb), parameter :: vdw82 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C1", &
      kijvalue = 0.04200000  &
      )

  type (kijdatadb), parameter :: vdw83 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C2", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw84 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C2_1", &
      kijvalue = 0.07500000  &
      )

  type (kijdatadb), parameter :: vdw85 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C3", &
      kijvalue = 0.09000000  &
      )

  type (kijdatadb), parameter :: vdw86 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "IC4", &
      kijvalue = 0.11300000  &
      )

  type (kijdatadb), parameter :: vdw87 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "IC5", &
      kijvalue = 0.08700000  &
      )

  type (kijdatadb), parameter :: vdw88 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC10", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw89 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC11", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw90 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC4", &
      kijvalue = 0.11300000  &
      )

  type (kijdatadb), parameter :: vdw91 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC5", &
      kijvalue = 0.14000000  &
      )

  type (kijdatadb), parameter :: vdw92 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC6", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw93 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC7", &
      kijvalue = 0.14200000  &
      )

  type (kijdatadb), parameter :: vdw94 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC8", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw95 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC9", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw96 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "O2", &
      kijvalue = -0.00800000  &
      )

  type (kijdatadb), parameter :: vdw97 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "BENZENE", &
      kijvalue = 0.01100000  &
      )

  type (kijdatadb), parameter :: vdw98 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R12", &
      uid2 = "R11", &
      kijvalue = 0.00540000  &
      )

  type (kijdatadb), parameter :: vdw99 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R12", &
      uid2 = "R114", &
      kijvalue = 0.00150000  &
      )

  type (kijdatadb), parameter :: vdw100 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R12", &
      uid2 = "R152a", &
      kijvalue = 0.08670000  &
      )

  type (kijdatadb), parameter :: vdw101 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13", &
      uid2 = "R11", &
      kijvalue = 0.02620000  &
      )

  type (kijdatadb), parameter :: vdw102 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13", &
      uid2 = "R113", &
      kijvalue = 0.02430000  &
      )

  type (kijdatadb), parameter :: vdw103 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13", &
      uid2 = "R12", &
      kijvalue = 0.02990000  &
      )

  type (kijdatadb), parameter :: vdw104 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13B", &
      uid2 = "R12", &
      kijvalue = -0.00320000  &
      )

  type (kijdatadb), parameter :: vdw105 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13B", &
      uid2 = "R152a", &
      kijvalue = 0.07990000  &
      )

  type (kijdatadb), parameter :: vdw106 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R14", &
      uid2 = "R13", &
      kijvalue = 0.03040000  &
      )

  type (kijdatadb), parameter :: vdw107 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R14", &
      uid2 = "R23", &
      kijvalue = 0.10080000  &
      )

  type (kijdatadb), parameter :: vdw108 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R11", &
      kijvalue = 0.04660000  &
      )

  type (kijdatadb), parameter :: vdw109 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R114", &
      kijvalue = 0.03990000  &
      )

  type (kijdatadb), parameter :: vdw110 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R12", &
      kijvalue = 0.05640000  &
      )

  type (kijdatadb), parameter :: vdw111 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R142b", &
      kijvalue = 0.00570000  &
      )

  type (kijdatadb), parameter :: vdw112 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R115", &
      kijvalue = 0.08900000  &
      )

  type (kijdatadb), parameter :: vdw113 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R23", &
      uid2 = "R13", &
      kijvalue = 0.10320000  &
      )

  type (kijdatadb), parameter :: vdw114 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R218", &
      uid2 = "R152a", &
      kijvalue = 0.12000000  &
      )

  type (kijdatadb), parameter :: vdw115 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R125", &
      uid2 = "R143a", &
      kijvalue = -0.01110000  &
      )

  type (kijdatadb), parameter :: vdw116 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R125", &
      uid2 = "R134a", &
      kijvalue = -0.00240000  &
      )

  type (kijdatadb), parameter :: vdw117 = &
      kijdatadb(eosid = "CSP-SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R143a", &
      uid2 = "R134a", &
      kijvalue = 0.00130000  &
      )

  type (kijdatadb), parameter :: vdw118 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "C2", &
      kijvalue = 1.05200000  &
      )

  type (kijdatadb), parameter :: vdw119 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "C2_1", &
      kijvalue = 1.01400000  &
      )

  type (kijdatadb), parameter :: vdw120 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "C3", &
      kijvalue = 1.11300000  &
      )

  type (kijdatadb), parameter :: vdw121 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "PRLN", &
      kijvalue = 1.08900000  &
      )

  type (kijdatadb), parameter :: vdw122 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "IC4", &
      kijvalue = 1.15500000  &
      )

  type (kijdatadb), parameter :: vdw123 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "NC4", &
      kijvalue = 1.17100000  &
      )

  type (kijdatadb), parameter :: vdw124 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "NC5", &
      kijvalue = 1.24000000  &
      )

  type (kijdatadb), parameter :: vdw125 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "IC5", &
      kijvalue = 1.22800000  &
      )

  type (kijdatadb), parameter :: vdw126 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "NC6", &
      kijvalue = 1.30400000  &
      )

  type (kijdatadb), parameter :: vdw127 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "NC7", &
      kijvalue = 1.36700000  &
      )

  type (kijdatadb), parameter :: vdw128 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "NC8", &
      kijvalue = 1.42300000  &
      )

  type (kijdatadb), parameter :: vdw129 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "NC9", &
      kijvalue = 1.48400000  &
      )

  type (kijdatadb), parameter :: vdw130 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "NC10", &
      kijvalue = 1.53300000  &
      )

  type (kijdatadb), parameter :: vdw131 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C1", &
      uid2 = "BENZENE", &
      kijvalue = 1.23400000  &
      )

  type (kijdatadb), parameter :: vdw132 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "C2_1", &
      kijvalue = 0.99100000  &
      )

  type (kijdatadb), parameter :: vdw133 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "C3", &
      kijvalue = 1.01000000  &
      )

  type (kijdatadb), parameter :: vdw134 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "PRLN", &
      kijvalue = 1.00200000  &
      )

  type (kijdatadb), parameter :: vdw135 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "IC4", &
      kijvalue = 1.03600000  &
      )

  type (kijdatadb), parameter :: vdw136 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "NC4", &
      kijvalue = 1.02900000  &
      )

  type (kijdatadb), parameter :: vdw137 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "NC5", &
      kijvalue = 1.06400000  &
      )

  type (kijdatadb), parameter :: vdw138 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "IC5", &
      kijvalue = 1.07000000  &
      )

  type (kijdatadb), parameter :: vdw139 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "NC6", &
      kijvalue = 1.10600000  &
      )

  type (kijdatadb), parameter :: vdw140 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "NC7", &
      kijvalue = 1.14300000  &
      )

  type (kijdatadb), parameter :: vdw141 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "NC8", &
      kijvalue = 1.16500000  &
      )

  type (kijdatadb), parameter :: vdw142 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "NC9", &
      kijvalue = 1.21400000  &
      )

  type (kijdatadb), parameter :: vdw143 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "NC10", &
      kijvalue = 1.23700000  &
      )

  type (kijdatadb), parameter :: vdw144 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2", &
      uid2 = "BENZENE", &
      kijvalue = 1.06600000  &
      )

  type (kijdatadb), parameter :: vdw145 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2_1", &
      uid2 = "NC4", &
      kijvalue = 0.99800000  &
      )

  type (kijdatadb), parameter :: vdw146 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2_1", &
      uid2 = "BENZENE", &
      kijvalue = 1.09400000  &
      )

  type (kijdatadb), parameter :: vdw147 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C2_1", &
      uid2 = "NC7", &
      kijvalue = 1.16300000  &
      )

  type (kijdatadb), parameter :: vdw148 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C3", &
      uid2 = "PRLN", &
      kijvalue = 0.99200000  &
      )

  type (kijdatadb), parameter :: vdw149 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C3", &
      uid2 = "NC4", &
      kijvalue = 1.00300000  &
      )

  type (kijdatadb), parameter :: vdw150 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C3", &
      uid2 = "IC4", &
      kijvalue = 1.00300000  &
      )

  type (kijdatadb), parameter :: vdw151 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C3", &
      uid2 = "NC5", &
      kijvalue = 1.00600000  &
      )

  type (kijdatadb), parameter :: vdw152 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C3", &
      uid2 = "IC5", &
      kijvalue = 1.00900000  &
      )

  type (kijdatadb), parameter :: vdw153 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C3", &
      uid2 = "NC6", &
      kijvalue = 1.04700000  &
      )

  type (kijdatadb), parameter :: vdw154 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C3", &
      uid2 = "BENZENE", &
      kijvalue = 1.01100000  &
      )

  type (kijdatadb), parameter :: vdw155 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C3", &
      uid2 = "NC7", &
      kijvalue = 1.06700000  &
      )

  type (kijdatadb), parameter :: vdw156 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C3", &
      uid2 = "NC8", &
      kijvalue = 1.09000000  &
      )

  type (kijdatadb), parameter :: vdw157 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C3", &
      uid2 = "NC9", &
      kijvalue = 1.01150000  &
      )

  type (kijdatadb), parameter :: vdw158 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "C3", &
      uid2 = "NC10", &
      kijvalue = 1.13900000  &
      )

  type (kijdatadb), parameter :: vdw159 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "PRLN", &
      uid2 = "NC4", &
      kijvalue = 1.01000000  &
      )

  type (kijdatadb), parameter :: vdw160 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "PRLN", &
      uid2 = "IC4", &
      kijvalue = 1.00900000  &
      )

  type (kijdatadb), parameter :: vdw161 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC4", &
      uid2 = "IC4", &
      kijvalue = 1.00100000  &
      )

  type (kijdatadb), parameter :: vdw162 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC4", &
      uid2 = "NC5", &
      kijvalue = 0.99400000  &
      )

  type (kijdatadb), parameter :: vdw163 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC4", &
      uid2 = "IC5", &
      kijvalue = 0.99800000  &
      )

  type (kijdatadb), parameter :: vdw164 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC4", &
      uid2 = "NC6", &
      kijvalue = 1.01800000  &
      )

  type (kijdatadb), parameter :: vdw165 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC4", &
      uid2 = "BENZENE", &
      kijvalue = 0.99900000  &
      )

  type (kijdatadb), parameter :: vdw166 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC4", &
      uid2 = "NC7", &
      kijvalue = 1.02700000  &
      )

  type (kijdatadb), parameter :: vdw167 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC4", &
      uid2 = "NC8", &
      kijvalue = 1.04600000  &
      )

  type (kijdatadb), parameter :: vdw168 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC4", &
      uid2 = "NC9", &
      kijvalue = 1.06400000  &
      )

  type (kijdatadb), parameter :: vdw169 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC4", &
      uid2 = "NC10", &
      kijvalue = 1.07800000  &
      )

  type (kijdatadb), parameter :: vdw170 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC5", &
      uid2 = "IC5", &
      kijvalue = 0.98700000  &
      )

  type (kijdatadb), parameter :: vdw171 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC5", &
      uid2 = "NC6", &
      kijvalue = 0.99600000  &
      )

  type (kijdatadb), parameter :: vdw172 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC5", &
      uid2 = "BENZENE", &
      kijvalue = 0.97700000  &
      )

  type (kijdatadb), parameter :: vdw173 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC5", &
      uid2 = "NC7", &
      kijvalue = 1.00400000  &
      )

  type (kijdatadb), parameter :: vdw174 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC5", &
      uid2 = "NC8", &
      kijvalue = 1.02000000  &
      )

  type (kijdatadb), parameter :: vdw175 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC5", &
      uid2 = "NC9", &
      kijvalue = 1.03300000  &
      )

  type (kijdatadb), parameter :: vdw176 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC5", &
      uid2 = "NC10", &
      kijvalue = 1.04500000  &
      )

  type (kijdatadb), parameter :: vdw177 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC6", &
      uid2 = "BENZENE", &
      kijvalue = 0.97800000  &
      )

  type (kijdatadb), parameter :: vdw178 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC6", &
      uid2 = "NC7", &
      kijvalue = 1.00800000  &
      )

  type (kijdatadb), parameter :: vdw179 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC6", &
      uid2 = "NC8", &
      kijvalue = 1.00500000  &
      )

  type (kijdatadb), parameter :: vdw180 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC6", &
      uid2 = "NC9", &
      kijvalue = 1.01500000  &
      )

  type (kijdatadb), parameter :: vdw181 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC6", &
      uid2 = "NC10", &
      kijvalue = 1.02500000  &
      )

  type (kijdatadb), parameter :: vdw182 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "BENZENE", &
      uid2 = "NC7", &
      kijvalue = 0.98500000  &
      )

  type (kijdatadb), parameter :: vdw183 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "BENZENE", &
      uid2 = "NC8", &
      kijvalue = 0.98700000  &
      )

  type (kijdatadb), parameter :: vdw184 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "BENZENE", &
      uid2 = "NC9", &
      kijvalue = 1.03400000  &
      )

  type (kijdatadb), parameter :: vdw185 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "BENZENE", &
      uid2 = "NC10", &
      kijvalue = 1.04700000  &
      )

  type (kijdatadb), parameter :: vdw186 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC7", &
      uid2 = "NC8", &
      kijvalue = 0.99300000  &
      )

  type (kijdatadb), parameter :: vdw187 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC7", &
      uid2 = "NC9", &
      kijvalue = 1.00200000  &
      )

  type (kijdatadb), parameter :: vdw188 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC7", &
      uid2 = "NC10", &
      kijvalue = 1.01000000  &
      )

  type (kijdatadb), parameter :: vdw189 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC8", &
      uid2 = "NC9", &
      kijvalue = 0.99300000  &
      )

  type (kijdatadb), parameter :: vdw190 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC8", &
      uid2 = "NC10", &
      kijvalue = 0.99900000  &
      )

  type (kijdatadb), parameter :: vdw191 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "NC9", &
      uid2 = "NC10", &
      kijvalue = 0.99100000  &
      )

  type (kijdatadb), parameter :: vdw192 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "C1", &
      kijvalue = 0.97700000  &
      )

  type (kijdatadb), parameter :: vdw193 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "C2_1", &
      kijvalue = 1.03200000  &
      )

  type (kijdatadb), parameter :: vdw194 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "C2", &
      kijvalue = 1.08200000  &
      )

  type (kijdatadb), parameter :: vdw195 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "C3", &
      kijvalue = 1.17700000  &
      )

  type (kijdatadb), parameter :: vdw196 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "PRLN", &
      kijvalue = 1.15100000  &
      )

  type (kijdatadb), parameter :: vdw197 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "NC4", &
      kijvalue = 1.27600000  &
      )

  type (kijdatadb), parameter :: vdw198 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "NC5", &
      kijvalue = 1.37200000  &
      )

  type (kijdatadb), parameter :: vdw199 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "NC6", &
      kijvalue = 1.44200000  &
      )

  type (kijdatadb), parameter :: vdw200 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "O2", &
      kijvalue = 0.99700000  &
      )

  type (kijdatadb), parameter :: vdw201 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "CO", &
      kijvalue = 0.98700000  &
      )

  type (kijdatadb), parameter :: vdw202 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "H2S", &
      kijvalue = 0.98300000  &
      )

  type (kijdatadb), parameter :: vdw203 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "CO2", &
      kijvalue = 1.11000000  &
      )

  type (kijdatadb), parameter :: vdw204 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "N2O", &
      kijvalue = 1.07300000  &
      )

  type (kijdatadb), parameter :: vdw205 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2", &
      uid2 = "NH3", &
      kijvalue = 1.03300000  &
      )

  type (kijdatadb), parameter :: vdw206 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "C1", &
      kijvalue = 0.97500000  &
      )

  type (kijdatadb), parameter :: vdw207 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "C2", &
      kijvalue = 0.93800000  &
      )

  type (kijdatadb), parameter :: vdw208 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "C3", &
      kijvalue = 0.92500000  &
      )

  type (kijdatadb), parameter :: vdw209 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "NC4", &
      kijvalue = 0.95500000  &
      )

  type (kijdatadb), parameter :: vdw210 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "IC4", &
      kijvalue = 0.94600000  &
      )

  type (kijdatadb), parameter :: vdw211 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "NC5", &
      kijvalue = 1.00200000  &
      )

  type (kijdatadb), parameter :: vdw212 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "NC6", &
      kijvalue = 1.01800000  &
      )

  type (kijdatadb), parameter :: vdw213 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "BENZENE", &
      kijvalue = 1.01800000  &
      )

  type (kijdatadb), parameter :: vdw214 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "NC7", &
      kijvalue = 1.05800000  &
      )

  type (kijdatadb), parameter :: vdw215 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "NC8", &
      kijvalue = 1.09000000  &
      )

  type (kijdatadb), parameter :: vdw216 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "NC9", &
      kijvalue = 1.12600000  &
      )

  type (kijdatadb), parameter :: vdw217 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "NC10", &
      kijvalue = 1.16000000  &
      )

  type (kijdatadb), parameter :: vdw218 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "H2S", &
      kijvalue = 0.92200000  &
      )

  type (kijdatadb), parameter :: vdw219 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "R12", &
      kijvalue = 0.96900000  &
      )

  type (kijdatadb), parameter :: vdw220 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO2", &
      uid2 = "MEOH", &
      kijvalue = 1.06900000  &
      )

  type (kijdatadb), parameter :: vdw221 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2", &
      uid2 = "C1", &
      kijvalue = 1.21600000  &
      )

  type (kijdatadb), parameter :: vdw222 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2", &
      uid2 = "C2", &
      kijvalue = 1.60400000  &
      )

  type (kijdatadb), parameter :: vdw223 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2", &
      uid2 = "PRLN", &
      kijvalue = 1.49800000  &
      )

  type (kijdatadb), parameter :: vdw224 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2", &
      uid2 = "C3", &
      kijvalue = 1.82600000  &
      )

  type (kijdatadb), parameter :: vdw225 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2", &
      uid2 = "NC4", &
      kijvalue = 2.09300000  &
      )

  type (kijdatadb), parameter :: vdw226 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2", &
      uid2 = "NC5", &
      kijvalue = 2.33500000  &
      )

  type (kijdatadb), parameter :: vdw227 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2", &
      uid2 = "NC6", &
      kijvalue = 2.45600000  &
      )

  type (kijdatadb), parameter :: vdw228 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2", &
      uid2 = "NC7", &
      kijvalue = 2.63400000  &
      )

  type (kijdatadb), parameter :: vdw229 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2", &
      uid2 = "N2", &
      kijvalue = 1.08000000  &
      )

  type (kijdatadb), parameter :: vdw230 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2", &
      uid2 = "CO", &
      kijvalue = 1.08500000  &
      )

  type (kijdatadb), parameter :: vdw231 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2", &
      uid2 = "CO2", &
      kijvalue = 1.62400000  &
      )

  type (kijdatadb), parameter :: vdw232 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "O2", &
      uid2 = "N2O", &
      kijvalue = 1.05700000  &
      )

  type (kijdatadb), parameter :: vdw233 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "CO", &
      uid2 = "C1", &
      kijvalue = 0.97400000  &
      )

  type (kijdatadb), parameter :: vdw234 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2S", &
      uid2 = "IC4", &
      kijvalue = 0.94700000  &
      )

  type (kijdatadb), parameter :: vdw235 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "N2O", &
      uid2 = "C1", &
      kijvalue = 1.01700000  &
      )

  type (kijdatadb), parameter :: vdw236 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2O", &
      uid2 = "CO2", &
      kijvalue = 0.92000000  &
      )

  type (kijdatadb), parameter :: vdw237 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2O", &
      uid2 = "NH3", &
      kijvalue = 1.15200000  &
      )

  type (kijdatadb), parameter :: vdw238 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.1021/i260067a020", &
      uid1 = "H2O", &
      uid2 = "MEOH", &
      kijvalue = 0.97900000  &
      )

  type (kijdatadb), parameter :: vdw239 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "10.3303/CET1332311", &
      uid1 = "CO2", &
      uid2 = "AR", &
      kijvalue = 0.99200000  &
      )

  type (kijdatadb), parameter :: vdw240 = &
      kijdatadb(eosid = "LK", &
      mruleid = "vdW", &
      ref = "Plocker1982(+)", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "O2", &
      kijvalue = 1.03200000  &
      )

  type (kijdatadb), parameter :: vdw241 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "H2S", &
      kijvalue = 0.09500000  &
      )

  type (kijdatadb), parameter :: vdw242 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "O2", &
      kijvalue = 0.10900000  &
      )

  type (kijdatadb), parameter :: vdw243 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NH3", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw244 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "N2O", &
      kijvalue = 0.01000000  &
      )

  type (kijdatadb), parameter :: vdw245 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "N2O4", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw246 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C1", &
      kijvalue = 0.10000000  &
      )

  type (kijdatadb), parameter :: vdw247 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C2", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw248 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C2_1", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw249 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C3", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw250 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "H2O", &
      kijvalue = 0.07400000  &
      )

  type (kijdatadb), parameter :: vdw251 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "IC4", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw252 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "IC5", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw253 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC10", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw254 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC11", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw255 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC4", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw256 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC5", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw257 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "H2S", &
      kijvalue = 0.10200000  &
      )

  type (kijdatadb), parameter :: vdw258 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "N2", &
      kijvalue = -0.03600000  &
      )

  type (kijdatadb), parameter :: vdw259 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "AR", &
      kijvalue = 0.08800000  &
      )

  type (kijdatadb), parameter :: vdw260 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "SO2", &
      kijvalue = 0.07500000  &
      )

  type (kijdatadb), parameter :: vdw261 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "H2", &
      kijvalue = 0.12100000  &
      )

  type (kijdatadb), parameter :: vdw262 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "CO", &
      kijvalue = -0.05900000  &
      )

  type (kijdatadb), parameter :: vdw263 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NH3", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw264 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC4", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw265 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC4", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw266 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC5", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw267 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC6", &
      kijvalue = 0.05000000  &
      )

  type (kijdatadb), parameter :: vdw268 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC7", &
      kijvalue = 0.04000000  &
      )

  type (kijdatadb), parameter :: vdw269 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC8", &
      kijvalue = 0.04000000  &
      )

  type (kijdatadb), parameter :: vdw270 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC9", &
      kijvalue = 0.03000000  &
      )

  type (kijdatadb), parameter :: vdw271 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MEG", &
      uid2 = "CO2", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw272 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MEG", &
      uid2 = "H2O", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw273 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MEG", &
      uid2 = "C1", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw274 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MEOH", &
      uid2 = "CO2", &
      kijvalue = 0.01700000  &
      )

  type (kijdatadb), parameter :: vdw275 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C1", &
      kijvalue = 0.03800000  &
      )

  type (kijdatadb), parameter :: vdw276 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C2", &
      kijvalue = 0.04100000  &
      )

  type (kijdatadb), parameter :: vdw277 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C2_1", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw278 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C3", &
      kijvalue = 0.07600000  &
      )

  type (kijdatadb), parameter :: vdw279 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "IC4", &
      kijvalue = 0.09400000  &
      )

  type (kijdatadb), parameter :: vdw280 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "IC5", &
      kijvalue = 0.08700000  &
      )

  type (kijdatadb), parameter :: vdw281 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC10", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw282 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC11", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw283 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC4", &
      kijvalue = 0.07000000  &
      )

  type (kijdatadb), parameter :: vdw284 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC5", &
      kijvalue = 0.08800000  &
      )

  type (kijdatadb), parameter :: vdw285 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC6", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw286 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC7", &
      kijvalue = 0.14200000  &
      )

  type (kijdatadb), parameter :: vdw287 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC8", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw288 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC9", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw289 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "O2", &
      kijvalue = -0.01300000  &
      )

  type (kijdatadb), parameter :: vdw290 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "BENZENE", &
      kijvalue = 0.01100000  &
      )

  type (kijdatadb), parameter :: vdw291 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R12", &
      uid2 = "R11", &
      kijvalue = 0.00540000  &
      )

  type (kijdatadb), parameter :: vdw292 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R12", &
      uid2 = "R114", &
      kijvalue = 0.00150000  &
      )

  type (kijdatadb), parameter :: vdw293 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R12", &
      uid2 = "R152a", &
      kijvalue = 0.08670000  &
      )

  type (kijdatadb), parameter :: vdw294 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13", &
      uid2 = "R11", &
      kijvalue = 0.02620000  &
      )

  type (kijdatadb), parameter :: vdw295 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13", &
      uid2 = "R113", &
      kijvalue = 0.02430000  &
      )

  type (kijdatadb), parameter :: vdw296 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13", &
      uid2 = "R12", &
      kijvalue = 0.02990000  &
      )

  type (kijdatadb), parameter :: vdw297 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13B", &
      uid2 = "R12", &
      kijvalue = -0.00320000  &
      )

  type (kijdatadb), parameter :: vdw298 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13B", &
      uid2 = "R152a", &
      kijvalue = 0.07990000  &
      )

  type (kijdatadb), parameter :: vdw299 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R14", &
      uid2 = "R13", &
      kijvalue = 0.03040000  &
      )

  type (kijdatadb), parameter :: vdw300 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R14", &
      uid2 = "R23", &
      kijvalue = 0.10080000  &
      )

  type (kijdatadb), parameter :: vdw301 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R11", &
      kijvalue = 0.04660000  &
      )

  type (kijdatadb), parameter :: vdw302 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R114", &
      kijvalue = 0.03990000  &
      )

  type (kijdatadb), parameter :: vdw303 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R12", &
      kijvalue = 0.05640000  &
      )

  type (kijdatadb), parameter :: vdw304 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R142b", &
      kijvalue = 0.00570000  &
      )

  type (kijdatadb), parameter :: vdw305 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R115", &
      kijvalue = 0.08700000  &
      )

  type (kijdatadb), parameter :: vdw306 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R23", &
      uid2 = "R13", &
      kijvalue = 0.10320000  &
      )

  type (kijdatadb), parameter :: vdw307 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R218", &
      uid2 = "R152a", &
      kijvalue = 0.12000000  &
      )

  type (kijdatadb), parameter :: vdw308 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R125", &
      uid2 = "R143a", &
      kijvalue = -0.01110000  &
      )

  type (kijdatadb), parameter :: vdw309 = &
      kijdatadb(eosid = "CSP-PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R125", &
      uid2 = "R134a", &
      kijvalue = -0.00240000  &
      )

  type (kijdatadb), parameter :: vdw310 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "H2S", &
      kijvalue = 0.09300000  &
      )

  type (kijdatadb), parameter :: vdw311 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "O2", &
      kijvalue = 0.10200000  &
      )

  type (kijdatadb), parameter :: vdw312 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NH3", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw313 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "N2O", &
      kijvalue = 0.00700000  &
      )

  type (kijdatadb), parameter :: vdw314 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "N2O4", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw315 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C1", &
      kijvalue = 0.09200000  &
      )

  type (kijdatadb), parameter :: vdw316 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C2", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw317 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C2_1", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw318 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C3", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw319 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "H2O", &
      kijvalue = 0.07400000  &
      )

  type (kijdatadb), parameter :: vdw320 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "IC4", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw321 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "IC5", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw322 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC10", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw323 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC11", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw324 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC4", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw325 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC5", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw326 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "H2S", &
      kijvalue = 0.09900000  &
      )

  type (kijdatadb), parameter :: vdw327 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "N2", &
      kijvalue = -0.03600000  &
      )

  type (kijdatadb), parameter :: vdw328 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "AR", &
      kijvalue = 0.08600000  &
      )

  type (kijdatadb), parameter :: vdw329 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "SO2", &
      kijvalue = 0.07200000  &
      )

  type (kijdatadb), parameter :: vdw330 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "H2", &
      kijvalue = 0.10400000  &
      )

  type (kijdatadb), parameter :: vdw331 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "CO", &
      kijvalue = -0.06600000  &
      )

  type (kijdatadb), parameter :: vdw332 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NH3", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw333 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "CO2", &
      kijvalue = -0.06500000  &
      )

  type (kijdatadb), parameter :: vdw334 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC4", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw335 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC4", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw336 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC5", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw337 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC6", &
      kijvalue = 0.05000000  &
      )

  type (kijdatadb), parameter :: vdw338 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC7", &
      kijvalue = 0.04000000  &
      )

  type (kijdatadb), parameter :: vdw339 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC8", &
      kijvalue = 0.04000000  &
      )

  type (kijdatadb), parameter :: vdw340 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC9", &
      kijvalue = 0.03000000  &
      )

  type (kijdatadb), parameter :: vdw341 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MEOH", &
      uid2 = "CO2", &
      kijvalue = 0.01700000  &
      )

  type (kijdatadb), parameter :: vdw342 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C1", &
      kijvalue = 0.03500000  &
      )

  type (kijdatadb), parameter :: vdw343 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C2", &
      kijvalue = 0.04100000  &
      )

  type (kijdatadb), parameter :: vdw344 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C2_1", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw345 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C3", &
      kijvalue = 0.07600000  &
      )

  type (kijdatadb), parameter :: vdw346 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "IC4", &
      kijvalue = 0.09400000  &
      )

  type (kijdatadb), parameter :: vdw347 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "IC5", &
      kijvalue = 0.08700000  &
      )

  type (kijdatadb), parameter :: vdw348 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC10", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw349 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC11", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw350 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC4", &
      kijvalue = 0.07000000  &
      )

  type (kijdatadb), parameter :: vdw351 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC5", &
      kijvalue = 0.08800000  &
      )

  type (kijdatadb), parameter :: vdw352 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC6", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw353 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC7", &
      kijvalue = 0.14200000  &
      )

  type (kijdatadb), parameter :: vdw354 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC8", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw355 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC9", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw356 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "O2", &
      kijvalue = -0.01400000  &
      )

  type (kijdatadb), parameter :: vdw357 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R12", &
      uid2 = "R11", &
      kijvalue = 0.00540000  &
      )

  type (kijdatadb), parameter :: vdw358 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R12", &
      uid2 = "R114", &
      kijvalue = 0.00150000  &
      )

  type (kijdatadb), parameter :: vdw359 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R12", &
      uid2 = "R152a", &
      kijvalue = 0.08670000  &
      )

  type (kijdatadb), parameter :: vdw360 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13", &
      uid2 = "R11", &
      kijvalue = 0.02620000  &
      )

  type (kijdatadb), parameter :: vdw361 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13", &
      uid2 = "R12", &
      kijvalue = 0.02990000  &
      )

  type (kijdatadb), parameter :: vdw362 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R14", &
      uid2 = "R13", &
      kijvalue = 0.03040000  &
      )

  type (kijdatadb), parameter :: vdw363 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R14", &
      uid2 = "R23", &
      kijvalue = 0.10080000  &
      )

  type (kijdatadb), parameter :: vdw364 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R11", &
      kijvalue = 0.04660000  &
      )

  type (kijdatadb), parameter :: vdw365 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R114", &
      kijvalue = 0.03990000  &
      )

  type (kijdatadb), parameter :: vdw366 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R12", &
      kijvalue = 0.05640000  &
      )

  type (kijdatadb), parameter :: vdw367 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R142b", &
      kijvalue = 0.00570000  &
      )

  type (kijdatadb), parameter :: vdw368 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R115", &
      kijvalue = 0.08700000  &
      )

  type (kijdatadb), parameter :: vdw369 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R23", &
      uid2 = "R13", &
      kijvalue = 0.10320000  &
      )

  type (kijdatadb), parameter :: vdw370 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R218", &
      uid2 = "R152a", &
      kijvalue = 0.12000000  &
      )

  type (kijdatadb), parameter :: vdw371 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R125", &
      uid2 = "R143a", &
      kijvalue = -0.01110000  &
      )

  type (kijdatadb), parameter :: vdw372 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R125", &
      uid2 = "R134a", &
      kijvalue = -0.00240000  &
      )

  type (kijdatadb), parameter :: vdw373 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R143a", &
      uid2 = "R134a", &
      kijvalue = 0.00130000  &
      )

  type (kijdatadb), parameter :: vdw374 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R1234yf", &
      uid2 = "R32", &
      kijvalue = 0.03700000  &
      )

  type (kijdatadb), parameter :: vdw375 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R1234yf", &
      uid2 = "R125", &
      kijvalue = 0.00400000  &
      )

  type (kijdatadb), parameter :: vdw376 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R1234yf", &
      uid2 = "R134a", &
      kijvalue = 0.02000000  &
      )

  type (kijdatadb), parameter :: vdw377 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R1234yf", &
      uid2 = "CO2", &
      kijvalue = 0.02000000  &
      )

  type (kijdatadb), parameter :: vdw378 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "C3", &
      kijvalue = 0.01400000  &
      )

  type (kijdatadb), parameter :: vdw379 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC6", &
      kijvalue = 0.04220000  &
      )

  type (kijdatadb), parameter :: vdw380 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "C2", &
      kijvalue = -0.00260000  &
      )

  type (kijdatadb), parameter :: vdw381 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "H2O", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw382 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC4", &
      kijvalue = 0.01330000  &
      )

  type (kijdatadb), parameter :: vdw383 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "IC4", &
      kijvalue = 0.02560000  &
      )

  type (kijdatadb), parameter :: vdw384 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC5", &
      kijvalue = 0.02300000  &
      )

  type (kijdatadb), parameter :: vdw385 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC7", &
      kijvalue = 0.03520000  &
      )

  type (kijdatadb), parameter :: vdw386 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC8", &
      kijvalue = 0.04960000  &
      )

  type (kijdatadb), parameter :: vdw387 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "IC5", &
      kijvalue = -0.00560000  &
      )

  type (kijdatadb), parameter :: vdw388 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC6", &
      kijvalue = 0.00070000  &
      )

  type (kijdatadb), parameter :: vdw389 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "C2", &
      kijvalue = 0.00110000  &
      )

  type (kijdatadb), parameter :: vdw390 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "H2S", &
      kijvalue = 0.07500000  &
      )

  type (kijdatadb), parameter :: vdw391 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "H2O", &
      kijvalue = 0.48000000  &
      )

  type (kijdatadb), parameter :: vdw392 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC4", &
      kijvalue = 0.00330000  &
      )

  type (kijdatadb), parameter :: vdw393 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "IC4", &
      kijvalue = -0.00780000  &
      )

  type (kijdatadb), parameter :: vdw394 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC5", &
      kijvalue = 0.02670000  &
      )

  type (kijdatadb), parameter :: vdw395 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC7", &
      kijvalue = 0.00560000  &
      )

  type (kijdatadb), parameter :: vdw396 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC8", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw397 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "IC5", &
      kijvalue = 0.01110000  &
      )

  type (kijdatadb), parameter :: vdw398 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "C2", &
      kijvalue = -0.01000000  &
      )

  type (kijdatadb), parameter :: vdw399 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "CO2", &
      kijvalue = 0.11000000  &
      )

  type (kijdatadb), parameter :: vdw400 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "H2O", &
      kijvalue = 0.48000000  &
      )

  type (kijdatadb), parameter :: vdw401 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "NC4", &
      kijvalue = -0.00560000  &
      )

  type (kijdatadb), parameter :: vdw402 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "IC4", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw403 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "NC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw404 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "NC7", &
      kijvalue = -0.00780000  &
      )

  type (kijdatadb), parameter :: vdw405 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "NC8", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw406 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw407 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "H2S", &
      kijvalue = 0.08500000  &
      )

  type (kijdatadb), parameter :: vdw408 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "H2O", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw409 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC4", &
      kijvalue = 0.00960000  &
      )

  type (kijdatadb), parameter :: vdw410 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "IC4", &
      kijvalue = -0.00670000  &
      )

  type (kijdatadb), parameter :: vdw411 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC5", &
      kijvalue = 0.00780000  &
      )

  type (kijdatadb), parameter :: vdw412 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC7", &
      kijvalue = 0.00740000  &
      )

  type (kijdatadb), parameter :: vdw413 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC8", &
      kijvalue = 0.01850000  &
      )

  type (kijdatadb), parameter :: vdw414 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw415 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC7", &
      kijvalue = 0.10000000  &
      )

  type (kijdatadb), parameter :: vdw416 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC8", &
      kijvalue = 0.10700000  &
      )

  type (kijdatadb), parameter :: vdw417 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "H2O", &
      kijvalue = 0.16400000  &
      )

  type (kijdatadb), parameter :: vdw418 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "IC4", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw419 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "IC5", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw420 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "NC4", &
      kijvalue = 0.48000000  &
      )

  type (kijdatadb), parameter :: vdw421 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "IC4", &
      kijvalue = 0.48000000  &
      )

  type (kijdatadb), parameter :: vdw422 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "NC5", &
      kijvalue = 0.48000000  &
      )

  type (kijdatadb), parameter :: vdw423 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "NC7", &
      kijvalue = 0.48000000  &
      )

  type (kijdatadb), parameter :: vdw424 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "NC8", &
      kijvalue = 0.48000000  &
      )

  type (kijdatadb), parameter :: vdw425 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "IC5", &
      kijvalue = 0.48000000  &
      )

  type (kijdatadb), parameter :: vdw426 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "IC4", &
      kijvalue = -0.00040000  &
      )

  type (kijdatadb), parameter :: vdw427 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "NC5", &
      kijvalue = 0.01740000  &
      )

  type (kijdatadb), parameter :: vdw428 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "NC7", &
      kijvalue = 0.00330000  &
      )

  type (kijdatadb), parameter :: vdw429 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "NC8", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw430 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw431 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "NC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw432 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "NC7", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw433 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "NC8", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw434 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw435 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "NC7", &
      kijvalue = 0.00740000  &
      )

  type (kijdatadb), parameter :: vdw436 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "NC8", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw437 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw438 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC7", &
      uid2 = "NC8", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw439 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC7", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw440 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC8", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw441 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC5", &
      uid2 = "NC5", &
      kijvalue = 0.00000123  &
      )

  type (kijdatadb), parameter :: vdw442 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "NC7", &
      kijvalue = 0.00137330  &
      )

  type (kijdatadb), parameter :: vdw443 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "NC8", &
      kijvalue = 0.00276186  &
      )

  type (kijdatadb), parameter :: vdw444 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "BENZENE", &
      kijvalue = 0.08060000  &
      )

  type (kijdatadb), parameter :: vdw445 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "TOLU", &
      kijvalue = 0.09360000  &
      )

  type (kijdatadb), parameter :: vdw446 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "EBZN", &
      kijvalue = 0.10100000  &
      )

  type (kijdatadb), parameter :: vdw447 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "MXYL", &
      kijvalue = 0.08790000  &
      )

  type (kijdatadb), parameter :: vdw448 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "OXYL", &
      kijvalue = 0.09000000  &
      )

  type (kijdatadb), parameter :: vdw449 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC9", &
      kijvalue = 0.10100000  &
      )

  type (kijdatadb), parameter :: vdw450 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "H2S", &
      kijvalue = 0.16760000  &
      )

  type (kijdatadb), parameter :: vdw451 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "BENZENE", &
      kijvalue = 0.15970000  &
      )

  type (kijdatadb), parameter :: vdw452 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "TOLU", &
      kijvalue = 0.19320000  &
      )

  type (kijdatadb), parameter :: vdw453 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "EBZN", &
      kijvalue = 0.10000000  &
      )

  type (kijdatadb), parameter :: vdw454 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "MXYL", &
      kijvalue = 0.21690000  &
      )

  type (kijdatadb), parameter :: vdw455 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "OXYL", &
      kijvalue = 0.21400000  &
      )

  type (kijdatadb), parameter :: vdw456 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "H2O", &
      kijvalue = -0.31560000  &
      )

  type (kijdatadb), parameter :: vdw457 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C3_1", &
      kijvalue = 0.06730000  &
      )

  type (kijdatadb), parameter :: vdw458 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "BENZENE", &
      kijvalue = 0.00900000  &
      )

  type (kijdatadb), parameter :: vdw459 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "TOLU", &
      kijvalue = 0.00810000  &
      )

  type (kijdatadb), parameter :: vdw460 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "EBZN", &
      kijvalue = 0.04500000  &
      )

  type (kijdatadb), parameter :: vdw461 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "MXYL", &
      kijvalue = 0.01710000  &
      )

  type (kijdatadb), parameter :: vdw462 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "OXYL", &
      kijvalue = -0.02310000  &
      )

  type (kijdatadb), parameter :: vdw463 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC10", &
      kijvalue = 0.04500000  &
      )

  type (kijdatadb), parameter :: vdw464 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC11", &
      kijvalue = 0.04500000  &
      )

  type (kijdatadb), parameter :: vdw465 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "BENZENE", &
      kijvalue = 0.04000000  &
      )

  type (kijdatadb), parameter :: vdw466 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "TOLU", &
      kijvalue = 0.06490000  &
      )

  type (kijdatadb), parameter :: vdw467 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "EBZN", &
      kijvalue = 0.02404000  &
      )

  type (kijdatadb), parameter :: vdw468 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "MXYL", &
      kijvalue = 0.04910000  &
      )

  type (kijdatadb), parameter :: vdw469 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "OXYL", &
      kijvalue = 0.05000000  &
      )

  type (kijdatadb), parameter :: vdw470 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC9", &
      kijvalue = 0.03893000  &
      )

  type (kijdatadb), parameter :: vdw471 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC10", &
      kijvalue = 0.04361000  &
      )

  type (kijdatadb), parameter :: vdw472 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC11", &
      kijvalue = 0.04799000  &
      )

  type (kijdatadb), parameter :: vdw473 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "BENZENE", &
      kijvalue = 0.02000000  &
      )

  type (kijdatadb), parameter :: vdw474 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "TOLU", &
      kijvalue = 0.03440000  &
      )

  type (kijdatadb), parameter :: vdw475 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "EBZN", &
      kijvalue = 0.01182000  &
      )

  type (kijdatadb), parameter :: vdw476 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "MXYL", &
      kijvalue = 0.02950000  &
      )

  type (kijdatadb), parameter :: vdw477 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "OXYL", &
      kijvalue = 0.03300000  &
      )

  type (kijdatadb), parameter :: vdw478 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC9", &
      kijvalue = 0.02302000  &
      )

  type (kijdatadb), parameter :: vdw479 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC10", &
      kijvalue = 0.02673000  &
      )

  type (kijdatadb), parameter :: vdw480 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC11", &
      kijvalue = 0.03026000  &
      )

  type (kijdatadb), parameter :: vdw481 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "BENZENE", &
      kijvalue = 0.02000000  &
      )

  type (kijdatadb), parameter :: vdw482 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "TOLU", &
      kijvalue = 0.03100000  &
      )

  type (kijdatadb), parameter :: vdw483 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "EBZN", &
      kijvalue = 0.00542000  &
      )

  type (kijdatadb), parameter :: vdw484 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "MXYL", &
      kijvalue = 0.03000000  &
      )

  type (kijdatadb), parameter :: vdw485 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "OXYL", &
      kijvalue = 0.03000000  &
      )

  type (kijdatadb), parameter :: vdw486 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC9", &
      kijvalue = 0.01370000  &
      )

  type (kijdatadb), parameter :: vdw487 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC10", &
      kijvalue = 0.01663000  &
      )

  type (kijdatadb), parameter :: vdw488 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC11", &
      kijvalue = 0.01948000  &
      )

  type (kijdatadb), parameter :: vdw489 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "BENZENE", &
      kijvalue = 0.00000180  &
      )

  type (kijdatadb), parameter :: vdw490 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "TOLU", &
      kijvalue = 0.00047000  &
      )

  type (kijdatadb), parameter :: vdw491 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "EBZN", &
      kijvalue = 0.00172000  &
      )

  type (kijdatadb), parameter :: vdw492 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "MXYL", &
      kijvalue = 0.00176000  &
      )

  type (kijdatadb), parameter :: vdw493 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "OXYL", &
      kijvalue = 0.00159000  &
      )

  type (kijdatadb), parameter :: vdw494 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "NC9", &
      kijvalue = 0.00725000  &
      )

  type (kijdatadb), parameter :: vdw495 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "NC10", &
      kijvalue = 0.00945000  &
      )

  type (kijdatadb), parameter :: vdw496 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "NC11", &
      kijvalue = 0.01164000  &
      )

  type (kijdatadb), parameter :: vdw497 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "BENZENE", &
      kijvalue = 0.00001000  &
      )

  type (kijdatadb), parameter :: vdw498 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "TOLU", &
      kijvalue = 0.00064000  &
      )

  type (kijdatadb), parameter :: vdw499 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "EBZN", &
      kijvalue = 0.00203000  &
      )

  type (kijdatadb), parameter :: vdw500 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "MXYL", &
      kijvalue = 0.00208000  &
      )

  type (kijdatadb), parameter :: vdw501 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "OXYL", &
      kijvalue = 0.00190000  &
      )

  type (kijdatadb), parameter :: vdw502 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "NC9", &
      kijvalue = 0.00788000  &
      )

  type (kijdatadb), parameter :: vdw503 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "NC10", &
      kijvalue = 0.01016000  &
      )

  type (kijdatadb), parameter :: vdw504 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "NC11", &
      kijvalue = 0.01243000  &
      )

  type (kijdatadb), parameter :: vdw505 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC5", &
      uid2 = "NC5", &
      kijvalue = 0.00000130  &
      )

  type (kijdatadb), parameter :: vdw506 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC5", &
      uid2 = "BENZENE", &
      kijvalue = 0.00040000  &
      )

  type (kijdatadb), parameter :: vdw507 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC5", &
      uid2 = "TOLU", &
      kijvalue = 0.00001000  &
      )

  type (kijdatadb), parameter :: vdw508 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC5", &
      uid2 = "EBZN", &
      kijvalue = 0.00052000  &
      )

  type (kijdatadb), parameter :: vdw509 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC5", &
      uid2 = "MXYL", &
      kijvalue = 0.00055000  &
      )

  type (kijdatadb), parameter :: vdw510 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC5", &
      uid2 = "OXYL", &
      kijvalue = 0.00046000  &
      )

  type (kijdatadb), parameter :: vdw511 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC5", &
      uid2 = "NC9", &
      kijvalue = 0.00445000  &
      )

  type (kijdatadb), parameter :: vdw512 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC5", &
      uid2 = "NC10", &
      kijvalue = 0.00621000  &
      )

  type (kijdatadb), parameter :: vdw513 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC5", &
      uid2 = "NC11", &
      kijvalue = 0.00801000  &
      )

  type (kijdatadb), parameter :: vdw514 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "BENZENE", &
      kijvalue = 0.01600000  &
      )

  type (kijdatadb), parameter :: vdw515 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "TOLU", &
      kijvalue = 0.00000360  &
      )

  type (kijdatadb), parameter :: vdw516 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "EBZN", &
      kijvalue = 0.00047000  &
      )

  type (kijdatadb), parameter :: vdw517 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "MXYL", &
      kijvalue = 0.00050000  &
      )

  type (kijdatadb), parameter :: vdw518 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "OXYL", &
      kijvalue = 0.00041000  &
      )

  type (kijdatadb), parameter :: vdw519 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "NC7", &
      kijvalue = 0.00137000  &
      )

  type (kijdatadb), parameter :: vdw520 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "NC8", &
      kijvalue = 0.00276000  &
      )

  type (kijdatadb), parameter :: vdw521 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "NC9", &
      kijvalue = 0.00430000  &
      )

  type (kijdatadb), parameter :: vdw522 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "NC10", &
      kijvalue = 0.00603000  &
      )

  type (kijdatadb), parameter :: vdw523 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "NC11", &
      kijvalue = 0.00781000  &
      )

  type (kijdatadb), parameter :: vdw524 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "BENZENE", &
      uid2 = "TOLU", &
      kijvalue = 0.00053000  &
      )

  type (kijdatadb), parameter :: vdw525 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "BENZENE", &
      uid2 = "EBZN", &
      kijvalue = 0.00183000  &
      )

  type (kijdatadb), parameter :: vdw526 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "BENZENE", &
      uid2 = "MXYL", &
      kijvalue = 0.00188000  &
      )

  type (kijdatadb), parameter :: vdw527 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "BENZENE", &
      uid2 = "OXYL", &
      kijvalue = 0.00170000  &
      )

  type (kijdatadb), parameter :: vdw528 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "BENZENE", &
      uid2 = "NC6", &
      kijvalue = 0.00700000  &
      )

  type (kijdatadb), parameter :: vdw529 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "BENZENE", &
      uid2 = "NC7", &
      kijvalue = -0.00200000  &
      )

  type (kijdatadb), parameter :: vdw530 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "BENZENE", &
      uid2 = "NC8", &
      kijvalue = 0.00300000  &
      )

  type (kijdatadb), parameter :: vdw531 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "BENZENE", &
      uid2 = "NC9", &
      kijvalue = 0.00749000  &
      )

  type (kijdatadb), parameter :: vdw532 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "BENZENE", &
      uid2 = "NC10", &
      kijvalue = 0.01000000  &
      )

  type (kijdatadb), parameter :: vdw533 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "BENZENE", &
      uid2 = "NC11", &
      kijvalue = 0.01193000  &
      )

  type (kijdatadb), parameter :: vdw534 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "BENZENE", &
      uid2 = "H2O", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw535 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "TOLU", &
      uid2 = "EBZN", &
      kijvalue = 0.00039000  &
      )

  type (kijdatadb), parameter :: vdw536 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "TOLU", &
      uid2 = "MXYL", &
      kijvalue = 0.00042000  &
      )

  type (kijdatadb), parameter :: vdw537 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "TOLU", &
      uid2 = "OXYL", &
      kijvalue = 0.00034000  &
      )

  type (kijdatadb), parameter :: vdw538 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "TOLU", &
      uid2 = "NC6", &
      kijvalue = 0.00032000  &
      )

  type (kijdatadb), parameter :: vdw539 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "TOLU", &
      uid2 = "NC7", &
      kijvalue = 0.00600000  &
      )

  type (kijdatadb), parameter :: vdw540 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "TOLU", &
      uid2 = "NC8", &
      kijvalue = 0.01000000  &
      )

  type (kijdatadb), parameter :: vdw541 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "TOLU", &
      uid2 = "NC9", &
      kijvalue = 0.00406000  &
      )

  type (kijdatadb), parameter :: vdw542 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "TOLU", &
      uid2 = "NC10", &
      kijvalue = 0.01000000  &
      )

  type (kijdatadb), parameter :: vdw543 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "TOLU", &
      uid2 = "NC11", &
      kijvalue = 0.00749000  &
      )

  type (kijdatadb), parameter :: vdw544 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "TOLU", &
      uid2 = "H2O", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw545 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "EBZN", &
      uid2 = "MXYL", &
      kijvalue = 0.00000020  &
      )

  type (kijdatadb), parameter :: vdw546 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "EBZN", &
      uid2 = "OXYL", &
      kijvalue = 0.00000020  &
      )

  type (kijdatadb), parameter :: vdw547 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "EBZN", &
      uid2 = "NC6", &
      kijvalue = 0.00000400  &
      )

  type (kijdatadb), parameter :: vdw548 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "EBZN", &
      uid2 = "NC7", &
      kijvalue = 0.00024000  &
      )

  type (kijdatadb), parameter :: vdw549 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "EBZN", &
      uid2 = "NC8", &
      kijvalue = -0.00180000  &
      )

  type (kijdatadb), parameter :: vdw550 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "EBZN", &
      uid2 = "NC9", &
      kijvalue = 0.00193000  &
      )

  type (kijdatadb), parameter :: vdw551 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "EBZN", &
      uid2 = "NC10", &
      kijvalue = 0.00314000  &
      )

  type (kijdatadb), parameter :: vdw552 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "EBZN", &
      uid2 = "NC11", &
      kijvalue = 0.00446000  &
      )

  type (kijdatadb), parameter :: vdw553 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "EBZN", &
      uid2 = "H2O", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw554 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MXYL", &
      uid2 = "OXYL", &
      kijvalue = 0.00000400  &
      )

  type (kijdatadb), parameter :: vdw555 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MXYL", &
      uid2 = "NC6", &
      kijvalue = 0.00001000  &
      )

  type (kijdatadb), parameter :: vdw556 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MXYL", &
      uid2 = "NC7", &
      kijvalue = 0.00022000  &
      )

  type (kijdatadb), parameter :: vdw557 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MXYL", &
      uid2 = "NC8", &
      kijvalue = 0.00092000  &
      )

  type (kijdatadb), parameter :: vdw558 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MXYL", &
      uid2 = "NC9", &
      kijvalue = 0.00188000  &
      )

  type (kijdatadb), parameter :: vdw559 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MXYL", &
      uid2 = "NC10", &
      kijvalue = 0.00308000  &
      )

  type (kijdatadb), parameter :: vdw560 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MXYL", &
      uid2 = "NC11", &
      kijvalue = 0.00439000  &
      )

  type (kijdatadb), parameter :: vdw561 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MXYL", &
      uid2 = "H2O", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw562 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "OXYL", &
      uid2 = "NC6", &
      kijvalue = 0.00000020  &
      )

  type (kijdatadb), parameter :: vdw563 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "OXYL", &
      uid2 = "NC7", &
      kijvalue = 0.00029000  &
      )

  type (kijdatadb), parameter :: vdw564 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "OXYL", &
      uid2 = "NC8", &
      kijvalue = 0.00105000  &
      )

  type (kijdatadb), parameter :: vdw565 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "OXYL", &
      uid2 = "NC9", &
      kijvalue = 0.00207000  &
      )

  type (kijdatadb), parameter :: vdw566 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "OXYL", &
      uid2 = "NC10", &
      kijvalue = 0.00331000  &
      )

  type (kijdatadb), parameter :: vdw567 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "OXYL", &
      uid2 = "NC11", &
      kijvalue = 0.00467000  &
      )

  type (kijdatadb), parameter :: vdw568 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "OXYL", &
      uid2 = "H2O", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw569 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "NC9", &
      kijvalue = 0.00210000  &
      )

  type (kijdatadb), parameter :: vdw570 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "NC10", &
      kijvalue = 0.00335000  &
      )

  type (kijdatadb), parameter :: vdw571 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "NC11", &
      kijvalue = 0.00472000  &
      )

  type (kijdatadb), parameter :: vdw572 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC7", &
      uid2 = "NC9", &
      kijvalue = 0.00082000  &
      )

  type (kijdatadb), parameter :: vdw573 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC7", &
      uid2 = "NC10", &
      kijvalue = 0.00166000  &
      )

  type (kijdatadb), parameter :: vdw574 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC7", &
      uid2 = "NC11", &
      kijvalue = 0.00266000  &
      )

  type (kijdatadb), parameter :: vdw575 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC8", &
      uid2 = "NC9", &
      kijvalue = 0.00017000  &
      )

  type (kijdatadb), parameter :: vdw576 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC8", &
      uid2 = "NC10", &
      kijvalue = 0.00064000  &
      )

  type (kijdatadb), parameter :: vdw577 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC8", &
      uid2 = "NC11", &
      kijvalue = 0.00130000  &
      )

  type (kijdatadb), parameter :: vdw578 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC9", &
      uid2 = "NC10", &
      kijvalue = 0.00130000  &
      )

  type (kijdatadb), parameter :: vdw579 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC9", &
      uid2 = "NC11", &
      kijvalue = 0.00053000  &
      )

  type (kijdatadb), parameter :: vdw580 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC9", &
      uid2 = "H2O", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw581 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC10", &
      uid2 = "NC11", &
      kijvalue = 0.00012000  &
      )

  type (kijdatadb), parameter :: vdw582 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC10", &
      uid2 = "H2O", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw583 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC11", &
      uid2 = "H2O", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw584 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "QuantumCubic", &
      bib_ref = "10.1016/j.fluid.2020.112790", &
      uid1 = "H2", &
      uid2 = "D2", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw585 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "QuantumCubic", &
      bib_ref = "10.1016/j.fluid.2020.112790", &
      uid1 = "H2", &
      uid2 = "NE", &
      kijvalue = 0.18000000  &
      )

  type (kijdatadb), parameter :: vdw586 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "QuantumCubic", &
      bib_ref = "10.1016/j.fluid.2020.112790", &
      uid1 = "H2", &
      uid2 = "HE", &
      kijvalue = 0.17000000  &
      )

  type (kijdatadb), parameter :: vdw587 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "QuantumCubic", &
      bib_ref = "10.1016/j.fluid.2020.112790", &
      uid1 = "HE", &
      uid2 = "NE", &
      kijvalue = -0.17000000  &
      )

  type (kijdatadb), parameter :: vdw588 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "QuantumCubic", &
      bib_ref = "10.1016/j.fluid.2020.112790", &
      uid1 = "HE", &
      uid2 = "D2", &
      kijvalue = 0.45000000  &
      )

  type (kijdatadb), parameter :: vdw589 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "QuantumCubic", &
      bib_ref = "10.1016/j.fluid.2020.112790", &
      uid1 = "NE", &
      uid2 = "D2", &
      kijvalue = 0.18000000  &
      )

  type (kijdatadb), parameter :: vdw590 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O2", &
      uid2 = "H2O", &
      kijvalue = -0.05000000  &
      )

  type (kijdatadb), parameter :: vdw591 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "tcPR-ENGINEERING", &
      bib_ref = "", &
      uid1 = "H2O2", &
      uid2 = "O2", &
      kijvalue = -0.75000000  &
      )

  type (kijdatadb), parameter :: vdw592 = &
      kijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "tcPR-ENGINEERING", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "O2", &
      kijvalue = -0.26000000  &
      )

  type (kijdatadb), parameter :: vdw593 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "C2", &
      kijvalue = -0.00780000  &
      )

  type (kijdatadb), parameter :: vdw594 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "C2_1", &
      kijvalue = 0.02000000  &
      )

  type (kijdatadb), parameter :: vdw595 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "C3", &
      kijvalue = 0.00900000  &
      )

  type (kijdatadb), parameter :: vdw596 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "IC4", &
      kijvalue = 0.02410000  &
      )

  type (kijdatadb), parameter :: vdw597 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC4", &
      kijvalue = 0.00560000  &
      )

  type (kijdatadb), parameter :: vdw598 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC5", &
      kijvalue = 0.01900000  &
      )

  type (kijdatadb), parameter :: vdw599 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "H2S", &
      kijvalue = 0.09100000  &
      )

  type (kijdatadb), parameter :: vdw600 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "C2_1", &
      kijvalue = 0.01120000  &
      )

  type (kijdatadb), parameter :: vdw601 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "C3", &
      kijvalue = -0.00220000  &
      )

  type (kijdatadb), parameter :: vdw602 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "IC4", &
      kijvalue = -0.01000000  &
      )

  type (kijdatadb), parameter :: vdw603 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC4", &
      kijvalue = 0.00670000  &
      )

  type (kijdatadb), parameter :: vdw604 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC5", &
      kijvalue = 0.00560000  &
      )

  type (kijdatadb), parameter :: vdw605 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "C2_1", &
      kijvalue = 0.10000000  &
      )

  type (kijdatadb), parameter :: vdw606 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "IC4", &
      kijvalue = -0.01000000  &
      )

  type (kijdatadb), parameter :: vdw607 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC5", &
      kijvalue = 0.02300000  &
      )

  type (kijdatadb), parameter :: vdw608 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "NC4", &
      kijvalue = 0.00110000  &
      )

  type (kijdatadb), parameter :: vdw609 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "NC5", &
      kijvalue = 0.02040000  &
      )

  type (kijdatadb), parameter :: vdw610 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "C1", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw611 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "MEG", &
      kijvalue = -0.06300000  &
      )

  type (kijdatadb), parameter :: vdw612 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "MEG", &
      kijvalue = 0.05000000  &
      )

  type (kijdatadb), parameter :: vdw613 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "O2", &
      kijvalue = 0.10400000  &
      )

  type (kijdatadb), parameter :: vdw614 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NH3", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw615 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "N2O", &
      kijvalue = 0.00400000  &
      )

  type (kijdatadb), parameter :: vdw616 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "N2O4", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw617 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "CO", &
      kijvalue = -0.08000000  &
      )

  type (kijdatadb), parameter :: vdw618 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C1", &
      kijvalue = 0.09500000  &
      )

  type (kijdatadb), parameter :: vdw619 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C2", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw620 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C2_1", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw621 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "C3", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw622 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "H2O", &
      kijvalue = 0.07400000  &
      )

  type (kijdatadb), parameter :: vdw623 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "IC4", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw624 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "IC5", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw625 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC10", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw626 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC11", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw627 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC4", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw628 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC5", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw629 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "H2S", &
      kijvalue = 0.10000000  &
      )

  type (kijdatadb), parameter :: vdw630 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "N2", &
      kijvalue = -0.05100000  &
      )

  type (kijdatadb), parameter :: vdw631 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "AR", &
      kijvalue = 0.08800000  &
      )

  type (kijdatadb), parameter :: vdw632 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "SO2", &
      kijvalue = 0.07100000  &
      )

  type (kijdatadb), parameter :: vdw633 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "H2", &
      kijvalue = 0.00900000  &
      )

  type (kijdatadb), parameter :: vdw634 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC4", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw635 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC5", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw636 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC6", &
      kijvalue = 0.05000000  &
      )

  type (kijdatadb), parameter :: vdw637 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC7", &
      kijvalue = 0.04000000  &
      )

  type (kijdatadb), parameter :: vdw638 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC8", &
      kijvalue = 0.04000000  &
      )

  type (kijdatadb), parameter :: vdw639 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "NC9", &
      kijvalue = 0.03000000  &
      )

  type (kijdatadb), parameter :: vdw640 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "MEOH", &
      uid2 = "CO2", &
      kijvalue = 0.01700000  &
      )

  type (kijdatadb), parameter :: vdw641 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C1", &
      kijvalue = 0.03400000  &
      )

  type (kijdatadb), parameter :: vdw642 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C2", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw643 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C2_1", &
      kijvalue = 0.07500000  &
      )

  type (kijdatadb), parameter :: vdw644 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "C3", &
      kijvalue = 0.09000000  &
      )

  type (kijdatadb), parameter :: vdw645 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "IC4", &
      kijvalue = 0.11300000  &
      )

  type (kijdatadb), parameter :: vdw646 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "IC5", &
      kijvalue = 0.08700000  &
      )

  type (kijdatadb), parameter :: vdw647 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC10", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw648 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC11", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw649 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC4", &
      kijvalue = 0.11300000  &
      )

  type (kijdatadb), parameter :: vdw650 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC5", &
      kijvalue = 0.14000000  &
      )

  type (kijdatadb), parameter :: vdw651 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC6", &
      kijvalue = 0.15000000  &
      )

  type (kijdatadb), parameter :: vdw652 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC7", &
      kijvalue = 0.14200000  &
      )

  type (kijdatadb), parameter :: vdw653 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC8", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw654 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "NC9", &
      kijvalue = 0.08000000  &
      )

  type (kijdatadb), parameter :: vdw655 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "O2", &
      kijvalue = -0.01100000  &
      )

  type (kijdatadb), parameter :: vdw656 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R12", &
      uid2 = "R11", &
      kijvalue = 0.00540000  &
      )

  type (kijdatadb), parameter :: vdw657 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R12", &
      uid2 = "R114", &
      kijvalue = 0.00150000  &
      )

  type (kijdatadb), parameter :: vdw658 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R12", &
      uid2 = "R152a", &
      kijvalue = 0.08670000  &
      )

  type (kijdatadb), parameter :: vdw659 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13", &
      uid2 = "R11", &
      kijvalue = 0.02620000  &
      )

  type (kijdatadb), parameter :: vdw660 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R13", &
      uid2 = "R12", &
      kijvalue = 0.02990000  &
      )

  type (kijdatadb), parameter :: vdw661 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R14", &
      uid2 = "R13", &
      kijvalue = 0.03040000  &
      )

  type (kijdatadb), parameter :: vdw662 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R14", &
      uid2 = "R23", &
      kijvalue = 0.10080000  &
      )

  type (kijdatadb), parameter :: vdw663 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R11", &
      kijvalue = 0.04660000  &
      )

  type (kijdatadb), parameter :: vdw664 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R114", &
      kijvalue = 0.03990000  &
      )

  type (kijdatadb), parameter :: vdw665 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R12", &
      kijvalue = 0.05640000  &
      )

  type (kijdatadb), parameter :: vdw666 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R142b", &
      kijvalue = 0.00570000  &
      )

  type (kijdatadb), parameter :: vdw667 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R22", &
      uid2 = "R115", &
      kijvalue = 0.08900000  &
      )

  type (kijdatadb), parameter :: vdw668 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R23", &
      uid2 = "R13", &
      kijvalue = 0.10320000  &
      )

  type (kijdatadb), parameter :: vdw669 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R218", &
      uid2 = "R152a", &
      kijvalue = 0.12000000  &
      )

  type (kijdatadb), parameter :: vdw670 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R125", &
      uid2 = "R143a", &
      kijvalue = -0.01110000  &
      )

  type (kijdatadb), parameter :: vdw671 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R125", &
      uid2 = "R134a", &
      kijvalue = -0.00240000  &
      )

  type (kijdatadb), parameter :: vdw672 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "R143a", &
      uid2 = "R134a", &
      kijvalue = 0.00130000  &
      )

  type (kijdatadb), parameter :: vdw673 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC6", &
      kijvalue = 0.03740000  &
      )

  type (kijdatadb), parameter :: vdw674 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC7", &
      kijvalue = 0.03070000  &
      )

  type (kijdatadb), parameter :: vdw675 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "NC8", &
      kijvalue = 0.04480000  &
      )

  type (kijdatadb), parameter :: vdw676 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C1", &
      uid2 = "IC5", &
      kijvalue = -0.00780000  &
      )

  type (kijdatadb), parameter :: vdw677 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC6", &
      kijvalue = -0.00220000  &
      )

  type (kijdatadb), parameter :: vdw678 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "H2S", &
      kijvalue = 0.07600000  &
      )

  type (kijdatadb), parameter :: vdw679 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "H2O", &
      kijvalue = 0.53000000  &
      )

  type (kijdatadb), parameter :: vdw680 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC4", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw681 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC7", &
      kijvalue = 0.00440000  &
      )

  type (kijdatadb), parameter :: vdw682 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "NC8", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw683 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C3", &
      uid2 = "IC5", &
      kijvalue = 0.00780000  &
      )

  type (kijdatadb), parameter :: vdw684 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "C2", &
      kijvalue = -0.01560000  &
      )

  type (kijdatadb), parameter :: vdw685 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "CO2", &
      kijvalue = 0.11200000  &
      )

  type (kijdatadb), parameter :: vdw686 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "H2O", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw687 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "NC4", &
      kijvalue = -0.01110000  &
      )

  type (kijdatadb), parameter :: vdw688 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "IC4", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw689 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "NC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw690 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "NC7", &
      kijvalue = -0.00110000  &
      )

  type (kijdatadb), parameter :: vdw691 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "NC8", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw692 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC6", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw693 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "H2S", &
      kijvalue = 0.08500000  &
      )

  type (kijdatadb), parameter :: vdw694 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "H2O", &
      kijvalue = 0.55000000  &
      )

  type (kijdatadb), parameter :: vdw695 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC7", &
      kijvalue = 0.00410000  &
      )

  type (kijdatadb), parameter :: vdw696 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "NC8", &
      kijvalue = 0.01700000  &
      )

  type (kijdatadb), parameter :: vdw697 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "C2", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw698 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC7", &
      kijvalue = 0.11000000  &
      )

  type (kijdatadb), parameter :: vdw699 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "CO2", &
      uid2 = "NC8", &
      kijvalue = 0.12000000  &
      )

  type (kijdatadb), parameter :: vdw700 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "N2", &
      kijvalue = 0.17000000  &
      )

  type (kijdatadb), parameter :: vdw701 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "H2O", &
      kijvalue = 0.13500000  &
      )

  type (kijdatadb), parameter :: vdw702 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "IC4", &
      kijvalue = 0.06000000  &
      )

  type (kijdatadb), parameter :: vdw703 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2S", &
      uid2 = "IC5", &
      kijvalue = 0.06500000  &
      )

  type (kijdatadb), parameter :: vdw704 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "N2", &
      uid2 = "H2O", &
      kijvalue = 0.53000000  &
      )

  type (kijdatadb), parameter :: vdw705 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "NC4", &
      kijvalue = 0.52000000  &
      )

  type (kijdatadb), parameter :: vdw706 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "IC4", &
      kijvalue = 0.52000000  &
      )

  type (kijdatadb), parameter :: vdw707 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "NC5", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw708 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "NC7", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw709 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "NC8", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw710 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "H2O", &
      uid2 = "IC5", &
      kijvalue = 0.50000000  &
      )

  type (kijdatadb), parameter :: vdw711 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "NC7", &
      kijvalue = -0.00040000  &
      )

  type (kijdatadb), parameter :: vdw712 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "NC8", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw713 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC4", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw714 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "NC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw715 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "NC7", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw716 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "NC8", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw717 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "IC4", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw718 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "NC7", &
      kijvalue = 0.00190000  &
      )

  type (kijdatadb), parameter :: vdw719 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "NC8", &
      kijvalue = -0.00220000  &
      )

  type (kijdatadb), parameter :: vdw720 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC5", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw721 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC7", &
      uid2 = "NC8", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw722 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC7", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (kijdatadb), parameter :: vdw723 = &
      kijdatadb(eosid = "SRK", &
      mruleid = "vdW", &
      ref = "Default", &
      bib_ref = "", &
      uid1 = "NC8", &
      uid2 = "IC5", &
      kijvalue = 0.00000000  &
      )

  type (interGEdatadb), parameter :: ge1 = &
      interGEdatadb(eosid = "PR", &
      mruleid = "HV1", &
      ref = "Hemmingsen2011", &
      bib_ref = "10.1016/j.fluid.2011.05.010", &
      uid1 = "H2O", &
      uid2 = "MEG", &
      kijvalue = -0.06500000, &
      correlation = 1, &
      alphaijvalue = (/4.00000000e-01, 4.00000000e-01/), &
      polyij = (/0.00000000e+00, 2.18300000e+02, 0.0/), &
      polyji = (/0.00000000e+00, 7.20200000e+01, 0.0/) &
      )

  type (interGEdatadb), parameter :: ge2 = &
      interGEdatadb(eosid = "PR", &
      mruleid = "NRTL", &
      ref = "Dicko2012", &
      bib_ref = "10.1021/je300111m", &
      uid1 = "H2S", &
      uid2 = "C3", &
      kijvalue = 0.07500000, &
      correlation = 1, &
      alphaijvalue = (/3.00000000e-01, 3.00000000e-01/), &
      polyij = (/3.45712000e+02, 0.00000000e+00, 0.00000000e+00/), &
      polyji = (/-2.93377000e+01, 0.00000000e+00, 0.00000000e+00/) &
      )

  type (interGEdatadb), parameter :: ge3 = &
      interGEdatadb(eosid = "SRK", &
      mruleid = "HV1", &
      ref = "Default", &
      bib_ref = "10.1205/cherd05023", &
      uid1 = "C1", &
      uid2 = "H2O", &
      kijvalue = 0.52000000, &
      correlation = 1, &
      alphaijvalue = (/1.50000000e-01, 1.50000000e-01/), &
      polyij = (/5.03570000e+03, -7.15000000e+00, 0.0/), &
      polyji = (/-1.62800000e+02, 2.16000000e+00, 0.0/) &
      )

  type (interGEdatadb), parameter :: ge4 = &
      interGEdatadb(eosid = "SRK", &
      mruleid = "HV2", &
      ref = "Default", &
      bib_ref = "10.1205/cherd05023", &
      uid1 = "C1", &
      uid2 = "H2O", &
      kijvalue = 0.52000000, &
      correlation = 1, &
      alphaijvalue = (/1.50000000e-01, 1.50000000e-01/), &
      polyij = (/8.40400000e+03, -2.80310000e+01, 3.14200000e-02/), &
      polyji = (/-1.14900000e+03, 8.01900000e+00, -8.61000000e-03/) &
      )

  type (interGEdatadb), parameter :: ge5 = &
      interGEdatadb(eosid = "SRK", &
      mruleid = "HV1", &
      ref = "Default", &
      bib_ref = "10.1016/j.fluid.2006.08.021", &
      uid1 = "C1", &
      uid2 = "MEG", &
      kijvalue = 0.13400000, &
      correlation = 1, &
      alphaijvalue = (/7.00000000e-02, 7.00000000e-02/), &
      polyij = (/2.27400000e+03, 0.00000000e+00, 0.0/), &
      polyji = (/1.81000000e+02, 0.00000000e+00, 0.0/) &
      )

  type (interGEdatadb), parameter :: ge6 = &
      interGEdatadb(eosid = "SRK", &
      mruleid = "HV2", &
      ref = "Default", &
      bib_ref = "10.1016/j.fluid.2006.08.021", &
      uid1 = "C1", &
      uid2 = "MEG", &
      kijvalue = 0.13400000, &
      correlation = 1, &
      alphaijvalue = (/7.00000000e-02, 7.00000000e-02/), &
      polyij = (/2.27400000e+03, 0.00000000e+00, 0.00000000e+00/), &
      polyji = (/1.81000000e+02, 0.00000000e+00, 0.00000000e+00/) &
      )

  type (interGEdatadb), parameter :: ge7 = &
      interGEdatadb(eosid = "SRK", &
      mruleid = "HV1", &
      ref = "Default", &
      bib_ref = "10.1205/cherd05023", &
      uid1 = "CO2", &
      uid2 = "H2O", &
      kijvalue = 0.19300000, &
      correlation = 1, &
      alphaijvalue = (/3.00000000e-02, 3.00000000e-02/), &
      polyij = (/6.56300000e+03, -5.12000000e+00, 0.0/), &
      polyji = (/-3.74100000e+03, 1.55000000e+00, 0.0/) &
      )

  type (interGEdatadb), parameter :: ge8 = &
      interGEdatadb(eosid = "SRK", &
      mruleid = "HV2", &
      ref = "Default", &
      bib_ref = "10.1205/cherd05023", &
      uid1 = "CO2", &
      uid2 = "H2O", &
      kijvalue = 0.19300000, &
      correlation = 1, &
      alphaijvalue = (/3.00000000e-02, 3.00000000e-02/), &
      polyij = (/5.85839000e+03, 2.41120000e+00, -1.71300000e-02/), &
      polyji = (/-1.23741000e+03, -1.55058000e+01, 2.89600000e-02/) &
      )

  type (interGEdatadb), parameter :: ge9 = &
      interGEdatadb(eosid = "SRK", &
      mruleid = "HV1", &
      ref = "Default", &
      bib_ref = "10.1016/j.fluid.2006.08.021", &
      uid1 = "H2O", &
      uid2 = "MEG", &
      kijvalue = -0.06300000, &
      correlation = 1, &
      alphaijvalue = (/9.50000000e-01, 9.50000000e-01/), &
      polyij = (/5.90000000e+01, 0.00000000e+00, 0.0/), &
      polyji = (/1.05000000e+02, 0.00000000e+00, 0.0/) &
      )

  type (interGEdatadb), parameter :: ge10 = &
      interGEdatadb(eosid = "SRK", &
      mruleid = "HV2", &
      ref = "Default", &
      bib_ref = "10.1016/j.fluid.2006.08.021", &
      uid1 = "H2O", &
      uid2 = "MEG", &
      kijvalue = -0.06300000, &
      correlation = 1, &
      alphaijvalue = (/9.50000000e-01, 9.50000000e-01/), &
      polyij = (/5.90000000e+01, 0.00000000e+00, 0.00000000e+00/), &
      polyji = (/1.05000000e+02, 0.00000000e+00, 0.00000000e+00/) &
      )

  type (lijdatadb), parameter :: lij1 = &
      lijdatadb(eosid = "PR", &
      mruleid = "vdW", &
      ref = "QuantumCubic", &
      bib_ref = "10.1016/j.fluid.2020.112790", &
      uid1 = "H2", &
      uid2 = "HE", &
      lijvalue = -0.16000000  &
      )


  integer, parameter :: maxkij =723
  type (kijdatadb), dimension(maxkij), parameter :: kijdb = (/&
      vdw1,vdw2,vdw3,vdw4,vdw5, &
      vdw6,vdw7,vdw8,vdw9,vdw10, &
      vdw11,vdw12,vdw13,vdw14,vdw15, &
      vdw16,vdw17,vdw18,vdw19,vdw20, &
      vdw21,vdw22,vdw23,vdw24,vdw25, &
      vdw26,vdw27,vdw28,vdw29,vdw30, &
      vdw31,vdw32,vdw33,vdw34,vdw35, &
      vdw36,vdw37,vdw38,vdw39,vdw40, &
      vdw41,vdw42,vdw43,vdw44,vdw45, &
      vdw46,vdw47,vdw48,vdw49,vdw50, &
      vdw51,vdw52,vdw53,vdw54,vdw55, &
      vdw56,vdw57,vdw58,vdw59,vdw60, &
      vdw61,vdw62,vdw63,vdw64,vdw65, &
      vdw66,vdw67,vdw68,vdw69,vdw70, &
      vdw71,vdw72,vdw73,vdw74,vdw75, &
      vdw76,vdw77,vdw78,vdw79,vdw80, &
      vdw81,vdw82,vdw83,vdw84,vdw85, &
      vdw86,vdw87,vdw88,vdw89,vdw90, &
      vdw91,vdw92,vdw93,vdw94,vdw95, &
      vdw96,vdw97,vdw98,vdw99,vdw100, &
      vdw101,vdw102,vdw103,vdw104,vdw105, &
      vdw106,vdw107,vdw108,vdw109,vdw110, &
      vdw111,vdw112,vdw113,vdw114,vdw115, &
      vdw116,vdw117,vdw118,vdw119,vdw120, &
      vdw121,vdw122,vdw123,vdw124,vdw125, &
      vdw126,vdw127,vdw128,vdw129,vdw130, &
      vdw131,vdw132,vdw133,vdw134,vdw135, &
      vdw136,vdw137,vdw138,vdw139,vdw140, &
      vdw141,vdw142,vdw143,vdw144,vdw145, &
      vdw146,vdw147,vdw148,vdw149,vdw150, &
      vdw151,vdw152,vdw153,vdw154,vdw155, &
      vdw156,vdw157,vdw158,vdw159,vdw160, &
      vdw161,vdw162,vdw163,vdw164,vdw165, &
      vdw166,vdw167,vdw168,vdw169,vdw170, &
      vdw171,vdw172,vdw173,vdw174,vdw175, &
      vdw176,vdw177,vdw178,vdw179,vdw180, &
      vdw181,vdw182,vdw183,vdw184,vdw185, &
      vdw186,vdw187,vdw188,vdw189,vdw190, &
      vdw191,vdw192,vdw193,vdw194,vdw195, &
      vdw196,vdw197,vdw198,vdw199,vdw200, &
      vdw201,vdw202,vdw203,vdw204,vdw205, &
      vdw206,vdw207,vdw208,vdw209,vdw210, &
      vdw211,vdw212,vdw213,vdw214,vdw215, &
      vdw216,vdw217,vdw218,vdw219,vdw220, &
      vdw221,vdw222,vdw223,vdw224,vdw225, &
      vdw226,vdw227,vdw228,vdw229,vdw230, &
      vdw231,vdw232,vdw233,vdw234,vdw235, &
      vdw236,vdw237,vdw238,vdw239,vdw240, &
      vdw241,vdw242,vdw243,vdw244,vdw245, &
      vdw246,vdw247,vdw248,vdw249,vdw250, &
      vdw251,vdw252,vdw253,vdw254,vdw255, &
      vdw256,vdw257,vdw258,vdw259,vdw260, &
      vdw261,vdw262,vdw263,vdw264,vdw265, &
      vdw266,vdw267,vdw268,vdw269,vdw270, &
      vdw271,vdw272,vdw273,vdw274,vdw275, &
      vdw276,vdw277,vdw278,vdw279,vdw280, &
      vdw281,vdw282,vdw283,vdw284,vdw285, &
      vdw286,vdw287,vdw288,vdw289,vdw290, &
      vdw291,vdw292,vdw293,vdw294,vdw295, &
      vdw296,vdw297,vdw298,vdw299,vdw300, &
      vdw301,vdw302,vdw303,vdw304,vdw305, &
      vdw306,vdw307,vdw308,vdw309,vdw310, &
      vdw311,vdw312,vdw313,vdw314,vdw315, &
      vdw316,vdw317,vdw318,vdw319,vdw320, &
      vdw321,vdw322,vdw323,vdw324,vdw325, &
      vdw326,vdw327,vdw328,vdw329,vdw330, &
      vdw331,vdw332,vdw333,vdw334,vdw335, &
      vdw336,vdw337,vdw338,vdw339,vdw340, &
      vdw341,vdw342,vdw343,vdw344,vdw345, &
      vdw346,vdw347,vdw348,vdw349,vdw350, &
      vdw351,vdw352,vdw353,vdw354,vdw355, &
      vdw356,vdw357,vdw358,vdw359,vdw360, &
      vdw361,vdw362,vdw363,vdw364,vdw365, &
      vdw366,vdw367,vdw368,vdw369,vdw370, &
      vdw371,vdw372,vdw373,vdw374,vdw375, &
      vdw376,vdw377,vdw378,vdw379,vdw380, &
      vdw381,vdw382,vdw383,vdw384,vdw385, &
      vdw386,vdw387,vdw388,vdw389,vdw390, &
      vdw391,vdw392,vdw393,vdw394,vdw395, &
      vdw396,vdw397,vdw398,vdw399,vdw400, &
      vdw401,vdw402,vdw403,vdw404,vdw405, &
      vdw406,vdw407,vdw408,vdw409,vdw410, &
      vdw411,vdw412,vdw413,vdw414,vdw415, &
      vdw416,vdw417,vdw418,vdw419,vdw420, &
      vdw421,vdw422,vdw423,vdw424,vdw425, &
      vdw426,vdw427,vdw428,vdw429,vdw430, &
      vdw431,vdw432,vdw433,vdw434,vdw435, &
      vdw436,vdw437,vdw438,vdw439,vdw440, &
      vdw441,vdw442,vdw443,vdw444,vdw445, &
      vdw446,vdw447,vdw448,vdw449,vdw450, &
      vdw451,vdw452,vdw453,vdw454,vdw455, &
      vdw456,vdw457,vdw458,vdw459,vdw460, &
      vdw461,vdw462,vdw463,vdw464,vdw465, &
      vdw466,vdw467,vdw468,vdw469,vdw470, &
      vdw471,vdw472,vdw473,vdw474,vdw475, &
      vdw476,vdw477,vdw478,vdw479,vdw480, &
      vdw481,vdw482,vdw483,vdw484,vdw485, &
      vdw486,vdw487,vdw488,vdw489,vdw490, &
      vdw491,vdw492,vdw493,vdw494,vdw495, &
      vdw496,vdw497,vdw498,vdw499,vdw500, &
      vdw501,vdw502,vdw503,vdw504,vdw505, &
      vdw506,vdw507,vdw508,vdw509,vdw510, &
      vdw511,vdw512,vdw513,vdw514,vdw515, &
      vdw516,vdw517,vdw518,vdw519,vdw520, &
      vdw521,vdw522,vdw523,vdw524,vdw525, &
      vdw526,vdw527,vdw528,vdw529,vdw530, &
      vdw531,vdw532,vdw533,vdw534,vdw535, &
      vdw536,vdw537,vdw538,vdw539,vdw540, &
      vdw541,vdw542,vdw543,vdw544,vdw545, &
      vdw546,vdw547,vdw548,vdw549,vdw550, &
      vdw551,vdw552,vdw553,vdw554,vdw555, &
      vdw556,vdw557,vdw558,vdw559,vdw560, &
      vdw561,vdw562,vdw563,vdw564,vdw565, &
      vdw566,vdw567,vdw568,vdw569,vdw570, &
      vdw571,vdw572,vdw573,vdw574,vdw575, &
      vdw576,vdw577,vdw578,vdw579,vdw580, &
      vdw581,vdw582,vdw583,vdw584,vdw585, &
      vdw586,vdw587,vdw588,vdw589,vdw590, &
      vdw591,vdw592,vdw593,vdw594,vdw595, &
      vdw596,vdw597,vdw598,vdw599,vdw600, &
      vdw601,vdw602,vdw603,vdw604,vdw605, &
      vdw606,vdw607,vdw608,vdw609,vdw610, &
      vdw611,vdw612,vdw613,vdw614,vdw615, &
      vdw616,vdw617,vdw618,vdw619,vdw620, &
      vdw621,vdw622,vdw623,vdw624,vdw625, &
      vdw626,vdw627,vdw628,vdw629,vdw630, &
      vdw631,vdw632,vdw633,vdw634,vdw635, &
      vdw636,vdw637,vdw638,vdw639,vdw640, &
      vdw641,vdw642,vdw643,vdw644,vdw645, &
      vdw646,vdw647,vdw648,vdw649,vdw650, &
      vdw651,vdw652,vdw653,vdw654,vdw655, &
      vdw656,vdw657,vdw658,vdw659,vdw660, &
      vdw661,vdw662,vdw663,vdw664,vdw665, &
      vdw666,vdw667,vdw668,vdw669,vdw670, &
      vdw671,vdw672,vdw673,vdw674,vdw675, &
      vdw676,vdw677,vdw678,vdw679,vdw680, &
      vdw681,vdw682,vdw683,vdw684,vdw685, &
      vdw686,vdw687,vdw688,vdw689,vdw690, &
      vdw691,vdw692,vdw693,vdw694,vdw695, &
      vdw696,vdw697,vdw698,vdw699,vdw700, &
      vdw701,vdw702,vdw703,vdw704,vdw705, &
      vdw706,vdw707,vdw708,vdw709,vdw710, &
      vdw711,vdw712,vdw713,vdw714,vdw715, &
      vdw716,vdw717,vdw718,vdw719,vdw720, &
      vdw721,vdw722,vdw723 &
  /)

  integer, parameter :: maxinterGEij =10
  type (interGEdatadb), dimension(maxinterGEij), parameter :: interGEdb = (/&
      ge1,ge2,ge3,ge4,ge5, &
      ge6,ge7,ge8,ge9,ge10 &
  /)

  integer, parameter :: maxlij =1
  type (lijdatadb), dimension(maxlij), parameter :: lijdb = (/&
      lij1 &
  /)

end module mixdatadb
