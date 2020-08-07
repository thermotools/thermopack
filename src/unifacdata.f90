module unifacdata
  use thermopack_constants, only: ref_len, uid_len
  implicit none
  save

  type :: unifacprm
    integer :: subGrp
    integer :: mainGrp
    character (len=uid_len) :: formula
    real :: Rk
    real :: Qk
    character(len=ref_len) :: dataSource
  end type unifacprm

  integer, parameter :: nSubGroups = 117

  type (unifacprm), dimension(nSubGroups), parameter :: unifacprmdb = (/&
       unifacprm(1, 1, "CH3", 0.9011, 0.848, "Hansen 1991"), &
       unifacprm(2, 1, "CH2", 0.6744, 0.54, "Hansen 1991"), &
       unifacprm(3, 1, "CH", 0.4469, 0.228, "Hansen 1991"), &
       unifacprm(4, 1, "C1", 0.2195, 0, "Hansen 1991"), &
       unifacprm(5, 2, "CH2=CH", 1.3454, 1.176, "Hansen 1991"), &
       unifacprm(6, 2, "CH=CH", 1.1167, 0.867, "Hansen 1991"), &
       unifacprm(7, 2, "CH2=C", 1.1173, 0.988, "Hansen 1991"), &
       unifacprm(8, 2, "CH=C", 0.8886, 0.676, "Hansen 1991"), &
       unifacprm(9, 2, "C=C", 0.6605, 0.485, "Hansen 1991"), &
       unifacprm(10, 3, "ACH", 0.5313, 0.4, "Hansen 1991"), &
       unifacprm(11, 3, "AC", 0.3652, 0.12, "Hansen 1991"), &
       unifacprm(12, 4, "ACCH3", 1.2663, 0.968, "Hansen 1991"), &
       unifacprm(13, 4, "ACCH2", 1.0396, 0.66, "Hansen 1991"), &
       unifacprm(14, 4, "ACCH", 0.8121, 0.348, "Hansen 1991"), &
       unifacprm(15, 5, "OH", 1, 1.2, "Hansen 1991"), &
       unifacprm(16, 6, "CH3OH", 1.4311, 1.432, "Hansen 1991"), &
       unifacprm(17, 7, "H2O", 0.92, 1.4, "Hansen 1991"), &
       unifacprm(18, 8, "ACOH", 0.8952, 0.68, "Hansen 1991"), &
       unifacprm(19, 9, "CH3CO", 1.6724, 1.448, "Hansen 1991"), &
       !unifacprm(19, 9, "CO", 0.7713, 0.640, "Fredenslund 1975"), &
       unifacprm(20, 9, "CH2CO", 1.4457, 1.18, "Hansen 1991"), &
       unifacprm(21, 10, "CHO", 0.998, 0.948, "Hansen 1991"), &
       unifacprm(22, 11, "CH3COO", 1.9031, 1.728, "Hansen 1991"), &
       unifacprm(23, 11, "CH2COO", 1.6764, 1.42, "Hansen 1991"), &
       unifacprm(24, 12, "HCOO", 1.242, 1.188, "Hansen 1991"), &
       unifacprm(25, 13, "CH3O", 1.145, 1.088, "Hansen 1991"), &
       unifacprm(26, 13, "CH2O", 0.9183, 0.78, "Hansen 1991"), &
       unifacprm(27, 13, "CHO", 0.6908, 0.468, "Hansen 1991"), &
       unifacprm(28, 13, "THF", 0.9183, 1.1, "Hansen 1991"), &
       unifacprm(29, 14, "CH3NH2", 1.5959, 1.544, "Hansen 1991"), &
       unifacprm(30, 14, "CH2NH2", 1.3692, 1.236, "Hansen 1991"), &
       unifacprm(31, 14, "CHNH2", 1.1417, 0.924, "Hansen 1991"), &
       unifacprm(32, 15, "CH3NH", 1.4337, 1.244, "Hansen 1991"), &
       unifacprm(33, 15, "CH2NH", 1.207, 0.936, "Hansen 1991"), &
       unifacprm(34, 15, "CHNH", 0.9795, 0.624, "Hansen 1991"), &
       unifacprm(35, 16, "CH3N", 1.1865, 0.94, "Hansen 1991"), &
       unifacprm(36, 16, "CH2N", 0.9597, 0.632, "Hansen 1991"), &
       unifacprm(37, 17, "ACNH2", 1.06, 0.816, "Hansen 1991"), &
       unifacprm(38, 18, "C5H5N", 2.9993, 2.113, "Hansen 1991"), &
       unifacprm(39, 18, "C5H4N", 2.8332, 1.833, "Hansen 1991"), &
       unifacprm(40, 18, "C5H3N", 2.667, 1.553, "Hansen 1991"), &
       unifacprm(41, 19, "CH3CN", 1.8701, 1.724, "Hansen 1991"), &
       unifacprm(42, 19, "CH2CN", 1.6434, 1.416, "Hansen 1991"), &
       unifacprm(43, 20, "COOH", 1.3013, 1.224, "Hansen 1991"), &
       unifacprm(44, 20, "HCOOH", 1.528, 1.532, "Hansen 1991"), &
       unifacprm(45, 21, "CH2Cl", 1.4654, 1.264, "Hansen 1991"), &
       unifacprm(46, 21, "CHCl", 1.238, 0.952, "Hansen 1991"), &
       unifacprm(47, 21, "CCl", 1.0106, 0.724, "Hansen 1991"), &
       unifacprm(48, 22, "CH2Cl2", 2.2564, 1.998, "Hansen 1991"), &
       unifacprm(49, 22, "CHCl2", 2.0606, 1.684, "Hansen 1991"), &
       unifacprm(50, 22, "CCl2", 1.8016, 1.448, "Hansen 1991"), &
       unifacprm(51, 23, "CHCl3", 2.87, 2.41, "Hansen 1991"), &
       unifacprm(52, 23, "CCl3", 2.6401, 2.184, "Hansen 1991"), &
       unifacprm(53, 24, "CCl4", 3.39, 2.91, "Hansen 1991"), &
       unifacprm(54, 25, "ACCl", 1.1562, 0.844, "Hansen 1991"), &
       unifacprm(55, 26, "CH3NO2", 2.0086, 1.868, "Hansen 1991"), &
       unifacprm(56, 26, "CH2NO2", 1.7818, 1.56, "Hansen 1991"), &
       unifacprm(57, 26, "CHNO2", 1.5544, 1.248, "Hansen 1991"), &
       unifacprm(58, 27, "ACNO2", 1.4199, 1.104, "Hansen 1991"), &
       unifacprm(59, 28, "CS2", 2.057, 1.65, "Hansen 1991"), &
       unifacprm(60, 29, "CH3SH", 1.877, 1.676, "Hansen 1991"), &
       unifacprm(61, 29, "CH2SH", 1.651, 1.368, "Hansen 1991"), &
       unifacprm(62, 30, "Furfural", 3.168, 2.484, "Hansen 1991"), &
       unifacprm(63, 31, "DOH", 2.4088, 2.248, "Hansen 1991"), &
       unifacprm(64, 32, "I", 1.264, 0.992, "Hansen 1991"), &
       unifacprm(65, 33, "Br", 0.9492, 0.832, "Hansen 1991"), &
       unifacprm(66, 34, "CH-=C", 1.292, 1.088, "Hansen 1991"), &
       unifacprm(67, 34, "C-=C", 1.0613, 0.784, "Hansen 1991"), &
       unifacprm(68, 35, "DMSO", 2.8266, 2.472, "Hansen 1991"), &
       unifacprm(69, 36, "Acrylnitril", 2.3144, 2.052, "Hansen 1991"), &
       unifacprm(70, 37, "Cl-unifacprm(C=C)", 0.791, 0.72, "Hansen 1991"), &
       unifacprm(71, 38, "ACF", 0.6948, 0.524, "Hansen 1991"), &
       unifacprm(72, 39, "DMF", 3.0856, 2.736, "Hansen 1991"), &
       unifacprm(73, 39, "HCONunifacprm(CH2)2", 2.6322, 2.12, "Hansen 1991"), &
       unifacprm(74, 40, "CF3", 1.406, 1.38, "Hansen 1991"), &
       unifacprm(75, 40, "CF2", 1.0105, 0.92, "Hansen 1991"), &
       unifacprm(76, 40, "CF", 0.615, 0.46, "Hansen 1991"), &
       unifacprm(77, 41, "COO", 1.38, 1.2, "Hansen 1991"), &
       unifacprm(78, 42, "SiH3", 1.6035, 1.263, "Hansen 1991"), &
       unifacprm(79, 42, "SiH2", 1.4443, 1.006, "Hansen 1991"), &
       unifacprm(80, 42, "SiH2", 1.2853, 0.749, "Hansen 1991"), &
       unifacprm(81, 42, "Si", 1.047, 0.41, "Hansen 1991"), &
       unifacprm(82, 43, "SiH20", 1.4838, 1.062, "Hansen 1991"), &
       unifacprm(83, 43, "SiHO", 1.303, 0.764, "Hansen 1991"), &
       unifacprm(84, 43, "SiO", 1.1044, 0.466, "Hansen 1991"), &
       unifacprm(85, 44, "NMP", 3.981, 3.2, "Hansen 1991"), &
       unifacprm(86, 45, "CCl3F", 3.0356, 2.644, "Hansen 1991"), &
       unifacprm(87, 45, "CCl2F", 2.2287, 1.916, "Hansen 1991"), &
       unifacprm(88, 45, "HCCl2F", 2.406, 2.116, "Hansen 1991"), &
       unifacprm(89, 45, "HCClF", 1.6493, 1.416, "Hansen 1991"), &
       unifacprm(90, 45, "CClF2", 1.8174, 1.648, "Hansen 1991"), &
       unifacprm(91, 45, "HCClF2", 1.967, 1.828, "Hansen 1991"), &
       unifacprm(92, 45, "CClF3", 2.1721, 2.1, "Hansen 1991"), &
       unifacprm(93, 45, "CCl2F2", 2.6243, 2.376, "Hansen 1991"), &
       unifacprm(94, 46, "CONH2", 1.4515, 1.248, "Hansen 1991"), &
       unifacprm(95, 46, "CONHCH3", 2.1905, 1.796, "Hansen 1991"), &
       unifacprm(96, 46, "CONHCH2", 1.9637, 1.488, "Hansen 1991"), &
       unifacprm(97, 46, "CONunifacprm(CH3)2", 2.8589, 2.428, "Hansen 1991"), &
       unifacprm(98, 46, "CONCH3CH2", 2.6322, 2.12, "Hansen 1991"), &
       unifacprm(99, 46, "CONunifacprm(CH2)2", 2.4054, 1.812, "Hansen 1991"), &
       unifacprm(100, 47, "C2H5O2", 2.1226, 1.904, "Hansen 1991"), &
       unifacprm(101, 47, "C2H4O2", 1.8952, 1.592, "Hansen 1991"), &
       unifacprm(102, 48, "CH3S", 1.613, 1.368, "Hansen 1991"), &
       unifacprm(103, 48, "CH2S", 1.3863, 1.06, "Hansen 1991"), &
       unifacprm(104, 48, "CHS", 1.1589, 0.748, "Hansen 1991"), &
       unifacprm(105, 49, "MORPH", 3.474, 2.796, "Hansen 1991"), &
       unifacprm(106, 50, "C4H4S", 2.8569, 2.14, "Hansen 1991"), &
       unifacprm(107, 50, "C4H3S", 2.6908, 1.86, "Hansen 1991"), &
       unifacprm(108, 50, "C4H2S", 2.5247, 1.58, "Hansen 1991"), &
       unifacprm(109, 55, "NH3", 0.851, 0.778, "Fisher 1996 unifacprm(PSRK)"), &
       unifacprm(110, 56, "C02", 1.3, 0.982, "Holderbaum 1991 unifacprm(PSRK)"), &
       unifacprm(111, 57, "CH4", 1.129, 1.124, "Holderbaum 1991 unifacprm(PSRK)"), &
       unifacprm(112, 58, "O2", 0.733, 0.849, "Fisher 1996 unifacprm(PSRK)"), &
       unifacprm(113, 59, "Ar", 1.177, 1.116, "Fisher 1996 unifacprm(PSRK)"), &
       unifacprm(114, 60, "N2", 0.856, 0.93, "Holderbaum 1991 unifacprm(PSRK)"), &
       unifacprm(115, 61, "H2S", 1.235, 1.202, "Holderbaum 1991 unifacprm(PSRK)"), &
       unifacprm(116, 62, "H2", 0.416, 0.571, "Holderbaum 1991 unifacprm(PSRK)"), &
       unifacprm(117, 63, "CO", 0.711, 0.828, "Holderbaum 1991 (PSRK)") &
       /)

  type :: unifacComp
    character (len=20) :: formula
    character (len=20) :: uid
    integer, dimension(nSubGroups) :: v
  end type unifacComp

  integer, parameter :: nUnifacComp = 20
  type (unifacComp), dimension(nUnifacComp), parameter :: unifacCompdb = (/&
       unifacComp("C2H6", "C2", (/2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0/) ), &
       unifacComp("C3H8", "C3", (/2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0/) ), &
       unifacComp("CH4H10", "IC4", (/3, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("CH4H10", "NC4", (/2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("C5H12", "NC5", (/2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("C5H12", "IC5", (/3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("C6H14", "NC6", (/2, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("C7H16", "NC7", (/2, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("C8H18", "NC8", (/2, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("C9H20", "NC9", (/2, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("C10H22", "NC10", (/2, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("C11H24", "NC11", (/2, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("C6H6", "BENZENE", (/0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("C7H8", "TOLUENE", (/0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 1, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0/) ), &
       unifacComp("H2O", "H2O", (/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("N2", "N2", (/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0/) ), &
       unifacComp("CO2", "CO2", (/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("H2S", "H2S", (/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0/) ), &
       unifacComp("CH4", "C1", (/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0/) ), &
       unifacComp("C3H6O", "ACETONE", (/1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/) ) &
       ! unifacComp("C3H6O", "ACETONE", (/2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       ! 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       ! 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       ! 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       ! 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/) ) &
       /)


  type :: unifacUij
    integer :: mgi
    integer :: mgj
    real :: aij !< [K]
    real :: bij !< [-]
    real :: cij !< [1/K]
    character (len=ref_len) :: dataSource
  end type unifacUij

  integer, parameter :: nUnifacUij = 1603
  type (unifacUij), dimension(nUnifacUij), parameter :: unifacUijdb = (/&
       unifacUij(1, 1, 0, 0, 0, "Hansen 1991"), &
       unifacUij(2, 1, -35.36, 0, 0, "Hansen 1991"), &
       unifacUij(3, 1, -11.12, 0, 0, "Hansen 1991"), &
       unifacUij(4, 1, -69.7, 0, 0, "Hansen 1991"), &
       unifacUij(5, 1, 156.4, 0, 0, "Hansen 1991"), &
       unifacUij(6, 1, 16.51, 0, 0, "Hansen 1991"), &
       unifacUij(7, 1, 300, 0, 0, "Hansen 1991"), &
       unifacUij(8, 1, 275.8, 0, 0, "Hansen 1991"), &
       unifacUij(9, 1, 26.76, 0, 0, "Hansen 1991"), &
       !unifacUij(9, 1, 3000.0, 0, 0, "Fredenslund 1975"), &
       unifacUij(10, 1, 505.7, 0, 0, "Hansen 1991"), &
       unifacUij(11, 1, 114.8, 0, 0, "Hansen 1991"), &
       unifacUij(12, 1, 329.3, 0, 0, "Hansen 1991"), &
       unifacUij(13, 1, 83.36, 0, 0, "Hansen 1991"), &
       unifacUij(14, 1, -30.48, 0, 0, "Hansen 1991"), &
       unifacUij(15, 1, 65.33, 0, 0, "Hansen 1991"), &
       unifacUij(16, 1, -83.98, 0, 0, "Hansen 1991"), &
       unifacUij(17, 1, 1139, 0, 0, "Hansen 1991"), &
       unifacUij(18, 1, -101.6, 0, 0, "Hansen 1991"), &
       unifacUij(19, 1, 24.82, 0, 0, "Hansen 1991"), &
       unifacUij(20, 1, 315.3, 0, 0, "Hansen 1991"), &
       unifacUij(21, 1, 91.46, 0, 0, "Hansen 1991"), &
       unifacUij(22, 1, 34.01, 0, 0, "Hansen 1991"), &
       unifacUij(23, 1, 36.7, 0, 0, "Hansen 1991"), &
       unifacUij(24, 1, -78.45, 0, 0, "Hansen 1991"), &
       unifacUij(25, 1, 106.8, 0, 0, "Hansen 1991"), &
       unifacUij(26, 1, -32.69, 0, 0, "Hansen 1991"), &
       unifacUij(27, 1, 5541, 0, 0, "Hansen 1991"), &
       unifacUij(28, 1, -52.65, 0, 0, "Hansen 1991"), &
       unifacUij(29, 1, -7.481, 0, 0, "Hansen 1991"), &
       unifacUij(30, 1, -25.31, 0, 0, "Hansen 1991"), &
       unifacUij(31, 1, 140, 0, 0, "Hansen 1991"), &
       unifacUij(32, 1, 128, 0, 0, "Hansen 1991"), &
       unifacUij(33, 1, -31.52, 0, 0, "Hansen 1991"), &
       unifacUij(34, 1, -72.88, 0, 0, "Hansen 1991"), &
       unifacUij(35, 1, 50.49, 0, 0, "Hansen 1991"), &
       unifacUij(36, 1, -165.9, 0, 0, "Hansen 1991"), &
       unifacUij(37, 1, 47.41, 0, 0, "Hansen 1991"), &
       unifacUij(38, 1, -5.132, 0, 0, "Hansen 1991"), &
       unifacUij(39, 1, -31.95, 0, 0, "Hansen 1991"), &
       unifacUij(40, 1, 147.3, 0, 0, "Hansen 1991"), &
       unifacUij(41, 1, 529, 0, 0, "Hansen 1991"), &
       unifacUij(42, 1, -34.36, 0, 0, "Hansen 1991"), &
       unifacUij(43, 1, 110.2, 0, 0, "Hansen 1991"), &
       unifacUij(44, 1, 13.89, 0, 0, "Hansen 1991"), &
       unifacUij(45, 1, 30.74, 0, 0, "Hansen 1991"), &
       unifacUij(46, 1, 27.97, 0, 0, "Hansen 1991"), &
       unifacUij(47, 1, -11.92, 0, 0, "Hansen 1991"), &
       unifacUij(48, 1, 39.93, 0, 0, "Hansen 1991"), &
       unifacUij(49, 1, -23.61, 0, 0, "Hansen 1991"), &
       unifacUij(50, 1, -8.479, 0, 0, "Hansen 1991"), &
       unifacUij(52, 1, 245.21, 0, 0, "Hansen 1991"), &
       unifacUij(1, 2, 86.02, 0, 0, "Hansen 1991"), &
       unifacUij(2, 2, 0, 0, 0, "Hansen 1991"), &
       unifacUij(3, 2, 3.446, 0, 0, "Hansen 1991"), &
       unifacUij(4, 2, -113.6, 0, 0, "Hansen 1991"), &
       unifacUij(5, 2, 457, 0, 0, "Hansen 1991"), &
       unifacUij(6, 2, -12.52, 0, 0, "Hansen 1991"), &
       unifacUij(7, 2, 496.1, 0, 0, "Hansen 1991"), &
       unifacUij(8, 2, 217.5, 0, 0, "Hansen 1991"), &
       unifacUij(9, 2, 42.92, 0, 0, "Hansen 1991"), &
       unifacUij(10, 2, 56.3, 0, 0, "Hansen 1991"), &
       unifacUij(11, 2, 132.1, 0, 0, "Hansen 1991"), &
       unifacUij(12, 2, 110.4, 0, 0, "Hansen 1991"), &
       unifacUij(13, 2, 26.51, 0, 0, "Hansen 1991"), &
       unifacUij(14, 2, 1.163, 0, 0, "Hansen 1991"), &
       unifacUij(15, 2, -28.7, 0, 0, "Hansen 1991"), &
       unifacUij(16, 2, -25.38, 0, 0, "Hansen 1991"), &
       unifacUij(17, 2, 2000, 0, 0, "Hansen 1991"), &
       unifacUij(18, 2, -47.63, 0, 0, "Hansen 1991"), &
       unifacUij(19, 2, -40.62, 0, 0, "Hansen 1991"), &
       unifacUij(20, 2, 1264, 0, 0, "Hansen 1991"), &
       unifacUij(21, 2, 40.25, 0, 0, "Hansen 1991"), &
       unifacUij(22, 2, -23.5, 0, 0, "Hansen 1991"), &
       unifacUij(23, 2, 51.06, 0, 0, "Hansen 1991"), &
       unifacUij(24, 2, 160.9, 0, 0, "Hansen 1991"), &
       unifacUij(25, 2, 70.32, 0, 0, "Hansen 1991"), &
       unifacUij(26, 2, -1.996, 0, 0, "Hansen 1991"), &
       unifacUij(28, 2, 16.62, 0, 0, "Hansen 1991"), &
       unifacUij(30, 2, 82.64, 0, 0, "Hansen 1991"), &
       unifacUij(33, 2, 174.6, 0, 0, "Hansen 1991"), &
       unifacUij(34, 2, 41.38, 0, 0, "Hansen 1991"), &
       unifacUij(35, 2, 64.07, 0, 0, "Hansen 1991"), &
       unifacUij(36, 2, 573, 0, 0, "Hansen 1991"), &
       unifacUij(37, 2, 124.2, 0, 0, "Hansen 1991"), &
       unifacUij(38, 2, -131.7, 0, 0, "Hansen 1991"), &
       unifacUij(39, 2, 249, 0, 0, "Hansen 1991"), &
       unifacUij(40, 2, 62.4, 0, 0, "Hansen 1991"), &
       unifacUij(41, 2, 1397, 0, 0, "Hansen 1991"), &
       unifacUij(44, 2, -16.11, 0, 0, "Hansen 1991"), &
       unifacUij(46, 2, 9.755, 0, 0, "Hansen 1991"), &
       unifacUij(47, 2, 132.4, 0, 0, "Hansen 1991"), &
       unifacUij(48, 2, 543.6, 0, 0, "Hansen 1991"), &
       unifacUij(49, 2, 161.1, 0, 0, "Hansen 1991"), &
       unifacUij(52, 2, 384.45, 0, 0, "Hansen 1991"), &
       unifacUij(1, 3, 61.13, 0, 0, "Hansen 1991"), &
       unifacUij(2, 3, 38.81, 0, 0, "Hansen 1991"), &
       unifacUij(3, 3, 0, 0, 0, "Hansen 1991"), &
       unifacUij(4, 3, -146.8, 0, 0, "Hansen 1991"), &
       unifacUij(5, 3, 89.6, 0, 0, "Hansen 1991"), &
       unifacUij(6, 3, -50, 0, 0, "Hansen 1991"), &
       unifacUij(7, 3, 362.3, 0, 0, "Hansen 1991"), &
       unifacUij(8, 3, 25.34, 0, 0, "Hansen 1991"), &
       unifacUij(9, 3, 140.1, 0, 0, "Hansen 1991"), &
       unifacUij(10, 3, 23.39, 0, 0, "Hansen 1991"), &
       unifacUij(11, 3, 85.84, 0, 0, "Hansen 1991"), &
       unifacUij(12, 3, 18.12, 0, 0, "Hansen 1991"), &
       unifacUij(13, 3, 52.13, 0, 0, "Hansen 1991"), &
       unifacUij(14, 3, -44.85, 0, 0, "Hansen 1991"), &
       unifacUij(15, 3, -22.31, 0, 0, "Hansen 1991"), &
       unifacUij(16, 3, -223.9, 0, 0, "Hansen 1991"), &
       unifacUij(17, 3, 247.5, 0, 0, "Hansen 1991"), &
       unifacUij(18, 3, 31.87, 0, 0, "Hansen 1991"), &
       unifacUij(19, 3, -22.97, 0, 0, "Hansen 1991"), &
       unifacUij(20, 3, 62.32, 0, 0, "Hansen 1991"), &
       unifacUij(21, 3, 4.68, 0, 0, "Hansen 1991"), &
       unifacUij(22, 3, 121.3, 0, 0, "Hansen 1991"), &
       unifacUij(23, 3, 288.5, 0, 0, "Hansen 1991"), &
       unifacUij(24, 3, -4.7, 0, 0, "Hansen 1991"), &
       unifacUij(25, 3, -97.27, 0, 0, "Hansen 1991"), &
       unifacUij(26, 3, 10.38, 0, 0, "Hansen 1991"), &
       unifacUij(27, 3, 1824, 0, 0, "Hansen 1991"), &
       unifacUij(28, 3, 21.5, 0, 0, "Hansen 1991"), &
       unifacUij(29, 3, 28.41, 0, 0, "Hansen 1991"), &
       unifacUij(30, 3, 157.3, 0, 0, "Hansen 1991"), &
       unifacUij(31, 3, 221.4, 0, 0, "Hansen 1991"), &
       unifacUij(32, 3, 58.68, 0, 0, "Hansen 1991"), &
       unifacUij(33, 3, -154.2, 0, 0, "Hansen 1991"), &
       unifacUij(34, 3, -101.12, 0, 0, "Hansen 1991"), &
       unifacUij(35, 3, -2.504, 0, 0, "Hansen 1991"), &
       unifacUij(36, 3, -123.6, 0, 0, "Hansen 1991"), &
       unifacUij(37, 3, 395.8, 0, 0, "Hansen 1991"), &
       unifacUij(38, 3, -237.2, 0, 0, "Hansen 1991"), &
       unifacUij(39, 3, -133.9, 0, 0, "Hansen 1991"), &
       unifacUij(40, 3, 140.6, 0, 0, "Hansen 1991"), &
       unifacUij(41, 3, 317.6, 0, 0, "Hansen 1991"), &
       unifacUij(42, 3, 787.9, 0, 0, "Hansen 1991"), &
       unifacUij(43, 3, 234.4, 0, 0, "Hansen 1991"), &
       unifacUij(44, 3, -23.88, 0, 0, "Hansen 1991"), &
       unifacUij(45, 3, 167.9, 0, 0, "Hansen 1991"), &
       unifacUij(47, 3, -86.88, 0, 0, "Hansen 1991"), &
       unifacUij(49, 3, 142.9, 0, 0, "Hansen 1991"), &
       unifacUij(50, 3, 23.93, 0, 0, "Hansen 1991"), &
       unifacUij(52, 3, 47.05, 0, 0, "Hansen 1991"), &
       unifacUij(1, 4, 76.5, 0, 0, "Hansen 1991"), &
       unifacUij(2, 4, 74.15, 0, 0, "Hansen 1991"), &
       unifacUij(3, 4, 167, 0, 0, "Hansen 1991"), &
       unifacUij(4, 4, 0, 0, 0, "Hansen 1991"), &
       unifacUij(5, 4, 25.82, 0, 0, "Hansen 1991"), &
       unifacUij(6, 4, -44.5, 0, 0, "Hansen 1991"), &
       unifacUij(7, 4, 377.6, 0, 0, "Hansen 1991"), &
       unifacUij(8, 4, 244.2, 0, 0, "Hansen 1991"), &
       unifacUij(9, 4, 365.8, 0, 0, "Hansen 1991"), &
       unifacUij(10, 4, 106, 0, 0, "Hansen 1991"), &
       unifacUij(11, 4, -170, 0, 0, "Hansen 1991"), &
       unifacUij(12, 4, 428, 0, 0, "Hansen 1991"), &
       unifacUij(13, 4, 65.69, 0, 0, "Hansen 1991"), &
       unifacUij(14, 4, 296.4, 0, 0, "Hansen 1991"), &
       unifacUij(15, 4, 223, 0, 0, "Hansen 1991"), &
       unifacUij(16, 4, 109.9, 0, 0, "Hansen 1991"), &
       unifacUij(17, 4, 762.8, 0, 0, "Hansen 1991"), &
       unifacUij(18, 4, 49.8, 0, 0, "Hansen 1991"), &
       unifacUij(19, 4, -138.4, 0, 0, "Hansen 1991"), &
       unifacUij(20, 4, 89.86, 0, 0, "Hansen 1991"), &
       unifacUij(21, 4, 122.9, 0, 0, "Hansen 1991"), &
       unifacUij(22, 4, 140.8, 0, 0, "Hansen 1991"), &
       unifacUij(23, 4, 69.9, 0, 0, "Hansen 1991"), &
       unifacUij(24, 4, 134.7, 0, 0, "Hansen 1991"), &
       unifacUij(25, 4, 402.5, 0, 0, "Hansen 1991"), &
       unifacUij(26, 4, -97.05, 0, 0, "Hansen 1991"), &
       unifacUij(27, 4, -127.8, 0, 0, "Hansen 1991"), &
       unifacUij(28, 4, 40.68, 0, 0, "Hansen 1991"), &
       unifacUij(29, 4, 19.56, 0, 0, "Hansen 1991"), &
       unifacUij(30, 4, 128.8, 0, 0, "Hansen 1991"), &
       unifacUij(31, 4, 150.6, 0, 0, "Hansen 1991"), &
       unifacUij(32, 4, 26.41, 0, 0, "Hansen 1991"), &
       unifacUij(33, 4, 1112, 0, 0, "Hansen 1991"), &
       unifacUij(34, 4, 614.52, 0, 0, "Hansen 1991"), &
       unifacUij(35, 4, -143.2, 0, 0, "Hansen 1991"), &
       unifacUij(36, 4, 397.4, 0, 0, "Hansen 1991"), &
       unifacUij(37, 4, 419.1, 0, 0, "Hansen 1991"), &
       unifacUij(38, 4, -157.3, 0, 0, "Hansen 1991"), &
       unifacUij(39, 4, -240.2, 0, 0, "Hansen 1991"), &
       unifacUij(40, 4, 839.83, 0, 0, "Hansen 1991"), &
       unifacUij(41, 4, 615.8, 0, 0, "Hansen 1991"), &
       unifacUij(42, 4, 191.6, 0, 0, "Hansen 1991"), &
       unifacUij(43, 4, 221.8, 0, 0, "Hansen 1991"), &
       unifacUij(44, 4, 6.214, 0, 0, "Hansen 1991"), &
       unifacUij(47, 4, -19.45, 0, 0, "Hansen 1991"), &
       unifacUij(49, 4, 274.1, 0, 0, "Hansen 1991"), &
       unifacUij(50, 4, 2.845, 0, 0, "Hansen 1991"), &
       unifacUij(52, 4, 347.13, 0, 0, "Hansen 1991"), &
       unifacUij(1, 5, 986.5, 0, 0, "Hansen 1991"), &
       unifacUij(2, 5, 524.1, 0, 0, "Hansen 1991"), &
       unifacUij(3, 5, 636.1, 0, 0, "Hansen 1991"), &
       unifacUij(4, 5, 803.2, 0, 0, "Hansen 1991"), &
       unifacUij(5, 5, 0, 0, 0, "Hansen 1991"), &
       unifacUij(6, 5, 249.1, 0, 0, "Hansen 1991"), &
       unifacUij(7, 5, -229.1, 0, 0, "Hansen 1991"), &
       unifacUij(8, 5, -451.6, 0, 0, "Hansen 1991"), &
       unifacUij(9, 5, 164.5, 0, 0, "Hansen 1991"), &
       unifacUij(10, 5, 529, 0, 0, "Hansen 1991"), &
       unifacUij(11, 5, 245.4, 0, 0, "Hansen 1991"), &
       unifacUij(12, 5, 139.4, 0, 0, "Hansen 1991"), &
       unifacUij(13, 5, 237.7, 0, 0, "Hansen 1991"), &
       unifacUij(14, 5, -242.8, 0, 0, "Hansen 1991"), &
       unifacUij(15, 5, -150, 0, 0, "Hansen 1991"), &
       unifacUij(16, 5, 28.6, 0, 0, "Hansen 1991"), &
       unifacUij(17, 5, -17.4, 0, 0, "Hansen 1991"), &
       unifacUij(18, 5, -132.3, 0, 0, "Hansen 1991"), &
       unifacUij(19, 5, 185.4, 0, 0, "Hansen 1991"), &
       unifacUij(20, 5, -151, 0, 0, "Hansen 1991"), &
       unifacUij(21, 5, 562.2, 0, 0, "Hansen 1991"), &
       unifacUij(22, 5, 527.6, 0, 0, "Hansen 1991"), &
       unifacUij(23, 5, 742.1, 0, 0, "Hansen 1991"), &
       unifacUij(24, 5, 856.3, 0, 0, "Hansen 1991"), &
       unifacUij(25, 5, 325.7, 0, 0, "Hansen 1991"), &
       unifacUij(26, 5, 261.6, 0, 0, "Hansen 1991"), &
       unifacUij(27, 5, 561.6, 0, 0, "Hansen 1991"), &
       unifacUij(28, 5, 609.8, 0, 0, "Hansen 1991"), &
       unifacUij(29, 5, 461.6, 0, 0, "Hansen 1991"), &
       unifacUij(30, 5, 521.6, 0, 0, "Hansen 1991"), &
       unifacUij(31, 5, 267.6, 0, 0, "Hansen 1991"), &
       unifacUij(32, 5, 501.3, 0, 0, "Hansen 1991"), &
       unifacUij(33, 5, 524.9, 0, 0, "Hansen 1991"), &
       unifacUij(34, 5, 68.95, 0, 0, "Hansen 1991"), &
       unifacUij(35, 5, -25.87, 0, 0, "Hansen 1991"), &
       unifacUij(36, 5, 389.3, 0, 0, "Hansen 1991"), &
       unifacUij(37, 5, 738.9, 0, 0, "Hansen 1991"), &
       unifacUij(38, 5, 649.7, 0, 0, "Hansen 1991"), &
       unifacUij(39, 5, 64.16, 0, 0, "Hansen 1991"), &
       unifacUij(41, 5, 88.63, 0, 0, "Hansen 1991"), &
       unifacUij(42, 5, 1913, 0, 0, "Hansen 1991"), &
       unifacUij(43, 5, 84.85, 0, 0, "Hansen 1991"), &
       unifacUij(44, 5, 796.9, 0, 0, "Hansen 1991"), &
       unifacUij(45, 5, 794.4, 0, 0, "Hansen 1991"), &
       unifacUij(46, 5, 394.8, 0, 0, "Hansen 1991"), &
       unifacUij(47, 5, 517.5, 0, 0, "Hansen 1991"), &
       unifacUij(49, 5, -61.2, 0, 0, "Hansen 1991"), &
       unifacUij(50, 5, 682.5, 0, 0, "Hansen 1991"), &
       unifacUij(52, 5, 72.19, 0, 0, "Hansen 1991"), &
       unifacUij(1, 6, 697.2, 0, 0, "Hansen 1991"), &
       unifacUij(2, 6, 787.6, 0, 0, "Hansen 1991"), &
       unifacUij(3, 6, 637.35, 0, 0, "Hansen 1991"), &
       unifacUij(4, 6, 603.25, 0, 0, "Hansen 1991"), &
       unifacUij(5, 6, -137.1, 0, 0, "Hansen 1991"), &
       unifacUij(6, 6, 0, 0, 0, "Hansen 1991"), &
       unifacUij(7, 6, 289.6, 0, 0, "Hansen 1991"), &
       unifacUij(8, 6, -265.2, 0, 0, "Hansen 1991"), &
       unifacUij(9, 6, 108.7, 0, 0, "Hansen 1991"), &
       unifacUij(10, 6, -340.2, 0, 0, "Hansen 1991"), &
       unifacUij(11, 6, 249.63, 0, 0, "Hansen 1991"), &
       unifacUij(12, 6, 227.8, 0, 0, "Hansen 1991"), &
       unifacUij(13, 6, 238.4, 0, 0, "Hansen 1991"), &
       unifacUij(14, 6, -481.7, 0, 0, "Hansen 1991"), &
       unifacUij(15, 6, -370.3, 0, 0, "Hansen 1991"), &
       unifacUij(16, 6, -406.8, 0, 0, "Hansen 1991"), &
       unifacUij(17, 6, -118.1, 0, 0, "Hansen 1991"), &
       unifacUij(18, 6, -378.2, 0, 0, "Hansen 1991"), &
       unifacUij(19, 6, 162.6, 0, 0, "Hansen 1991"), &
       unifacUij(20, 6, 339.8, 0, 0, "Hansen 1991"), &
       unifacUij(21, 6, 529, 0, 0, "Hansen 1991"), &
       unifacUij(22, 6, 669.9, 0, 0, "Hansen 1991"), &
       unifacUij(23, 6, 649.1, 0, 0, "Hansen 1991"), &
       unifacUij(24, 6, 709.6, 0, 0, "Hansen 1991"), &
       unifacUij(25, 6, 612.8, 0, 0, "Hansen 1991"), &
       unifacUij(26, 6, 252.6, 0, 0, "Hansen 1991"), &
       unifacUij(27, 6, 511.29, 0, 0, "Hansen 1991"), &
       unifacUij(28, 6, 914.2, 0, 0, "Hansen 1991"), &
       unifacUij(29, 6, 448.6, 0, 0, "Hansen 1991"), &
       unifacUij(30, 6, 287, 0, 0, "Hansen 1991"), &
       unifacUij(31, 6, 240.8, 0, 0, "Hansen 1991"), &
       unifacUij(32, 6, 431.3, 0, 0, "Hansen 1991"), &
       unifacUij(33, 6, 494.7, 0, 0, "Hansen 1991"), &
       unifacUij(34, 6, 967.71, 0, 0, "Hansen 1991"), &
       unifacUij(35, 6, 695, 0, 0, "Hansen 1991"), &
       unifacUij(36, 6, 218.8, 0, 0, "Hansen 1991"), &
       unifacUij(37, 6, 528, 0, 0, "Hansen 1991"), &
       unifacUij(38, 6, 645.9, 0, 0, "Hansen 1991"), &
       unifacUij(39, 6, 172.2, 0, 0, "Hansen 1991"), &
       unifacUij(41, 6, 171, 0, 0, "Hansen 1991"), &
       unifacUij(45, 6, 762.7, 0, 0, "Hansen 1991"), &
       unifacUij(48, 6, 420, 0, 0, "Hansen 1991"), &
       unifacUij(49, 6, -89.24, 0, 0, "Hansen 1991"), &
       unifacUij(50, 6, 597.8, 0, 0, "Hansen 1991"), &
       unifacUij(52, 6, 265.75, 0, 0, "Hansen 1991"), &
       unifacUij(1, 7, 1318, 0, 0, "Hansen 1991"), &
       unifacUij(2, 7, 270.6, 0, 0, "Hansen 1991"), &
       unifacUij(3, 7, 903.8, 0, 0, "Hansen 1991"), &
       unifacUij(4, 7, 5695, 0, 0, "Hansen 1991"), &
       unifacUij(5, 7, 353.5, 0, 0, "Hansen 1991"), &
       unifacUij(6, 7, -181, 0, 0, "Hansen 1991"), &
       unifacUij(7, 7, 0, 0, 0, "Hansen 1991"), &
       unifacUij(8, 7, -601.8, 0, 0, "Hansen 1991"), &
       unifacUij(9, 7, 472.5, 0, 0, "Hansen 1991"), &
       unifacUij(10, 7, 480.8, 0, 0, "Hansen 1991"), &
       unifacUij(11, 7, 200.8, 0, 0, "Hansen 1991"), &
       unifacUij(12, 7, 124.63, 0, 0, "Hansen 1991"), &
       unifacUij(13, 7, -314.7, 0, 0, "Hansen 1991"), &
       unifacUij(14, 7, -330.48, 0, 0, "Hansen 1991"), &
       unifacUij(15, 7, -448.2, 0, 0, "Hansen 1991"), &
       unifacUij(16, 7, -598.8, 0, 0, "Hansen 1991"), &
       unifacUij(17, 7, -341.6, 0, 0, "Hansen 1991"), &
       unifacUij(18, 7, -332.9, 0, 0, "Hansen 1991"), &
       unifacUij(19, 7, 242.8, 0, 0, "Hansen 1991"), &
       unifacUij(20, 7, -66.17, 0, 0, "Hansen 1991"), &
       unifacUij(21, 7, 698.2, 0, 0, "Hansen 1991"), &
       unifacUij(22, 7, 708.7, 0, 0, "Hansen 1991"), &
       unifacUij(23, 7, 826.76, 0, 0, "Hansen 1991"), &
       unifacUij(24, 7, 1201, 0, 0, "Hansen 1991"), &
       unifacUij(25, 7, -274.5, 0, 0, "Hansen 1991"), &
       unifacUij(26, 7, 417.9, 0, 0, "Hansen 1991"), &
       unifacUij(27, 7, 360.7, 0, 0, "Hansen 1991"), &
       unifacUij(28, 7, 1081, 0, 0, "Hansen 1991"), &
       unifacUij(30, 7, 23.48, 0, 0, "Hansen 1991"), &
       unifacUij(31, 7, -137.4, 0, 0, "Hansen 1991"), &
       unifacUij(33, 7, 79.18, 0, 0, "Hansen 1991"), &
       unifacUij(35, 7, -240, 0, 0, "Hansen 1991"), &
       unifacUij(36, 7, 386.6, 0, 0, "Hansen 1991"), &
       unifacUij(39, 7, -287.1, 0, 0, "Hansen 1991"), &
       unifacUij(41, 7, 284.4, 0, 0, "Hansen 1991"), &
       unifacUij(42, 7, 180.2, 0, 0, "Hansen 1991"), &
       unifacUij(44, 7, 832.2, 0, 0, "Hansen 1991"), &
       unifacUij(46, 7, -509.3, 0, 0, "Hansen 1991"), &
       unifacUij(47, 7, -205.7, 0, 0, "Hansen 1991"), &
       unifacUij(49, 7, -384.3, 0, 0, "Hansen 1991"), &
       unifacUij(52, 7, 627.39, 0, 0, "Hansen 1991"), &
       unifacUij(1, 8, 1333, 0, 0, "Hansen 1991"), &
       unifacUij(2, 8, 526.1, 0, 0, "Hansen 1991"), &
       unifacUij(3, 8, 1329, 0, 0, "Hansen 1991"), &
       unifacUij(4, 8, 884.9, 0, 0, "Hansen 1991"), &
       unifacUij(5, 8, -259.7, 0, 0, "Hansen 1991"), &
       unifacUij(6, 8, -101.7, 0, 0, "Hansen 1991"), &
       unifacUij(7, 8, 324.5, 0, 0, "Hansen 1991"), &
       unifacUij(8, 8, 0, 0, 0, "Hansen 1991"), &
       unifacUij(9, 8, -133.1, 0, 0, "Hansen 1991"), &
       unifacUij(10, 8, -155.6, 0, 0, "Hansen 1991"), &
       unifacUij(11, 8, -36.72, 0, 0, "Hansen 1991"), &
       unifacUij(12, 8, -234.25, 0, 0, "Hansen 1991"), &
       unifacUij(13, 8, -178.5, 0, 0, "Hansen 1991"), &
       unifacUij(14, 8, -870.8, 0, 0, "Hansen 1991"), &
       unifacUij(17, 8, -253.1, 0, 0, "Hansen 1991"), &
       unifacUij(18, 8, -341.6, 0, 0, "Hansen 1991"), &
       unifacUij(20, 8, -11, 0, 0, "Hansen 1991"), &
       unifacUij(22, 8, 1633.5, 0, 0, "Hansen 1991"), &
       unifacUij(24, 8, 10000, 0, 0, "Hansen 1991"), &
       unifacUij(25, 8, 622.3, 0, 0, "Hansen 1991"), &
       unifacUij(27, 8, 815.12, 0, 0, "Hansen 1991"), &
       unifacUij(28, 8, 1421, 0, 0, "Hansen 1991"), &
       unifacUij(31, 8, 838.4, 0, 0, "Hansen 1991"), &
       unifacUij(41, 8, -167.3, 0, 0, "Hansen 1991"), &
       unifacUij(44, 8, -234.7, 0, 0, "Hansen 1991"), &
       unifacUij(50, 8, 810.5, 0, 0, "Hansen 1991"), &
       unifacUij(1, 9, 476.4, 0, 0, "Hansen 1991"), &
       !unifacUij(1, 9, 1565.0, 0, 0, "Fredenslund 1975"), &
       unifacUij(2, 9, 182.6, 0, 0, "Hansen 1991"), &
       unifacUij(3, 9, 25.77, 0, 0, "Hansen 1991"), &
       unifacUij(4, 9, -52.1, 0, 0, "Hansen 1991"), &
       unifacUij(5, 9, 84, 0, 0, "Hansen 1991"), &
       unifacUij(6, 9, 23.39, 0, 0, "Hansen 1991"), &
       unifacUij(7, 9, -195.4, 0, 0, "Hansen 1991"), &
       unifacUij(8, 9, -356.1, 0, 0, "Hansen 1991"), &
       unifacUij(9, 9, 0, 0, 0, "Hansen 1991"), &
       unifacUij(10, 9, 128, 0, 0, "Hansen 1991"), &
       unifacUij(11, 9, 372.2, 0, 0, "Hansen 1991"), &
       unifacUij(12, 9, 385.4, 0, 0, "Hansen 1991"), &
       unifacUij(13, 9, 191.1, 0, 0, "Hansen 1991"), &
       unifacUij(15, 9, 394.6, 0, 0, "Hansen 1991"), &
       unifacUij(16, 9, 225.3, 0, 0, "Hansen 1991"), &
       unifacUij(17, 9, -450.3, 0, 0, "Hansen 1991"), &
       unifacUij(18, 9, 29.1, 0, 0, "Hansen 1991"), &
       unifacUij(19, 9, -287.5, 0, 0, "Hansen 1991"), &
       unifacUij(20, 9, -297.8, 0, 0, "Hansen 1991"), &
       unifacUij(21, 9, 286.3, 0, 0, "Hansen 1991"), &
       unifacUij(22, 9, 82.86, 0, 0, "Hansen 1991"), &
       unifacUij(23, 9, 552.1, 0, 0, "Hansen 1991"), &
       unifacUij(24, 9, 372, 0, 0, "Hansen 1991"), &
       unifacUij(25, 9, 518.4, 0, 0, "Hansen 1991"), &
       unifacUij(26, 9, -142.6, 0, 0, "Hansen 1991"), &
       unifacUij(27, 9, -101.5, 0, 0, "Hansen 1991"), &
       unifacUij(28, 9, 303.7, 0, 0, "Hansen 1991"), &
       unifacUij(29, 9, 160.6, 0, 0, "Hansen 1991"), &
       unifacUij(30, 9, 317.5, 0, 0, "Hansen 1991"), &
       unifacUij(31, 9, 135.4, 0, 0, "Hansen 1991"), &
       unifacUij(32, 9, 138, 0, 0, "Hansen 1991"), &
       unifacUij(33, 9, -142.6, 0, 0, "Hansen 1991"), &
       unifacUij(34, 9, 443.6, 0, 0, "Hansen 1991"), &
       unifacUij(35, 9, 110.4, 0, 0, "Hansen 1991"), &
       unifacUij(36, 9, 114.55, 0, 0, "Hansen 1991"), &
       unifacUij(37, 9, -40.9, 0, 0, "Hansen 1991"), &
       unifacUij(39, 9, 97.04, 0, 0, "Hansen 1991"), &
       unifacUij(41, 9, 123.4, 0, 0, "Hansen 1991"), &
       unifacUij(42, 9, 992.4, 0, 0, "Hansen 1991"), &
       unifacUij(47, 9, 156.4, 0, 0, "Hansen 1991"), &
       unifacUij(50, 9, 278.8, 0, 0, "Hansen 1991"), &
       unifacUij(1, 10, 677, 0, 0, "Hansen 1991"), &
       unifacUij(2, 10, 448.8, 0, 0, "Hansen 1991"), &
       unifacUij(3, 10, 347.3, 0, 0, "Hansen 1991"), &
       unifacUij(4, 10, 586.6, 0, 0, "Hansen 1991"), &
       unifacUij(5, 10, -203.6, 0, 0, "Hansen 1991"), &
       unifacUij(6, 10, 306.4, 0, 0, "Hansen 1991"), &
       unifacUij(7, 10, -116, 0, 0, "Hansen 1991"), &
       unifacUij(8, 10, -271.1, 0, 0, "Hansen 1991"), &
       unifacUij(9, 10, -37.36, 0, 0, "Hansen 1991"), &
       unifacUij(10, 10, 0, 0, 0, "Hansen 1991"), &
       unifacUij(11, 10, 185.1, 0, 0, "Hansen 1991"), &
       unifacUij(12, 10, -236.5, 0, 0, "Hansen 1991"), &
       unifacUij(13, 10, -7.838, 0, 0, "Hansen 1991"), &
       unifacUij(19, 10, 224.66, 0, 0, "Hansen 1991"), &
       unifacUij(20, 10, -165.5, 0, 0, "Hansen 1991"), &
       unifacUij(21, 10, -47.51, 0, 0, "Hansen 1991"), &
       unifacUij(22, 10, 190.6, 0, 0, "Hansen 1991"), &
       unifacUij(23, 10, 242.8, 0, 0, "Hansen 1991"), &
       unifacUij(32, 10, 245.9, 0, 0, "Hansen 1991"), &
       unifacUij(34, 10, -55.87, 0, 0, "Hansen 1991"), &
       unifacUij(36, 10, 354, 0, 0, "Hansen 1991"), &
       unifacUij(37, 10, 183.8, 0, 0, "Hansen 1991"), &
       unifacUij(39, 10, 13.89, 0, 0, "Hansen 1991"), &
       unifacUij(41, 10, 577.5, 0, 0, "Hansen 1991"), &
       unifacUij(1, 11, 232.1, 0, 0, "Hansen 1991"), &
       unifacUij(2, 11, 37.85, 0, 0, "Hansen 1991"), &
       unifacUij(3, 11, 5.994, 0, 0, "Hansen 1991"), &
       unifacUij(4, 11, 5688, 0, 0, "Hansen 1991"), &
       unifacUij(5, 11, 101.1, 0, 0, "Hansen 1991"), &
       unifacUij(6, 11, -10.72, 0, 0, "Hansen 1991"), &
       unifacUij(7, 11, 72.87, 0, 0, "Hansen 1991"), &
       unifacUij(8, 11, -449.4, 0, 0, "Hansen 1991"), &
       unifacUij(9, 11, -213.7, 0, 0, "Hansen 1991"), &
       unifacUij(10, 11, -110.3, 0, 0, "Hansen 1991"), &
       unifacUij(11, 11, 0, 0, 0, "Hansen 1991"), &
       unifacUij(12, 11, 1167, 0, 0, "Hansen 1991"), &
       unifacUij(13, 11, 461.3, 0, 0, "Hansen 1991"), &
       unifacUij(15, 11, 136, 0, 0, "Hansen 1991"), &
       unifacUij(16, 11, 2889, 0, 0, "Hansen 1991"), &
       unifacUij(17, 11, -294.8, 0, 0, "Hansen 1991"), &
       unifacUij(18, 11, 8.87, 0, 0, "Hansen 1991"), &
       unifacUij(19, 11, -266.6, 0, 0, "Hansen 1991"), &
       unifacUij(20, 11, -256.3, 0, 0, "Hansen 1991"), &
       unifacUij(21, 11, 35.38, 0, 0, "Hansen 1991"), &
       unifacUij(22, 11, -132.9, 0, 0, "Hansen 1991"), &
       unifacUij(23, 11, 176.5, 0, 0, "Hansen 1991"), &
       unifacUij(24, 11, 129.5, 0, 0, "Hansen 1991"), &
       unifacUij(25, 11, -171.1, 0, 0, "Hansen 1991"), &
       unifacUij(26, 11, 129.3, 0, 0, "Hansen 1991"), &
       unifacUij(28, 11, 243.8, 0, 0, "Hansen 1991"), &
       unifacUij(30, 11, -146.3, 0, 0, "Hansen 1991"), &
       unifacUij(31, 11, 152, 0, 0, "Hansen 1991"), &
       unifacUij(32, 11, 21.92, 0, 0, "Hansen 1991"), &
       unifacUij(33, 11, 24.37, 0, 0, "Hansen 1991"), &
       unifacUij(34, 11, -111.45, 0, 0, "Hansen 1991"), &
       unifacUij(35, 11, 41.57, 0, 0, "Hansen 1991"), &
       unifacUij(36, 11, 175.5, 0, 0, "Hansen 1991"), &
       unifacUij(37, 11, 611.3, 0, 0, "Hansen 1991"), &
       unifacUij(39, 11, -82.12, 0, 0, "Hansen 1991"), &
       unifacUij(41, 11, -234.9, 0, 0, "Hansen 1991"), &
       unifacUij(47, 11, -3.444, 0, 0, "Hansen 1991"), &
       unifacUij(1, 12, 507, 0, 0, "Hansen 1991"), &
       unifacUij(2, 12, 333.5, 0, 0, "Hansen 1991"), &
       unifacUij(3, 12, 287.1, 0, 0, "Hansen 1991"), &
       unifacUij(4, 12, 197.8, 0, 0, "Hansen 1991"), &
       unifacUij(5, 12, 267.8, 0, 0, "Hansen 1991"), &
       unifacUij(6, 12, 179.7, 0, 0, "Hansen 1991"), &
       unifacUij(7, 12, 233.87, 0, 0, "Hansen 1991"), &
       unifacUij(8, 12, -32.52, 0, 0, "Hansen 1991"), &
       unifacUij(9, 12, -190.4, 0, 0, "Hansen 1991"), &
       unifacUij(10, 12, 766, 0, 0, "Hansen 1991"), &
       unifacUij(11, 12, -241.8, 0, 0, "Hansen 1991"), &
       unifacUij(12, 12, 0, 0, 0, "Hansen 1991"), &
       unifacUij(13, 12, 457.3, 0, 0, "Hansen 1991"), &
       unifacUij(18, 12, 554.4, 0, 0, "Hansen 1991"), &
       unifacUij(19, 12, 99.37, 0, 0, "Hansen 1991"), &
       unifacUij(20, 12, 193.9, 0, 0, "Hansen 1991"), &
       unifacUij(22, 12, 80.99, 0, 0, "Hansen 1991"), &
       unifacUij(23, 12, 235.6, 0, 0, "Hansen 1991"), &
       unifacUij(24, 12, 351.9, 0, 0, "Hansen 1991"), &
       unifacUij(25, 12, 383.3, 0, 0, "Hansen 1991"), &
       unifacUij(29, 12, 201.5, 0, 0, "Hansen 1991"), &
       unifacUij(33, 12, -92.26, 0, 0, "Hansen 1991"), &
       unifacUij(37, 12, 134.5, 0, 0, "Hansen 1991"), &
       unifacUij(39, 12, -116.7, 0, 0, "Hansen 1991"), &
       unifacUij(41, 12, 65.37, 0, 0, "Hansen 1991"), &
       unifacUij(1, 13, 251.5, 0, 0, "Hansen 1991"), &
       unifacUij(2, 13, 214.5, 0, 0, "Hansen 1991"), &
       unifacUij(3, 13, 32.14, 0, 0, "Hansen 1991"), &
       unifacUij(4, 13, 213.1, 0, 0, "Hansen 1991"), &
       unifacUij(5, 13, 28.06, 0, 0, "Hansen 1991"), &
       unifacUij(6, 13, -128.6, 0, 0, "Hansen 1991"), &
       unifacUij(7, 13, 540.5, 0, 0, "Hansen 1991"), &
       unifacUij(8, 13, -162.9, 0, 0, "Hansen 1991"), &
       unifacUij(9, 13, -103.6, 0, 0, "Hansen 1991"), &
       unifacUij(10, 13, 304.1, 0, 0, "Hansen 1991"), &
       unifacUij(11, 13, -235.7, 0, 0, "Hansen 1991"), &
       unifacUij(12, 13, -234, 0, 0, "Hansen 1991"), &
       unifacUij(13, 13, 0, 0, 0, "Hansen 1991"), &
       unifacUij(14, 13, 222.1, 0, 0, "Hansen 1991"), &
       unifacUij(15, 13, -56.08, 0, 0, "Hansen 1991"), &
       unifacUij(16, 13, -194.1, 0, 0, "Hansen 1991"), &
       unifacUij(17, 13, 285.36, 0, 0, "Hansen 1991"), &
       unifacUij(18, 13, -156.1, 0, 0, "Hansen 1991"), &
       unifacUij(19, 13, 38.81, 0, 0, "Hansen 1991"), &
       unifacUij(20, 13, -338.5, 0, 0, "Hansen 1991"), &
       unifacUij(21, 13, 225.4, 0, 0, "Hansen 1991"), &
       unifacUij(22, 13, -197.7, 0, 0, "Hansen 1991"), &
       unifacUij(23, 13, -20.93, 0, 0, "Hansen 1991"), &
       unifacUij(24, 13, 113.9, 0, 0, "Hansen 1991"), &
       unifacUij(25, 13, -25.15, 0, 0, "Hansen 1991"), &
       unifacUij(26, 13, -94.49, 0, 0, "Hansen 1991"), &
       unifacUij(27, 13, 220.66, 0, 0, "Hansen 1991"), &
       unifacUij(28, 13, 112.4, 0, 0, "Hansen 1991"), &
       unifacUij(29, 13, 63.71, 0, 0, "Hansen 1991"), &
       unifacUij(30, 13, -87.31, 0, 0, "Hansen 1991"), &
       unifacUij(31, 13, 9.207, 0, 0, "Hansen 1991"), &
       unifacUij(32, 13, 476.6, 0, 0, "Hansen 1991"), &
       unifacUij(33, 13, 736.4, 0, 0, "Hansen 1991"), &
       unifacUij(34, 13, 173.77, 0, 0, "Hansen 1991"), &
       unifacUij(35, 13, -93.51, 0, 0, "Hansen 1991"), &
       unifacUij(37, 13, -217.9, 0, 0, "Hansen 1991"), &
       unifacUij(38, 13, 167.1, 0, 0, "Hansen 1991"), &
       unifacUij(39, 13, -158.2, 0, 0, "Hansen 1991"), &
       unifacUij(40, 13, 278.15, 0, 0, "Hansen 1991"), &
       unifacUij(41, 13, -247.8, 0, 0, "Hansen 1991"), &
       unifacUij(42, 13, 448.5, 0, 0, "Hansen 1991"), &
       unifacUij(1, 14, 391.5, 0, 0, "Hansen 1991"), &
       unifacUij(2, 14, 240.9, 0, 0, "Hansen 1991"), &
       unifacUij(3, 14, 161.7, 0, 0, "Hansen 1991"), &
       unifacUij(4, 14, 19.02, 0, 0, "Hansen 1991"), &
       unifacUij(5, 14, 83.02, 0, 0, "Hansen 1991"), &
       unifacUij(6, 14, 359.3, 0, 0, "Hansen 1991"), &
       unifacUij(7, 14, 48.89, 0, 0, "Hansen 1991"), &
       unifacUij(8, 14, -832.97, 0, 0, "Hansen 1991"), &
       unifacUij(13, 14, -78.36, 0, 0, "Hansen 1991"), &
       unifacUij(14, 14, 0, 0, 0, "Hansen 1991"), &
       unifacUij(15, 14, 127.4, 0, 0, "Hansen 1991"), &
       unifacUij(16, 14, 38.89, 0, 0, "Hansen 1991"), &
       unifacUij(17, 14, -15.07, 0, 0, "Hansen 1991"), &
       unifacUij(19, 14, -157.3, 0, 0, "Hansen 1991"), &
       unifacUij(21, 14, 131.2, 0, 0, "Hansen 1991"), &
       unifacUij(24, 14, 261.1, 0, 0, "Hansen 1991"), &
       unifacUij(25, 14, 108.5, 0, 0, "Hansen 1991"), &
       unifacUij(29, 14, 106.7, 0, 0, "Hansen 1991"), &
       unifacUij(35, 14, -366.51, 0, 0, "Hansen 1991"), &
       unifacUij(39, 14, 49.7, 0, 0, "Hansen 1991"), &
       unifacUij(42, 14, 961.8, 0, 0, "Hansen 1991"), &
       unifacUij(43, 14, -125.2, 0, 0, "Hansen 1991"), &
       unifacUij(1, 15, 255.7, 0, 0, "Hansen 1991"), &
       unifacUij(2, 15, 163.9, 0, 0, "Hansen 1991"), &
       unifacUij(3, 15, 122.8, 0, 0, "Hansen 1991"), &
       unifacUij(4, 15, -49.29, 0, 0, "Hansen 1991"), &
       unifacUij(5, 15, 42.7, 0, 0, "Hansen 1991"), &
       unifacUij(6, 15, -20.98, 0, 0, "Hansen 1991"), &
       unifacUij(7, 15, 168, 0, 0, "Hansen 1991"), &
       unifacUij(9, 15, -174.2, 0, 0, "Hansen 1991"), &
       unifacUij(11, 15, -73.5, 0, 0, "Hansen 1991"), &
       unifacUij(13, 15, 251.5, 0, 0, "Hansen 1991"), &
       unifacUij(14, 15, -107.2, 0, 0, "Hansen 1991"), &
       unifacUij(15, 15, 0, 0, 0, "Hansen 1991"), &
       unifacUij(16, 15, 865.9, 0, 0, "Hansen 1991"), &
       unifacUij(17, 15, 64.3, 0, 0, "Hansen 1991"), &
       unifacUij(18, 15, -207.66, 0, 0, "Hansen 1991"), &
       unifacUij(19, 15, -108.5, 0, 0, "Hansen 1991"), &
       unifacUij(24, 15, 91.13, 0, 0, "Hansen 1991"), &
       unifacUij(25, 15, 102.2, 0, 0, "Hansen 1991"), &
       unifacUij(31, 15, -213.74, 0, 0, "Hansen 1991"), &
       unifacUij(38, 15, -198.8, 0, 0, "Hansen 1991"), &
       unifacUij(39, 15, 10.03, 0, 0, "Hansen 1991"), &
       unifacUij(41, 15, 284.5, 0, 0, "Hansen 1991"), &
       unifacUij(42, 15, 1464, 0, 0, "Hansen 1991"), &
       unifacUij(43, 15, 1604, 0, 0, "Hansen 1991"), &
       unifacUij(1, 16, 206.6, 0, 0, "Hansen 1991"), &
       unifacUij(2, 16, 61.11, 0, 0, "Hansen 1991"), &
       unifacUij(3, 16, 90.49, 0, 0, "Hansen 1991"), &
       unifacUij(4, 16, 23.5, 0, 0, "Hansen 1991"), &
       unifacUij(5, 16, -323, 0, 0, "Hansen 1991"), &
       unifacUij(6, 16, 53.9, 0, 0, "Hansen 1991"), &
       unifacUij(7, 16, 304, 0, 0, "Hansen 1991"), &
       unifacUij(9, 16, -169, 0, 0, "Hansen 1991"), &
       unifacUij(11, 16, -196.7, 0, 0, "Hansen 1991"), &
       unifacUij(13, 16, 5422.3, 0, 0, "Hansen 1991"), &
       unifacUij(14, 16, -41.11, 0, 0, "Hansen 1991"), &
       unifacUij(15, 16, -189.2, 0, 0, "Hansen 1991"), &
       unifacUij(16, 16, 0, 0, 0, "Hansen 1991"), &
       unifacUij(17, 16, -24.46, 0, 0, "Hansen 1991"), &
       unifacUij(19, 16, -446.86, 0, 0, "Hansen 1991"), &
       unifacUij(21, 16, 151.38, 0, 0, "Hansen 1991"), &
       unifacUij(22, 16, -141.4, 0, 0, "Hansen 1991"), &
       unifacUij(23, 16, -293.7, 0, 0, "Hansen 1991"), &
       unifacUij(24, 16, 316.9, 0, 0, "Hansen 1991"), &
       unifacUij(25, 16, 2951, 0, 0, "Hansen 1991"), &
       unifacUij(35, 16, -257.2, 0, 0, "Hansen 1991"), &
       unifacUij(38, 16, 116.5, 0, 0, "Hansen 1991"), &
       unifacUij(39, 16, -185.2, 0, 0, "Hansen 1991"), &
       unifacUij(1, 17, 920.7, 0, 0, "Hansen 1991"), &
       unifacUij(2, 17, 749.3, 0, 0, "Hansen 1991"), &
       unifacUij(3, 17, 648.2, 0, 0, "Hansen 1991"), &
       unifacUij(4, 17, 664.2, 0, 0, "Hansen 1991"), &
       unifacUij(5, 17, -52.39, 0, 0, "Hansen 1991"), &
       unifacUij(6, 17, 489.7, 0, 0, "Hansen 1991"), &
       unifacUij(7, 17, 459, 0, 0, "Hansen 1991"), &
       unifacUij(8, 17, -305.5, 0, 0, "Hansen 1991"), &
       unifacUij(9, 17, 6201, 0, 0, "Hansen 1991"), &
       unifacUij(11, 17, 475.5, 0, 0, "Hansen 1991"), &
       unifacUij(13, 17, -46.39, 0, 0, "Hansen 1991"), &
       unifacUij(14, 17, -200.7, 0, 0, "Hansen 1991"), &
       unifacUij(15, 17, 138.54, 0, 0, "Hansen 1991"), &
       unifacUij(16, 17, 287.43, 0, 0, "Hansen 1991"), &
       unifacUij(17, 17, 0, 0, 0, "Hansen 1991"), &
       unifacUij(18, 17, 117.4, 0, 0, "Hansen 1991"), &
       unifacUij(19, 17, 777.4, 0, 0, "Hansen 1991"), &
       unifacUij(20, 17, 493.8, 0, 0, "Hansen 1991"), &
       unifacUij(21, 17, 429.7, 0, 0, "Hansen 1991"), &
       unifacUij(22, 17, 140.8, 0, 0, "Hansen 1991"), &
       unifacUij(24, 17, 898.2, 0, 0, "Hansen 1991"), &
       unifacUij(25, 17, 334.9, 0, 0, "Hansen 1991"), &
       unifacUij(27, 17, 134.9, 0, 0, "Hansen 1991"), &
       unifacUij(31, 17, 192.3, 0, 0, "Hansen 1991"), &
       unifacUij(39, 17, 343.7, 0, 0, "Hansen 1991"), &
       unifacUij(41, 17, -22.1, 0, 0, "Hansen 1991"), &
       unifacUij(1, 18, 287.77, 0, 0, "Hansen 1991"), &
       unifacUij(2, 18, 280.5, 0, 0, "Hansen 1991"), &
       unifacUij(3, 18, -4.449, 0, 0, "Hansen 1991"), &
       unifacUij(4, 18, 52.8, 0, 0, "Hansen 1991"), &
       unifacUij(5, 18, 170, 0, 0, "Hansen 1991"), &
       unifacUij(6, 18, 580.5, 0, 0, "Hansen 1991"), &
       unifacUij(7, 18, 459, 0, 0, "Hansen 1991"), &
       unifacUij(8, 18, -305.5, 0, 0, "Hansen 1991"), &
       unifacUij(9, 18, 7.341, 0, 0, "Hansen 1991"), &
       unifacUij(11, 18, -0.13, 0, 0, "Hansen 1991"), &
       unifacUij(12, 18, -233.4, 0, 0, "Hansen 1991"), &
       unifacUij(13, 18, 213.2, 0, 0, "Hansen 1991"), &
       unifacUij(15, 18, 431.49, 0, 0, "Hansen 1991"), &
       unifacUij(17, 18, 89.7, 0, 0, "Hansen 1991"), &
       unifacUij(18, 18, 0, 0, 0, "Hansen 1991"), &
       unifacUij(19, 18, 134.3, 0, 0, "Hansen 1991"), &
       unifacUij(20, 18, -313.5, 0, 0, "Hansen 1991"), &
       unifacUij(22, 18, 587.3, 0, 0, "Hansen 1991"), &
       unifacUij(23, 18, 18.98, 0, 0, "Hansen 1991"), &
       unifacUij(24, 18, 368.5, 0, 0, "Hansen 1991"), &
       unifacUij(25, 18, 20.18, 0, 0, "Hansen 1991"), &
       unifacUij(27, 18, 2475, 0, 0, "Hansen 1991"), &
       unifacUij(33, 18, -42.71, 0, 0, "Hansen 1991"), &
       unifacUij(37, 18, 281.6, 0, 0, "Hansen 1991"), &
       unifacUij(38, 18, 159.8, 0, 0, "Hansen 1991"), &
       unifacUij(50, 18, 221.4, 0, 0, "Hansen 1991"), &
       unifacUij(1, 19, 597, 0, 0, "Hansen 1991"), &
       unifacUij(2, 19, 336.9, 0, 0, "Hansen 1991"), &
       unifacUij(3, 19, 212.5, 0, 0, "Hansen 1991"), &
       unifacUij(4, 19, 6096, 0, 0, "Hansen 1991"), &
       unifacUij(5, 19, 6.712, 0, 0, "Hansen 1991"), &
       unifacUij(6, 19, 53.28, 0, 0, "Hansen 1991"), &
       unifacUij(7, 19, 112.6, 0, 0, "Hansen 1991"), &
       unifacUij(9, 19, 481.7, 0, 0, "Hansen 1991"), &
       unifacUij(10, 19, -106.4, 0, 0, "Hansen 1991"), &
       unifacUij(11, 19, 494.6, 0, 0, "Hansen 1991"), &
       unifacUij(12, 19, -47.25, 0, 0, "Hansen 1991"), &
       unifacUij(13, 19, -18.51, 0, 0, "Hansen 1991"), &
       unifacUij(14, 19, 358.9, 0, 0, "Hansen 1991"), &
       unifacUij(15, 19, 147.1, 0, 0, "Hansen 1991"), &
       unifacUij(16, 19, 1255.1, 0, 0, "Hansen 1991"), &
       unifacUij(17, 19, -281.6, 0, 0, "Hansen 1991"), &
       unifacUij(18, 19, -169.7, 0, 0, "Hansen 1991"), &
       unifacUij(19, 19, 0, 0, 0, "Hansen 1991"), &
       unifacUij(20, 19, 92.07, 0, 0, "Hansen 1991"), &
       unifacUij(21, 19, 54.32, 0, 0, "Hansen 1991"), &
       unifacUij(22, 19, 258.6, 0, 0, "Hansen 1991"), &
       unifacUij(23, 19, 74.04, 0, 0, "Hansen 1991"), &
       unifacUij(24, 19, 492, 0, 0, "Hansen 1991"), &
       unifacUij(25, 19, 363.5, 0, 0, "Hansen 1991"), &
       unifacUij(26, 19, 0.2827, 0, 0, "Hansen 1991"), &
       unifacUij(28, 19, 335.7, 0, 0, "Hansen 1991"), &
       unifacUij(29, 19, 161, 0, 0, "Hansen 1991"), &
       unifacUij(31, 19, 169.6, 0, 0, "Hansen 1991"), &
       unifacUij(33, 19, 136.9, 0, 0, "Hansen 1991"), &
       unifacUij(34, 19, 329.1, 0, 0, "Hansen 1991"), &
       unifacUij(36, 19, -42.31, 0, 0, "Hansen 1991"), &
       unifacUij(37, 19, 335.2, 0, 0, "Hansen 1991"), &
       unifacUij(39, 19, 150.6, 0, 0, "Hansen 1991"), &
       unifacUij(41, 19, -61.6, 0, 0, "Hansen 1991"), &
       unifacUij(47, 19, 119.2, 0, 0, "Hansen 1991"), &
       unifacUij(1, 20, 663.5, 0, 0, "Hansen 1991"), &
       unifacUij(2, 20, 318.9, 0, 0, "Hansen 1991"), &
       unifacUij(3, 20, 537.4, 0, 0, "Hansen 1991"), &
       unifacUij(4, 20, 872.3, 0, 0, "Hansen 1991"), &
       unifacUij(5, 20, 199, 0, 0, "Hansen 1991"), &
       unifacUij(6, 20, -202, 0, 0, "Hansen 1991"), &
       unifacUij(7, 20, -14.09, 0, 0, "Hansen 1991"), &
       unifacUij(8, 20, 408.9, 0, 0, "Hansen 1991"), &
       unifacUij(9, 20, 669.4, 0, 0, "Hansen 1991"), &
       unifacUij(10, 20, 497.5, 0, 0, "Hansen 1991"), &
       unifacUij(11, 20, 660.2, 0, 0, "Hansen 1991"), &
       unifacUij(12, 20, -268.1, 0, 0, "Hansen 1991"), &
       unifacUij(13, 20, 664.6, 0, 0, "Hansen 1991"), &
       unifacUij(17, 20, -396, 0, 0, "Hansen 1991"), &
       unifacUij(18, 20, -153.7, 0, 0, "Hansen 1991"), &
       unifacUij(19, 20, 205.27, 0, 0, "Hansen 1991"), &
       unifacUij(20, 20, 0, 0, 0, "Hansen 1991"), &
       unifacUij(21, 20, 519.1, 0, 0, "Hansen 1991"), &
       unifacUij(22, 20, 543.3, 0, 0, "Hansen 1991"), &
       unifacUij(23, 20, 504.2, 0, 0, "Hansen 1991"), &
       unifacUij(24, 20, 631, 0, 0, "Hansen 1991"), &
       unifacUij(25, 20, 993.4, 0, 0, "Hansen 1991"), &
       unifacUij(30, 20, 570.6, 0, 0, "Hansen 1991"), &
       unifacUij(32, 20, 616.6, 0, 0, "Hansen 1991"), &
       unifacUij(33, 20, 5256, 0, 0, "Hansen 1991"), &
       unifacUij(35, 20, -180.2, 0, 0, "Hansen 1991"), &
       unifacUij(37, 20, 898.2, 0, 0, "Hansen 1991"), &
       unifacUij(39, 20, -97.77, 0, 0, "Hansen 1991"), &
       unifacUij(41, 20, 1179, 0, 0, "Hansen 1991"), &
       unifacUij(42, 20, 2450, 0, 0, "Hansen 1991"), &
       unifacUij(43, 20, 2496, 0, 0, "Hansen 1991"), &
       unifacUij(46, 20, -70.25, 0, 0, "Hansen 1991"), &
       unifacUij(1, 21, 35.93, 0, 0, "Hansen 1991"), &
       unifacUij(2, 21, -36.87, 0, 0, "Hansen 1991"), &
       unifacUij(3, 21, -18.81, 0, 0, "Hansen 1991"), &
       unifacUij(4, 21, -114.1, 0, 0, "Hansen 1991"), &
       unifacUij(5, 21, 75.62, 0, 0, "Hansen 1991"), &
       unifacUij(6, 21, -38.32, 0, 0, "Hansen 1991"), &
       unifacUij(7, 21, 325.4, 0, 0, "Hansen 1991"), &
       unifacUij(9, 21, -191.7, 0, 0, "Hansen 1991"), &
       unifacUij(10, 21, 751.9, 0, 0, "Hansen 1991"), &
       unifacUij(11, 21, -34.74, 0, 0, "Hansen 1991"), &
       unifacUij(13, 21, 301.1, 0, 0, "Hansen 1991"), &
       unifacUij(14, 21, -82.92, 0, 0, "Hansen 1991"), &
       unifacUij(16, 21, -182.91, 0, 0, "Hansen 1991"), &
       unifacUij(17, 21, 287, 0, 0, "Hansen 1991"), &
       unifacUij(19, 21, 4.933, 0, 0, "Hansen 1991"), &
       unifacUij(20, 21, 13.41, 0, 0, "Hansen 1991"), &
       unifacUij(21, 21, 0, 0, 0, "Hansen 1991"), &
       unifacUij(22, 21, -84.53, 0, 0, "Hansen 1991"), &
       unifacUij(23, 21, -157.1, 0, 0, "Hansen 1991"), &
       unifacUij(24, 21, 11.8, 0, 0, "Hansen 1991"), &
       unifacUij(25, 21, -129.7, 0, 0, "Hansen 1991"), &
       unifacUij(26, 21, 113, 0, 0, "Hansen 1991"), &
       unifacUij(27, 21, 1971, 0, 0, "Hansen 1991"), &
       unifacUij(28, 21, -73.09, 0, 0, "Hansen 1991"), &
       unifacUij(29, 21, -27.94, 0, 0, "Hansen 1991"), &
       unifacUij(30, 21, -39.46, 0, 0, "Hansen 1991"), &
       unifacUij(32, 21, 179.25, 0, 0, "Hansen 1991"), &
       unifacUij(33, 21, -262.3, 0, 0, "Hansen 1991"), &
       unifacUij(37, 21, 383.2, 0, 0, "Hansen 1991"), &
       unifacUij(39, 21, -55.21, 0, 0, "Hansen 1991"), &
       unifacUij(41, 21, 182.2, 0, 0, "Hansen 1991"), &
       unifacUij(1, 22, 53.76, 0, 0, "Hansen 1991"), &
       unifacUij(2, 22, 58.55, 0, 0, "Hansen 1991"), &
       unifacUij(3, 22, -144.4, 0, 0, "Hansen 1991"), &
       unifacUij(4, 22, -111, 0, 0, "Hansen 1991"), &
       unifacUij(5, 22, 65.28, 0, 0, "Hansen 1991"), &
       unifacUij(6, 22, -102.5, 0, 0, "Hansen 1991"), &
       unifacUij(7, 22, 370.4, 0, 0, "Hansen 1991"), &
       unifacUij(8, 22, 517.27, 0, 0, "Hansen 1991"), &
       unifacUij(9, 22, -130.3, 0, 0, "Hansen 1991"), &
       unifacUij(10, 22, 67.52, 0, 0, "Hansen 1991"), &
       unifacUij(11, 22, 108.9, 0, 0, "Hansen 1991"), &
       unifacUij(12, 22, 31, 0, 0, "Hansen 1991"), &
       unifacUij(13, 22, 137.8, 0, 0, "Hansen 1991"), &
       unifacUij(16, 22, -73.85, 0, 0, "Hansen 1991"), &
       unifacUij(17, 22, -111, 0, 0, "Hansen 1991"), &
       unifacUij(18, 22, -351.6, 0, 0, "Hansen 1991"), &
       unifacUij(19, 22, -152.7, 0, 0, "Hansen 1991"), &
       unifacUij(20, 22, -44.7, 0, 0, "Hansen 1991"), &
       unifacUij(21, 22, 108.3, 0, 0, "Hansen 1991"), &
       unifacUij(22, 22, 0, 0, 0, "Hansen 1991"), &
       unifacUij(23, 22, 0, 0, 0, "Hansen 1991"), &
       unifacUij(24, 22, 17.97, 0, 0, "Hansen 1991"), &
       unifacUij(25, 22, -8.309, 0, 0, "Hansen 1991"), &
       unifacUij(26, 22, -9.639, 0, 0, "Hansen 1991"), &
       unifacUij(30, 22, -116.21, 0, 0, "Hansen 1991"), &
       unifacUij(32, 22, -40.82, 0, 0, "Hansen 1991"), &
       unifacUij(33, 22, -174.5, 0, 0, "Hansen 1991"), &
       unifacUij(35, 22, -215, 0, 0, "Hansen 1991"), &
       unifacUij(37, 22, 301.9, 0, 0, "Hansen 1991"), &
       unifacUij(39, 22, 397.24, 0, 0, "Hansen 1991"), &
       unifacUij(41, 22, 305.4, 0, 0, "Hansen 1991"), &
       unifacUij(47, 22, -194.7, 0, 0, "Hansen 1991"), &
       unifacUij(1, 23, 24.9, 0, 0, "Hansen 1991"), &
       unifacUij(2, 23, -13.99, 0, 0, "Hansen 1991"), &
       unifacUij(3, 23, -231.9, 0, 0, "Hansen 1991"), &
       unifacUij(4, 23, -80.25, 0, 0, "Hansen 1991"), &
       unifacUij(5, 23, -98.12, 0, 0, "Hansen 1991"), &
       unifacUij(6, 23, -139.4, 0, 0, "Hansen 1991"), &
       unifacUij(7, 23, 353.7, 0, 0, "Hansen 1991"), &
       unifacUij(9, 23, -354.6, 0, 0, "Hansen 1991"), &
       unifacUij(10, 23, -483.7, 0, 0, "Hansen 1991"), &
       unifacUij(11, 23, -209.7, 0, 0, "Hansen 1991"), &
       unifacUij(12, 23, -126.2, 0, 0, "Hansen 1991"), &
       unifacUij(13, 23, -154.3, 0, 0, "Hansen 1991"), &
       unifacUij(16, 23, -352.9, 0, 0, "Hansen 1991"), &
       unifacUij(18, 23, -114.7, 0, 0, "Hansen 1991"), &
       unifacUij(19, 23, -15.62, 0, 0, "Hansen 1991"), &
       unifacUij(20, 23, 39.63, 0, 0, "Hansen 1991"), &
       unifacUij(21, 23, 249.2, 0, 0, "Hansen 1991"), &
       unifacUij(22, 23, 0, 0, 0, "Hansen 1991"), &
       unifacUij(23, 23, 0, 0, 0, "Hansen 1991"), &
       unifacUij(24, 23, 51.9, 0, 0, "Hansen 1991"), &
       unifacUij(25, 23, -0.2266, 0, 0, "Hansen 1991"), &
       unifacUij(28, 23, -26.06, 0, 0, "Hansen 1991"), &
       unifacUij(30, 23, 48.48, 0, 0, "Hansen 1991"), &
       unifacUij(32, 23, 21.76, 0, 0, "Hansen 1991"), &
       unifacUij(33, 23, -46.8, 0, 0, "Hansen 1991"), &
       unifacUij(35, 23, -343.6, 0, 0, "Hansen 1991"), &
       unifacUij(37, 23, -149.8, 0, 0, "Hansen 1991"), &
       unifacUij(41, 23, -193, 0, 0, "Hansen 1991"), &
       unifacUij(44, 23, -196.2, 0, 0, "Hansen 1991"), &
       unifacUij(48, 23, -363.1, 0, 0, "Hansen 1991"), &
       unifacUij(1, 24, 104.3, 0, 0, "Hansen 1991"), &
       unifacUij(2, 24, -109.7, 0, 0, "Hansen 1991"), &
       unifacUij(3, 24, 3, 0, 0, "Hansen 1991"), &
       unifacUij(4, 24, -141.3, 0, 0, "Hansen 1991"), &
       unifacUij(5, 24, 143.1, 0, 0, "Hansen 1991"), &
       unifacUij(6, 24, -44.76, 0, 0, "Hansen 1991"), &
       unifacUij(7, 24, 497.5, 0, 0, "Hansen 1991"), &
       unifacUij(8, 24, 1827, 0, 0, "Hansen 1991"), &
       unifacUij(9, 24, -39.2, 0, 0, "Hansen 1991"), &
       unifacUij(11, 24, 54.57, 0, 0, "Hansen 1991"), &
       unifacUij(12, 24, 179.7, 0, 0, "Hansen 1991"), &
       unifacUij(13, 24, 47.67, 0, 0, "Hansen 1991"), &
       unifacUij(14, 24, -99.81, 0, 0, "Hansen 1991"), &
       unifacUij(15, 24, 71.23, 0, 0, "Hansen 1991"), &
       unifacUij(16, 24, -262, 0, 0, "Hansen 1991"), &
       unifacUij(17, 24, 882, 0, 0, "Hansen 1991"), &
       unifacUij(18, 24, -205.3, 0, 0, "Hansen 1991"), &
       unifacUij(19, 24, -54.86, 0, 0, "Hansen 1991"), &
       unifacUij(20, 24, 183.4, 0, 0, "Hansen 1991"), &
       unifacUij(21, 24, 62.42, 0, 0, "Hansen 1991"), &
       unifacUij(22, 24, 56.33, 0, 0, "Hansen 1991"), &
       unifacUij(23, 24, -30.1, 0, 0, "Hansen 1991"), &
       unifacUij(24, 24, 0, 0, 0, "Hansen 1991"), &
       unifacUij(25, 24, -248.4, 0, 0, "Hansen 1991"), &
       unifacUij(26, 24, -34.68, 0, 0, "Hansen 1991"), &
       unifacUij(27, 24, 514.6, 0, 0, "Hansen 1991"), &
       unifacUij(28, 24, -60.71, 0, 0, "Hansen 1991"), &
       unifacUij(30, 24, -133.16, 0, 0, "Hansen 1991"), &
       unifacUij(32, 24, 48.49, 0, 0, "Hansen 1991"), &
       unifacUij(33, 24, 77.55, 0, 0, "Hansen 1991"), &
       unifacUij(35, 24, -58.43, 0, 0, "Hansen 1991"), &
       unifacUij(36, 24, -85.15, 0, 0, "Hansen 1991"), &
       unifacUij(37, 24, -134.2, 0, 0, "Hansen 1991"), &
       unifacUij(38, 24, -124.6, 0, 0, "Hansen 1991"), &
       unifacUij(39, 24, -186.7, 0, 0, "Hansen 1991"), &
       unifacUij(41, 24, 335.7, 0, 0, "Hansen 1991"), &
       unifacUij(43, 24, 70.81, 0, 0, "Hansen 1991"), &
       unifacUij(47, 24, 3.163, 0, 0, "Hansen 1991"), &
       unifacUij(48, 24, -11.3, 0, 0, "Hansen 1991"), &
       unifacUij(50, 24, -79.34, 0, 0, "Hansen 1991"), &
       unifacUij(52, 24, 75.04, 0, 0, "Hansen 1991"), &
       unifacUij(1, 25, 11.44, 0, 0, "Hansen 1991"), &
       unifacUij(2, 25, 100.1, 0, 0, "Hansen 1991"), &
       unifacUij(3, 25, 187, 0, 0, "Hansen 1991"), &
       unifacUij(4, 25, -211, 0, 0, "Hansen 1991"), &
       unifacUij(5, 25, 123.5, 0, 0, "Hansen 1991"), &
       unifacUij(6, 25, -28.25, 0, 0, "Hansen 1991"), &
       unifacUij(7, 25, 133.9, 0, 0, "Hansen 1991"), &
       unifacUij(8, 25, 6915, 0, 0, "Hansen 1991"), &
       unifacUij(9, 25, -119.8, 0, 0, "Hansen 1991"), &
       unifacUij(11, 25, 442.4, 0, 0, "Hansen 1991"), &
       unifacUij(12, 25, 24.28, 0, 0, "Hansen 1991"), &
       unifacUij(13, 25, 134.8, 0, 0, "Hansen 1991"), &
       unifacUij(14, 25, 30.05, 0, 0, "Hansen 1991"), &
       unifacUij(15, 25, -18.93, 0, 0, "Hansen 1991"), &
       unifacUij(16, 25, -181.9, 0, 0, "Hansen 1991"), &
       unifacUij(17, 25, 617.5, 0, 0, "Hansen 1991"), &
       unifacUij(18, 25, -2.17, 0, 0, "Hansen 1991"), &
       unifacUij(19, 25, -4.624, 0, 0, "Hansen 1991"), &
       unifacUij(20, 25, -79.08, 0, 0, "Hansen 1991"), &
       unifacUij(21, 25, 153, 0, 0, "Hansen 1991"), &
       unifacUij(22, 25, 223.1, 0, 0, "Hansen 1991"), &
       unifacUij(23, 25, 192.1, 0, 0, "Hansen 1991"), &
       unifacUij(24, 25, -75.97, 0, 0, "Hansen 1991"), &
       unifacUij(25, 25, 0, 0, 0, "Hansen 1991"), &
       unifacUij(26, 25, 132.9, 0, 0, "Hansen 1991"), &
       unifacUij(27, 25, -123.1, 0, 0, "Hansen 1991"), &
       unifacUij(33, 25, -185.3, 0, 0, "Hansen 1991"), &
       unifacUij(35, 25, -334.12, 0, 0, "Hansen 1991"), &
       unifacUij(39, 25, -374.16, 0, 0, "Hansen 1991"), &
       unifacUij(40, 25, 33.95, 0, 0, "Hansen 1991"), &
       unifacUij(41, 25, 1107, 0, 0, "Hansen 1991"), &
       unifacUij(44, 25, 161.5, 0, 0, "Hansen 1991"), &
       unifacUij(47, 25, 7.082, 0, 0, "Hansen 1991"), &
       unifacUij(1, 26, 661.5, 0, 0, "Hansen 1991"), &
       unifacUij(2, 26, 357.5, 0, 0, "Hansen 1991"), &
       unifacUij(3, 26, 168, 0, 0, "Hansen 1991"), &
       unifacUij(4, 26, 3629, 0, 0, "Hansen 1991"), &
       unifacUij(5, 26, 256.5, 0, 0, "Hansen 1991"), &
       unifacUij(6, 26, 75.14, 0, 0, "Hansen 1991"), &
       unifacUij(7, 26, 220.6, 0, 0, "Hansen 1991"), &
       unifacUij(9, 26, 137.5, 0, 0, "Hansen 1991"), &
       unifacUij(11, 26, -81.13, 0, 0, "Hansen 1991"), &
       unifacUij(13, 26, 95.18, 0, 0, "Hansen 1991"), &
       unifacUij(19, 26, -0.515, 0, 0, "Hansen 1991"), &
       unifacUij(21, 26, 32.73, 0, 0, "Hansen 1991"), &
       unifacUij(22, 26, 108.9, 0, 0, "Hansen 1991"), &
       unifacUij(24, 26, 490.9, 0, 0, "Hansen 1991"), &
       unifacUij(25, 26, 132.7, 0, 0, "Hansen 1991"), &
       unifacUij(26, 26, 0, 0, 0, "Hansen 1991"), &
       unifacUij(27, 26, -85.12, 0, 0, "Hansen 1991"), &
       unifacUij(28, 26, 277.8, 0, 0, "Hansen 1991"), &
       unifacUij(31, 26, 481.3, 0, 0, "Hansen 1991"), &
       unifacUij(32, 26, 64.28, 0, 0, "Hansen 1991"), &
       unifacUij(33, 26, 125.3, 0, 0, "Hansen 1991"), &
       unifacUij(34, 26, 174.4, 0, 0, "Hansen 1991"), &
       unifacUij(37, 26, 379.4, 0, 0, "Hansen 1991"), &
       unifacUij(39, 26, 223.6, 0, 0, "Hansen 1991"), &
       unifacUij(41, 26, -124.7, 0, 0, "Hansen 1991"), &
       unifacUij(45, 26, 844, 0, 0, "Hansen 1991"), &
       unifacUij(50, 26, 176.3, 0, 0, "Hansen 1991"), &
       unifacUij(1, 27, 543, 0, 0, "Hansen 1991"), &
       unifacUij(3, 27, 194.9, 0, 0, "Hansen 1991"), &
       unifacUij(4, 27, 4448, 0, 0, "Hansen 1991"), &
       unifacUij(5, 27, 157.1, 0, 0, "Hansen 1991"), &
       unifacUij(6, 27, 457.88, 0, 0, "Hansen 1991"), &
       unifacUij(7, 27, 399.5, 0, 0, "Hansen 1991"), &
       unifacUij(8, 27, -413.48, 0, 0, "Hansen 1991"), &
       unifacUij(9, 27, 548.5, 0, 0, "Hansen 1991"), &
       unifacUij(13, 27, 155.11, 0, 0, "Hansen 1991"), &
       unifacUij(17, 27, -139.3, 0, 0, "Hansen 1991"), &
       unifacUij(18, 27, 2845, 0, 0, "Hansen 1991"), &
       unifacUij(21, 27, 86.2, 0, 0, "Hansen 1991"), &
       unifacUij(24, 27, 534.7, 0, 0, "Hansen 1991"), &
       unifacUij(25, 27, 2213, 0, 0, "Hansen 1991"), &
       unifacUij(26, 27, 533.2, 0, 0, "Hansen 1991"), &
       unifacUij(27, 27, 0, 0, 0, "Hansen 1991"), &
       unifacUij(32, 27, 2448, 0, 0, "Hansen 1991"), &
       unifacUij(33, 27, 4288, 0, 0, "Hansen 1991"), &
       unifacUij(1, 28, 153.6, 0, 0, "Hansen 1991"), &
       unifacUij(2, 28, 76.302, 0, 0, "Hansen 1991"), &
       unifacUij(3, 28, 52.07, 0, 0, "Hansen 1991"), &
       unifacUij(4, 28, -9.451, 0, 0, "Hansen 1991"), &
       unifacUij(5, 28, 488.9, 0, 0, "Hansen 1991"), &
       unifacUij(6, 28, -31.09, 0, 0, "Hansen 1991"), &
       unifacUij(7, 28, 887.1, 0, 0, "Hansen 1991"), &
       unifacUij(8, 28, 8484, 0, 0, "Hansen 1991"), &
       unifacUij(9, 28, 216.1, 0, 0, "Hansen 1991"), &
       unifacUij(11, 28, 183, 0, 0, "Hansen 1991"), &
       unifacUij(13, 28, 140.9, 0, 0, "Hansen 1991"), &
       unifacUij(19, 28, 230.9, 0, 0, "Hansen 1991"), &
       unifacUij(21, 28, 450.1, 0, 0, "Hansen 1991"), &
       unifacUij(23, 28, 116.6, 0, 0, "Hansen 1991"), &
       unifacUij(24, 28, 132.2, 0, 0, "Hansen 1991"), &
       unifacUij(26, 28, 320.2, 0, 0, "Hansen 1991"), &
       unifacUij(28, 28, 0, 0, 0, "Hansen 1991"), &
       unifacUij(32, 28, -27.45, 0, 0, "Hansen 1991"), &
       unifacUij(37, 28, 167.9, 0, 0, "Hansen 1991"), &
       unifacUij(41, 28, 885.5, 0, 0, "Hansen 1991"), &
       unifacUij(1, 29, 184.4, 0, 0, "Hansen 1991"), &
       unifacUij(3, 29, -10.43, 0, 0, "Hansen 1991"), &
       unifacUij(4, 29, 393.6, 0, 0, "Hansen 1991"), &
       unifacUij(5, 29, 147.5, 0, 0, "Hansen 1991"), &
       unifacUij(6, 29, 17.5, 0, 0, "Hansen 1991"), &
       unifacUij(9, 29, -46.28, 0, 0, "Hansen 1991"), &
       unifacUij(12, 29, 103.9, 0, 0, "Hansen 1991"), &
       unifacUij(13, 29, -8.538, 0, 0, "Hansen 1991"), &
       unifacUij(14, 29, -70.14, 0, 0, "Hansen 1991"), &
       unifacUij(19, 29, 0.4604, 0, 0, "Hansen 1991"), &
       unifacUij(21, 29, 59.02, 0, 0, "Hansen 1991"), &
       unifacUij(29, 29, 0, 0, 0, "Hansen 1991"), &
       unifacUij(35, 29, 85.7, 0, 0, "Hansen 1991"), &
       unifacUij(39, 29, -71, 0, 0, "Hansen 1991"), &
       unifacUij(44, 29, -274.1, 0, 0, "Hansen 1991"), &
       unifacUij(48, 29, 6.971, 0, 0, "Hansen 1991"), &
       unifacUij(1, 30, 354.55, 0, 0, "Hansen 1991"), &
       unifacUij(2, 30, 262.9, 0, 0, "Hansen 1991"), &
       unifacUij(3, 30, -64.69, 0, 0, "Hansen 1991"), &
       unifacUij(4, 30, 48.49, 0, 0, "Hansen 1991"), &
       unifacUij(5, 30, -120.5, 0, 0, "Hansen 1991"), &
       unifacUij(6, 30, -61.76, 0, 0, "Hansen 1991"), &
       unifacUij(7, 30, 188, 0, 0, "Hansen 1991"), &
       unifacUij(9, 30, -163.7, 0, 0, "Hansen 1991"), &
       unifacUij(11, 30, 202.3, 0, 0, "Hansen 1991"), &
       unifacUij(13, 30, 170.1, 0, 0, "Hansen 1991"), &
       unifacUij(20, 30, -208.9, 0, 0, "Hansen 1991"), &
       unifacUij(21, 30, 65.56, 0, 0, "Hansen 1991"), &
       unifacUij(22, 30, 149.56, 0, 0, "Hansen 1991"), &
       unifacUij(23, 30, -64.38, 0, 0, "Hansen 1991"), &
       unifacUij(24, 30, 546.7, 0, 0, "Hansen 1991"), &
       unifacUij(30, 30, 0, 0, 0, "Hansen 1991"), &
       unifacUij(37, 30, 82.64, 0, 0, "Hansen 1991"), &
       unifacUij(41, 30, -64.28, 0, 0, "Hansen 1991"), &
       unifacUij(1, 31, 3025, 0, 0, "Hansen 1991"), &
       unifacUij(3, 31, 210.4, 0, 0, "Hansen 1991"), &
       unifacUij(4, 31, 4975, 0, 0, "Hansen 1991"), &
       unifacUij(5, 31, -318.9, 0, 0, "Hansen 1991"), &
       unifacUij(6, 31, -119.2, 0, 0, "Hansen 1991"), &
       unifacUij(7, 31, 12.72, 0, 0, "Hansen 1991"), &
       unifacUij(8, 31, -687.1, 0, 0, "Hansen 1991"), &
       unifacUij(9, 31, 71.46, 0, 0, "Hansen 1991"), &
       unifacUij(11, 31, -101.7, 0, 0, "Hansen 1991"), &
       unifacUij(13, 31, -20.11, 0, 0, "Hansen 1991"), &
       unifacUij(15, 31, 939.07, 0, 0, "Hansen 1991"), &
       unifacUij(17, 31, 0.1004, 0, 0, "Hansen 1991"), &
       unifacUij(19, 31, 177.5, 0, 0, "Hansen 1991"), &
       unifacUij(26, 31, 139.8, 0, 0, "Hansen 1991"), &
       unifacUij(31, 31, 0, 0, 0, "Hansen 1991"), &
       unifacUij(35, 31, 535.8, 0, 0, "Hansen 1991"), &
       unifacUij(39, 31, -191.7, 0, 0, "Hansen 1991"), &
       unifacUij(41, 31, -264.3, 0, 0, "Hansen 1991"), &
       unifacUij(44, 31, 262, 0, 0, "Hansen 1991"), &
       unifacUij(47, 31, 515.8, 0, 0, "Hansen 1991"), &
       unifacUij(1, 32, 335.8, 0, 0, "Hansen 1991"), &
       unifacUij(3, 32, 113.3, 0, 0, "Hansen 1991"), &
       unifacUij(4, 32, 259, 0, 0, "Hansen 1991"), &
       unifacUij(5, 32, 313.5, 0, 0, "Hansen 1991"), &
       unifacUij(6, 32, 212.1, 0, 0, "Hansen 1991"), &
       unifacUij(9, 32, 53.59, 0, 0, "Hansen 1991"), &
       unifacUij(10, 32, 117, 0, 0, "Hansen 1991"), &
       unifacUij(11, 32, 148.3, 0, 0, "Hansen 1991"), &
       unifacUij(13, 32, -149.5, 0, 0, "Hansen 1991"), &
       unifacUij(20, 32, 228.4, 0, 0, "Hansen 1991"), &
       unifacUij(21, 32, 2.22, 0, 0, "Hansen 1991"), &
       unifacUij(22, 32, 177.6, 0, 0, "Hansen 1991"), &
       unifacUij(23, 32, 86.4, 0, 0, "Hansen 1991"), &
       unifacUij(24, 32, 247.8, 0, 0, "Hansen 1991"), &
       unifacUij(26, 32, 304.3, 0, 0, "Hansen 1991"), &
       unifacUij(27, 32, 2990, 0, 0, "Hansen 1991"), &
       unifacUij(28, 32, 292.7, 0, 0, "Hansen 1991"), &
       unifacUij(32, 32, 0, 0, 0, "Hansen 1991"), &
       unifacUij(33, 32, 37.1, 0, 0, "Hansen 1991"), &
       unifacUij(41, 32, 288.1, 0, 0, "Hansen 1991"), &
       unifacUij(1, 33, 479.5, 0, 0, "Hansen 1991"), &
       unifacUij(2, 33, 183.8, 0, 0, "Hansen 1991"), &
       unifacUij(3, 33, 261.3, 0, 0, "Hansen 1991"), &
       unifacUij(4, 33, 210, 0, 0, "Hansen 1991"), &
       unifacUij(5, 33, 202.1, 0, 0, "Hansen 1991"), &
       unifacUij(6, 33, 106.3, 0, 0, "Hansen 1991"), &
       unifacUij(7, 33, 777.1, 0, 0, "Hansen 1991"), &
       unifacUij(9, 33, 245.2, 0, 0, "Hansen 1991"), &
       unifacUij(11, 33, 18.88, 0, 0, "Hansen 1991"), &
       unifacUij(12, 33, 298.13, 0, 0, "Hansen 1991"), &
       unifacUij(13, 33, -202.3, 0, 0, "Hansen 1991"), &
       unifacUij(18, 33, -60.78, 0, 0, "Hansen 1991"), &
       unifacUij(19, 33, -62.17, 0, 0, "Hansen 1991"), &
       unifacUij(20, 33, -95, 0, 0, "Hansen 1991"), &
       unifacUij(21, 33, 344.4, 0, 0, "Hansen 1991"), &
       unifacUij(22, 33, 315.9, 0, 0, "Hansen 1991"), &
       unifacUij(23, 33, 168.8, 0, 0, "Hansen 1991"), &
       unifacUij(24, 33, 146.6, 0, 0, "Hansen 1991"), &
       unifacUij(25, 33, 593.4, 0, 0, "Hansen 1991"), &
       unifacUij(26, 33, 10.17, 0, 0, "Hansen 1991"), &
       unifacUij(27, 33, -124, 0, 0, "Hansen 1991"), &
       unifacUij(32, 33, 6.37, 0, 0, "Hansen 1991"), &
       unifacUij(33, 33, 0, 0, 0, "Hansen 1991"), &
       unifacUij(35, 33, -111.2, 0, 0, "Hansen 1991"), &
       unifacUij(37, 33, 322.42, 0, 0, "Hansen 1991"), &
       unifacUij(39, 33, -176.26, 0, 0, "Hansen 1991"), &
       unifacUij(41, 33, 627.7, 0, 0, "Hansen 1991"), &
       unifacUij(1, 34, 298.9, 0, 0, "Hansen 1991"), &
       unifacUij(2, 34, 31.14, 0, 0, "Hansen 1991"), &
       unifacUij(3, 34, 154.26, 0, 0, "Hansen 1991"), &
       unifacUij(4, 34, -152.55, 0, 0, "Hansen 1991"), &
       unifacUij(5, 34, 727.8, 0, 0, "Hansen 1991"), &
       unifacUij(6, 34, -119.1, 0, 0, "Hansen 1991"), &
       unifacUij(9, 34, -246.6, 0, 0, "Hansen 1991"), &
       unifacUij(10, 34, 2.21, 0, 0, "Hansen 1991"), &
       unifacUij(11, 34, 71.48, 0, 0, "Hansen 1991"), &
       unifacUij(13, 34, -156.57, 0, 0, "Hansen 1991"), &
       unifacUij(19, 34, -203, 0, 0, "Hansen 1991"), &
       unifacUij(26, 34, -27.7, 0, 0, "Hansen 1991"), &
       unifacUij(34, 34, 0, 0, 0, "Hansen 1991"), &
       unifacUij(37, 34, 631.5, 0, 0, "Hansen 1991"), &
       unifacUij(39, 34, 6.699, 0, 0, "Hansen 1991"), &
       unifacUij(1, 35, 526.5, 0, 0, "Hansen 1991"), &
       unifacUij(2, 35, 179, 0, 0, "Hansen 1991"), &
       unifacUij(3, 35, 169.9, 0, 0, "Hansen 1991"), &
       unifacUij(4, 35, 4284, 0, 0, "Hansen 1991"), &
       unifacUij(5, 35, -202.1, 0, 0, "Hansen 1991"), &
       unifacUij(6, 35, -399.3, 0, 0, "Hansen 1991"), &
       unifacUij(7, 35, -139, 0, 0, "Hansen 1991"), &
       unifacUij(9, 35, -44.58, 0, 0, "Hansen 1991"), &
       unifacUij(11, 35, 52.08, 0, 0, "Hansen 1991"), &
       unifacUij(13, 35, 128.8, 0, 0, "Hansen 1991"), &
       unifacUij(14, 35, 874.19, 0, 0, "Hansen 1991"), &
       unifacUij(16, 35, 243.1, 0, 0, "Hansen 1991"), &
       unifacUij(20, 35, -463.6, 0, 0, "Hansen 1991"), &
       unifacUij(22, 35, 215, 0, 0, "Hansen 1991"), &
       unifacUij(23, 35, 363.7, 0, 0, "Hansen 1991"), &
       unifacUij(24, 35, 337.7, 0, 0, "Hansen 1991"), &
       unifacUij(25, 35, 1337.37, 0, 0, "Hansen 1991"), &
       unifacUij(29, 35, 31.66, 0, 0, "Hansen 1991"), &
       unifacUij(31, 35, -417.2, 0, 0, "Hansen 1991"), &
       unifacUij(33, 35, 32.9, 0, 0, "Hansen 1991"), &
       unifacUij(35, 35, 0, 0, 0, "Hansen 1991"), &
       unifacUij(39, 35, 136.6, 0, 0, "Hansen 1991"), &
       unifacUij(41, 35, -29.34, 0, 0, "Hansen 1991"), &
       unifacUij(1, 36, 689, 0, 0, "Hansen 1991"), &
       unifacUij(2, 36, -52.87, 0, 0, "Hansen 1991"), &
       unifacUij(3, 36, 383.9, 0, 0, "Hansen 1991"), &
       unifacUij(4, 36, -119.2, 0, 0, "Hansen 1991"), &
       unifacUij(5, 36, 74.27, 0, 0, "Hansen 1991"), &
       unifacUij(6, 36, -5.224, 0, 0, "Hansen 1991"), &
       unifacUij(7, 36, 160.8, 0, 0, "Hansen 1991"), &
       unifacUij(9, 36, -63.5, 0, 0, "Hansen 1991"), &
       unifacUij(10, 36, -339.2, 0, 0, "Hansen 1991"), &
       unifacUij(11, 36, -28.61, 0, 0, "Hansen 1991"), &
       unifacUij(19, 36, 81.57, 0, 0, "Hansen 1991"), &
       unifacUij(24, 36, 369.5, 0, 0, "Hansen 1991"), &
       unifacUij(36, 36, 0, 0, 0, "Hansen 1991"), &
       unifacUij(37, 36, 837.2, 0, 0, "Hansen 1991"), &
       unifacUij(39, 36, 5.15, 0, 0, "Hansen 1991"), &
       unifacUij(41, 36, -53.91, 0, 0, "Hansen 1991"), &
       unifacUij(1, 37, -4.189, 0, 0, "Hansen 1991"), &
       unifacUij(2, 37, -66.46, 0, 0, "Hansen 1991"), &
       unifacUij(3, 37, -259.1, 0, 0, "Hansen 1991"), &
       unifacUij(4, 37, -282.5, 0, 0, "Hansen 1991"), &
       unifacUij(5, 37, 225.8, 0, 0, "Hansen 1991"), &
       unifacUij(6, 37, 33.47, 0, 0, "Hansen 1991"), &
       unifacUij(9, 37, -34.57, 0, 0, "Hansen 1991"), &
       unifacUij(10, 37, 172.4, 0, 0, "Hansen 1991"), &
       unifacUij(11, 37, -275.2, 0, 0, "Hansen 1991"), &
       unifacUij(12, 37, -11.4, 0, 0, "Hansen 1991"), &
       unifacUij(13, 37, 240.2, 0, 0, "Hansen 1991"), &
       unifacUij(18, 37, 160.7, 0, 0, "Hansen 1991"), &
       unifacUij(19, 37, -55.77, 0, 0, "Hansen 1991"), &
       unifacUij(20, 37, -11.16, 0, 0, "Hansen 1991"), &
       unifacUij(21, 37, -168.2, 0, 0, "Hansen 1991"), &
       unifacUij(22, 37, -91.8, 0, 0, "Hansen 1991"), &
       unifacUij(23, 37, 111.2, 0, 0, "Hansen 1991"), &
       unifacUij(24, 37, 187.1, 0, 0, "Hansen 1991"), &
       unifacUij(26, 37, 10.76, 0, 0, "Hansen 1991"), &
       unifacUij(28, 37, -47.37, 0, 0, "Hansen 1991"), &
       unifacUij(30, 37, 262.9, 0, 0, "Hansen 1991"), &
       unifacUij(33, 37, -48.33, 0, 0, "Hansen 1991"), &
       unifacUij(34, 37, 2073, 0, 0, "Hansen 1991"), &
       unifacUij(36, 37, -208.8, 0, 0, "Hansen 1991"), &
       unifacUij(37, 37, 0, 0, 0, "Hansen 1991"), &
       unifacUij(39, 37, -137.7, 0, 0, "Hansen 1991"), &
       unifacUij(41, 37, -198, 0, 0, "Hansen 1991"), &
       unifacUij(44, 37, -66.31, 0, 0, "Hansen 1991"), &
       unifacUij(48, 37, 148.9, 0, 0, "Hansen 1991"), &
       unifacUij(1, 38, 125.8, 0, 0, "Hansen 1991"), &
       unifacUij(2, 38, 359.3, 0, 0, "Hansen 1991"), &
       unifacUij(3, 38, 389.3, 0, 0, "Hansen 1991"), &
       unifacUij(4, 38, 101.4, 0, 0, "Hansen 1991"), &
       unifacUij(5, 38, 44.78, 0, 0, "Hansen 1991"), &
       unifacUij(6, 38, -48.25, 0, 0, "Hansen 1991"), &
       unifacUij(13, 38, -273.9, 0, 0, "Hansen 1991"), &
       unifacUij(15, 38, 570.9, 0, 0, "Hansen 1991"), &
       unifacUij(16, 38, -196.3, 0, 0, "Hansen 1991"), &
       unifacUij(18, 38, -158.8, 0, 0, "Hansen 1991"), &
       unifacUij(24, 38, 215.2, 0, 0, "Hansen 1991"), &
       unifacUij(38, 38, 0, 0, 0, "Hansen 1991"), &
       unifacUij(39, 38, 50.06, 0, 0, "Hansen 1991"), &
       unifacUij(40, 38, 185.6, 0, 0, "Hansen 1991"), &
       unifacUij(1, 39, 485.3, 0, 0, "Hansen 1991"), &
       unifacUij(2, 39, -70.45, 0, 0, "Hansen 1991"), &
       unifacUij(3, 39, 245.6, 0, 0, "Hansen 1991"), &
       unifacUij(4, 39, 5629, 0, 0, "Hansen 1991"), &
       unifacUij(5, 39, -143.9, 0, 0, "Hansen 1991"), &
       unifacUij(6, 39, -172.4, 0, 0, "Hansen 1991"), &
       unifacUij(7, 39, 319, 0, 0, "Hansen 1991"), &
       unifacUij(9, 39, -61.7, 0, 0, "Hansen 1991"), &
       unifacUij(10, 39, -268.8, 0, 0, "Hansen 1991"), &
       unifacUij(11, 39, 85.33, 0, 0, "Hansen 1991"), &
       unifacUij(12, 39, 308.9, 0, 0, "Hansen 1991"), &
       unifacUij(13, 39, 254.8, 0, 0, "Hansen 1991"), &
       unifacUij(14, 39, -164, 0, 0, "Hansen 1991"), &
       unifacUij(15, 39, -255.22, 0, 0, "Hansen 1991"), &
       unifacUij(16, 39, 22.05, 0, 0, "Hansen 1991"), &
       unifacUij(17, 39, -334.4, 0, 0, "Hansen 1991"), &
       unifacUij(19, 39, -151.5, 0, 0, "Hansen 1991"), &
       unifacUij(20, 39, -228, 0, 0, "Hansen 1991"), &
       unifacUij(21, 39, 6.57, 0, 0, "Hansen 1991"), &
       unifacUij(22, 39, -160.28, 0, 0, "Hansen 1991"), &
       unifacUij(24, 39, 498.6, 0, 0, "Hansen 1991"), &
       unifacUij(25, 39, 5143.14, 0, 0, "Hansen 1991"), &
       unifacUij(26, 39, -223.1, 0, 0, "Hansen 1991"), &
       unifacUij(29, 39, 78.92, 0, 0, "Hansen 1991"), &
       unifacUij(31, 39, 302.2, 0, 0, "Hansen 1991"), &
       unifacUij(33, 39, 336.25, 0, 0, "Hansen 1991"), &
       unifacUij(34, 39, -119.8, 0, 0, "Hansen 1991"), &
       unifacUij(35, 39, -97.71, 0, 0, "Hansen 1991"), &
       unifacUij(36, 39, -8.804, 0, 0, "Hansen 1991"), &
       unifacUij(37, 39, 255, 0, 0, "Hansen 1991"), &
       unifacUij(38, 39, -110.65, 0, 0, "Hansen 1991"), &
       unifacUij(39, 39, 0, 0, 0, "Hansen 1991"), &
       unifacUij(40, 39, 55.8, 0, 0, "Hansen 1991"), &
       unifacUij(41, 39, -28.65, 0, 0, "Hansen 1991"), &
       unifacUij(1, 40, -2.859, 0, 0, "Hansen 1991"), &
       unifacUij(2, 40, 449.4, 0, 0, "Hansen 1991"), &
       unifacUij(3, 40, 22.67, 0, 0, "Hansen 1991"), &
       unifacUij(4, 40, -245.39, 0, 0, "Hansen 1991"), &
       unifacUij(13, 40, -172.51, 0, 0, "Hansen 1991"), &
       unifacUij(25, 40, 309.58, 0, 0, "Hansen 1991"), &
       unifacUij(38, 40, -117.2, 0, 0, "Hansen 1991"), &
       unifacUij(39, 40, -5.579, 0, 0, "Hansen 1991"), &
       unifacUij(40, 40, 0, 0, 0, "Hansen 1991"), &
       unifacUij(45, 40, -32.17, 0, 0, "Hansen 1991"), &
       unifacUij(1, 41, 387.1, 0, 0, "Hansen 1991"), &
       unifacUij(2, 41, 48.33, 0, 0, "Hansen 1991"), &
       unifacUij(3, 41, 103.5, 0, 0, "Hansen 1991"), &
       unifacUij(4, 41, 69.26, 0, 0, "Hansen 1991"), &
       unifacUij(5, 41, 190.3, 0, 0, "Hansen 1991"), &
       unifacUij(6, 41, 165.7, 0, 0, "Hansen 1991"), &
       unifacUij(7, 41, -197.5, 0, 0, "Hansen 1991"), &
       unifacUij(8, 41, -494.2, 0, 0, "Hansen 1991"), &
       unifacUij(9, 41, -18.8, 0, 0, "Hansen 1991"), &
       unifacUij(10, 41, -275.5, 0, 0, "Hansen 1991"), &
       unifacUij(11, 41, 560.2, 0, 0, "Hansen 1991"), &
       unifacUij(12, 41, -70.24, 0, 0, "Hansen 1991"), &
       unifacUij(13, 41, 417, 0, 0, "Hansen 1991"), &
       unifacUij(15, 41, -38.77, 0, 0, "Hansen 1991"), &
       unifacUij(17, 41, -89.42, 0, 0, "Hansen 1991"), &
       unifacUij(19, 41, 120.3, 0, 0, "Hansen 1991"), &
       unifacUij(20, 41, -337, 0, 0, "Hansen 1991"), &
       unifacUij(21, 41, 63.67, 0, 0, "Hansen 1991"), &
       unifacUij(22, 41, -96.87, 0, 0, "Hansen 1991"), &
       unifacUij(23, 41, 255.8, 0, 0, "Hansen 1991"), &
       unifacUij(24, 41, 256.5, 0, 0, "Hansen 1991"), &
       unifacUij(25, 41, -145.1, 0, 0, "Hansen 1991"), &
       unifacUij(26, 41, 248.4, 0, 0, "Hansen 1991"), &
       unifacUij(28, 41, 469.8, 0, 0, "Hansen 1991"), &
       unifacUij(30, 41, 43.37, 0, 0, "Hansen 1991"), &
       unifacUij(31, 41, 347.8, 0, 0, "Hansen 1991"), &
       unifacUij(32, 41, 68.55, 0, 0, "Hansen 1991"), &
       unifacUij(33, 41, -195.1, 0, 0, "Hansen 1991"), &
       unifacUij(35, 41, 153.7, 0, 0, "Hansen 1991"), &
       unifacUij(36, 41, 423.4, 0, 0, "Hansen 1991"), &
       unifacUij(37, 41, 730.8, 0, 0, "Hansen 1991"), &
       unifacUij(39, 41, 72.31, 0, 0, "Hansen 1991"), &
       unifacUij(41, 41, 0, 0, 0, "Hansen 1991"), &
       unifacUij(47, 41, 101.2, 0, 0, "Hansen 1991"), &
       unifacUij(1, 42, -450.4, 0, 0, "Hansen 1991"), &
       unifacUij(3, 42, -432.3, 0, 0, "Hansen 1991"), &
       unifacUij(4, 42, 683.3, 0, 0, "Hansen 1991"), &
       unifacUij(5, 42, -817.7, 0, 0, "Hansen 1991"), &
       unifacUij(7, 42, -363.8, 0, 0, "Hansen 1991"), &
       unifacUij(9, 42, -588.9, 0, 0, "Hansen 1991"), &
       unifacUij(13, 42, 1338, 0, 0, "Hansen 1991"), &
       unifacUij(14, 42, -664.4, 0, 0, "Hansen 1991"), &
       unifacUij(15, 42, 448.1, 0, 0, "Hansen 1991"), &
       unifacUij(20, 42, 169.3, 0, 0, "Hansen 1991"), &
       unifacUij(42, 42, 0, 0, 0, "Hansen 1991"), &
       unifacUij(43, 42, 745.3, 0, 0, "Hansen 1991"), &
       unifacUij(1, 43, 252.7, 0, 0, "Hansen 1991"), &
       unifacUij(3, 43, 238.9, 0, 0, "Hansen 1991"), &
       unifacUij(4, 43, 355.5, 0, 0, "Hansen 1991"), &
       unifacUij(5, 43, 202.7, 0, 0, "Hansen 1991"), &
       unifacUij(14, 43, 275.9, 0, 0, "Hansen 1991"), &
       unifacUij(15, 43, -1327, 0, 0, "Hansen 1991"), &
       unifacUij(20, 43, 127.2, 0, 0, "Hansen 1991"), &
       unifacUij(24, 43, 233.1, 0, 0, "Hansen 1991"), &
       unifacUij(42, 43, -2166, 0, 0, "Hansen 1991"), &
       unifacUij(43, 43, 0, 0, 0, "Hansen 1991"), &
       unifacUij(1, 44, 220.3, 0, 0, "Hansen 1991"), &
       unifacUij(2, 44, 86.46, 0, 0, "Hansen 1991"), &
       unifacUij(3, 44, 30.04, 0, 0, "Hansen 1991"), &
       unifacUij(4, 44, 46.38, 0, 0, "Hansen 1991"), &
       unifacUij(5, 44, -504.2, 0, 0, "Hansen 1991"), &
       unifacUij(7, 44, -452.2, 0, 0, "Hansen 1991"), &
       unifacUij(8, 44, -659, 0, 0, "Hansen 1991"), &
       unifacUij(23, 44, -35.68, 0, 0, "Hansen 1991"), &
       unifacUij(25, 44, -209.7, 0, 0, "Hansen 1991"), &
       unifacUij(29, 44, 1004, 0, 0, "Hansen 1991"), &
       unifacUij(31, 44, -262, 0, 0, "Hansen 1991"), &
       unifacUij(38, 44, 26.35, 0, 0, "Hansen 1991"), &
       unifacUij(44, 44, 0, 0, 0, "Hansen 1991"), &
       unifacUij(1, 45, -5.869, 0, 0, "Hansen 1991"), &
       unifacUij(3, 45, -88.11, 0, 0, "Hansen 1991"), &
       unifacUij(5, 45, 72.96, 0, 0, "Hansen 1991"), &
       unifacUij(6, 45, -52.1, 0, 0, "Hansen 1991"), &
       unifacUij(26, 45, -218.9, 0, 0, "Hansen 1991"), &
       unifacUij(40, 45, 111.8, 0, 0, "Hansen 1991"), &
       unifacUij(45, 45, 0, 0, 0, "Hansen 1991"), &
       unifacUij(1, 46, 390.9, 0, 0, "Hansen 1991"), &
       unifacUij(2, 46, 200.2, 0, 0, "Hansen 1991"), &
       unifacUij(5, 46, -382.7, 0, 0, "Hansen 1991"), &
       unifacUij(7, 46, 835.6, 0, 0, "Hansen 1991"), &
       unifacUij(20, 46, -322.3, 0, 0, "Hansen 1991"), &
       unifacUij(46, 46, 0, 0, 0, "Hansen 1991"), &
       unifacUij(1, 47, 553.3, 0, 0, "Hansen 1991"), &
       unifacUij(2, 47, 268.1, 0, 0, "Hansen 1991"), &
       unifacUij(3, 47, 333.3, 0, 0, "Hansen 1991"), &
       unifacUij(4, 47, 421.9, 0, 0, "Hansen 1991"), &
       unifacUij(5, 47, -248.3, 0, 0, "Hansen 1991"), &
       unifacUij(7, 47, 139.6, 0, 0, "Hansen 1991"), &
       unifacUij(9, 47, 37.54, 0, 0, "Hansen 1991"), &
       unifacUij(11, 47, 151.8, 0, 0, "Hansen 1991"), &
       unifacUij(19, 47, 16.23, 0, 0, "Hansen 1991"), &
       unifacUij(22, 47, 361.1, 0, 0, "Hansen 1991"), &
       unifacUij(24, 47, 423.1, 0, 0, "Hansen 1991"), &
       unifacUij(25, 47, 434.1, 0, 0, "Hansen 1991"), &
       unifacUij(31, 47, -353.5, 0, 0, "Hansen 1991"), &
       unifacUij(47, 47, 0, 0, 0, "Hansen 1991"), &
       unifacUij(1, 48, 187, 0, 0, "Hansen 1991"), &
       unifacUij(2, 48, -617, 0, 0, "Hansen 1991"), &
       unifacUij(6, 48, 37.63, 0, 0, "Hansen 1991"), &
       unifacUij(23, 48, 565.9, 0, 0, "Hansen 1991"), &
       unifacUij(24, 48, 63.95, 0, 0, "Hansen 1991"), &
       unifacUij(29, 48, -18.27, 0, 0, "Hansen 1991"), &
       unifacUij(37, 48, 2429, 0, 0, "Hansen 1991"), &
       unifacUij(48, 48, 0, 0, 0, "Hansen 1991"), &
       unifacUij(1, 49, 216.1, 0, 0, "Hansen 1991"), &
       unifacUij(2, 49, 62.56, 0, 0, "Hansen 1991"), &
       unifacUij(3, 49, -59.58, 0, 0, "Hansen 1991"), &
       unifacUij(4, 49, -203.6, 0, 0, "Hansen 1991"), &
       unifacUij(5, 49, 104.7, 0, 0, "Hansen 1991"), &
       unifacUij(6, 49, -59.4, 0, 0, "Hansen 1991"), &
       unifacUij(7, 49, 407.9, 0, 0, "Hansen 1991"), &
       unifacUij(49, 49, 0, 0, 0, "Hansen 1991"), &
       unifacUij(1, 50, 92.99, 0, 0, "Hansen 1991"), &
       unifacUij(3, 50, -39.16, 0, 0, "Hansen 1991"), &
       unifacUij(4, 50, 184.9, 0, 0, "Hansen 1991"), &
       unifacUij(5, 50, 57.65, 0, 0, "Hansen 1991"), &
       unifacUij(6, 50, -46.01, 0, 0, "Hansen 1991"), &
       unifacUij(8, 50, 1005, 0, 0, "Hansen 1991"), &
       unifacUij(9, 50, -162.6, 0, 0, "Hansen 1991"), &
       unifacUij(18, 50, -136.6, 0, 0, "Hansen 1991"), &
       unifacUij(24, 50, 108.5, 0, 0, "Hansen 1991"), &
       unifacUij(26, 50, -4.565, 0, 0, "Hansen 1991"), &
       unifacUij(1, 52, 808.59, 0, 0, "Hansen 1991"), &
       unifacUij(2, 52, 200.94, 0, 0, "Hansen 1991"), &
       unifacUij(3, 52, 360.82, 0, 0, "Hansen 1991"), &
       unifacUij(4, 52, 233.51, 0, 0, "Hansen 1991"), &
       unifacUij(5, 52, 215.81, 0, 0, "Hansen 1991"), &
       unifacUij(6, 52, 150.02, 0, 0, "Hansen 1991"), &
       unifacUij(7, 52, -255.63, 0, 0, "Hansen 1991"), &
       unifacUij(24, 52, 585.19, 0, 0, "Hansen 1991"), &
       unifacUij(56, 1, -20.31, 0, 0, "Hansen 1991"), &
       unifacUij(56, 3, -106.7, 0, 0, "Hansen 1991"), &
       unifacUij(56, 4, 568.47, 0, 0, "Hansen 1991"), &
       unifacUij(56, 5, 284.28, 0, 0, "Hansen 1991"), &
       unifacUij(56, 7, 401.2, 0, 0, "Hansen 1991"), &
       unifacUij(56, 9, 106.21, 0, 0, "Hansen 1991"), &
       unifacUij(56, 24, -108.37, 0, 0, "Hansen 1991"), &
       unifacUij(56, 25, 5.76, 0, 0, "Hansen 1991"), &
       unifacUij(56, 27, -272.01, 0, 0, "Hansen 1991"), &
       unifacUij(56, 38, 107.84, 0, 0, "Hansen 1991"), &
       unifacUij(56, 39, -33.93, 0, 0, "Hansen 1991"), &
       unifacUij(1, 56, 153.72, 0, 0, "Hansen 1991"), &
       unifacUij(3, 56, 174.35, 0, 0, "Hansen 1991"), &
       unifacUij(4, 56, -280.9, 0, 0, "Hansen 1991"), &
       unifacUij(5, 56, 147.97, 0, 0, "Hansen 1991"), &
       unifacUij(7, 56, 580.28, 0, 0, "Hansen 1991"), &
       unifacUij(9, 56, 179.74, 0, 0, "Hansen 1991"), &
       unifacUij(24, 56, 127.16, 0, 0, "Hansen 1991"), &
       unifacUij(25, 56, 8.48, 0, 0, "Hansen 1991"), &
       unifacUij(27, 56, 1742.53, 0, 0, "Hansen 1991"), &
       unifacUij(38, 56, 117.59, 0, 0, "Hansen 1991"), &
       unifacUij(39, 56, 39.84, 0, 0, "Hansen 1991"), &
       unifacUij(53, 1, 21.49, 0, 0, "Hansen 1991"), &
       unifacUij(53, 2, -2.8, 0, 0, "Hansen 1991"), &
       unifacUij(53, 3, 344.42, 0, 0, "Hansen 1991"), &
       unifacUij(53, 4, 510.32, 0, 0, "Hansen 1991"), &
       unifacUij(53, 5, 244.67, 0, 0, "Hansen 1991"), &
       unifacUij(53, 6, 163.76, 0, 0, "Hansen 1991"), &
       unifacUij(53, 7, 833.21, 0, 0, "Hansen 1991"), &
       unifacUij(53, 9, 569.18, 0, 0, "Hansen 1991"), &
       unifacUij(53, 10, -1.25, 0, 0, "Hansen 1991"), &
       unifacUij(53, 11, -38.4, 0, 0, "Hansen 1991"), &
       unifacUij(53, 12, 69.7, 0, 0, "Hansen 1991"), &
       unifacUij(53, 13, -375.6, 0, 0, "Hansen 1991"), &
       unifacUij(53, 20, 600.78, 0, 0, "Hansen 1991"), &
       unifacUij(53, 21, 291.1, 0, 0, "Hansen 1991"), &
       unifacUij(53, 23, -286.26, 0, 0, "Hansen 1991"), &
       unifacUij(53, 24, -52.93, 0, 0, "Hansen 1991"), &
       unifacUij(53, 37, 177.12, 0, 0, "Hansen 1991"), &
       unifacUij(1, 53, 408.3, 0, 0, "Hansen 1991"), &
       unifacUij(2, 53, 219.9, 0, 0, "Hansen 1991"), &
       unifacUij(3, 53, 171.49, 0, 0, "Hansen 1991"), &
       unifacUij(4, 53, -184.68, 0, 0, "Hansen 1991"), &
       unifacUij(5, 53, 6.39, 0, 0, "Hansen 1991"), &
       unifacUij(6, 53, 98.2, 0, 0, "Hansen 1991"), &
       unifacUij(7, 53, -144.77, 0, 0, "Hansen 1991"), &
       unifacUij(9, 53, -288.94, 0, 0, "Hansen 1991"), &
       unifacUij(10, 53, 79.71, 0, 0, "Hansen 1991"), &
       unifacUij(11, 53, 36.34, 0, 0, "Hansen 1991"), &
       unifacUij(12, 53, -77.96, 0, 0, "Hansen 1991"), &
       unifacUij(13, 53, 567, 0, 0, "Hansen 1991"), &
       unifacUij(20, 53, 12.55, 0, 0, "Hansen 1991"), &
       unifacUij(21, 53, -127.9, 0, 0, "Hansen 1991"), &
       unifacUij(23, 53, 165.67, 0, 0, "Hansen 1991"), &
       unifacUij(24, 53, 291.87, 0, 0, "Hansen 1991"), &
       unifacUij(37, 53, -127.06, 0, 0, "Hansen 1991"), &
       unifacUij(54, 1, 272.82, 0, 0, "Hansen 1991"), &
       unifacUij(54, 2, 569.71, 0, 0, "Hansen 1991"), &
       unifacUij(54, 3, 165.18, 0, 0, "Hansen 1991"), &
       unifacUij(54, 4, 369.89, 0, 0, "Hansen 1991"), &
       unifacUij(54, 9, -62.02, 0, 0, "Hansen 1991"), &
       unifacUij(54, 11, -229.01, 0, 0, "Hansen 1991"), &
       unifacUij(54, 13, -196.59, 0, 0, "Hansen 1991"), &
       unifacUij(54, 18, 100.25, 0, 0, "Hansen 1991"), &
       unifacUij(54, 20, 472.04, 0, 0, "Hansen 1991"), &
       unifacUij(54, 24, 196.73, 0, 0, "Hansen 1991"), &
       unifacUij(54, 28, 434.32, 0, 0, "Hansen 1991"), &
       unifacUij(54, 32, 313.14, 0, 0, "Hansen 1991"), &
       unifacUij(54, 41, -244.59, 0, 0, "Hansen 1991"), &
       unifacUij(1, 54, 718.01, 0, 0, "Hansen 1991"), &
       unifacUij(2, 54, -677.25, 0, 0, "Hansen 1991"), &
       unifacUij(3, 54, 272.33, 0, 0, "Hansen 1991"), &
       unifacUij(4, 54, 9.63, 0, 0, "Hansen 1991"), &
       unifacUij(9, 54, 91.01, 0, 0, "Hansen 1991"), &
       unifacUij(11, 54, 446.9, 0, 0, "Hansen 1991"), &
       unifacUij(13, 54, 102.21, 0, 0, "Hansen 1991"), &
       unifacUij(18, 54, 98.82, 0, 0, "Hansen 1991"), &
       unifacUij(20, 54, -60.07, 0, 0, "Hansen 1991"), &
       unifacUij(24, 54, 532.73, 0, 0, "Hansen 1991"), &
       unifacUij(28, 54, 684.78, 0, 0, "Hansen 1991"), &
       unifacUij(32, 54, 190.81, 0, 0, "Hansen 1991"), &
       unifacUij(41, 54, -100.53, 0, 0, "Hansen 1991"), &
       unifacUij(55, 3, 920.49, 0, 0, "Hansen 1991"), &
       unifacUij(55, 4, 305.77, 0, 0, "Hansen 1991"), &
       unifacUij(55, 20, 171.94, 0, 0, "Hansen 1991"), &
       unifacUij(3, 55, 22.06, 0, 0, "Hansen 1991"), &
       unifacUij(4, 55, 795.38, 0, 0, "Hansen 1991"), &
       unifacUij(20, 55, 88.09, 0, 0, "Hansen 1991"), &
       unifacUij(63, 1, 165.81, -1.149, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(63, 57, 71.623, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(63, 56, 4.2038, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(63, 61, 116.97, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(63, 62, 494.67, -8.1869, 0.04718, "Holderbaum 1991 (PSRK)"), &
       unifacUij(63, 60, 6.423, 0.57946, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(61, 1, 742.31, -5.7074, 0.012651, "Holderbaum 1991 (PSRK)"), &
       unifacUij(61, 57, 295.7, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(61, 56, 114.96, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(61, 60, 648.2, -0.30072, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(60, 1, -101.96, 0.68629, -0.002066, "Holderbaum 1991 (PSRK)"), &
       unifacUij(60, 57, 11.865, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(60, 56, 694.28, -3.0173, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(60, 62, 77.701, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(56, 1, -38.672, 0.86149, -0.0017906, "Holderbaum 1991 (PSRK)"), &
       unifacUij(56, 57, 73.563, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(56, 62, 838.06, -1.0158, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(62, 1, 361.79, -1.0651, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(62, 57, 128.55, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(57, 1, 68.141, -0.7386, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(1, 63, -78.389, 1.8727, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(57, 63, 62.419, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(56, 63, 161.54, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(61, 63, 665.7, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(62, 63, 863.18, -12.309, 0.046316, "Holderbaum 1991 (PSRK)"), &
       unifacUij(60, 63, 25.06, -0.77261, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(1, 61, 893.01, -3.1342, 0.0013022, "Holderbaum 1991 (PSRK)"), &
       unifacUij(57, 61, 91.023, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(56, 61, 78.98, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(60, 61, 862.84, -2.1569, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(1, 60, 527.33, -2.1596, 0.0043234, "Holderbaum 1991 (PSRK)"), &
       unifacUij(57, 60, 64.108, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(56, 60, -580.82, 3.6997, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(62, 60, 247.42, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(1, 56, 919.8, -3.9132, 0.0046309, "Holderbaum 1991 (PSRK)"), &
       unifacUij(57, 56, 196.16, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(62, 56, 3048.9, -10.247, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(1, 62, 175.92, 1.1966, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(57, 62, 253.92, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(1, 57, -39.101, 0.84587, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(1, 6, 674.8, 0.7396, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(1, 55, 2054.3, -3.4233, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(1, 58, -163.27, 0, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(1, 60, 271.12, -0.9903, 3.19E-003, "Fisher 1996 (PSRK)"), &
       unifacUij(1, 62, 613.3, -2.5418, 6.64E-003, "Fisher 1996 (PSRK)"), &
       unifacUij(2, 55, 2287.5, 0, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(2, 56, -52.107, 1.5473, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(2, 57, 38.602, 0, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(2, 60, -231.09, 5.8221, -1.66E-002, "Fisher 1996 (PSRK)"), &
       unifacUij(2, 62, 585, -0.8727, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(2, 63, -241.56, 1.2296, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(3, 6, 1500.1, -2.6423, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(3, 55, 627.81, 0, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(3, 56, 219.25, 0, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(3, 57, 131.51, 0, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(3, 60, 309.59, 0, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(3, 61, 49.132, 0, 0.00E+000, "Fisher 1996 (PSRK)"), &
       unifacUij(3, 62, 734.87, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(3, 63, 680.05, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(4, 55, 821.15, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(4, 56, 296.88, -0.2073, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(4, 57, 37.876, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(4, 60, -303.76, 2.1966, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(4, 61, 23.158, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(4, 62, 320, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(5, 56, 510.64, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(5, 57, 671.69, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(5, 62, 2847.4, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(5, 63, 2138.4, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(6, 55, 138.95, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(6, 56, -72.04, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(6, 57, 553.56, -4.7829, 0.01032, "Fisher 1996 (PSRK)"), &
       unifacUij(6, 60, 190.24, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(6, 61, -33.079, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(6, 62, 250.05, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(6, 63, 261.54, 0.335, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(7, 55, -655.58, 1.0667, 5.79E-004, "Fisher 1996 (PSRK)"), &
       unifacUij(7, 56, -1163.5, 5.4765, -2.60E-003, "Fisher 1996 (PSRK)"), &
       unifacUij(7, 57, -1159.6, 8.6791, -6.99E-003, "Fisher 1996 (PSRK)"), &
       unifacUij(7, 58, -2181.9, 14.238, -1.25E-002, "Fisher 1996 (PSRK)"), &
       unifacUij(7, 59, -318.89, 2.8368, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(7, 60, 2093.1001, -1.9652, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(7, 61, 724.93, -0.0136, 1.06E-003, "Fisher 1996 (PSRK)"), &
       unifacUij(7, 62, 1951.4, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(7, 63, -1665.5, 13.772, -1.41E-002, "Fisher 1996 (PSRK)"), &
       unifacUij(8, 56, 695.7, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(8, 60, 1503.5, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(8, 62, 4027.5, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(9, 56, 132.28, -1.4761, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(9, 60, 646.47, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(9, 61, 7.6453, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(9, 62, 679.19, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(10, 56, -162, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(10, 60, 1310.9, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(11, 56, 818.72, -3.5627, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(13, 56, 2795.3, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(13, 57, 197.09, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(20, 56, 73.859, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(24, 59, -191.54, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(44, 56, -209.22, 1.7024, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(44, 60, 456.4, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(44, 61, -94.959, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(44, 62, 826.49, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(44, 63, 767.11, -1.3117, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(45, 56, 95.548, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(45, 60, -255.14, 2.2203, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(55, 59, 590.21, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(55, 60, 973.45, -0.0302, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(55, 62, 1339, 0.1403, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 58, 208.14, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(57, 59, 17.425, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(57, 63, 62.419, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(58, 59, -7.7202, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(58, 60, -23.358, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(59, 60, 11.986, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(59, 63, 64.1, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(61, 62, 628, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(6, 1, 50.155, -0.1287, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(55, 1, 1243.9, 17.412, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(58, 1, 1855, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 1, 3.7506, 0.3229, -2.01E-003, "Fisher 1996 (PSRK)"), &
       unifacUij(62, 1, 315.96, -0.4563, -1.56E-003, "Fisher 1996 (PSRK)"), &
       unifacUij(55, 2, 2381.1001, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 2, 148.57, -1.1151, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(57, 2, 18.78, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 2, 667.1, -8.0999, 2.26E-002, "Fisher 1996 (PSRK)"), &
       unifacUij(62, 2, 399.44, -0.5806, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(63, 2, -364.32, 0.8134, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(6, 3, -305.9, 0.872, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(55, 3, 392.16, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 3, -29.4, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(57, 3, -65.123, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 3, 58.687, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(61, 3, -98.107, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(62, 3, 16.884, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(63, 3, -252.61, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(55, 4, 607.54, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 4, 249.32, -0.9249, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(57, 4, 5.5762, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 4, 406.32, -1.523, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(61, 4, 40.011, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(62, 4, 126.44, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 5, 148.16, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(57, 5, 516.17, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(62, 5, 2357.2, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(63, 5, 505.73, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(55, 6, -490.47, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 6, 414.57, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(57, 6, 2534.4, -9.2193, 8.02E-003, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 6, 2389.2, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(61, 6, 307.19, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(62, 6, 2136.6001, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(63, 6, 231.32, -0.0476, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(55, 7, -395.59, -0.2461, 7.67E-005, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 7, 1720.6, -4.3437, 1.31E-003, "Fisher 1996 (PSRK)"), &
       unifacUij(57, 7, 60.553, 4.1, -6.24E-003, "Fisher 1996 (PSRK)"), &
       unifacUij(58, 7, -1899.6, 58.045, -9.76E-002, "Fisher 1996 (PSRK)"), &
       unifacUij(59, 7, 200.01, 12.19, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 7, -1715.5, 5.4802, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(61, 7, -8.1881, -0.2077, 1.30E-004, "Fisher 1996 (PSRK)"), &
       unifacUij(62, 7, 2183.5, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(63, 7, -6058.1001, 31.281, -3.31E-002, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 8, 628.8, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 8, 1419, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(62, 8, 3988.6001, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 9, 18.074, 1.8879, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 9, 649.52, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(61, 9, -119.69, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(62, 9, 1602.1, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 10, 340, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 10, 1455.7, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 11, -742.2, 2.9173, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 13, -350.71, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(57, 13, 70.093, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 20, 50.349, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(59, 24, 378.87, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 44, 57.144, -0.9138, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 44, 113.21, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(61, 44, -164.86, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(62, 44, 14.904, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(63, 44, 373.02, -0.4555, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 45, -30.027, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 45, 209.42, -1.3662, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(59, 55, 1453.4, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 55, 1900, 3.415, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(62, 55, 1893.1, 11.556, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(58, 56, 32.043, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(59, 57, 11.868, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(63, 57, 1.6233, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(59, 58, 32.631, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 58, 44.349, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 59, 2.1214, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(63, 59, -18.703, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(62, 61, 137, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(55, 55, 0, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(56, 56, 0, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(57, 57, 0, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(58, 58, 0, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(59, 59, 0, 0, 0, "Fisher 1996 (PSRK)"), &
       unifacUij(60, 60, 0, 0, 0, "Hansen 1991"), &
       unifacUij(61, 61, 0, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(62, 62, 0, 0, 0, "Holderbaum 1991 (PSRK)"), &
       unifacUij(63, 63, 0, 0, 0, "Holderbaum 1991 (PSRK)") &
       /)

end module unifacdata
