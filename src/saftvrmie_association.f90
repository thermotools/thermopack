!---------------------------------------------------------------------
! Module and subroutines for the association part of SAFT-VR Mie:
! The A in SAFT: Developing the contribution of
! association to the Helmholtz free energy within a
! Wertheim TPT1 treatment of generic Mie fluids
! DOI: 10.1080/00268976.2015.1029027
! Programmed by: M. Hammer, August 2021
!---------------------------------------------------------------------

module saftvrmie_association
  use thermopack_var, only: nce, numAssocSites
  implicit none
  private
  save

  public :: calc_aij, calc_delta_ab_ij_TVN
  public :: calc_I, calc_F_ab, calc_delta_ab_ij
  public :: g_rdf_saftvrmie_ij_TVN

contains

  !> Mixing rule for Kab
  !!
  !! \author Morten Hammer, August 2021
  subroutine calc_Kab_ij(Kab, Kab_ij)
    !type(association), intent(in) :: assoc
    ! Input
    real, intent(in) :: Kab(numAssocSites,numAssocSites,nce)
    ! Output
    real, allocatable, intent(inout) :: Kab_ij(:,:,:,:) !<
    ! Locals
    integer :: a, b, i, j
    if (allocated(Kab_ij)) deallocate(Kab_ij)
    allocate(Kab_ij(numAssocSites,numAssocSites,nce,nce))
    do a=1,numAssocSites
      do b=1,numAssocSites
        do i=1,nce
          !if (assoc%comp_vs_sites(i,1) /= noSitesFlag) cycle
          do j=1,nce
            !if (assoc%comp_vs_sites(j,1) /= noSitesFlag) cycle
            if (j /= i) Kab_ij(a,b,i,j) = ((Kab(a,b,i)**(1.0/3.0) + Kab(a,b,j)**(1.0/3.0))/2)**3
          enddo
        enddo
      enddo
    enddo
  end subroutine calc_Kab_ij

  !> Mixing rule for eps_ab
  !!
  !! \author Morten Hammer, August 2021
  subroutine calc_eps_ab_ij(eps_ab, eps_ab_ij)
    !type(association), intent(in) :: assoc
    ! Input
    real, intent(in) :: eps_ab(numAssocSites,numAssocSites,nce)
    ! Output
    real, allocatable, intent(inout) :: eps_ab_ij(:,:,:,:) !<
    ! Locals
    integer :: a, b, i, j
    if (allocated(eps_ab_ij)) deallocate(eps_ab_ij)
    allocate(eps_ab_ij(numAssocSites,numAssocSites,nce,nce))
    eps_ab_ij = 0
    do a=1,numAssocSites
      do b=1,numAssocSites
        do i=1,nce
          !if (assoc%comp_vs_sites(i,1) /= noSitesFlag) cycle
          do j=1,nce
            !if (assoc%comp_vs_sites(j,1) /= noSitesFlag) cycle
            if (j /= i) eps_ab_ij(a,b,i,j) = sqrt(eps_ab(a,b,i)*eps_ab(a,b,j))
          enddo
        enddo
      enddo
    enddo
  end subroutine calc_eps_ab_ij

  !> Calculate a_ij for given repulsive exponent
  !!
  !! \author Morten Hammer, August 2021
  subroutine calc_aij(lambda_r, aij)
    ! Input
    real, intent(in) :: lambda_r !< Repulsive exponent
    ! Output
    real, intent(out) :: aij(0:10,0:10) !<
    ! Locals
    integer :: i, j, k
    real, dimension(0:10,0:10,0:6) :: b
    real, dimension(0:6) :: lam_r_k
    b = 0
    b(0,0,0) = 1.32970702182068E-02
    b(0,0,1) = 5.56479463564548E-04
    b(0,0,2) = 4.68753836985661E-05
    b(0,0,3) = -1.52750755540612E-06
    b(0,0,4) = -3.67201230932920E-08
    b(0,0,5) = 1.88048156944327E-09
    b(0,0,6) = -1.84421844661105E-11
    b(0,1,0) = -1.77199122935443E-02
    b(0,1,1) = -2.82932524693843E-04
    b(0,1,2) = -2.01534029276569E-04
    b(0,1,3) = 6.65734616244750E-06
    b(0,1,4) = 6.65433123492297E-08
    b(0,1,5) = -5.20041750295709E-09
    b(0,1,6) = 5.39073389353030E-11
    b(0,2,0) = 2.93736747694974E-02
    b(0,2,1) = -2.12156862728691E-03
    b(0,2,2) = 4.08971640196116E-04
    b(0,2,3) = -1.49163457856162E-05
    b(0,2,4) = 8.09753253026481E-08
    b(0,2,5) = 4.35881692094647E-09
    b(0,2,6) = -5.61963272868663E-11
    b(0,3,0) = -2.05527304404423E-02
    b(0,3,1) = 2.24197388058698E-03
    b(0,3,2) = -3.34428221392103E-04
    b(0,3,3) = 1.28251715836463E-05
    b(0,3,4) = -1.29349636796981E-07
    b(0,3,5) = -1.90967917025464E-09
    b(0,3,6) = 3.23187813505929E-11
    b(0,4,0) = 8.61683420907605E-03
    b(0,4,1) = -1.15385075260200E-03
    b(0,4,2) = 1.54951071291061E-04
    b(0,4,3) = -6.13363502620892E-06
    b(0,4,4) = 7.75034942364271E-08
    b(0,4,5) = 4.36098784939288E-10
    b(0,4,6) = -1.13265016166587E-11
    b(0,5,0) = -2.28505275303600E-03
    b(0,5,1) = 3.45261305021541E-04
    b(0,5,2) = -4.39272547633533E-05
    b(0,5,3) = 1.77374828289499E-06
    b(0,5,4) = -2.50442449553800E-08
    b(0,5,5) = -4.66145781784596E-11
    b(0,5,6) = 2.59326201844053E-12
    b(0,6,0) = 3.90171133200072E-04
    b(0,6,1) = -6.36772702454557E-05
    b(0,6,2) = 7.86877804626315E-06
    b(0,6,3) = -3.21719278338969E-07
    b(0,6,4) = 4.81969764886444E-09
    b(0,6,5) = 2.56821311477203E-13
    b(0,6,6) = -4.01407631594698E-13
    b(0,7,0) = -4.26035888869942E-05
    b(0,7,1) = 7.32538316171651E-06
    b(0,7,2) = -8.92538365355001E-07
    b(0,7,3) = 3.67816571475073E-08
    b(0,7,4) = -5.68335864091982E-10
    b(0,7,5) = 4.53836639240824E-13
    b(0,7,6) = 4.20546124661511E-14
    b(0,8,0) = 2.86246920519487E-06
    b(0,8,1) = -5.10881831259873E-07
    b(0,8,2) = 6.20541126317064E-08
    b(0,8,3) = -2.57070860278200E-09
    b(0,8,4) = 4.02544652605731E-11
    b(0,8,5) = -4.35569028016719E-14
    b(0,8,6) = -2.87018830339552E-15
    b(0,9,0) = -1.07315320963937E-07
    b(0,9,1) = 1.97049247692837E-08
    b(0,9,2) = -2.40904019797940E-09
    b(0,9,3) = 1.00187848111417E-10
    b(0,9,4) = -1.57057135789188E-12
    b(0,9,5) = 1.51660179879606E-15
    b(0,9,6) = 1.15159946184350E-16
    b(0,10,0) = 1.70912976772329E-09
    b(0,10,1) = -3.21285622252695E-10
    b(0,10,2) = 3.99225753392296E-11
    b(0,10,3) = -1.66615681224878E-12
    b(0,10,4) = 2.59018065150187E-14
    b(0,10,5) = -1.34231785680549E-17
    b(0,10,6) = -2.05485407533207E-18
    b(1,0,0) = -4.65504528847432E-02
    b(1,0,1) = 4.92332122696642E-02
    b(1,0,2) = -6.36142804034125E-03
    b(1,0,3) = 3.52707112753263E-04
    b(1,0,4) = -1.00533098186312E-05
    b(1,0,5) = 1.44492319539037E-07
    b(1,0,6) = -8.27665331374217E-10
    b(1,1,0) = 3.32597325549352E-01
    b(1,1,1) = -1.34456183191719E-01
    b(1,1,2) = 1.48870088793584E-02
    b(1,1,3) = -7.61494022700993E-04
    b(1,1,4) = 2.04973785455268E-05
    b(1,1,5) = -2.81833237588783E-07
    b(1,1,6) = 1.55896078845997E-09
    b(1,2,0) = -3.26575316241193E-01
    b(1,2,1) = 1.08517185632299E-01
    b(1,2,2) = -1.12930088940554E-02
    b(1,2,3) = 5.51126114938543E-04
    b(1,2,4) = -1.42364525555262E-05
    b(1,2,5) = 1.88795822312826E-07
    b(1,2,6) = -1.01231143749898E-09
    b(1,3,0) = 1.44653671541451E-01
    b(1,3,1) = -4.25401431513956E-02
    b(1,3,2) = 4.23537018032177E-03
    b(1,3,3) = -1.97194099229223E-04
    b(1,3,4) = 4.84859626533717E-06
    b(1,3,5) = -6.12284776821388E-08
    b(1,3,6) = 3.13409590234525E-10
    b(1,4,0) = -3.63193315289496E-02
    b(1,4,1) = 9.54409805564329E-03
    b(1,4,2) = -9.09633136025822E-04
    b(1,4,3) = 3.97623327839760E-05
    b(1,4,4) = -9.03287394521535E-07
    b(1,4,5) = 1.04054640162243E-08
    b(1,4,6) = -4.81050397623846E-11
    b(1,5,0) = 5.69934220115537E-03
    b(1,5,1) = -1.31479859839035E-03
    b(1,5,2) = 1.18406467168496E-04
    b(1,5,3) = -4.65836276873807E-06
    b(1,5,4) = 8.95403985256519E-08
    b(1,5,5) = -7.91894115995732E-10
    b(1,5,6) = 2.30261130144263E-12
    b(1,6,0) = -5.81966173216051E-04
    b(1,6,1) = 1.13485041044446E-04
    b(1,6,2) = -9.31205117458790E-06
    b(1,6,3) = 2.92850782905453E-07
    b(1,6,4) = -3.05175477078061E-09
    b(1,6,5) = -1.78097006314277E-11
    b(1,6,6) = 3.82442150265529E-13
    b(1,7,0) = 3.83608167089024E-05
    b(1,7,1) = -5.97590027689909E-06
    b(1,7,2) = 4.07198179416416E-07
    b(1,7,3) = -5.67255537711216E-09
    b(1,7,4) = -2.51206227550094E-10
    b(1,7,5) = 8.46173387258907E-12
    b(1,7,6) = -7.05793035532524E-14
    b(1,8,0) = -1.50305409953983E-06
    b(1,8,1) = 1.73112888670222E-07
    b(1,8,2) = -7.24002263850399E-09
    b(1,8,3) = -3.50155347187566E-10
    b(1,8,4) = 2.85888337544568E-11
    b(1,8,5) = -6.29468789021339E-13
    b(1,8,6) = 4.59807633591802E-15
    b(1,9,0) = 2.66749257811143E-08
    b(1,9,1) = -2.01831274082934E-09
    b(1,9,2) = -2.85054817786792E-11
    b(1,9,3) = 1.64319946681537E-11
    b(1,9,4) = -8.35713691699930E-13
    b(1,9,5) = 1.62914975397206E-14
    b(1,9,6) = -1.12579371971295E-16
    b(2,0,0) = 1.64972499633366E-01
    b(2,0,1) = -1.58447251265296E-01
    b(2,0,2) = 2.11014984424621E-02
    b(2,0,3) = -1.27835416513710E-03
    b(2,0,4) = 3.98289826660923E-05
    b(2,0,5) = -6.16808609401052E-07
    b(2,0,6) = 3.74547734805679E-09
    b(2,1,0) = -9.74898725377830E-01
    b(2,1,1) = 4.30576656790814E-01
    b(2,1,2) = -4.95382445219106E-02
    b(2,1,3) = 2.72631887421459E-03
    b(2,1,4) = -7.92066147384541E-05
    b(2,1,5) = 1.16624214027143E-06
    b(2,1,6) = -6.83326218348459E-09
    b(2,2,0) = 9.19082550772666E-01
    b(2,2,1) = -3.42386658999542E-01
    b(2,2,2) = 3.71909493534914E-02
    b(2,2,3) = -1.95876152374924E-03
    b(2,2,4) = 5.49066841157859E-05
    b(2,2,5) = -7.85736422030061E-07
    b(2,2,6) = 4.50304220343962E-09
    b(2,3,0) = -3.67978443660284E-01
    b(2,3,1) = 1.26318819450003E-01
    b(2,3,2) = -1.32662359974178E-02
    b(2,3,3) = 6.80170219619246E-04
    b(2,3,4) = -1.86402735397850E-05
    b(2,3,5) = 2.61841021616088E-07
    b(2,3,6) = -1.47842817249421E-09
    b(2,4,0) = 7.88054156983951E-02
    b(2,4,1) = -2.57089028169037E-02
    b(2,4,2) = 2.64252348906371E-03
    b(2,4,3) = -1.33171449466011E-04
    b(2,4,4) = 3.59640611239294E-06
    b(2,4,5) = -4.99004695268396E-08
    b(2,4,6) = 2.78924412091172E-10
    b(2,5,0) = -9.81102799831725E-03
    b(2,5,1) = 3.10740070593876E-03
    b(2,5,2) = -3.16720398760111E-04
    b(2,5,3) = 1.58508191731109E-05
    b(2,5,4) = -4.25136583233798E-07
    b(2,5,5) = 5.85970807128971E-09
    b(2,5,6) = -3.25532731205188E-11
    b(2,6,0) = 7.17901835772044E-04
    b(2,6,1) = -2.26247702797060E-04
    b(2,6,2) = 2.32682269109226E-05
    b(2,6,3) = -1.17119945008334E-06
    b(2,6,4) = 3.14701885589044E-08
    b(2,6,5) = -4.33266245043971E-10
    b(2,6,6) = 2.40008362810406E-12
    b(2,7,0) = -2.91191052989125E-05
    b(2,7,1) = 9.40337471999922E-06
    b(2,7,2) = -9.93969228161767E-07
    b(2,7,3) = 5.09014996522337E-08
    b(2,7,4) = -1.37942010763254E-09
    b(2,7,5) = 1.90360900947593E-11
    b(2,7,6) = -1.05309962007498E-13
    b(2,8,0) = 5.17207032026779E-07
    b(2,8,1) = -1.75927011626452E-07
    b(2,8,2) = 1.93247731973246E-08
    b(2,8,3) = -1.01109171728229E-09
    b(2,8,4) = 2.76625894833152E-11
    b(2,8,5) = -3.82506468133384E-13
    b(2,8,6) = 2.11144278824276E-15
    b(3,0,0) = -5.53080054304108E-01
    b(3,0,1) = 1.30961541597078E-01
    b(3,0,2) = -2.04522546881708E-02
    b(3,0,3) = 1.64258140843764E-03
    b(3,0,4) = -6.22606152603800E-05
    b(3,0,5) = 1.08977129458469E-06
    b(3,0,6) = -7.12756897329521E-09
    b(3,1,0) = 1.07124021914524E+00
    b(3,1,1) = -3.58137806097004E-01
    b(3,1,2) = 4.56627841733661E-02
    b(3,1,3) = -2.94360803306568E-03
    b(3,1,4) = 9.78661859355705E-05
    b(3,1,5) = -1.59381377689636E-06
    b(3,1,6) = 1.00393010742688E-08
    b(3,2,0) = -9.14915281734471E-01
    b(3,2,1) = 3.23521260711548E-01
    b(3,2,2) = -3.75241588712114E-02
    b(3,2,3) = 2.15659810736663E-03
    b(3,2,4) = -6.54711127544114E-05
    b(3,2,5) = 9.99496177147637E-07
    b(3,2,6) = -6.02106444139650E-09
    b(3,3,0) = 3.28132391309741E-01
    b(3,3,1) = -1.13820803061658E-01
    b(3,3,2) = 1.25888572116524E-02
    b(3,3,3) = -6.85370482291315E-04
    b(3,3,4) = 1.98394792303955E-05
    b(3,3,5) = -2.91595588554532E-07
    b(3,3,6) = 1.70643459526364E-09
    b(3,4,0) = -5.77909160509185E-02
    b(3,4,1) = 1.92702466504444E-02
    b(3,4,2) = -2.04736822270412E-03
    b(3,4,3) = 1.07391347801278E-04
    b(3,4,4) = -3.01458972861098E-06
    b(3,4,5) = 4.32652379690724E-08
    b(3,4,6) = -2.48716300671280E-10
    b(3,5,0) = 5.39941935098940E-03
    b(3,5,1) = -1.69264438876774E-03
    b(3,5,2) = 1.71672501041056E-04
    b(3,5,3) = -8.69254977310800E-06
    b(3,5,4) = 2.38076581975360E-07
    b(3,5,5) = -3.36277868350366E-09
    b(3,5,6) = 1.91426474551801E-11
    b(3,6,0) = -2.49522541631115E-04
    b(3,6,1) = 7.20470394807155E-05
    b(3,6,2) = -6.91918528995779E-06
    b(3,6,3) = 3.38632787324714E-07
    b(3,6,4) = -9.12137235540096E-09
    b(3,6,5) = 1.28216601435668E-10
    b(3,6,6) = -7.31095625646731E-13
    b(3,7,0) = 4.36324500072586E-06
    b(3,7,1) = -1.12895786020720E-06
    b(3,7,2) = 1.02234151256019E-07
    b(3,7,3) = -4.90436065438310E-09
    b(3,7,4) = 1.33298781401136E-10
    b(3,7,5) = -1.91702973428801E-12
    b(3,7,6) = 1.12112566023581E-14
    b(4,0,0) = 6.97481173735912E-01
    b(4,0,1) = -6.78784049001058E-02
    b(4,0,2) = 2.67157884350682E-02
    b(4,0,3) = -2.96574649489361E-03
    b(4,0,4) = 1.24810638661335E-04
    b(4,0,5) = -2.25971486750789E-06
    b(4,0,6) = 1.48400265810797E-08
    b(4,1,0) = -1.27802319197572E-01
    b(4,1,1) = -1.59716521155965E-01
    b(4,1,2) = 6.04022182900132E-03
    b(4,1,3) = 7.37340818584580E-04
    b(4,1,4) = -5.13612052054272E-05
    b(4,1,5) = 1.11917575440765E-06
    b(4,1,6) = -8.18214743502133E-09
    b(4,2,0) = 2.38559496985344E-01
    b(4,2,1) = -4.08260244006866E-02
    b(4,2,2) = 8.22450412680841E-03
    b(4,2,3) = -7.62065788926240E-04
    b(4,2,4) = 3.09432853737792E-05
    b(4,2,5) = -5.65159033070467E-07
    b(4,2,6) = 3.81858691213582E-09
    b(4,3,0) = -1.24364954974360E-01
    b(4,3,1) = 3.97261699377944E-02
    b(4,3,2) = -5.02124889994980E-03
    b(4,3,3) = 3.15097078455367E-04
    b(4,3,4) = -1.02009751236812E-05
    b(4,3,5) = 1.62627589914010E-07
    b(4,3,6) = -1.00814014221601E-09
    b(4,4,0) = 1.83583032370354E-02
    b(4,4,1) = -6.68853458452261E-03
    b(4,4,2) = 8.03630265796884E-04
    b(4,4,3) = -4.64683975120556E-05
    b(4,4,4) = 1.39714413579629E-06
    b(4,4,5) = -2.09918759575187E-08
    b(4,4,6) = 1.24324450815098E-10
    b(4,5,0) = -1.33946361388127E-03
    b(4,5,1) = 4.90515978818942E-04
    b(4,5,2) = -5.55280746686634E-05
    b(4,5,3) = 3.00942802764189E-06
    b(4,5,4) = -8.55034278983589E-08
    b(4,5,5) = 1.22756291392471E-09
    b(4,5,6) = -7.02051970613448E-12
    b(4,6,0) = 3.68888649118614E-05
    b(4,6,1) = -1.26640904345952E-05
    b(4,6,2) = 1.30402695735287E-06
    b(4,6,3) = -6.47808066443911E-08
    b(4,6,4) = 1.71150044927963E-09
    b(4,6,5) = -2.32414275161676E-11
    b(4,6,6) = 1.27771307749498E-13
    b(5,0,0) = -6.82258598593205E-03
    b(5,0,1) = 6.11672146147809E-01
    b(5,0,2) = -1.31004401410042E-01
    b(5,0,3) = 1.04319873447675E-02
    b(5,0,4) = -3.74044099050907E-04
    b(5,0,5) = 6.17883043382806E-06
    b(5,0,6) = -3.82128399646057E-08
    b(5,1,0) = -2.96768597044265E-01
    b(5,1,1) = 2.57963755040360E-01
    b(5,1,2) = -9.85163509226622E-03
    b(5,1,3) = -7.75429459116355E-04
    b(5,1,4) = 5.60796050391817E-05
    b(5,1,5) = -1.20280382926209E-06
    b(5,1,6) = 8.61165773602971E-09
    b(5,2,0) = 3.46077751701231E-01
    b(5,2,1) = -1.50206376568020E-01
    b(5,2,2) = 1.28544254818812E-02
    b(5,2,3) = -3.79064014723981E-04
    b(5,2,4) = 1.84626127086989E-06
    b(5,2,5) = 8.66499935530469E-08
    b(5,2,6) = -1.03558713701492E-09
    b(5,3,0) = 1.22496582163678E-02
    b(5,3,1) = 3.87683716563604E-03
    b(5,3,2) = -2.16912930642026E-04
    b(5,3,3) = -1.60783833898331E-05
    b(5,3,4) = 1.34288528575647E-06
    b(5,3,5) = -3.13263066251902E-08
    b(5,3,6) = 2.39230607884171E-10
    b(5,4,0) = -8.05951611068984E-04
    b(5,4,1) = 1.13887448052591E-04
    b(5,4,2) = -4.55721397631024E-05
    b(5,4,3) = 4.71880061366660E-06
    b(5,4,4) = -1.94614655230730E-07
    b(5,4,5) = 3.53265909236495E-09
    b(5,4,6) = -2.35824243963634E-11
    b(5,5,0) = 5.01775524378700E-05
    b(5,5,1) = -2.67212361539355E-05
    b(5,5,2) = 4.78472396442476E-06
    b(5,5,3) = -3.40226871507608E-07
    b(5,5,4) = 1.14157863743549E-08
    b(5,5,5) = -1.81505995831496E-10
    b(5,5,6) = 1.10391700857033E-12
    b(6,0,0) = -2.46482416179796E+00
    b(6,0,1) = -1.27512696874714E+00
    b(6,0,2) = 2.60822991454304E-01
    b(6,0,3) = -1.95358884282327E-02
    b(6,0,4) = 6.72078359176232E-04
    b(6,0,5) = -1.08048264112286E-05
    b(6,0,6) = 6.55633333711198E-08
    b(6,1,0) = -1.45370322973416E+00
    b(6,1,1) = 4.21048118713917E-01
    b(6,1,2) = -6.26432527852684E-02
    b(6,1,3) = 4.20143444237083E-03
    b(6,1,4) = -1.38031320636187E-04
    b(6,1,5) = 2.17366900821365E-06
    b(6,1,6) = -1.31376750286488E-08
    b(6,2,0) = -4.48827080921154E-01
    b(6,2,1) = 1.22792263121120E-01
    b(6,2,2) = -9.59839254636274E-03
    b(6,2,3) = 2.97766652427410E-04
    b(6,2,4) = -3.14894396008512E-06
    b(6,2,5) = -1.53022515538303E-08
    b(6,2,6) = 3.55287199097211E-10
    b(6,3,0) = -5.66229179136722E-03
    b(6,3,1) = -1.58386412295998E-03
    b(6,3,2) = 2.23341727285368E-04
    b(6,3,3) = -8.36498131244475E-06
    b(6,3,4) = 5.76785692862702E-08
    b(6,3,5) = 2.11500280833261E-09
    b(6,3,6) = -2.85768231341124E-11
    b(6,4,0) = -1.80870029200998E-04
    b(6,4,1) = 1.98370689434891E-04
    b(6,4,2) = -2.78886526475732E-05
    b(6,4,3) = 1.51044087021239E-06
    b(6,4,4) = -3.88710150387342E-08
    b(6,4,5) = 4.74260725560735E-10
    b(6,4,6) = -2.19531090111075E-12
    b(7,0,0) = 8.78388694047369E+00
    b(7,0,1) = 2.69957785723510E-01
    b(7,0,2) = -1.79706752593973E-01
    b(7,0,3) = 1.64362361737192E-02
    b(7,0,4) = -6.09145837554261E-04
    b(7,0,5) = 1.01466386059898E-05
    b(7,0,6) = -6.26595167233416E-08
    b(7,1,0) = 3.23807384513205E+00
    b(7,1,1) = -8.99401835359301E-01
    b(7,1,2) = 9.89212484451187E-02
    b(7,1,3) = -5.27606737824625E-03
    b(7,1,4) = 1.48275594790438E-04
    b(7,1,5) = -2.10287523493265E-06
    b(7,1,6) = 1.18345081383843E-08
    b(7,2,0) = 2.81816142695178E-01
    b(7,2,1) = -5.93934567946635E-02
    b(7,2,2) = 3.88197187439871E-03
    b(7,2,3) = -9.27089410962249E-05
    b(7,2,4) = 2.61353677755675E-07
    b(7,2,5) = 1.91184417519603E-08
    b(7,2,6) = -1.95221464285268E-10
    b(7,3,0) = 3.40169105539079E-03
    b(7,3,1) = -1.10977335239395E-03
    b(7,3,2) = 1.51511550199890E-04
    b(7,3,3) = -9.32930119768768E-06
    b(7,3,4) = 2.91087061127825E-07
    b(7,3,5) = -4.48828024315313E-09
    b(7,3,6) = 2.70989222978676E-11
    b(8,0,0) = -1.35178089781880E+01
    b(8,0,1) = 1.49379616940916E+00
    b(8,0,2) = -3.15900588187112E-02
    b(8,0,3) = -4.39666318131193E-03
    b(8,0,4) = 2.52724106819134E-04
    b(8,0,5) = -4.89026741776877E-06
    b(8,0,6) = 3.22964489272539E-08
    b(8,1,0) = -2.48217551606281E+00
    b(8,1,1) = 6.28459498670159E-01
    b(8,1,2) = -6.07681438566115E-02
    b(8,1,3) = 2.87829011566385E-03
    b(8,1,4) = -7.36076396864788E-05
    b(8,1,5) = 9.75457041464072E-07
    b(8,1,6) = -5.24025196193952E-09
    b(8,2,0) = -7.83334040233511E-02
    b(8,2,1) = 1.77032259427776E-02
    b(8,2,2) = -1.41908354521815E-03
    b(8,2,3) = 5.24103094975034E-05
    b(8,2,4) = -1.00642768627400E-06
    b(8,2,5) = 1.00074660431270E-08
    b(8,2,6) = -4.14196554462777E-11
    b(9,0,0) = 9.42415649943917E+00
    b(9,0,1) = -1.50913781396606E+00
    b(9,0,2) = 9.26908832419059E-02
    b(9,0,3) = -1.59566365617165E-03
    b(9,0,4) = -2.49255867787548E-05
    b(9,0,5) = 1.04333009405285E-06
    b(9,0,6) = -8.35760085684382E-09
    b(9,1,0) = 6.87363680163044E-01
    b(9,1,1) = -1.66624388301135E-01
    b(9,1,2) = 1.51506077043458E-02
    b(9,1,3) = -6.67764807450442E-04
    b(9,1,4) = 1.59720037817341E-05
    b(9,1,5) = -2.00783047336916E-07
    b(9,1,6) = 1.03868339224250E-09
    b(10,0,0) = -2.46151453173016E+00
    b(10,0,1) = 4.44942467424175E-01
    b(10,0,2) = -3.12717011022248E-02
    b(10,0,3) = 8.61407443430205E-04
    b(10,0,4) = -7.70341617155424E-06
    b(10,0,5) = -5.01517451094617E-08
    b(10,0,6) = 8.47595203890549E-10

    lam_r_k(0) = 1.0
    do k=1,6
      lam_r_k(k) = lam_r_k(k-1)*lambda_r
    enddo
    aij = 0
    do i=0,10
      do j=0,10-i
        do k=0,6
          aij(i,j) = aij(i,j) + lam_r_k(k)*b(i,j,k)
        enddo
      enddo
    enddo
  end subroutine calc_aij


  !> Calculate associated kernel for given repulsive exponent
  !!
  !! \author Morten Hammer, August 2021
  subroutine calc_I(T,rho,aij,Int,I_r,I_rr,I_T,I_TT,I_rT)
    ! Input
    real, intent(in) :: T !< Reduced temperature
    real, intent(in) :: rho !< Reduced density
    real, intent(in) :: aij(0:10,0:10) !<
    ! Output
    real, intent(out) :: Int
    real, optional, intent(out) :: I_r,I_rr,I_T,I_TT,I_rT
    ! Locals
    integer :: i, j
    real, dimension(0:10,0:2) :: T_j, rho_i

    T_j = 0
    rho_i = 0
    T_j(0,0) = 1
    rho_i(0,0) = 1
    T_j(1,1) = 1
    rho_i(1,1) = 1
    T_j(2,2) = 2.0
    rho_i(2,2) = 2.0
    do i=1,10
      T_j(i,0) = T_j(i-1,0)*T
      rho_i(i,0) = rho_i(i-1,0)*rho
      if (i > 1) then
        T_j(i,1) = i*T_j(i-1,0)
        rho_i(i,1) = i*rho_i(i-1,0)
      endif
      if (i > 2) then
        T_j(i,2) = (i-1)*i*T_j(i-2,0)
        rho_i(i,2) = (i-1)*i*rho_i(i-2,0)
      endif
    enddo
    Int = 0
    do i=0,10
      do j=0,10-i
        Int = Int + aij(i,j)*rho_i(i,0)*T_j(j,0)
      enddo
    enddo
    if (present(I_r)) then
      I_r = 0
      do i=0,10
        do j=0,10-i
          I_r = I_r + aij(i,j)*rho_i(i,1)*T_j(j,0)
        enddo
      enddo
    endif
    if (present(I_rr)) then
      I_rr = 0
      do i=0,10
        do j=0,10-i
          I_rr = I_rr + aij(i,j)*rho_i(i,2)*T_j(j,0)
        enddo
      enddo
    endif
    if (present(I_T)) then
      I_T = 0
      do i=0,10
        do j=0,10-i
          I_T = I_T + aij(i,j)*rho_i(i,0)*T_j(j,1)
        enddo
      enddo
    endif
    if (present(I_TT)) then
      I_TT = 0
      do i=0,10
        do j=0,10-i
          I_TT = I_TT + aij(i,j)*rho_i(i,0)*T_j(j,2)
        enddo
      enddo
    endif
    if (present(I_rT)) then
      I_rT = 0
      do i=0,10
        do j=0,10-i
          I_rT = I_rT + aij(i,j)*rho_i(i,1)*T_j(j,1)
        enddo
      enddo
    endif
  end subroutine calc_I

  !> Calculate temperature dependent magnitude
  !!
  !! \author Morten Hammer, August 2021
  subroutine calc_F_ab(T,eps_ab_HB_div_kb,F,F_T,F_TT)
    ! Input
    real, intent(in) :: T !< Temperature (K)
    real, intent(in) :: eps_ab_HB_div_kb !< Reduced bounding energy (K)
    ! Output
    real, intent(out) :: F,F_T,F_TT
    ! Locals
    real :: exp_hb
    exp_hb = exp(eps_ab_HB_div_kb/T)
    F = exp_hb - 1
    F_T = -exp_hb*eps_ab_HB_div_kb/T**2
    F_TT = exp_hb*(eps_ab_HB_div_kb**2/T**4 + 2*eps_ab_HB_div_kb/T**3)
  end subroutine calc_F_ab

  !> Calculate integrated associated strength
  !!
  !! \author Morten Hammer, August 2021
  subroutine calc_delta_ab_ij(T,rho,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,D,D_r,D_rr,D_T,D_TT,D_rT)
    ! Input
    real, intent(in) :: T !< Temperature (K)
    real, intent(in) :: rho !< Reduced density (-)
    real, intent(in) :: eps_ab_HB_div_kb !< Reduced bounding energy (K)
    real, intent(in) :: Kab_ij, eps_ij
    real, intent(in) :: aij(0:10,0:10) !<
    ! Output
    real, intent(out) :: D
    real, optional, intent(out) :: D_r,D_rr,D_T,D_TT,D_rT
    ! Locals
    real :: F,F_T,F_TT
    real :: Ts, I_T
    Ts = T/eps_ij
    call calc_I(Ts,rho,aij,D,D_r,D_rr,I_T,D_TT,D_rT)
    I_T = I_T/eps_ij
    if (present(D_T)) then
      D_T = I_T
    endif
    if (present(D_TT)) then
      D_TT = D_TT/eps_ij**2
    endif
    if (present(D_rT)) then
      D_rT = D_rT/eps_ij
    endif
    call calc_F_ab(T,eps_ab_HB_div_kb,F,F_T,F_TT)
    F = F*Kab_ij
    F_T = F_T*Kab_ij
    F_TT = F_TT*Kab_ij
    if (present(D_TT)) then
      D_TT = D_TT*F + 2*D_T*F_T + D*F_TT
    endif
    if (present(D_T)) then
      D_T = D_T*F + D*F_T
    endif
    if (present(D_rT)) then
      D_rT = D_rT*F + D_r*F_T
    endif
    D = D*F
    if (present(D_r)) then
      D_r = D_r*F
    endif
    if (present(D_rr)) then
      D_rr = D_rr*F
    endif
  end subroutine calc_delta_ab_ij

  !> Calculate integrated associated strength
  !!
  !! \author Morten Hammer, August 2021
  subroutine calc_delta_ab_ij_TVN(T,v,n,rho_star,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,&
       D,D_v,D_vv,D_T,D_TT,D_Tv,D_n,D_vn,D_Tn,D_nn)
    use saftvrmie_utils, only: convert_zeta_x_to_TVn
    use saftvrmie_containers, only: saftvrmie_zeta
    ! Input
    real, intent(in) :: T !< Temperature (K)
    real, intent(in) :: V !< Volume (m3)
    real, intent(in) :: n(nce) !< Mol numbers (mol)
    type(saftvrmie_zeta), intent(in) :: rho_star
    real, intent(in) :: eps_ab_HB_div_kb !< Reduced bounding energy (K)
    real, intent(in) :: Kab_ij, eps_ij
    real, intent(in) :: aij(0:10,0:10) !<
    ! Output
    real, intent(out) :: D
    real, optional, intent(out) :: D_v,D_vv,D_T,D_TT,D_Tv
    real, optional, intent(out) :: D_n(nce),D_vn(nce),D_Tn(nce),D_nn(nce,nce)
    ! Locals
    logical :: first_order_present, second_order_present
    real :: Di_r,Di_rr,Di_T,Di_TT,Di_rT
    integer :: difflevel
    first_order_present = (present(D_T) .and. present(D_n) .and. present(D_V))
    second_order_present = (present(D_TT) .or. present(D_TV) .or. present(D_Tn) .or. &
         present(D_Vn) .or. present(D_VV) .or. present(D_nn))
    if (second_order_present) then
      difflevel = 2
    else if (first_order_present) then
      difflevel = 1
    else
      difflevel = 0
    endif
    call calc_delta_ab_ij(T,rho_star%zx,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij, &
         D,Di_r,Di_rr,Di_T,Di_TT,Di_rT)

    call convert_zeta_x_to_TVn(nce,0.0,1.0,0.0,rho_star,&
         D,Di_r,Di_T,Di_rr,Di_TT,Di_rT,0.0,0.0,0.0,&
         D_T,D_V,D_n,D_TT,D_VV,D_TV,D_Tn,D_Vn,D_nn,difflevel=difflevel)

  end subroutine calc_delta_ab_ij_TVN

  !> Calculate integrated associated strength
  !!
  !! \author Morten Hammer, August 2021
  subroutine g_rdf_saftvrmie_ij_TVN(T,v,n,i,j,s_vc,&
       g,g_v,g_vv,g_T,g_TT,g_Tv,g_n,g_vn,g_Tn,g_nn)
    use saftvrmie_utils, only: convert_zeta_x_to_TVn
    use saftvrmie_containers, only: saftvrmie_param, saftvrmie_var_container
    use saftvrmie_dispersion, only: calcZetaX_vdW
    ! Input
    real, intent(in) :: T !< Temperature (K)
    real, intent(in) :: V !< Volume (m3)
    real, intent(in) :: n(nce) !< Mol numbers (mol)
    integer, intent(in) :: i, j
    type(saftvrmie_var_container), intent(inout):: s_vc
    ! Output
    real, intent(out) :: g
    real, optional, intent(out) :: g_v,g_vv,g_T,g_TT,g_Tv
    real, optional, intent(out) :: g_n(nce),g_vn(nce),g_Tn(nce),g_nn(nce,nce)
    ! Locals
    logical :: first_order_present, second_order_present
    real :: gi_r,gi_rr,gi_T,gi_TT,gi_rT
    integer :: difflevel
    real :: eps_ij, Ts
    first_order_present = (present(g_T) .and. present(g_n) .and. present(g_V))
    second_order_present = (present(g_TT) .or. present(g_TV) .or. present(g_Tn) .or. &
         present(g_Vn) .or. present(g_VV) .or. present(g_nn))
    if (second_order_present) then
      difflevel = 2
    else if (first_order_present) then
      difflevel = 1
    else
      difflevel = 0
    endif

    call calcZetaX_vdW(nce,T,V,n,difflevel,dhs=saftvrmie_param%hbc%sigma_ij,zeta=s_vc%rho_star,is_rho=.true.)

    eps_ij = saftvrmie_param%eps_divk_ij(i,j)
    Ts = T/eps_ij
    call calc_I(Ts,s_vc%rho_star%zx,saftvrmie_param%hbc%aij(:,:,i,j),&
         g,gi_r,gi_rr,gi_T,gi_TT,gi_rT)
    gi_T = gi_T/eps_ij
    gi_TT = gi_TT/eps_ij**2
    gi_rT = gi_rT/eps_ij

    call convert_zeta_x_to_TVn(nce,0.0,1.0,0.0,s_vc%rho_star,&
         g,gi_r,gi_T,gi_rr,gi_TT,gi_rT,0.0,0.0,0.0,&
         g_T,g_V,g_n,g_TT,g_VV,g_TV,g_Tn,g_Vn,g_nn,difflevel=difflevel)

  end subroutine g_rdf_saftvrmie_ij_TVN

  subroutine get_c_LJ(c)
    real, intent(out) :: c(0:10,0:10)
    c = 0
    c(0,0) = 7.56425183020431E-02
    c(0,1) = -1.28667137050961E-01
    c(0,2) = 1.28350632316055E-01
    c(0,3) = -7.25321780970292E-02
    c(0,4) = 2.57782547511452E-02
    c(0,5) = -6.01170055221687E-03
    c(0,6) = 9.33363147191978E-04
    c(0,7) = -9.55607377143667E-05
    c(0,8) = 6.19576039900837E-06
    c(0,9) = -2.30466608213628E-07
    c(0,10) = 3.74605718435540E-09
    c(1,0) = 1.34228218276565E-01
    c(1,1) = -1.82682168504886E-01
    c(1,2) = 7.71662412959262E-02
    c(1,3) = -7.17458641164565E-04
    c(1,4) = -8.72427344283170E-03
    c(1,5) = 2.97971836051287E-03
    c(1,6) = -4.84863997651451E-04
    c(1,7) = 4.35262491516424E-05
    c(1,8) = -2.07789181640066E-06
    c(1,9) = 4.13749349344802E-08
    c(2,0) = -5.65116428942893E-01
    c(2,1) = 1.00930692226792E+00
    c(2,2) = -6.60166945915607E-01
    c(2,3) = 2.14492212294301E-01
    c(2,4) = -3.88462990166792E-02
    c(2,5) = 4.06016982985030E-03
    c(2,6) = -2.39515566373142E-04
    c(2,7) = 7.25488368831468E-06
    c(2,8) = -8.58904640281928E-08
    c(3,0) = -3.87336382687019E-01
    c(3,1) = -2.11614570109503E-01
    c(3,2) = 4.50442894490509E-01
    c(3,3) = -1.76931752538907E-01
    c(3,4) = 3.17171522104923E-02
    c(3,5) = -2.91368915845693E-03
    c(3,6) = 1.30193710011706E-04
    c(3,7) = -2.14505500786531E-06
    c(4,0) = 2.13713180911797E+00
    c(4,1) = -2.02798460133021E+00
    c(4,2) = 3.36709255682693E-01
    c(4,3) = 1.18106507393722E-03
    c(4,4) = -6.00058423301506E-03
    c(4,5) = 6.26343952584415E-04
    c(4,6) = -2.03636395699819E-05
    c(5,0) = -3.00527494795524E-01
    c(5,1) = 2.89920714512243E+00
    c(5,2) = -5.67134839686498E-01
    c(5,3) = 5.18085125423494E-02
    c(5,4) = -2.39326776760414E-03
    c(5,5) = 4.15107362643844E-05
    c(6,0) = -6.21028065719194E+00
    c(6,1) = -1.92883360342573E+00
    c(6,2) = 2.84109761066570E-01
    c(6,3) = -1.57606767372364E-02
    c(6,4) = 3.68599073256615E-04
    c(7,0) = 1.16083532818029E+01
    c(7,1) = 7.42215544511197E-01
    c(7,2) = -8.23976531246117E-02
    c(7,3) = 1.86167650098254E-03
    c(8,0) = -1.02632535542427E+01
    c(8,1) = -1.25035689035085E-01
    c(8,2) = 1.14299144831867E-02
    c(9,0) = 4.65297446837297E+00
    c(9,1) = -1.92518067137033E-03
    c(10,0) = -8.67296219639940E-01
  end subroutine get_c_LJ

end module saftvrmie_association

subroutine testing_saftvrmie_association()
  use thermopack_constants, only: N_AVOGADRO
  use thermopack_var, only: nc
  use saftvrmie_containers, only: saftvrmie_zeta, saftvrmie_dhs, &
       init_saftvrmie_containers, saftvrmie_param, allocate_saftvrmie_dhs, &
       allocate_saftvrmie_zeta
  use saftvrmie_dispersion, only: calcZetaX_vdW
  use saftvrmie_association
  implicit none
  type(saftvrmie_zeta) :: rho_star, rho_star0
  type(saftvrmie_dhs) :: dhs
  real :: lambda_r, aij(0:10,0:10), T, rho
  real :: I0,I0_r,I0_rr,I0_T,I0_TT,I0_rT
  real :: I2,I2_r,I2_rr,I2_T,I2_TT,I2_rT
  real :: I1,I1_r,I1_rr,I1_T,I1_TT,I1_rT
  real :: F0,F0_T,F0_TT
  real :: F1,F1_T,F1_TT
  real :: F2,F2_T,F2_TT
  real :: D0,D0_r,D0_rr,D0_T,D0_TT,D0_rT
  real :: D1,D1_r,D1_rr,D1_T,D1_TT,D1_rT
  real :: D2,D2_r,D2_rr,D2_T,D2_TT,D2_rT
  real :: eps, eps_ab_HB_div_kb,eps_ij, Kab_ij
  real :: D0_v,D0_vv,D0_Tv,D0_n(nc),D0_vn(nc),D0_Tn(nc),D0_nn(nc,nc)
  real :: D1_v,D1_n(nc),D2_v,D2_n(nc)
  real :: n0(nc), n1(nc), dT, dV, V
  !call init_saftvrmie_containers(nc,comp)
  lambda_r = 12.0
  call calc_aij(lambda_r, aij)
  T = 1.125
  rho = 0.5
  eps = 1.0e-5
  print *,"Testing I(T,r)"
  call calc_I(T,rho,aij,I0,I0_r,I0_rr,I0_T,I0_TT,I0_rT)
  call calc_I(T-eps,rho,aij,I1,I1_r,I1_rr,I1_T,I1_TT,I1_rT)
  call calc_I(T+eps,rho,aij,I2,I2_r,I2_rr,I2_T,I2_TT,I2_rT)
  print *,I0_T,(I2-I1)/eps/2
  print *,I0_TT,(I2_T-I1_T)/eps/2
  print *,I0_rT,(I2_r-I1_r)/eps/2
  call calc_I(T,rho-eps,aij,I1,I1_r,I1_rr,I1_T,I1_TT,I1_rT)
  call calc_I(T,rho+eps,aij,I2,I2_r,I2_rr,I2_T,I2_TT,I2_rT)
  print *,I0_r,(I2-I1)/eps/2
  print *,I0_rr,(I2_r-I1_r)/eps/2
  print *,I0_rT,(I2_T-I1_T)/eps/2

  print *,"Testing Fab(T)"
  eps_ab_HB_div_kb = 1.23456
  call calc_F_ab(T,eps_ab_HB_div_kb,F0,F0_T,F0_TT)
  call calc_F_ab(T-eps,eps_ab_HB_div_kb,F1,F1_T,F1_TT)
  call calc_F_ab(T+eps,eps_ab_HB_div_kb,F2,F2_T,F2_TT)
  print *,F0_T,(F2-F1)/eps/2
  print *,F0_TT,(F2_T-F1_T)/eps/2

  eps_ab_HB_div_kb = 89.0
  eps_ij = 30.0
  Kab_ij = 3.14
  T = T*eps_ij
  call calc_delta_ab_ij(T,rho,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,D0,D0_r,D0_rr,D0_T,D0_TT,D0_rT)
  call calc_delta_ab_ij(T-eps*eps_ij,rho,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,D1,D1_r,D1_rr,D1_T,D1_TT,D1_rT)
  call calc_delta_ab_ij(T+eps*eps_ij,rho,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,D2,D2_r,D2_rr,D2_T,D2_TT,D2_rT)
  print *,D0_T,(D2-D1)/eps/eps_ij/2
  print *,D0_TT,(D2_T-D1_T)/eps/eps_ij/2
  print *,D0_rT,(D2_r-D1_r)/eps/eps_ij/2
  call calc_delta_ab_ij(T,rho,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,D0,D0_r,D0_rr,D0_T,D0_TT,D0_rT)
  call calc_delta_ab_ij(T,rho-eps,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,D1,D1_r,D1_rr,D1_T,D1_TT,D1_rT)
  call calc_delta_ab_ij(T,rho+eps,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,D2,D2_r,D2_rr,D2_T,D2_TT,D2_rT)
  print *,D0_r,(D2-D1)/eps/2
  print *,D0_rr,(D2_r-D1_r)/eps/2
  print *,D0_rT,(D2_T-D1_T)/eps/2

  n0 = 1.1
  call allocate_saftvrmie_zeta(nc,rho_star)
  call allocate_saftvrmie_zeta(nc,rho_star0)
  call allocate_saftvrmie_dhs(nc,dhs)
  dhs%d = saftvrmie_param%sigma_ij
  dhs%d_T = 0
  dhs%d_TT = 0
  V = sum(n0)*N_AVOGADRO*dhs%d(1,1)**3/rho
  call calcZetaX_vdW(nc,T,V,n0,difflevel=2,dhs=dhs,zeta=rho_star0,is_rho=.true.)

  call calc_delta_ab_ij_TVN(T,v,n0,rho_star0,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,&
       D0,D0_v,D0_vv,D0_T,D0_TT,D0_Tv,D0_n,D0_vn,D0_Tn,D0_nn)
  dT = eps*eps_ij
  dV = eps*V
  call calc_delta_ab_ij_TVN(T-dT,v,n0,rho_star0,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,&
       D=D1,D_v=D1_v,D_T=D1_T,D_n=D1_n)
  call calc_delta_ab_ij_TVN(T+dT,v,n0,rho_star0,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,&
       D=D2,D_v=D2_v,D_T=D2_T,D_n=D2_n)
  print *,D0_T,(D2-D1)/dT/2
  print *,D0_TT,(D2_T-D1_T)/dT/2
  print *,D0_Tv,(D2_v-D1_v)/dT/2
  print *,D0_Tn,(D2_n-D1_n)/dT/2

  call calcZetaX_vdW(nc,T,V-dV,n0,difflevel=1,dhs=dhs,zeta=rho_star,is_rho=.true.)
  call calc_delta_ab_ij_TVN(T,v-dv,n0,rho_star,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,&
       D=D1,D_v=D1_v,D_T=D1_T,D_n=D1_n)
  call calcZetaX_vdW(nc,T,V+dV,n0,difflevel=1,dhs=dhs,zeta=rho_star,is_rho=.true.)
  call calc_delta_ab_ij_TVN(T,v+dv,n0,rho_star,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,&
       D=D2,D_v=D2_v,D_T=D2_T,D_n=D2_n)
  print *,D0_v,(D2-D1)/dv/2
  print *,D0_vv,(D2_v-D1_v)/dv/2
  print *,D0_Tv,(D2_T-D1_T)/dv/2
  print *,D0_vn,(D2_n-D1_n)/dv/2

  n1 = n0
  n1(1) = n1(1) - eps
  call calcZetaX_vdW(nc,T,V,n1,difflevel=1,dhs=dhs,zeta=rho_star,is_rho=.true.)
  call calc_delta_ab_ij_TVN(T,v,n1,rho_star,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,&
       D=D1,D_v=D1_v,D_T=D1_T,D_n=D1_n)
  n1 = n0
  n1(1) = n1(1) + eps
  call calcZetaX_vdW(nc,T,V,n1,difflevel=1,dhs=dhs,zeta=rho_star,is_rho=.true.)
  call calc_delta_ab_ij_TVN(T,v,n1,rho_star,eps_ab_HB_div_kb,eps_ij,Kab_ij,aij,&
       D=D2,D_v=D2_v,D_T=D2_T,D_n=D2_n)
  print *,D0_n,(D2-D1)/eps/2
  print *,D0_nn,(D2_n-D1_n)/eps/2
  print *,D0_Tn,(D2_T-D1_T)/eps/2
  print *,D0_vn,(D2_v-D1_v)/eps/2
  stop
end subroutine testing_saftvrmie_association


subroutine testing_saftvrmie_association_g()
  use thermopack_constants, only: N_AVOGADRO
  use thermopack_var, only: thermo_model, nc, get_active_thermo_model
  use saftvrmie_containers, only: saftvrmie_param, saftvrmie_var_container, get_saftvrmie_var
  use saftvrmie_association
  use saftvrmie_dispersion, only: calcZetaX_vdW
  implicit none
  integer :: i,j
  real :: T, rho
  real :: g0,g0_T,g0_TT
  real :: g1,g1_T
  real :: g2,g2_T
  real :: eps, eps_ij
  real :: g0_v,g0_vv,g0_Tv,g0_n(nc),g0_vn(nc),g0_Tn(nc),g0_nn(nc,nc)
  real :: g1_v,g1_n(nc),g2_v,g2_n(nc)
  real :: n0(nc), n1(nc), dT, dV, V
  type(thermo_model), pointer :: act_mod_ptr
  type(saftvrmie_var_container), pointer :: svrm_var
  act_mod_ptr => get_active_thermo_model()
  !call init_saftvrmie(nc,act_mod_ptr%comps,act_mod_ptr%eos(1)%p_eos,"DEFAULT",1)
  svrm_var => get_saftvrmie_var()

  T = 1.125
  rho = 0.5
  eps = 1.0e-5
  n0 = 1.1

  i = 1
  j = 1
  eps_ij = saftvrmie_param%eps_divk_ij(i,j)
  T = T*eps_ij
  V = sum(n0)*N_AVOGADRO*saftvrmie_param%sigma_ij(i,j)**3/rho
  print *,eps_ij

  call calcZetaX_vdW(nc,T,V,n0,difflevel=2,dhs=svrm_var%dhs,&
       zeta=svrm_var%rho_star,is_rho=.true.)
  call g_rdf_saftvrmie_ij_TVN(T,v,n0,i,j,svrm_var, &
       g0,g0_v,g0_vv,g0_T,g0_TT,g0_Tv,g0_n,g0_vn,g0_Tn,g0_nn)
  dT = eps*eps_ij
  dV = eps*V
  call g_rdf_saftvrmie_ij_TVN(T-dT,v,n0,i,j,svrm_var, &
       g1,g_v=g1_v,g_T=g1_T,g_n=g1_n)
  call g_rdf_saftvrmie_ij_TVN(T+dT,v,n0,i,j,svrm_var, &
       g2,g_v=g2_v,g_T=g2_T,g_n=g2_n)
  print *,g0_T,(g2-g1)/dT/2
  print *,g0_TT,(g2_T-g1_T)/dT/2
  print *,g0_Tv,(g2_v-g1_v)/dT/2
  print *,g0_Tn,(g2_n-g1_n)/dT/2

  call calcZetaX_vdW(nc,T,V-dv,n0,difflevel=1,dhs=svrm_var%dhs,&
       zeta=svrm_var%rho_star,is_rho=.true.)
  call g_rdf_saftvrmie_ij_TVN(T,v-dv,n0,i,j,svrm_var, &
       g1,g_v=g1_v,g_T=g1_T,g_n=g1_n)
  call calcZetaX_vdW(nc,T,V+dv,n0,difflevel=1,dhs=svrm_var%dhs,&
       zeta=svrm_var%rho_star,is_rho=.true.)
  call g_rdf_saftvrmie_ij_TVN(T,v+dv,n0,i,j,svrm_var, &
       g2,g_v=g2_v,g_T=g2_T,g_n=g2_n)
  print *,g0_v,(g2-g1)/dv/2
  print *,g0_vv,(g2_v-g1_v)/dv/2
  print *,g0_Tv,(g2_T-g1_T)/dv/2
  print *,g0_vn,(g2_n-g1_n)/dv/2

  n1 = n0
  n1(i) = n1(i) - eps
  call calcZetaX_vdW(nc,T,V,n1,difflevel=1,dhs=svrm_var%dhs,&
       zeta=svrm_var%rho_star,is_rho=.true.)
  call g_rdf_saftvrmie_ij_TVN(T,v,n1,i,j,svrm_var, &
       g1,g_v=g1_v,g_T=g1_T,g_n=g1_n)
  n1 = n0
  n1(i) = n1(i) + eps
  call calcZetaX_vdW(nc,T,V,n1,difflevel=1,dhs=svrm_var%dhs,&
       zeta=svrm_var%rho_star,is_rho=.true.)
  call g_rdf_saftvrmie_ij_TVN(T,v,n1,i,j,svrm_var, &
       g2,g_v=g2_v,g_T=g2_T,g_n=g2_n)
  print *,g0_n(i),(g2-g1)/eps/2
  print *,g0_nn(:,i),(g2_n-g1_n)/eps/2
  print *,g0_Tn(i),(g2_T-g1_T)/eps/2
  print *,g0_vn(i),(g2_v-g1_v)/eps/2
  stop
end subroutine testing_saftvrmie_association_g
