!> DESCRIPTION OF THE MODULE COMPDATA
!! The module compdata stores pure component data.
!! After initialisation of a mixture, data from the database "compdatadb.mod"
!! are selected and copied into the working array "comp" of active components.
!!
!! The parameters are:
!!      \param ident The component id used when searching through the database
!!      \param formula The component formula
!!      \param name The full name for the component
!!      \param mw The molweight
!!      \param tc Critical temperature (K)
!!      \param pc Critical pressure (Pa)
!!      \param zc Critical compressibility (-)
!!      \param acf Acentric factor (-)
!!      \param tb Normal boiling point (K)
!!      \param psatcode A flag for selecting vapour pressure correlation. the default is
!!          using the antoine equation 1: Antoine, 2: Wilson (or Michelsen)
!!          This is used for initial calulation of ideal k-values.
!!      \param ANT Parameters for the Antoine vapour pressure curve - should be updated to SI-units
!!      \param tminant Lower temperature for Antoine equation
!!      \param tmaxant Upper temperature for Antoine equation
!!      \param cptype Type of correlation used for ideal cp calculations
!!      \param CP up to 10 parameters for the cp-equation
!!      \param tmincp Lower temperature for cp-equation
!!      \param tmaxcp Upper temperature for cp-equation
!!      \param href Reference enthalpy [J/kmol]
!!      \param sref Reference entropy [J/kmol K]
!!
!!
!! The data type compdata used is basically the same as the type gendatadb in
!! the data base, except that only one active ideal cp-correlation is stored.
!!
!! Available CP-ideal correlations can vary, depending on the fluid.
!!
!! The ones that are in use are:
!!\verbatim
!! CPTYPE   - METHOD FOR IDEAL-GAS HEAT-CAPACITY CALCULATION                *
!!             - 1 : SHERWOOD, REID & PRAUSNITZ, THIRD EDITION              *
!!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
!!                               CP(4)*T**3                    (cal/gmol K) *
!!             - 2 : API-PROJECT 44                                         *
!!             - 3 : HYPOTETIC COMPONENTS                                   *
!!             - 4 : SHERWOOD, REID & PRAUSNITZ, FOURTH EDITION             *
!!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
!!                               CP(4)*T**3                    (J/mol K)    *
!!             - 5 : ICI (KRISTER STR\M)                                    *
!!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
!!                               CP(4)*T**3 + CP(5)/T**2           (kJ/kgK) *
!!             - 6 : CHEN, BENDER (PETTER NEKSÅ)                            *
!!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)*T**2 +             *
!!                               CP(4)*T**3+ CP(5)*T**4        (kJ/kg K)    *
!!             - 7 : AIChE, Daubert and Danner, DIPPR-databasen             *
!!                   CP(ideal) = A + B[(C/T)/sinh(C/T)]**2                  *
!!                               + D[(E/T)/cosh(E/T)]**2      (J/(kmol K))  *
!!             - 8 : POLING, PRAUSNITZ & O'CONNEL, FIFTH EDITION            *
!!                   CP(ideal)/R = CP(1) + CP(2)*T + CP(3)*T**2 +           *
!!                               CP(4)*T**3 + CP(5)*T**4       (-)          *
!!             - 9 : Linear function and fraction (J/mol/K)                 *
!!                   CP(ideal) = CP(1) + CP(2)*T + CP(3)/(T + CP(4))        *
!!                                                                          *
!!             -10 : Leachman (NIST) and Valenta expression H2              *
!!                                                                          *
!!             -11 : Use TREND model                                        *
!! \endverbatim
!!
!! \param maxnc The maximum number of components - determined from the input-files
!! \param maxnca The maximum number of active components


Module compdata
   implicit none
   save

   type :: gendata
      sequence
      integer :: idx                        !< The index in the database
      character (len=8) :: ident            !< The component ID
      character (len=12) :: formula         !< Chemical formula
      character (len=40) :: name
      real :: mw                            !< Molweight [g/mol]
      real :: tc, pc                        !< Critical temperature [K], pressure [Pa]
      real :: zc, acf                       !< Compressibility and acentric factor
      real :: tb                            !< Normal boiling point [K]
      real :: ttr, ptr                      !< Triple point data
      integer :: psatcode                   !< Vapour pressure correlation 1: Antoine 2: Wilson (Michelsen) 3: Starling
      real, dimension(3) :: ant
      real :: tantmin, tantmax
      integer  :: ncptype, cptype           !< The number of ideal CP-correlations and the default
      real, dimension(10) :: cp
      real  :: tcpmin, tcpmax
      real  :: href, sref                   !< The reference enthalpy and entropy
      real  :: zra                          !< Rackett compressibility factor
      real  :: ci                           !< Volume shift parameter [m3/mol]

      integer :: assoc_scheme               !< Association scheme for use in the SAFT model. The various schemes are defined in saft_parameters_db.f90.
    end type gendata

    integer, parameter :: CP_POLY3_CAL=1, CP_API44_MASS=2, CP_HYPOTETIC_MASS=3, CP_POLY3_SI=4, &
         CP_ICI_MASS=5, CP_CHEN_BENDER_MASS=6, CP_DIPPR_KMOL=7, CP_POLY4_SI=8, CP_MOGENSEN_SI=9, &
         CP_H2_KMOL=10, CP_TREND_SI=11

End module compdata
