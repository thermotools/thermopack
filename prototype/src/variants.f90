! Module demonstrating chained inheritance
! The class VariantEoS is a dummy for e.g. ''cubic'', 'saft' etc.
!   It contains:
!   Two Parameters (p1, p2) not contained in BaseEoS, that each inheriting EoS must set.
!   One procedure that is common to all inheriting classes (implemented)
!       returns: The average of the two parameters (p1 + p2) / 2.0
!   Procedure internal_comp (deferred)

! Variant1 
!   Implements internalcomp
!     Returns: T * p1 + V * p2
!   Fres:
!     Returns:
!       Fres: ((p1 + p2) / 2.0) + T * p1 + V * p2
!       Fv = Ft = Fn = 0.0

! Variant2
!   Implements internalcomp
!     Returns: V * p1 - T * p2
!   Fres:
!     Returns:
!       Fres: ((p1 + p2) / 2.0) - (V * p1 - T * p2)
!       Fv : sum(n) * Rgas * T / V
!       Ft = Fn = 0.0

module variants
  use base_eos, only: BaseEos
  use constants, only: Rgas

  IMPLICIT NONE
  PRIVATE
  public :: VariantEoS, Variant1, Variant2, Variant1_ctor_sub, Variant2_ctor_sub

  ! Intermediate class for many EoS's that are similar,
  ! e.g. cubics, saft-type, etc.
  TYPE, ABSTRACT, EXTENDS(BaseEos) :: VariantEoS
    ! Contains some parammeters that are common to e.g. all cubics
    INTEGER :: param1
    real :: param2 
  CONTAINS
    ! Some internal computation that is common to e.g. all cubics
    ! For example mixing rules.
    PROCEDURE :: variant_common_comp

    ! Some method that all inherriting classes must implement
    PROCEDURE (internal_comp_template), DEFERRED :: internal_comp
  END TYPE VariantEoS

  ABSTRACT INTERFACE
    SUBROUTINE internal_comp_template(this, T, V, n, computed)
      IMPORT VariantEoS
      CLASS (VariantEoS), INTENT(in) :: this
      real, intent(in) :: T, V
      real, dimension(this%ncomps), intent(in) :: n
      real, intent(out) :: computed
    END SUBROUTINE internal_comp_template
  END INTERFACE

  TYPE, EXTENDS(VariantEoS) :: Variant1
    INTEGER :: variant1_specific_param
  CONTAINS
    PROCEDURE :: internal_comp => variant1_internal_comp
    PROCEDURE :: Fres => variant1_Fres
  END TYPE Variant1

  INTERFACE Variant1
    PROCEDURE Variant1_ctor
  END INTERFACE Variant1

  TYPE, EXTENDS(VariantEoS) :: Variant2
    INTEGER :: variant2_specific_param
  CONTAINS
    PROCEDURE :: internal_comp => variant2_internal_comp
    PROCEDURE :: Fres => variant2_Fres
  END TYPE Variant2

  INTERFACE Variant2
    PROCEDURE Variant2_ctor
  END INTERFACE Variant2

Contains

subroutine variant_common_comp(this, mixed_common)
    class(VariantEoS), intent(in) :: this
    real, intent(out) :: mixed_common

    mixed_common = (this%param1 + this%param2) / 2.0
end subroutine variant_common_comp

function Variant1_ctor(ident, nc, Tc, Vc, var1, var2) result(instance)
  integer, intent(in) :: ident
  integer, intent(in) :: nc
  real, intent(in) :: Tc, Vc
  integer, intent(in) :: var1
  real, intent(in) :: var2
  type(Variant1) :: instance

  instance%ident = ident
  instance%ncomps = nc
  instance%Tc = Tc
  instance%Vc = Vc
  instance%param1 = var1
  instance%param2 = var2
end function Variant1_ctor

SUBROUTINE Variant1_ctor_sub(this, ident, nc, Tc, Vc, var1, var2)
    TYPE (Variant1) :: this
    INTEGER, INTENT(in) :: ident
    integer, intent(in) :: nc
    real, intent(in) :: Tc, Vc ! Read these from some database
    integer, intent(in) :: var1
    real, intent(in) :: var2
    this = Variant1(ident, nc, Tc, Vc, var1, var2)
END SUBROUTINE Variant1_ctor_sub

subroutine variant1_internal_comp(this, T, V, n, computed)
    class(Variant1), intent(in) :: this
    real, intent(in) :: T, V
    real, dimension(this%ncomps), intent(in) :: n
    real, intent(out) :: computed

    computed = T * this%param1 + V * this%param2
end subroutine variant1_internal_comp

subroutine variant1_Fres(this, T, V, n, Fres, Ft, Fv, Fn)
    CLASS(Variant1), INTENT(in) :: this
    real, intent(in) :: T, V
    real, dimension(this%ncomps), intent(in) :: n
    real, intent(out) :: Fres
    real, optional, intent(out) :: Ft, Fv
    real, optional, dimension(this%ncomps), intent(out) :: Fn

    real :: mixed_common, computed

    call this%variant_common_comp(mixed_common)
    call this%internal_comp(T, V, n, computed)

    Fres = mixed_common + computed
    if (present(Fv)) Fv = 0.0
    if (present(Ft)) Ft = 0.0
    if (present(Fn)) Fn = 0.0
end subroutine variant1_Fres

function Variant2_ctor(ident, nc, Tc, Vc, var1, var2) result(instance)
  integer, intent(in) :: ident
  integer, intent(in) :: nc
  real, intent(in) :: Tc, Vc
  integer, intent(in) :: var1
  real, intent(in) :: var2
  type(Variant2) :: instance

  instance%ident = ident
  instance%ncomps = nc
  instance%Tc = Tc
  instance%Vc = Vc
  instance%param1 = var1
  instance%param2 = var2
end function Variant2_ctor

SUBROUTINE Variant2_ctor_sub(instance, ident, nc, Tc, Vc, var1, var2)
    TYPE (Variant2) :: instance
    INTEGER, INTENT(in) :: ident
    integer, intent(in) :: nc
    real, intent(in) :: Tc, Vc ! Read these from some database
    integer, intent(in) :: var1
    real, intent(in) :: var2
    instance = Variant2(ident, nc, Tc, Vc, var1, var2)
END SUBROUTINE Variant2_ctor_sub

subroutine variant2_internal_comp(this, T, V, n, computed)
    class(Variant2), intent(in) :: this
    real, intent(in) :: T, V
    real, dimension(this%ncomps), intent(in) :: n
    real, intent(out) :: computed

    computed = V * this%param1 - T * this%param2
end subroutine variant2_internal_comp

subroutine variant2_Fres(this, T, V, n, Fres, Ft, Fv, Fn)
    CLASS(Variant2), INTENT(in) :: this
    real, intent(in) :: T, V
    real, dimension(this%ncomps), intent(in) :: n
    real, intent(out) :: Fres
    real, optional, intent(out) :: Ft, Fv
    real, optional, dimension(this%ncomps), intent(out) :: Fn

    real :: mixed_common, computed

    call this%variant_common_comp(mixed_common)
    call this%internal_comp(T, V, n, computed)

    Fres = mixed_common - computed
    if (present(Fv)) Fv = sum(n) * Rgas * T / V
    if (present(Ft)) Ft = 0.0
    if (present(Fn)) Fn = 0.0
end subroutine variant2_Fres

end module variants