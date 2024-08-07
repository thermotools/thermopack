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
  use base_eos, only: BaseEos, BaseEos_init
  use constants, only: Rgas
  use compdata_mod, only: VariantData
  use compdatautils, only: get_variant_compdata

  implicit none
  private
  public :: VariantEoS, Variant1, Variant2, &
            Variant1_ctor_sub, Variant1_db_ctor_sub, Variant1_db_ctor, &
            Variant2_ctor_sub, Variant2_db_ctor_sub, Variant2_db_ctor

  ! Intermediate class for many EoS's that are similar,
  ! e.g. cubics, saft-type, etc.
  type, abstract, extends(BaseEos) :: VariantEoS
    ! Contains some parameters that are common to e.g. all cubics
    integer :: param1
    real :: param2 
  contains
    ! Some internal computation that is common to e.g. all cubics
    ! For example mixing rules.
    procedure :: variant_common_comp

    ! Some method that all inherriting classes must implement
    procedure (internal_comp_template), deferred :: internal_comp
  end type VariantEoS

  abstract interface
    subroutine internal_comp_template(this, T, V, n, computed)
      import VariantEoS
      class(VariantEoS), intent(in) :: this
      real, intent(in) :: T, V
      real, dimension(this%ncomps), intent(in) :: n
      real, intent(out) :: computed
    end subroutine internal_comp_template
  end interface

  type, extends(VariantEoS) :: Variant1
    integer :: variant1_specific_param
  contains
    procedure :: internal_comp => variant1_internal_comp
    procedure :: Fres => variant1_Fres
  end type Variant1

  interface Variant1
    procedure Variant1_ctor
    procedure Variant1_db_ctor
  end interface Variant1

  type, extends(VariantEoS) :: Variant2
    integer :: variant2_specific_param
  contains
    procedure :: internal_comp => variant2_internal_comp
    procedure :: Fres => variant2_Fres
  end type Variant2

  interface Variant2
    procedure Variant2_ctor
    procedure Variant2_db_ctor
  end interface Variant2

contains

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

subroutine Variant1_ctor_sub(this, ident, nc, Tc, Vc, var1, var2)
    TYPE (Variant1) :: this
    INTEGER, INTENT(in) :: ident
    integer, intent(in) :: nc
    real, intent(in) :: Tc, Vc ! Read these from some database
    integer, intent(in) :: var1
    real, intent(in) :: var2
    this = Variant1(ident, nc, Tc, Vc, var1, var2)
end subroutine Variant1_ctor_sub

function Variant1_db_ctor(ident, ref) result(instance)
    character(len=10), intent(in) :: ident
    character(len=10), optional, intent(in) :: ref
    character(len=10) :: ref_ = "default" // char(0)
    type(Variant1) :: instance
    type(VariantData) :: data

    if (present(ref)) then
        ref_ = ref
    end if

    print*, "Getting VariantData for : ", ident, ", ", ref_

    data = get_variant_compdata(ident, ref_)

    print*, "Got VariantData for : ", ident, ", ", ref_

    call BaseEos_init(instance, ident)

    instance%param1 = data%p1
    instance%param2 = data%p2

end function Variant1_db_ctor

subroutine Variant1_db_ctor_sub(this, ident, ref)
    type(Variant1) :: this
    character(len=10), intent(in) :: ident
    character(len=10), optional, intent(inout) :: ref

    this = Variant1_db_ctor(ident, ref)
end subroutine Variant1_db_ctor_sub

subroutine variant1_internal_comp(this, T, V, n, computed)
    class(Variant1), intent(in) :: this
    real, intent(in) :: T, V
    real, dimension(this%ncomps), intent(in) :: n
    real, intent(out) :: computed

    computed = T * this%param1 + V * this%param2
end subroutine variant1_internal_comp

subroutine variant1_Fres(this, T, V, n, Fres, Ft, Fv, Fn)
    class(Variant1), intent(in) :: this
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

subroutine Variant2_ctor_sub(instance, ident, nc, Tc, Vc, var1, var2)
    TYPE (Variant2) :: instance
    INTEGER, INTENT(in) :: ident
    integer, intent(in) :: nc
    real, intent(in) :: Tc, Vc ! Read these from some database
    integer, intent(in) :: var1
    real, intent(in) :: var2
    instance = Variant2(ident, nc, Tc, Vc, var1, var2)
end subroutine Variant2_ctor_sub

function Variant2_db_ctor(ident, ref) result(instance)
    character(len=10), intent(in) :: ident
    character(len=10), optional, intent(in) :: ref
    character(len=10) :: ref_ = "default" // char(0)
    type(Variant2) :: instance
    type(VariantData) :: data

    if (present(ref)) then
        ref_ = ref
    end if

    data = get_variant_compdata(ident, ref_)
    call BaseEos_init(instance, ident)

    instance%param1 = data%p1
    instance%param2 = data%p2

end function Variant2_db_ctor

subroutine Variant2_db_ctor_sub(this, ident, ref)
    type(Variant2) :: this
    character(len=10), intent(in) :: ident
    character(len=10), optional, intent(inout) :: ref

    this = Variant2_db_ctor(ident, ref)
end subroutine Variant2_db_ctor_sub

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