!> Hyperdual number definition & type declaration
!!
!! Original code provided by Philipp Rehner and Gernot Bauer,
!! Institute of Thermodynamics and Thermal Process Engineering (ITT),
!! University of Stuttgart, Stuttgart, Germany
!!
!!  #### Hypderdual numbers
!!
!!  Hypderdual numbers extend the idea of additional, non-real
!!  components from one non-real component (complex numbers) to four
!!  non-real components: \f$\varepsilon_1\f$, \f$\varepsilon_2\f$ and
!!  \f$\varepsilon_1 \varepsilon_2\f$.
!!  Hyperdual numbers require: \f$(\varepsilon_1)^2 = 0\f$,
!!  \f$(\varepsilon_2)^2 = 0\f$ and
!!  \f$(\varepsilon_1\varepsilon_2)^2 = 0\f$
!!  This leads to the fact, that the Taylor series of a function with
!!  hyperdual arguments can be truncated _exactly_ after the second
!!  derivative term:
!!  \f[
!!     f(\mathbf{x} + h_1 \varepsilon_1 + h_2 \varepsilon_2
!!       + h_1 h_2 \varepsilon_1 \varepsilon_2)
!!     = f(\mathbf{x}) + h_1 f'(\mathbf{x}) \varepsilon_1
!!       + h_2 f'(\mathbf{x}) \varepsilon_2
!!       + h_1 h_2 f''(\mathbf{x}) \varepsilon_1 \varepsilon_2
!!  \f]
!!  Because there is _no truncation error_, all first and second order
!!  derivatives can be obtained _exactly_, regardless of the step size ''
!!  \f$h_1\f$ and \f$h_2\f$.
!!  The derivatives can be obtained for a function \f$ f(\mathbf{x}) \f$
!!  with multiple variables \f$ \mathbf{x} \in \mathbb{R}^n \f$ via
!!  \f{eqnarray*}{
!!    \frac{\partial f(\mathbf{x})}{\partial x_i} &=& \frac{
!!      \varepsilon_{1, \mathrm{part}} \Big\{
!!      f(\mathbf{x} + h_1 \varepsilon_1 \mathbf{e}_i
!!      + h_2 \varepsilon_2 \mathbf{e}_j + h_1 h_2 \mathbf{0})\Big\}}
!!      {h_1}\\
!!    \frac{\partial f(\mathbf{x})}{\partial x_i} &=& \frac{
!!       \varepsilon_{2, \mathrm{part}} \Big\{
!!       f(\mathbf{x} + h_1 \varepsilon_1 \mathbf{e}_i
!!       + h_2 \varepsilon_2 \mathbf{e}_j + h_1 h_2 \mathbf{0})\Big\}}
!!       {h_2}\\
!!    \frac{\partial^2 f(\mathbf{x})}{\partial x_i \partial x_j} &=&
!!      \frac{(\varepsilon_1 \varepsilon_2)_\mathrm{part} \Big\{
!!      f(\mathbf{x} + h_1 \varepsilon_1 \mathbf{e}_i
!!      + h_2 \varepsilon_2 \mathbf{e}_j + h_1 h_2 \mathbf{0})\Big\}}
!!      {h_1 h_2}  \\
!!  \f}
!!  where \f$\mathbf{e}_i\f$ and \f$\mathbf{e}_j\f$ are unit vectors,
!!  which are all zero except for the \f$i\f$-th and \f$j\f$-th
!!  component, respectively.
!!
!!  #### Computation principles for hypderdual numbers
!!
!!  Hyperdual numbers \f$\mathbf{x} \in \mathbb{HD}\f$ can be expressed
!!  as tuples: \f$\mathbf{x} = [x_0, x_1, x_2, x_{12}] = x_0
!!  + x_1 \varepsilon_1 + x_2 \varepsilon_2
!!  + x_{12} \varepsilon_1\varepsilon_2\f$.
!!  By using the Taylor expansion of the function \f$f(\mathbf{x})\f$
!!  one gets computation priniple for functions with hyperdual
!!  arguments from
!!  \f[
!!     f(\mathbf{x}) = f(x_0) + x_1 f'(x_0) \varepsilon_1
!!     + x_2 f'(x_0) \varepsilon_2 + \big( x_{12} f'(x_0)
!!     + x_1 x_2 f''(x_0) \big) \varepsilon_1 \varepsilon_2
!!  \f]
!!
!!  A hyperdual number derived type is provided by: \ref hyperdual.
!!
!!  #### References
!!
!!  [[1]](https://doi.org/10.2514/6.2011-886)
!!       Fike, Alonso: **The Development of Hyper-Dual Numbers for Exact
!!                       Second-Derivative Calculations.**
!!       _49th AIAA Aerospace Sciences Meeting including the New
!!        Horizons Forum and Aerospace Exposition_ (2011) \n
!!  [[2]](https://doi.org/10.3389/fceng.2021.758090)
!!       Rehner, P. and Bauer, G.: **Application of Generalized
!!                                   (Hyper-) Dual Numbers in Equation
!!                                   of State Modeling.**
!!       Frontiers in Chemical Engineering_ (2021) \n
!!
module hyperdual_mod
  use iso_fortran_env, only: dp => REAL64
  implicit none

  !> Derived type for hyperdual numbers
  !!
  !!  Hyperdual numbers are represented by the tuple \f$\mathbf{f} =
  !!  [f_0, f_1, f_2, f_{12}] = f_0 + f_1 \varepsilon_1
  !!  + f_2 \varepsilon_2 + f_{12} \varepsilon_1 \varepsilon_2 \f$.
  !!  Calculations specificaions are defined in module hyperdual_mod.
  type hyperdual
    sequence
    real(dp) :: f0 = 0  !< real part of the hyperdual number
    real(dp) :: f1 = 0  !< \f$\varepsilon_1\f$-part of  the hyperdual number
    real(dp) :: f2 = 0  !< \f$\varepsilon_2\f$-part of  the hyperdual number
    real(dp) :: f12 = 0 !< \f$\varepsilon_1\varepsilon_2\f$-part of the
  end type hyperdual


  !---------------------------------------------------------------------
  !--- Operator interfaces ---------------------------------------------
  !---------------------------------------------------------------------

  ! Equal assignment
  interface assignment (=)
    procedure EqualHyperDualHyperDual
    procedure EqualHyperDualReal
  end interface

  ! Unary operator +
  interface operator (+)
    procedure PlusHyperDualHyperDual
  end interface

  ! Addition operator
  interface operator (+)
    procedure AddHyperDualHyperDual
    procedure AddHyperDualReal
    procedure AddRealHyperDual
  end interface

  ! Unary operator -
  interface operator (-)
    procedure MinusHyperDualHyperDual
  end interface

  ! Subtraction operator
  interface operator (-)
    procedure SubtractHyperDualHyperDual
    procedure SubtractHyperDualReal
    procedure SubtractRealHyperDual
  end interface

  ! Multiplication operator
  interface operator (*)
    procedure MultiplyHyperDualHyperDual
    procedure MultiplyHyperDualReal
    procedure MultiplyRealHyperDual
    procedure MultiplyHyperDualInt
    procedure MultiplyIntHyperDual
  end interface

  ! Division operator
  interface operator (/)
    procedure DivideHyperDualHyperDual
    procedure DivideHyperDualReal
    procedure DivideRealHyperDual
  end interface

  ! Power operator
  interface operator (**)
    procedure PowerHyperDualInt
    procedure PowerHyperDualHyperDual
    procedure PowerHyperDualReal
  end interface



  !---------------------------------------------------------------------
  !--- Summation interface ---------------------------------------------
  !---------------------------------------------------------------------
  interface sum
    module procedure SumHyperDual
    module procedure SumHyperDual2
  end interface sum



  !---------------------------------------------------------------------
  !--- Logical operator interfaces -------------------------------------
  !---------------------------------------------------------------------

  ! Equal operator.
  interface operator (.eq.)  ! or (==)
    procedure eq_dd
    procedure eq_dr
    procedure eq_rd
    procedure eq_di
    procedure eq_id
  end interface

  ! Not equal operator.
  interface operator (.ne.)  ! or (/=)
    procedure ne_dd
    procedure ne_dr
    procedure ne_rd
    procedure ne_di
    procedure ne_id
  end interface

  ! Less than operator.
  interface operator (.lt.)  ! or (<)
    procedure lt_dd
    procedure lt_dr
    procedure lt_rd
    procedure lt_di
    procedure lt_id
  end interface

  ! Less than or equal operator.
  interface operator (.le.)  ! or (<=)
    procedure le_dd
    procedure le_dr
    procedure le_rd
    procedure le_di
    procedure le_id
  end interface

  ! Greater than operator.
  interface operator (.gt.)  ! or (>)
    procedure gt_dd
    procedure gt_dr
    procedure gt_rd
    procedure gt_di
    procedure gt_id
  end interface

  ! Greater than or equal operator.
  interface operator (.ge.)  ! or (>=)
    procedure ge_dd
    procedure ge_dr
    procedure ge_rd
    procedure ge_di
    procedure ge_id
  end interface



  !---------------------------------------------------------------------
  !--- Math function interfaces ----------------------------------------
  !---------------------------------------------------------------------

  ! Absolute value function
  interface abs
    module procedure absHyperDual
  end interface

  ! Integer function
  interface int
    module procedure intHyperDual
  end interface

  ! Nearest integer function
  interface nint
    module procedure nintHyperDual
  end interface

  ! Real function
  interface real
    module procedure realHyperDual
  end interface

  ! Sign function
  interface sign
    module procedure sign_dd
    module procedure sign_dr
    module procedure sign_rd
  end interface

  ! Sine function
  interface sin
    module procedure sinHyperDual
  end interface

  ! Cosine function
  interface cos
    module procedure cosHyperDual
  end interface

  ! Tangent function
  interface tan
    module procedure tanHyperDual
  end interface

  ! Sqrt function
  interface sqrt
    module procedure sqrtHyperDual
  end interface

  ! Log function
  interface log
    module procedure logHyperDual
  end interface

  ! Log10 function
  interface log10
    module procedure log10HyperDual
  end interface

  ! Exp function
  interface exp
    module procedure expHyperDual
  end interface

  ! Sinh function
  interface sinh
    module procedure sinhHyperDual
  end interface

  ! Cosh function
  interface cosh
    module procedure coshHyperDual
  end interface

  ! Tanh function
  interface tanh
    module procedure tanhHyperDual
  end interface

  ! Acos function
  interface acos
    module procedure acosHyperDual
  end interface

  ! Asin function
  interface asin
    module procedure asinHyperDual
  end interface

  ! Atan function
  interface atan
    module procedure atanHyperDual
  end interface

  ! Atan2 function
  interface atan2
    module procedure atan2HyperDual
  end interface

  ! Max function (limited to combinations below, but that
  ! can be extended)
  interface max
    module procedure max_dd
    module procedure max_ddd
    module procedure max_dr
    module procedure max_rd
  end interface

  ! Min function (limited for now to 2 arguments, but that
  ! can be extended)
  interface min
    module procedure min_dd
    module procedure min_dr
    module procedure min_rd
  end interface

  !=====================================================================



  contains



    !-------------------------------------------------------------------
    !--- Functions for the equal assignment. ---------------------------
    !-------------------------------------------------------------------

    elemental subroutine EqualHyperDualHyperDual(res, inp)
      implicit none
      type (hyperdual), intent(out) :: res
      type (hyperdual), intent(in)  :: inp

      res%f0  = inp%f0
      res%f1  = inp%f1
      res%f2  = inp%f2
      res%f12 = inp%f12
    end subroutine EqualHyperDualHyperDual

    elemental subroutine EqualHyperDualReal(res, inp)
      implicit none
      type (hyperdual), intent(out) :: res
      real(dp),         intent(in)  :: inp

      res%f0  = inp
      res%f1  = 0.0_dp
      res%f2  = 0.0_dp
      res%f12 = 0.0_dp
    end subroutine EqualHyperDualReal



    !-------------------------------------------------------------------
    !--- Function for the unary operator +. ----------------------------
    !-------------------------------------------------------------------

    elemental function PlusHyperDualHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2

      v2%f0  = v1%f0
      v2%f1  = v1%f1
      v2%f2  = v1%f2
      v2%f12 = v1%f12
    end function PlusHyperDualHyperDual



    !-------------------------------------------------------------------
    !--- Functions for the addition operator. --------------------------
    !-------------------------------------------------------------------

    elemental function AddHyperDualHyperDual(v1,v2) result (v3)
      type (hyperdual), intent(in) :: v1, v2
      type (hyperdual)             :: v3

      v3%f0  = v1%f0  + v2%f0
      v3%f1  = v1%f1  + v2%f1
      v3%f2  = v1%f2  + v2%f2
      v3%f12 = v1%f12 + v2%f12
    end function AddHyperDualHyperDual

    elemental function AddHyperDualReal(v1,v2) result (v3)
      type (hyperdual), intent(in) :: v1
      real(dp),         intent(in) :: v2
      type (hyperdual)             :: v3

      v3%f0  = v1%f0 + v2
      v3%f1  = v1%f1
      v3%f2  = v1%f2
      v3%f12 = v1%f12
    end function AddHyperDualReal

    elemental function AddRealHyperDual(v1,v2) result (v3)
      real(dp),         intent(in) :: v1
      type (hyperdual), intent(in) :: v2
      type (hyperdual)             :: v3

      v3%f0  = v1 + v2%f0
      v3%f1  =      v2%f1
      v3%f2  =      v2%f2
      v3%f12 =      v2%f12
    end function AddRealHyperDual



    !-------------------------------------------------------------------
    !--- Function for the unary operator -. ----------------------------
    !-------------------------------------------------------------------

    elemental function MinusHyperDualHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2

      v2%f0  = -v1%f0
      v2%f1  = -v1%f1
      v2%f2  = -v1%f2
      v2%f12 = -v1%f12
    end function MinusHyperDualHyperDual



    !-------------------------------------------------------------------
    !--- Functions for the subtraction operator. -----------------------
    !-------------------------------------------------------------------

    elemental function SubtractHyperDualHyperDual(v1,v2) result (v3)
      type (hyperdual), intent(in) :: v1, v2
      type (hyperdual)             :: v3

      v3%f0  = v1%f0  - v2%f0
      v3%f1  = v1%f1  - v2%f1
      v3%f2  = v1%f2  - v2%f2
      v3%f12 = v1%f12 - v2%f12
    end function SubtractHyperDualHyperDual

    elemental function SubtractHyperDualReal(v1,v2) result (v3)
      type (hyperdual), intent(in) :: v1
      real(dp),         intent(in) :: v2
      type (hyperdual)             :: v3

      v3%f0  = v1%f0 - v2
      v3%f1  = v1%f1
      v3%f2  = v1%f2
      v3%f12 = v1%f12
    end function SubtractHyperDualReal

    elemental function SubtractRealHyperDual(v1,v2) result (v3)
      real(dp),         intent(in) :: v1
      type (hyperdual), intent(in) :: v2
      type (hyperdual)             :: v3

      v3%f0  = v1 - v2%f0
      v3%f1  =    - v2%f1
      v3%f2  =    - v2%f2
      v3%f12 =    - v2%f12
    end function SubtractRealHyperDual



    !-------------------------------------------------------------------
    !--- Functions for the multiplication operator. --------------------
    !-------------------------------------------------------------------

    elemental function MultiplyHyperDualHyperDual(v1,v2) result (v3)
      type (hyperdual), intent(in) :: v1, v2
      type (hyperdual)             :: v3

      v3%f0  = v1%f0 * v2%f0
      v3%f1  = v1%f0 * v2%f1 + v1%f1 * v2%f0
      v3%f2  = v1%f0 * v2%f2 + v1%f2 * v2%f0
      v3%f12 = v1%f0*v2%f12 + v1%f1*v2%f2 + v1%f2*v2%f1 + v1%f12*v2%f0
    end function MultiplyHyperDualHyperDual

    elemental function MultiplyHyperDualReal(v1,v2) result (v3)
      type (hyperdual), intent(in) :: v1
      real(dp),         intent(in) :: v2
      type (hyperdual)             :: v3

      v3%f0  = v1%f0  * v2
      v3%f1  = v1%f1  * v2
      v3%f2  = v1%f2  * v2
      v3%f12 = v1%f12 * v2
    end function MultiplyHyperDualReal

    elemental function MultiplyRealHyperDual(v1,v2) result (v3)
      real(dp),         intent(in) :: v1
      type (hyperdual), intent(in) :: v2
      type (hyperdual)             :: v3

      v3%f0  = v1 * v2%f0
      v3%f1  = v1 * v2%f1
      v3%f2  = v1 * v2%f2
      v3%f12 = v1 * v2%f12
    end function MultiplyRealHyperDual

    elemental function MultiplyHyperDualInt(v1,v2) result (v3)
      type (hyperdual), intent(in) :: v1
      integer,          intent(in) :: v2
      type (hyperdual)             :: v3

      v3%f0  = v1%f0  * v2
      v3%f1  = v1%f1  * v2
      v3%f2  = v1%f2  * v2
      v3%f12 = v1%f12 * v2
    end function MultiplyHyperDualInt

    elemental function MultiplyIntHyperDual(v1,v2) result (v3)
      integer,          intent(in) :: v1
      type (hyperdual), intent(in) :: v2
      type (hyperdual)             :: v3

      v3%f0  = v1 * v2%f0
      v3%f1  = v1 * v2%f1
      v3%f2  = v1 * v2%f2
      v3%f12 = v1 * v2%f12
    end function MultiplyIntHyperDual



    !-------------------------------------------------------------------
    !--- Functions for the division operator. --------------------------
    !-------------------------------------------------------------------

    elemental function DivideHyperDualHyperDual(v1,v2) result (v3)
      type (hyperdual), intent(in) :: v1, v2
      type (hyperdual)             :: v3

      v3 = v1 * v2**(-1.0_dp)
    end function DivideHyperDualHyperDual

    elemental function DivideHyperDualReal(v1,v2) result (v3)
    type (hyperdual), intent(in) :: v1
    real(dp),         intent(in) :: v2
    type (hyperdual)             :: v3
    real(dp)                     :: invV2

      invV2 = 1.0_dp / v2
      v3 = v1 * invV2
    end function DivideHyperDualReal

    elemental function DivideRealHyperDual(v1,v2) result (v3)
      real(dp), intent(in) :: v1
      type (hyperdual), intent(in) :: v2
      type (hyperdual)             :: invV2, v3

      invV2 = 1.0_dp * v2**(-1.0_dp)
      v3 = v1 * invV2
    end function DivideRealHyperDual



    !-------------------------------------------------------------------
    !--- Functions for the power operator. -----------------------------
    !-------------------------------------------------------------------

    elemental function PowerHyperDualInt(v1,v2) result (v3)
      type (hyperdual), intent(in) :: v1
      integer,          intent(in) :: v2
      integer                      :: i, vv2
      type (hyperdual)             :: v3

      v3  = 1.0_dp
      vv2 = abs(v2)
      do i=1,vv2
        v3 = v3*v1
      enddo

      if(v2 < 0) v3 = 1.0_dp / v3
    end function PowerHyperDualInt

    elemental function PowerHyperDualHyperDual(v1,v2) result (v3)
      type (hyperdual), intent(in) :: v1, v2
      type (hyperdual)             :: v3, v4

      v4 = logHyperDual(v1)
      v3 = expHyperDual(v2*v4)
    end function PowerHyperDualHyperDual

    elemental function PowerHyperDualReal(v1,v2) result (v3)
      type (hyperdual),      intent(in) :: v1
      real(dp),              intent(in) :: v2
      type (hyperdual)                  :: v3

      real(dp), parameter               :: tol = 1.0e-15_dp
      real(dp)                          :: xval, deriv

      xval = v1%f0
      if(abs(xval) < tol) then
        if(xval >= 0.0_dp) then
          xval = tol
        else
          xval = -tol
        endif
      endif

      deriv  = v2*(xval**(v2-1.0_dp))
      v3%f0  = (v1%f0)**v2
      v3%f1  =  v1%f1 * deriv
      v3%f2  =  v1%f2 * deriv
      v3%f12 = v1%f12 * deriv &
               & + v2*(v2 - 1.0_dp)*v1%f1*v1%f2*xval**(v2-2.0_dp)
    end function PowerHyperDualReal



    !-------------------------------------------------------------------
    !--- Sum -----------------------------------------------------------
    !-------------------------------------------------------------------
    pure type(hyperdual) function SumHyperDual(v1, mask)
      type(hyperdual), intent(in) :: v1(:)
      logical, intent(in), optional :: mask(:)
      integer :: i

      SumHyperDual = hyperdual(0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp)
      if (present(mask)) then
        do i = 1, size(v1)
          if(mask(i)) SumHyperDual = SumHyperDual + v1(i)
      end do
      else
        do i = 1, size(v1)
            SumHyperDual = SumHyperDual + v1(i)
        end do
      end if
    end function SumHyperDual

    pure function SumHyperDual2(v1, dim)
      type(hyperdual), intent(in) :: v1(:,:)
      integer, intent(in) :: dim
      type(hyperdual), allocatable :: SumHyperDual2(:)
      integer                     :: i

      allocate(SumHyperDual2(size(v1)/size(v1,dim)))

      SumHyperDual2 = hyperdual(0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp)
      do i = 1, size(v1,dim)
          if (dim == 1) then
            SumHyperDual2 = SumHyperDual2 + v1(i,:)
          else
            SumHyperDual2 = SumHyperDual2 + v1(:,i)
          end if
      end do
    end function SumHyperDual2



    !-------------------------------------------------------------------
    !--- Functions for the equal operator. -----------------------------
    !-------------------------------------------------------------------

    logical function eq_dd(lhs, rhs)
      type (hyperdual), intent(in) :: lhs, rhs

      eq_dd = lhs%f0 == rhs%f0
    end function eq_dd

    logical function eq_dr(lhs, rhs)
      type (hyperdual), intent(in) :: lhs
      real(dp),         intent(in) :: rhs

      eq_dr = lhs%f0 == rhs
    end function eq_dr

    logical function eq_rd(lhs, rhs)
      real(dp),         intent(in) :: lhs
      type (hyperdual), intent(in) :: rhs

      eq_rd = lhs == rhs%f0
    end function eq_rd

    logical function eq_di(lhs, rhs)
      type (hyperdual), intent(in) :: lhs
      integer,          intent(in) :: rhs

      eq_di = lhs%f0 == rhs
    end function eq_di

    logical function eq_id(lhs, rhs)
      integer,          intent(in) :: lhs
      type (hyperdual), intent(in) :: rhs

      eq_id = lhs == rhs%f0
    end function eq_id



    !-------------------------------------------------------------------
    !--- Functions for the not equal operator. -------------------------
    !-------------------------------------------------------------------

    logical function ne_dd(lhs, rhs)
      type (hyperdual), intent(in) :: lhs, rhs

      ne_dd = lhs%f0 /= rhs%f0
    end function ne_dd

    logical function ne_dr(lhs, rhs)
      type (hyperdual), intent(in) :: lhs
      real(dp),         intent(in) :: rhs

      ne_dr = lhs%f0 /= rhs
    end function ne_dr

    logical function ne_rd(lhs, rhs)
      real(dp),         intent(in) :: lhs
      type (hyperdual), intent(in) :: rhs

      ne_rd = lhs /= rhs%f0
    end function ne_rd

    logical function ne_di(lhs, rhs)
      type (hyperdual), intent(in) :: lhs
      integer,          intent(in) :: rhs

      ne_di = lhs%f0 /= rhs
    end function ne_di

    logical function ne_id(lhs, rhs)
      integer,          intent(in) :: lhs
      type (hyperdual), intent(in) :: rhs

      ne_id = lhs /= rhs%f0
    end function ne_id



    !-------------------------------------------------------------------
    !--- Functions for the less than operator. -------------------------
    !-------------------------------------------------------------------

    logical function lt_dd(lhs, rhs)
      type (hyperdual), intent(in) :: lhs, rhs

      lt_dd = lhs%f0 < rhs%f0
    end function lt_dd

    logical function lt_dr(lhs, rhs)
      type (hyperdual), intent(in) :: lhs
      real(dp),         intent(in) :: rhs

      lt_dr = lhs%f0 < rhs
    end function lt_dr

    logical function lt_rd(lhs, rhs)
      real(dp),         intent(in) :: lhs
      type (hyperdual), intent(in) :: rhs

      lt_rd = lhs < rhs%f0
    end function lt_rd

    logical function lt_di(lhs, rhs)
      type (hyperdual), intent(in) :: lhs
      integer,          intent(in) :: rhs

      lt_di = lhs%f0 < rhs
    end function lt_di

    logical function lt_id(lhs, rhs)
      integer,          intent(in) :: lhs
      type (hyperdual), intent(in) :: rhs

      lt_id = lhs < rhs%f0
    end function lt_id



    !-------------------------------------------------------------------
    !--- Functions for the less than or equal operator. ----------------
    !-------------------------------------------------------------------

    logical function le_dd(lhs, rhs)
      type (hyperdual), intent(in) :: lhs, rhs

      le_dd = lhs%f0 <= rhs%f0
    end function le_dd

    logical function le_dr(lhs, rhs)
      type (hyperdual), intent(in) :: lhs
      real(dp),         intent(in) :: rhs

      le_dr = lhs%f0 <= rhs
    end function le_dr

    logical function le_rd(lhs, rhs)
      real(dp),         intent(in) :: lhs
      type (hyperdual), intent(in) :: rhs

      le_rd = lhs <= rhs%f0
    end function le_rd

    logical function le_di(lhs, rhs)
      type (hyperdual), intent(in) :: lhs
      integer,          intent(in) :: rhs

      le_di = lhs%f0 <= rhs
    end function le_di

    logical function le_id(lhs, rhs)
      integer,          intent(in) :: lhs
      type (hyperdual), intent(in) :: rhs

      le_id = lhs <= rhs%f0
    end function le_id



    !-------------------------------------------------------------------
    !--- Functions for the greater than operator. ----------------------
    !-------------------------------------------------------------------

    logical function gt_dd(lhs, rhs)
      type (hyperdual), intent(in) :: lhs, rhs

      gt_dd = lhs%f0 > rhs%f0
    end function gt_dd

    logical function gt_dr(lhs, rhs)
      type (hyperdual), intent(in) :: lhs
      real(dp),         intent(in) :: rhs

      gt_dr = lhs%f0 > rhs
    end function gt_dr

    logical function gt_rd(lhs, rhs)
      real(dp),         intent(in) :: lhs
      type (hyperdual), intent(in) :: rhs

      gt_rd = lhs > rhs%f0
    end function gt_rd

    logical function gt_di(lhs, rhs)
      type (hyperdual), intent(in) :: lhs
      integer,          intent(in) :: rhs

      gt_di = lhs%f0 > rhs
    end function gt_di

    logical function gt_id(lhs, rhs)
      integer,          intent(in) :: lhs
      type (hyperdual), intent(in) :: rhs

      gt_id = lhs > rhs%f0
    end function gt_id



    !-------------------------------------------------------------------
    !--- Functions for the greater than or equal operator. -------------
    !-------------------------------------------------------------------

    logical function ge_dd(lhs, rhs)
      type (hyperdual), intent(in) :: lhs, rhs

      ge_dd = lhs%f0 >= rhs%f0
    end function ge_dd

    logical function ge_dr(lhs, rhs)
      type (hyperdual), intent(in) :: lhs
      real(dp),         intent(in) :: rhs

      ge_dr = lhs%f0 >= rhs
    end function ge_dr

    logical function ge_rd(lhs, rhs)
      real(dp),         intent(in) :: lhs
      type (hyperdual), intent(in) :: rhs

      ge_rd = lhs >= rhs%f0
    end function ge_rd

    logical function ge_di(lhs, rhs)
      type (hyperdual), intent(in) :: lhs
      integer,          intent(in) :: rhs

      ge_di = lhs%f0 >= rhs
    end function ge_di

    logical function ge_id(lhs, rhs)
      integer,          intent(in) :: lhs
      type (hyperdual), intent(in) :: rhs

      ge_id = lhs >= rhs%f0
    end function ge_id



    !-------------------------------------------------------------------
    !--- Math functions. -----------------------------------------------
    !-------------------------------------------------------------------

    ! Absolute value function.
    elemental function absHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2

      if(v1%f0 >= 0.0) then
        v2%f0  = v1%f0
        v2%f1  = v1%f1
        v2%f2  = v1%f2
        v2%f12 = v1%f12
      else
        v2%f0  = -v1%f0
        v2%f1  = -v1%f1
        v2%f2  = -v1%f2
        v2%f12 = -v1%f12
      endif
    end function absHyperDual

    ! Integer function.
    elemental function intHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      integer                      :: v2

      v2 = int(v1%f0)
    end function intHyperDual

    ! Nearest integer function.
    elemental function nintHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      integer                      :: v2

      v2 = nint(v1%f0)
    end function nintHyperDual

    ! Real function.
    elemental function realHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      real(dp)                     :: v2

      v2 = v1%f0
    end function realHyperDual

    ! Functions for the sign function.
    elemental function sign_dd(v1,v2) result (v3)
      type (hyperdual), intent(in) :: v1, v2
      type (hyperdual)             :: v3
      real(dp)                     :: ssign

      if(v2%f0 < 0.0) then
        ssign = -1.0
      else
        ssign =  1.0
      endif
      v3 = ssign*v1
    end function sign_dd

    elemental function sign_dr(v1,v2) result (v3)
      type (hyperdual), intent(in) :: v1
      real(dp),         intent(in) :: v2
      type (hyperdual)             :: v3
      real(dp)                     :: ssign

      if(v2 < 0.0) then
        ssign = -1.0
      else
        ssign =  1.0
      endif
      v3 = ssign*v1
    end function sign_dr

    elemental function sign_rd(v1,v2) result (v3)
      real(dp),         intent(in) :: v1
      type (hyperdual), intent(in) :: v2
      type (hyperdual)             :: v3
      real(dp)                     :: ssign

      if(v2%f0 < 0.0) then
        ssign = -1.0
      else
        ssign =  1.0
      endif
      v3 = ssign*v1
    end function sign_rd

    ! Sine function.
    elemental function sinHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2
      real(dp)                     :: f, dx

      f = sin(v1%f0)
      dx = cos(v1%f0)
      v2%f0  = f
      v2%f1  = dx * v1%f1
      v2%f2  = dx * v1%f2
      v2%f12 = dx * v1%f12 - f * v1%f1 * v1%f2
    end function sinHyperDual

    ! Cosine function.
    elemental function cosHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2
      real(dp)                     :: f, dx

      f = cos(v1%f0)
      dx = -sin(v1%f0)
      v2%f0  = f
      v2%f1  = dx * v1%f1
      v2%f2  = dx * v1%f2
      v2%f12 = dx * v1%f12 - f * v1%f1 * v1%f2
    end function cosHyperDual

    ! Tangent function.
    elemental function tanHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2
      real(dp)                     :: f, dx

      f = tan(v1%f0)
      dx = f * f + 1.0_dp
      v2%f0  = f
      v2%f1  = dx * v1%f1
      v2%f2  = dx * v1%f2
      v2%f12 = dx * v1%f12 + v1%f1 * v1%f2 * 2.0_dp * f * dx
    end function tanHyperDual

    ! Sqrt function
    elemental function sqrtHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2

      v2 = v1**(0.5_dp)
    end function sqrtHyperDual

    ! Log function
    elemental function logHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2
      real(dp)                     :: dx1, dx2

      dx1 = v1%f1 / v1%f0
      dx2 = v1%f2 / v1%f0
      v2%f0  = log(v1%f0)
      v2%f1  = dx1
      v2%f2  = dx2
      v2%f12 = v1%f12 / v1%f0 - (dx1 * dx2)
    end function logHyperDual

    ! Log10 function
    elemental function log10HyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2

      v2 = log(v1)/log(10.0_dp)
    end function log10HyperDual

    ! Exp function
    elemental function expHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2
      real(dp)                     :: dx

      dx = exp(v1%f0)
      v2%f0  = dx
      v2%f1  = dx * v1%f1
      v2%f2  = dx * v1%f2
      v2%f12 = dx * (v1%f12 + v1%f1 * v1%f2)
    end function expHyperDual

    ! Sinh function
    elemental function sinhHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: t1, t2, v2

      t1 = exp(v1)
      t2 = exp(-v1)
      v2 = 0.5_dp*(t1-t2)
    end function sinhHyperDual

    ! Cosh function
    elemental function coshHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: t1, t2, v2

      t1 = exp(v1)
      t2 = exp(-v1)
      v2 = 0.5_dp*(t1+t2)
    end function coshHyperDual

    ! Tanh function
    elemental function tanhHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: t1, t2, v2

      t1 = exp(v1)
      t2 = exp(-v1)
      v2 = (t1-t2)/(t1+t2)
    end function tanhHyperDual

    ! Acos function
    elemental function acosHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2
      real(dp)                     :: deriv, deriv1

      deriv1 = 1.0_dp - v1%f0*v1%f0
      deriv  = -1.0_dp / sqrt(deriv1)
      v2%f0  = acos(v1%f0)
      v2%f1  = deriv*v1%f1
      v2%f2  = deriv*v1%f2
      v2%f12 = deriv*v1%f12 &
             & + v1%f1 * v1%f2 * (-v1%f0 * deriv1**(-1.5_dp))
    end function acosHyperDual

    ! Asin function
    elemental function asinHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2
      real(dp)                     :: deriv, deriv1

      deriv1 = 1.0_dp - v1%f0*v1%f0
      deriv  = 1.0_dp / sqrt(deriv1)
      v2%f0  = asin(v1%f0)
      v2%f1  = deriv*v1%f1
      v2%f2  = deriv*v1%f2
      v2%f12 = deriv*v1%f12 &
             & + v1%f1 * v1%f2 * (v1%f0 * deriv1**(-1.5_dp))
    end function asinHyperDual

    ! Atan function
    elemental function atanHyperDual(v1) result (v2)
      type (hyperdual), intent(in) :: v1
      type (hyperdual)             :: v2
      real(dp)                     :: deriv, deriv1

      deriv1 = 1.0_dp + v1%f0*v1%f0
      deriv  = 1.0_dp / deriv1
      v2%f0  = atan(v1%f0)
      v2%f1  = deriv*v1%f1
      v2%f2  = deriv*v1%f2
      v2%f12 = deriv*v1%f12 &
             & + v1%f1 * v1%f2 * (-2.0_dp * v1%f0 / (deriv1 * deriv1))
    end function atanHyperDual

    ! Atan2 function
    elemental function atan2HyperDual(v1, v2) result (v3)
      type (hyperdual), intent(in) :: v1, v2
      type (hyperdual)             :: v3
      real(dp)                     :: a, b, c, d

      a = v1%f0; b = v1%f1
      c = v2%f0; d = v2%f1

      v3%f0 = atan2(a,c)
      v3%f1 = (c*b - a*d)/(a*a + c*c)
    end function atan2HyperDual

    ! Max functions
    elemental function max_dd(v1, v2) result (v3)
      type (hyperdual), intent(in) :: v1, v2
      type (hyperdual)             :: v3

      if(v1%f0 > v2%f0) then
        v3 = v1
      else
        v3 = v2
      endif
    end function max_dd

    elemental function max_ddd(v1, v2, v3) result (v4)
      type (hyperdual), intent(in) :: v1, v2, v3
      type (hyperdual)             :: v4

      if(v1%f0 > v2%f0) then
        v4 = v1
      else
        v4 = v2
      endif

      if(v3%f0 > v4%f0) v4 = v3
    end function max_ddd

    elemental function max_dr(v1, v2) result (v3)
      type (hyperdual), intent(in) :: v1
      real(dp),         intent(in) :: v2
      type (hyperdual)             :: v3

      if(v1%f0 > v2) then
        v3 = v1
      else
        v3 = v2
      endif
    end function max_dr

    elemental function max_rd(v1, v2) result (v3)
      real(dp),         intent(in) :: v1
      type (hyperdual), intent(in) :: v2
      type (hyperdual)             :: v3

      if(v1 > v2%f0) then
        v3 = v1
      else
        v3 = v2
      endif
    end function max_rd

    ! Min functions
    elemental function min_dd(v1, v2) result (v3)
      type (hyperdual), intent(in) :: v1, v2
      type (hyperdual)             :: v3

      if(v1%f0 < v2%f0) then
        v3 = v1
      else
        v3 = v2
      endif
    end function min_dd

    elemental function min_dr(v1, v2) result (v3)
      type (hyperdual), intent(in) :: v1
      real(dp),         intent(in) :: v2
      type (hyperdual)             :: v3

      if(v1%f0 < v2) then
        v3 = v1
      else
        v3 = v2
      endif
    end function min_dr

    elemental function min_rd(v1, v2) result (v3)
      real(dp),         intent(in) :: v1
      type (hyperdual), intent(in) :: v2
      type (hyperdual)             :: v3

      if(v1 < v2%f0) then
        v3 = v1
      else
        v3 = v2
      endif
    end function min_rd

end module hyperdual_mod
