MODULE notidgas_mod
  use base_eos, only: BaseEos
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: NotIdGas, NotIdGas_ctor_sub

  !> Not Ideal gas EoS
  TYPE, EXTENDS(BaseEos) :: NotIdGas
  CONTAINS
    PROCEDURE :: Fres => NotIdGas_Fres
  END TYPE NotIdGas

  INTERFACE NotIdGas
    PROCEDURE NotIdGas_ctor
  END INTERFACE NotIdGas

Contains

  function NotIdGas_ctor(ident, ncomps, Tc, Vc) result(instance)
    INTEGER, INTENT(in) :: ident
    integer, intent(in) :: ncomps
    real, intent(in) :: Tc, Vc ! Read these from some database
    TYPE (NotIdGas) :: instance

    instance%ident = ident
    instance%ncomps = ncomps
    instance%Tc = Tc
    instance%Vc = Vc
  END function NotIdGas_ctor

  subroutine NotIdGas_ctor_sub(instance, ident, nc, Tc, Vc)
    TYPE (NotIdGas) :: instance
    INTEGER, INTENT(in) :: ident
    integer, intent(in) :: nc
    real, intent(in) :: Tc, Vc ! Read these from some database
    
    instance = NotIdGas(ident, nc, Tc, Vc)
  END subroutine NotIdGas_ctor_sub

  !> Compute residual and derivatives for ideal gas
  subroutine NotIdGas_Fres(this, T, V, n, Fres, Ft, Fv, Fn)
    CLASS(NotIdGas), INTENT(in) :: this
    real, intent(in) :: T, V
    real, dimension(this%ncomps), intent(in) :: n
    real, intent(out) :: Fres
    real, optional, intent(out) :: Ft, Fv
    real, optional, dimension(this%ncomps), intent(out) :: Fn
    integer :: i

    Fres = T / this%Tc + (V / sum(n)) / this%Vc
    Ft = 1 / this%Tc
    Fv = 1 / (sum(n) * this%Vc) 
    do i = 1,this%ncomps
        Fn(i) = 1 / (this%Vc * n(i))
    enddo
  END subroutine NotIdGas_Fres

end MODULE notidgas_mod