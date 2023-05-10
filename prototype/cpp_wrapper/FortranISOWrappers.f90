MODULE constants_wrap_

  USE ISO_C_BINDING
  IMPLICIT NONE

CONTAINS

  ! C binding wrappers:

END MODULE constants_wrap_


MODULE base_eos_wrap_

  USE ISO_C_BINDING
  USE base_eos
  IMPLICIT NONE

  TYPE BaseEos_container_
    CLASS (BaseEos), ALLOCATABLE :: c
  END TYPE BaseEos_container_

CONTAINS

  ! C binding wrappers:

  SUBROUTINE base_eos__fideal_wrap(c_this, T, V, n, Fid, Ft, Fv, Fn) BIND(C)
    TYPE(C_PTR), VALUE :: c_this
    REAL(C_FLOAT) :: T
    REAL(C_FLOAT) :: V
    REAL(C_FLOAT), DIMENSION(*) :: n
    REAL(C_FLOAT) :: Fid
    REAL(C_FLOAT), OPTIONAL :: Ft
    REAL(C_FLOAT), OPTIONAL :: Fv
    REAL(C_FLOAT), OPTIONAL, DIMENSION(*) :: Fn
    TYPE(BaseEos_container_), POINTER :: c_this__p
    CALL C_F_POINTER(c_this, c_this__p)
    CALL c_this__p%c%Fideal( T, V, n, Fid, Ft, Fv, Fn)
  END SUBROUTINE base_eos__fideal_wrap

  SUBROUTINE base_eos__pressure_wrap(c_this, T, V, n, p) BIND(C)
    TYPE(C_PTR), VALUE :: c_this
    REAL(C_FLOAT) :: T
    REAL(C_FLOAT) :: V
    REAL(C_FLOAT), DIMENSION(*) :: n
    REAL(C_FLOAT) :: p
    TYPE(BaseEos_container_), POINTER :: c_this__p
    CALL C_F_POINTER(c_this, c_this__p)
    CALL c_this__p%c%pressure( T, V, n, p)
  END SUBROUTINE base_eos__pressure_wrap

  SUBROUTINE base_eos__set_tc_wrap(c_this, Tc) BIND(C)
    TYPE(C_PTR), VALUE :: c_this
    REAL(C_FLOAT) :: Tc
    TYPE(BaseEos_container_), POINTER :: c_this__p
    CALL C_F_POINTER(c_this, c_this__p)
    CALL c_this__p%c%set_Tc( Tc)
  END SUBROUTINE base_eos__set_tc_wrap

END MODULE base_eos_wrap_


MODULE notidgas_mod_wrap_

  USE ISO_C_BINDING
  USE base_eos_wrap_
  USE base_eos
  USE notidgas_mod
  IMPLICIT NONE

CONTAINS

  ! Derived type allocate and deallocate functions:

  SUBROUTINE allocate_notidgas_(NotIdGas_cptr) BIND(C)
    TYPE (C_PTR) :: NotIdGas_cptr

    TYPE (BaseEos_container_), POINTER :: NotIdGas_fptr

    ALLOCATE( NotIdGas_fptr )
    ALLOCATE( NotIdGas::NotIdGas_fptr%c )
    NotIdGas_cptr = C_LOC(NotIdGas_fptr)
  END SUBROUTINE allocate_notidgas_

  SUBROUTINE deallocate_notidgas_(NotIdGas_cptr) BIND(C)
    TYPE (C_PTR), VALUE :: NotIdGas_cptr

    TYPE (BaseEos_container_), POINTER :: NotIdGas_fptr

    CALL C_F_POINTER(NotIdGas_cptr, NotIdGas_fptr)
    DEALLOCATE( NotIdGas_fptr )
  END SUBROUTINE deallocate_notidgas_

  ! C binding wrappers:

  SUBROUTINE notidgas_mod__notidgas_ctor_sub_wrap(instance, ident, nc, Tc, V&
    &c) BIND(C)
    TYPE(C_PTR), VALUE :: instance
    INTEGER(C_INT) :: ident
    INTEGER(C_INT) :: nc
    REAL(C_FLOAT) :: Tc
    REAL(C_FLOAT) :: Vc
    TYPE(BaseEos_container_), POINTER :: instance__p
    CALL C_F_POINTER(instance, instance__p)
    SELECT TYPE (instance__pp => instance__p%c)
    CLASS IS (NotIdGas)
      CALL NotIdGas_ctor_sub(instance__pp, ident, nc, Tc, Vc)
    END SELECT
  END SUBROUTINE notidgas_mod__notidgas_ctor_sub_wrap

  SUBROUTINE notidgas_mod__notidgas_fres_wrap(c_this, T, V, n, Fres, Ft, Fv,&
    & Fn) BIND(C)
    TYPE(C_PTR), VALUE :: c_this
    REAL(C_FLOAT) :: T
    REAL(C_FLOAT) :: V
    REAL(C_FLOAT), DIMENSION(*) :: n
    REAL(C_FLOAT) :: Fres
    REAL(C_FLOAT), OPTIONAL :: Ft
    REAL(C_FLOAT), OPTIONAL :: Fv
    REAL(C_FLOAT), OPTIONAL, DIMENSION(*) :: Fn
    TYPE(BaseEos_container_), POINTER :: c_this__p
    CALL C_F_POINTER(c_this, c_this__p)
    SELECT TYPE (c_this__pp => c_this__p%c)
    CLASS IS (NotIdGas)
      CALL c_this__pp%Fres( T, V, n, Fres, Ft, Fv, Fn)
    END SELECT
  END SUBROUTINE notidgas_mod__notidgas_fres_wrap

END MODULE notidgas_mod_wrap_


MODULE variants_wrap_

  USE ISO_C_BINDING
  USE base_eos_wrap_
  USE base_eos
  USE variants
  IMPLICIT NONE

CONTAINS

  ! Derived type allocate and deallocate functions:

  SUBROUTINE allocate_variant1_(Variant1_cptr) BIND(C)
    TYPE (C_PTR) :: Variant1_cptr

    TYPE (BaseEos_container_), POINTER :: Variant1_fptr

    ALLOCATE( Variant1_fptr )
    ALLOCATE( Variant1::Variant1_fptr%c )
    Variant1_cptr = C_LOC(Variant1_fptr)
  END SUBROUTINE allocate_variant1_

  SUBROUTINE deallocate_variant1_(Variant1_cptr) BIND(C)
    TYPE (C_PTR), VALUE :: Variant1_cptr

    TYPE (BaseEos_container_), POINTER :: Variant1_fptr

    CALL C_F_POINTER(Variant1_cptr, Variant1_fptr)
    DEALLOCATE( Variant1_fptr )
  END SUBROUTINE deallocate_variant1_

  ! Derived type allocate and deallocate functions:

  SUBROUTINE allocate_variant2_(Variant2_cptr) BIND(C)
    TYPE (C_PTR) :: Variant2_cptr

    TYPE (BaseEos_container_), POINTER :: Variant2_fptr

    ALLOCATE( Variant2_fptr )
    ALLOCATE( Variant2::Variant2_fptr%c )
    Variant2_cptr = C_LOC(Variant2_fptr)
  END SUBROUTINE allocate_variant2_

  SUBROUTINE deallocate_variant2_(Variant2_cptr) BIND(C)
    TYPE (C_PTR), VALUE :: Variant2_cptr

    TYPE (BaseEos_container_), POINTER :: Variant2_fptr

    CALL C_F_POINTER(Variant2_cptr, Variant2_fptr)
    DEALLOCATE( Variant2_fptr )
  END SUBROUTINE deallocate_variant2_

  ! C binding wrappers:

  SUBROUTINE variants__variant_common_comp_wrap(c_this, mixed_common) BIND(C&
    &)
    TYPE(C_PTR), VALUE :: c_this
    REAL(C_FLOAT) :: mixed_common
    TYPE(BaseEos_container_), POINTER :: c_this__p
    CALL C_F_POINTER(c_this, c_this__p)
    SELECT TYPE (c_this__pp => c_this__p%c)
    CLASS IS (VariantEoS)
      CALL c_this__pp%variant_common_comp( mixed_common)
    END SELECT
  END SUBROUTINE variants__variant_common_comp_wrap

  SUBROUTINE variants__variant1_ctor_sub_wrap(c_this, ident, nc, Tc, Vc, var&
    &1, var2) BIND(C)
    TYPE(C_PTR), VALUE :: c_this
    INTEGER(C_INT) :: ident
    INTEGER(C_INT) :: nc
    REAL(C_FLOAT) :: Tc
    REAL(C_FLOAT) :: Vc
    INTEGER(C_INT) :: var1
    REAL(C_FLOAT) :: var2
    TYPE(BaseEos_container_), POINTER :: c_this__p
    CALL C_F_POINTER(c_this, c_this__p)
    SELECT TYPE (c_this__pp => c_this__p%c)
    CLASS IS (Variant1)
      CALL Variant1_ctor_sub(c_this__pp, ident, nc, Tc, Vc, var1, var2)
    END SELECT
  END SUBROUTINE variants__variant1_ctor_sub_wrap

  SUBROUTINE variants__variant1_internal_comp_wrap(c_this, T, V, n, computed&
    &) BIND(C)
    TYPE(C_PTR), VALUE :: c_this
    REAL(C_FLOAT) :: T
    REAL(C_FLOAT) :: V
    REAL(C_FLOAT), DIMENSION(*) :: n
    REAL(C_FLOAT) :: computed
    TYPE(BaseEos_container_), POINTER :: c_this__p
    CALL C_F_POINTER(c_this, c_this__p)
    SELECT TYPE (c_this__pp => c_this__p%c)
    CLASS IS (Variant1)
      CALL c_this__pp%internal_comp( T, V, n, computed)
    END SELECT
  END SUBROUTINE variants__variant1_internal_comp_wrap

  SUBROUTINE variants__variant1_fres_wrap(c_this, T, V, n, Fres, Ft, Fv, Fn)&
    & BIND(C)
    TYPE(C_PTR), VALUE :: c_this
    REAL(C_FLOAT) :: T
    REAL(C_FLOAT) :: V
    REAL(C_FLOAT), DIMENSION(*) :: n
    REAL(C_FLOAT) :: Fres
    REAL(C_FLOAT), OPTIONAL :: Ft
    REAL(C_FLOAT), OPTIONAL :: Fv
    REAL(C_FLOAT), OPTIONAL, DIMENSION(*) :: Fn
    TYPE(BaseEos_container_), POINTER :: c_this__p
    CALL C_F_POINTER(c_this, c_this__p)
    SELECT TYPE (c_this__pp => c_this__p%c)
    CLASS IS (Variant1)
      CALL c_this__pp%Fres( T, V, n, Fres, Ft, Fv, Fn)
    END SELECT
  END SUBROUTINE variants__variant1_fres_wrap

  SUBROUTINE variants__variant2_ctor_sub_wrap(instance, ident, nc, Tc, Vc, v&
    &ar1, var2) BIND(C)
    TYPE(C_PTR), VALUE :: instance
    INTEGER(C_INT) :: ident
    INTEGER(C_INT) :: nc
    REAL(C_FLOAT) :: Tc
    REAL(C_FLOAT) :: Vc
    INTEGER(C_INT) :: var1
    REAL(C_FLOAT) :: var2
    TYPE(BaseEos_container_), POINTER :: instance__p
    CALL C_F_POINTER(instance, instance__p)
    SELECT TYPE (instance__pp => instance__p%c)
    CLASS IS (Variant2)
      CALL Variant2_ctor_sub(instance__pp, ident, nc, Tc, Vc, var1, var2)
    END SELECT
  END SUBROUTINE variants__variant2_ctor_sub_wrap

  SUBROUTINE variants__variant2_internal_comp_wrap(c_this, T, V, n, computed&
    &) BIND(C)
    TYPE(C_PTR), VALUE :: c_this
    REAL(C_FLOAT) :: T
    REAL(C_FLOAT) :: V
    REAL(C_FLOAT), DIMENSION(*) :: n
    REAL(C_FLOAT) :: computed
    TYPE(BaseEos_container_), POINTER :: c_this__p
    CALL C_F_POINTER(c_this, c_this__p)
    SELECT TYPE (c_this__pp => c_this__p%c)
    CLASS IS (Variant2)
      CALL c_this__pp%internal_comp( T, V, n, computed)
    END SELECT
  END SUBROUTINE variants__variant2_internal_comp_wrap

  SUBROUTINE variants__variant2_fres_wrap(c_this, T, V, n, Fres, Ft, Fv, Fn)&
    & BIND(C)
    TYPE(C_PTR), VALUE :: c_this
    REAL(C_FLOAT) :: T
    REAL(C_FLOAT) :: V
    REAL(C_FLOAT), DIMENSION(*) :: n
    REAL(C_FLOAT) :: Fres
    REAL(C_FLOAT), OPTIONAL :: Ft
    REAL(C_FLOAT), OPTIONAL :: Fv
    REAL(C_FLOAT), OPTIONAL, DIMENSION(*) :: Fn
    TYPE(BaseEos_container_), POINTER :: c_this__p
    CALL C_F_POINTER(c_this, c_this__p)
    SELECT TYPE (c_this__pp => c_this__p%c)
    CLASS IS (Variant2)
      CALL c_this__pp%Fres( T, V, n, Fres, Ft, Fv, Fn)
    END SELECT
  END SUBROUTINE variants__variant2_fres_wrap

END MODULE variants_wrap_


