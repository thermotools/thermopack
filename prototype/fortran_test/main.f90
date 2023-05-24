program main
    use variants, only : Variant1, Variant2, Variant1_db_ctor
    implicit none

    type(Variant1) :: v1_A_default
    type(Variant1) :: v1_A_otherref
    real :: T, V, F, Ft, Fv, p
    real, dimension(2) :: n, Fn

    v1_A_default = Variant1_db_ctor("speciesA" // char(0))
    v1_A_otherref = Variant1_db_ctor("speciesA" // char(0), "ref2" // char(0))

    T = 300.
    V = 5.
    n = [1., 2.]

    print*, "default is Tc : ", v1_A_default%Tc, ", p1 : ", v1_A_default%param1
    print*, "other   is Tc : ", v1_A_otherref%Tc, ", p1 : ", v1_A_otherref%param1

    ! call v1_A_default%pressure(T, V, n, p)
    ! print*, "Pressure is :", p
    ! call v1_A_default%Fideal(T, V, n, F, Ft, Fv, Fn)
    ! p = - Fv
    ! print*, "Ideal pressure is :", p

end program main

