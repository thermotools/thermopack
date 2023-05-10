program main
    use notidgas, only : NotIdGas
    implicit none

    type(NotIdGas) :: nid
    real :: T, V, F, Ft, Fv, p
    real, dimension(2) :: n, Fn

    nid = NotIdGas(0, 2, 100., 50.)

    T = 300.
    V = 5.
    n = [1., 2.]

    call nid%pressure(T, V, n, p)
    print*, "Pressure is :", p
    call nid%Fideal(T, V, n, F, Ft, Fv, Fn)
    p = - Fv
    print*, "Ideal pressure is :", p

end program main

