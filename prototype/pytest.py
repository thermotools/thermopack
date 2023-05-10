from prototype.py_wrapper.BaseEoS import NotIdGas

nid = NotIdGas(0, 2, 100, 100)

T = 300
V = 1
n = [1, 2]

print(nid.pressure(T, V, n))
print(nid.Fres(T, V, n, True, None, True))

