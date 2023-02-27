## More advanced usage - Python

### Setting different parameters and coefficients for cubic equations of state

In thermopack we're able to both set and get a wide array of coefficients and parameters depending on the models we are utilizing. 
### Setting and getting the attractive energy interaction parameter kij and co-volume interaction parameter lij
Starting with the attractive energy interaction parameter (kij). The parameter can be set using the function `set_kij` after intialising the equation and state. The function requires that you first write in the number of the components and subsequently the new interaction parameter i.e. (component number 1, component number 2, new kij value). If we're curious as to what parameter the EOS is already using we can see this by using the function `get_kij` which returns the value as a float given the component numbers as input i.e. (component number 1, component number 2).
```Python
cs = cubic('CO2,N2',"SRK","Classic","Classic")
#We set the interaction parameter to be -0.032
cs.set_kij(1,2,-0.032)
#We want to see what the interaction parameter is which returns that kij = -0.032
kij = cs.get_kij(1,2)
```
The procedure for setting and getting co-volume interaction parameters is analogous to the getting and setting of attractive energy parameters. Simply use the functions `set_lij` and `get_lij` instead.
```Python
#We set the parameter to be -0.032
cs.set_lij(1,2,-0.032)
#We want to see what the parameter is which returns that lij = -0.032
lij = cs.get_lij(1,2)
```
