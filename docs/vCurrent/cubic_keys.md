<!---This file (`cubic_keys.md`) is prepended when generating the file `cubic_methods.md`, 
and is not intended to be rendered on its own--->

# Initialiser keys

| Model                    | Input key   |
| ------------------------ | ----------- |
| Van der Waal             | VdW         |
| Soave Redlich Kwong      | SRK         |
| Peng Robinson            | PR          |
| Schmidt-Wensel           | SW          |
| Patel Teja               | PT          |
| Translated consistent PR | tcPR        |


## Pure fluid &alpha;

| Model                    | Input key   |
| ------------------------ | ----------- |
| Model default (*)        | Classic         |
| Twu-Coon-Bluck-Cunninghan       | TWU         |
| Mathias-Copeman | MC          |
| [Graboski and Daubert](https://doi.org/10.1021/i260068a009) | GD           |
| [Redlich-Kwong](https://doi.org/10.1021/cr60137a013)              | RK           |
| Soave | Soave        |
| [Peng Robinson 76](https://doi.org/10.1021/i160057a011)           | PR          |
| [UMR &alpha; formulation](https://doi.org/10.1021/ie049580p)           | UMR          |
| Peng Robinson 78           | PR78          |
| Van der Waal             | VdW         |
| Schmidt-Wensel           | SW          |
| Patel Teja               | PT          |

(*) Will use original &alpha; for specified EOS. E.g. SRK will use Soave &alpha;, Peng-Robinson will use PR &alpha; etc.


## &alpha; mixing rules

* van der Waals
* Huron-Vidal/NRTL
* [UNIFAC.pdf](/thermopack/memo/unifac/unifac.pdf)
* [WongSandler.pdf](/thermopack/memo/wongsandler/wongsandler.pdf)

| Model                    | Input key      |
| ------------------------ | -------------- |
| Van der Waal             | Classic or vdW |
| Huron-Vidal              | HV or HV2      |
| Wong-Sandler             | WS             |
| NRTL                     | NRTL           |
| UNIFAC                   | UNIFAC         |


## &beta; mixing rules

* ....