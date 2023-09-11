import json
import os
from warnings import warn

HERE = os.path.dirname(__file__)

def find_fluid_file(ident, include_alias=False):
    """
    Get the absolute path to the fluid file of the fluid with identifier 'ident'

    Args:
        ident (str) : Fluid identifier
        include_alias (bool, optional) : Also search in aliases? (Default : False)
                                        NB: Aliases are not guaranteed to be unique!

    Raises:
        FileNotFoundError : If the fluid identifier is not found

    Returns:
        str : The absolute path to the matching fluid file
    """
    try:
        filepath = f'{HERE}/{ident}.json'
        data = json.load(open(filepath, 'r'))
        if ident == data['ident']:
            return filepath

        if include_alias is True:
            if ident in data['aliases']:
                return filepath
    except FileNotFoundError:
        pass

    for filename in os.listdir(HERE):
        filepath = f'{HERE}/{filename}'
        data = json.load(open(filepath, 'r'))
        if ident == data['ident']:
            return filepath

        if include_alias is True:
            if ident in data['aliases']:
                return filepath

    raise FileNotFoundError(f'Did not find a fluid file with identifier : {ident}')

def add_fluid(filename, base_data):
    """
    Add a new fluid to the database, with minimal required data

    Args:
        filename (str) : Name of the fluid file (including file ending, which should be .json)
        base_data (dict) : A dict matching 'default_base_data', which supplies values for all parameters that are 'None'
                            in 'default_base_data'.
    """
    filepath = f'{HERE}/{filename}'
    if os.path.isfile(filepath):
        raise FileExistsError(f'A fluid file already exists at {filepath}')

    base_data = verify_and_amend_dataset('base_model', base_data)

    if base_data['ident'] != filename.split('.')[0]:
        warn(f'Fluid identifier {base_data["ident"]} does not match filename {filename}', SyntaxWarning, stacklevel=2)

    json.dump(base_data, open(filepath, 'w'), indent=4)
    print(f'Created Fluid file {filepath}')

def verify_and_amend_dataset(model, model_data):
    """
    Check that the dict 'model_data' contains all required parameters. If optional parameters are omitted, insert
    the default values.

    Args:
        model (str) : The model key
        model_data (dict) : The model parameter set

    Raises:
        KeyError : If 'model' does not have a default data set to verify against
        KeyError : If 'model_data' is missing required parameters
        SyntaxWarning : If 'model_data' contains parameters (other than "note", "comment" or "doi") that are not found
                        in the default parameter set.

    Returns:
        dict : The supplied 'model_data' with inserted default values if optional values were not supplied
    """

    if model == 'base_model':
        default_data = {'ident': None,  # Fluid identifier (str)
                         'aliases': (),  # Alternative fluid identifiers (tuple[str]) (optional)
                         'CAS_number': None,  # int
                         'SMILES': None,  # str
                         'mole_weight': None,  # float
                         'T_crit': None,  # float
                         'p_crit': None,  # float
                         'rho_crit': None,  # float
                         'acentric_factor': None,  # float
                         'T_triple': None,  # float
                         'p_triple': None,  # float
                         'rho_triple': None,  # float
                         'melting_enthalpy_triple': None,  # float
                         'melting_entropy_triple': None,  # float
                         'dipole_moment': 0,  # float (optional)
                         'quadrupole_moment': 0,  # float (optional
                         'formation_enthalpy': None,  # float
                         'formation_entropy': None,  # float
                         'default_ideal_cp_correlation': None,  # int
                         'default_ideal_cp_coefficients': None  # tuple[float]
                         }
    elif model == 'SAFTVRMIE':
        default_data = {'m' : None,
                        'eps_div_k' : None,
                        'sigma' : None,
                        'lambda_a' : None,
                        'lambda_r' : None,
                        }
    else:
        raise KeyError(f'Unknown model type {model}')

    for k in model_data.keys():
        if (k not in default_data.keys()) and (k.lower() not in ('note', 'comment', 'doi')):
            warn(f'A value for {k} was supplied when creating Model type : {model}, '
                 f'but no corresponding entry exists in the default set!', SyntaxWarning, stacklevel=2)

    for k, v in default_data.items():
        if (v is None) and k not in model_data.keys():
            raise KeyError(f'A value for {k} must be supplied to create {model}')
        elif k not in model_data.keys:
            model_data[k] = v

    return model_data

def add_model_to_fluid(ident, model, reference, model_data, make_default=False):
    """
    Add a new model parameter set for the fluid 'ident'

    Args:
        ident (str) : fluid identifier (not alias)
        model (str) : Model identifier
        reference (str) : Parameter reference
        model_data (dict) : Model parameters
        make_default (bool, optional) : Make this parameter set the default for this model? (Default : False)
    """
    filepath = find_fluid_file(ident)
    data = json.load(open(filepath, 'r'))

    if model not in data.keys():
        data[model] = {}
        make_default = True

    if 'alt_reference' in model_data.keys():
        alt_reference = model_data['alt_reference']
    else:
        alt_reference = None

    if reference.lower() != 'default':
        if (reference in data[model].keys()) or (reference == data[model]['default']['alt_reference']):
            raise KeyError(f'Parameter reference : {reference} already exists for Fluid {ident}, model {model}')
        if make_default is True:
            model_data['alt_reference'] = reference
            add_model_to_fluid(ident, model, 'default', model_data)
    else:
        if alt_reference is None:
            raise KeyError('An alternative reference must be supplied when setting the default parameter set.')
        elif (alt_reference in data[model].keys()) or (alt_reference == data[model]['default']['alt_reference']):
            raise KeyError(f'The alternative reference {alt_reference} already exists for Fluid {ident}, model {model}')


    if (reference.lower() == 'default') and ('default' in data[model].keys()):
        data[model][data[model]['defalut']['alt_reference']] = data[model]['default']

    model_data = verify_and_amend_dataset(model, model_data)
    data[model][reference] = model_data
    json.dump(data, open(filepath, 'w'), indent=4)
    print(f'Added parameter set {reference} for {model} to {ident}')

if __name__ == '__main__':

    add_fluid('speciesA', )