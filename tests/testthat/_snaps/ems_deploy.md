# ems_deploy errors when .data is missing

    x argument `.data` is missing, with no default

# ems_deploy errors when model is missing

    x argument `model` is missing, with no default

# ems_deploy errors when invalid variable provided for swap-in

    x Swap variable "not_a_var" not found in the model.

# ems_deploy errors when invalid variable provided for swap-out

    x Swap variable "not_a_var" not found in the model.

# ems_deploy errors when shock_file and shock are both provided

    x No additional shocks are accepted if a shock file is provided.

# write_coefficients must be a logical scalar

    x `write_coefficients` must be logical of length 1.

# ems_deploy errors when read-in headers not present in data

    x Read-in headers missing from loaded data: "SAVE".

# ems_deploy errors when read-in headers are missing mapping

    x Some read-in model sets have no mappings: REG.

# ems_deploy errors when timesteps provided to static model

    x `time_steps` provided but no intertemporal sets detected in the model. See `teems::ems_data()`.

# ems_deploy errors when timesteps not provided to a dynamic model

    x `time_steps` required for intertemporal models. See `teems::ems_data()`.

# ems_deploy errors when set-calculated number of entries does not match a finalized data header

    x ESBT has 45 entries; 30 expected.

# an unlabelled header is dimensioned from its reading coefficient

    x Header "MEXP" carries no set labels and its 6 values (shape 3 x 2) do not fit coefficient MEXP, declared over COM (3) x EXP2 (3).
    i An unlabelled header is read positionally against the declared sets of the coefficient that reads it; label the header's dimensions or correct the declaration.

# ems_deploy errors when aggregated inputs are incomplete

    x 7 tuples in the provided input file for "SAVE" were missing: 1: chn 1, 2: chn 2, 3: row 1, 4: row 2, 5: usa 0, 6: usa 1, and 7: usa 2.

