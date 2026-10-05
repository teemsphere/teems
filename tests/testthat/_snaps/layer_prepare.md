# GTAP-E preparation on a synthetic layer

    x The GTAP-E layer is incomplete: header "FUEL" is missing.
    x GTAP-E preparation needs the sets COMM, ACTS, REG, COME and FUEL and the parameters SUBP and INCP and the data VDPP and VMPP.

---

    x CDE parameter SUBP is dimensioned on "TOPP", not "COMM".
    x The CDE parameters the model reads over "TOPP" are built from the "COMM" ones, the energy node as their private-consumption-weighted mean over the energy commodities.

---

    x The CDE substitution parameter SUBP reaches 5.078, so `ALPHA = 1 - SUBPAR` would be negative.
    i A GTAP parameter file holds SUBP in (0, 1].

# GTAP-EP preparation on a synthetic layer

    x The GTAP-Power generation technologies "coalbl", "gasp", and "nuclear" do not split into base load and peak load.
    x The split is read from the "BL" and "P" name suffixes; the long labels are inconsistent and cannot stand in for them.

---

    x The GTAP-Power layer is incomplete: header "ELEA" is missing.
    x GTAP-Power preparation needs the sets COMM, ACTS, REG, COME, FUEL, ELEC and ELEA and the parameters SUBP and INCP and the data VDPP and VMPP.

# a COMM weight is recast onto TOPP

    x GTAP-E weight VDPB cannot be recast onto "TOPP" from "an absent header".
    x The CDE parameters are aggregated with private consumption over "COMM", which "TOPP" must cover with exactly one aggregate element.

---

    x GTAP-E weight VDPB cannot be recast onto "TOPP" from "REG".
    x The CDE parameters are aggregated with private consumption over "COMM", which "TOPP" must cover with exactly one aggregate element.

---

    x GTAP-E weight VDPB cannot be recast onto "TOPP" from "COMM".
    x The CDE parameters are aggregated with private consumption over "COMM", which "TOPP" must cover with exactly one aggregate element.

