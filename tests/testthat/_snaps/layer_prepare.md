# GTAP-E preparation on a synthetic layer

    x The GTAP-E layer is incomplete: header "FUEL" is missing.
    x GTAP-E preparation needs the sets COMM, ACTS, REG, COME and FUEL and the parameters SUBE and INCE.

---

    x GTAP-E parameter SUBE is dimensioned on "COMM", not "TOPP".
    x SUBE supplies the CDE parameters the model reads over "TOPP"; a different dimension would bind the wrong values.

---

    x The CDE substitution parameter SUBE reaches 5.078, so `ALPHA = 1 - SUBPAR` would be negative.
    i The "eny" row of the 11c releases is an un-normalised sum over the energy commodities rather than a share-weighted parameter; the 12a releases carry it correctly.

# GTAP-EP preparation on a synthetic layer

    x The GTAP-Power generation technologies "coalbl", "gasp", and "nuclear" do not split into base load and peak load.
    x The split is read from the "BL" and "P" name suffixes; the long labels are inconsistent and cannot stand in for them.

---

    x The GTAP-Power layer is incomplete: header "ELEA" is missing.
    x GTAP-Power preparation needs the sets COMM, ACTS, REG, COME, FUEL, ELEC and ELEA and the parameters SUBE and INCE.

# a COMM weight is recast onto TOPP

    x GTAP-E weight VDPB cannot be recast onto "TOPP" from "an absent header".
    x The CDE parameters are aggregated with private consumption over "COMM", which "TOPP" must cover with exactly one aggregate element.

---

    x GTAP-E weight VDPB cannot be recast onto "TOPP" from "REG".
    x The CDE parameters are aggregated with private consumption over "COMM", which "TOPP" must cover with exactly one aggregate element.

---

    x GTAP-E weight VDPB cannot be recast onto "TOPP" from "COMM".
    x The CDE parameters are aggregated with private consumption over "COMM", which "TOPP" must cover with exactly one aggregate element.

