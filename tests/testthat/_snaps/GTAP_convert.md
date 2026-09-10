# GTAP_convert errors when invalid target

    `target` must be one of "GTAPv6", "GTAPv7", "GTAP-AEZ", or "GTAP-E", not "GTAPv3".
    i Did you mean "GTAPv6"?

# GTAP_convert warns when v9 inputs are already in target format

    ! `target` set to GTAPv6 but data appears to already be this format.

# GTAP_convert warns when v10 inputs are already in target format

    ! `target` set to GTAPv6 but data appears to already be this format.

# GTAP_convert warns when v11 inputs are already in target format

    ! `target` set to GTAPv7 but data appears to already be this format.

# GTAP_convert warns when v12 inputs are already in target format

    ! `target` set to GTAPv7 but data appears to already be this format.

# GTAP-AEZ preparation on a synthetic layer

    x The GTAP-AEZ layer is incomplete: header "LUSA" is missing.
    x GTAP-AEZ preparation needs the sets AEZS, COVS, CROP, LUSA, ACTS and REG and the data AREA, TONS and LCOV.

# GTAP_convert GTAP-AEZ target rejects the v6-format layer (v10a AEZ)

    x The "GTAP-AEZ" target prepares the GTAPv7-format AEZ layer (GTAP 11/12 AEZ databases); the input is in the v6.2 format.
    x The v6-format AEZ layer (GTAP 10a) carries different headers (ESBL, ETL1-3, ETA, YD01, YDEL over LAND_COMM/ENDWL_COMM/PROD_COMM) and is not supported.

