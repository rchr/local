SELECT gid, d_gas_ewp, d_gas_ep, d_gas_lt, d_oel_kat, d_oel_ep, d_oel_lt, d_strom_lt, d_fw_ep, d_fw_ewp, d_fw_lt, d_kohle_ep, d_kohle_lt, d_wp_kat, i_qe
	FROM mapping.building_sector_ecoregion INNER JOIN warmekataster.bestand 
	ON building_sector_ecoregion.name=bestand.a_objektna 
	WHERE building_sector_ecoregion.sector_id=1

