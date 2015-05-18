-- Exports co2 emissions per sector as GPC compliant report.
WITH
residential_buildings AS (
	SELECT *
	FROM mapping.building_sector_gpc INNER JOIN warmekataster.bestand
	ON building_sector_gpc.name=bestand.a_objektna
	WHERE building_sector_gpc.sector_id=1
),
scope1_residential AS (
	SELECT sum(i_co2)
	FROM residential_buildings
	WHERE d_gas_ewp=1 OR d_gas_ep=1 OR d_gas_lt=1
	OR d_oel_kat=1 OR d_oel_ep=1 OR d_oel_lt=1
	OR d_kohle_ep=1 OR d_kohle_lt=1
),
scope2_residential AS (
	SELECT sum(i_co2)
	FROM residential_buildings
	WHERE d_strom_lt=1
	OR d_fw_ewp=1 OR d_fw_ep=1 OR d_fw_lt=1
),
co2_residential_buildings AS (
	SELECT scope1_residential.sum + scope2_residential.sum
	FROM scope1_residential, scope2_residential
),

commercial_buildings AS (
	SELECT *
	FROM mapping.building_sector_gpc INNER JOIN warmekataster.bestand
	ON building_sector_gpc.name=bestand.a_objektna
	WHERE building_sector_gpc.sector_id=2
),
scope1_commercial AS (
	SELECT sum(i_co2)
	FROM commercial_buildings
	WHERE d_gas_ewp=1 OR d_gas_ep=1 OR d_gas_lt=1
	OR d_oel_kat=1 OR d_oel_ep=1 OR d_oel_lt=1
	OR d_kohle_ep=1 OR d_kohle_lt=1
),
scope2_commercial AS (
	SELECT sum(i_co2)
	FROM commercial_buildings
	WHERE d_strom_lt=1
	OR d_fw_ewp=1 OR d_fw_ep=1 OR d_fw_lt=1
),
co2_commercial_buildings AS (
	SELECT scope1_commercial.sum + scope2_commercial.sum
	FROM scope1_commercial, scope2_commercial
),

industry_buildings AS (
	SELECT *
	FROM mapping.building_sector_gpc INNER JOIN warmekataster.bestand
	ON building_sector_gpc.name=bestand.a_objektna
	WHERE building_sector_gpc.sector_id=3
),
scope1_industry AS (
	SELECT sum(i_co2)
	FROM industry_buildings
	WHERE d_gas_ewp=1 OR d_gas_ep=1 OR d_gas_lt=1
	OR d_oel_kat=1 OR d_oel_ep=1 OR d_oel_lt=1
	OR d_kohle_ep=1 OR d_kohle_lt=1
),
scope2_industry AS (
	SELECT sum(i_co2)
	FROM industry_buildings
	WHERE d_strom_lt=1
	OR d_fw_ewp=1 OR d_fw_ep=1 OR d_fw_lt=1
),
co2_industry_buildings AS (
	SELECT scope1_industry.sum + scope2_industry.sum
	FROM scope1_industry, scope2_industry
)

SELECT NULL AS scope, 'Residential Buildings' AS ghg_emissions_source, co2_residential_buildings.* AS gases
FROM co2_residential_buildings
UNION ALL
SELECT '1', 'Emissions from in-boundary fuel combustion', scope1_commercial.*
FROM scope1_commercial
UNION ALL
SELECT '2', 'Emissions from consumption of grid-supplied energy', scope2_commercial.*
FROM scope2_commercial
UNION ALL
SELECT NULL, 'Commercial and institutional buildings/facilities', co2_commercial_buildings.*
FROM co2_commercial_buildings
UNION ALL
SELECT '1', 'Emissions from in-boundary fuel combustion', scope1_commercial.*
FROM scope1_commercial
UNION ALL
SELECT '2', 'Emissions from consumption of grid-supplied energy', scope2_commercial.*
FROM scope2_commercial
UNION ALL
SELECT NULL, 'Manufacturing industry and construction', co2_industry_buildings.*
FROM co2_industry_buildings
UNION ALL
SELECT '1', 'Emissions from in-boundary fuel combustion', scope1_industry.*
FROM scope1_industry
UNION ALL
SELECT '2', 'Emissions from consumption of grid-supplied energy', scope2_industry.*
FROM scope2_industry
