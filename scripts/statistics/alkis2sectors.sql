WITH
gpc AS (
	SELECT a.building_function_id, b.name::text || ', ' || COALESCE(c.name::text, '') || ', ' || COALESCE(d.name::text, '') AS gpc_subsectors
	FROM mapping.alkis2gpc a
	INNER JOIN gpc.subsector b
	ON a.subsector_id = b.subsector_id
	LEFT OUTER JOIN gpc.subsector c
	ON a.subsector_id_2 = c.subsector_id
	LEFT OUTER JOIN gpc.subsector d
	ON a.subsector_id_3 = d.subsector_id
),
eco_region AS (
	SELECT a.building_function_id, b.name::text || ', ' || COALESCE(c.name::text, '') || ', ' || COALESCE(d.name::text, '') AS eco_subsectors
	FROM mapping.alkis2gpc a
	INNER JOIN eco_region.sector b
	ON a.subsector_id = b.sector_id
	LEFT OUTER JOIN eco_region.sector c
	ON a.subsector_id_2 = c.sector_id
	LEFT OUTER JOIN eco_region.sector d
	ON a.subsector_id_3 = d.sector_id
)
SELECT * 
FROM gpc 
INNER JOIN eco_region
USING (building_function_id);
