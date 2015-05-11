#!/bin/bash

HOST=xxx.xx.xx.xx
DB=???

echo "COPY ( 
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
	FROM mapping.alkis2ecoregion a
	INNER JOIN eco_region.sector b
	ON a.sector_id = b.sector_id
	LEFT OUTER JOIN eco_region.sector c
	ON a.sector_id_2 = c.sector_id
	LEFT OUTER JOIN eco_region.sector d
	ON a.sector_id_3 = d.sector_id
)
SELECT building_function.building_function_id, building_function.description, gpc.gpc_subsectors, eco_region.eco_subsectors
FROM gpc 
INNER JOIN eco_region
USING (building_function_id)
INNER JOIN alkis.building_function
USING (building_function_id))
TO STDOUT WITH DELIMITER ';' CSV HEADER" | psql -h $HOST -o mapping_all.csv $DB

echo "COPY (
WITH
gpc AS (
	SELECT a.building_function_id, b.name::text || ', ' || COALESCE(c.name::text, '')|| ', ' || COALESCE(d.name::text, '')  AS gpc_subsectors
	FROM mapping.alkis2gpc a
	INNER JOIN gpc.subsector b
	ON a.subsector_id = b.subsector_id
	INNER JOIN gpc.subsector c
	ON a.subsector_id_2 = c.subsector_id
	LEFT JOIN gpc.subsector d
	ON a.subsector_id_3 = d.subsector_id
	WHERE a.subsector_id_2 IS NOT NULL
),
eco_region AS (
	SELECT a.building_function_id, b.name::text || ', ' || COALESCE(c.name::text, '') || ', ' || COALESCE(d.name::text, '') AS eco_subsectors
	FROM mapping.alkis2ecoregion a
	INNER JOIN eco_region.sector b
	ON a.sector_id = b.sector_id
	INNER JOIN eco_region.sector c
	ON a.sector_id_2 = c.sector_id
	LEFT OUTER JOIN eco_region.sector d
	ON a.sector_id_3 = d.sector_id
	WHERE a.sector_id IS NOT NULL
)
SELECT building_function.building_function_id, building_function.description, gpc.gpc_subsectors, eco_region.eco_subsectors
FROM gpc 
FULL OUTER JOIN eco_region
USING (building_function_id)
INNER JOIN alkis.building_function
USING (building_function_id))
TO STDOUT WITH DELIMITER ';' CSV HEADER" | psql -h $HOST -o mapping_uncertain.csv $DB
