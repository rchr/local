WITH
subsector AS (
	SELECT count(id) AS subsector_count, subsector_id, subsector_name, parent_sector 
	FROM mapping.building_sector_ecoregion 
	GROUP BY subsector_id, subsector_name, parent_sector 
	ORDER BY subsector_id ASC
),
sector AS (
	SELECT count(id) AS sector_count, sector_id, sector_name
	FROM mapping.building_sector_ecoregion 
	GROUP BY sector_id, sector_name
	ORDER BY sector_id ASC
)
SELECT * FROM 
subsector RIGHT OUTER JOIN sector
ON parent_sector=sector_id;
