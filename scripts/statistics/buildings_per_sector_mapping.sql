WITH 
one2one AS (
	SELECT count(id), sector_id, sector_name
	FROM mapping.building_sector_XXXX 
	WHERE sector_id_2 IS NULL AND sector_id_3 IS NULL
	GROUP BY sector_id, sector_name
	ORDER BY sector_id ASC
),
one2n AS (
	SELECT count(id), sector_id, sector_name
	FROM mapping.building_sector_XXXX
	WHERE sector_id_2 IS NOT NULL
	GROUP BY sector_id, sector_name
	ORDER BY sector_id ASC 
)
SELECT *, '1:1' AS mapping FROM one2one
UNION ALL
SELECT *, '1:n' AS  mapping FROM one2n;

