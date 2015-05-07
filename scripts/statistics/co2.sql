WITH
co2 AS (
	SELECT * 
	FROM mapping.building_sector_XXXX INNER JOIN public.cityobject_genericattrib
	ON building_sector_XXXX.id=cityobject_genericattrib.cityobject_id
	WHERE attrname='i_co2'
)
SELECT sum(realval) AS co2, sector_name, sector_id 
FROM co2
GROUP BY sector_name, sector_id;
