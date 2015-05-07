WITH 
co2 AS (
	SELECT * 
	FROM mapping.building_sector_XXXX INNER JOIN public.cityobject_genericattrib
	ON building_sector_XXXX.id=cityobject_genericattrib.cityobject_id
	WHERE attrname='i_co2' and realval > 0
),
volume AS (
	SELECT cityobject_id, id, attrname, realval
	FROM cityobject_genericattrib
	WHERE attrname='volume'
),
groundsurface AS (
	SELECT cityobject_id, id, attrname, realval
	FROM cityobject_genericattrib
	WHERE attrname='groundsurface_area'
)
SELECT co2.sector_id, co2.sector_name, co2.realval AS co2, volume.realval as volume, co2.realval/volume.realval*1000 AS ratio_vol, groundsurface.realval AS groundsurface, co2.realval/groundsurface.realval*1000 AS ratio_ground, co2.cityobject_id as cityobject_id  
FROM co2 INNER JOIN volume 
USING (cityobject_id)
INNER JOIN groundsurface
USING (cityobject_id);
