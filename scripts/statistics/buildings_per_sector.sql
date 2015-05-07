SELECT count(id), sector_id, sector_name
FROM mapping.building_sector_XXXX 
GROUP BY sector_id, sector_name
ORDER BY sector_id ASC;


