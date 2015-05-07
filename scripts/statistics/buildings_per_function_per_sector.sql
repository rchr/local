SELECT count(id), sector_id, sector_name, function AS func, func_desc
FROM mapping.building_sector_XXXX 
GROUP BY sector_id, sector_name, function, func_desc
ORDER BY sector_id ASC;
