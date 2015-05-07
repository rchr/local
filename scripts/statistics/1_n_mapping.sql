SELECT count(id), function AS func, func_desc
FROM mapping.building_sector_XXXX
WHERE sector_id_2 IS NOT NULL 
AND sector_id=?
GROUP BY func_desc, func 
ORDER BY func, func_desc ASC 

