WITH r AS (
SELECT
    RS_SetCRS(
    RS_FromPath($1),
    'OGC:CRS84'
    ) AS raster
)
SELECT
db_locs.{locs_id_sql} AS {locs_id_sql},
CAST($2 AS TIMESTAMP(6)) AS time,
CAST($3 AS TEXT) AS level,
{sql_extraction} AS {variable_sql},
FROM db_locs
CROSS JOIN r
