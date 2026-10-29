WITH ranked AS (
  SELECT
    db_locs.{locs_id_sql} AS {locs_id_sql},

    MAX(CASE LOWER(TRIM(db_hms."Density"))
      WHEN 'light'  THEN 1
      WHEN 'medium' THEN 2
      WHEN 'heavy'  THEN 3
      ELSE 0
    END) AS density_rank

  FROM db_locs
  LEFT JOIN db_hms
    ON ST_Intersects(db_locs.geometry, db_hms.wkb_geometry)
  GROUP BY db_locs.{locs_id_sql}
)

SELECT
  ranked.{locs_id_sql} AS {locs_id_sql},
  CAST($1 AS DATE) AS time,

  CASE WHEN density_rank = 1 THEN 1 ELSE 0 END AS {light_sql},
  CASE WHEN density_rank = 2 THEN 1 ELSE 0 END AS {medium_sql},
  CASE WHEN density_rank = 3 THEN 1 ELSE 0 END AS {heavy_sql}

FROM ranked
