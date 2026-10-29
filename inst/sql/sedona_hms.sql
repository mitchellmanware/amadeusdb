WITH cleaned AS (
  SELECT
    LOWER(TRIM("Density")) AS density,
    CASE LOWER(TRIM("Density"))
      WHEN 'light'  THEN 1
      WHEN 'medium' THEN 2
      WHEN 'heavy'  THEN 3
    END AS density_rank,
    ST_Buffer(wkb_geometry, 0.0) AS geometry
  FROM db_hms
  WHERE LOWER(TRIM("Density")) IN ('light', 'medium', 'heavy')
),

-- Merge overlapping plumes of the same density.
dissolved AS (
  SELECT
    density,
    density_rank,
    ST_Union_Agg(geometry) AS geometry
  FROM cleaned
  GROUP BY density, density_rank
),

-- Build the combined higher-density mask for each class.
higher_density AS (
  SELECT
    d.density,
    ST_Union_Agg(h.geometry) AS geometry
  FROM dissolved d
  JOIN dissolved h
    ON h.density_rank > d.density_rank
  GROUP BY d.density
),

-- Remove higher-density coverage from each class.
-- WKB conversion avoids geometry metadata loss through CASE.
exclusive_plumes AS (
  SELECT
    d.density,
    ST_SetCRS(
      ST_GeomFromWKB(
        CASE
          WHEN h.geometry IS NULL
            THEN ST_AsBinary(d.geometry)
          ELSE ST_AsBinary(
            ST_Difference(d.geometry, h.geometry)
          )
        END
      ),
      'OGC:CRS84'
    ) AS geometry
  FROM dissolved d
  LEFT JOIN higher_density h
    ON d.density = h.density
)

SELECT
  l.{locs_id_sql} AS {locs_id_sql},
  CAST($1 AS DATE) AS date,

  MAX(CASE WHEN p.density = 'light'
    THEN 1 ELSE 0 END) AS {light_sql},

  MAX(CASE WHEN p.density = 'medium'
    THEN 1 ELSE 0 END) AS {medium_sql},

  MAX(CASE WHEN p.density = 'heavy'
    THEN 1 ELSE 0 END) AS {heavy_sql}

FROM db_locs l
LEFT JOIN exclusive_plumes p
  ON NOT ST_IsEmpty(p.geometry)
  AND ST_Intersects(l.geometry, p.geometry)
  AND (
    ST_Dimension(l.geometry) = 0
    OR ST_Area(ST_Intersection(l.geometry, p.geometry)) > 0
  )

GROUP BY l.{locs_id_sql}
