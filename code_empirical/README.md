README
================
Akira Terui
7/20/2021

# GIS analysis (R)

`gis_` prefix indicates scripts for GIS analysis.

-   `gis_watershed`: watershed delineation
-   `gis_environment`: estimate land use (Copernicus) and climates
    (Chelsa)
-   `data_gis/`:
    -   `albers_channel_hkd.gpkg`: channel network across hokkaido. Copy
        from the repository “public-proj\_hokkaido-data”.
    -   `albers_hkd_shape.gpkg`: shape of hokkaido island. Copy from the
        repository “public-proj\_hokkaido-data”.
    -   `albers_point_outlet_prtwsd.gpkg`: sampling sites for protected
        watershed. edited manually for watershed delineation (see below
        for details).
    -   `albers_point_snap_prtwsd.gpkg`: sampling sites for protected
        watershed. snapped to the nearest channel segment.
    -   `albers_wsd_env_prtwsd.gpkg`: watershed polygon for each
        sampling site with environmental data. land use values are
        fraction of each land use categoty in the polygon while climate
        data represent values at the sampling sites/outlet points (!=
        watershed polygon average).
    -   `albers_wsd_prtwsd.gpkg`: watershed polygon for each sampling
        site.
    -   `site-coordinate_hogosuimen_terui-org-2019.csv`: original site
        coordinates for protected watersheds.
    -   `CHELSA_bio10_01.tif`: CHELSA climate layer (temperature) used
        for extraction of climatic conditions.
    -   `CHELSA_bio10_12.tif`: CHELSA climate layer (precipitation) used
        for extraction of climatic conditions
    -   `wgs84_lu_hkd.tif`: Copernics land use layer used for estimating
        land use fraction for urban, forest, and agriculture.

NOTE: the following sites were manually edited for watershed delineation

    ## Simple feature collection with 20 features and 2 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: 398095.2 ymin: 198444.5 xmax: 541105.2 ymax: 620274.5
    ## Projected CRS: JGD2000 / Conus Albers
    ##           river site                      geom
    ## 1        atsuta    1 POINT (526165.2 409854.5)
    ## 3        atsuta    3   POINT (523239.8 407740)
    ## 6       chihase    1 POINT (412945.2 305724.5)
    ## 7       chihase    2 POINT (412045.2 307344.5)
    ## 14       futoro    2 POINT (401065.2 272604.5)
    ## 15       futoro    3 POINT (398095.2 275214.5)
    ## 35     ishizaki    3 POINT (420865.2 198624.5)
    ## 37     ishizaki    5 POINT (421405.2 198444.5)
    ## 44      kenichi    7 POINT (415825.2 249294.5)
    ## 50     masuhoro    2 POINT (538045.2 614874.5)
    ## 51     masuhoro    3 POINT (541105.2 620274.5)
    ## 82     shakotan    2 POINT (450475.2 382314.5)
    ## 97  shokanbetsu    1 POINT (520945.2 437484.5)
    ## 103      tomari    1 POINT (417445.2 307794.5)
    ## 104      tomari    2   POINT (416238.8 308821)
    ## 105      tomari    3 POINT (417175.2 307794.5)
    ## 109  toshibetsu    1 POINT (416275.2 286284.5)
    ## 110  toshibetsu    2 POINT (416545.2 284664.5)
    ## 112  toshibetsu    4 POINT (416365.2 292674.5)
    ## 116    usubetsu    3 POINT (405205.2 259284.5)
