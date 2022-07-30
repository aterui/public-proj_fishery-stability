README
================
Akira Terui
2021-10-11

# GIS analysis (R)

`gis_` prefix indicates scripts for GIS analysis.

-   `gis_watershed`: watershed delineation

-   `gis_environment`: estimate land use (Copernicus) and climates
    (Chelsa)

-   `data_gis/`:

    -   `site-coordinate_hogosuimen_terui-org-2019.csv`: original site
        coordinates for protected watersheds.
    -   `albers_hkd_shape.gpkg`: shape of hokkaido island. Copy from the
        repository “public-proj\_hokkaido-data”.
    -   `albers_watershed_env.gpkg`: watershed polygon for each sampling
        site with environmental data. Land use values are the fraction
        of each land use category in the polygon while climate data
        represent values at the sampling sites/outlet points (!=
        watershed polygon average).
    -   `epsg4326_channel_1sqkm.gpkg` : channel network with 1 sqkm
        threshold. Output from MERIT hydro.
    -   `epsg4326_point_snap_prtwsd.gpkg` : sampling site coordinates
        snapped to river networks.
    -   `epsg4326_point_snap_prtwsd_edit.gpkg` : sampling site
        coordinates snapped to river networks. Manually edited for
        watershed delineation.
    -   `epsg4326_watershed.gpkg`: watershed polygon for each sampling
        site.
    -   `CHELSA_bio10_01.tif`: CHELSA climate layer (temperature) used
        for extraction of climatic conditions.
    -   `CHELSA_bio10_12.tif`: CHELSA climate layer (precipitation) used
        for extraction of climatic conditions
    -   `epsg4326_dir.tif` : flow direction layer from MERIT hydro.
    -   `epsg4326_dir_d8.tif` : flow direction layer from MERIT hydro.
        Converted to d8 class for `whitebox` operation.
    -   `epsg4326_lu_hkd.tif`: Copernics land use layer used for
        estimating land use fraction for urban, forest, and agriculture.
    -   `epsg4326_upa.tif` : upstream catchment area from MERIT hydro.

NOTE: the following sites were manually edited for watershed delineation

    ## Simple feature collection with 12 features and 2 fields
    ## geometry type:  POINT
    ## dimension:      XY
    ## bbox:           xmin: 139.9075 ymin: 41.68167 xmax: 141.841 ymax: 45.3575
    ## geographic CRS: WGS 84
    ##           river site                      geom
    ## 6       chihase    1 POINT (140.0329 42.64869)
    ## 7       chihase    2 POINT (140.0229 42.66417)
    ## 35     ishizaki    3  POINT (140.055 41.68167)
    ## 39      kenichi    2    POINT (140.04 42.1475)
    ## 50     masuhoro    2 POINT (141.8383 45.35583)
    ## 52     masuhoro    4   POINT (141.841 45.3575)
    ## 98  shokanbetsu    1 POINT (141.4608 43.77167)
    ## 104      tomari    1   POINT (140.0892 42.665)
    ## 106      tomari    3 POINT (140.0854 42.66544)
    ## 111  toshibetsu    2 POINT (140.0619 42.45809)
    ## 113  toshibetsu    4 POINT (140.0654 42.52963)
    ## 117    usubetsu    3   POINT (139.9075 42.235)
