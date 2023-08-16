---
editor_options: 
  markdown: 
    wrap: 72
---

# csodata

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

    #> Last Update: 2022-12-14

# Planned Updates

-   Continue to implement PxStat features

# Version History

## v1.4.2

- Fixed an issue with pivot_format = "tidy"

## v1.4.1

-   Fixed issue where passing from_date = NULL would cause an error in
    cso_get_toc.

-   Added check for csodata directory in cache before attempting to
    flush it.

## v1.4.0

-   Switched from RESTful API to JSON RPC for sending requests to PxStat

-   Added new tidy data format

-   Deprecated the wide_format augment and replaced it with pivot_format

-   Various bug fixes and quality of life additions

## v1.3.0

-   Minor bugfix in cso_geo_meta and quick_start_guide.
-   Updated to newest Local Authority boundaries in cso_get_geo
-   cso_get_meta now includes indicator if the dataset contains a
    geographical element

## v1.2.1

-   Minor bugfix in cso_get_geo.

## v1.2.1

-   Minor bugfix in cso_download_tbl.

## v1.2.0

-   Implementing the active managment of saved tables in the cache.

## v1.1.0

-   Now using PxStat API as the data resource.

## v1.0.2

-   Fixed a bug in cso_get_geo.

## v1.0.1

-   Added graceful failure when internet resources not available, in
    accordance with CRAN policies.

## v1.0.0

-   Major increment to version number to reflect release on CRAN.
-   Changed maintainer to Conor Crowley.
-   Added suppress_messages option to cso_get_toc and cso_download_tbl.
-   Caching of data now uses the LastModified field from the table of
    contents as part of the key so that a new version is only retrieved
    when the table is updated. Due to this all users should clear the
    cache with cso_clear_cache() or otherwise.
-   Added two new options to cso_get_geo for updated NUTS shapefiles
    from the OSi website.
-   Changed projection of new map files to Irish Grid system.

## v0.1.5

2019-12-13

-   Minor edits to meet CRAN requirements.

## v0.1.4

2019-12-04

-   Changed naming scheme from xx_cso_yy to cso_xx_yy.
-   Added cso_disp_meta function.
-   Added caching funtionality using R.cache package.
-   General improvements to documentation.
-   Implemented cso_get_geo function, to download geographic data from
    cso.ie.
-   Separated cso_get_content into its own function, from cso_get_meta.
-   New metadata functions, cso_get_vars, cso_get_interval and
    cso_get_content.
-   Cache is now stored in a csodata subdirectory, and clear_cso_cache
    can be used to empty it.
-   Added Mervyn and Conor to authors field on DESCRIPTION.
-   Changed double backslashes (\\\\) to single forward slashes (/).
-   Moved cso_get_toc to its own file.
-   cso_search_toc function added.
-   Added cso_get_var_values function.
-   New geographic metadata functions, cso_get_meta_geo and
    cso_disp_meta_geo.
-   Changed LastModified column of cso_get_toc from character format to
    datetime (POSIXct).
-   New vignette, quick_start_guide.

## v0.1.3

2019-11-11

-   Added get_cso_meta function.

## v0.1.2

2019-11-08

-   Add NEWS.Rmd and README.Rmd.
-   New id_list argument for get_cso_data.
-   Changed get_cso_names to include last modified date and table name.
-   Added "very_wide" option for wide_format in get_cso_data.

## v0.1.1

2019-11-05

-   Added roxygen2 documentation.

## v0.1.0

2019-11-04

-   Initial version based on previous R scipt.
