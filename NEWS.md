csodata
================

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

    #> Last Update: 2021-07-05 

# Planned Updates

  - Continue to implement PxStat features

# Version History
## v1.2.1

  - Minor bugfix in cso\get\_geo.
  
  
## v1.2.1

  - Minor bugfix in cso\_download\_tbl.

## v1.2.0

  - Implementing the active managment of saved tables in the cache.

## v1.1.0

  - Now using PxStat API as the data resource.

## v1.0.2

  - Fixed a bug in cso\_get\_geo.

## v1.0.1

  - Added graceful failure when internet resources not available, in
    accordance with CRAN policies.

## v1.0.0

  - Major increment to version number to reflect release on CRAN.
  - Changed maintainer to Conor Crowley.
  - Added suppress\_messages option to cso\_get\_toc and
    cso\_download\_tbl.
  - Caching of data now uses the LastModified field from the table of
    contents as part of the key so that a new version is only retrieved
    when the table is updated. Due to this all users should clear the
    cache with cso\_clear\_cache() or otherwise.
  - Added two new options to cso\_get\_geo for updated NUTS shapefiles
    from the OSi website.
  - Changed projection of new map files to Irish Grid system.

## v0.1.5

2019-12-13

  - Minor edits to meet CRAN requirements.

## v0.1.4

2019-12-04

  - Changed naming scheme from xx\_cso\_yy to cso\_xx\_yy.
  - Added cso\_disp\_meta function.
  - Added caching funtionality using R.cache package.
  - General improvements to documentation.
  - Implemented cso\_get\_geo function, to download geographic data from
    cso.ie.
  - Separated cso\_get\_content into its own function, from
    cso\_get\_meta.
  - New metadata functions, cso\_get\_vars, cso\_get\_interval and
    cso\_get\_content.
  - Cache is now stored in a csodata subdirectory, and clear\_cso\_cache
    can be used to empty it.
  - Added Mervyn and Conor to authors field on DESCRIPTION.
  - Changed double backslashes (\\\\) to single forward slashes (/).
  - Moved cso\_get\_toc to its own file.
  - cso\_search\_toc function added.
  - Added cso\_get\_var\_values function.
  - New geographic metadata functions, cso\_get\_meta\_geo and
    cso\_disp\_meta\_geo.
  - Changed LastModified column of cso\_get\_toc from character format
    to datetime (POSIXct).
  - New vignette, quick\_start\_guide.

## v0.1.3

2019-11-11

  - Added get\_cso\_meta function.

## v0.1.2

2019-11-08

  - Add NEWS.Rmd and README.Rmd.
  - New id\_list argument for get\_cso\_data.
  - Changed get\_cso\_names to include last modified date and table
    name.
  - Added “very\_wide” option for wide\_format in get\_cso\_data.

## v0.1.1

2019-11-05

  - Added roxygen2 documentation.

## v0.1.0

2019-11-04

  - Initial version based on previous R scipt.
