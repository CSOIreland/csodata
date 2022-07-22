csodata
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

## An R package for downloading CSO data.

The csodata package allows for easily downloading CSO (Central
Statistics Office, the statistics agency of Ireland) PxStat data into
R. It also includes multiple functions for examining the metadata of CSO
tables, as well as a function to download geographic
data in the ESRI vector format from the CSO website.

PxStat is the Central Statistics Office’s (CSO) online database of
Official Statistics. This database contains current and historical data
series compiled from CSO statistical releases and is accessed
[here](https://data.cso.ie ) .
The CSO PxStat Application Programming Interface (API), which is accessed in this package, provides access to
PxStat data in [JSON-stat
format](https://github.com/CSOIreland/PxStat ). This dissemination
tool allows developers machine to machine access to CSO PxStat data.

### References

Graeme Walsh (2018). statbanker R package version 6.2.0. *For
inspiration and code used for reshaping tables.*
