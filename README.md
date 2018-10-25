Overview
========

This package provides functions to convert element characteristic X-ray
intensity maps into element mass concentration maps.  
Current version supports data from JEOL-style electron probe
microanalyer (EPMA).  
For conversion, you need to run spot analysis before mapping analysis.  
See “How to” section for usage and Yasumoto et al. (submitted) for
implementations.

Installation
============

Install devtools package if you haven’t.

``` r
install.packages("devtools")
```

Then, run following code.

``` r
devtools::install_github("atusy/qntmap")
```

How to
======

1.  Export data from EPMA analysis.
2.  Run QntMap

Read below for details.

EPMA analysis
-------------

QntMap handles matrix effect by preparing internal standards based on
spot analysis.

### Spot analysis

-   Analytical conditions
    -   Same as those conventionally applied in your lab.
    -   Use wavelength-dispersive X-ray spectrometer
-   Spots to be analyzed
    -   **20 spots per phase** in the area to be mapped (the more is
        better).
    -   It is better but not necessary to quantify grains larger than
        mapping probe diameter.
    -   Make sure at least **20 spots per element** are analyzing grains
        larger than mapping probe diameter.
-   Commenting analysis
    -   Give same comments on the same phase with the similar
        compositions. Do not number them.
        -   e.g., quartz, plagioclase, …
    -   Otherwise, comment them with different name. This is important
        treatment for phase identification and handling matrix effect.
        -   e.g., garnet-core, garnet-rim
    -   If you give comments containing underscore such as
        “garnet\_core” and “garnet\_rim”, then they are treated as if
        different phases in phase identification, but are treated as if
        their matrix effect are approximately same.
    -   Alternatively, give comments manually by external file.

Spot quantitative analytical conditions in your lab.

### Mapping analysis

-   Analytical conditions
    -   Acceralating voltage should be same as one applied in spot
        analysis.
    -   Probe diameter should be larger than spot analysis. The larger
        saves more time, but decreases spatial resolutions.
    -   Probe current is recommended to be 100 nA following Lanari et
        al. (2014).
    -   Dwell time is recommended to be 0.1 - 0.3 sec following Lanari
        et al. (2014).
    -   Note that increasing probe current accepts decreasing dwell
        time, but probe current must not be too high to saturate X-ray
        detectors. If you prefer high probe current in some reason,
        consider changing dispersive crystals from those chosen in spot
        analysis.
        -   e.g., chose PET instead of TAP for Si

### Example of analytical conditions

Yasumoto et al. (submitted)

|                      |    Spot|       Map| Comment                          |
|:---------------------|-------:|---------:|:---------------------------------|
| Acceralating Voltate |   15 kV|     15 kV| Must be same in spot and map     |
| Probe diameter       |    3 μm|     20 μm| Must be smaller in spot than map |
| Probe current        |   10 nA|    100 nA|                                  |
| Peak dwell           |  10 sec|  120 msec|                                  |
| Background dwell     |   5 sec|        NA| No need to analyze in map        |

Data processing with QntMap package on R
----------------------------------------

### Quantification

By running following code, you’ll see that phase identification result
in ‘clustering’ directory and mass concentration data as csv files in
‘qntmap’ directory both under the directory contaning mapping data.

#### Interactive mode

Follow instructions shown by running the following code.

``` r
library(qntmap)
qntmap()
```

#### Manual mode for experts (example)

``` r
library(qntmap)

# Required parameters
wd <- '.' # path to the working directory
dir_map <- '.map/1' # relative/absolute path to the directory containing ascii converted X-ray map files (1_map.txt, 2_map.txt, and so on)"
dir_qnt <- '.qnt' # relative/absolute path to the directory containing .qnt files (pkint.qnt, net.qnt, and so on)"


# Optional parameters

## A character vector to specify phases tend to be smaller than mapping probe diameter
fine_phase <- NULL 

## A csv file indicating name of the phase of n-th quantitative point analysis.
## The file path is absolute or relative to `dir_qnt`.
## If NULL, names are assumed to be specified in comments during EPMA analysis.
phase_list <- NULL 

# Run analysis

# Set working directory
setwd(wd)

# Load mapping data
# Change value of DT (dead time in nanoseconds) depending on EPMA.
# 1100 ns is a value applied by JEOL JXA-8105.
xmap <- read_xmap(wd = dir_map, DT = 1100)

# Compile quantitative data
qnt <- read_qnt(wd = dir_qnt, phase_list = phase_list, renew = TRUE)
## Check 'phase_list0.csv' under 'dir_qnt' to see if name of phases are provided properly.
## If not, modify the csv file and specify the path of modified one to `phase_list` in "Optional parameters" section and rerun the above code.

# Determine initial cluster centers
centers <- find_centers(xmap = xmap, qnt = qnt, fine_phase = fine_phase)
## Check 'centers0.csv' under the `wd` and modify on demand.
## if modified, assign content of the modified csv file by running
## centers <- data.table::fread('path to the modified csv file')

# Phase identification
# assign group_cluster = TRUE if you want to integrate same phases subgrouped by suffix after '_' 
# (e.g., garnet_a and garnet_b are integrated to garnet if TRUE)
cls <- cluster_xmap(xmap = xmap, centers = centers, group_cluster = FALSE)

# quantify X-ray maps
qmap <- quantify(
  xmap = xmap, qnt = qnt, cluster = cls, fine_phase = fine_phase
)
## Resulting files are saved in `qntmap` directory` under `dir_map`.

# summarize result
summary(qmap)
## This shows minimum, lower quantile, median, mean, upper quantile, and maximum values of variables.
```
