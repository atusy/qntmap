Enhance quantitative analysis of EPMA maps with QntMap
================

[![Travis build
status](https://travis-ci.org/atusy/qntmap.svg?branch=master)](https://travis-ci.org/atusy/qntmap)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/atusy/qntmap?branch=master&svg=true)](https://ci.appveyor.com/project/atusy/qntmap)
[![Coverage
status](https://codecov.io/gh/atusy/qntmap/branch/master/graph/badge.svg)](https://codecov.io/github/atusy/qntmap?branch=master)
[![license](https://img.shields.io/badge/license-MIT-blue.svg)]()

# Overview

This package generates mass concentration maps and phase distribution
maps based on X-ray mapping data and spot analysis data from EPMA.

See “[How to](#how-to)” for a usage and [Yasumoto et
al. (2018)](https://doi.org/10.2138/am-2018-6323CCBY) for
implementations.

Current version supports data from JEOL-style EPMA.

# Installation

Copy & paste a following command to R.

``` r
source("https://install-github.me/atusy/qntmap")
```

# How to

1.  [EPMA analysis](#epma-analysis) (spot before map)
2.  [Export data](#Export) from EPMA to PC
3.  [Run QntMap on R](#run-qntmap-on-r) for data processing.

Details below.

## EPMA analysis

Conversion is performed by utilizing spot analysis data as internal
standards. Thus, [spot analysis](#spot-analysis-1) must be done prior to
[mapping](#mapping).

### Spot analysis

  - Analytical conditions
      - Same as those conventionally applied in your lab.
      - Use wavelength-dispersive X-ray spectrometer
  - Spots to be analyzed
      - **20 spots per phase** in the area to be mapped (the more is
        better).
      - It is better but not necessary to quantify grains larger than
        mapping probe diameter.
      - Make sure at least **20 spots per element** are analyzing grains
        larger than mapping probe diameter.
  - Identify phases in comment
      - Give same comments on the same phase with the similar
        compositions.
          - e.g., quartz, plagioclase, garnet-core, garnet-rim, …
      - An alternative is to use external file later.

### Mapping

  - Analytical conditions
      - Acceralating voltage must be same as that in spot analysis.
      - Probe diameter should be larger than that in spot analysis.
      - Probe current is recommended to be 100 nA following Lanari et
        al. (2014).
      - Dwell time is recommended to be 0.1 - 0.3 sec following Lanari
        et
al. (2014).

### Example of analytical conditions

|                      |   Spot |      Map | Comment                          |
| :------------------- | -----: | -------: | :------------------------------- |
| Acceralating Voltate |  15 kV |    15 kV | Must be same in spot and map     |
| Probe diameter       |   3 μm |    20 μm | Must be smaller in spot than map |
| Probe current        |  10 nA |   100 nA |                                  |
| Peak dwell           | 10 sec | 120 msec |                                  |
| Background dwell     |  5 sec |       NA | No need to analyze in map        |

## Export data

1.  **ASCII convert** mapping data into matrix format, and save the
    result in the directory where raw data is stored (e.g., `.map/1`).
2.  Export whole directory of analysis containing `.map` directory and
    `.qnt` directory

### Required files

#### Spot analysis

The exported data are stored in a directory named by `.qnt` in most
environments. If using JXA-8230, a directory’s name is
`{PROJECT}_{#}_QNT` where `{PROJECT}` is name of a project’s name
defined by user or “PROJECT” if undefined, and`{#}` is a variable
integer (e.g.,
`PROJECT_0001_QNT`).

| File name                              | Descriptions                                                                                                                                                                                                                                                                                                                         |
| :------------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| .cnd/elemw.cnd or Pos\_001/data001.qnt | Metadata including dwell time for peak and background, and relative positions of backgrounds. In some case, `.cnd/elemw.qnt` is incomplete or missing, and needs to be prepared manually (e.g., <https://gist.github.com/atusy/f1577b67b8874c9e915941c0725d0e22>). JXA-8230 lacks `.cnd/elemw.cnd`, but provides `Pos_001/data/qnt`. |
| bgm.qnt                                | Minus-side background intensity                                                                                                                                                                                                                                                                                                      |
| bgp.qnt                                | Plus-side background intensity                                                                                                                                                                                                                                                                                                       |
| elem.qnt                               | Element names specified for matrix corrections (e.g., oxide or metal in ZAF)                                                                                                                                                                                                                                                         |
| elint.qnt                              | Element names as is.                                                                                                                                                                                                                                                                                                                 |
| mes.qnt                                | Probe current                                                                                                                                                                                                                                                                                                                        |
| net.qnt                                | Net intensity                                                                                                                                                                                                                                                                                                                        |
| peak.qnt                               | Peak intensity. This file is optional. If missing, peak intensity is calculated from net and background intensities.                                                                                                                                                                                                                 |
| stg.qnt                                | Coordinates and comments                                                                                                                                                                                                                                                                                                             |
| wt.qnt                                 | Mass concentrations \[wt%\]                                                                                                                                                                                                                                                                                                          |

#### Map analysis

The exported data are stored in a directory `.map/{#}` where `{#}` is a
variable integer in most environments (e.g., `.map/1`). If using
JXA-8230, a directory name is `{PROJECT}_{#1}_MAP_{#2}_csv` where
`{PROJECT}` is name of a project’s name defined by user or “PROJECT” if
undefined, and`{#1}` and `{#2}` are variable integers (e.g.,
`PROJECT_0001_MAP_0001_csv`).

| File name   | Descriptions                                                                                           |
| :---------- | :----------------------------------------------------------------------------------------------------- |
| \*\_map.txt | ASCII converted mapping data                                                                           |
| \*.cnd      | Analytical conditions: element name, dwell time, probe current, step size, pixel size, and coordinates |

`*` indicates wild cards.

## Run QntMap on R

For data processing

### Interactive mode

Follow instructions shown by running the following code.

``` r
library(qntmap)
qntmap()
```

As a result, phase identification result is saved in “`clustering`”
directory and mass concentration data as csv files in “`qntmap`”
directory both under the directory contaning mapping data.

Note that interactive mode has limited functions. Use [manual
mode](#manual-mode) for full functionality.

### Manual mode

A work-flow is available with an example dataset at
<https://atusy.github.io/qntmap/articles/basic.html> .

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
## If modified, assign content of the modified csv file by running
## centers <- data.table::fread('path to the modified csv file')

# Phase identification
# Assign group_cluster = TRUE if you want to integrate same phases subgrouped by suffix after '_' 
# (e.g., garnet_a and garnet_b are integrated to garnet if TRUE)
cls <- cluster_xmap(xmap = xmap, centers = centers, group_cluster = FALSE)

# Quantify X-ray maps
qmap <- quantify(
  xmap = xmap, qnt = qnt, cluster = cls, fine_phase = fine_phase
)
## Resulting files are saved in `qntmap` directory` under `dir_map`.

# Summarize result
summary(qmap)
## This shows minimum, lower quantile, median, mean, upper quantile, and maximum values of variables.
```
