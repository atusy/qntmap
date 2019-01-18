Enhance quantitative analysis of EPMA maps with QntMap
================

[![Travis build
status](https://travis-ci.org/atusy/qntmap.svg?branch=master)](https://travis-ci.org/atusy/qntmap)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/atusy/qntmap?branch=master&svg=true)](https://ci.appveyor.com/project/atusy/qntmap)
[![Coverage
status](https://codecov.io/gh/atusy/qntmap/branch/master/graph/badge.svg)](https://codecov.io/github/atusy/qntmap?branch=master)
[![license](https://img.shields.io/badge/license-MIT-blue.svg)](https://atusy.github.io/qntmap/LICENSE-text.html)

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

| File name                 | Descriptions                                                                                                                                                                       |
| :------------------------ | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| \*\_map.txt or data\*.csv | ASCII converted mapping data (e.g., 1\_map.txt, 2\_map.txt,… or data001.csv, data002.csv, …)                                                                                       |
| \*.cnd                    | Analytical conditions: element name, dwell time, probe current, step size, pixel size, and coordinates. File names must corresponds to mapping data (e.g, 1\_map.cnd, data001.cnd) |

`*` indicates wild cards.

## Run QntMap on R

For data processing.

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
<https://atusy.github.io/qntmap/articles/qntmap.html> .
