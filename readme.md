---
title: "QntMap"
output: 
  html_document: 
    number_sections: TRUE
    keep_md: TRUE
---



# Overview

This package provides functions to convert element characteristic X-ray intensity maps into element mass concentration maps.  
Current version supports data from JEOL-style electron probe microanalyer (EPMA).  
For conversion, you need to run spot analysis before mapping analysis.  
See "How to" section for usage and Yasumoto et al. (submitted) for implementations.

# Installation

Install devtools package if you haven't.


```r
install.packages("devtools")
```

Then, run following code.


```r
devtools::install_github("atusy/qntmap")
```

# How to

## EPMA analysis

QntMap handles matrix effect by preparing internal standards based on spot analysis.


### Spot analysis

- Analytical conditions
    - Same as those conventionally applied in your lab.
    - Use wavelength-dispersive X-ray spectrometer
- Spots to be analyzed
    - **20 spots per phase** in the area to be mapped (the more is better).
    - It is better but not necessary to quantify grains larger than mapping probe diameter.
    - Make sure at least **20 spots per element** are analyzing grains larger than mapping probe diameter.
- Commenting analysis
    - Give same comments on the same phase with the similar compositions. Do not number them.
      - e.g., quartz, plagioclase, ...
    - Otherwise, comment them with different name. This is important treatment for phase identification and handling matrix effect.
      - e.g., garnet-core, garnet-rim
    - If you give comments containing underscore such as "garnet_core" and "garnet_rim", then they are treated as if different phases in phase identification, but are treated as if their matrix effect are approximately same. 
    - Alternatively, give comments manually by external file.
    
Spot quantitative analytical conditions in your lab.

### Mapping analysis

- Analytical conditions
    - Acceralating voltage should be same as one applied in spot analysis.
    - Probe diameter should be larger than spot analysis. The larger saves more time, but decreases spatial resolutions.
    - Probe current is recommended to be 100 nA following Lanari et al. (2014).
    - Dwell time is recommended to be 0.1 - 0.3 sec following Lanari et al. (2014).
    - Note that increasing probe current accepts decreasing dwell time, but probe current must not be too high to saturate X-ray detectors. If you prefer high probe current in some reason, consider changing dispersive crystals from those chosen in spot analysis.
        - e.g., chose PET instead of TAP for Si


### Example of analytical conditions

Yasumoto et al. (submitted)

|                     | Spot  | Map     | Comment                         |
|:--------------------|------:|--------:|:--------------------------------|
|Acceralating Voltate | 15 kV | 15 kV   | Must be same in spot and map    |
|Probe diameter       | 3 μm  | 20 μm   | Must be smaller in spot than map|
|Probe current        | 10 nA | 100 nA  |                                 |
|Peak dwell           | 10 sec| 120 msec|                                 |
|Background dwell     |  5 sec| NA      | No need to analyze in map       |


## Data processing with QntMap package on R

### Quantification

By running following code, you'll see that phase identification result in 'clustering' directory and mass concentration data as csv files in 'qntmap' directory both under the directory contaning mapping data.

#### Interactive mode

Follow instructions shown by running the following code.


```r
library(qntmap)
qntmap()
```


#### Manual mode for experts (example)


```r
library(qntmap)

setwd("Directory which contains analytical data like .qnt directory, .map directory, and so on")
dir_map <- "path to the directory containing X-ray map files (directory containing 1_map.txt, 2_map.txt, and so on)"

# Load mapping data
# Change value of DT (dead time in nanoseconds) depending on EPMA.
# 1100 ns is a value applied by JEOL JXA-8105.
xmap <- qltmap_load(dir_map, DT = 1100)

# Compile quantitative data
qnt <- qnt_load()
## If you want to change phase names of each analysis different from those determined preliminary given during EPMA analysis, prepare csv file that indicates phase name, and input its path to phase_list parameter.
## In addition, make renew = TRUE.
## qnt <- qnt_load(phase_list = "phase_list.csv", renew = TRUE)

# Determine initial cluster centers
centers <- qltmap_cls_centers(qnt = qnt, qlmap = xmap, dir_map = dir_map)

# Phase identification
# assign integration = FALSE if you do not want to integrate underscored phases (e.g., garnet_a and garnet_b are integrated to garnet if TRUE)
cls <- qltmap_cls_pois(centers, xmap, wd = dir_map)

# quantify X-ray maps
qntmap <- qntmap_quantify(
  dir_map = dir_map,
  qnt = qnt,
  cluster = cls,
  fine_phase = NULL #Specify phase whose grain size tend to be smaller than mapping probe diameter.
)
```

# Upcoming updates

- Automation
    - Instead of running each functions manually and giving parameter in R console, 
      I am planning to add a function called "qntmap", 
      which runs all commands above and determines parameters based on a given text file.
- HTML reporting


