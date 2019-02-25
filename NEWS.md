# qntmap 0.3.4

## New features

- `cluster_xmap` suports `...` argument, which is passed to `PoiClaClu::Classify`.

## Major changes

- Drastically improved performance of `plot()`
- Clustering results will be saved as an RDS file, 
  a dot-by-dot `png` image without legend, and 
  a ggplot2 plot with legend saved as `svg`.
    - The size of ggplot2 plot is set to A4 (297 mm * 420 mm), 
      either horizontal or vertical which fits best to the plot.
    - The legend used to be saved as a pie chart.

# qntmap 0.3.3

## Breaking changes

- `quantify()` no more calculates standard errors by default.
  This is to save time and memory usage.
  Set `se = TRUE` to calculate the errors.
- Format of "params.csv" created by `quantify()` is changed.
    - Column "elem" is renamed to "oxide"
    - Column "elint" is renamed to "element"
    - Columns "*_se" are removed.

## New features

- `quantify()` and `tidy_epma()` supports maps and spots analyzing inequal elements.
    
    | Example | Mapped elements | Spot elements   |
    |:-------:|:----------------|:----------------|
    | A       | Si, **Ti**, Mg  | Si, Mg          |
    | B       | Si, Mg          | Si, **Ti**, Mg  |
    | C       | Si, **Ti**, Mg  | Si, Mg, **Ca**  |
    
- `cluster_xmap()` has dot-dot-dot argument to pass arguments to `PoiClaClu::Classify()`.
  This feature helps
    - train with limited data to save time
    - separate data into train and test for cross validations
    - and so on.

## Bug fix

- Calculation of standard errors for compositions of pixels were wrong.

# qntmap 0.3.2

## Major changes

- `quantify()` saves parameters alpha, beta, gamma, 
  and their standard errors when `saving = TRUE`.
- `quantify()` loads parameters from a csv file by 
  giving its path to `fix` parameter.
- `read_qnt()` and `read_xmap()` supports to retrieve analytical conditions from
  manually prepared csv files. This feature is implemented to avoid failures in
  reading data caused by difference in formats output by different EPMA 
  instruments.
- Hover on `plot()` of mapping data shows coordinates and 
  a chemical composition of a selected element.

# qntmap 0.3.1

## Major changes

- `quantify()` supports fixing compositions of certain components of phases.
- Defunct deprecated functions:
    - `qltmap_cls_centers`
    - `qltmap_load`
    - `qnt_load`
    - `qntmap_cls_pois`
    - `qntmap_quantify`
- Support for searching online documents: https://qntmap.atusy.net
    
## Minor changes

- Refactored to
    - simplify codes
    - separate monster functions into small ones
- Introduced continuous integrations:
    - Travis CI, AppVeyor, and Codecov
    - As Travis CI passes building on macOS, 
      [#23](https://github.com/atusy/qntmap/issues/23) is closed.
      
## Decisions

- `qntmap()` will be maintained without new features to focus on developing
  web user interface with `shiny` package.

# qntmap 0.3.0

## Major changes

- `plot()`
    - Interactive plot is a combination of 
      shiny, ggplot2, and more. No more using plotly.
        - Color control with histogram
        - Hover information of pixels
        - Zooming
        - Moving
        - Summarizing data in a table below heatmap
            - Double click to keep pixel data
            - Box select to keep mean values of data
    - Grayscale is supported in addition to viridis. 
      [#28](https://github.com/atusy/qntmap/issues/28)
    - Clustering result is supported.
      [#28](https://github.com/atusy/qntmap/issues/28)
- `segment()`
    - Only allows PNG format as an input. 
      No more JPG nor BMP allowed.
    - Independent from `imager` package.
      [#16](https://github.com/atusy/qntmap/issues/16)

# qntmap 0.2.2

## Major changes

### New features

- `read_xmap()` 
    - It checks version of QntMap which created former version of `xmap.RDS` 
      in case `renew = FALSE`
    - It's returning value has following attributes:
        - deadtime: deadtime specified by a parameter DT
        - dir_map: path of the directory which contains raw data
        - dwell: dwell time
        - current: probe current
        - start: mapping starting position
        - pixel: size of mapping area
        - step: step size of pixels
        - ver: version of qntmap package
        - instrument: version of EPMA
        - class: `c('qm_xmap', 'list')`
- plot
    - Uses viridis color scale: 
      https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
- plot_shiny
    - alos uses viridis color scale
    - limits hover info for performance improvement
    
## Minor changes

- `read_cnd`, `read_map_pos`, and `read_map_beam` 
  are no more in use but are remained for awhile.
  Note they are not exported functions.

# qntmap 0.2.1

## New features

- Visualize X-ray maps and quantitative maps using a `plot` function.
    - Parameter `shiny = TRUE` gives rich interactive features 
      although still under development.
- Functions to add phases manually 
  in case user finds some phases not quantified after EPMA analysis. 
  See 
  https://qntmap.atusy.net/articles/add_phase.html 
  for more detail.
- `mean` to calculate mean value of quantitative maps.
    - Parameter `index` enables index or mask based calculation of mean.
      See 
      https://qntmap.atusy.net/articles/qntmap.html#summary-based-on-mask-images 
      for more detail.

# qntmap 0.2.0

Check NEWS as 0.2.0 have lots of breaking changes.

## Breaking changes

- Renamed functions to follow `verb_hoge` style. 
  This leads some functions to be deprecated or defunct. 
  See "Summary of defunct, deprecated, and unexported functions".
- Unexported `cipois` and `flag0`.
- Renamed arguments to be shorter and easy-remembering.
- Reduced arguments which are reused again and again by utiliing `attributes`. 
  For example,
    - Path to the directory containing X-ray map data is specified by `dir_map` 
      or `wd` in use of `epma_tidy`, `qltmap_cls_centers`, `qltmap_cls_pois`, 
      `qltmap_load`, and so on. However, now is only specified in `read_xmap`. 
- The above change cause `read_qnt` to save its result (`qnt.RDS`) and 
  semi-product (`phase_list0.csv`) in the directory containing `.qnt` files 
  unlike `qnt_load` saved them under the working directory.

## Major changes

- Monster functions are separated into smaller functions.
    - A wrapper part of `PoiClaClu::Classify` in `cluster_xmap` 
      (formerly `qltmap_cls_pois`) is isolated as `cluster`
    - Internal parameters for `quantify` are calculated 
      using `find_AB`, `find_AG`, and `find_B`
    - Codes for saving result of functions are 
      isolated as S3 generics of `save4qm`.
- Condition files of EPMA analysis are read by S3 generics 
  to support various types of file formats.
- `find_centers` (former `qltmap_cls_centers`) become 
  more robust against multi-phase pixels by
      - finding pixels analyzing multi-phase pixels by 
        comaring with corresponding quantified points.
      - predicting X-ray map counts of a target phase in 
        multi-phase pixels based on regression analysis.
- `cluster_xmap` does not `group_clusters` by default.

## Minor changes

- Performance improvements by simplifying codes, 
  utilizing `matrixStats` package, and so on.
- `quantify` supports fixing internal parameters (`A` and `B`).

## Summary of defunct, deprecated, and unexported functions

| old (0.1.0)                 | new (0.2.0)         | condition of old  |
|:----------------------------|:--------------------|:------------------|
| `cipois`                    | `cipois`            | unexported        |
| `epma_tidy`                 | `tidy_epma`         | defunct           |
| `flag0`                     | `flag0`             | unexported        |
| `qltmap_cls_centers`        | `find_centers`      | deprecated        |
| `qltmap_cls_centers_random` | `find_centers_kpp`  | defunct           |
| `qltmap_cls_pois`           | `cluster_xmap`      | deprecated        |
| `qltmap_cls_pois_integrate` | `group_clusters`    | defunct           |
| `qltmap_load`               | `read_xmap`         | deprecated        |
| `qnt_load`                  | `read_qnt`          | deprecated        |
| `qntmap_quantify`           | `quantify`          | deprecated        |
