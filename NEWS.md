# qntmap 1.0.2

## Bug fix

- Fix `qntmap()` raising error on the **Mapping conditions** pane of the **Input** page, "Can't create column `Values`: Can't combine `..1` <character> and `..2` <integer>" (#34).

- Fix `cluster_xmap()` raising error when checking unit of pixels (#36)

## Internal changes

- Use GitHub Actions for CI (#35)
- Updated tests and `DESCRIPTION` to pass CI (#37)


# qntmap 1.0.1

- Phase list csv can be without id column.
- Reorder summarized tables in the shiny GUI.
- `autoplot.qm_xmap()` supports data with unknown step size.

## Bug fix

- Fix beta being wrong in the *Params for Quantify* page.

## Planning

- Add `remove_outliers` option to quantify and find_centers


# qntmap 1.0.0

## Biggest NEWS

- **Graphical user interface is ready!** Run `qntmap()` to enjoy it.

## Breaking changes

- `qntmap()` default to use `shiny` web app. Specify `shiny = FALSE` to use legacy
  console-based intearactive mode. Note that the legacy one is no more maintained.
- `read_qnt()`
    - omits the `renew` option
    - uses the `saving` option only to save phase list, no more to cache data
- `read_xmap()`
    - omits the `saving` and the `renew` option
    - returns a data frame with columns `x` and `y` indicating coordinates of
      pixels instead of a list
- `cluster_xmap()`
    - returns a data frame with the `center` attribute unlike older one returned
      a list
    - omits `group_cluster` option. Instead, apply `group_subclusters()` to the
      result to integrate subclusters.
    - saves the result
        - as is in the formats of 
          RDS, png (dot-by-dot phase map), and svg (phase map with legend)
        - converted by `group_subclusters` in the format of only png and svg
- `group_subclusters()`
    - is defined to defunct `group_cluster()`
    - returns a data frame in the same format as `cluster_xmap()` but lacks the
      `center` attribute.
- `pick()` changed its first argument to `.df` from `.data`.
- The `magma` color scale become the default one to plot of mapping data instead of the `viridis`.
- Use `message` rather than `cat` so that they can be suppressed by users.

## Major changes and new features

- `autoplot()` methods are implemented to follow the best practice of `ggplot2`.
  From now on, `plot()` methods are wrappers of `autoplot`.
- `find_outlier()` is implemented to detect outliers cause mainly by mapping
  fine grained phases.
- `find_outlier()` is implemented to detect unexpectedly high or low mapping
  intensities compared to spot intenstieis, which typically occurs due to
  outliers are analysing multi-phase pixels
- `find_centers()`
    - has more robust approach to find centroids powered by `find_outlier()`
    - has additional arguments
        - `phase` and `element`: Specifying them controls
          which phases and elements are used to find outliers. They suppot
          tidyeval. For example `element = c(Si, Ti, Al)` indicates the 3
          elements are used, and `element = c(-Si, -Ti, -Al)` indicates the 3
          elements are NOT used.
        - `epma`: If `tidy_epma` is already performed, then specify the result
          here to save time.
    - deprecates `fine_phase` which is generalized by `phase`.
- `tidy_epma` has new arguments `subcluster` and `suffix`.
- `hmean()` and `vmean()` supports the objects with class `qntmap`, `qm_xmap`,
  and `qm_cluster`.mPreviously they only supported the `qntmap` class object.
- All layers for `ggplot2` are no longer pre-compiled at installation stage.
  This is to avoid possible conflicts on updates on `ggplot2` after installing
  `qntmap`.
- `quantify()` get the `params` attribute showing the parameters, $alpha$,
  $beta$, and $gamma$.
- `autoplot.qm_epma()` is defined for the result of `tidy_epma()`

## Internal changes

- `tidy_epma()` returns `qm_epma` class object.
- `gghist.numeric()`
    - draws border to be legible regardless of background colors.
    - supports a histogram with a single column.
- `correct_deadtime()` supports re-calculation of dead time.
- `prioritize()` accepts unnamed vector.
- `save4qm()` methods, `summary()` methods, `pick()`, `mean()`, `vmean()`, and
  `hmean()` are re-written to support new data structures.
- `correct_deadtime()` is factored out from `read_xmap()`, but not exported currently.
- Themes for plot supports modifying `base_size`, so that the appearance can be optimized in Shiny App.
- Full support of tidyeval
- Tidyverse and r-lib related functions are imported in `qntmap-package.R` to
  avoid importing the same funcions in multiple files.
- Tests run faster by exporting duplicated codes to `setup.R`.

# qntmap 0.3.4

## New features

- `cluster_xmap()` passes `...` to `PoiClaClu::Classify()`, enabling
    - training with selected data
    - testing with selected data
    - fine tuning on parameters
    - and so on.
- Added `vmean()` and `hmean()` to calculate profile of mean values of mapping data. `vmean()` stands for vertical mean and `hmean()` for horizontal mean.

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
