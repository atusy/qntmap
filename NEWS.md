# qntmap 0.2.0

Check NEWS as 0.2.0 have lots of breaking changes.

## Breaking changes

- Renamed functions to follow `verb_hoge` style. 
  This leads some functions to be deprecated or defunct. 
  See "Summary of defunct, deprecated, and unexported functions".
- Unexported `cipois` and `flag0`.
- Renamed arguments to be shorter and easy-remembering.
- Reduced arguments which are reused again and again by utiliing `attributes`. For example,
    - Path to the directory containing X-ray map data is specified by `dir_map` or `wd` in use of `epma_tidy`, `qltmap_cls_centers`, `qltmap_cls_pois`, `qltmap_load`, and so on. However, now is only specified in `read_xmap`. 
- The above change cause `read_qnt` to save its result (`qnt.RDS`) and semi-product (`phase_list0.csv`) in the directory containing `.qnt` files unlike `qnt_load` saved them under the working directory.

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