#' Enhance quantitative analysis of EPMA maps with QntMap
#'
#' QntMap enables conversion of EPMA X-ray maps to mass concetration maps.
#' See website for more information <https://qntmap.atusy.net>.
#'
#' @name qntmap-package
#' @docType package
#'
#' @references
#'   Yasumoto, A., Yoshida, K., Kuwatani, T., Nakamura, D., Svojtka, M., &
#'   Hirajima, T. (2018).
#'   A rapid and precise quantitative electron probe chemical mapping technique
#'   and its application to an ultrahigh-pressure eclogite
#'   from the Moldanubian Zone of the Bohemian Massif
#'   (Nové Dvory, Czech Republic).
#'   American Mineralogist, 103(10), 1690-1698,
#'   <https://doi.org/10.2138/am-2018-6323CCBY>.
#'
#' @importFrom dplyr
#'   anti_join any_vars arrange
#'   bind_cols bind_rows
#'   distinct distinct_all
#'   filter filter_all
#'   group_by group_modify
#'   left_join
#'   mutate mutate_at
#'   n
#'   rename rename_at right_join row_number
#'   select select_if semi_join summarize summarize_all summarize_if
#'   transmute transmute_at
#'   ungroup
#'   vars
#' @importFrom ggplot2
#'   aes annotation_raster autoplot
#'   coord_cartesian coord_fixed
#'   element_blank element_rect
#'   geom_bar geom_col geom_step geom_smooth geom_tile
#'   ggplot ggsave guide_colorbar guides
#'   labs
#'   position_nudge
#'   scale_alpha_identity scale_color_discrete
#'   scale_color_gradient scale_color_manual scale_color_viridis_c
#'   scale_fill_gradient scale_fill_manual scale_fill_viridis_c
#'   scale_y_reverse
#'   stat
#'   theme theme_classic theme_minimal
#' @importFrom pipeR %>>%
#' @importFrom purrr map map2 map_at map_dbl map_if modify_at pmap walk2
#' @importFrom rlang !! .data enquo
#' @importFrom stringr 
#'   str_c str_detect str_extract str_replace str_replace_all str_subset
#' @importFrom tidyr
#'   gather separate spread unnest
#' @importFrom tidyselect
#'   ends_with everything matches starts_with vars_select
#'   
NULL

# 10 major oxides
.oxide <- c(
  "SiO2", "TiO2", "Al2O3", "Cr2O3", "FeO", "MnO", "MgO", "CaO", "Na2O", "K2O"
)
# 10 major elements
.element <- c("Si", "Ti", "Al", "Cr", "Fe", "Mn", "Mg", "Ca", "Na", "K")
# Signals
.electron <- c("BSE", "BSI", "COMPO", "CP", "SEI", "SL", "TOPO", "TP")
# combination of the above
.component <- c(.electron, .element, .oxide)

utils::globalVariables(".")
