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
#' 
#' @importFrom pipeR %>>%
#' 
NULL

# 10 major oxides
.oxide <- c(
    "SiO2", "TiO2", "Al2O3", "Cr2O3", "FeO", "MnO", "MgO", "CaO", "Na2O", "K2O"
  )
# 10 major elements
.element <- c("Si", "Ti", "Al", "Cr", "Fe", "Mn", 'Mg', "Ca", "Na", "K")
# Signals
.electron <- c("BSE", "BSI", "COMPO", "CP", "SEI", "SL", "TOPO", "TP")
# combination of the above
.component <- c(.electron, .element, .oxide)

# global variables used in NSE
utils::globalVariables(c(
  ".", ".A", ".B", ".index", ".kept", ".tmp", ".val", ".var",
  "a", "a_se", "Area", "b", "b_se", "beam_map", "bg_t", 
  "bgint", "bgint.H", "bgint.L", "bgm", "bgm_pos", "bgm.H", 
  "bgm.L", "bgm2", "bgp", "bgp_pos", "bgp2", "cls", 
  "components", "count", "data", "dwell", "elem", "Element", 
  "elint", "elm", "fit", "g", "g_se", "i", "id", "ID", "k",
  "len_eq_1", "map_est", "mapint", "mem", "n_within_pi", "net", 
  "no", "not_mapped_phase", "nr", "nr0", "path", "phase", "Phase", 
  "phase2", "phase3", "pi_H", "pi_L", "pk_t", "pkint", "pkint.H", 
  "pkint.L", "se", "stat", "stg", "use", "V0", "val", "var", "w", 
  "within_pi", "wt", "x", "x_px", "x_stg", "y", "y_px", "y_stg"
))
