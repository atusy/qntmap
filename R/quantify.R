#' Quantify X-ray maps
#'
#' @param xmap `qm_xmap` class object returned by [`read_xmap()`].
#' @param qnt `qm_qnt` class object returned by [`read_qnt()`].
#' @param cluster `qm_cluster` class object returned by [`cluster_xmap()`].
#' @param maps_x,maps_y
#'   Sizes of maps along x- and y-axes comprising guide net map.
#'   (default: `NULL`).
#' @inheritParams find_centers
#' @param fine_th A threshold of membership degrees to 0.9
#' @param fix
#'   A path to the file specifying chemical compositions of
#'   some elements in some phases (default: `NULL`).
#' @param se
#'   `FALSE` in default, and is forced when `fix` is specified.
#'   `TRUE` calculates standard errors, but require large memories.
#' @param saving
#'   `TRUE` (default) saves the results into `qntmap` directory under
#'   the directory `xmap` is read from. `FALSE` does not save.`
#'
#' @importFrom stats setNames
#'
#' @export
quantify <- function(
                     xmap,
                     qnt,
                     cluster,
                     maps_x = attr(xmap, "pixel")[1L],
                     maps_y = attr(xmap, "pixel")[2L],
                     fine_phase = NULL,
                     fine_th = 0.9,
                     fix = NULL,
                     se = FALSE,
                     saving = TRUE
) {

  cd <- getwd()
  on.exit(setwd(cd))

  # Mapping conditions
  dir_map <- attr(xmap, "dir_map")
  pixel <- attr(xmap, "pixel")

  stg <- do.call(
    flag0,
    unclass(expand.grid(
      x_stg = seq(0L, pixel[1L] - 1L) %/% maps_x + 1L,
      y_stg = seq(0L, pixel[2L] - 1L) %/% maps_y + 1L
    ))
  )

  se <- if (is.null(fix)) se else FALSE                  # © 2018 JAMSTEC
  params <- if (is.null(fix)) list() else fread(fix)    # © 2018 JAMSTEC
  TF_inherit_params <- check_ABG(params, xmap, cluster) # © 2018 JAMSTEC

  X <- select(cluster, -"x", -"y", -"cluster", -"membership")

  # Find alpha (A), beta (B), and gamma (G)
  if (TF_inherit_params) {                              # © 2018 JAMSTEC
    AG <- fix_AG(params)                                # © 2018 JAMSTEC
    B <- fix_B(params)                                  # © 2018 JAMSTEC
    nm <- setNames(params$oxide, params$element)        # © 2018 JAMSTEC
    nm <- nm[!duplicated(nm)]                           # © 2018 JAMSTEC
  } else {
    # Tidy compilation of epma data
    epma <- tidy_epma_for_quantify(
      tidy_epma(qnt = qnt, xmap = xmap, cluster = cluster) %>>%
        filter(.data$elint %in% names(!!xmap)),
      maps_x, maps_y,
      elements = qnt$elm$elem,
      distinguished = any(grepl("_", colnames(cluster$membership))),
      fine_phase = fine_phase,
      fine_th = fine_th
    )
    AG <- find_AG(epma, setdiff(names(X), unique(epma$phase3))) # Future work: supress calc se
    B <- find_B(epma)                                           # Future work: supress calc se
    rm(epma)
    nm <- setNames(qnt$elm$elem, qnt$elm$elint)
  }
  
  names(xmap) <- setdiff(names(xmap), names(nm)) %>>% 
    setNames(.) %>>%
    c(nm) %>>%
    `[`(names(xmap))
  # names(xmap) <- c(x = "x", y = "y", nm)[names(xmap)]

  XAG <- find_XAG(
    X,
    AG %>>%
      mutate(
        ag = .data$a * .data$g, 
        ag_se = `if`(!!se, L2(.data$a * .data$g_se, .data$g * .data$a_se), NA_real_),
        g = NULL, g_se = NULL
      ),
    se = se
  )

  AB <- find_AB(AG, B, se = se) %>>%
    join_AB(fix_AB_by_wt(xmap = xmap, cls = cluster, params = params))

  dir_qntmap <- paste0(dir_map, "/qntmap")
  dir.create(dir_qntmap, FALSE)

  if (is.null(fix) && saving) # © 2018 JAMSTEC
    save4qm(tidy_params(AG, B, qnt), nm = file.path(dir_qntmap, "parameters.csv")) # © 2018 JAMSTEC

  rm(AG, B)

  AB %>>%
    rename(se = "ab_se") %>>%
    select(setdiff(names(.), "se"[!(!!(se))])) %>>%
    expand_AB(stg) %>>%
    lapply(function(x) { # XAB and its err
      x %>>%
        lapply(`*`, X) %>>%
        map_at("se", lapply, square) %>>%
        lapply(reduce_add) %>>%
        map_at("se", sqrt)
    }) %>>%
    map2(xmap[names(.)], function(xab, i) lapply(xab, `*`, i)) %>>% # XABI
    map2(XAG, function(xabi, xag) c( # XABI - XAG
      wt = list(xabi[[1L]] - xag[[1L]]), 
      se = if (se) list(L2(xabi[[2L]], xag[[2L]]))
    )) %>>% 
    lapply(map_at, 'wt', as_positive) %>>%
    c(list(
      Total = c(
        list(wt = reduce_add(lapply(., `[[`, "wt"))),
        `if`(se, list(
          se = sqrt(reduce_add(lapply(lapply(., `[[`, "se"), square)))
        ))
      )
    )) %>>%
    prioritize(.component) %>>%
    unlist(recursive = FALSE) %>>%
    bind_cols(select(xmap, "x", "y")) %>>%
    select("x", "y", everything()) %>>%
    rename_at(vars(ends_with(".wt")), str_replace, "\\.wt$", "") %>>%
    as.data.frame %>>%
    structure(
      pixel = attributes(xmap)$pixel,
      step  = attributes(xmap)$step,
      class = c("qntmap", class(.))
    ) %>>%
    save4qm(nm = dir_qntmap, saving = saving)
}

#' Check alpha, beta, and gamma in params
#' @noRd
#'
#' @return `TRUE` or `FALSE`
#' @note © 2018 JAMSTEC
#'
check_ABG <- function(params, xmap, cls) {
  # FALSE if not fixed
  if (!is.data.frame(params)) return(FALSE)

  # FALSE if only fixing product of alpha and beta
  nm <- names(params)
  nm_common <- c("oxide", "phase")
  nm_wt <- c("wt")
  nm_AGB <- c("element", "alpha", "beta", "gamma")

  # FALSE if parameters are only fixed by wt
  if ((!all(nm_AGB %in% nm)) & (all(c(nm_common, nm_wt) %in% nm)))
    return(FALSE)

  # FALSE if required columns are missing
  col_missing <- setdiff(c(nm_AGB, nm_common), names(params))
  if (length(col_missing) > 0L)
    stop(
      "Tried to fix parameters, ",
      "but there are missing columns in the input file: ",
      paste(col_missing, collapse = ", ")
    )

  # Check if all elements are quantified
  element_missing <- expand.grid(
    phase = unique(cls$cluster),
    element = setdiff(names(xmap), c("x", "y", .electron)),
    stringsAsFactors = FALSE
  ) %>>%
    anti_join(params, by = c("phase", "element")) %>>%
    `[[`("element") %>>%
    unique

  if (length(element_missing) > 0L)
    stop(
      "Tried to fix parameters, ",
      "but there are missing elements in the input file: ",
      paste(element_missing, collapse = ", ")
    )

  return(TRUE)
}
