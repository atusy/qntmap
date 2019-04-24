#' Compile epma data into a data frame
#'
#' @param qnt object returned by read_qnt
#' @param xmap object returned by read_xmap
#' @param cluster object returned by cluster_xmap
#'
#' @importFrom dplyr distinct left_join mutate mutate_at rename transmute
#' @importFrom matrixStats rowMaxs
#' @importFrom purrr map2
#' @importFrom rlang !!
#' @importFrom stats setNames
#' @importFrom stringr str_c str_replace_all str_replace str_detect
#'
#' @export
#'
#'
tidy_epma <- function(
                      qnt,
                      xmap, # NA,
                      cluster = NULL
) {

  # load mapping conditions
  pos <- attributes(xmap)[c("start", "pixel", "step")]
  beam <-
    setNames(attributes(xmap)[c("current", "dwell")], c("beam_map", "dwell"))
  inst <- attributes(xmap)[["instrument"]]

  # tidy data

  ## mm -> px
  qnt$cnd <- qnt$cnd[!is.na(qnt$cnd$phase), ] %>>%
    mutate(
      x_px = (round((x - pos$start[1L]) * 1e3 / pos$step[1]) + 1L) *
        `if`(inst %in% "JXA8230", -1L, 1L),
      y_px = round((y - pos$start[2L]) * 1e3 / pos$step[2]) + 1L,
      nr0 = `if`(
        inst %in% "JXA8230",
        (x_px - 1L) * pos$pixel[2L] + y_px,
        (y_px - 1L) * pos$pixel[1L] + x_px
      ),
      nr = ifelse(0L < nr0 & nr0 < prod(pos$pixel), nr0, NA_integer_),
      phase2 = str_replace(phase, "_.*", "")
    ) %>>%
    distinct(nr0, .keep_all = TRUE)

  qnt$elm$dwell <- beam[["dwell"]] * 1e-3
  qnt$elm$beam_map <- beam[["beam_map"]]

  ## Error if
  if (all(is.na(qnt$cnd$nr))) stop("No points are quantified in mapping area.")

  ## Let's join
  qnt$cmp <- xmap[qnt$elm$elint] %>>%
    setNames(qnt$elm$elem) %>>%
    lapply(unlist, use.names = FALSE) %>>%
    lapply(function(x) `if`(is.null(x), NA_integer_, x)) %>>%
    as.data.frame %>>%
    `[`(qnt$cnd$nr, ) %>>%
    list %>>%
    setNames("map") %>>%
    c(lapply(qnt$cmp, `[`, qnt$cnd$id, )) %>>%
    lapply(mutate, id = qnt$cnd$id) %>>%
    bind_rows(.id = ".var") %>>%
    gather(elm, .val, -.var, -id) %>>%
    spread(.var, .val)

  ## join cmp, cnd, elem in qnt
  ## calculate 95% ci of data
  clustered <- !is.null(cluster)
  qnt$cnd %>>%
    mutate(
      cls = `if`(!!clustered, cluster$cluster[qnt$cnd$nr], NA_real_),
      mem =
        `if`(!!clustered, rowMaxs(cluster$membership[qnt$cnd$nr, ]), NA_real_),
      elm = list(qnt$elm)
    ) %>>%
    unnest %>>%
    rename(elm = elem) %>>%
    left_join(qnt$cmp, by = c("id", "elm")) %>>%
    mutate(
      elint = ifelse(is.na(elint), elm, elint),
      beam = beam * 1e+6,
      beam_map = beam_map * 1e+6,
      mapint = map / dwell / beam_map,
      bgm2 = bgm * bgm_pos,
      bgp2 = bgp * bgp_pos,
      bgint = bgm2 + bgp2,
      pkint = `if`(exists("pkint"), pkint, net + bgint  / (bgm_pos + bgp_pos)),
      pkint = pkint * (pkint > 0L)
    ) %>>%
    cipois(
      vars = c("pkint", "bgm", "bgp", "mapint"),
      offset =
        transmute(
          .,
          pkint = pk_t * beam,
          bgm = bg_t * beam,
          bgp = bgm, # == bg_t * beam
          mapint = dwell * beam_map
        )
    ) %>>%
    mutate(
      beam = beam * 1e-6,
      beam_map = beam_map * 1e-6,
      .tmp = bgm.L * bgm_pos,
      bgint.L = bgint - propagate_add(bgm2, .tmp, bgp2, .tmp),
      .tmp = bgm.H * bgm_pos,
      bgint.H = bgint + propagate_add(bgm2, .tmp, bgp2, .tmp),
      .tmp = bgp_pos + bgm_pos,
      bgint = bgint / .tmp,
      bgint.L = bgint.L / .tmp,
      bgint.H = bgint.H / .tmp,
      .tmp = NULL,
      net.L = net - propagate_add(pkint, pkint.L, bgint, bgint.L),
      net.H = net + propagate_add(pkint, pkint.H, bgint, bgint.H)
    ) %>>%
    as.data.frame
}



#' Only used interanlly by quantify
#' @importFrom dplyr mutate
#' @importFrom rlang !!
#' @importFrom stringr str_replace
#' @noRd
tidy_epma_for_quantify <- function(
                                   epma, maps_x, maps_y, elements,
                                   distinguished = FALSE, fine_phase = NULL, fine_th = .9
) {
  mutate(
    epma[epma$elm %in% elements, ],
    net = net * (net > 0L),
    phase3 = if (!!distinguished) phase else phase2,
    x_stg = ((x_px - 1L) %/% !!maps_x + 1L) * (0L < x_px) * (x_px <= maps_x),
    y_stg = ((y_px - 1L) %/% !!maps_y + 1L) * (0L < y_px) * (y_px <= maps_y),
    stg = ifelse((x_stg * y_stg) <= 0L, NA_character_, flag0(x_stg, y_stg)),
    mem = mem *
      (str_replace(cls, "_.*", "") == phase2) *
      (cls %nin% fine_phase) *
      (mem > fine_th)
  )
}
