#' Compile epma data into a data frame
#'
#' @param qnt object returned by read_qnt
#' @param xmap object returned by read_xmap
#' @param cluster object returned by cluster_xmap
#'
#' @importFrom matrixStats rowMaxs
#' @importFrom stats setNames
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
  qnt$cmp <- as.list(xmap)[qnt$elm$elint] %>>%
    setNames(qnt$elm$elem) %>>%
    lapply(unlist, use.names = FALSE) %>>%
    lapply(function(x) `if`(is.null(x), NA_integer_, x)) %>>%
    as.data.frame %>>%
    `[`(qnt$cnd$nr, , drop = FALSE) %>>%
    list %>>%
    setNames("map") %>>%
    c(lapply(qnt$cmp, `[`, qnt$cnd$id, , drop = FALSE)) %>>%
    lapply(mutate, id = qnt$cnd$id) %>>%
    bind_rows(.id = ".var") %>>%
    gather("elm", ".val", -".var", -"id") %>>%
    spread(".var", ".val")

  ## join cmp, cnd, elem in qnt
  ## calculate 95% ci of data
  clustered <- !is.null(cluster)
  qnt$cnd %>>%
    mutate(
      cls = !! `if`(clustered, cluster$cluster[qnt$cnd$nr], NA_real_),
      mem = !! `if`(clustered, cluster$membership[qnt$cnd$nr], NA_real_),
      elm = !! list(qnt$elm)
    ) %>>%
    unnest %>>%
    rename(elm = .data$elem) %>>%
    left_join(qnt$cmp, by = c("id", "elm")) %>>%
    mutate(
      elint = ifelse(is.na(.data$elint), .data$elm, .data$elint),
      beam = .data$beam * 1e+6,
      beam_map = .data$beam_map * 1e+6,
      mapint = .data$map / .data$dwell / .data$beam_map,
      bgm2 = .data$bgm * .data$bgm_pos,
      bgp2 = .data$bgp * .data$bgp_pos,
      bgint = .data$bgm2 + .data$bgp2,
      pkint = `if`(
        exists("pkint"),
        .data$pkint, .data$net + .data$bgint  / (.data$bgm_pos + .data$bgp_pos)
      ),
      pkint = .data$pkint * (.data$pkint > 0L)
    ) %>>%
    cipois(
      vars = c("pkint", "bgm", "bgp", "mapint"),
      offset =
        transmute(
          .,
          pkint = .data$pk_t * .data$beam,
          bgm = .data$bg_t * .data$beam,
          bgp = .data$bgm, # == bg_t * beam
          mapint = .data$dwell * .data$beam_map
        )
    ) %>>%
    mutate(
      beam = .data$beam * 1e-6,
      beam_map = .data$beam_map * 1e-6,
      .tmp = .data$bgm.L * .data$bgm_pos,
      bgint.L = .data$bgint - propagate_add(.data$bgm2, .data$.tmp, .data$bgp2, .data$.tmp),
      .tmp = .data$bgm.H * .data$bgm_pos,
      bgint.H = .data$bgint + propagate_add(.data$bgm2, .data$.tmp, .data$bgp2, .data$.tmp),
      .tmp = .data$bgp_pos + .data$bgm_pos,
      bgint = .data$bgint / .data$.tmp,
      bgint.L = .data$bgint.L / .data$.tmp,
      bgint.H = .data$bgint.H / .data$.tmp,
      .tmp = NULL,
      net.L = .data$net - propagate_add(.data$pkint, .data$pkint.L, .data$bgint, .data$bgint.L),
      net.H = .data$net + propagate_add(.data$pkint, .data$pkint.H, .data$bgint, .data$bgint.H)
    ) %>>%
    as.data.frame
}



#' Only used interanlly by quantify
#' @noRd
tidy_epma_for_quantify <- function(
                                   epma, maps_x, maps_y, elements,
                                   distinguished = FALSE, fine_phase = NULL, fine_th = .9
) {
  mutate(
    epma[epma$elm %in% elements, ],
    net = .data$net * (.data$net > 0L),
    phase3 = if (!!distinguished) .data$phase else .data$phase2,
    x_stg = ((.data$x_px - 1L) %/% !!maps_x + 1L) * 
      (0L < .data$x_px) * (.data$x_px <= !!maps_x),
    y_stg = ((.data$y_px - 1L) %/% !!maps_y + 1L) * 
      (0L < .data$y_px) * (.data$y_px <= !!maps_y),
    stg = ifelse(
      (.data$x_stg * .data$y_stg) <= 0L, 
      NA_character_,
      flag0(.data$x_stg, .data$y_stg)
    ),
    mem = .data$mem *
      (str_replace(.data$cls, "_.*", "") == .data$phase2) *
      (.data$cls %nin% !!fine_phase) *
      (.data$mem > !!fine_th)
  )
}
