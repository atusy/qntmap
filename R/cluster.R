#' Poisson distribution based custering based on [`PoiClaClu::Classify()`]
#'
#' @param centers c-by-p matrix returned by [`find_centers()`] or by manually;
#'   c clusters and p features.
#'   Used to guess initial centers (or centroids) of clusters.
#'   A value returned by , typically [`data.frame`] or [`matrix`],
#'   indicating initial guess centers (or centroids) or clusters.
#'   See [`find_centers()`].
#' @inheritParams PoiClaClu::Classify
#' @inheritDotParams PoiClaClu::Classify -x -y -xte
#' @inherit PoiClaClu::Classify return references
#' @seealso [PoiClaClu::Classify()], [find_centers()]
#'
#' @importFrom PoiClaClu Classify
#' @importFrom matrixStats colSums2
#' @export
cluster <- function(x, centers, xte = x, ...) {
  x_trans <- t(x)
  y <- centers %>>%
    apply(1L, function(y) colSums2(square(x_trans - y))) %>>%
    apply(1L, which.min)
  rm(x_trans)
  Classify(x, y, xte, ...)
}



#' Cluster mapping data into mineral species
#'
#' @inheritParams cluster
#' @param xmap
#'   A `qm_xmap` class object returned by [`read_xmap()`]
#' @param elements
#'   A character vector to chose elements to be utilized in cluster analysis.
#'   `NULL` (default) selects as much elements as possible.
#' @param saving
#'   `TRUE` or `FALSE` to save result. 
#'   Specifying `xte` coerces `saving` to be `FALSE`.
#' @param suffix
#'   A regular expression of suffix of cluster names.
#'   Clusters with the same prefix comprise a super cluster.
#'   For example, "Pl_NaRich" and "Pl_NaPoor" becomes "Pl" cluster if
#'   `suffix = "_.*"` (default).
#' @inheritDotParams PoiClaClu::Classify -x -y
#'
#' @importFrom matrixStats rowMaxs rowSums2
#'
#' @export
cluster_xmap <- function(
                         xmap,
                         centers,
                         elements = intersect(names(xmap), colnames(centers)),
                         saving = TRUE,
                         suffix = "_.*",
                         ...
) {
  xte <- list(...)[["xte"]]
  if (!is.null(xte)) saving <- FALSE
  centers$phase <- as.character(centers$phase)

  # Classify by PoiClaClu
  result <- cluster(
    xmap[elements], centers[elements], ...
  )[c("ytehat", "discriminant")]

  # give phase names to result$ytehat
  clusters <- centers$phase[result$ytehat]

  # estimate membership of each clusters
  find_membership(result$discriminant, centers, clusters) %>>%
    cbind(membership = rowMaxs(.)) %>>%
    as.data.frame %>>%
    mutate(cluster = !!clusters) %>>%
    bind_cols(if (is.null(xte)) xmap[c("x", "y")]) %>>%
    prioritize(c("x", "y", "cluster", "membership")) %>>%
    structure(
      class = c("qm_cluster", class(.)),
      centers = `if`(is.null(xte), select(xmap, -"x", -"y"), xte) %>>%
        mutate(phase = !! clusters) %>>% 
        group_by(.data$phase) %>>%
        summarize_all(median),
      step = attributes(xmap)$step
    ) %>>%
    save4qm(saving = saving, dir_out = attr(xmap, "dir_map"), suffix = suffix)
}

#' Find membership degrees based on discriminant
#' 
#' If some clusters are removed due to sparsity, they are added with 0 values.
#' 
#' @param discriminant Discriminant returned by `cluster`
#' @param centers Initial centers
#' @param clusters Name of hard clusters
#' 
#' @noRd
#' @return numeric matrix
find_membership <- function(discriminant, centers, clusters) {
  membership <- exp(discriminant - rowMaxs(discriminant))
  membership <- membership / rowSums2(membership)

  if (nrow(centers) == ncol(membership)) {
    colnames(membership) <- centers$phase
    return(membership)
  }

  if (ncol(membership) == 1L) {
    colnames(membership) <- clusters[1L]
  } else {
    TF <- !duplicated(clusters)
    colnames(membership)[apply(membership[TF, ], 1L, which.max)] <- clusters[TF]
  }

  missings <- setdiff(centers$phase, colnames(membership))

  structure(
    c(membership, integer(nrow(membership) * length(missings))),
    .Dim = c(nrow(membership), length(missings) + ncol(membership)),
    .Dimnames = list(NULL, c(colnames(membership), missings))
  )[, centers$phase]
}


#' Group sub-clusters who share same prefix
#'
#' When data points are assigned to clusters A_1 and A_2,
#' their clusters are renamed to be A by matching regular expressions.
#' 
#' @param x The `qm_cluster` class object.
#' @inheritParams cluster_xmap
#'
#' @export
group_subclusters <- function(x, suffix = "_.*") {
  x %>>%
    select(-"x", -"y", -"cluster", -"membership") %>>%
    mutate(n = row_number()) %>>%
    gather("cluster", "membership", -"n") %>>%
    mutate(cluster = str_replace(.data$cluster, !! suffix, "")) %>>%
    group_by(.data$cluster, .data$n) %>>%
    summarize(membership = sum(.data$membership)) %>>%
    ungroup %>>%
    spread("cluster", "membership") %>>%
    select(-"n") %>>%
    mutate(membership = reduce_add(.)) %>>%
    bind_cols(
      mutate(
        x[c("x", "y", "cluster")], cluster = str_replace(cluster, !! suffix, "")
      )
    ) %>>%
    prioritize(c("x", "y", "cluster", "membership")) %>>%
    structure(
      step = attributes(x)$step,
      class = c("qm_cluster", class(.))
    )
}
