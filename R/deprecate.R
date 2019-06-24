deprecate_fine_phase <- function(fine_phase = NULL) {
  if (!is.null(fine_phase)) warning(
    "From qntmap > 0.4.0, use `phase` instead of `fine_phase`.",
    'For example, `fine_phase = c("Qtz", "Pl")` is equivalent to',
    "`phase = c(-Qtz, -Pl)."
  )
}
