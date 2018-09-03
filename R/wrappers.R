# wrap functions from other packages
# change default values for parameters

#' A wrapper of `data.frame` which avoid coercion of `character` to `factor`.
#' @noRd
data.frame <- getExportedValue('base', 'data.frame')
formals(data.frame)$stringsAsFactors <- FALSE

#' A wrapper of `as.data.frame` which avoid coercion of `character` to `factor`.
#' @noRd
as.data.frame <- getExportedValue('base', 'as.data.frame')
formals(as.data.frame) <- append(formals(as.data.frame), list(stringsAsFacotrs=FALSE), 3)

#' A wrapper of `data.table::fread` which returns `data.frame` instead of `data.table`.
#' @importFrom data.table fread
#' @noRd
fread <- getExportedValue('data.table', 'fread')
formals(fread)$data.table <- FALSE
