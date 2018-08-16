# wrap functions from other packages
# change default values for parameters

data.frame <- base::data.frame
formals(data.frame)$stringsAsFactors <- FALSE

as.data.frame <- base::as.data.frame
formals(as.data.frame) <- append(formals(as.data.frame), list(stringsAsFacotrs=FALSE), 3)

#' @importFrom data.table fread
fread <- data.table::fread
formals(fread)$data.table <- FALSE
