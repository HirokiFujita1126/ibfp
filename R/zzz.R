.onAttach <- function(libname, pkgname) {
  data_file_path <- system.file("Item.txt", package = pkgname)
  Material <- read.table(data_file_path, header = FALSE, col.names = paste0("a",seq_len(18)), fill = TRUE)
  assign("Material", Material, envir = .GlobalEnv)
}

.onLoad <- function(libname, pkgname) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' needed but not installed.")
  }
  library(dplyr, character.only = TRUE)
}
