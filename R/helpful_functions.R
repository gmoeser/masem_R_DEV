#' Information attached package
#'
#' @param package_name name of the package
#'
#' @return 
#' @export
#'
#' @examples
#' library(survival)
#' info_attached_package(package_name = "survival")
info_attached_package <- function(package_name) {
  if (paste0("package:",package_name) %in% search()) {
    cat("Das Paket", package_name, "ist im search()-path.")
    version_attached_package <- getNamespaceVersion(package_name)
    cat("\nVersionsnummer: ",version_attached_package)
  }
}


#' Store a data.frame in tempdir and open it 
#'
#' @description Stores a data.frame in tempdir and opens the file
#' Please note: Currently only Windows supported 
#'
#' @param object_to_store data.frame to store and open
#'
#' @return data.frame
#' @export
#'
#' @examples
#' open_file_in_tempdir(object_to_store = iris)
open_file_in_tempdir <- function(object_to_store) {
  ## Cureently works only on Windows OS!
  # detect OS
  if ("Darwin" %in% Sys.info()[['sysname']]) {
    stop("Currently only WIN OS supported")
  }
  # Find tempdir-folder
  tempdir_path <- tempdir()
  # Store object in tempdir-folder
  write.csv2(x = object_to_store, file = paste0(tempdir_path, "\\data.frame", ".csv"))
  # Open the file
  shell.exec(file = paste0(tempdir_path, "\\data.frame", ".csv"))
}



#' Get OS
#'
#' @return operating system
#' @export
#'
#' @examples
#' get_os()
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "MAC (osx)"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

