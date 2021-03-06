#' @title Information attached package
#'
#' @param package_name name of the package
#'
#' @description Helpful to find out if the latest version (build) is in search-path or not and prints out the version
#' @return 
#' @export
#'
#' @examples
#' library(stats)
#' info_attached_package(package_name = "survival")
info_attached_package <- function(package_name) {
  if (paste0("package:",package_name) %in% search()) {
    cat("Das Paket", package_name, "ist im search()-path.")
    version_attached_package <- getNamespaceVersion(package_name)
    cat("\nVersionsnummer: ",version_attached_package)
  }
}


#' @title Store a data.frame in tempdir and open it 
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
  # Find tempdir-folder
  tempdir_path <- tempdir()
  # Store object in tempdir-folder
  if (Sys.info()[['sysname']] == "Darwin" | Sys.info()[['sysname']] == "Linux") {
    # Filepath
    file_path_and_name <- paste0(tempdir_path, "/data.frame", ".csv")
    # write
    write.csv2(x = object_to_store, file = file_path_and_name)
    # open file
    system(paste("open", file_path_and_name)) 
    }
  else if ("Windows" %in% Sys.info()[['sysname']]) {
    # Store object in tempdir-folder
    write.csv2(x = object_to_store, file = paste0(tempdir_path, "\\data.frame", ".csv"))
    # Open the file
    shell.exec(file = paste0(tempdir_path, "\\data.frame", ".csv"))
  }
  return(warning("Please close the current file before opening another one!"))
  }
  

#' @title Get OS
#'
#' @description Prints out the os 
#'
#' @return operating system
#' @export
#'
#' @examples
#' get_os()
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "MAC (osx)"
  } else {## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}



#' Install required packages if not installed already
#'
#' @param required_packages 
#'
#' @return nothing
#' @export
#'
#' @examples load_and_install_packages(required_packages = c("Rblpapi", "openxlsx", "ggplot2", "data.table", "mschart"))
load_and_install_packages <- function(required_packages) {
  packages_to_install <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  if (length(packages_to_install) > 0) {
    install.packages(packages_to_install)}
  
  # Load required packages
  for (packages in packages_to_install) {
    require(packages, character.only = T)
  }
  
  # Clean-up

}




#' @title New method to apply is.nan on a data.frame
#' 
#' @description is.nan as well as is.infinte will not work on a data.frame.
#'  
#' @param x data.frame. 
#'
#' @return data.frame.  
#' @export
#'
#' @examples
#' # Generate data.frame
#' df1 <- data.frame(ID = 1:5, Var1 = rnorm(5), Var2 = rnorm(5, 5, 1))
#' # Add NaN and +/-Inf-Werte
#' df1[2,2] <- NaN
#' df1[4,3] <- Inf
#' df1[3,3] <- -Inf
#' # Print to console
#' df1
#' # Use the new method
#' is.nan(df1)
#' # Replace NaN by NA
#' df1[is.nan(df1)] <- NA
#' # Print
#' df1
is.nan.data.frame <- function(x) { 
  do.call(cbind, lapply(x, is.nan))}



#' @title New method to apply is.infinite on a data.frame
#'
#' @description is.nan as well as is.infinte will not work on a list
#' or data.frame.
#' 
#' @param x data.frame. 
#'
#' @return data.frame. 
#' @export
#'
#' @examples
#' # Generate data.frame
#' df1 <- data.frame(ID = 1:5, Var1 = rnorm(5), Var2 = rnorm(5, 5, 1))
#' # Add NaN and +/-Inf-Werte
#' df1[2,2] <- NaN
#' df1[4,3] <- Inf
#' df1[3,3] <- -Inf
#' # Print to console
#' df1
#' # Use the new method
#' is.infinite(df1)
#' # Replace inf and -inf by NA
#' df1[is.infinite(df1)] <- NA
#' # Print
#' df1
is.infinite.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.infinite))}




#' @title Function to get paket version and corresponding R-base version
#' 
#' @description Based on a vector with package names returns a data.frame
#' with package name, package version and corresponding R version
#'
#' @param Vektor_Paketliste character. Vector with package names
#'
#' @return data.frame. Three columns: Package name, package version and built
#' @export
#'
#' @examples
#' PackageInformation(c("base", "stats", "utils"))
PackageInformation <- function(Vektor_Packages = c("base")) {
  # Arguments: Vector_Packages
  #  default: c("base")
  
  ## Check Input 
  if (!is.character(Vektor_Packages)) {
    stop("Input ist kein character-Vektor") 
  }
  
  ## Get the currently installed packages
  #   Result is a matrix, will be transformed into a data.frame:
  InstalliertePakete <- data.frame(installed.packages())
  
  ## Extract relevant packages
  InstalliertePaketePaketliste <- InstalliertePakete[InstalliertePakete$Package %in% Vektor_Packages,]
  
  ## Extract Package, Version and Built
  InstalliertePakete_PaketnameVersionBuilt <- InstalliertePaketePaketliste[, c("Package", "Version", "Built")]
  
  # return
  return(InstalliertePakete_PaketnameVersionBuilt)
}


