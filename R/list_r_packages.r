#' List R Packages
#'
#' @description
#' Lists R packages either from an renv lockfile or from the current session.
#' When using renv mode, it reads package information from a lockfile.
#' When not using renv, it lists both loaded and base packages from the current session.
#'
#' @param use_renv Logical. If `TRUE`, reads packages from an renv lockfile.
#'   If `FALSE` (default), lists packages from the current R session.
#' @param lockfile_loc Character string. Path to the renv lockfile location.
#'   Required when `use_renv = TRUE`. Default is `NULL`.
#'
#' @return Invisibly returns `NULL`. The function prints package information
#'   as side effects:
#'   \itemize{
#'     \item If `use_renv = TRUE`: Prints a data frame with Package, Version,
#'       and Title columns from the lockfile.
#'     \item If `use_renv = FALSE`: Prints three data frames:
#'       \enumerate{
#'         \item Loaded packages (from `sessionInfo()$loadedOnly`)
#'         \item Base packages (from `sessionInfo()$basePkgs`)
#'         \item All packages combined
#'       }
#'   }
#'
#' @details
#' When `use_renv = TRUE`, the function attempts to read the renv lockfile
#' from the specified path. If the file doesn't exist, a message is printed.
#'
#' When `use_renv = FALSE`, the function retrieves package information from
#' the current R session, including both loaded packages (those loaded but
#' not attached) and base packages.
#'
#' @examples
#' \dontrun{
#' # List packages from current session
#' list_r_packages()
#'
#' # List packages from renv lockfile
#' list_r_packages(use_renv = TRUE, lockfile_loc = "renv.lock")
#' }
#'
#' @seealso
#' \code{\link[utils]{sessionInfo}}, \code{\link[utils]{packageDescription}},
#' \code{\link[renv]{lockfile_read}}
#'
#' @export
list_r_packages <- function(use_renv = FALSE, lockfile_loc = NULL) {

  if (use_renv == TRUE) {

    # renv code
    lockfile_path <- lockfile_loc

    if (file.exists(lockfile_path)) {
      lockfile <- renv::lockfile_read(lockfile_path)
      packages <- lockfile$Packages

      if (!is.null(packages) && length(packages) > 0) {
        package_list <- data.frame(
          Package = names(packages),
          Version = sapply(packages, function(x) x$Version),
          Title = sapply(packages, function(x) x$Title),
          stringsAsFactors = FALSE
        )
        print(package_list)
      } else {
        print("No packages found in the lockfile.")
      }
    } else {
      print("renv lockfile not found. Make sure renv has been initialized.")
    }
  }
  else if (use_renv == FALSE) {
    # Non-renv code
    loaded_packages <- names(sessionInfo()$loadedOnly)

    package_info <- data.frame(
      Package = character(),
      Version = character(),
      Title = character(),
      stringsAsFactors = FALSE
    )

    for (pkg in loaded_packages) {
      pkg_desc <- packageDescription(pkg)
      pkg_name <- pkg_desc$Package
      pkg_version <- pkg_desc$Version
      pkg_title <- pkg_desc$Title

      package_info <- rbind(package_info, data.frame(
        Package = pkg_name,
        Version = pkg_version,
        Title = pkg_title,
        stringsAsFactors = FALSE
      ))
    }

    print(package_info)

    base_packages <- sessionInfo()$basePkgs

    base_package_info <- data.frame(
      Package = character(),
      Version = character(),
      Title = character(),
      stringsAsFactors = FALSE
    )

    for (pkg in base_packages) {
      pkg_desc <- packageDescription(pkg)
      pkg_name <- pkg_desc$Package
      pkg_version <- pkg_desc$Version
      pkg_title <- pkg_desc$Title

      base_package_info <- rbind(base_package_info, data.frame(
        Package = pkg_name,
        Version = pkg_version,
        Title = pkg_title,
        stringsAsFactors = FALSE
      ))
    }

    print("Base Packages:")
    print(base_package_info)

    all_packages_info <- rbind(package_info, base_package_info)

    print("All Packages (Base and Loaded):")
    print(all_packages_info)
  }
}
