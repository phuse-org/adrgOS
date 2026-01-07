#' List R Packages
#'
#' Retrieves information about R packages from either the current R session
#' or from an renv lockfile.
#'
#' @param use_renv Logical. If TRUE, reads packages from renv lockfile.
#'   If FALSE (default), reads from current R session.
#' @param lockfile_loc Character. Path to the renv lockfile. Required when
#'   use_renv = TRUE.
#'
#' @return A data.frame with columns Package, Version, and Title.
#'   When use_renv = FALSE, returns all packages (loaded + base).
#'   When use_renv = TRUE, returns packages from lockfile.
#'
#' @details
#' When use_renv = FALSE:
#' - Retrieves loaded packages from sessionInfo()$loadedOnly
#' - Retrieves base packages from sessionInfo()$basePkgs
#' - Combines both into a single data.frame
#' - Missing package information is replaced with default values
#'
#' When use_renv = TRUE:
#' - Reads package information from the specified renv lockfile
#' - Validates that the lockfile exists and throws error if not found
#' - Returns empty data.frame if no packages found in lockfile
#' - Missing package information is replaced with default values
#'
#' @examples
#' \dontrun{
#' # List packages from current R session
#' packages <- list_r_packages()
#' head(packages)
#'
#' # List packages from current R session (explicit)
#' session_packages <- list_r_packages(use_renv = FALSE)
#' nrow(session_packages)
#'
#' # List packages from renv lockfile
#' lockfile_packages <- list_r_packages(use_renv = TRUE, lockfile_loc = "renv.lock")
#' print(lockfile_packages)
#' }
#'
#' @seealso \code{\link{sessionInfo}}, \code{\link{packageDescription}}
#'
#' @export
list_r_packages <- function(use_renv = FALSE, lockfile_loc = NULL) {
  if (use_renv) {
    if (is.null(lockfile_loc)) {
      stop("lockfile_loc must be provided when use_renv = TRUE")
    }

    if (!file.exists(lockfile_loc)) {
      stop("renv lockfile not found at: ", lockfile_loc)
    }

    lockfile <- renv::lockfile_read(lockfile_loc)
    packages <- lockfile$Packages

    return(
      get_package_info(renv_packages = packages)
    )
  } else {
    session_info <- utils::sessionInfo()
    loaded_packages <- names(session_info$loadedOnly)
    base_packages <- session_info$basePkgs

    loaded_info <- get_package_info(pkg_names = loaded_packages)
    base_info <- get_package_info(pkg_names = base_packages)

    return(
      rbind(loaded_info, base_info)
    )
  }
}

#' Null Coalescing Operator
#'
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Get Package Information
#'
#' Helper function to extract package information from package names or renv packages.
#'
#' @param pkg_names Character vector of package names (for session packages)
#' @param renv_packages List of renv package objects (for renv packages)
#'
#' @return A data.frame with columns Package, Version, and Title.
#'   Missing information is replaced with default values.
#'
#' @keywords internal
get_package_info <- function(pkg_names = NULL, renv_packages = NULL) {
  if (!is.null(renv_packages)) {
    # Handle renv packages
    if (length(renv_packages) == 0) {
      return(
        data.frame(
          Package = character(),
          Version = character(),
          Title = character(),
          stringsAsFactors = FALSE
        )
      )
    }

    return(
      data.frame(
        Package = names(renv_packages),
        Version = sapply(renv_packages, function(x) x$Version %||% "Unknown"),
        Title = sapply(renv_packages, function(x) {
          x$Title %||% "No title available"
        }),
        stringsAsFactors = FALSE
      )
    )
  }

  # Handle session packages
  if (is.null(pkg_names) || length(pkg_names) == 0) {
    return(
      data.frame(
        Package = character(),
        Version = character(),
        Title = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  pkg_data <- lapply(pkg_names, function(pkg) {
    pkg_desc <- utils::packageDescription(pkg)
    data.frame(
      Package = pkg_desc$Package %||% pkg,
      Version = pkg_desc$Version %||% "Unknown",
      Title = pkg_desc$Title %||% "No title available",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, pkg_data)
}
