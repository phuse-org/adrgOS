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
