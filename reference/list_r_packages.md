# List R Packages

Lists R packages either from an renv lockfile or from the current
session. When using renv mode, it reads package information from a
lockfile. When not using renv, it lists both loaded and base packages
from the current session.

## Usage

``` r
list_r_packages(use_renv = FALSE, lockfile_loc = NULL)
```

## Arguments

- use_renv:

  Logical. If `TRUE`, reads packages from an renv lockfile. If `FALSE`
  (default), lists packages from the current R session.

- lockfile_loc:

  Character string. Path to the renv lockfile location. Required when
  `use_renv = TRUE`. Default is `NULL`.

## Value

Invisibly returns `NULL`. The function prints package information as
side effects:

- If `use_renv = TRUE`: Prints a data frame with Package, Version, and
  Title columns from the lockfile.

- If `use_renv = FALSE`: Prints three data frames:

  1.  Loaded packages (from `sessionInfo()$loadedOnly`)

  2.  Base packages (from `sessionInfo()$basePkgs`)

  3.  All packages combined

## Details

When `use_renv = TRUE`, the function attempts to read the renv lockfile
from the specified path. If the file doesn't exist, a message is
printed.

When `use_renv = FALSE`, the function retrieves package information from
the current R session, including both loaded packages (those loaded but
not attached) and base packages.

## See also

[`sessionInfo`](https://rdrr.io/r/utils/sessionInfo.html),
[`packageDescription`](https://rdrr.io/r/utils/packageDescription.html),
[`lockfile_read`](https://rstudio.github.io/renv/reference/lockfiles.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# List packages from current session
list_r_packages()

# List packages from renv lockfile
list_r_packages(use_renv = TRUE, lockfile_loc = "renv.lock")
} # }
```
