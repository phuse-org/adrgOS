# List ADaM Programs, Packages, and Functions

This function reads R ADaM programs to list all programs used and the
packages/functions within each. It offers two output formats controlled
by the `adrg` flag: a nested list (for data access) or a flattened data
frame (for documentation). It automatically derives the expected ADaM
dataset name (`.xpt`) from the program file name.

## Usage

``` r
list_adam_pgms(target_dir = ".", all_one_file = "YN", adrg = "Y")
```

## Arguments

- target_dir:

  This is the file path to the program(s) that are to be processed.  
   If `all_one_file` is 'YN', this should be a **directory path**. If  
   `all_one_file` is 'NY', this should be the **full path to a single R
  file**.    Defaults to the current working directory (`.`).

- all_one_file:

  This flag controls processing scope:    

  - `'YN'` (Default): Process **All** ADaM files in `target_dir`
    (matching `^ad.*\.[rR]$`).      

  - `'NY'`: Process **Only** the single file specified by `target_dir`
    (matching `^ad.*\.[rR]$`)..    

- adrg:

  Flag to select the output format:    

  - `'Y'` (Default): Returns a **Data Frame** formatted for an Analysis
    Data Reviewers Guide (ADRG), with all package/function usage
    consolidated into a single string per program.      

  - `'N'`: Returns a **Nested List** structure, where each program's
    package/function details are stored in a nested data frame for easy
    programmatic access.    

## Value

The output format depends on the `adrg` flag:    

- **If `adrg = 'Y'` (Data Frame):** Columns are `program`, `dataset`,
  and `functions` (a single, newline-separated string).      

- **If `adrg = 'N'` (Nested List):** A list where each element has:    
     

  - `program`: The R program file name.          

  - `dataset`: The derived ADaM dataset name.          

  - `packages_used`: A nested data frame with columns `package` and
    `func_names`.        

     

## Examples

``` r
if (FALSE) { # \dontrun{
# NOTE: These examples use non-portable file paths and require the files to exist.

# --- Scenario 1: Process ALL files, Output for ADRG (adrg='Y', default) ---
# Returns a single data frame with collapsed functions per program.
adrg_all_adams_df <- list_adam_pgms(
  target_dir = "./dev/pilot3/m5/datasets/rconsortiumpilot3/analysis/adam/programs/",
  all_one_file = 'YN',
  adrg = 'Y'
)
print(adrg_all_adams_df)

# --- Scenario 2: Process ALL files, Output as Nested List (adrg='N') ---
# Returns a list where each program's details are nested. Ideal for programmatic access.
all_adams_list <- list_adam_pgms(
  target_dir = "./dev/pilot3/m5/datasets/rconsortiumpilot3/analysis/adam/programs/",
  all_one_file = 'YN',
  adrg = 'N'
)
# Accessing the nested packages_used data frame for the first program:
all_adams_list[[1]]$packages_used

# --- Scenario 3: Process ONE file, Output for ADRG (adrg='Y', default) ---
# Returns a data frame with one row, formatted for the ADRG.
adrg_one_adam_df <- list_adam_pgms(
   target_dir = "./dev/pilot3/m5/datasets/rconsortiumpilot3/analysis/adam/programs/adlbc.r",
   all_one_file = 'NY',
   adrg = 'Y'
)
print(adrg_one_adam_df)

# --- Scenario 4: Process ONE file, Output as Nested List (adrg='N') ---
# Returns a list with one element (the specified program's details).
one_adam_list <- list_adam_pgms(
  target_dir = "./dev/pilot3/m5/datasets/rconsortiumpilot3/analysis/adam/programs/adlbc.r",
  all_one_file = 'NY',
  adrg = 'N'
)
print(one_adam_list)
} # }
```
