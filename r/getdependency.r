

dir_to_programs <- file.path(getwd(), "r")

# Scrape scripts ----------------------------------------------------------

scrapemea <- list.files(dir_to_programs, pattern = "\\.r$", full.names = TRUE)
scrapemeb <- list.files(dir_to_programs, pattern = "^ad", full.names = TRUE)

scrapeme <- intersect(scrapemea, scrapemeb)

scrape_inputfiles <- function(script=scrapeme){

  # do some checks on input


  # read file to extract lines where input is read into the script, ignoring comments
  tmp <- data.frame(lines = trimws(suppressWarnings(readLines(script)), which=("both")), stringsAsFactors = F)
  tmp <- data.frame(lines = tmp$lines[which(!grepl("#", tmp$lines) & !grepl("write", tmp$lines))], stringsAsFactors = F)
  lines <- tolower(tmp$lines)

  # extract file types to detect
  filetypes <- c("rds", "xpt", "sas7bdat", "json")


  # Extract input files for each type and simplify the regex operation
  inputfiles  <- list()

  for (type in filetypes) {
    pattern <- paste0("\\b\\S+\\.", type, "\\b")
    inputfiles[[type]] <- unlist(regmatches(lines, gregexpr(pattern, lines)))
  }

  # Include excel files specifically for .xls and .xlsx
  inputfiles$excel <- unlist(regmatches(lines, gregexpr("\\b\\S+\\.xls[x]?\\b", lines)))

  return(inputfiles)
}

infiles <- sapply(scrapeme, FUN = scrape_inputfiles, simplify = F, USE.NAMES = F)
names(infiles) <- sapply(scrapeme, basename)
print(infiles)
