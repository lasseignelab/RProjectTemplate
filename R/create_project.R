#' This package will create a function called create_project()
#'
#' It's callback is at: inst/rstudio/templates/project/create_project.dcf
#'
#' @export
#'

create_project <- function(path, ...) {
  # collect ellipsis arguments
  dots <- list(...)

  # rest of the function below
  demoPath_sys <- function (..., lib.loc = NULL, mustWork = FALSE){
    system.file(..., package = "RProjectTemplate",
                lib.loc = lib.loc, mustWork = mustWork)
  }

  from <- demoPath_sys("LasseigneRprojTemplate")

  fs::dir_copy(
    path = from,
    new_path = path,
    overwrite = TRUE
  )

  # Function to loop through all the files we need to make replacements in
  replace_package_name <- function(copied_files,
                                   package_name,
                                   path){
    # Going through copied files to replace package name
    for (f in copied_files) {
      copied_file <- file.path(path, f)

      try({
        replace_word(
          file = copied_file,
          pattern = "LasseigneRprojTemplate",
          replace = package_name
        )
      },silent = TRUE)
    }
  }

  # utility funciton to make a replacement into a single file
  replace_word <- function (file, pattern, replace){
    suppressWarnings(tx <- readLines(file))
    tx2 <- gsub(pattern = pattern, replacement = replace, x = tx)
    writeLines(tx2, con = file)
  }

  # Listing copied files ***from source directory***
  copied_files <- list.files(
    path = from,
    full.names = FALSE,
    all.files = TRUE,
    recursive = TRUE
  )

  # main code to replace all references to templatedemo
  replace_package_name(
    copied_files,
    basename(path),
    path
  )

  setwd(path)

  if (dots$git) {
    git2r::init(path = ".", branch = "main")
    git2r::add(path = ".")
    git2r::commit(repo = ".", message = "Initial Project Commit")
  }

  if (dots$createGitignore){
    git_ignores <-
      c("# History files",
        ".Rhistory",
        ".Rapp.history",
        "\n",
        "# Session Data files",
        ".RData",
        "\n",
        "# User-specific files",
        ".Ruserdata",
        "data/",
        "\n",
        " # Example code in package build process",
        "*-Ex.R",
        "\n",
        " # Output files from R CMD build",
        "/*.tar.gz",
        "\n",
        "# Output files from R CMD check",
        "/*.Rcheck/",
        "\n",
        "# RStudio files",
     ".Rproj.user/",
     "\n" ,
  "# produced vignettes",
  " vignettes/*.html",
  " vignettes/*.pdf",
  "\n",
  "# OAuth2 token, see https://github.com/hadley/httr/releases/tag/v0.3",
   ".httr-oauth",
   "\n",
   "# knitr and R markdown default cache directories",
   "*_cache/",
     "/cache/",
     "\n" ,
    " # Temporary files created by R markdown",
     "*.utf8.md",
   "*.knit.md",
   "\n",
   "# R Environment Variables",
   ".Renviron")
    writeLines(paste(git_ignores, sep = '\n'), '.gitignore')
  }
}
