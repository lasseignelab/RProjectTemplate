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
    system.file(..., package = "RProjectTemplate", lib.loc = lib.loc, mustWork = mustWork)
  }

  from <- demoPath_sys("LasseigneRprojTemplate")

  fs::dir_copy(
    path = from,
    new_path = path,
    overwrite = TRUE
  )

  # utility funciton to loop through all the files we need to make replacements in
  replace_package_name <- function(copied_files,
                                   package_name,
                                   path){
    # Going through copied files to replace package name
    for (f in copied_files) {
      copied_file <- file.path(path, f)

      try({
        replace_word(
          file = copied_file,
          pattern = "templatedemo",
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

  # main code to replace all references to templatedemo
  replace_package_name(
    copied_files,
    basename(path),
    path
  )

  if (dots$git) {
    git2r::init(path)
  }
}
