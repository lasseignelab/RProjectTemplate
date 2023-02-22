#' This package will create a function called create_project()
#'
#' It's callback is at: inst/rstudio/templates/project/create_project.dcf
#'
#' @export
#'

create_project <- function(path, ...) {
  # collect ellipsis arguments
  dots <- list(...)

  # Create the project path given the name chosen by the user:
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # Change the working directory to the recently created folder:
  setwd(file.path(getwd(), path))


  if (dots$git) {
    git2r::init(path)
  }

}
