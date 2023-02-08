#' Run the ggquickeda application
#' 
#' Run the \code{ggquickeda} application. 
#' 
#' @param data The initial data.frame to load into the application.
#' @param ... Additional arguments
#' 
#' @examples
#' if (interactive()) {
#'   run_ggquickeda()
#' }
#' @export
run_ggquickeda <- function(data = NULL, ...) {
  if (!is.null(data) && !is.data.frame(data)) {
    stop("data must be a data.frame", call. = FALSE)
  }
  
  args <- list(...)
  appDir <- system.file("shinyapp", package = "ggquickeda")
  
  if (appDir == "") {
    stop("Could not find shiny app directory. Try re-installing `ggquickeda`.",
         call. = FALSE)
  } else if (!is.null(args$phx_app_dir)) {
    stopifnot(dir.exists(args$phx_app_dir))
    appDir <- file.path(args$phx_app_dir, "shinyapp")
    
    if (file.exists(file.path(appDir, "stop.txt"))) {
      message("Removing previous stop file")
      file.remove(file.path(appDir, "stop.txt"))
    }
    message("phx_app_dir: ", appDir)
    .GlobalEnv$ggquickeda_phx_app_dir <- appDir
    on.exit(rm(ggquickeda_phx_app_dir, envir = .GlobalEnv))
  }
  
  if (!is.null(data)) {
    .GlobalEnv$ggquickeda_initdata <- data
    on.exit(rm(ggquickeda_initdata, envir = .GlobalEnv))
  }

  if (!is.null(args$phx_bookmark_dir)) {
    .GlobalEnv$phx_bookmark_dir <- args$phx_bookmark_dir
    message("phx_bookmark_dir: ", phx_bookmark_dir)
    on.exit(rm(phx_bookmark_dir, envir = .GlobalEnv))
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}

# Make CRAN happy
if (getRversion() >= "2.15.1") utils::globalVariables(c("ggquickeda_initdata", "ggquickeda_phx_app_dir", "phx_bookmark_dir"))
