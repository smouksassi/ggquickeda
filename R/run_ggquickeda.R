#' Run the ggquickeda application
#' 
#' Run the \code{ggquickeda} application. 
#' 
#' @param data The initial data.frame to load into the application.
#' @param settingsFile The path to the file that contains the application settings.
#' 
#' @examples
#' if (interactive()) {
#'   run_ggquickeda()
#' }
#' @export
run_ggquickeda <- function(data = NULL, settingsFile = NULL) {
  if (!is.null(data) && !is.data.frame(data)) {
    stop("data must be a data.frame", call. = FALSE)
  }
  appDir <- system.file("shinyapp", package = "ggquickeda")
  if (appDir == "") {
    stop("Could not find shiny app directory. Try re-installing `ggquickeda`.",
         call. = FALSE)
  }
  
  if (!is.null(data)) {
    .GlobalEnv$ggquickeda_initdata <- data
    on.exit(rm(ggquickeda_initdata, envir = .GlobalEnv))
  }

  if (!is.null(settingsFile)) {
    .GlobalEnv$ggquickeda_settingsfile <- settingsFile
    on.exit(rm(ggquickeda_settingsfile, envir = .GlobalEnv))
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}

# Make CRAN happy
if (getRversion() >= "2.15.1") utils::globalVariables(c("ggquickeda_initdata", "ggquickeda_settingsfile"))
