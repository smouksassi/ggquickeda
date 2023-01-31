# File settings

# fileSettings <- reactiveValues(
#   canUse = FALSE,
#   canSave = FALSE,
#   bookmarkDir = "",
#   fieldNames = c(
#     "x", "y",
#     "catvarin", "catvarquantin", "catvar2in", "contvarin", "catvar3in",
#     "colorin", "facetcolin", "pointshapein", "pointsizein",
#     "pointsizes", "pointstransparency")
# )
# Init

latestBookmarkURL <- reactiveVal()

onBookmarked(
  fun = function(url) {
    latestBookmarkURL(parseQueryString(url)) #Update reactiveVal for each session$doBookmarked
  }
)

onRestore(function(state) {
  values$maindata <- get("ggquickeda_initdata")
})

onRestored(function(state) {
  mockFileUpload("Initial Data")
  colorin <- state$input$colorin
  print(paste0("updating input$colorin to ", colorin))
  updateSelectInput(session, "colorin", selected = colorin)
  #session$sendInputMessage(inputIds[i], list(value=inputValues[[i]]))

  showNotification(paste("Restored session:", basename(state$dir)),
                   duration = 10,
                   type = "message")
})

# TO DO: Make below smarter with reactive listener for select input names
observeEvent(c(input$x, input$y, input$colorin), {
  session$doBookmark()
  message(paste0("Bookmark copied from:", file.path(".", "shiny_bookmarks", req(latestBookmarkURL()), "input.rds")))
  file.copy(from = file.path(".", "shiny_bookmarks", req(latestBookmarkURL()), "input.rds"),
            to = file.path(".", "shiny_bookmarks", basename(phx_bookmark_dir) , "input.rds"),
            overwrite = TRUE)
  message(paste0("Bookmark copied to:",file.path(".", "shiny_bookmarks", basename(phx_bookmark_dir) , "input.rds")))
  
}, ignoreInit = TRUE)

# Startup observer gets autodestroyed because no reactive domain, ui.r first loads, then we destory
observe({
  if(useBookMark && !file.exists("stop.txt")) {
    restoreURL <-
      paste0(
        session$clientData$url_protocol,
        "//",
        session$clientData$url_hostname,
        ":",
        session$clientData$url_port,
        session$clientData$url_pathname,
        "?_state_id_=",
        basename(phx_bookmark_dir)
      )
      writeLines("", con = "stop.txt")
      cat(paste0("Restoring to: ", restoreURL, sep = "\n"))
      message("...restoring bookmarked startup")
      runjs(sprintf("window.location = '%s';", restoreURL))
  }
}, priority = 99)

