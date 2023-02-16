# Bookmarking persistent state

latestBookmarkURL <- reactiveVal()

onBookmarked(
  fun = function(url) {
    latestBookmarkURL(parseQueryString(url)) #Update reactiveVal for each session$doBookmarked
  }
)

onRestore(function(state) {
  values$maindata <- get("ggquickeda_initdata")
  items <- .get_choice_items(values$maindata, state$input$x, state$input$y, state$input$pastevarin)
  choice_items(items)
  items_char <- .get_choice_items_char(values$maindata)
  choice_items_char(items_char)
})

onRestored(function(state) {
  showNotification(paste("Restored session:", basename(state$dir)),
                   duration = 10,
                   type = "message")
  # savedInputs <- list(
  #                     "pastevarin" = state$input$pastevarin
  #                     )
  # inputIds <- names(savedInputs)
  # for (i in seq_along(savedInputs)) {
  #   session$sendInputMessage(inputIds[i], list(value=savedInputs[[i]]))
  #   message(inputIds[i], "=", savedInputs[[i]])
  # }
})

# Create additional bookmark triggers that we want to persist, but don't affect plotObject() change
bookMarkTriggers <- reactive({
  list(
    input$Penalty,
    input$Constraints
  )
})
# Bookmark on plotObject() and misc bookmark triggers change (e.g., inputs that do not affect plot display)
observeEvent(c(plotObject(), bookMarkTriggers()), {
  if (exists("phx_bookmark_dir")) {
  session$doBookmark()
  message(paste0("Bookmark copied from:", file.path(".", "shiny_bookmarks", req(latestBookmarkURL()), "input.rds")))
  file.copy(from = file.path(".", "shiny_bookmarks", req(latestBookmarkURL()), "input.rds"),
            to = file.path(".", "shiny_bookmarks", basename(phx_bookmark_dir) , "input.rds"),
            overwrite = TRUE)
  message(paste0("Bookmark copied to:",file.path(".", "shiny_bookmarks", basename(phx_bookmark_dir) , "input.rds")))
  }
}, ignoreInit = TRUE)

# Startup observer gets auto-destroyed because no reactive domain
observe({
  if (exists("ggquickeda_phx_app_dir")) {
    if (useBookMark && !file.exists("stop.txt")) {
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
  }
}, priority = 99)
