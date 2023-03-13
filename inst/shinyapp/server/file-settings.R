# Bookmarking persistent state

latestBookmarkURL <- reactiveVal()

onBookmarked(
  fun = function(url) {
    latestBookmarkURL(parseQueryString(url)) #Update reactiveVal for each session$doBookmarked
  }
)

onRestore(function(state) {
  values$maindata <- get("ggquickeda_initdata")
  choice_items(.get_choice_items(values$maindata, state$input$x, state$input$y, state$input$pastevarin))
  choice_items_char(.get_choice_items_char(values$maindata))
  choice_items_num(.get_choice_items_num(values$maindata))
  choice_facet_scales(.get_choice_facet_scales(state$input$x, state$input$y))
  choice_items_dstatscolextrain(.get_choice_items(values$maindata, pastevarin =  state$input$pastevarin))
})

onRestored(function(state) {
  showNotification(paste("Restored session:", basename(state$dir)),
                   duration = 10,
                   type = "message")
  # update misc inputs that cannot be automatically updated in onRestore
  savedInputs <- list(
                      "yaxisformat" = state$input$yaxisformat,
                      "xaxisformat" = state$input$xaxisformat,
                      "Smooth" = state$input$Smooth
                      )
  inputIds <- names(savedInputs)
  for (i in seq_along(savedInputs)) {
    session$sendInputMessage(inputIds[i], list(value=savedInputs[[i]]))
    message(inputIds[i], "=", savedInputs[[i]])
  }

})

# Create additional bookmark triggers that we want to persist, but don't affect plotObject() change
bookMarkTriggers <- reactive({
  triggers <- list(
    input$Penalty,
    input$Constraints,
    input$scalesizearea,
    input$scalesizearearange1,
    input$scalesizearearange2,
    input$height,
    input$tablecaption,
    input$tablefootnote,
    input$table_incl_overall,
    input$table_style,
    input$dstats_cont_list,
    input$table_suppress_missing,
    input$dstats_sigfig,
    input$round_median_min_max,
    input$table_na_is_category,
    input$dstatscolextrain,
    input$flipthelevelsin
  )
  yvars <- input$y
  dynamic_relabel_triggers <- lapply(yvars, function(yvar) {
    input[[paste0("quick_relabel_", yvar)]]
  })
  return(c(triggers, dynamic_relabel_triggers))
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
