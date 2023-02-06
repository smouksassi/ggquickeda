# Bookmarking persistent state

latestBookmarkURL <- reactiveVal()

onBookmarked(
  fun = function(url) {
    latestBookmarkURL(parseQueryString(url)) #Update reactiveVal for each session$doBookmarked
  }
)

onRestore(function(state) {
  values$maindata <- get("ggquickeda_initdata")
  items <- names(values$maindata)
  names(items) <- items
  items <- c("None",items)
  if ( !is.null(state$input$y) ){
    items <- c(items, "yvars","yvalues") 
  }
  if ( !is.null(state$input$x) ){
    items <- c(items, "xvars","xvalues") 
  }
  if (!is.null(state$input$pastevarin) && length(state$input$pastevarin) > 1 ){
    nameofcombinedvariables<- paste(as.character(state$input$pastevarin),collapse="_",sep="") 
    items <- c(items,nameofcombinedvariables)
  }
  message("Settings choices_items() to: ", items)
  choice_items(items)
  # TO DO
  # Create additional reactiveVal for other choices where we are evaluating input$ inside
})

onRestored(function(state) {
  # savedInputs <- list(
  #                     "catvarin" = state$input$catvarin,
  #                     "pastevarin" =  state$input$pastevarin,
  #                     "colorin" = state$input$colorin,
  #                     "facetcolin" =  state$input$facetcolin,
  #                     "facetcolextrain" = state$input$facetcolextrain,
  #                     "pointshapein" = state$input$pointshapein)
  # inputIds <- names(savedInputs)
  # for (i in 1:length(savedInputs)) {
  #   session$sendInputMessage(inputIds[i], list(value=savedInputs[[i]]))
  # }
  showNotification(paste("Restored session:", basename(state$dir)),
                   duration = 10,
                   type = "message")
})

# TO DO: Make below smarter with reactive listener for select input names
observeEvent(bookMarkTriggers(), {
  if (exists("phx_bookmark_dir")) {
  session$doBookmark()
  message(paste0("Bookmark copied from:", file.path(".", "shiny_bookmarks", req(latestBookmarkURL()), "input.rds")))
  file.copy(from = file.path(".", "shiny_bookmarks", req(latestBookmarkURL()), "input.rds"),
            to = file.path(".", "shiny_bookmarks", basename(phx_bookmark_dir) , "input.rds"),
            overwrite = TRUE)
  message(paste0("Bookmark copied to:",file.path(".", "shiny_bookmarks", basename(phx_bookmark_dir) , "input.rds")))
  }
  
}, ignoreInit = TRUE)

# Startup observer gets autodestroyed because no reactive domain, ui.r first loads, then we destory
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

bookMarkTriggers <- reactive({
  list(
    input$catvarin,
    input$catvar2in,
    input$catvar3in,
    input$catvarquantin,
    input$colorin,
    input$contvarin,
    input$facetcolin,
    input$facetcolextrain,
    input$facetrowin,
    input$pointshapein,
    input$pointsizein,
    input$pointsizes,
    input$pointstransparency,
    input$x,
    input$y
  )
})

