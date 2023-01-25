# File settings

fileSettings <- reactiveValues(
  canUse = FALSE,
  canSave = FALSE,
  filePath = "settings.rds",
  fieldNames = c(
    "x", "y",
    "catvarin", "catvarquantin", "catvar2in", "contvarin", "catvar3in",
    "colorin", "facetcolin", "pointshapein", "pointsizein",
    "pointsizes", "pointstransparency")
)

# Load settings

loadFileSettings <- function() {
  if (!file.exists(fileSettings$filePath)) {
    return()
  }

  # print("loadFileSettings()")
  
  oldCanSave <- fileSettings$canSave
  fileSettings$canSave <- FALSE
  
  tryCatch({
      savedInputs <- readRDS(fileSettings$filePath)
      inputIds <- names(savedInputs) 
      inputValues <- unlist(savedInputs)
      # print(paste0("Loading field IDs: ", inputIds))
      
      for (i in 1:length(savedInputs)) {
        session$sendInputMessage(inputIds[i], list(value=inputValues[[i]]))
      }
    },
    finally = function() {
      fileSettings$canSave <- oldCanSave      
    }
  )
}

observeEvent({
  req(fileSettings$canUse, !is.null(values$maindata))
}, {
  # print("observeEvent()")
  loadFileSettings()
  fileSettings$canSave <- TRUE
}, once=TRUE)

# Save settings

saveFileSettings <- function() {
  if (!fileSettings$canSave) {
    return();
  }
  
  # print("saveFileSettings()")

  inputValues <- reactiveValuesToList(input)
  inputNamesToExport <- fileSettings$fieldNames
  inputValues <- inputValues[inputNamesToExport]
  inputValues <- inputValues[order(match(inputValues, inputNamesToExport))]
  inputValues <- inputValues[sapply(inputValues, function(x) !is.null(x))]
  # print(inputValues)
  saveRDS(inputValues, file = fileSettings$filePath)
}

# session$onFlush(function() {
#   isolate(saveFileSettings());
# }, once=FALSE)

saveFileSettingsEventTrigger <- reactive({
  list(fileSettings$canSave, reactiveValuesToList(input)[fileSettings$fieldNames])
})

observeEvent(saveFileSettingsEventTrigger(), {
  saveFileSettings()
});

# Init

if (exists("ggquickeda_settingsfile")) {
  fileSettings$filePath <- get("ggquickeda_settingsfile")
  fileSettings$canUse <- TRUE
}
