function(input, output, session) {
  values <- reactiveValues(
    plots = list(),      # list of plots the user saved
    maindata = NULL,     # the data frame used throughout the app
    updatePlot = FALSE,  # whether to manually update the plot
    prevPlot = NULL,     # the last plot that was successfully plotted
    updateTable = FALSE  # whether to manually update the dstats table
  )
  
  # gradient <- callModule(gradientInput, "gradientcol",
  #                        init_col =c("#832424","white","#3A3A98"),
  #            allow_modify = FALSE, col_expand = TRUE) 
  
  # gradientTableData <- reactive({
  #   df <- gradient$result()
  # }
  # )
  # observeEvent(input$gradientreset, {
  #   gradient$reset()
  # })  
  observeEvent(input$userdefinedcontcolorreset, {
    cols <- c(muted("red"),"white",muted("blue"))
    updateColourInput(session = session,
                      inputId = paste0("colcont1"),
                      value = cols[1]
    )
    updateColourInput(session = session,
                      inputId = paste0("colcont2"),
                      value = cols[2]
    )
    updateColourInput(session = session,
                      inputId = paste0("colcont3"),
                      value = cols[3]
    )
  })
  
  mockFileUpload <- function(name) {
    shinyjs::runjs(paste0('$("#datafile").closest(".input-group").find("input[type=\'text\']").val(\'', name, '\')')) 
    shinyjs::runjs('$("#datafile_progress").removeClass("active").css("visibility", "visible"); $("#datafile_progress .progress-bar").width("100%").text("Upload complete")')
  }
  
  # If this app was launched from a function that explicitly set an initial dataset
  if (exists("ggquickeda_initdata")) {
    values$maindata <- get("ggquickeda_initdata")
    mockFileUpload("Initial Data")
  }
  
  # Kill the application/R session when a single shiny session is closed
  session$onSessionEnded(stopApp)
  
  # Variables to help with maintaining the dynamic number of "change the labels
  # of a variable" boxes
  changeLblsVals <- reactiveValues(
    numCurrent = 0,  # How many boxes are there currently
    numTotal = 0  # Max # of boxes at the same time, to prevent memory leaks
  )
  
  # Variables to reorder levels for in the table
  # changeorderVals <- reactiveValues(
  #   numCurrent = 0,  # How many boxes are there currently
  #   numTotal = 0  # Max # of boxes at the same time, to prevent memory leaks
  # )
  
  # Variables to help with maintaining the dynamic number of "quick relabel" boxes
  quickRelabel <- reactiveValues(
    numCurrent = 0,  # How many boxes are there currently
    numTotal = 0     # Max # of boxes at the same time, to prevent memory leaks
  )
  relabels <- character(0)
  
  # This object will track the order of values even through being renamed by
  # factor_lvl_change_select_* widgets
  factor_lvl_diff_tracker <- reactiveValues()
  
  # Add UI and corresponding outputs+observers for a "change factor levels"
  # section
  add_factor_lvl_change_box <- function() {
    changeLblsVals$numCurrent <- changeLblsVals$numCurrent + 1
    
    df <- factor_merge_data()
    items <- names(df)
    names(items) <- items
    MODEDF <- sapply(df, is.numeric)
    ALLNAMES <- names(df)[!MODEDF]
    ALLNAMES <- ALLNAMES[!ALLNAMES=="custombins"]
    names_used <- lapply(seq_len(changeLblsVals$numCurrent - 1),
                         function(i) {
                           input[[paste0("factor_lvl_change_select_", i)]]
                         }) %>% unlist()
    NAMESTOKEEP2 <- setdiff(ALLNAMES, names_used)
    NAMESTOKEEP2["Please select a variable"] = ""
    shinyjs::disable("factor_lvl_change_add")
    
    insertUI(
      selector = "#factor_lvl_change_placeholder", where = "beforeEnd",
      immediate = TRUE,
      div(class = "factor_lvl_change_box",
          selectizeInput(
            paste0("factor_lvl_change_select_", changeLblsVals$numCurrent),
            sprintf('Select a variable (%s):', changeLblsVals$numCurrent),
            choices = NAMESTOKEEP2, selected = ""
          ),
          textOutput(paste0("factor_lvl_change_labeltext_",
                            changeLblsVals$numCurrent)),
          div(
            class = "blind-dropdown",
            shinyjs::hidden(
              selectizeInput(
                inputId = paste0("factor_lvl_change_labels_", changeLblsVals$numCurrent),
                label = "",
                choices = c(),
                multiple = TRUE
              )
            )
          )
      )
    )
    
    # if we already had this many sections before, no need to wire up any
    # new observers
    if (changeLblsVals$numCurrent <= changeLblsVals$numTotal) {
      return()
    }
    num1 <- changeLblsVals$numCurrent
    changeLblsVals$numTotal <- num1
    
    output[[paste0("factor_lvl_change_labeltext_", num1)]] <- renderText({
      df <- factor_merge_data()
      selected_var <- input[[paste0("factor_lvl_change_select_", num1)]]
      if (is.null(selected_var) || selected_var == "") return(NULL)
      if (!selected_var %in% names(df)) return(NULL)
      labeltextout <- c("Old labels", levels(df[, selected_var]))
      labeltextout   
    })
    
    observeEvent(input[[paste0("factor_lvl_change_select_", num1)]], {
      selected_var <- input[[paste0("factor_lvl_change_select_", num1)]]
      if (selected_var == "") return()
      shinyjs::disable(paste0("factor_lvl_change_select_", num1))
      
      df <- factor_merge_data()
      MODEDF <- sapply(df, is.numeric)
      
      ALLNAMES <- names(df)[!MODEDF]
      ALLNAMES <- ALLNAMES[!ALLNAMES=="custombins"]
      if (changeLblsVals$numCurrent < length(ALLNAMES)) {
        shinyjs::enable("factor_lvl_change_add")
      }
      df <- factor_merge_data()
      shinyjs::show(paste0("factor_lvl_change_labels_", num1))
      
      selected_var_factor <- as.factor( df[, selected_var] )
      nlevels <- nlevels(selected_var_factor)
      levelsvalues <- levels(selected_var_factor)
      
      # Start tracking Recoding/Reordering in this variable
      # This object contains snapshots of the factor levels
      # including their recoded values. The elements represent the
      # newly named recoded level, while its name refers to the value
      # found in the data. Order is also retained for values present.
      # The dictionary keeps track of known recodings so that you can add
      # a level back using its new name (not resticted to only its true level)
      factor_lvl_diff_tracker[[ as.character(num1) ]] <- list(
        var = selected_var,
        last_value = setNames(levelsvalues, levelsvalues),
        second_last_value = setNames(levelsvalues, levelsvalues),
        dictionary_of_edits = setNames(levelsvalues, levelsvalues)
      )
      
      updateSelectizeInput(
        session, paste0("factor_lvl_change_labels_", num1),
        label = paste(selected_var, "requires", nlevels, "new labels,
                      edit the labels via Backspace/Enter keys. Drag and Drop the items to the desired order. Do not use semicolons."),
        choices = levelsvalues,
        selected = levelsvalues,
        options = list(
          create = TRUE, createOnBlur = TRUE,
          delimiter = ";",
          plugins = list('drag_drop', 'restore_on_backspace'),
          maxItems = nlevels
        )
        )
    })
    
    observeEvent(input[[ paste0("factor_lvl_change_labels_", num1) ]], {
      
      value_on_arrival <- input[[ paste0("factor_lvl_change_labels_", num1) ]]
      names(value_on_arrival) <- value_on_arrival
      
      diff_tracker <- factor_lvl_diff_tracker[[ as.character(num1) ]]
      previous_value <- diff_tracker[[ "last_value" ]]
      second_last_value <- diff_tracker[[ "second_last_value" ]]
      
      if ( identical(value_on_arrival, previous_value)) return()
      
      # The condition below handles label-adding events,
      # including addition of previously deleted levels.
      # These show up as a delete followed by an addition with a different name/value.
      # Hence, need to track the last 2 values and compare the newest with
      # the value twice preceding it
      #       EG.       Renaming Susan to Sue looks like this:
      #             1. c('Alfred', 'Betty', 'Susan')  <-- compare this
      #             2. c('Alfred', 'Betty')
      #             3. c('Alfred', 'Betty', 'Sue')   <-- against this
      #
      if (length(previous_value[ !is.na(previous_value)]) < length(value_on_arrival) ) {
        
        lvl_dict <-  diff_tracker[[ "dictionary_of_edits" ]]
        new_value <- setdiff(value_on_arrival, previous_value)
        
        already_in_dict <- new_value %in% lvl_dict
        
        if ( !isTRUE(already_in_dict)) { # If label has never been seen, add it to the dictionary
          
          value_before_edit <- setdiff(second_last_value[ !is.na(second_last_value)], value_on_arrival)
          lvl_in_data <- names(lvl_dict[ match(value_before_edit, lvl_dict)])
          
          new_value_tmp <- new_value
          names(new_value_tmp) <- lvl_in_data
          
          updated_lvl_dict <- c(lvl_dict, new_value_tmp)
          updated_lvl_dict <- updated_lvl_dict[ !duplicated(updated_lvl_dict)]
          updated_lvl_dict <- updated_lvl_dict[ !is.na(names(updated_lvl_dict))]
          
          factor_lvl_diff_tracker[[ as.character(num1) ]][[ "dictionary_of_edits" ]] <-
            updated_lvl_dict
        }
        
      }
      
      # This line should read directly from the most up-to-date value
      # (not the object `difftracker`)
      refreshed_lvl_dict <-  factor_lvl_diff_tracker[[ as.character(num1) ]][[ "dictionary_of_edits" ]]
      
      # If a level was removed, determine which level was removed by looking
      # at the levels before the change, and impute it with NA, while keeping
      # a place for it.
      #       EG.       Removing bat looks like this:
      #             1. c(ant = 'ant',  bat = 'bat', cat = 'cat')
      #             ... becomes...
      #             2. c(ant = 'ant',  bat = NA,    cat = 'cat')
      if( length(value_on_arrival) < length(previous_value) ){
        
        imputed_missing_current_value <- previous_value
        true_values <- names(refreshed_lvl_dict)[ match(value_on_arrival, refreshed_lvl_dict)]
        imputed_missing_current_value[ !names(imputed_missing_current_value) %in% true_values] <- NA_character_
        imputed_missing_current_value[ names(imputed_missing_current_value) %in% true_values] <- value_on_arrival
        value_on_arrival <- imputed_missing_current_value
        
      }
      
      # Next we change the *names* of the levels (after recoding) to match the
      # values taken in the data.
      # This is accomplished using the dictionary_of_edits that has been tracking
      # all recoding/relabelling events.
      #       EG.  Given a vector like this..
      #               c(ant = 'ant',  bat = 'bat', cat = 'MrChestington')
      #             .. and a dictionary like this...
      #               c(ant = 'ant', bat = 'bat', cat = 'cat', ant = 'MrAnt',
      #                 bat = 'batface', cat = 'MrChestington')
      #
      #             ... becomes...
      #             2. c(ant = 'ant',  bat = NA,    cat = 'cat')
      
      
      # names(value_on_arrival)[ !is.na(value_on_arrival)] <- names(refreshed_lvl_dict)[match(value_on_arrival[ !is.na(value_on_arrival)], refreshed_lvl_dict)]
      
      
      names(value_on_arrival)[ !is.na(value_on_arrival)] <-
        names(refreshed_lvl_dict)[ match(value_on_arrival[ !is.na(value_on_arrival)], refreshed_lvl_dict) ]
      
      factor_lvl_diff_tracker[[ as.character(num1) ]][[ "last_value" ]] <- value_on_arrival
      factor_lvl_diff_tracker[[ as.character(num1) ]][[ "second_last_value" ]] <- previous_value
      
    })
  }
  
  remove_last_factor_lvl_change_box <- function() {
    factor_lvl_diff_tracker[[ as.character(changeLblsVals$numCurrent) ]] <- NULL
    selector <- paste0("#factor_lvl_change_placeholder .factor_lvl_change_box:nth-child(", changeLblsVals$numCurrent, ")")
    removeUI(selector, multiple = FALSE, immediate = TRUE)
    changeLblsVals$numCurrent <- changeLblsVals$numCurrent - 1
    shinyjs::enable("factor_lvl_change_add")
  }
  
  # Decide if to enable/disable the remove variable labels button
  observeEvent(changeLblsVals$numCurrent, {
    shinyjs::toggleState("factor_lvl_change_remove", condition = changeLblsVals$numCurrent > 0)
  })
  
  # Load user data
  observeEvent(input$datafile, {
    file <- input$datafile$datapath
    values$maindata <- read.csv(file, na.strings = c("NA","."), stringsAsFactors = input$stringasfactor,
                                sep = input$fileseparator)
  })
  
  # Load sample dataset
  observeEvent(input$sample_data_btn, {
    file <- "data/sample_data.csv"
    values$maindata <- read.csv(file, na.strings = c("NA","."), stringsAsFactors = input$stringasfactor,
                                sep = input$fileseparator)
    mockFileUpload("Sample Data")
  })
  
  # reset the dynamic "change factor levels" boxes
  observeEvent(factor_merge_data(), {
    shinyjs::show("factor_lvl_change_section")
    
    changeLblsVals$numCurrent <- 0
    
    removeUI(selector = ".factor_lvl_change_box",
             multiple = TRUE, immediate = TRUE)
    
    add_factor_lvl_change_box()
  })
  
  # add another "change factor levels" box
  observeEvent(input$factor_lvl_change_add, {
    add_factor_lvl_change_box()
  })
  # remove the last "change factor levels" box
  observeEvent(input$factor_lvl_change_remove, {
    remove_last_factor_lvl_change_box()
  })
  
  observeEvent(input$majorgridlinescolreset, {
    shinyjs::reset("majorgridlinescolx")
    shinyjs::reset("majorgridlinescoly")
    
  })
  observeEvent(input$minorgridlinescolreset, {
    shinyjs::reset("minorgridlinescolx")
    shinyjs::reset("minorgridlinescoly")
    
  })
  
  observeEvent(input$colpointreset, {
    shinyjs::reset("colpoint")
  })
  observeEvent(input$collinereset, {
    shinyjs::reset("colline")
  })
  observeEvent(input$colsmoothreset, {
    shinyjs::reset("colsmooth")
  })
  
  observeEvent(input$stripbackfillresetx, {
    shinyjs::reset("stripbackgroundfillx")
  })
  observeEvent(input$stripbackfillresety, {
    shinyjs::reset("stripbackgroundfilly")
  })
  observeEvent(input$vlinecol1reset, {
    shinyjs::reset("vlinecol1")
  })
  observeEvent(input$vlinecol2reset, {
    shinyjs::reset("vlinecol2")
  })
  observeEvent(input$hlinecol1reset, {
    shinyjs::reset("hlinecol1")
  })
  observeEvent(input$hlinecol2reset, {
    shinyjs::reset("hlinecol2")
  })
  output$ycol <- renderUI({
    df <- values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    selectizeInput("y", "y variable(s):",choices=items,selected = items[1],multiple=TRUE,
                   options = list(
                     plugins = list('remove_button', 'drag_drop')))
  })
  
  output$xcol <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    selectInput("x", "x variable:",items,selected=items[2])
    
  })
  
  output$xcolrug <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items = names(df)
    items = c("None",items, "yvars","yvalues") 
    names(items) = items
    selectizeInput("xrug", "rug variable(s):",choices = items,
                   multiple=TRUE,
                   options = list(plugins = list('remove_button', 'drag_drop')))  })
  
  # If an X is selected but no Y is selected, switch to the Histograms tab
  observe({
    if (!is.null(input$x) && is.null(input$y)) {
      updateTabsetPanel(session, "graphicaltypes", "histograms_density")
    }
  })
  observe({
    if (input$KM!="None") {
      updateRadioButtons(session, "yaxisscale", choices = c(
        "Linear" = "lineary"))
    }
    if (input$KM=="None") {
      updateRadioButtons(session, "yaxisscale", choices = c(
        "Linear" = "lineary",
        "Log10" = "logy"))
    }
 })
  observe({
    if (input$KM=="None") {
      updateCheckboxInput(session, "addrisktable", value = FALSE)
    }
  })

  observe({
    if (input$yaxisscale=="lineary"&& input$KM=="None") {
      updateRadioButtons(session, "yaxisformat", choices = c("default" = "default",
                                                             "Comma separated" = "scientificy",
                                                             "Percent" = "percenty"))
    }
    if (input$yaxisscale=="lineary" && input$KM!="None") {
      updateRadioButtons(session, "yaxisformat", choices = c("default" = "default",
                                                             "Percent" = "percenty"))
    }
    
    if (input$yaxisscale!="lineary") {
      updateRadioButtons(session, "yaxisformat", choices = c("default" = "default",
                                                             "Log 10^x Format" = "logyformat",
                                                             "Pretty Y" ="logyformat2"))
    }
  })
  observe({
    if (input$yaxisformat!="default") {
    updateCheckboxInput(session, "customytickslabel", value = FALSE)
    }
    if (input$xaxisformat!="default") {
      updateCheckboxInput(session, "customxtickslabel", value = FALSE)
    }
  })
  
  observe({
    if (input$xaxisscale=="linearx") {
      updateRadioButtons(session, "xaxisformat", choices = c("default" = "default",
                                                             "Comma separated" = "scientificx",
                                                             "Percent" = "percentx"))
    }
    if (input$xaxisscale!="linearx") {
      updateRadioButtons(session, "xaxisformat", choices = c("default" = "default",
                                                             "Log 10^x Format" = "logxformat",
                                                             "Pretty X" ="logxformat2"))
    }
  })
  
  observe({
    
    if (length(input$y)>1) {
      updateRadioButtons(session, "yaxiszoom", choices = c("None" = "noyzoom"),inline=TRUE)
    }
    if (length(input$y)<2) {
      updateRadioButtons(session, "yaxiszoom", choices = c("None" = "noyzoom",
                                                           "Automatic" = "automaticyzoom",
                                                           "User" = "useryzoom"),inline=TRUE)
    }
  })
  
  # observe({
  #   if (input$Median== 'Median/PI' ) {
  #     updateCheckboxInput(session, "medianpoints", value = FALSE)
  #     shinyjs::disable("medianpoints")
  #   }
  # })
  # observe({
  #   if (input$Median== 'Median/PI' ) {
  #     updateCheckboxInput(session, "medianlines", value = TRUE)
  #     shinyjs::disable("medianlines")
  #   }
  # })
  # observe({
  #   if (input$Median== 'Median' ) {
  #     shinyjs::enable("medianlines")
  #     shinyjs::enable("medianpoints")
  #   }
  # })
  
  
  outputOptions(output, "ycol", suspendWhenHidden=FALSE)
  outputOptions(output, "xcol", suspendWhenHidden=FALSE)
  
  output$catvar <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    selectInput('catvarin',label = 'Recode into Binned Categories:',choices=NAMESTOKEEP2,multiple=TRUE)
  })
  
  # Show/hide the "N of cut breaks" input
  observeEvent(input$catvarin, ignoreNULL = FALSE, {
    shinyjs::toggle("ncuts", condition = !is.null(input$catvarin) && length(input$catvarin) >= 1)
  })
  
  output$catvarquant <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    if (!is.null(input$catvarin)) {
      if (length(input$catvarin ) >=1) {
        NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvarin) ]
      }  
    }
    selectInput('catvarquantin',
                label = 'Recode into Quantile Categories:',
                choices=NAMESTOKEEP2,multiple=TRUE)
  })
  
  # Show/hide the "N of cut quantiles" input
  observeEvent(input$catvarquantin, ignoreNULL = FALSE, {
    shinyjs::toggle("ncutsquant",
                    condition = !is.null(input$catvarquantin) && length(input$catvarquantin) >= 1)
  })
  observeEvent(input$catvarquantin, ignoreNULL = FALSE, {
    shinyjs::toggle("zeroplacebo",
                    condition = !is.null(input$catvarquantin) && length(input$catvarquantin) >= 1)
  })
  observeEvent(input$catvarquantin, ignoreNULL = FALSE, {
    shinyjs::toggle("missingcategory",
                    condition = !is.null(input$catvarquantin) && length(input$catvarquantin) >= 1)
  })
  
  output$catvar2 <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) !is.factor(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    if (!is.null(input$catvarquantin)) {
      if (length(input$catvarquantin ) >=1) {
        NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvarquantin) ]
      }  
    }
    if (!is.null(input$catvarin)) {
      if (length(input$catvarin ) >=1) {
        NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvarin) ]
      }  
    }
    selectInput('catvar2in',label = 'Treat as Categories:',choices=NAMESTOKEEP2,multiple=TRUE)
    
  })
  

  
  output$catvar3 <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    if (!is.null(input$catvarin)&length(input$catvarin ) >=1) {
      NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvarin) ]
    }
    if (!is.null(input$catvarquantin)&length(input$catvarquantin ) >=1) {
      NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvarquantin) ]
    }
    if (!is.null(input$catvar2in)&length(input$catvar2in ) >=1) {
      NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvar2in) ]
    }
    selectizeInput(  "catvar3in", 'Custom cuts of this variable, defaults to min, median, max before any applied filtering:',
                     choices =NAMESTOKEEP2 ,multiple=FALSE,
                     options = list(    placeholder = 'Please select a variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                     )
    )
  })
  output$ncuts2 <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$catvar3in)) return()
    if (!is.null(input$catvar3in) && length(input$catvar3in ) <1)  return(NULL)
    if ( input$catvar3in!=""){
      textInput("xcutoffs", label =  paste(input$catvar3in,"Cuts"),
                value = as.character(paste(
                  min(df[,input$catvar3in] ,na.rm=T),
                  median(df[,input$catvar3in],na.rm=T),
                  max(df[,input$catvar3in],na.rm=T)
                  ,sep=",")
                )
      )
    }
  })
  output$asnumeric <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$catvar3in)) return()
    if (!is.null(input$catvar3in) && length(input$catvar3in ) <1)  return(NULL)
    if ( input$catvar3in!=""){
      column(12,
             checkboxInput('asnumericin', 'Treat as Numeric (helpful to overlay a smooth/regression line on top of a boxplot or to convert a variable into 0/1 and overlay a logistic fit', value = FALSE)
      )
    }
  })
  
  
  outputOptions(output, "catvar", suspendWhenHidden=FALSE)
  outputOptions(output, "catvar2", suspendWhenHidden=FALSE)
  outputOptions(output, "catvar3", suspendWhenHidden=FALSE)
  outputOptions(output, "ncuts2", suspendWhenHidden=FALSE)
  outputOptions(output, "asnumeric", suspendWhenHidden=FALSE)
  outputOptions(output, "catvarquant", suspendWhenHidden=FALSE)
  
  recodedata1  <- reactive({
    df <- values$maindata 
    validate(       need(!is.null(df), "Please select a data set"))
    if(!is.null(input$catvarin)&length(input$catvarin ) >=1) {
      for (i in 1:length(input$catvarin ) ) {
        varname<- input$catvarin[i]
        df[,varname] <- cut(df[,varname],input$ncuts , include.lowest = TRUE, right = FALSE, ordered_result = FALSE)
        df[,varname]   <- as.factor( df[,varname])
      }
    }
    df
  })
  
  recodedataquant  <- reactive({
    df <- recodedata1() 
    validate(       need(!is.null(df), "Please select a data set"))
    ngroups<- input$ncutsquant
    zeroplacebo<- input$zeroplacebo
    missingcategory <- input$missingcategory
    if(!is.null(input$catvarquantin)&length(input$catvarquantin ) >=1) {
      for (i in 1:length(input$catvarquantin ) ) {
        varname<- input$catvarquantin[i]
        x2<- unlist(df[,varname])
        if(zeroplacebo&&missingcategory){
          df[,varname]   <- table1::eqcut(x2, ngroups=ngroups,
                                          varlabel=varname,
                                          withhold=list(
                                            Placebo=(x2==0),
                                            Missing=(is.na(x2))))
        }
        if(zeroplacebo&&!missingcategory){
          df[,varname]   <- table1::eqcut(x2, ngroups=ngroups,
                                          varlabel=varname,
                                          withhold=list(
                                            Placebo=(x2==0)))
        }
        if(!zeroplacebo&&missingcategory){
          df[,varname]   <- table1::eqcut(x2, ngroups=ngroups,
                                          varlabel=varname,
                                          withhold=list(
                                            Missing=(is.na(x2))))
        }
        if(!zeroplacebo&&!missingcategory){
          withhold<- NULL
          df[,varname]   <- table1::eqcut(x2, ngroups=ngroups,
                                          varlabel=varname,
                                          withhold=NULL)
        }
        
      }
    }
    df
  })
  
  
  recodedata2  <- reactive({
    df <- recodedataquant()
    validate(       need(!is.null(df), "Please select a data set"))
    if(!is.null(input$catvar2in) ){
      if(length(input$catvar2in ) >=1) {
        for (i in 1:length(input$catvar2in ) ) {
          varname<- input$catvar2in[i]
          df[,varname]   <- as.factor( as.character(df[,varname]))
        }
      }  
    }
    df
  })
  
  output$contvar <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) !is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    selectInput('contvarin',label = 'Treat as Numeric:',choices=NAMESTOKEEP2,multiple=TRUE)
  })
  outputOptions(output, "contvar", suspendWhenHidden=FALSE)
  
  makedatacont  <- reactive({
    df <- recodedata2()
    validate(       need(!is.null(df), "Please select a data set"))
    if(!is.null(input$contvarin) ){
      if(length(input$contvarin ) >=1) {
        for (i in 1:length(input$contvarin ) ) {
          varname<- input$contvarin[i]
          df[,varname]   <- as.double( as.character(df[,varname]))
        }
      }
    }
    df
  })

  recodedata3  <- reactive({
    df <- makedatacont()
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$catvar3in)) return(NULL)
    if(input$catvar3in!="" && !is.null(input$xcutoffs)) {
      varname<- input$catvar3in
      xlimits <- input$xcutoffs
      nxintervals <- length(as.numeric(unique(unlist (strsplit(xlimits, ",")) ))) -1
      df[,varname] <- cut( as.numeric ( as.character(  df[,varname])),
                           breaks=   as.numeric(unique(unlist (strsplit(xlimits, ","))) ),include.lowest=TRUE)
      df[,"custombins"] <-   df[,varname] 
      
      if(input$asnumericin) {
        df[,varname] <- as.numeric(as.factor(df[,varname]) ) -1 
      }
    }
    df
  })
  output$bintext <- renderText({
    df <- factor_merge_data()
    validate(       need(!is.null(df), "Please select a data set"))
    bintextout <- ""
    if(input$catvar3in!="" && !is.null(input$asnumericin)) {
      varname<- input$catvar3in
      if(!input$asnumericin){
        bintextout <- levels(df[,"custombins"] )
      }
      if(input$asnumericin){
        bintextout <- paste( sort(unique(as.numeric(as.factor(df[,varname]) ) -1))  ,levels(df[,"custombins"] ),sep="/") 
      }}
    bintextout   
  })   
  
  recodedata4  <- reactive({
    df <- factor_merge_data()
    validate(       need(!is.null(df), "Please select a data set"))
    # get all the "change factor levels" inputs and apply them
    for (i in seq_len(changeLblsVals$numCurrent)) {
      variable_name <- input[[paste0("factor_lvl_change_select_", i)]]
      if (is.null(variable_name) || variable_name == "") next
      labels <- input[[paste0("factor_lvl_change_labels_", i)]]
      if (is.null(labels) || labels == "") next
      labels <- gsub("\\\\n", "\\\n", labels)
      if (!variable_name %in% names(df)) next
      
      ordered_lvls <- factor_lvl_diff_tracker[[ as.character(i) ]][[ "last_value" ]]
      ordered_lvls <- ordered_lvls[ order(is.na(ordered_lvls))]
      
      if (!is.null(ordered_lvls)) {
        ordered_lvls[ is.na(ordered_lvls)] <- ""
        df[, variable_name] <- factor(df[, variable_name],
                                      levels = names(ordered_lvls))
      }
      
      new_labels <- labels[1:nlevels(df[, variable_name])]
      new_labels[is.na(new_labels)] <- ""
      levels(df[, variable_name]) <- new_labels
    }
    df
  })
  
  
  output$pastevar <- renderUI({
    df <- recodedata4()
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    df <- df[!names(df)%in%"custombins"]
    MODEDF <- sapply(df, function(x) is.numeric(x))
    yvariables <- input$y
    NAMESTOKEEP2<- names(df)  [! MODEDF ]
    NAMESTOKEEP2<- NAMESTOKEEP2[!NAMESTOKEEP2 %in% yvariables]
    selectizeInput("pastevarin", "Combine the categories of these two variables:", choices = NAMESTOKEEP2,multiple=TRUE,
                   options = list(
                     maxItems = 2 ,
                     placeholder = 'Please select two variables',
                     onInitialize = I('function() { this.setValue(""); }'),
                     plugins = list('remove_button', 'drag_drop')
                   )
    )
  })
  
  pastedata  <- reactive({
    df <- recodedata4()
    validate(       need(!is.null(df), "Please select a data set"))
    df <- df[!names(df)%in%"custombins"]
    if( !is.null(input$pastevarin)   ) {
      if (length(input$pastevarin) > 1) {
        newcol_name <- paste(as.character(input$pastevarin),collapse="_",sep="")
        df <- unite(df, !!newcol_name,
                    c(input$pastevarin[1], input$pastevarin[2] ), remove=FALSE)
        
      }
    }
    df
  })
  
  
  
  
  outputOptions(output, "pastevar", suspendWhenHidden=FALSE)
  outputOptions(output, "bintext", suspendWhenHidden=FALSE)
  
  output$maxlevels <- renderUI({
    df <-pastedata()
    validate(       need(!is.null(df), "Please select a data set"))
    numericInput( inputId = "inmaxlevels",label = "Maximum number of unique values for Filter variable (1),(2),(3) (this is to avoid performance issues):",value = 500,min = 1,max = NA)
    
  })
  outputOptions(output, "maxlevels", suspendWhenHidden=FALSE)
  
  
  output$filtervar1 <- renderUI({
    df <-pastedata()
    validate(       need(!is.null(df), "Please select a data set"))
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    selectInput("infiltervar1" , "Filter variable (1):",c('None',NAMESTOKEEP ) )
  })
  
  output$filtervar2 <- renderUI({
    df <- pastedata()
    validate(       need(!is.null(df), "Please select a data set"))
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    selectInput("infiltervar2" , "Filter variable (2):",c('None',NAMESTOKEEP ) )
  })
  
  output$filtervar3 <- renderUI({
    df <- pastedata()
    validate(       need(!is.null(df), "Please select a data set"))
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    selectInput("infiltervar3" , "Filter variable (3):",c('None',NAMESTOKEEP ) )
  })
  
  
  output$filtervarcont1 <- renderUI({
    df <-pastedata()
    validate(       need(!is.null(df), "Please select a data set"))
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)
    NAMESTOKEEP<- NAMESTOKEEP[ is.element ( NAMESTOKEEP,names(df[sapply(df,is.numeric)]))]
    selectInput("infiltervarcont1" , "Filter continuous (1):",c('None',NAMESTOKEEP ) )
  })
  output$filtervarcont2 <- renderUI({
    df <-pastedata()
    validate(       need(!is.null(df), "Please select a data set"))
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  
    NAMESTOKEEP<- NAMESTOKEEP[ is.element ( NAMESTOKEEP,names(df[sapply(df,is.numeric)]))]
    selectInput("infiltervarcont2" , "Filter continuous (2):",c('None',NAMESTOKEEP ) )
  })
  output$filtervarcont3 <- renderUI({
    df <-pastedata()
    validate(       need(!is.null(df), "Please select a data set"))
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  
    NAMESTOKEEP<- NAMESTOKEEP[ is.element ( NAMESTOKEEP,names(df[sapply(df,is.numeric)]))]
    selectInput("infiltervarcont3" , "Filter continuous (3):",c('None',NAMESTOKEEP ) )
  })
  output$filtervar1values <- renderUI({
    df <-pastedata()
    validate(       need(!is.null(df), "Please select a data set"))
    if(is.null(input$infiltervar1) || input$infiltervar1=="None") {return(NULL)}
    if(!is.null(input$infiltervar1) && input$infiltervar1!="None" )  {
      choices <- levels(as.factor(df[,input$infiltervar1]))
      selectInput('infiltervar1valuesnotnull',
                  label = paste("Select values", input$infiltervar1),
                  choices = c(choices),
                  selected = choices,
                  multiple=TRUE, selectize=FALSE)   
    }
  }) 
  
  filterdata  <- reactive({
    df <-   pastedata()
    validate(       need(!is.null(df), "Please select a data set"))
    if(is.null(input$infiltervar1)) {
      return(df)
    }
    if(!is.null(input$infiltervar1)&input$infiltervar1!="None") {
      
      df <-  df [ is.element(df[,input$infiltervar1],input$infiltervar1valuesnotnull),]
    }
    
    df
  })
  
  output$filtervar2values <- renderUI({
    df <- filterdata()
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$infiltervar2)) return()
    if(input$infiltervar2=="None") {
      selectInput('infiltervar2valuesnull',
                  label ='No filter variable 2 specified', 
                  choices = list(""),multiple=TRUE, selectize=FALSE)   
    }
    if(input$infiltervar2!="None"&&!is.null(input$infiltervar2) )  {
      choices <- levels(as.factor(as.character(df[,input$infiltervar2])))
      selectInput('infiltervar2valuesnotnull',
                  label = paste("Select values", input$infiltervar2),
                  choices = c(choices),
                  selected = choices,
                  multiple=TRUE, selectize=FALSE)   
    }
  })
  
  filterdata2  <- reactive({
    df <- filterdata()
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$infiltervar2)) {
      return(df)
    }
    if(input$infiltervar2 != "None") {
      df <-  df [ is.element(df[,input$infiltervar2],input$infiltervar2valuesnotnull),]
    }
    df
  }) 
  output$filtervar3values <- renderUI({
    df <- filterdata2()
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$infiltervar3)) return()
    if(input$infiltervar3 == "None") {
      selectInput('infiltervar3valuesnull',
                  label ='No filter variable 3 specified', 
                  choices = list(""),multiple=TRUE, selectize=FALSE)   
    } 
    if(input$infiltervar3!="None"&&!is.null(input$infiltervar3) )  {
      choices <- levels(as.factor(as.character(df[,input$infiltervar3])))
      selectizeInput('infiltervar3valuesnotnull',
                     label = paste("Select values", input$infiltervar3),
                     choices = c(choices),
                     selected = choices,
                     multiple=TRUE,
                     options = list(
                       plugins = list('remove_button'))
      )   
    }
  })
  
  filterdata3  <- reactive({
    df <- filterdata2()
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$infiltervar3)) {
      return(df)
    }
    if(input$infiltervar3!="None") {
      df <-  df [ is.element(df[,input$infiltervar3],input$infiltervar3valuesnotnull),]
    }
    df
  })  
  
  output$fslider1 <- renderUI({ 
    df <-  filterdata3()
    validate(       need(!is.null(df), "Please select a data set"))
    xvariable<- input$infiltervarcont1
    if(is.null(xvariable) || input$infiltervarcont1=="None" ){
      return(NULL)  
    }
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    if(input$infiltervarcont1!="None" ){
      sliderInput("infSlider1", paste("Select",xvariable,"Range"),
                  min=min(df[,xvariable],na.rm=T),
                  max=max(df[,xvariable],na.rm=T),
                  value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
      )
    }             
  })
  filterdata4  <- reactive({
    df <- filterdata3()
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$infiltervarcont1)) return()
    if(input$infiltervarcont1!="None" ){
      if(is.numeric( input$infSlider1[1]) & is.numeric(df[,input$infiltervarcont1])) {
        df <- df [!is.na(df[,input$infiltervarcont1]),]
        df <-  df [df[,input$infiltervarcont1] >= input$infSlider1[1]&df[,input$infiltervarcont1] <= input$infSlider1[2],]
      }
    }
    
    df
  })
  output$fslider2 <- renderUI({ 
    df <-  filterdata4()
    validate(       need(!is.null(df), "Please select a data set"))
    xvariable<- input$infiltervarcont2
    if(input$infiltervarcont2=="None" ){
      return(NULL)  
    }
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    if(input$infiltervarcont2!="None" ){
      sliderInput("infSlider2", paste("Select",xvariable,"Range"),
                  min=min(df[,xvariable],na.rm=T),
                  max=max(df[,xvariable],na.rm=T),
                  value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
      )
    }             
  })
  
  
  filterdata5  <- reactive({
    df <- filterdata4()
    validate(       need(!is.null(df), "Please select a data set"))
    if(input$infiltervarcont2!="None" ){
      if(is.numeric( input$infSlider2[1]) & is.numeric(df[,input$infiltervarcont2])) {
        df<- df [!is.na(df[,input$infiltervarcont2]),]
        df<-df [df[,input$infiltervarcont2] >= input$infSlider2[1]&df[,input$infiltervarcont2] <= input$infSlider2[2],]
      }
    }
    
    df
  })
  
  output$fslider3 <- renderUI({ 
    df <-  filterdata5()
    validate(       need(!is.null(df), "Please select a data set"))
    xvariable<- input$infiltervarcont3
    if(input$infiltervarcont3=="None" ){
      return(NULL)  
    }
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    if(input$infiltervarcont3!="None" ){
      sliderInput("infSlider3", paste("Select",xvariable,"Range"),
                  min=min(df[,xvariable],na.rm=T),
                  max=max(df[,xvariable],na.rm=T),
                  value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
      )
    }             
  })
  
  
  filterdata6  <- reactive({
    df <- filterdata5()
    validate(       need(!is.null(df), "Please select a data set"))
    if(input$infiltervarcont3!="None" ){
      if(is.numeric( input$infSlider3[1]) & is.numeric(df[,input$infiltervarcont3])) {
        df<- df [!is.na(df[,input$infiltervarcont3]),]
        df<-df [df[,input$infiltervarcont3] >= input$infSlider3[1]&df[,input$infiltervarcont3] <= input$infSlider3[2],]
      }
    }
    
    df
  })
  
  outputOptions(output, "filtervar1", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar2", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar3", suspendWhenHidden=FALSE)
  
  outputOptions(output, "filtervarcont1", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervarcont2", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervarcont3", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar1values", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar2values", suspendWhenHidden=FALSE)
  outputOptions(output, "filtervar3values", suspendWhenHidden=FALSE)
  
  outputOptions(output, "fslider1", suspendWhenHidden=FALSE)
  outputOptions(output, "fslider2", suspendWhenHidden=FALSE)
  outputOptions(output, "fslider3", suspendWhenHidden=FALSE)
  
  output$onerowidgroup <- renderUI({
    df <- filterdata6()
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items 
    items= c(items) 
    selectizeInput("onerowidgroupin", "Keep First Row by ID(s):", choices = items,multiple=TRUE,
                   options = list(
                     placeholder = 'Please select at least one variable that is not in y variable(s)',
                     onInitialize = I('function() { this.setValue(""); }'),
                     plugins = list('remove_button')
                   )
    )
    
  })
  outputOptions(output, "onerowidgroup", suspendWhenHidden=FALSE)
  filterdata7  <- reactive({
    df <- filterdata6()
    validate(       need(!is.null(df), "Please select a data set"))
    if( !is.null(input$onerowidgroupin) && length(input$onerowidgroupin) >0 ){
      vars<- c(as.vector(input$onerowidgroupin) )
      df <-   df %>%
        group_by(!!!syms(vars))
      df<- df %>% filter(row_number()==1 ) %>%
        ungroup()
    }
    if(is.null(input$onerowidgroupin) || length(input$onerowidgroupin) <1 ){
      df <-   df
    }
    as.data.frame(df)
  })
  
  output$onerowidlastgroup <- renderUI({
    df <- filterdata7()
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items 
    items= c(items) 
    selectizeInput("onerowidlastgroupin", "Keep Last Row by ID(s):", choices = items,multiple=TRUE,
                   options = list(
                     placeholder = 'Please select at least one variable that is not in y variable(s)',
                     onInitialize = I('function() { this.setValue(""); }'),
                     plugins = list('remove_button')
                   )
    )
    
  })
  outputOptions(output, "onerowidlastgroup", suspendWhenHidden=FALSE)  
  filterdata8  <- reactive({
    df <- filterdata7()
    validate(       need(!is.null(df), "Please select a data set"))
    if( !is.null(input$onerowidlastgroupin) && length(input$onerowidlastgroupin) >0 ){
      vars<- c(as.vector(input$onerowidlastgroupin) )
      df <-   df %>%
        group_by(!!!syms(vars))
      df<- df %>% filter(row_number()==n() ) %>%
        ungroup()
    }
    if(is.null(input$onerowidlastgroupin) || length(input$onerowidlastgroupin) <1 ){
      df <-   df
    }
    as.data.frame(df)
  })
  
  output$roundvar <- renderUI({
    df <- filterdata8()
    validate(       need(!is.null(df), "Please select a data set"))
    if (!is.null(df)){
      items=names(df)
      names(items)=items
      MODEDF <- sapply(df, function(x) is.numeric(x))
      NAMESTOKEEP2<- names(df)  [MODEDF]
      selectizeInput(  "roundvarin", "Round the Values to the Specified N Digits:", choices = NAMESTOKEEP2,multiple=TRUE,
                       options = list(
                         placeholder = 'Please select some variables',
                         onInitialize = I('function() { this.setValue(""); }')
                       )
      )
    }
  }) 
  outputOptions(output, "roundvar", suspendWhenHidden=FALSE)
  
  rounddata <- reactive({
    df <- filterdata8()
    validate(       need(!is.null(df), "Please select a data set"))
    if(!is.null(input$roundvarin)&&length(input$roundvarin ) >=1) {
      for (i in 1:length(input$roundvarin ) ) {
        varname<- input$roundvarin[i]
        df[,varname]   <- round( df[,varname],input$rounddigits)
      }
    }
    df
  })  
  
  output$divideynum <- renderUI({
    df <- rounddata()
    validate(       need(!is.null(df), "Please select a data set"))
    if (!is.null(df)){
      items=names(df)
      names(items)=items
      MODEDF <- sapply(df, function(x) is.numeric(x))
      NAMESTOKEEP2<- names(df)  [MODEDF]
      selectizeInput(  "divideynumin", "Divide the Values by the specified column:", choices = NAMESTOKEEP2,multiple=TRUE,
                       options = list(
                         placeholder = 'Please select some variables',
                         onInitialize = I('function() { this.setValue(""); }')
                       )
      )
    }
  })
  
  output$divideydenom <- renderUI({
    df <- rounddata()
    validate(       need(!is.null(df), "Please select a data set"))
    if (!is.null(df)){
      items=names(df)
      names(items)=items
      MODEDF <- sapply(df, function(x) is.numeric(x) )
      NAMESTOKEEP2<- names(df)  [MODEDF]
      selectInput(  "divideydenomin", "Variable to divide by", choices = NAMESTOKEEP2,multiple=FALSE)
    }
  })
  
  output$divideynum2 <- renderUI({
    df <- dividedata()
    validate(       need(!is.null(df), "Please select a data set"))
    if (!is.null(df)){
      items=names(df)
      names(items)=items
      MODEDF <- sapply(df, function(x) is.numeric(x))
      NAMESTOKEEP2<- names(df)  [MODEDF]
      selectizeInput(  "divideynumin2", "Divide the Values by a constant:", choices = NAMESTOKEEP2,
                       multiple=TRUE,
                       options = list(
                         placeholder = 'Please select some variables',
                         onInitialize = I('function() { this.setValue(""); }')
                       )
      )
    }
  })
  
  
  outputOptions(output, "divideynum", suspendWhenHidden=FALSE)
  outputOptions(output, "divideydenom", suspendWhenHidden=FALSE)
  outputOptions(output, "divideynum2", suspendWhenHidden=FALSE)
  
  
  dividedata <- reactive({
    df <- rounddata()
    validate(       need(!is.null(df), "Please select a data set"))
    if(!is.null(input$divideynumin)&&length(input$divideynumin ) >=1 &&
       !is.null(input$divideydenomin)) {
      for (i in 1:length(input$divideynumin ) ) {
        varname<- input$divideynumin[i]
        dosname<- input$divideydenomin
        df[,varname]   <-  df[,varname] /as.numeric(as.character(df[,dosname]))
      }
    }
    df
  })
  
  dividedata2 <- reactive({
    df <- dividedata()
    validate(       need(!is.null(df), "Please select a data set"))
    if(!is.null(input$divideynumin2)&&length(input$divideynumin2 ) >=1) {
      for (i in 1:length(input$divideynumin2 ) ) {
        varname<- input$divideynumin2[i]
        df[,varname]   <-  df[,varname] /input$divideyconstant
      }
    }
    df
  })
  
  tabledata <- reactive({
    df <- dividedata2() 
    df
  })
  
  stackdata <- reactive({
    
    df <- dividedata2()
    validate(       need(!is.null(df), "Please select a data set"))
    if (!is.null(df)){
      validate(  need(!is.element(input$x,input$y) ,
                      "Please select a different x variable or remove the x variable from the list of y variable(s)"))
      
      tidydata <- NULL
      if(!is.null(input$y) ){
        
        validate(need(all(input$y %in% names(df)), "Invalid y value(s)"))
        
        tidydata <- df %>%
          gather( "yvars", "yvalues", !!!input$y ,factor_key = TRUE) 
        if (!all( sapply(df[,as.vector(input$y)], is.numeric)) ) {
          tidydata <- tidydata %>%
            mutate(yvalues=as.factor(as.character(yvalues) ))
        }
        
      } else {
        tidydata <- df
        if (nrow(tidydata) > 0) {
          tidydata$yvars <- "None"
          tidydata$yvalues <- NA
        }
      }
      
      tidydata
    }
  })
  
  
  
  output$reordervar <- renderUI({
    df <- stackdata()
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ !MODEDF ]
    selectizeInput(  "reordervarin", 'Reorder This Variable:',
                     choices =NAMESTOKEEP2 ,multiple=FALSE,
                     options = list(    placeholder = 'Please select a variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                     )
    )
  })
  
  
  
  output$variabletoorderby <- renderUI({
    df <-stackdata()
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$reordervarin)) return()
    if (length(input$reordervarin ) <1)  return(NULL)
    if ( input$reordervarin!=""){
      yinputs <- input$y
      items=names(df)
      names(items)=items
      MODEDF <- sapply(df, function(x) is.numeric(x))
      NAMESTOKEEP2<- names(df)  [ MODEDF ]
      selectInput('varreorderin',label = 'Of this Variable:', choices=NAMESTOKEEP2,multiple=FALSE,selected="yvalues")
    }
  })
  
  
  
  outputOptions(output, "reordervar", suspendWhenHidden=FALSE)
  outputOptions(output, "variabletoorderby", suspendWhenHidden=FALSE)
  
  
  
  reorderdata <- reactive({
    df <- stackdata()
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$reordervarin)) {
      return(df)
    }
    if(length(input$reordervarin ) >=1 &&
       length(input$varreorderin ) >=1 && input$reordervarin!=""  ) {
      varname<- input$reordervarin[1]
      if(input$functionordervariable=="Median" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin], FUN=function(x) median(x[!is.na(x)]))
      }
      if(input$functionordervariable=="Mean" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin],  FUN=function(x) mean(x[!is.na(x)]))
      }
      if(input$functionordervariable=="Minimum" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin],  FUN=function(x) min(x[!is.na(x)]))
      }
      if(input$functionordervariable=="Maximum" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin],  FUN=function(x) max(x[!is.na(x)]))
      }
      if(input$functionordervariable=="N" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin],  FUN=function(x) length(x[!is.na(x)]))
      }
      if(input$functionordervariable=="SD" )  {
        df[,varname]   <- reorder( df[,varname],df[,input$varreorderin],  FUN=function(x) sd(x[!is.na(x)]))
      }
      
      if(input$reverseorder )  {
        df[,varname] <- factor( df[,varname], levels=rev(levels( df[,varname])))
        
      }
    }
    df
  })  
  output$reordervar2 <- renderUI({
    df <- reorderdata()
    validate(       need(!is.null(df), "Please select a data set"))
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP<- names(df)  [ !MODEDF ]
    if(!is.null(input$reordervarin)&&length(input$reordervarin ) >=1  ){
      NAMESTOKEEP<- NAMESTOKEEP  [ NAMESTOKEEP!=input$reordervarin ]
    }
    NAMESTOKEEP<- NAMESTOKEEP[ NAMESTOKEEP!="yvars" ]
    selectInput("reordervar2in" , "Custom Reorder this variable:",c('None',NAMESTOKEEP ) )
  })
  
  output$reordervar2values <- renderUI({
    df <- reorderdata()
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$reordervar2in)) return()
    if(input$reordervar2in=="None") {
      selectInput('reordervar2valuesnull',
                  label ='No reorder variable specified', 
                  choices = list(""),multiple=TRUE, selectize=FALSE)   
    }
    if(input$reordervar2in!="None"&&!is.null(input$reordervar2in) )  {
      choices <- levels(as.factor(as.character(df[,input$reordervar2in])))
      selectizeInput('reordervar2valuesnotnull',
                     label = paste("Drag/Drop to reorder",input$reordervar2in, "values"),
                     choices = c(choices),
                     selected = choices,
                     multiple=TRUE,  options = list(
                       plugins = list('drag_drop')
                     )
      )   
    }
  })
  outputOptions(output, "reordervar2", suspendWhenHidden=FALSE)
  outputOptions(output, "reordervar2values", suspendWhenHidden=FALSE)
  
  reorderdata2 <- reactive({
    df <- reorderdata()
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$reordervar2in)) {
      return(df)
    }
    if(input$reordervar2in!="None"  ) {
      df [,input$reordervar2in] <- factor(df [,input$reordervar2in],
                                          levels = input$reordervar2valuesnotnull)
      
    }
    df
  })
  
  
  
  # Populate the "Change levels of this variable:" list
  observeEvent(stackdata(), {
    df <- stackdata()
    items <- names(df)
    names(items) <- items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2 <- names(df)[!MODEDF]
    updateSelectizeInput(session, "change_labels_stat_var", selected = "",
                         choices = NAMESTOKEEP2)
  })
  
  output$change_labels_stat_old <- renderText({
    df <- reorderdata2()
    req(df, input$change_labels_stat_var != "")
    paste(levels(as.factor(df[, input$change_labels_stat_var])), collapse = " ")
  })   
  
  # Show the input of the labels the user wants to change for a stat variable
  observe({
    df <- reorderdata2()
    if (is.null(df) || is.null(input$change_labels_stat_var) || length(input$change_labels_stat_var) < 1 || input$change_labels_stat_var == "") {
      return()
    }
    nlevels <- length( unique( levels(as.factor( df[,input$change_labels_stat_var] ))))
    levelsvalues <- levels(as.factor( df[,input$change_labels_stat_var] ))
    label <- paste(input$change_labels_stat_var, "requires", nlevels, "new labels, type in a comma separated list below")
    value <- paste(as.character(levelsvalues), collapse=", ", sep="")
    updateTextInput(session, "change_labels_stat_levels", label = label, value = value) 
  })
  
  recodedata5  <- reactive({
    df <- reorderdata2()
    validate(       need(!is.null(df), "Please select a data set"))
    if (is.null(input$change_labels_stat_var)) {
      return(df)
    }
    if(input$change_labels_stat_var!="" && input$change_labels_stat_levels != "") {
      varname <- input$change_labels_stat_var
      xlabels <- input$change_labels_stat_levels 
      xlabels <- gsub("\\\\n", "\\\n", xlabels)
      df[,varname] <- as.factor(df[,varname])
      levels(df[,varname])  <-  unlist (strsplit(xlabels, ",") )
    }
    df
  })
  
  # --- Merge factor levels feature ---
  
  
  # Variables to help with maintaining the dynamic number of "merge levels of
  # a factor" boxes
  factor_merge_vals <- reactiveValues(
    num_current = 0,  # How many boxes are there currently
    num_total = 0  # Max # of boxes at the same time, to prevent memory leaks
  )
  
  # Add UI and corresponding outputs+observers for a "merge factor levels"
  # section
  add_factor_merge_box <- function() {
    factor_merge_vals$num_current <- factor_merge_vals$num_current + 1
    
    df <- recodedata3()
    factors <- df %>%
      sapply(is.factor) %>%
      which() %>%
      names()
    
    insertUI(
      selector = "#factor_merge_placeholder", where = "beforeEnd",
      immediate = TRUE,
      div(
        class = "factor_merge_box",
        selectizeInput(
          paste0("factor_merge_select_", factor_merge_vals$num_current),
          sprintf("Factor to merge (%s):", factor_merge_vals$num_current),
          choices = c("", factors),
          selected = ""
        ),
        div(
          class = "blind-dropdown",
          shinyjs::hidden(
            checkboxGroupInput(
              inputId = paste0("factor_merge_levels_", factor_merge_vals$num_current),
              label = "Levels to merge",
              choices = c()
            )
          )
        )
      )
    )
    
    # if we already had this many sections before, no need to wire up any
    # new observers
    if (factor_merge_vals$num_current <= factor_merge_vals$num_total) {
      return()
    }
    
    num1 <- factor_merge_vals$num_current
    factor_merge_vals$num_total <- num1
    
    # when the user selects a factor to merge
    observeEvent(input[[paste0("factor_merge_select_", num1)]], {
      selected_var <- input[[paste0("factor_merge_select_", num1)]]
      
      if (selected_var == "") {
        shinyjs::hide(paste0("factor_merge_levels_", num1))
        return()
      }
      shinyjs::show(paste0("factor_merge_levels_", num1))
      
      df <- factor_merge_data()
      levelsvalues <- levels(df[[selected_var]])
      
      updateCheckboxGroupInput(
        session, paste0("factor_merge_levels_", num1),
        choices = levelsvalues,
        selected = c()
      )
    })
  }
  
  remove_last_factor_merge_box <- function() {
    updateSelectInput(session, paste0("factor_merge_select_", factor_merge_vals$num_current), selected = "")
    selector <- paste0("#factor_merge_placeholder .factor_merge_box:nth-child(", factor_merge_vals$num_current, ")")
    removeUI(selector, multiple = FALSE, immediate = TRUE)
    factor_merge_vals$num_current <- factor_merge_vals$num_current - 1
    shinyjs::enable("factor_merge_add")
  }
  
  # Decide if to enable/disable the remove variable labels button
  observeEvent(factor_merge_vals$num_current, {
    shinyjs::toggleState("factor_merge_remove", condition = factor_merge_vals$num_current > 0)
  })
  
  # when recodedata3 changes, reset the merge levels UI
  observeEvent(recodedata3(), {
    shinyjs::show("factor_merge_section")
    
    factor_merge_vals$num_current <- 0
    
    removeUI(selector = ".factor_merge_box",
             multiple = TRUE, immediate = TRUE)
    
    add_factor_merge_box()
  })
  
  # add another "merge factor levels" box
  observeEvent(input$factor_merge_add, {
    shinyjs::disable(paste0("factor_merge_select_", factor_merge_vals$num_current))
    shinyjs::disable(paste0("factor_merge_levels_", factor_merge_vals$num_current))
    add_factor_merge_box()
  })
  # remove the last "merge factor levels" box
  observeEvent(input$factor_merge_remove, {
    remove_last_factor_merge_box()
  })
  
  # The final dataframe that transforms the data from te previous step into data
  # after the mergings are processed
  factor_merge_data_raw <- reactive({
    df <-  recodedata3()
    if (is.null(df)) return()
    
    for (i in seq_len(factor_merge_vals$num_current)) {
      # no valid factor is selected
      variable_name <- input[[paste0("factor_merge_select_", i)]]
      if (is.null(variable_name) || variable_name == "") next
      
      # the checkboxes (levels) of a factor don't match the factor
      old_levels <- levels(df[[variable_name]])
      levels_to_merge <- input[[paste0("factor_merge_levels_", i)]]
      if (is.null(levels_to_merge) || !all(levels_to_merge %in% old_levels)) next
      
      new_level <- paste0(levels_to_merge, collapse = "/")
      new_levels <- c(
        levels(df[[variable_name]])[!levels(df[[variable_name]]) %in% levels_to_merge],
        new_level
      )
      df[[variable_name]] <-
        df[[variable_name]] %>%
        as.character() %>%
        {.[. %in% levels_to_merge] = new_level; .} %>%
        factor(levels = new_levels)
    }
    df
  })
  # so that plot doesn't update too rapidly and the user has time to select multiple labels
  factor_merge_data <- factor_merge_data_raw %>% debounce(400)
  
  # --- End: Merge Factor Levels feature
  
  finalplotdata <- reactive({
    df <- recodedata5()
    as.data.frame(df)
  })
  
  output$xaxiszoom <- renderUI({
    df <- finalplotdata()
    validate(       need(!is.null(df), "Please select a data set"))
    
    if (!is.numeric(df[,input$x] ) ) return(NULL)
    if (all(is.numeric(df[,input$x]) &&
            input$facetscalesin!="free_x"&&
            input$facetscalesin!="free")){
      xvalues <- df[,input$x][!is.na( df[,input$x])]
      if (length(xvalues) > 0) {
        xmin <- min(xvalues)
        xmax <- max(xvalues)
        xstep <- (xmax -xmin)/100
        sliderInput('xaxiszoomin',label = 'Zoom to X variable range:', min=xmin, max=xmax, value=c(xmin,xmax),step=xstep)
      }
    }
    
    
  })
  outputOptions(output, "xaxiszoom", suspendWhenHidden=FALSE)
  
  output$lowerx <- renderUI({
    df <-finalplotdata()
    if (is.null(df)| !is.numeric(df[,input$x] ) ) return(NULL)
    if (all(is.numeric(df[,input$x]) &&
            input$facetscalesin!="free_x"&&
            input$facetscalesin!="free")){
      xvalues <- df[,input$x][!is.na( df[,input$x])]
      if (length(xvalues) > 0) {
        xmin <- min(xvalues)
        numericInput("lowerxin",label = "Lower X Limit",value = xmin,min=NA,max=NA,width='100%')
      }
    }
  })
  output$upperx <- renderUI({
    df <-finalplotdata()
    if (is.null(df)| !is.numeric(df[,input$x] ) ) return(NULL)
    if (all(is.numeric(df[,input$x]) &&
            input$facetscalesin!="free_x"&&
            input$facetscalesin!="free")){
      xvalues <- df[,input$x][!is.na( df[,input$x])]
      if (length(xvalues) > 0) {
        xmax <- max(xvalues)
        numericInput("upperxin",label = "Upper X Limit",value = xmax,min=NA,max=NA,width='100%')
      }
    }
  }) 
  outputOptions(output, "lowerx", suspendWhenHidden=FALSE)
  outputOptions(output, "upperx", suspendWhenHidden=FALSE)
  
  output$yaxiszoom <- renderUI({
    df <-finalplotdata()
    if ( is.null(input$y)  ) return(NULL)
    if ( !is.null(input$y)  ){
      if (is.null(df)|| !is.numeric(df[,"yvalues"] ) || (length(input$y) > 1 ) ) return(NULL)
      if (all(is.numeric(df[,"yvalues"]) &&  (length(input$y) < 2 ) &&
              input$facetscalesin!="free_y"&&
              input$facetscalesin!="free")){
        yvalues <- df[,"yvalues"][!is.na( df[,"yvalues"])]
        ymin <- min(yvalues)
        if(input$yaxisscale=="logy"&& ymin==0) ymin <- 0.01
        ymax <- max(yvalues)
        ystep <- (ymax -ymin)/100
        sliderInput('yaxiszoomin',label = 'Zoom to Y variable range:', min=ymin, max=ymax, value=c(ymin,ymax),step=ystep)
        
      }
    }
    
    
    
    
  })
  outputOptions(output, "yaxiszoom", suspendWhenHidden=FALSE)  
  
  output$lowery <- renderUI({
    df <-finalplotdata()
    if (is.null(df) || is.null(df$yvalues) || !is.numeric(df[,"yvalues"] ) || (length(input$y) > 1 ) ) return(NULL)
    if (all(is.numeric(df[,"yvalues"]) &&  (length(input$y) < 2 ) &&
            input$facetscalesin!="free_y"&&
            input$facetscalesin!="free")){
      yvalues <- df[,"yvalues"][!is.na( df[,"yvalues"])]
      ymin <- min(yvalues)
      if(input$yaxisscale=="logy") ymin <- min(yvalues)+0.01
      numericInput("loweryin",label = "Lower Y Limit",value = ymin,min=NA,max=NA,width='50%')
    }
  })
  output$uppery <- renderUI({
    df <-finalplotdata()
    if (is.null(df) || is.null(df$yvalues) || !is.numeric(df[,"yvalues"] ) || (length(input$y) > 1 ) ) return(NULL)
    if (all(is.numeric(df[,"yvalues"]) &&  (length(input$y) < 2 ) &&
            input$facetscalesin!="free_y"&&
            input$facetscalesin!="free")){
      yvalues <- df[,"yvalues"][!is.na( df[,"yvalues"])]
      ymax <- max(yvalues)
      numericInput("upperyin",label = "Upper Y Limit",value = ymax,min=NA,max=NA,width='50%')
    }
  }) 
  outputOptions(output, "lowery", suspendWhenHidden=FALSE)
  outputOptions(output, "uppery", suspendWhenHidden=FALSE)
  
  
  output$colour <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items
    if ( !is.null(input$y) ){
      items = c("None",items, "yvars","yvalues") 
    }
    if ( is.null(input$y) ){
      items = c("None",items) 
    }
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("colorin", "Colour By:",items) 
    
  })
  
  
  output$group <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items 
    items= c("None",items, "yvars","yvalues") 
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("groupin", "Group By:",items)
  })
  outputOptions(output, "colour", suspendWhenHidden=FALSE)
  outputOptions(output, "group", suspendWhenHidden=FALSE)
  
  
  output$facet_col <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items 
    items =c(None='.',items,"yvars", "yvalues")
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("facetcolin", "Column Split:",items)
  })
  output$facet_row <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items 
    items =c(None='.',items,"yvars", "yvalues")
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("facetrowin", "Row Split:", items)
  })
  
  output$facet_col_extra <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items
    items =c(None='.',items,"yvars", "yvalues")
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("facetcolextrain", "Extra Column Split:",items)
  })
  output$facet_row_extra <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items
    
    if (length(input$y) < 2 ){
      items= c(None=".",items,"yvars", "yvalues")    
      if (!is.null(input$pastevarin) && length(input$pastevarin) >1 ){
        nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
        items= c(items,nameofcombinedvariables)    
      }
    }
    if (length(input$y) > 1  ){
      items= c("yvars",None=".",items, "yvalues")    
      if (!is.null(input$pastevarin) && length(input$pastevarin) >1 ){
        nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="")
        items= c(items,nameofcombinedvariables)    
      }
    }
    
    selectInput("facetrowextrain", "Extra Row Split:",items)
  })
  
  
  
  output$facetscales <- renderUI({
    items= c("fixed","free_x","free_y","free")   
    if (!is.null(input$y)&&length(input$y) > 1 ){
      items= c("free_y","fixed","free_x","free")    
    }
    selectInput('facetscalesin','Facet Scales:',items)
  })
  
  outputOptions(output, "facet_row_extra", suspendWhenHidden=FALSE)
  outputOptions(output, "facet_col_extra", suspendWhenHidden=FALSE)
  outputOptions(output, "facet_row", suspendWhenHidden=FALSE)
  outputOptions(output, "facet_col", suspendWhenHidden=FALSE)
  outputOptions(output, "facetscales", suspendWhenHidden=FALSE)
  
  output$pointsize <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items 
    items= c("None",items, "yvars","yvalues") 
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("pointsizein", "Size By:",items )
    
  })
  
  
  output$labeltext <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items 
    items= c("None",items, "yvars","yvalues") 
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("labeltextin", "Label By:",items )
    
  })
  
  
  
  output$pointshape <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items 
    items= c("None",items, "yvars","yvalues") 
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("pointshapein", "Shape By:",items )
    
  })
  
  output$linetype <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items 
    items= c("None",items, "yvars","yvalues") 
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("linetypein", "Linetype By:",items )
  })
  
  output$fill <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    names(items)=items
    items= items
    items= c("None",items, "yvars","yvalues") 
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    selectInput("fillin", "Fill By:"    ,items )
  })
  
  output$weight <- renderUI({
    df <- recodedata5()
    validate(       need(!is.null(df), "Please select a data set"))
    items=names(df)
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    items= c("None",NAMESTOKEEP2, "yvalues") 
    selectInput("weightin", "Weight By:",items )
  })
  outputOptions(output, "pointsize", suspendWhenHidden=FALSE)
  outputOptions(output, "fill", suspendWhenHidden=FALSE)
  outputOptions(output, "weight", suspendWhenHidden=FALSE)
  outputOptions(output, "linetype", suspendWhenHidden=FALSE)
  outputOptions(output, "labeltext", suspendWhenHidden=FALSE)
  outputOptions(output, "pointshape", suspendWhenHidden=FALSE)
  
  
  
  output$mytablex = renderDataTable({
    df <- finalplotdata() 
    validate(       need(!is.null(df), "Please select a data set"))
    
    datatable(df ,
              extensions = c('ColReorder','Buttons','FixedColumns'),
              options = list(dom = 'Bfrtip',
                             searchHighlight = TRUE,
                             pageLength=5 ,
                             lengthMenu = list(c(5, 10, 15, -1), c('5','10', '15', 'All')),
                             colReorder = list(realtime = TRUE),
                             buttons = 
                               list('colvis', 'pageLength','print','copy', list(
                                 extend = 'collection',
                                 buttons = list(
                                   list(extend='csv'  ,filename = 'plotdata'),
                                   list(extend='excel',filename = 'plotdata'),
                                   list(extend='pdf'  ,filename = 'plotdata')),
                                 text = 'Download'
                               )),
                             scrollX = TRUE,scrollY = 400,
                             fixedColumns = TRUE
              ), 
              filter = 'bottom',
              style = "bootstrap")
  })
  
  output$userdefinedcolor <- renderUI({ 
    req(input$nusercol)
    lev <- 1:input$nusercol
    if( length(lev) > 10 ){
      cols <- c(tableau20)
    }
    if( length(lev) <= 10 ){
      cols <- c(tableau10)
    }
    if(input$themecolorswitcher=="themeuser"){
      lapply(seq_along(lev), function(i) {
        div(
        colourpicker::colourInput(inputId = paste0("col", lev[i]),
                                  label = paste0("Choose color", lev[i]),
                                  value = cols[i]
        ), style = "display: inline-block;")        
      })
    }
    
  })
  
  output$userdefinedshape <- renderUI({ 
    req(input$nusershape)
    lev <- 1:input$nusershape
      shapes <- c("circle open",
                  "triangle open",
                  "square open",
                  "plus",
                  "square cross",
                  "asterisk",
                  "circle small" ,"triangle" ,"square")
    shapes <- rep_len(shapes, input$nusershape)

    if(input$scaleshapeswitcher=="themeuser"){
      lapply(seq_along(lev), function(i) {
        div(
          selectInput(inputId = paste0("shape", lev[i]),label = paste0('Choose shape:', lev[i]),
                    c(
                      "square open"           ,
                      "circle open"           ,
                      "triangle open"         ,
                      "plus"                  ,
                      "cross"                 ,
                      "asterisk"              ,
                      "diamond open"          ,
                      "triangle down open"    ,
                      "square cross"          ,
                      "diamond plus"          ,
                      "circle plus"           ,
                      "star"                  ,
                      "square plus"           ,
                      "circle cross"          ,
                      "square triangle"       ,
                      "square"                ,
                      "circle small"          ,
                      "triangle"              ,
                      "diamond"               ,
                      "circle"                ,
                      "bullet"                ,
                      "circle filled"         ,
                      "square filled"         ,
                      "diamond filled"        ,
                      "triangle filled"       ,
                      "triangle down filled" 
                    ), selected = shapes[i]
        ), style = "display: inline-block;")  
        
      })
    }
    
  })
  
  output$userdefinedlinetype <- renderUI({ 
    req(input$nuserlinetype)
    lev <- 1:input$nuserlinetype
    linetypes <- c("solid","dashed", "dotted", "dotdash", "longdash", "twodash","blank")
    linetypes <- rep_len(linetypes, input$nuserlinetype)
    
    if(input$scalelinetypeswitcher=="themeuser"){
      lapply(seq_along(lev), function(i) {
        div(selectInput(inputId = paste0("linetype", lev[i]),label = paste0('Choose linetype:', lev[i]),
                    c("solid","dashed", "dotted", "dotdash", "longdash", "twodash","blank"), selected = linetypes[i]
        ), style = "display: inline-block;")  
        
      })
    }
    
  })
  
  
  output$userdefinedcontcolor <- renderUI({ 
      if(input$themecontcolorswitcher=="themeuser"){
      list(
        colourpicker::colourInput(
          "colcont1",
          "Starting Color",
          value = muted("red"),
          showColour = "both",
          allowTransparent = FALSE,returnName = TRUE),
        
        colourpicker::colourInput(
          "colcont2",
          "Midpoint Color",
          value ="white",
          showColour = "both",
          allowTransparent = FALSE,returnName = TRUE),
        
        colourpicker::colourInput(
          "colcont3",
          "Ending Color",
          value =muted("blue"),
          showColour = "both",
          allowTransparent = FALSE,returnName = TRUE)
  )
      }
    })
  
  
  observeEvent(input$userdefinedcolorreset, {
    req(input$nusercol)
    lev <- 1:input$nusercol
    if( length(lev) > 10 ){
      cols <- c(tableau20)
    }
    if( length(lev) <= 10 ){
      cols <- c(tableau10)
    }
    lapply(seq_along(lev), function(i) {
      do.call(what = "updateColourInput",
              args = list(
                session = session,
                inputId = paste0("col", lev[i]),
                value = cols[i]
              )
      )
    })
  })
  
  observeEvent(input$userdefinedcolorhighlight, {
    req(input$nusercol)
    lev <- 1:input$nusercol
    cols <- c("#D62728",rep("lightgray",input$nusercol-1))
    lapply(seq_along(lev), function(i) {
      do.call(what = "updateColourInput",
              args = list(
                session = session,
                inputId = paste0("col", lev[i]),
                value = cols[i]
              )
      )
    })
  })


  observe({
    facet_choices <- unique(c(
      input$facetcolextrain,
      input$facetcolin,
      input$facetrowin,
      input$facetrowextrain
    ))
    facet_choices <- setdiff(facet_choices, ".")
    updateSelectInput(session, "facetmargin_vars", choices = facet_choices)
  })
  
  facetmargins <- reactive(
    if (input$facetmargin == "none") {
      return(FALSE)
    } else if (input$facetmargin == "all") {
      return(TRUE)
    } else if (input$facetmargin == "some") {
      return(input$facetmargin_vars)
    } else {
      stop("Invalid facetmargin input")
    }
  )
  
  plotObject <- reactive({
    # Don't generate a new plot if the user wants to refresh manually
    if (!input$auto_update_plot) {
      if (values$updatePlot == TRUE) {
        values$updatePlot <- FALSE
      } else {
        return(values$prevPlot)
      }
    }
    
    # Retrieve the correct dataset
    if (input$show_pairs) {
      validate(need(!is.null(input$y), "Please select at least one Y variable"))
      plotdata <- rounddata()
    } else {
      plotdata <- finalplotdata() 
    }
    validate(need(!is.null(plotdata), "Please select a data set") )
    
    # Fix the colour palettes
    #continuous
    if (input$themecontcolorswitcher=="themeggplot"){
      scale_colour_continuous<- function(...) 
        scale_colour_gradient(...,guide = "colourbar")
      
      scale_fill_continuous<- function(...) 
        scale_fill_gradient(...,guide = "colourbar")
    }
    
    if (input$themecontcolorswitcher=="RedWhiteBlue"){
      
      scale_colour_continuous<- function(...) 
        scale_colour_gradient2(..., 
                               low = muted("red"), 
                               mid = input$midcolor,
                               high = muted("blue"),
                               midpoint = input$colormidpoint, space = "Lab",
                               na.value = "grey50", guide = "colourbar")
      
      scale_fill_continuous<- function(...) 
        scale_fill_gradient2(...,
                             low = muted("red"),
                             mid = input$midcolor,
                               high = muted("blue"),
                             midpoint = input$colormidpoint, space = "Lab",
                               na.value = "grey50", guide = "colourbar")
    }
    if (input$themecontcolorswitcher=="RedWhiteGreen"){
      
      scale_colour_continuous <- function(...) 
        scale_colour_gradient2(..., low = muted("red"),
                               mid = input$midcolor,
                               high = muted("darkgreen"),
                               midpoint = input$colormidpoint, space = "Lab",
                               na.value = "grey50", guide = "colourbar")
      
      scale_fill_continuous <- function(...) 
        scale_fill_gradient2(...,
                             low = muted("red"),
                             mid = input$midcolor,
                            high = muted("darkgreen"),
                            midpoint = input$colormidpoint, space = "Lab",
                             na.value = "grey50", guide = "colourbar")
      
    }
    
    if (input$themecontcolorswitcher=="themeuser"){
      scale_colour_continuous <- function(...) 
        scale_colour_gradient2(...,
                               low = input$colcont1,,#gradientTableData()[1,1],
                               mid = input$colcont2,#gradientTableData()[2,1],
                               high =input$colcont3,#gradientTableData()[3,1],
                               midpoint = input$colormidpoint, space = "Lab",
                               na.value = "grey50", guide = "colourbar")
      
      scale_fill_continuous <- function(...) 
        scale_fill_gradient2(...,
                             low = input$colcont1,,#gradientTableData()[1,1],
                             mid = input$colcont2,#gradientTableData()[2,1],
                             high = input$colcont3,#gradientTableData()[3,1],
                             midpoint = input$colormidpoint, space = "Lab",
                             na.value = "grey50", guide = "colourbar")
      
    }
    
    #discrete
    if (input$themecolorswitcher=="themetableau10"){
      scale_colour_discrete <- function(...) 
        scale_colour_manual(..., values = tableau10,drop=!input$themecolordrop)
      scale_fill_discrete <- function(...) 
        scale_fill_manual(..., values = tableau10,drop=!input$themecolordrop)
    }
    if (input$themecolorswitcher=="themeuser"){
      cols <- paste0("c(", paste0("input$col", 1:input$nusercol, collapse = ", "), ")")
      cols <- eval(parse(text = cols))
      scale_colour_discrete <- function(...) 
        scale_colour_manual(..., values = cols,drop=!input$themecolordrop)
      scale_fill_discrete <- function(...) 
        scale_fill_manual(..., values = cols,drop=!input$themecolordrop)
    }
    if (input$themecolorswitcher=="themetableau20"){
      scale_colour_discrete <- function(...) 
        scale_colour_manual(..., values = tableau20,drop=!input$themecolordrop)
      scale_fill_discrete <- function(...) 
        scale_fill_manual(..., values = tableau20,drop=!input$themecolordrop)
    }
    if (input$themecolorswitcher=="themecolorblind"){
      scale_colour_discrete <- function(...) 
        scale_colour_manual(..., values = cbPalette,drop=!input$themecolordrop)
      scale_fill_discrete <- function(...) 
        scale_fill_manual(..., values = cbPalette,drop=!input$themecolordrop)
    }
    if (input$themecolorswitcher=="themecolorblind2"){
      scale_colour_discrete <- function(...) 
        scale_colour_manual(..., values = cbbPalette,drop=!input$themecolordrop)
      scale_fill_discrete <- function(...) 
        scale_fill_manual(..., values = cbbPalette,drop=!input$themecolordrop)
    }
    
    if (input$scaleshapeswitcher=="themeuser"){
      shapes <- paste0("c(", paste0("input$shape", 1:input$nusershape, collapse = ", "), ")")
      shapes <- eval(parse(text = shapes))
      scale_shape_discrete <- function(...)
        scale_shape_manual(..., values = shapes)
    }
    
    if (input$scalelinetypeswitcher=="themeuser"){
      linetypes <- paste0("c(", paste0("input$linetype", 1:input$nuserlinetype, collapse = ", "), ")")
      linetypes <- eval(parse(text = linetypes))
      scale_linetype_discrete <- function(...)
        scale_linetype_manual(..., values = linetypes)
    }
    
    # Determine what type of plot to show based on what variables were chosen
    if (input$show_pairs) {
      # Matrix of pairs of plots of all the Y variables
      if (input$colorin == 'None'){
        p <- sourceable(GGally::ggpairs(plotdata, columns = input$y,
                                        diag = list(continuous = GGally::wrap("densityDiag", alpha=0.2),
                                                    discrete = GGally::wrap("barDiag",  alpha=0.2,position="dodge2")),
                                        lower = list(continuous = GGally::wrap("smooth", alpha = 0.2, size=0.1),
                                                     combo = GGally::wrap("facethist", alpha=0.2,position="dodge2"),
                                                     discrete = GGally::wrap("facetbar",  alpha=0.2,position="dodge2")
                                        ),
                                        upper = list(continuous = function(data, mapping, ...) {
                                          GGally::ggally_cor(data = data, mapping = mapping, size=4, alignPercent=0.8)
                                            },
                                                     combo = GGally::wrap("box_no_facet", alpha=0.2),
                                                     discrete = GGally::wrap("facetbar",  alpha=0.2,position="dodge2")),
                                        progress = FALSE)
                        )
      }
      if (input$colorin != 'None'){
        
        if( !is.numeric(plotdata[,input$colorin]) ){
        p <- sourceable(
          GGally::ggpairs(
            plotdata,
            columns = input$y,
            mapping = ggplot2::aes_string(color = input$colorin),
            diag = list(
              continuous = function(data, mapping, ...) {
                GGally::ggally_densityDiag(
                  data = data,
                  mapping = mapping,
                  alpha = 0.2,
                  linetype = 0
                ) +
                  scale_colour_discrete()+
                  scale_fill_discrete()
              },
              discrete = function(data, mapping, ...) {
                GGally::ggally_barDiag(
                  data = data,
                  mapping = mapping,
                  alpha = 0.2,
                  position = "dodge2"
                ) +
                  scale_colour_discrete()+
                  scale_fill_discrete()
              }
            ),
            lower = list(
              continuous = function(data, mapping, ...) {
                GGally::ggally_smooth(
                  data = data,
                  mapping = mapping,
                  alpha = 0.2,
                  size = 0.1
                ) +
                  scale_colour_discrete()+
                  scale_fill_discrete()
              },
              combo = function(data, mapping, ...) {
                GGally::ggally_facethist(
                  data = data,
                  mapping = mapping,
                  alpha = 0.2,
                  position = "dodge2"
                ) +
                  scale_colour_discrete()+
                  scale_fill_discrete()
              },
              discrete = function(data, mapping, ...) {
                GGally::ggally_facetbar(
                  data = data,
                  mapping = mapping,
                  alpha = 0.2,
                  position = "dodge2"
                ) +
                  scale_colour_discrete()+
                  scale_fill_discrete()
              }
            ),
            upper = list(
              continuous = function(data, mapping, ...) {
                GGally::ggally_cor(
                  data = data,
                  mapping = mapping,
                  size = 4,
                  alignPercent = 0.8
                ) +
                  scale_colour_discrete()+
                  scale_fill_discrete()
              },
              combo = function(data, mapping, ...) {
                GGally::ggally_box_no_facet(
                  data = data,
                  mapping = mapping,
                  alpha = 0.2) +
                  scale_colour_discrete()+
                  scale_fill_discrete()
              },
              discrete = function(data, mapping, ...) {
                GGally::ggally_facetbar(
                  data = data,
                  mapping = mapping,
                  alpha = 0.2,
                  position = "dodge2"
                ) +
                  scale_colour_discrete()+
                  scale_fill_discrete()
              }
            ),
            progress = FALSE
          )
        )
        }

      }
       

      
    } else if (is.null(input$y)) {
      # Univariate plot
      
      if(is.numeric(plotdata[,input$x]) ){
        #validate(       need(is.numeric(plotdata[,input$x]), "Please select a numeric x variable"))
        p <- sourceable(ggplot(plotdata, aes_string(x=input$x)))
        if (input$colorin != 'None')
          p <- p + aes_string(color=input$colorin)
        if (input$fillin != 'None')
          p <- p + aes_string(fill=input$fillin)
        if (input$groupin != 'None')
          p <- p + aes_string(group=input$groupin)
        if (input$linetypein != 'None'){
          p <- p  + aes_string(linetype=input$linetypein)
        }
        
        if (input$groupin == 'None' && !is.numeric(plotdata[,input$x]) 
            && input$colorin == 'None' && input$linetypein == 'None' && input$fillin == 'None')
          p <- p + aes(group=1L)
        
        if ( input$histogramaddition=="Counts"  && input$histogrambinwidth =="None"  ){
          p <- p+ 
            geom_histogram(aes(y=..count..), alpha=input$histogramalpha,bins = input$histonbins,
                           position =input$positionhistogram)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add)))+
            ylab("Counts")
        }
        
        if ( input$histogramaddition=="Counts" && input$histogrambinwidth =="userbinwidth" ){
          p <- p+ 
            geom_histogram(aes(y=..count..), alpha=input$histogramalpha, binwidth = input$histobinwidth,
                           position =input$positionhistogram)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add)))+
            ylab("Counts")
        }
        if ( input$histogramaddition=="Counts" && input$histogrambinwidth =="autobinwidth" ){
          p <- p+ 
            geom_histogram(aes(y=..count..),alpha=input$histogramalpha,
                           binwidth = function(x) { 2 * IQR(x) / (length(x)^(1/3)  )} ,
                           position =input$positionhistogram)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add)))+
            ylab("Counts")
        }
        
        
        
        if ( input$histogramaddition=="Density"  && input$histogrambinwidth =="None"  ){
          p <- p+ 
            geom_histogram(aes(y=..density..), alpha=input$histogramalpha,bins = input$histonbins,
                           position =input$positionhistogram)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) +
            ylab("Counts")
        }
        
        if ( input$histogramaddition=="Density" && input$histogrambinwidth =="userbinwidth" ){
          p <- p+ 
            geom_histogram(aes(y=..density..), alpha=input$histogramalpha, binwidth = input$histobinwidth,
                           position =input$positionhistogram)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) +
            ylab("Counts")
        }
        if ( input$histogramaddition=="Density" && input$histogrambinwidth =="autobinwidth" ){
          p <- p+ 
            geom_histogram(aes(y=..density..),alpha=input$histogramalpha, binwidth = function(x) { 2 * IQR(x) / (length(x)^(1/3)  )} ,
                           position =input$positionhistogram)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) +
            ylab("Counts")
        }
        
        
        if ( input$histogramaddition=="ncounts"  && input$histogrambinwidth =="None"  ){
          p <- p+ 
            geom_histogram(aes(y=..ncount..), alpha=input$histogramalpha,bins = input$histonbins,
                           position =input$positionhistogram)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) +
            ylab("Counts")
        }
        
        if ( input$histogramaddition=="ncounts" && input$histogrambinwidth =="userbinwidth" ){
          p <- p+ 
            geom_histogram(aes(y=..ncount..), alpha=input$histogramalpha, binwidth = input$histobinwidth,
                           position =input$positionhistogram)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) +
            ylab("Counts")
        }
        if ( input$histogramaddition=="ncounts" && input$histogrambinwidth =="autobinwidth" ){
          p <- p+ 
            geom_histogram(aes(y=..ncount..),alpha=input$histogramalpha, binwidth = function(x) { 2 * IQR(x) / (length(x)^(1/3)  )} ,
                           position =input$positionhistogram)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) +
            ylab("Counts")
        }
        
        
        if ( input$densityaddition=="Density"){
          p <- p+
            geom_density(aes(y=..density..),alpha=input$densityalpha,adjust=input$densityadjust)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) +
            ylab("Density")
          
        }
        if ( input$densityaddition=="Scaled Density"){
          p <- p+
            geom_density(aes(y=..scaled..),alpha=input$densityalpha,adjust=input$densityadjust)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) +
            ylab("Scaled Density")
          
        }
        if ( input$densityaddition=="Counts"){
          p <- p+
            geom_density(aes(y=..count..),alpha=input$densityalpha,adjust=input$densityadjust)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) +
            ylab("Counts")
          
        }
        if ( input$densityaddition=="histocount"){
          p <- p+
            geom_density(aes(binwidth=input$histobinwidth,y=binwidth*..count..),
                         alpha=input$densityalpha,adjust=input$densityadjust)+
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) +
            ylab("Counts")
          
        }
        ###### rug geom start
        if(input$addrugmarks) {
          
          if(! input$rugignorecol){
            p <- p +
              geom_rug(sides = paste(input$rugsides,collapse="",sep=""),
                       show.legend = FALSE,
                       alpha = input$ruglinealpha,
                       length = ggplot2::unit(input$ruglinelength ,"npc") 
              ) 
          }
          if(input$rugignorecol){
            p <- p +
              geom_rug(sides = paste(input$rugsides,collapse="",sep=""),
                       show.legend = FALSE,
                       alpha = input$ruglinealpha,
                       length = ggplot2::unit(input$ruglinelength ,"npc"),
                       col = input$colrug
              ) 
          }
          
        }
        if(input$addextrarugmarks &&
           !is.null(input$xrug) &&
           length(as.vector(input$xrug)) > 0) {
          for(i in input$xrug) {
            if(!input$rugignorecol){
              p <- p +
                geom_rug(aes_string(x=i),
                         sides = paste(input$extrarugsides, collapse="",sep=""),
                         show.legend = FALSE, inherit.aes = FALSE,
                         alpha = input$ruglinealpha,
                         length = ggplot2::unit(input$ruglinelength ,"npc")
                )
            }
            if(input$rugignorecol){
              p <- p +
                geom_rug(aes_string(x=i),
                         sides = paste(input$extrarugsides, collapse="",sep=""),
                         show.legend = FALSE, inherit.aes = FALSE,
                         alpha = input$ruglinealpha,
                         length = ggplot2::unit(input$ruglinelength ,"npc"),
                         col = input$colrug 
                )
            }
          }
        }
        #### rug geom end  
      }
      
      if(!is.numeric(plotdata[,input$x]) ){
        if(input$barplotorder=="frequency"){
          plotdata[,input$x]<- factor(as.factor(plotdata[,input$x]),
                                      levels=names(sort(table(plotdata[,input$x]), 
                                                        decreasing=FALSE)))
        }
        if(input$barplotorder=="revfrequency"){
          plotdata[,input$x]<- factor(as.factor(plotdata[,input$x]),
                                      levels=names(sort(table(plotdata[,input$x]), 
                                                        decreasing=TRUE)))           
        }
        p <- sourceable(ggplot(plotdata, aes_string(x=input$x)))
        
        
        
        if (input$colorin != 'None')
          p <- p + aes_string(color=input$colorin)
        
        if (input$fillin != 'None')
          p <- p + aes_string(fill=input$fillin)
        
        if (input$groupin != 'None')
          p <- p + aes_string(group=input$groupin)
        
        #if (input$groupin == 'None' & !is.numeric(plotdata[,input$x]) 
        #   & input$colorin == 'None')
        # p <- p + aes(group=1)
        
        if ( input$barplotaddition&!input$barplotpercent){
          p <- p + 
            geom_bar(alpha=0.2,position = eval(parse(text=input$positionbar)))
          
          p <- p +
            scale_y_continuous(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) +
            ylab("Count")
          
          if ( input$barplotlabel){
            p <- p+   geom_text(aes(y = ((..count..)),
                                    label = ((..count..))),
                                stat = "count", vjust = 0.5,size=5,
                                position = eval(parse(text=input$positionbar)))
          }
          
          
          if ( input$barplotflip){
            p <- p +
              coord_flip()
          }
        }
        if ( input$barplotaddition&input$barplotpercent){
          p <- p+  
            geom_bar(alpha=0.2,aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) ,
                     position = eval(parse(text=input$positionbar)))
          
          if ( input$barplotlabel){
            if(input$positionbar!="position_fill(vjust = 0.5)")
            {
              p <- p+   geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]),
                                      label = scales::percent(
                                        ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))),
                                  stat = "count", vjust = 0.5,size=5,
                                  position = eval(parse(text=input$positionbar)))    
            }
            
          }
          
          
          p <- p +   scale_y_continuous(labels = percent,
                                       expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                          add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) +
            ylab("Percentage")
          if ( input$barplotflip){
            p <- p +
              coord_flip()
          }
        }
      }
    } else {
      # X-Y plot
      
      p <- sourceable(ggplot(plotdata, aes_string(x=input$x, y="yvalues")))
      
      p <- p # helps in initializing the scales
      
      if (input$showtarget)  {
        if (is.numeric( plotdata[,"yvalues"] ) ) {
          if (!is.numeric( plotdata[,input$x] )){
            p <-   p   + scale_x_discrete() }
          
          p <-   p   +
            annotate("rect", xmin = -Inf, xmax = Inf,
                     ymin = input$lowerytarget1,
                     ymax = input$upperytarget1,
                     fill = input$targetcol1,
                     alpha = input$targetopacity1)
          
        } 
        
      } 
      
      if (input$showtarget2)  {
        if ( is.numeric( plotdata[,"yvalues"] ) ) {
          if (!is.numeric( plotdata[,input$x] )){
            p <-   p   + scale_x_discrete() }
          
          p <-   p   +
            annotate("rect", xmin = -Inf,
                     xmax = Inf,
                     ymin = input$lowerytarget2,
                     ymax = input$upperytarget2,
                     fill = input$targetcol2,
                     alpha = input$targetopacity2)  
        } 
      } 
      
      
      if (input$colorin != 'None')
        p <- p + aes_string(color=input$colorin)
      if (input$fillin != 'None')
        p <- p + aes_string(fill=input$fillin)
      if (input$pointsizein != 'None' )
        p <- p  + aes_string(size=input$pointsizein)
      
      if (input$pointshapein != 'None'){
        p <- p  + aes_string(shape=input$pointshapein)
      }
      if (input$linetypein != 'None'){
        p <- p  + aes_string(linetype=input$linetypein)
      }
      
      # if (input$groupin != 'None' & !is.factor(plotdata[,input$x]))
      if (input$groupin != 'None')
        p <- p + aes_string(group=input$groupin)
      if (input$groupin == 'None' & !is.numeric(plotdata[,input$x]) 
          & input$colorin == 'None')
        p <- p + aes(group=1)
      
      
      
      if (input$Points=="Points"){
        if (input$jitterdirection =="None"){
          positionpoints <- "position_identity()"
        }
        
        if (input$jitterdirection=="Vertical"){
          positionpoints <- "position_jitter(width=0)"
        }
        
        if (input$jitterdirection=="Horizontal"){
          positionpoints <-  "position_jitter(height=0)"
        }
        
        if (input$jitterdirection=="Default"){
          positionpoints <-  "position_jitter()"
        }
        if (input$jitterdirection=="Custom"){
          positionpoints <-  paste0("position_jitter(height=",
                              input$jittervertical,
                              ",width=",input$jitterhorizontal,")")
        }
        if (input$jitterdirection=="dodge"){
          positionpoints <-  paste0("position_dodge(width=",input$pointdodgewidth,")")
        }
        if (input$jitterdirection=="dodgev"){
          positionpoints <-  paste0("position_dodgev(height=",input$pointdodgeheight,")")
        }
        
        p <- attach_source_dep(p, "positionpoints")
        
        if (input$pointshapein != 'None' && !input$pointignoreshape){
          
          if (input$pointsizein == 'None'&& !input$pointignorecol)
            p <- p + geom_point(size=input$pointsizes,alpha=input$pointstransparency,
                                position=eval(parse(text=positionpoints)))
          if (input$pointsizein != 'None'&& !input$pointignorecol&& !input$pointignoresize)
            p <- p + geom_point(alpha=input$pointstransparency,
                                position=eval(parse(text=positionpoints)))
          if (input$pointsizein != 'None'&& !input$pointignorecol&& input$pointignoresize)
            p <- p + geom_point(size=input$pointsizes,alpha=input$pointstransparency,
                                position=eval(parse(text=positionpoints)))
          
          if (input$pointsizein == 'None'&&input$pointignorecol)
            p <- p + geom_point(size=input$pointsizes,
                                alpha=input$pointstransparency,
                                colour=input$colpoint,
                                position=eval(parse(text=positionpoints)))
          if (input$pointsizein != 'None'&& input$pointignorecol&& !input$pointignoresize)
            p <- p + geom_point(alpha=input$pointstransparency,
                                colour=input$colpoint,
                                position=eval(parse(text=positionpoints)))
          if (input$pointsizein != 'None'&& input$pointignorecol && input$pointignoresize )
            p <- p + geom_point(size=input$pointsizes,alpha=input$pointstransparency,
                                colour=input$colpoint,
                                position=eval(parse(text=positionpoints)))
        }
        
        if (input$pointshapein != 'None' && input$pointignoreshape){
          if (input$pointsizein == 'None'&& !input$pointignorecol)
            p <- p + geom_point(size=input$pointsizes,alpha=input$pointstransparency,
                                shape=input$pointshapes,
                                position=eval(parse(text=positionpoints)))
          if (input$pointsizein != 'None'&& !input$pointignorecol&& !input$pointignoresize)
            p <- p + geom_point(alpha=input$pointstransparency,
                                shape=input$pointshapes,
                                position=eval(parse(text=positionpoints)))
          if (input$pointsizein != 'None'&& !input$pointignorecol&& input$pointignoresize)
            p <- p + geom_point(size=input$pointsizes,alpha=input$pointstransparency,
                                shape=input$pointshapes,
                                position=eval(parse(text=positionpoints)))
          
          if (input$pointsizein == 'None'&&input$pointignorecol)
            p <- p + geom_point(size=input$pointsizes,alpha=input$pointstransparency,
                                colour=input$colpoint,
                                shape=input$pointshapes,
                                position=eval(parse(text=positionpoints)))
          if (input$pointsizein != 'None'&& input$pointignorecol&& !input$pointignoresize)
            p <- p + geom_point(alpha=input$pointstransparency,colour=input$colpoint,
                                position=eval(parse(text=positionpoints)))
          if (input$pointsizein != 'None'&& input$pointignorecol && input$pointignoresize )
            p <- p + geom_point(size=input$pointsizes,alpha=input$pointstransparency,colour=input$colpoint,
                                position=eval(parse(text=positionpoints)))
        }
        
        if(input$pointshapein == 'None' ){
          if (input$pointsizein == 'None'&& !input$pointignorecol)
            p <- p + geom_point(size=input$pointsizes,alpha=input$pointstransparency,
                                shape=input$pointshapes,
                                position=eval(parse(text=positionpoints)))
          if (input$pointsizein != 'None'&& !input$pointignorecol&& !input$pointignoresize)
            p <- p + geom_point(alpha=input$pointstransparency,
                                shape=input$pointshapes,
                                position=eval(parse(text=positionpoints)))
          if (input$pointsizein != 'None'&& !input$pointignorecol&& input$pointignoresize)
            p <- p + geom_point(size=input$pointsizes,alpha=input$pointstransparency,
                                shape=input$pointshapes,
                                position=eval(parse(text=positionpoints)))
          
          if (input$pointsizein == 'None'&&input$pointignorecol)
            p <- p + geom_point(size=input$pointsizes,alpha=input$pointstransparency,colour=input$colpoint,
                                shape=input$pointshapes,
                                position=eval(parse(text=positionpoints)))
          if (input$pointsizein != 'None'&& input$pointignorecol&& !input$pointignoresize)
            p <- p + geom_point(alpha=input$pointstransparency,colour=input$colpoint,
                                shape=input$pointshapes,
                                position=eval(parse(text=positionpoints)))
          if (input$pointsizein != 'None'&& input$pointignorecol && input$pointignoresize )
            p <- p + geom_point(size=input$pointsizes,alpha=input$pointstransparency,colour=input$colpoint,
                                shape=input$pointshapes,
                                position=eval(parse(text=positionpoints)))
          
          
        }
        
        
        
      }
      
      if (input$line=="Lines"){
        
        if (input$linetypein != 'None' && !input$lineignorelinetype){
          
          if (input$pointsizein == 'None'&& !input$lineignorecol)
            p <- p + geom_line(size=input$linesize,alpha=input$linestransparency)
          if (input$pointsizein != 'None'&& !input$lineignorecol&& !input$lineignoresize)
            p <- p + geom_line(alpha=input$linestransparency)
          if (input$pointsizein != 'None'&& !input$lineignorecol&& input$lineignoresize)
            p <- p + geom_line(size=input$linesize,alpha=input$linestransparency)
          
          if (input$pointsizein == 'None'&&input$lineignorecol)
            p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,colour=input$colline)
          if (input$pointsizein != 'None'&& input$lineignorecol&& !input$lineignoresize)
            p <- p + geom_line(alpha=input$linestransparency,colour=input$colline)
          if (input$pointsizein != 'None'&& input$lineignorecol && input$lineignoresize )
            p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,colour=input$colline)
        }
        
        if (input$linetypein != 'None' && input$lineignorelinetype){
          
          if (input$pointsizein == 'None'&& !input$lineignorecol)
            p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes)
          if (input$pointsizein != 'None'&& !input$lineignorecol&& !input$lineignoresize)
            p <- p + geom_line(alpha=input$linestransparency,linetype=input$linetypes)
          if (input$pointsizein != 'None'&& !input$lineignorecol&& input$lineignoresize)
            p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes)
          
          if (input$pointsizein == 'None'&&input$lineignorecol)
            p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
          if (input$pointsizein != 'None'&& input$lineignorecol&& !input$lineignoresize)
            p <- p + geom_line(alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
          if (input$pointsizein != 'None'&& input$lineignorecol && input$lineignoresize )
            p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
        }
        
        if(input$linetypein == 'None' ){
          if (input$pointsizein == 'None'&& !input$lineignorecol)
            p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes)
          if (input$pointsizein != 'None'&& !input$lineignorecol&& !input$lineignoresize)
            p <- p + geom_line(alpha=input$linestransparency,linetype=input$linetypes)
          if (input$pointsizein != 'None'&& !input$lineignorecol&& input$lineignoresize)
            p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes)
          
          if (input$pointsizein == 'None'&&input$lineignorecol)
            p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
          if (input$pointsizein != 'None'&& input$lineignorecol&& !input$lineignoresize)
            p <- p + geom_line(alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
          if (input$pointsizein != 'None'&& input$lineignorecol && input$lineignoresize )
            p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
        }
        
        
      }
      
      #### Boxplot Section START
      
      if (input$boxplotaddition) {
        if (input$groupin != 'None') {
          if (!input$boxplotignoregroup) {
            if (!input$boxplotignorecol) {
              p <- p + geom_boxplot(
                aes_string(group = input$groupin),
                varwidth = input$boxplotvarwidh,
                notch = input$boxplotnotch,
                show.legend = input$boxplotshowlegend,
                alpha = input$boxplotalpha,
                outlier.alpha = input$boxplotoutlieralpha,
                outlier.size = input$boxplotoutliersize
              )
            }
            if (input$boxplotignorecol) {
              p <- p + geom_boxplot(
                aes_string(group = input$groupin),
                col = input$boxcolline,
                outlier = input$boxcolline,
                varwidth = input$boxplotvarwidh,
                notch = input$boxplotnotch,
                show.legend = input$boxplotshowlegend,
                alpha = input$boxplotalpha,
                outlier.alpha = input$boxplotoutlieralpha,
                outlier.size = input$boxplotoutliersize
              )
            }
          }
        }
        if (input$groupin == 'None' || input$boxplotignoregroup) {
          if (!input$boxplotignorecol) {
            p <- p + geom_boxplot(
              aes(group = NULL),
              varwidth = input$boxplotvarwidh,
              notch = input$boxplotnotch,
              show.legend = input$boxplotshowlegend,
              alpha = input$boxplotalpha,
              outlier.alpha = input$boxplotoutlieralpha,
              outlier.size = input$boxplotoutliersize
            )
          }
          if (input$boxplotignorecol) {
            p <- p + geom_boxplot(
              aes(group = NULL),
              varwidth = input$boxplotvarwidh,
              notch = input$boxplotnotch,
              show.legend = input$boxplotshowlegend,
              col = input$boxcolline,
              alpha = input$boxplotalpha,
              outlier.alpha = input$boxplotoutlieralpha,
              outlier.size = input$boxplotoutliersize
            )
          }
        }
      }
      #### Boxplot Section END
      
      
      ###### Mean section  START
      if (input$Mean!="None") {
      if (input$positionmean=="position_identity"){
        positionmean <-  "position_identity()"
      }
      if (input$positionmean=="position_dodge"){
        positionmean<-  paste0("position_dodge(width=",input$errbar,")")
      }
      p <- attach_source_dep(p, "positionmean")
      }
      
      if (!input$meanignoregroup) {
        
        if (!input$meanignorecol) {
          
          if (input$Mean=="Mean") {
            
            if(input$meanlines && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",
                                alpha=input$alphameanl,
                                position = eval(parse(text=positionmean)))
            
            if(input$meanlines && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",
                                size=input$meanlinesize,
                                alpha=input$alphameanl,
                                position = eval(parse(text=positionmean)))
            

          }# mean
          if(input$Mean=="Mean/CI" && input$pointsizein == 'None')  {
            
            if (input$geommeanCI== "ribbon"){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                            fun.args=list(conf.int=input$CI), 
                            size=input$meanlinesize,
                            alpha=input$meancitransparency,
                            col=NA,
                            position = eval(parse(text=positionmean)))
            }
            if (input$geommeanCI== "errorbar"){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                            fun.args=list(conf.int=input$CI), width = input$errbar,
                            size=input$meanlinesize,
                            alpha=input$meancitransparency,
                            position = eval(parse(text=positionmean)))
            }
            if (input$meanlines){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line", stat ="smooth"  ,
                            fun.args=list(conf.int=input$CI),
                            size=input$meanlinesize,
                            alpha=input$alphameanl,
                            position = eval(parse(text=positionmean)))
            }
          }
          
          if (input$Mean=="Mean/CI" && input$pointsizein != 'None'){
              
              if (input$geommeanCI== "ribbon"){
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                              fun.args=list(conf.int=input$CI), 
                              alpha=input$meancitransparency,
                              col=NA,
                              position = eval(parse(text=positionmean)))
              }
              if (input$geommeanCI== "errorbar"){
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                              fun.args=list(conf.int=input$CI), width = input$errbar,
                              alpha=input$meancitransparency,
                              position = eval(parse(text=positionmean)))
              }
            if (input$meanlines){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line", stat ="smooth"  ,
                            fun.args=list(conf.int=input$CI),
                            alpha=input$alphameanl,
                            position = eval(parse(text=positionmean)))
            }
              
            }

          if(input$Mean!="None" &&  !input$forcemeanshape)    {
            if(input$meanpoints && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "point", alpha=input$alphameanp,
                                position = eval(parse(text=positionmean)))
            
            if(input$meanpoints && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "point", size=input$meanpointsize,
                                alpha=input$alphameanp,
                                position = eval(parse(text=positionmean)))               
          }
          
            if(input$Mean!="None" && input$forcemeanshape)    {
              if(input$meanpoints && input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",
                              alpha=input$alphameanp,
                              shape=input$meanshapes,
                              position = eval(parse(text=positionmean)))
              
              if(input$meanpoints && input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",
                              size=input$meanpointsize,
                              alpha=input$alphameanp,
                              shape=input$meanshapes,
                              position = eval(parse(text=positionmean)))
            }
          

            
          if (input$Mean!="None" && input$meanvalues )  {
            p <-   p   +
              stat_summary(fun.data = mean.n, geom = input$geommeanlabel,
                           alpha=input$alphameanlabel,
                           fun.y = mean,
                           fontface = "bold",
                           position = eval(parse(text=positionmean)),
                           show.legend=FALSE,size=6, seed=1234)
          }
          if (input$Mean!="None" && input$meanN)  {
            p <-   p   +
              stat_summary(fun.data = give.n,  geom = input$geommeanlabel,
                           alpha=input$alphameanlabel,
                           fun.y = mean, fontface = "bold",
                           position = eval(parse(text=positionmean)),
                           show.legend=FALSE,size=6, seed=1234)      
          }
          
        } #do not ignore col do not ignore group
        
        
        if (input$meanignorecol) {
          meancoll <- input$colmeanl
          meancolp <- input$colmeanp
          
          if (input$Mean=="Mean") {
            if(input$meanlines && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",col=meancoll,
                                alpha=input$alphameanl,
                                position = eval(parse(text=positionmean)))
            
            if(input$meanlines && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",col=meancoll,
                                size=input$meanlinesize,
                                alpha=input$alphameanl,
                                position = eval(parse(text=positionmean)))

          }
          
          if(input$Mean=="Mean/CI" && input$pointsizein != 'None') {
              
              if (input$geommeanCI== "ribbon"){
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                              fun.args=list(conf.int=input$CI), 
                              alpha=input$meancitransparency,
                              col=NA,
                              position = eval(parse(text=positionmean)))
              }
              if (input$geommeanCI== "errorbar"){
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                              fun.args=list(conf.int=input$CI), width = input$errbar,
                              alpha=input$meancitransparency,
                              col=meancoll,
                              position = eval(parse(text=positionmean)))
              }
              if(input$meanlines) {
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "line", stat ="smooth"  ,
                              fun.args=list(conf.int=input$CI),
                              alpha=input$alphameanl,
                              col=meancoll,
                              position = eval(parse(text=positionmean)))
                
              }
              
            }
            
          if(input$Mean=="Mean/CI" && input$pointsizein == 'None') {
              if (input$geommeanCI== "ribbon"){
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                              fun.args=list(conf.int=input$CI), 
                              size=input$meanlinesize,
                              alpha=input$meancitransparency,
                              col=NA,
                              position = eval(parse(text=positionmean)))
              }
              if (input$geommeanCI== "errorbar"){
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                              fun.args=list(conf.int=input$CI), width = input$errbar,
                              col=meancoll,
                              size=input$meanlinesize,
                              alpha=input$meancitransparency,
                              position = eval(parse(text=positionmean)))
              }
              if(input$meanlines) {
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "line", stat ="smooth"  ,
                              fun.args=list(conf.int=input$CI),
                              alpha=input$alphameanl,
                              col=meancoll,
                              size=input$meanlinesize,
                              position = eval(parse(text=positionmean)))
                
              }
              
              }

            if(input$Mean!="None" && !input$forcemeanshape)    {
              
              if(input$meanpoints && input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",
                              col=meancolp,
                              alpha=input$alphameanp,
                              position = eval(parse(text=positionmean)))
              
              if(input$meanpoints && input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",
                              col=meancolp,
                              size=input$meanpointsize,
                              alpha=input$alphameanp,
                              position = eval(parse(text=positionmean)))
              
            }
            if(input$Mean!="None" && input$forcemeanshape)    {
              
              if(input$meanpoints && input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",
                              col=meancolp,
                              alpha=input$alphameanp,
                              shape=input$meanshapes,
                              position = eval(parse(text=positionmean)))
              
              if(input$meanpoints && input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",
                              col=meancolp,
                              size=input$meanpointsize,
                              alpha=input$alphameanp,shape=input$meanshapes,
                              position = eval(parse(text=positionmean)))
              
            }
            
          if (input$Mean!="None" && input$meanvalues )  {
            p <-   p   +
              stat_summary(fun.data = mean.n, geom = input$geommeanlabel,
                           alpha=input$alphameanlabel,
                           fun.y = mean, fontface = "bold",
                           col=meancolp,position = eval(parse(text=positionmean)),
                           show.legend=FALSE,size=6, seed=1234)
          }
          if (input$Mean!="None" && input$meanN)  {
            p <-   p   +
              stat_summary(fun.data = give.n,  geom = input$geommeanlabel,alpha=input$alphameanlabel,
                           fun.y = mean, fontface = "bold", col=meancolp,position = eval(parse(text=positionmean)),
                           show.legend=FALSE,size=6, seed=1234)      
          }
          
        }#ignore col do not ignore group
    } # do not ignore group
      
      if (input$meanignoregroup) {
        if (!input$meanignorecol) {
          
          if (input$Mean=="Mean") {
            if(input$meanlines && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",aes(group=NULL),
                                alpha=input$alphameanl,
                                position = eval(parse(text=positionmean)))     
            if(input$meanlines && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",aes(group=NULL),size=input$meanlinesize,
                                alpha=input$alphameanl,
                                position = eval(parse(text=positionmean)))     
            
  
          } # input = mean
          
          if (input$Mean=="Mean/CI" && input$pointsizein == 'None'){

              if (input$geommeanCI== "ribbon"){
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                              fun.args=list(conf.int=input$CI), aes(group=NULL),
                              alpha=input$meancitransparency,
                              col=NA,
                              size=input$meanlinesize,
                              position = eval(parse(text=positionmean)))
              }
              if (input$geommeanCI== "errorbar"){
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = input$geommeanCI, 
                              fun.args=list(conf.int=input$CI),aes(group=NULL), 
                              alpha=input$meancitransparency,
                              size=input$meanlinesize, width = input$errbar,
                              position = eval(parse(text=positionmean)))
              }
            if(input$meanlines){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line",stat="smooth",
                            fun.args=list(conf.int=input$CI),
                            aes(group=NULL),
                            alpha=input$alphameanl,
                            size=input$meanlinesize,
                            position = eval(parse(text=positionmean))) 
              
            }
          }
          if (input$Mean=="Mean/CI" && input$pointsizein != 'None'){
            
            if (input$geommeanCI== "ribbon"){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                            fun.args=list(conf.int=input$CI), aes(group=NULL),
                            alpha=input$meancitransparency,
                            col=NA,
                            position = eval(parse(text=positionmean)))
            }
            if (input$geommeanCI== "errorbar"){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = input$geommeanCI, 
                            fun.args=list(conf.int=input$CI),aes(group=NULL), 
                            alpha=input$meancitransparency,
                            width = input$errbar,
                            position = eval(parse(text=positionmean)))
            }
            if(input$meanlines){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line",stat="smooth",
                            fun.args=list(conf.int=input$CI),
                            aes(group=NULL),
                            alpha=input$alphameanl,
                            position = eval(parse(text=positionmean))) 
              
            }
          }


            if(input$Mean!="None" && !input$forcemeanshape)    {
              if(input$meanpoints && input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",aes(group=NULL),
                              alpha=input$alphameanp,
                              position = eval(parse(text=positionmean)))
              if(input$meanpoints && input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",aes(group=NULL),
                              size=input$meanpointsize,
                              alpha=input$alphameanp,
                              position = eval(parse(text=positionmean)))
              
            }
            if(input$Mean!="None" && input$forcemeanshape)    {
              if(input$meanpoints && input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",aes(group=NULL),
                              alpha=input$alphameanp,shape=input$meanshapes,
                              position = eval(parse(text=positionmean)))
              if(input$meanpoints&input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",aes(group=NULL),
                              size=input$meanpointsize,
                              alpha=input$alphameanp,shape=input$meanshapes,
                              position = eval(parse(text=positionmean)))
              
            }

          if (input$Mean!="None" && input$meanvalues )  {
            p <-   p   +
              stat_summary(fun.data = mean.n, geom = input$geommeanlabel,
                           alpha=input$alphameanlabel,
                           aes(group=NULL),
                           fun.y = mean, fontface = "bold",
                           position = eval(parse(text=positionmean)),
                           show.legend=FALSE,size=6, seed=1234)
          }
          if (input$Mean!="None" && input$meanN)  {
            p <-   p   +
              stat_summary(fun.data = give.n,  geom = input$geommeanlabel,
                           alpha=input$alphameanlabel,
                           aes(group=NULL),
                           fun.y = mean, fontface = "bold",
                           position = eval(parse(text=positionmean)),
                           show.legend=FALSE,size=6, seed=1234)      
          }
          
        }# do not ignore color and ignore group
        
        
        if (input$meanignorecol) {
          meancoll <- input$colmeanl
          meancolp <- input$colmeanp
          
          if (input$Mean=="Mean") {
            if(input$meanlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",col=meancoll,aes(group=NULL),
                                alpha=input$alphameanl,
                                position = eval(parse(text=positionmean)))
            if(input$meanlines && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",col=meancoll,aes(group=NULL),
                                size=input$meanlinesize,alpha=input$alphameanl,
                                position = eval(parse(text=positionmean)))

          } # selected input mean
          

          if(input$Mean=="Mean/CI" && input$pointsizein != 'None') {
            
            if (input$geommeanCI== "ribbon"){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                            fun.args=list(conf.int=input$CI), 
                            alpha=input$meancitransparency,
                            col=NA,aes(group=NULL),
                            position = eval(parse(text=positionmean)))
            }
            if (input$geommeanCI== "errorbar"){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                            fun.args=list(conf.int=input$CI), width = input$errbar,
                            alpha=input$meancitransparency,
                            col=meancoll,aes(group=NULL),
                            position = eval(parse(text=positionmean)))
            }
            if(input$meanlines) {
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line", stat ="smooth"  ,
                            fun.args=list(conf.int=input$CI),
                            alpha=input$alphameanl,
                            col=meancoll,aes(group=NULL),
                            position = eval(parse(text=positionmean)))
              
            }
            
          }
          
          if(input$Mean=="Mean/CI" && input$pointsizein == 'None') {
            if (input$geommeanCI== "ribbon"){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                            fun.args=list(conf.int=input$CI), 
                            size=input$meanlinesize,
                            alpha=input$meancitransparency,
                            col=NA,aes(group=NULL),
                            position = eval(parse(text=positionmean)))
            }
            if (input$geommeanCI== "errorbar"){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = input$geommeanCI,
                            fun.args=list(conf.int=input$CI), width = input$errbar,
                            col=meancoll,aes(group=NULL),
                            size=input$meanlinesize,
                            alpha=input$meancitransparency,
                            position = eval(parse(text=positionmean)))
            }
            if(input$meanlines) {
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line", stat ="smooth"  ,
                            fun.args=list(conf.int=input$CI),
                            alpha=input$alphameanl,
                            col=meancoll,aes(group=NULL),
                            size=input$meanlinesize,
                            position = eval(parse(text=positionmean)))
              
            }
            
          }

            
            if(input$Mean!="None" && !input$forcemeanshape)    {
              
              if(input$meanpoints && input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",col=meancolp,aes(group=NULL),
                              alpha=input$alphameanp,
                              position = eval(parse(text=positionmean)))
              if(input$meanpoints & input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",col=meancolp,aes(group=NULL),
                              size=input$meanpointsize,alpha=input$alphameanp,
                              position = eval(parse(text=positionmean)))
            }
            
            if(input$Mean!="None" && input$forcemeanshape)    {
              
              if(input$meanpoints &input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",col=meancolp,aes(group=NULL),
                              alpha=input$alphameanp,shape=input$meanshapes,
                              position = eval(parse(text=positionmean)))
              if(input$meanpoints && input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",col=meancolp,aes(group=NULL),
                              size=input$meanpointsize,alpha=input$alphameanp,shape=input$meanshapes,
                              position = eval(parse(text=positionmean)))
            }

          if (input$Mean!="None" && input$meanvalues )  {
            p <-   p   +
              stat_summary(fun.data = mean.n, geom = input$geommeanlabel,alpha=input$alphameanlabel,
                           col=meancolp,aes(group=NULL),
                           fun.y = mean, fontface = "bold",
                           position = eval(parse(text=positionmean)),
                           show.legend=FALSE,size=6, seed=1234)
          }
          if (input$Mean!="None" && input$meanN)  {
            p <-   p   +
              stat_summary(fun.data = give.n,  geom = input$geommeanlabel,alpha=input$alphameanlabel,
                           col=meancolp,aes(group=NULL),
                           fun.y = mean, fontface = "bold",
                           position = eval(parse(text=positionmean)),
                           show.legend=FALSE,size=6, seed=1234)      
          }
          
        } #input$meanignorecol
      }#input$meanignoregroup
      ###### Mean section  END 
      
      ###### Smoothing Section START
      if(input$Smooth!="None"){
        smoothlinesize  <- input$smoothlinesize
        smoothlinealpha <- input$smoothlinealpha
        smoothCItransparency <- input$smoothCItransparency
        
        if(input$smoothmethod=="loess") {
          familyargument <- input$loessfamily
          methodsargument<- list(family = familyargument,degree=input$loessdegree) 
        }
        
        if(input$smoothmethod=="lm") {
          familyargument<- "gaussian"
          methodsargument<- list(family = familyargument) 
        }
        
        if(input$smoothmethod=="glm1") {
          familyargument<- "binomial" 
          methodsargument<- list(family = familyargument) 
          
        }
        if(input$smoothmethod=="glm2") {
          familyargument<- "poisson"
          methodsargument<- list(family = familyargument) 
        }
        
        if(input$smoothmethod=="emax" ) {
          
          if(!input$customemaxstart && !input$e0fit)  methodsargument <- list(formula = y ~ SSmicmen(x, Vm, K))
          if( input$customemaxstart && !input$e0fit)  methodsargument <- list(formula = y ~ SSmicmen(x, Vm, K),
                                                                              start=c(Vm =input$emaxstart , K =input$ec50start))
          if( input$customemaxstart &&  input$e0fit)  methodsargument <- list(formula = y ~ bsl + (Vm *x / (K+x)) ,
                                                                             start=c(Vm = input$emaxstart ,
                                                                                      K = input$ec50start,
                                                                                     bsl= input$e0start))
          if(!input$customemaxstart &&  input$e0fit)   methodsargument<- NULL
          
        }
  

        smoothmethodargument<- ifelse(input$smoothmethod%in%c("glm1","glm2"),
                                      "glm",input$smoothmethod)
        spanplot <- input$loessens
        levelsmooth<- input$smoothselevel
        colsmooth <- input$colsmooth
        if (input$weightin == 'None') aesweight <- 1L
        if (input$weightin != 'None') aesweight <- as.symbol(input$weightin)
        
        if ( input$ignoregroup) {
          if (!input$smoothignorecol && !input$smoothmethod=="emax") {
            if (input$Smooth=="Smooth"){
              p <- p + geom_line(stat="smooth",alpha=smoothlinealpha,
                                 method=smoothmethodargument,
                                 method.args = methodsargument,
                                 size=smoothlinesize,se=F,span=spanplot,aes(group=NULL,weight=!!aesweight))
            }
            
            if (input$Smooth=="Smooth and SE"){
              p <- p + 
                geom_ribbon(stat="smooth",alpha=smoothCItransparency,linetype=0,
                            method=smoothmethodargument,level=levelsmooth,
                            method.args = methodsargument,
                            size=smoothlinesize,se=T,span=spanplot,aes(group=NULL,weight=!!aesweight))+
                geom_line(stat="smooth",alpha=smoothlinealpha,
                          method=smoothmethodargument,level=levelsmooth,
                          method.args = methodsargument,
                          size=smoothlinesize,se=T,span=spanplot,aes(group=NULL,weight=!!aesweight))
            }
             if (input$smoothmethod=="lm"&&input$showadjrsquared){
               p <- p+
                ggpmisc::stat_fit_glance(method = "lm", 
                                         method.args = list(formula = y ~ x , weights = quote(weight)),
                                         geom = "text_repel",segment.color=NA,direction="y",
                                         label.x=-Inf ,label.y=-Inf,size=input$smoothtextsize,
                                         aes(label = paste("R[adj]^2==",
                                                           signif(..adj.r.squared.., digits = 2), sep = ""),
                                             group=NULL,weight=!!aesweight),
                                         show.legend = FALSE,parse=TRUE)


            }
            if (input$smoothmethod=="lm"&&input$showslopepvalue){
               p <- p+
                ggpmisc::stat_fit_glance(method = "lm", force = 3,
                                         method.args = list(formula = y ~ x , weights = quote(weight)),
                                         geom = "text_repel",segment.color=NA,direction="y",
                                         label.x=-Inf ,label.y=Inf,size=input$smoothtextsize,
                                         aes(label = paste("Slope P-value = ",
                                                           signif(..p.value.., digits = 3), sep = ""),
                                             group=NULL,weight=!!aesweight),
                                         show.legend = FALSE)
            }
            if (input$smoothmethod=="lm" && input$showlmequation){
              p <- p+ ggpmisc::stat_fit_tidy(method = "lm",
                                         method.args = list(formula = y ~ x, weights = quote(weight)),
                                         geom = "text_repel",segment.color=NA,direction="y",
                                         label.x = Inf ,label.y = -Inf,size=input$smoothtextsize,
                                         aes(label = paste("Intercept~`=`~", signif(..x_estimate.., digits = 3),
                                                                   "%+-%", signif(..x_se.., digits = 2),
                                                                   "~Slope~`=`~", signif(..Intercept_estimate.., digits = 3),
                                                                   "%+-%", signif(..Intercept_se.., digits = 2),
                                                                   sep = ""),
                                             group=NULL,weight=!!aesweight),
                                       parse = TRUE, show.legend = FALSE)
            }
            

            
          }#!input$smoothignorecol && !input$smoothmethod=="emax"
          
          if (!input$smoothignorecol&& input$smoothmethod=="emax") {

              p <- p + geom_line(stat="smooth",alpha=smoothlinealpha,
                                 method='nls',
                                 method.args = methodsargument,
                                 size=smoothlinesize,se=F,aes(group=NULL,weight=!!aesweight))
              
              if(input$shownlsparams && !input$e0fit){
                p <- p +ggpmisc::stat_fit_tidy(method = "nls",size=input$smoothtextsize, 
                                         method.args = c(methodsargument,weights =quote(weight)),
                                         label.x = "right",
                                         label.y = "bottom",
                                         aes(label = paste("E[max]~`=`~", signif(..Vm_estimate.., digits = 3),
                                                           "%+-%", signif(..Vm_se.., digits = 2),
                                                           "~~~EC[50]~`=`~", signif(..K_estimate.., digits = 3),
                                                           "%+-%", signif(..K_se.., digits = 2),
                                                           sep = ""),
                                             group=NULL,weight=!!aesweight),
                                         parse = TRUE)
              }
              
              if(input$shownlsparams && input$e0fit){
                p <- p +ggpmisc::stat_fit_tidy(method = "nls",size=input$smoothtextsize, 
                                               method.args = c(methodsargument,weights =quote(weight)),
                                               label.x = "right",
                                               label.y = "bottom",
                                               aes(label = paste("E[0]~`=`~", signif(..bsl_estimate.., digits = 3),
                                                                 "%+-%"       , signif(..bsl_se.., digits = 2),
                                                                 "~~~E[max]~`=`~", signif(..Vm_estimate.., digits = 3),
                                                                 "%+-%"       , signif(..Vm_se.., digits = 2),
                                                                 "~~~EC[50]~`=`~", signif(..K_estimate.., digits = 3),
                                                                 "%+-%"       , signif(..K_se.., digits = 2),
                                                                 sep = ""),
                                                   group=NULL,weight=!!aesweight),
                                               parse = TRUE)
              }
      
            }#!input$smoothignorecol&& input$smoothmethod=="emax"
          
          if (input$smoothignorecol && !input$smoothmethod=="emax") {
            
            if (input$Smooth=="Smooth")
              p <- p +  geom_line(stat="smooth",alpha=smoothlinealpha,
                                  method=smoothmethodargument,
                                  method.args = methodsargument,
                                  size=smoothlinesize,se=F,span=spanplot,col=colsmooth,aes(group=NULL,weight=!!aesweight))
            
            if (input$Smooth=="Smooth and SE")
              p <- p + geom_ribbon(stat="smooth",alpha=smoothCItransparency,linetype=0,
                                   method=smoothmethodargument,level=levelsmooth,
                                   method.args = methodsargument,
                                   size=smoothlinesize,se=T,span=spanplot,col=colsmooth,aes(group=NULL,weight=!!aesweight))+
                geom_line(stat="smooth",alpha=smoothlinealpha,
                          method=smoothmethodargument,level=levelsmooth,
                          method.args = methodsargument,
                          size=smoothlinesize,se=T,span=spanplot,col=colsmooth,aes(group=NULL,weight=!!aesweight))

            if (input$smoothmethod=="lm"&&input$showadjrsquared){
              p <- p+
                ggpmisc::stat_fit_glance(method = "lm",col=colsmooth,
                                         method.args = list(formula = y ~ x , weights = quote(weight)),
                                         geom = "text_repel",segment.color=NA,direction="y",
                                         label.x=-Inf ,label.y=-Inf,size=input$smoothtextsize,
                                         aes(label = paste("R[adj]^2==",
                                                           signif(..adj.r.squared.., digits = 2), sep = ""),
                                             group=NULL,weight=!!aesweight),
                                         show.legend = FALSE,parse=TRUE)


            }
            if (input$smoothmethod=="lm"&&input$showslopepvalue){
              p <- p+
                ggpmisc::stat_fit_glance(method = "lm", col=colsmooth,force = 3,
                                         method.args = list(formula = y ~ x , weights = quote(weight)),
                                         geom = "text_repel",segment.color=NA,direction="y",
                                         label.x=-Inf ,label.y=Inf,size=input$smoothtextsize,
                                         aes(label = paste("Slope P-value = ",
                                                           signif(..p.value.., digits = 3), sep = ""),
                                             group=NULL,weight=!!aesweight),
                                         show.legend = FALSE)
            }
            if (input$smoothmethod=="lm" && input$showlmequation){
              p <- p+ ggpmisc::stat_fit_tidy(method = "lm",col=colsmooth,size=input$smoothtextsize,
                                             method.args = list(formula = y ~ x, weights = quote(weight)),
                                             geom = "text_repel",segment.color=NA,direction="y",
                                             label.x = Inf ,label.y = -Inf,size=input$smoothtextsize,
                                             aes(label = paste("Intercept~`=`~", signif(..x_estimate.., digits = 3),
                                                               "%+-%", signif(..x_se.., digits = 2),
                                                               "~Slope~`=`~", signif(..Intercept_estimate.., digits = 3),
                                                               "%+-%", signif(..Intercept_se.., digits = 2),
                                                               sep = ""),
                                                 group=NULL,weight=!!aesweight),
                                             parse = TRUE, show.legend = FALSE)
            }
            
          }#input$smoothignorecol && !input$smoothmethod=="emax"

          if (input$smoothignorecol&& input$smoothmethod=="emax") {

              p <- p + geom_line(stat="smooth",alpha=smoothlinealpha,
                                 method='nls',
                                 method.args = methodsargument,
                                 size=smoothlinesize,se=F,col=colsmooth,aes(group=NULL,weight=!!aesweight))
              
              if(input$shownlsparams && !input$e0fit){
                p <- p +ggpmisc::stat_fit_tidy(method = "nls", size=input$smoothtextsize, col=colsmooth,
                                               method.args = c(methodsargument,weights =quote(weight)),
                                               label.x = "right",
                                               label.y = "bottom",
                                               aes(label = paste("E[max]~`=`~", signif(..Vm_estimate.., digits = 3),
                                                                 "%+-%", signif(..Vm_se.., digits = 2),
                                                                 "~~~EC[50]~`=`~", signif(..K_estimate.., digits = 3),
                                                                 "%+-%", signif(..K_se.., digits = 2),
                                                                 sep = ""),
                                                   group=NULL,weight=!!aesweight),
                                               parse = TRUE)
              }
              
              if(input$shownlsparams && input$e0fit){
                p <- p +ggpmisc::stat_fit_tidy(method = "nls", size=input$smoothtextsize, col=colsmooth,
                                               method.args = c(methodsargument,weights =quote(weight)),
                                               label.x = "right",
                                               label.y = "bottom",
                                               aes(label = paste("E[0]~`=`~", signif(..bsl_estimate.., digits = 3),
                                                                 "%+-%"       , signif(..bsl_se.., digits = 2),
                                                                 "~~~E[max]~`=`~", signif(..Vm_estimate.., digits = 3),
                                                                 "%+-%"       , signif(..Vm_se.., digits = 2),
                                                                 "~~~EC[50]~`=`~", signif(..K_estimate.., digits = 3),
                                                                 "%+-%"       , signif(..K_se.., digits = 2),
                                                                 sep = ""),
                                                   group=NULL,weight=!!aesweight),
                                               parse = TRUE)

              }

          }#input$smoothignorecol && !input$smoothmethod=="emax"
          
        }#smooth ignore group
        
        if ( !input$ignoregroup) {
          if (!input$smoothignorecol&& !input$smoothmethod=="emax") {
            if (input$Smooth=="Smooth" )
              p <- p +  geom_line(aes(weight=!!aesweight),stat="smooth",alpha=smoothlinealpha,
                                  method=smoothmethodargument,
                                  method.args = methodsargument,
                                  size=smoothlinesize,se=F,span=spanplot)
            
            if (input$Smooth=="Smooth and SE")
              p <- p + geom_ribbon(aes(weight=!!aesweight),stat="smooth",alpha=smoothCItransparency,linetype=0,
                                   method=smoothmethodargument,level=levelsmooth,
                                   method.args = methodsargument,
                                   size=smoothlinesize,se=T,span=spanplot)+  
                geom_line(aes(weight=!!aesweight),stat="smooth",alpha=smoothlinealpha,
                          method=smoothmethodargument,level=levelsmooth,
                          method.args = methodsargument,
                          size=smoothlinesize,se=T,span=spanplot)
        
          
            if (input$smoothmethod=="lm"&&input$showadjrsquared){
              p <- p+
                ggpmisc::stat_fit_glance(method = "lm",
                                         method.args = list(formula = y ~ x , weights = quote(weight)),
                                         geom = "text_repel",segment.color=NA,direction="y",
                                         label.x=-Inf ,label.y=-Inf,size=input$smoothtextsize,
                                         aes(label = paste("R[adj]^2==",
                                                           signif(..adj.r.squared.., digits = 2), sep = ""),
                                             weight=!!aesweight),
                                         show.legend = FALSE,parse=TRUE)
              
              
            }
            if (input$smoothmethod=="lm"&&input$showslopepvalue){
              p <- p+
                ggpmisc::stat_fit_glance(method = "lm", force = 3,
                                         method.args = list(formula = y ~ x , weights = quote(weight)),
                                         geom = "text_repel",segment.color=NA,direction="y",
                                         label.x=-Inf ,label.y=Inf,size=input$smoothtextsize,
                                         aes(label = paste("Slope P-value = ",
                                                           signif(..p.value.., digits = 3), sep = ""),
                                             weight=!!aesweight),
                                         show.legend = FALSE)
            }
            
            if (input$smoothmethod=="lm" && input$showlmequation){
              p <- p+ ggpmisc::stat_fit_tidy(method = "lm",
                                             method.args = list(formula = y ~ x, weights = quote(weight)),
                                             geom = "text_repel",segment.color=NA,direction="y",
                                             label.x = Inf ,label.y = -Inf,size=input$smoothtextsize,
                                             aes(label = paste("Intercept~`=`~", signif(..x_estimate.., digits = 3),
                                                               "%+-%", signif(..x_se.., digits = 2),
                                                               "~Slope~`=`~", signif(..Intercept_estimate.., digits = 3),
                                                               "%+-%", signif(..Intercept_se.., digits = 2),
                                                               sep = ""),
                                                 weight=!!aesweight),
                                             parse = TRUE, show.legend = FALSE)
            }
            
            
            
          }
          if (!input$smoothignorecol&& input$smoothmethod=="emax") {
           
              p <- p + geom_line(aes(weight=!!aesweight), stat="smooth",alpha=smoothlinealpha,
                                 method='nls',
                                 method.args = methodsargument,
                                 size=smoothlinesize,se=F)
              
              if(input$shownlsparams && !input$e0fit){
                p <- p +
                  ggpmisc::stat_fit_tidy(method = "nls", size=input$smoothtextsize, 
                                         method.args = c(methodsargument,weights =quote(weight)),
                                         label.x = "right",
                                         label.y = "bottom",
                                         aes(label = paste("E[max]~`=`~", signif(..Vm_estimate.., digits = 3),
                                                           "%+-%", signif(..Vm_se.., digits = 2),
                                                           "~~~EC[50]~`=`~", signif(..K_estimate.., digits = 3),
                                                           "%+-%", signif(..K_se.., digits = 2),
                                                           sep = ""),
                                             weight=!!aesweight),
                                         parse = TRUE)
              }
              if(input$shownlsparams && input$e0fit){
                p <- p +
                  ggpmisc::stat_fit_tidy(method = "nls", size=input$smoothtextsize, 
                                         method.args = c(methodsargument,weights =quote(weight)),
                                         label.x = "right",
                                         label.y = "bottom",
                                         aes(label = paste("E[0]~`=`~", signif(..bsl_estimate.., digits = 3),
                                                           "%+-%"       , signif(..bsl_se.., digits = 2),
                                                           "~~~E[max]~`=`~", signif(..Vm_estimate.., digits = 3),
                                                           "%+-%"       , signif(..Vm_se.., digits = 2),
                                                           "~~~EC[50]~`=`~", signif(..K_estimate.., digits = 3),
                                                           "%+-%"       , signif(..K_se.., digits = 2),
                                                           sep = ""),
                                             weight=!!aesweight),
                                         parse = TRUE)
              }
              
              

            }
          
          if (input$smoothignorecol&& !input$smoothmethod=="emax") {
            if (input$Smooth=="Smooth" )
              p <- p +  geom_line(aes(weight=!!aesweight),stat="smooth",alpha=smoothlinealpha,
                                  method=smoothmethodargument,
                                  method.args = methodsargument,
                                  size=smoothlinesize,se=F,span=spanplot,col=colsmooth)
            
            if (input$Smooth=="Smooth and SE" )
              p <- p + geom_ribbon(aes(weight=!!aesweight),stat="smooth",alpha=smoothCItransparency,linetype=0,
                                   method=smoothmethodargument,level=levelsmooth,
                                   method.args = methodsargument,
                                   size=smoothlinesize,se=T,span=spanplot,col=colsmooth)+
                geom_line(aes(weight=!!aesweight),stat="smooth",alpha=smoothlinealpha,
                          method=smoothmethodargument,level=levelsmooth,
                          method.args = methodsargument,
                          size=smoothlinesize,se=T,span=spanplot,col=colsmooth)
            

            if (input$smoothmethod=="lm"&&input$showadjrsquared){
              p <- p+
                ggpmisc::stat_fit_glance(method = "lm",col=colsmooth,
                                         method.args = list(formula = y ~ x , weights = quote(weight)),
                                         geom = "text_repel",segment.color=NA,direction="y",
                                         label.x=-Inf ,label.y=-Inf, size=input$smoothtextsize,
                                         aes(label = paste("R[adj]^2==",
                                                           signif(..adj.r.squared.., digits = 2), sep = ""),
                                             weight=!!aesweight),
                                         show.legend = FALSE,parse=TRUE)
              
              
            }
            if (input$smoothmethod=="lm"&&input$showslopepvalue){
              p <- p+
                ggpmisc::stat_fit_glance(method = "lm",col=colsmooth, force = 3,
                                         method.args = list(formula = y ~ x , weights = quote(weight)),
                                         geom = "text_repel",segment.color=NA,direction="y",
                                         label.x=-Inf ,label.y=Inf, size=input$smoothtextsize,
                                         aes(label = paste("Slope P-value = ",
                                                           signif(..p.value.., digits = 3), sep = ""),
                                             weight=!!aesweight),
                                         show.legend = FALSE)
            }
            
            if (input$smoothmethod=="lm" && input$showlmequation){
              p <- p+ ggpmisc::stat_fit_tidy(method = "lm",col=colsmooth,
                                             method.args = list(formula = y ~ x, weights = quote(weight)),
                                             geom = "text_repel",segment.color=NA,direction="y",
                                             label.x = Inf ,label.y = -Inf, size=input$smoothtextsize,
                                             aes(label = paste("Intercept~`=`~", signif(..x_estimate.., digits = 3),
                                                               "%+-%", signif(..x_se.., digits = 2),
                                                               "~Slope~`=`~", signif(..Intercept_estimate.., digits = 3),
                                                               "%+-%", signif(..Intercept_se.., digits = 2),
                                                               sep = ""),weight=!!aesweight),
                                             parse = TRUE, show.legend = FALSE)
            }
            
          }
          if (input$smoothignorecol && input$smoothmethod=="emax") {
          
              p <- p + geom_line(aes(weight=!!aesweight),stat="smooth",alpha=smoothlinealpha,
                                 method='nls',
                                 method.args = methodsargument,
                                 size=smoothlinesize,se=F,col=colsmooth)
              
              if(input$shownlsparams && !input$e0fit){
                p <- p +ggpmisc::stat_fit_tidy(method = "nls", size=input$smoothtextsize, col=colsmooth,
                                               method.args = c(methodsargument,weights =quote(weight)),
                                               label.x = "right",
                                               label.y = "bottom",
                                               aes(label = paste("E[max]~`=`~", signif(..Vm_estimate.., digits = 3),
                                                                 "%+-%", signif(..Vm_se.., digits = 2),
                                                                 "~~~EC[50]~`=`~", signif(..K_estimate.., digits = 3),
                                                                 "%+-%", signif(..K_se.., digits = 2),
                                                                 sep = ""),
                                                   weight=!!aesweight),
                                               parse = TRUE)
              }
              
              if(input$shownlsparams && input$e0fit){
                p <- p +ggpmisc::stat_fit_tidy(method = "nls", size=input$smoothtextsize, col=colsmooth,
                                               method.args = c(methodsargument,weights =quote(weight)),
                                               label.x = "right",
                                               label.y = "bottom",
                                               aes(label = paste("E[0]~`=`~", signif(..bsl_estimate.., digits = 3),
                                                                 "%+-%"       , signif(..bsl_se.., digits = 2),
                                                                 "~~~E[max]~`=`~", signif(..Vm_estimate.., digits = 3),
                                                                 "%+-%"       , signif(..Vm_se.., digits = 2),
                                                                 "~~~EC[50]~`=`~", signif(..K_estimate.., digits = 3),
                                                                 "%+-%"       , signif(..K_se.., digits = 2),
                                                                 sep = ""),
                                                   weight=!!aesweight),
                                               parse = TRUE)
              }
            
}
          }#smooth ignore group

      }# if smooth not none
      ###### smooth Section END
      
      
      ###### Median PI section  START
      if (input$Median!="None") {
      if (input$positionmedian=="position_identity"){
        positionmedian<-  "position_identity()"
      }
      if (input$positionmedian=="position_dodge"){
        positionmedian<-  paste0("position_dodge(width=",input$medianerrbar,")")
      }
      p <- attach_source_dep(p, "positionmedian")
      }
      if (!input$medianignoregroup) {
        
        if (!input$medianignorecol) {
          
          if (input$Median=="Median") {
            
            if(input$medianlines && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",
                                alpha=input$alphamedianl,
                                position = eval(parse(text=positionmedian)))
            
            if(input$medianlines && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",
                                size=input$medianlinesize,
                                alpha=input$alphamedianl,
                                position = eval(parse(text=positionmedian)))
            
            } #input$Median=="Median"
            
          
          if (input$Median=="Median/PI" && input$pointsizein == 'None'){
            
            if (input$geommedianPI== "ribbon"){
               p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,
                            fun.args=list(conf.int=input$PI), 
                            size=input$medianlinesize,
                            alpha=input$PItransparency,
                            col=NA,
                            position = eval(parse(text=positionmedian)))
            }
            if (input$geommedianPI== "errorbar"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,
                            fun.args=list(conf.int=input$PI), width = input$medianerrbar,
                            size=input$medianlinesize, alpha=input$PItransparency,
                            position = eval(parse(text=positionmedian)))
            }
            if(input$medianlines) {
              p <- p + stat_sum_df("median_hilow", geom = "line", stat ="smooth",
                                   fun.args=list(conf.int=input$PI),
                                   size=input$medianlinesize,
                                   alpha=input$alphamedianl,
                                   position = eval(parse(text=positionmedian)))
              
            }
            
            if ( input$sepguides ){
              
              p <-   p + 
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
            }

            
          }
          
          if (input$Median=="Median/PI" && input$pointsizein != 'None'){

            if (input$geommedianPI== "ribbon"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,
                            fun.args=list(conf.int=input$PI),
                            alpha=input$PItransparency,col=NA,
                            position = eval(parse(text=positionmedian)))
            }
            if (input$geommedianPI== "errorbar"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,
                            fun.args=list(conf.int=input$PI), width = input$medianerrbar,
                            alpha=input$PItransparency,
                            position = eval(parse(text=positionmedian)))
            }
            if(input$medianlines) {
              
            p <- p +    stat_sum_df("median_hilow", geom = "line", stat ="smooth"  ,
                        fun.args=list(conf.int=input$PI),alpha=input$alphamedianl,
                        position = eval(parse(text=positionmedian)))
            }
            if ( input$sepguides ){
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
            }

            
          }
          
          if(input$Median!="None" && !input$forcemedianshape)    {
            
            if(input$medianpoints && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",alpha=input$alphamedianp,
                                position = eval(parse(text=positionmedian)))
            
            if(input$medianpoints && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",size=input$medianpointsize,
                                alpha=input$alphamedianp,
                                position = eval(parse(text=positionmedian)))
          }
          
          if(input$Median!="None" && input$forcemedianshape)    {
            if(input$medianpoints && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",alpha=input$alphamedianp,shape=input$medianshapes,
                                position = eval(parse(text=positionmedian)))
            
            if(input$medianpoints && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",
                                size=input$medianpointsize,
                                alpha=input$alphamedianp,
                                shape=input$medianshapes,
                                position = eval(parse(text=positionmedian)))
          }
          
          if (input$Median!="None" && input$medianvalues )  {
            p <-   p   +
              stat_summary(fun.data = median.n,geom = input$geommedianlabel,alpha=input$alphamedianlabel,
                           fun.y = median, fontface = "bold",position = eval(parse(text=positionmedian)),
                           show.legend=FALSE,size=6, seed=1234)
          }
          if (input$Median!="None" && input$medianN)  {
            p <-   p   +
              stat_summary(fun.data = give.n, geom = input$geommedianlabel,alpha=input$alphamedianlabel,
                           fun.y = median, fontface = "bold", position = eval(parse(text=positionmedian)),
                           show.legend=FALSE,size=6, seed=1234)      
          }  
        } # do not ignore col
 
        if (input$medianignorecol) {
          mediancoll <- input$colmedianl
          mediancolp <- input$colmedianp
          
          if (input$Median=="Median") {
            if(input$medianlines && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",
                                col=mediancoll,
                                alpha=input$alphamedianl,
                                position = eval(parse(text=positionmedian)))
            
            if(input$medianlines && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",col=mediancoll,
                                alpha=input$alphamedianl,
                                size=input$medianlinesize,
                                position = eval(parse(text=positionmedian)))

          }
          
          if (input$Median=="Median/PI" && input$pointsizein == 'None'){

            if (input$geommedianPI== "ribbon"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,
                            fun.args=list(conf.int=input$PI),
                            alpha=input$PItransparency,col=NA,
                            size=input$medianlinesize,
                            position = eval(parse(text=positionmedian)))
            }
            if (input$geommedianPI== "errorbar"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,
                            fun.args=list(conf.int=input$PI), width = input$medianerrbar,
                            alpha=input$PItransparency,
                            col=mediancoll,
                            size=input$medianlinesize,
                            position = eval(parse(text=positionmedian)))
            }
            if(input$medianlines){
              p <- p +    stat_sum_df("median_hilow", geom = "line", stat ="smooth"  ,
                                      fun.args=list(conf.int=input$PI),
                                      size=input$medianlinesize,
                                      alpha=input$alphamedianl,
                                      col=mediancoll,
                                      position = eval(parse(text=positionmedian)))
              
            }
              
            if ( input$sepguides ){
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) ) 
            }

            }
          
          if (input$Median=="Median/PI" && input$pointsizein != 'None'){

            if (input$geommedianPI== "ribbon"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,
                            fun.args=list(conf.int=input$PI),
                            alpha=input$PItransparency,col=NA,
                            position = eval(parse(text=positionmedian)))
            }
            if (input$geommedianPI== "errorbar"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,
                            fun.args=list(conf.int=input$PI), width = input$medianerrbar,
                            alpha=input$PItransparency,
                            col=mediancoll,
                            position = eval(parse(text=positionmedian)))
            }
            if(input$medianlines){
            p <- p +    stat_sum_df("median_hilow", geom = "line", stat ="smooth"  ,
                                    fun.args=list(conf.int=input$PI),
                                    alpha=input$alphamedianl,
                                    col=mediancoll,
                                    position = eval(parse(text=positionmedian)))
            }
            
            if ( input$sepguides ){
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) ) 
            }

          }
          
          
          if(input$Median!="None" && !input$forcemedianshape)    {
            if(input$medianpoints&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",
                                col=mediancolp,
                                alpha=input$alphamedianp ,
                                position = eval(parse(text=positionmedian)))
            
            if(input$medianpoints&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",
                                col=mediancolp ,
                                alpha=input$alphamedianp,
                                size=input$medianpointsize,
                                position = eval(parse(text=positionmedian)))
          }
          
          if(input$Median!="None" && input$forcemedianshape)    {
            
            if(input$medianpoints && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",
                                col=mediancolp,
                                alpha=input$alphamedianp,
                                shape=input$medianshapes ,
                                position = eval(parse(text=positionmedian)))
            
            if(input$medianpoints && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",col=mediancolp ,
                                alpha=input$alphamedianp,
                                size=input$medianpointsize,shape=input$medianshapes,
                                position = eval(parse(text=positionmedian)))
          }
          
          if (input$Median!="None" && input$medianvalues )  {
            p <-   p   +
              stat_summary(fun.data = median.n,geom = input$geommedianlabel,alpha=input$alphamedianlabel,
                           fun.y = median, fontface = "bold",colour=mediancoll,position = eval(parse(text=positionmedian)),
                           show.legend=FALSE,size=6, seed=1234)
          }
          if (input$Median!="None" && input$medianN)  {
            p <-   p   +
              stat_summary(fun.data = give.n, geom = input$geommedianlabel,alpha=input$alphamedianlabel,
                           fun.y = median, fontface = "bold", colour=mediancolp,position = eval(parse(text=positionmedian)),
                           show.legend=FALSE,size=6, seed=1234)      
          }       
          
        }# ignore col
      }# do not ignore group
      
      if (input$medianignoregroup) {
        if (!input$medianignorecol) {
          if (input$Median=="Median") {
            
            if(input$medianlines && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",aes(group=NULL),alpha=input$alphamedianl,
                                position = eval(parse(text=positionmedian)))
            
            if(input$medianlines && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",aes(group=NULL),size=input$medianlinesize,
                                alpha=input$alphamedianl,
                                position = eval(parse(text=positionmedian)))
           }
          
          if (input$Median=="Median/PI" && input$pointsizein == 'None'){

            if (input$geommedianPI== "ribbon"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,
                            fun.args=list(conf.int=input$PI),aes(group=NULL),
                            alpha=input$PItransparency,col=NA,
                            size=input$medianlinesize,
                            position = eval(parse(text=positionmedian)))
            }
            if (input$geommedianPI== "errorbar"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,aes(group=NULL), 
                            fun.args=list(conf.int=input$PI), width = input$medianerrbar,
                            size=input$medianlinesize,
                            alpha=input$PItransparency,
                            position = eval(parse(text=positionmedian)))
            }
            if(input$medianlines){
              p <- p +    stat_sum_df("median_hilow", geom = "line", stat ="smooth"  ,
                                      fun.args=list(conf.int=input$PI),aes(group=NULL),
                                      alpha=input$alphamedianl,
                                      size=input$medianlinesize,
                                      position = eval(parse(text=positionmedian))) 
            }

            if ( input$sepguides ){
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )  
            }

          }
          
          if (input$Median=="Median/PI" && input$pointsizein != 'None'){
            if (input$geommedianPI== "ribbon"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,
                            fun.args=list(conf.int=input$PI),aes(group=NULL),
                            alpha=input$PItransparency,col=NA,
                            position = eval(parse(text=positionmedian)))
            }
            if (input$geommedianPI== "errorbar"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,aes(group=NULL), 
                            fun.args=list(conf.int=input$PI), width = input$medianerrbar,
                            alpha=input$PItransparency,
                            position = eval(parse(text=positionmedian)))
            }
            if(input$medianlines){
            p <- p +    stat_sum_df("median_hilow", geom = "line", stat ="smooth"  ,
                                    fun.args=list(conf.int=input$PI),aes(group=NULL),
                                    alpha=input$alphamedianl,
                                    position = eval(parse(text=positionmedian)))
            }
            if ( input$sepguides ){
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )  
            }
          }
          
          if(input$Median!="None" && !input$forcemedianshape)    {
            
            if(input$medianpoints && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",aes(group=NULL),alpha=input$alphamedianp,
                                position = eval(parse(text=positionmedian)))
            
            
            if(input$medianpoints && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",aes(group=NULL),
                                alpha=input$alphamedianp,
                                size=input$medianpointsize,
                                position = eval(parse(text=positionmedian)))
          }
          
          if(input$Median!="None" && input$forcemedianshape)    {
            
            if(input$medianpoints && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",aes(group=NULL),alpha=input$alphamedianp,shape=input$medianshapes,
                                position = eval(parse(text=positionmedian)))
            
            
            if(input$medianpoints && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",aes(group=NULL),
                                alpha=input$alphamedianp,
                                size=input$medianpointsize,shape=input$medianshapes,
                                position = eval(parse(text=positionmedian)))
          }
          
          if (input$Median!="None" && input$medianvalues )  {
            p <-   p   +
              stat_summary(fun.data = median.n, aes(group=NULL),geom = input$geommedianlabel,alpha=input$alphamedianlabel,
                           fun.y = median, fontface = "bold",position = eval(parse(text=positionmedian)), #fill="white",
                           show.legend=FALSE,
                           size=6, seed=1234)
          }
          if (input$Median!="None" && input$medianN)  {
            p <-   p   +
              stat_summary(fun.data = give.n, aes(group=NULL), geom = input$geommedianlabel,alpha=input$alphamedianlabel,
                           fun.y = median, fontface = "bold",position = eval(parse(text=positionmedian)), #fill="white",
                           show.legend=FALSE,size=6, seed=1234)      
          }
          
          
        }#!input$medianignorecol
        
        
        if (input$medianignorecol) {
          mediancoll <- input$colmedianl
          mediancolp <- input$colmedianp
          
          if (input$Median=="Median") {
            if(input$medianlines && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",col=mediancoll,
                                alpha=input$alphamedianl,
                                aes(group=NULL),
                                position = eval(parse(text=positionmedian)))
            if(input$medianlines && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",col=mediancoll,alpha=input$alphamedianl,
                                aes(group=NULL),size=input$medianlinesize,
                                position = eval(parse(text=positionmedian)))
            
          }
          
          if (input$Median=="Median/PI" && input$pointsizein == 'None'){

            if (input$geommedianPI== "ribbon"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,
                            fun.args=list(conf.int=input$PI),aes(group=NULL),
                            alpha=input$PItransparency,col=NA,
                            size=input$medianlinesize,
                            position = eval(parse(text=positionmedian)))
            }
            if (input$geommedianPI== "errorbar"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,aes(group=NULL), 
                            fun.args=list(conf.int=input$PI), width = input$medianerrbar,
                            alpha=input$PItransparency,
                            size=input$medianlinesize,col=mediancoll,
                            position = eval(parse(text=positionmedian)))
            }
            if(input$medianlines){
              p <- p +    stat_sum_df("median_hilow", geom = "line", stat ="smooth"  ,
                                      fun.args=list(conf.int=input$PI),aes(group=NULL),
                                      alpha=input$alphamedianl,
                                      size=input$medianlinesize,
                                      col=mediancoll,
                                      position = eval(parse(text=positionmedian))) 
              
            }

            if ( input$sepguides ){
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )  
            }

          }
          
          if (input$Median=="Median/PI" && input$pointsizein != 'None'){
            
            if (input$geommedianPI== "ribbon"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,
                            fun.args=list(conf.int=input$PI),aes(group=NULL),
                            alpha=input$PItransparency,col=NA,
                            position = eval(parse(text=positionmedian)))
            }
            if (input$geommedianPI== "errorbar"){
              p <- p + 
                stat_sum_df("median_hilow", geom = input$geommedianPI,aes(group=NULL), 
                            fun.args=list(conf.int=input$PI), width = input$medianerrbar,
                            alpha=input$PItransparency,
                            col=mediancoll,
                            position = eval(parse(text=positionmedian)))
            }
            if(input$medianlines){
            p <- p +    stat_sum_df("median_hilow", geom = "line", stat ="smooth"  ,
                                    fun.args=list(conf.int=input$PI),aes(group=NULL),
                                    alpha=input$alphamedianl,
                                    col=mediancoll,
                                    position = eval(parse(text=positionmedian)))
            
            }
            if ( input$sepguides ){
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
            }
          }

                    
          if(input$Median!="None" && !input$forcemedianshape)    {
            
            if(input$medianpoints && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",col=mediancolp,
                                alpha=input$alphamedianp,
                                aes(group=NULL),
                                position = eval(parse(text=positionmedian)))
            
            if(input$medianpoints && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",
                                col=mediancolp,
                                alpha=input$alphamedianp,
                                aes(group=NULL),size=input$medianpointsize,
                                position = eval(parse(text=positionmedian)))
          }
          if(input$Median!="None" && input$forcemedianshape)    {
            
            if(input$medianpoints && input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",col=mediancolp,
                                alpha=input$alphamedianp,
                                aes(group=NULL),shape=input$medianshapes,
                                position = eval(parse(text=positionmedian)))
            
            if(input$medianpoints && input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "point",
                                col=mediancolp,
                                alpha=input$alphamedianp,
                                aes(group=NULL),size=input$medianpointsize,shape=input$medianshapes,
                                position = eval(parse(text=positionmedian)))
          } 
          
          if (input$Median!="None" && input$medianvalues )  {
            p <-   p   +
              stat_summary(fun.data = median.n, aes(group=NULL),geom = input$geommedianlabel,alpha=input$alphamedianlabel,
                           fun.y = median, fontface = "bold",colour=mediancoll,
                           position = eval(parse(text=positionmedian)),
                           show.legend=FALSE,size=6, seed=1234)}
          if (input$Median!="None" && input$medianN)  {
            p <-   p   +
              stat_summary(fun.data = give.n, aes(group=NULL), geom = input$geommedianlabel,alpha=input$alphamedianlabel,
                           fun.y = median, fontface = "bold", colour=mediancolp,
                           position = eval(parse(text=positionmedian)),
                           show.legend=FALSE,size=6, seed=1234)      
          }
          
        }
      }
      
      
      
      ###### Median PI section  END
      
      
      
      ###### RQSS SECTION START  
      if (!input$ignoregroupqr) {
        if (!input$ignorecolqr) {
          if (input$qr=="dynamicquantile") {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(method = "rqss",quantiles =input$Tau,size=input$qrlinesize,alpha=input$qrlinealpha,
                                      
                                      formula=y ~ qss(x, constraint= input$Constraints,lambda=input$Penalty))       
            }
            
            p <- p +  stat_quantile(method = "rqss",quantiles = as.numeric(input$predefquantiles),
                                    size=input$qrlinesize,alpha=input$qrlinealpha,
                                    linetype=input$predefquantileslinetype,
                                    formula=y ~ qss(x, constraint= input$Constraints,
                                                    lambda=input$Penalty))
            
            
            
          }
        }
        if (input$ignorecolqr) {
          colqr <- input$colqr
          if (input$qr=="dynamicquantile") {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(method = "rqss",quantiles =input$Tau,size=input$qrlinesize,alpha=input$qrlinealpha,
                                      col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty)) 
            }
            
            p <- p +  stat_quantile(method = "rqss", quantiles = as.numeric(input$predefquantiles),
                                    size=input$qrlinesize,alpha=input$qrlinealpha,
                                    col=colqr,
                                    linetype=input$predefquantileslinetype,
                                    formula=y ~ qss(x, constraint= input$Constraints,
                                                    lambda=input$Penalty))
          }
        }
      }
      
      
      if (input$ignoregroupqr) {
        if (!input$ignorecolqr) {
          if (input$qr=="dynamicquantile") {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles =input$Tau,
                                      size=input$qrlinesize,alpha=input$qrlinealpha,
                                      
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty)) 
            }
            
            p <- p +  stat_quantile(aes(group=NULL),
                                    method = "rqss",quantiles = as.numeric(input$predefquantiles),
                                    linetype=input$predefquantileslinetype,
                                    size=input$qrlinesize,alpha=input$qrlinealpha,
                                    formula=y ~ qss(x, constraint= input$Constraints,
                                                    lambda=input$Penalty)) 
            
          }
        }
        if (input$ignorecolqr) {
          colqr <- input$colqr
          if (input$qr=="dynamicquantile") {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",
                                      quantiles =input$Tau,
                                      size=input$qrlinesize,alpha=input$qrlinealpha,
                                      col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))     
            }
            
            p <- p +  stat_quantile(aes(group=NULL),method = "rqss",
                                    quantiles =as.numeric(input$predefquantiles),
                                    size=input$qrlinesize,alpha=input$qrlinealpha,
                                    linetype=input$predefquantileslinetype,
                                    col=colqr,
                                    formula=y ~ qss(x, constraint= input$Constraints,
                                                    lambda=input$Penalty))
          }
        }
      }
      
      
      ###### RQSS SECTION END
      
      #### Corr coefficient Start
      
      if(!input$corrignorecol ) {
        
        cortextxpos <- input$cortextxpos
        cortextypos <- input$cortextypos

        if (is.null(cortextxpos) || is.na(cortextxpos)  ) cortextxpos <- Inf
        if (is.null(cortextypos) || is.na(cortextypos) ) cortextypos <- Inf
        
        label.x.value <- ifelse(input$geomcorr=="text",cortextxpos,Inf)
        label.y.value <- ifelse(input$geomcorr=="text",cortextypos,Inf)
        
        if(input$addcorrcoeff&&!input$addcorrcoeffignoregroup) {
          
          if(!input$addcorrcoeffpvalue){
            p <- p +
              stat_cor(data=plotdata,
                       aes(label = paste("italic(R)", ..r.., sep = "~`=`~")),
                       position = position_identity(),size=input$corrlabelsize,
                       method = input$corrtype,
                       geom = input$geomcorr,
                       segment.color=NA,direction="y",
                       label.x = label.x.value, label.y = label.y.value,
                       show.legend = input$correlationshowlegend)
            
          }
          if(input$addcorrcoeffpvalue){
            p <- p +
              stat_cor(data=plotdata,
                       aes(label = paste("list(italic(R)~`=`~",..r..,",italic(p)~`=`~", ..p..,")",sep="")
                           ),
                       position = position_identity(),size=input$corrlabelsize,
                       method = input$corrtype ,geom = input$geomcorr,segment.color=NA,direction="y",
                       label.x = label.x.value, label.y = label.y.value,
                       show.legend = input$correlationshowlegend)
            
          }
          
        }#do notignoregroup do not corrignorecol
        
        if(input$addcorrcoeff&&input$addcorrcoeffignoregroup) {
          
          if(!input$addcorrcoeffpvalue){
            p <- p +
              stat_cor(data=plotdata,
                       aes(label = paste("italic(R)", ..r.., sep = "~`=`~"),group=NULL),
                       position  = position_identity(),size=input$corrlabelsize,
                       method = input$corrtype ,geom = input$geomcorr,segment.color=NA,direction="y",
                       label.x = label.x.value, label.y = label.y.value,
                       show.legend = input$correlationshowlegend)
            
          }
          
          
          if(input$addcorrcoeffpvalue){
            p <- p +
              stat_cor(data=plotdata,
                       aes(label = paste("list(italic(R)~`=`~",..r..,",italic(p)~`=`~", ..p..,")",sep="")
                           ,group=NULL),
                       position = position_identity(),size=input$corrlabelsize,
                       method = input$corrtype ,geom = input$geomcorr,segment.color=NA,direction="y",
                       label.x = label.x.value,
                       label.y = label.y.value,
                       show.legend = input$correlationshowlegend)
            
          }
          
          
        }#ignoregroup do not corrignorecol 
      }#do not corrignorecol 
      
      if(input$corrignorecol ) {
        
        cortextxpos <- input$cortextxpos
        cortextypos <- input$cortextypos
        
        if (is.null(cortextxpos) || is.na(cortextxpos) ) cortextxpos <- Inf
        if (is.null(cortextypos) || is.na(cortextypos) ) cortextypos <- Inf
        
        label.x.value <- ifelse(input$geomcorr=="text",cortextxpos,Inf)
        label.y.value <- ifelse(input$geomcorr=="text",cortextypos,Inf)
        
        if(input$addcorrcoeff && !input$addcorrcoeffignoregroup) {
          
          if(!input$addcorrcoeffpvalue){
            p <- p +
              stat_cor(data=plotdata,
                       aes(label =paste("italic(R)", ..r.., sep = "~`=`~")),
                       position = position_identity(),size=input$corrlabelsize,
                       method = input$corrtype ,geom = input$geomcorr,segment.color=NA,direction="y",
                       label.x = label.x.value, label.y = label.y.value,
                       color=input$corrcol, show.legend = input$correlationshowlegend)
          }
          
          if(input$addcorrcoeffpvalue){
            p <- p +
              stat_cor(data=plotdata,
                       aes(label = paste("list(italic(R)~`=`~",..r..,",italic(p)~`=`~", ..p..,")",sep="") ),
                       position = position_identity(),size=input$corrlabelsize,
                       method = input$corrtype ,geom = input$geomcorr,segment.color=NA,direction="y",
                       label.x = label.x.value, label.y = label.y.value,
                       color=input$corrcol, show.legend = input$correlationshowlegend)
          }
          
          
          
        }#do not ignoregroup corrignorecol
        
        if(input$addcorrcoeff&&input$addcorrcoeffignoregroup) {
          if(!input$addcorrcoeffpvalue){
            p <- p +
              stat_cor(data=plotdata,
                       aes(label =paste("italic(R)", ..r.., sep = "~`=`~"),group=NULL),
                       position = position_identity(),size=input$corrlabelsize,
                       method = input$corrtype, geom = input$geomcorr,segment.color=NA,direction="y",
                       label.x = label.x.value, label.y = label.y.value,
                       color= input$corrcol, show.legend = input$correlationshowlegend)
          }
          if(input$addcorrcoeffpvalue){
            p <- p +
              stat_cor(data=plotdata,
                       aes(label =paste("list(italic(R)~`=`~",..r..,",italic(p)~`=`~", ..p..,")",sep="")
                           , group=NULL),
                       position = position_identity(),size=input$corrlabelsize,
                       method = input$corrtype, geom = input$geomcorr,segment.color=NA,direction="y",
                       label.x = label.x.value, label.y = label.y.value,
                       color= input$corrcol, show.legend = input$correlationshowlegend)
          }
        }#ignoregroup input$corrignorecol
        
      }#input$corrignorecol 
      
      
      #### Corr coefficient END
      
      
      #### data label Start
      if(input$addcustomlabel&&input$labeltextin != 'None') {
        p <- p + aes_string(label = input$labeltextin)
        
        if(!input$customlabelignorecol ) {
          if(!input$addcustomlabelignoregroup) {
            if(input$labelignoresize){
              p <- p + stat_identity(data=plotdata, geom=input$geomlabel,show.legend = input$customlabellegend,
                                     size=input$labelsize, seed = 1234)
            }
            if(!input$labelignoresize){
              p <- p + stat_identity(data=plotdata,geom=input$geomlabel, show.legend = input$customlabellegend, seed = 1234)
            } 
          }#do notignoregroup 
          
          if(input$addcustomlabelignoregroup) {
            if(input$labelignoresize){
              p <- p + stat_identity(data=plotdata,aes(group=NULL), geom=input$geomlabel, show.legend = input$customlabellegend,
                                     size=input$labelsize, seed = 1234)
            }
            if(!input$labelignoresize){
              p <- p + stat_identity(data=plotdata,aes(group=NULL),geom=input$geomlabel,
                                     show.legend = input$customlabellegend, seed = 1234)
              
            }
            
          }#ignoregroup   
          
        }#do not labelignorecol
        
        if(input$customlabelignorecol ) {
          if(!input$addcustomlabelignoregroup) {
            if(input$labelignoresize){
              p <- p + stat_identity(data=plotdata, color=input$customlabelcol ,geom=input$geomlabel, show.legend = input$customlabellegend,
                                     size=input$labelsize, seed = 1234)
              
            }
            if(!input$labelignoresize){
              p <- p  + stat_identity(data=plotdata, color=input$customlabelcol ,geom=input$geomlabel,
                                      show.legend = input$customlabellegend, seed = 1234)
              
            }
          }#do notignoregroup 
          
          if(input$addcustomlabelignoregroup) {
            if(input$labelignoresize){
              p <- p + stat_identity(data=plotdata,aes(group=NULL), color=input$customlabelcol ,geom=input$geomlabel,
                                     show.legend = input$customlabellegend,
                                     size=input$labelsize, seed = 1234)
            }
            
            if(!input$labelignoresize){
              p <- p + stat_identity(data=plotdata,aes(group=NULL), color=input$customlabelcol ,geom=input$geomlabel,
                                     show.legend = input$customlabellegend, seed = 1234 )                
            }
            
          }#ignoregroup   
          
        }# label ignorecol
        
      }# addcustom label
      #### data label END
      
      ###### rug geom start
      if(input$addrugmarks) {
        
        if(! input$rugignorecol){
          p <- p +
            geom_rug(sides = paste(input$rugsides,collapse="",sep=""),
                     show.legend = FALSE,
                     alpha = input$ruglinealpha,
                     length = ggplot2::unit(input$ruglinelength ,"npc") 
            ) 
        }
        if(input$rugignorecol){
          p <- p +
            geom_rug(sides = paste(input$rugsides,collapse="",sep=""),
                     show.legend = FALSE,
                     alpha = input$ruglinealpha,
                     length = ggplot2::unit(input$ruglinelength ,"npc"),
                     col = input$colrug
            ) 
        }

      }
        if(input$addextrarugmarks &&
           !is.null(input$xrug) &&
           length(as.vector(input$xrug)) > 0) {
        for(i in input$xrug) {
          if(!input$rugignorecol){
          p <- p +
          geom_rug(aes_string(x=i),
                   sides = paste(input$extrarugsides, collapse="",sep=""),
                   show.legend = FALSE, inherit.aes = FALSE,
                   alpha = input$ruglinealpha,
                   length = ggplot2::unit(input$ruglinelength ,"npc")
          )
          }
          if(input$rugignorecol){
            p <- p +
              geom_rug(aes_string(x=i),
                       sides = paste(input$extrarugsides, collapse="",sep=""),
                       show.legend = FALSE, inherit.aes = FALSE,
                       alpha = input$ruglinealpha,
                       length = ggplot2::unit(input$ruglinelength ,"npc"),
                       col = input$colrug 
              )
          }
        }
      }
      #### rug geom end
      
      ###### KM SECTION START
      if (input$KM!="None") {
        
        if (input$reversecenstoevent){
          plotdata[,"status"]<- ifelse(plotdata[,"yvalues"]==1,0,1)
        }
        if (!input$reversecenstoevent){
          plotdata[,"status"]<- plotdata[,"yvalues"]
        }
        
        p <- sourceable(ggplot(plotdata, aes_string(time=input$x, status="status")))
        if (input$colorin != 'None')
          p <- p + aes_string(color=input$colorin)
        if (input$fillin != 'None')
          p <- p + aes_string(fill=input$fillin)
        if (input$linetypein != 'None')
          p <- p + aes_string(linetype=input$linetypein)
        
        if( !input$KMignoregroup){
          if (input$groupin != 'None' && !is.factor(plotdata[,input$x]) ){ 
            p <- p + aes_string(group=input$groupin)
          }
        }
        
        
        if (!input$kmignorecol) {
          if (input$KM == "KM/CI") {
            p <- p +
              geom_ribbon(stat="kmband",
                          alpha=input$KMCItransparency,
                          conf.int = input$KMCI,
                          conf.type = input$kmconftype,
                          conf.lower = input$kmconflower,
                          trans=input$KMtrans,
                          color="transparent"
              )
          }
          
          
          if (input$KM != "None") {
            p  <- p +
              geom_line(
                stat = "km",
                trans = input$KMtrans ,
                size = input$kmlinesize,
                alpha = input$kmlinealpha
              )
            
            if (input$censoringticks) {
              p  <- p +
                geom_kmticks(trans = input$KMtrans)
            }
          }
        }
        if (input$kmignorecol){
          
          if (input$KM=="KM/CI") {
            p <- p +
              geom_ribbon(stat="kmband",
                          alpha=input$KMCItransparency,
                          conf.int = input$KMCI,
                          conf.type = input$kmconftype,
                          conf.lower = input$kmconflower,
                          trans=input$KMtrans,
                          color="transparent"
              )
          }

          
          if (input$KM!="None") {
            p  <- p +
              geom_line(stat="km",trans=input$KMtrans ,size = input$kmlinesize, alpha = input$kmlinealpha,color=input$colkml)
            
            if (input$censoringticks) {
              p  <- p +
                geom_kmticks(trans=input$KMtrans,color=input$colkmticks)
            }
          }
        }
        
        
        if(input$KM!="None" && (input$addmediansurv== "addmediansurvival" ||
                                input$addmediansurv== "addmediancisurvival" ||
                                input$addrisktable) ){
          timevar  <- input$x
          statusvar<- "status"
          colorinputvar <-   ifelse(input$kmignorecol,"None" ,input$colorin) 
          fillinputvar <-  input$fillin
          linetypeinputvar <-  input$linetypein
          groupinputvar<-   ifelse(input$KMignoregroup,"None" ,input$groupin)
          survformula  <-  paste( "Surv","(",timevar,",",statusvar,")",sep="")
          listvars <- unique(c(colorinputvar,fillinputvar,linetypeinputvar,groupinputvar,
                               input$facetrowin,input$facetcolin,input$facetrowextrain,input$facetcolextrain))
          listvars <- listvars[!is.element(listvars,c("None",".")) ]
          listvars <- listvars[!duplicated(listvars) ]
          if ( length(listvars) ==0 ){
            f <- as.formula(paste(survformula, "1", sep = " ~ "))
          }
          if ( length(listvars) >0 ){
            f <- as.formula(paste(survformula, paste(listvars, collapse = " + "), sep = " ~ "))
          }
          fitsurv <- eval(bquote( survfit( .(f)  , plotdata, conf.int=input$KMCI) ))

          if (is.null(input$breaktimeby) || input$breaktimeby == '' || is.na(input$breaktimeby)){
            ggsurv <- survminer::ggsurvplot(fitsurv,
                                            plotdata,risk.table = TRUE,
                                            ggtheme = theme_bw())
          } else {
            ggsurv <- survminer::ggsurvplot(fitsurv,
                                            plotdata,risk.table = TRUE,
                                            break.time.by = input$breaktimeby,
                                            ggtheme = theme_bw())
          }

          risktabledata<- ggsurv$table$data
          if(!is.null(input$risktablevariables) && length(as.vector(input$risktablevariables)) > 0){
            risktabledatag<- gather(risktabledata,key,value, !!!input$risktablevariables , factor_key = TRUE)
            risktabledatag$keynumeric<- - input$nriskpositionscaler* as.numeric(as.factor(risktabledatag$key)) 
          }
          if(is.null(input$risktablevariables) ){
            risktabledatag<- gather(risktabledata,key,value, n.risk, factor_key = TRUE)
            risktabledatag$keynumeric<- - input$nriskpositionscaler* as.numeric(as.factor(risktabledatag$key)) 
          }
          if(!is.null(fitsurv$strata) | is.matrix(fitsurv$surv))  {
            .table <- as.data.frame(summary(fitsurv)$table)
          } else {
            .table <- t(as.data.frame(summary(fitsurv)$table))
            rownames(.table) <- "All"
          }
          surv_median <- as.vector(.table[,"median"])
          
          dfmedian <- data.frame(x1 = surv_median,
                                 x2 = surv_median,
                                 x1lower =  as.vector(.table[,"0.95LCL"]),
                                 x1upper =  as.vector(.table[,"0.95UCL"]),
                                 y1 = rep(0, length(surv_median)),
                                 y2 = rep(0.5, length(surv_median)),
                                 strata = .clean_strata(rownames(.table)))
          if(!is.null(fitsurv$strata)){
            variables <- .get_variables(dfmedian$strata, fitsurv, plotdata)
            for(variable in variables) dfmedian[[variable]] <- .get_variable_value(variable, dfmedian$strata, fitsurv, plotdata)
          }
          
        }
        if (input$KM!="None") {
        
        if (input$addrisktable){
          if (!input$kmignorecol){
            p  <- p +
              geom_text(data=risktabledatag,aes(x=time,label=value,y=keynumeric,time=NULL,status=NULL ),show.legend = FALSE,
                        position =   position_dodgev(height =input$nriskpositiondodge)
              )
            
           }
           if (input$kmignorecol){
            p  <- p +
              geom_text(data=risktabledatag,
                        aes(x=time,label=value,y=keynumeric,time=NULL,status=NULL ),
                        show.legend = FALSE,
                        position =   position_dodgev(height =input$nriskpositiondodge),
                        color=input$colkml)
           }
          
          
          if(input$addhorizontallines){
            p  <- p +
              geom_hline(yintercept =- input$nriskpositionscaler *unique(c(1,(as.numeric(as.factor(risktabledatag$key))+1 )) )  +
                           (abs(input$nriskpositiondodge)/2 ) )
            
          }

        }#addrisktable input$addrisktable
        
        if(input$arrowmedian) {
          arrowmediandraw = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first")
        }
        
        if(!input$arrowmedian) {
          arrowmediandraw = NULL
        }
        
        if(input$addmediansurv== "addmediancisurvival"){
          if (!input$kmignorecol){
            p  <-  p +
              geom_label_repel(data = dfmedian, aes(x= x1 , y= y2 ,label =sprintf("%#.3g (%#.3g, %#.3g)",x1,x1lower,x1upper),
                                                    status=NULL,time=NULL),show.legend = FALSE,
                               label.size = NA, direction="both",fill="white",
                               segment.color="black",nudge_y = -0.1,segment.size = 0.5,
                               alpha = 0.5,label.padding=.1, force = 5,
                               na.rm=TRUE,
                               seed = 1234) +
              geom_label_repel(data = dfmedian, aes(x= x1 , y= y2 ,label =sprintf("%#.3g (%#.3g, %#.3g)",x1,x1lower,x1upper),
                                                    status=NULL,time=NULL),show.legend = FALSE,
                               label.size = NA,direction="both",
                               nudge_y = -0.1,segment.size = 0.5,
                               arrow = arrowmediandraw,
                               alpha = 1,label.padding=.1, force = 5,
                               na.rm=TRUE,
                               fill = NA,
                               seed = 1234)
          }
          if (input$kmignorecol){
            p  <-  p +
              geom_label_repel(data = dfmedian, aes(x= x1 , y= y2 ,label =sprintf("%#.3g (%#.3g, %#.3g)",x1,x1lower,x1upper),
                                                    status=NULL,time=NULL),show.legend = FALSE,
                               label.size = NA, direction="both",fill="white",color=input$colkml,
                               segment.color="black",nudge_y = -0.1,segment.size = 0.5,
                               alpha = 0.5,label.padding=.1, force = 5,
                               na.rm=TRUE,
                               seed = 1234) +
              geom_label_repel(data = dfmedian, aes(x= x1 , y= y2 ,label =sprintf("%#.3g (%#.3g, %#.3g)",x1,x1lower,x1upper),
                                                    status=NULL,time=NULL),show.legend = FALSE,
                               label.size = NA,direction="both",color=input$colkml,
                               nudge_y = -0.1,segment.size = 0.5,
                               arrow = arrowmediandraw,
                               alpha = 1,label.padding=.1, force = 5,
                               na.rm=TRUE,
                               fill = NA,
                               seed = 1234)
          }
        }
        
        if(input$addmediansurv== "addmediansurvival" ){
          if (!input$kmignorecol){
            p  <-  p +
              geom_label_repel(data = dfmedian, aes(x= x1 , y= y2 ,label = sprintf("%#.3g",x1), status=NULL,time=NULL),show.legend = FALSE,
                               label.size = NA, direction="both",fill="white",
                               segment.color="black",nudge_y = -0.1,segment.size = 0.5,
                               alpha = 0.5,label.padding=.1, force = 5,
                               na.rm=TRUE,
                               seed = 1234) +
              geom_label_repel(data = dfmedian, aes(x= x1 , y= y2 ,label =sprintf("%#.3g",x1),
                                                    status=NULL,time=NULL),show.legend = FALSE,
                               label.size = NA,direction="both",
                               nudge_y = -0.1,segment.size = 0.5,
                               arrow = arrowmediandraw,
                               alpha = 1,label.padding=.1, force = 5,
                               na.rm=TRUE,
                               fill = NA,
                               seed = 1234)
          }
          
          if (input$kmignorecol){
            p  <-  p +
              geom_label_repel(data = dfmedian, aes(x= x1 , y= y2 ,label =sprintf("%#.3g",x1),
                                                    status=NULL,time=NULL),show.legend = FALSE,
                               label.size = NA, direction="both",fill="white",color=input$colkml,
                               segment.color="black",nudge_y = -0.1,segment.size = 0.5,
                               alpha = 0.5,label.padding=.1, force = 5,
                               na.rm=TRUE,
                               seed = 1234) +
              geom_label_repel(data = dfmedian, aes(x= x1 , y= y2 ,label =sprintf("%#.3g",x1),
                                                    status=NULL,time=NULL),show.legend = FALSE,
                               label.size = NA,direction="both",color=input$colkml,
                               nudge_y = -0.1,segment.size = 0.5,
                               arrow = arrowmediandraw,
                               alpha = 1,label.padding=.1, force = 5,
                               na.rm=TRUE,
                               fill = NA,
                               seed = 1234)
          }
          
          
        }
}
      } ###### KM SECTION END still need to fix y scale labels
      
      p <- p + xlab(input$x)
    }
    
    if (!input$show_pairs) {
      allfacetsvariables<- c(input$facetrowin,input$facetrowextrain,input$facetcolin,input$facetcolextrain)
      allfacetsvariables[which(duplicated(allfacetsvariables))]<- "." # make it not fail
      labelwrapwidth <- input$labelwrapwidth
      facets <- paste(allfacetsvariables[1],
                      '+',
                      allfacetsvariables[2],
                      '~',
                      allfacetsvariables[3],
                      '+',
                      allfacetsvariables[4])
      
      ASTABLE <- ifelse(input$facetordering == "table", TRUE, FALSE)
      p <- attach_source_dep(p, "ASTABLE")
      if (facets != '. + . ~ . + .' && !input$facetwrap) {
        facets<- as.formula(facets)
        p <- attach_source_dep(p, "facets")
        
        facetswitch <-
          if (input$facetswitch == "none")
            NULL
        else {
          input$facetswitch
        }
        p <- attach_source_dep(p, "facetswitch")
        facetmargins <- facetmargins()
        p <- attach_source_dep(p, "facetmargins")
        
        if (input$facetlabeller != "label_wrap_gen"){
          p <- p + facet_grid(
            facets,
            scales = input$facetscalesin,
            space = input$facetspace,
            switch = facetswitch,
            labeller = eval(parse(
              text=paste0("function(labs){",input$facetlabeller,
                          "(labs, multi_line = ",input$facetwrapmultiline,")}")
            )),
            margins = facetmargins,
            as.table = ASTABLE
          )
        }
        if (input$facetlabeller == "label_wrap_gen"){
          p <-
            p + facet_grid(
              facets,
              scales = input$facetscalesin,
              space = input$facetspace,
              switch = facetswitch,
              labeller =label_wrap_gen(width = input$labelwrapwidth,
                                       multi_line = input$facetwrapmultiline),
              margins = facetmargins,
              as.table = ASTABLE
            ) 
        }
      }
      
      
      if (facets != '. + . ~ . + .' && input$facetwrap) {
        multiline <-  input$facetwrapmultiline
        wrapncol <-
          if (is.na(input$wrapncol) |
              is.null(input$wrapncol))
            NULL
        else {
          input$wrapncol
        }
        wrapnrow <-
          if (is.na(input$wrapnrow) |
              is.null(input$wrapnrow))
            NULL
        else {
          input$wrapnrow
        }
        
        facetgridvariables <-
          c(
            input$facetcolin,
            input$facetcolextrain,
            input$facetrowin,
            input$facetrowextrain
          ) [c(
            input$facetcolin,
            input$facetcolextrain,
            input$facetrowin,
            input$facetrowextrain
          ) != "."]
        if (input$facetlabeller != "label_wrap_gen"){
          p <- p + facet_wrap(
            facetgridvariables,
            scales = input$facetscalesin,
            ncol = input$wrapncol,
            nrow = input$wrapnrow,
            labeller = eval(parse(
              text=paste0("function(labs){",input$facetlabeller,
                          "(labs, multi_line = ",input$facetwrapmultiline,")}")
            )),
            strip.position = input$stripposition,
            as.table = ASTABLE
          )
        }
        if (input$facetlabeller == "label_wrap_gen"){
        p <- p + facet_wrap(
          facetgridvariables,
          scales = input$facetscalesin,
          ncol = input$wrapncol,
          nrow = input$wrapnrow,
          labeller = label_wrap_gen(width = input$labelwrapwidth,
                                    multi_line = input$facetwrapmultiline),
          strip.position = input$stripposition,
          as.table = ASTABLE
        )
        }
        
        
      }
      
      
      if (input$yaxisscale=="logy" &&
          !is.null(plotdata$yvalues) &&
          is.numeric(plotdata[,"yvalues"])){
        if(!input$customyticks){
        if (input$yaxisformat=="default"){
          p <- p + scale_y_log10(expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                    add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
                                 )
        }
        if (input$yaxisformat=="logyformat"){
        p <- p + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x)),
                               expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
                               )
        }
        if (input$yaxisformat=="logyformat2"){
        p <- p + scale_y_log10(labels=prettyNum,
                               expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
                               )
        }
        }#nocustomticks
       if(input$customyticks){
         
         if (input$yaxisformat=="default"){
           if ( !input$customytickslabel) {
           p <- p  + 
             scale_y_log10(breaks=as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ),
                           minor_breaks = as.numeric(unique(unlist (strsplit(input$yaxisminorbreaks, ","))) ),
                           expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                              add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
                           )
           }
           if (input$customytickslabel) {
             yaxislabels <- gsub("\\\\n", "\\\n", input$yaxislabels)
             p <- p  + 
               scale_y_log10(breaks=as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ),
                             labels= rep_len(unlist(strsplit(yaxislabels, ",")) ,
                                             length(as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ",")))))),
                             minor_breaks = as.numeric(unique(unlist (strsplit(input$yaxisminorbreaks, ","))) ),
                             expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
                             )
           }
         }#default
         if (input$yaxisformat=="logyformat"){
           p <- p  + 
             scale_y_log10(labels = trans_format("log10", math_format(10^.x)),
                           breaks=as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ),
                           minor_breaks = as.numeric(unique(unlist (strsplit(input$yaxisminorbreaks, ","))) ),
                           expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                              add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
                           ) 
         }
         if (input$yaxisformat=="logyformat2"){
           p <- p  + 
             scale_y_log10(labels = prettyNum,
                           breaks=as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ),
                           minor_breaks = as.numeric(unique(unlist (strsplit(input$yaxisminorbreaks, ","))) ),
                           expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                              add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
                           ) 
         }
         
       }#customyticks
} # logy

      if (input$yaxisscale=="lineary" && 
          !is.null(plotdata$yvalues) &&
          is.numeric(plotdata[,"yvalues"]) &&
          !input$customyticks &&
          input$yaxisformat=="default"){
        if (!input$addrisktable){
        p <- p  + 
          scale_y_continuous(
            expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                               add  = c(input$yexpansion_l_add, input$yexpansion_r_add)))
        }#norisktable no km
        if(input$KM!="None" &&  input$addrisktable){
        if(!is.null(input$risktablevariables)){
          p  <- p +
            scale_y_continuous(breaks =c(unique(risktabledatag$keynumeric),
                                         c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) ), 
                               labels= c(as.vector(input$risktablevariables),
                                         c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1") ),
                               expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
            )
        }
        if(is.null(input$risktablevariables)){
          p  <- p +
            scale_y_continuous(breaks =c(unique(risktabledatag$keynumeric),
                                         c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) ), 
                               labels= c("n.risk",
                                         c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1") ),
                               expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
            )
          
        } #defaultrisktablevariable is n.risk
      }#addrisktable
      } #yaxisscale=="lineary" yaxisformat=="default"
      
      if (input$yaxisscale=="lineary"
          && !is.null(plotdata$yvalues) &&
          is.numeric(plotdata[,"yvalues"])&&
          !input$customyticks &&
          input$yaxisformat=="scientificy"){
        p <- p  + 
          scale_y_continuous(labels=comma ,
                             expand = expansion(mult = c(input$yexpansion_l_mult,
                                                         input$yexpansion_r_mult),
                                                add  = c(input$yexpansion_l_add,
                                                         input$yexpansion_r_add)))
        
      }# input$yaxisscale=="lineary" input$yaxisformat=="scientificy"
      if (input$yaxisscale=="lineary" &&
          !is.null(plotdata$yvalues) &&
          is.numeric(plotdata[,"yvalues"]) &&
          !input$customyticks &&
          input$yaxisformat=="percenty"){ 
        if (!input$addrisktable){
          p <- p  + 
            scale_y_continuous(labels=percent,
              expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                 add  = c(input$yexpansion_l_add, input$yexpansion_r_add)))
        }#norisktable
        if (input$KM!="None" && input$addrisktable){
          if(!is.null(input$risktablevariables)){
            p  <- p +
              scale_y_continuous(
                breaks =c(unique(risktabledatag$keynumeric),
                                           c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) ), 
                                 labels= c(as.vector(input$risktablevariables),
                                           c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%") ),
                                 expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                    add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
              )
          }
          if(is.null(input$risktablevariables)){
            p  <- p +
              scale_y_continuous(breaks =c(unique(risktabledatag$keynumeric),
                                           c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) ), 
                                 labels= c("n.risk",
                                           c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%") ),
                                 expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                    add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
              )
            
          } #defaultrisktablevariable is n.risk
        }#addrisktable        
        
        
      } # input$yaxisscale=="lineary" input$yaxisformat=="percenty"
              
      
      if (input$yaxisscale=="lineary" &&
          !is.null(plotdata$yvalues) &&
          is.numeric(plotdata[,"yvalues"])&&
          input$customyticks &&
          input$yaxisformat=="default") {
        if ( !input$customytickslabel) {
        p <- p  + 
          scale_y_continuous(breaks=as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ),
                             minor_breaks = as.numeric(unique(unlist (strsplit(input$yaxisminorbreaks, ","))) ) ,
                             expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) 
        if (input$KM!="None" && input$addrisktable){
          p  <- p +
            scale_y_continuous(
              breaks =c(unique(risktabledatag$keynumeric),
                        as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ) ),
              minor_breaks = as.numeric(unique(unlist (strsplit(input$yaxisminorbreaks, ","))) ) ,
              expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                 add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
            )
        }#addrisktable 
        }
        if ( input$customytickslabel) {
          yaxislabels <- gsub("\\\\n", "\\\n", input$yaxislabels)
          p <- p  + 
            scale_y_continuous(breaks=as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ),
                               labels= rep_len(unlist(strsplit(yaxislabels, ",")) ,
                                               length(as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ",")))))),
                               minor_breaks = as.numeric(unique(unlist (strsplit(input$yaxisminorbreaks, ","))) ) ,
                               expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                  add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) 
        
          if (input$KM!="None" && input$addrisktable){
            p  <- p +
              scale_y_continuous(
                breaks =c(unique(risktabledatag$keynumeric),
                          as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ) ),
                labels= rep_len(unlist(strsplit(yaxislabels, ",")) ,
                                length(c(unique(risktabledatag$keynumeric),
                                         as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ) ))
                                ),
                minor_breaks = as.numeric(unique(unlist (strsplit(input$yaxisminorbreaks, ","))) ) ,
                expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                   add  = c(input$yexpansion_l_add, input$yexpansion_r_add))
              )
          }#addrisktable   
          }
      }
      if (input$yaxisscale=="lineary" &&
          !is.null(plotdata$yvalues) &&
          is.numeric(plotdata[,"yvalues"]) &&
          input$customyticks &&
          !input$customytickslabel &&
          input$yaxisformat=="scientificy" ) {
        p <- p  + 
          scale_y_continuous(labels=comma,
                             breaks=as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ),
                             minor_breaks = as.numeric(unique(unlist (strsplit(input$yaxisminorbreaks, ","))) ),
                             expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                add  = c(input$yexpansion_l_add, input$yexpansion_r_add)) ) 
      }
      
      if (input$yaxisscale=="lineary" &&
          !is.null(plotdata$yvalues) &&
          is.numeric(plotdata[,"yvalues"]) &&
          input$customyticks &&
          !input$customytickslabel &&
          input$yaxisformat=="percenty" ) {
        p <- p  + 
          scale_y_continuous(labels=percent,
                             breaks=as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ),
                             minor_breaks = as.numeric(unique(unlist (strsplit(input$yaxisminorbreaks, ","))) ) ,
                             expand = expansion(mult = c(input$yexpansion_l_mult,input$yexpansion_r_mult),
                                                add  = c(input$yexpansion_l_add, input$yexpansion_r_add))) 
      }
      
      if (input$xaxisscale=="logx" &&
          is.numeric(plotdata[,input$x])) {
        
        if (!input$customxticks){
          if (input$xaxisformat=="default") {
            p <- p + scale_x_log10(expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                                      add  = c(input$xexpansion_l_add, input$xexpansion_r_add))
            )
          }
          if (input$xaxisformat=="logxformat") {
            p <- p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                   labels = trans_format("log10", math_format(10^.x)),
                                   expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                                      add  = c(input$xexpansion_l_add, input$xexpansion_r_add))
            )
          }
          if (input$xaxisformat=="logxformat2") {
            p <- p + scale_x_log10(labels = prettyNum,
                                   expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                                      add  = c(input$xexpansion_l_add, input$xexpansion_r_add))
            )
          }  
        }#nocustomticks
        if (input$customxticks){
          if (input$xaxisformat=="default") {
            if ( !input$customxtickslabel) {
              p <- p  + 
                scale_x_log10(breaks=as.numeric(unique(unlist (strsplit(input$xaxisbreaks, ","))) ),
                              minor_breaks = as.numeric(unique(unlist (strsplit(input$xaxisminorbreaks, ","))) ),
                              expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                                 add  = c(input$xexpansion_l_add, input$xexpansion_r_add)
                              )) 
              
            }
            if ( input$customxtickslabel) {
              xaxislabels <- gsub("\\\\n", "\\\n", input$xaxislabels)
              p <- p  + 
                scale_x_log10(breaks=as.numeric(unique(unlist (strsplit(input$xaxisbreaks, ","))) ),
                              labels= rep_len(unlist(strsplit(xaxislabels, ",")) ,
                                              length(as.numeric(unique(unlist (strsplit(input$xaxisbreaks, ",")))))),
                              minor_breaks = as.numeric(unique(unlist (strsplit(input$xaxisminorbreaks, ","))) ),
                              expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                                 add  = c(input$xexpansion_l_add, input$xexpansion_r_add)
                              )
                ) 
            }
            
          }#xaxisformat default
          if (input$xaxisformat=="logxformat") {
            p <- p  + 
              scale_x_log10(labels = trans_format("log10", math_format(10^.x)),
                            breaks=as.numeric(unique(unlist (strsplit(input$xaxisbreaks, ","))) ),
                            minor_breaks = as.numeric(unique(unlist (strsplit(input$xaxisminorbreaks, ","))) ),
                            expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                               add  = c(input$xexpansion_l_add, input$xexpansion_r_add)
                            )) 
          }
          if (input$xaxisformat=="logxformat2") {
            p <- p  + 
              scale_x_log10(labels = prettyNum,
                            breaks=as.numeric(unique(unlist (strsplit(input$xaxisbreaks, ","))) ),
                            minor_breaks = as.numeric(unique(unlist (strsplit(input$xaxisminorbreaks, ","))) ),
                            expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                               add  = c(input$xexpansion_l_add, input$xexpansion_r_add)
                            )) 
          }  
        }#custom x ticks
      }#logx      

      if (input$xaxisscale=="linearx" &&
          is.numeric(plotdata[,input$x])) {
        if (!input$customxticks){
        if (input$xaxisformat=="default") {
          p <- p  + 
            scale_x_continuous(expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                                  add  = c(input$xexpansion_l_add, input$xexpansion_r_add))
            )
        }
        if (input$xaxisformat=="scientificx") {
          p <- p  + 
            scale_x_continuous(labels=comma,
                               expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                                  add  = c(input$xexpansion_l_add, input$xexpansion_r_add))
            )  
        }
        if (input$xaxisformat=="percentx") {
          p <- p  + 
            scale_x_continuous(labels=percent,
                               expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                                  add  = c(input$xexpansion_l_add, input$xexpansion_r_add))
            )
      }  
      }#nocustomticks
        if (input$customxticks){
          if (input$xaxisformat=="default") {
            if ( !input$customxtickslabel) {
              p <- p  + 
                scale_x_continuous(
                  breaks=as.numeric(unique(unlist (strsplit(input$xaxisbreaks, ","))) ),
                  minor_breaks = as.numeric(unique(unlist (strsplit(input$xaxisminorbreaks, ","))) ),
                  expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                     add  = c(input$xexpansion_l_add, input$xexpansion_r_add)
                  )
                ) 
              
            }
            if ( input$customxtickslabel) {
              xaxislabels <- gsub("\\\\n", "\\\n", input$xaxislabels)
              p <- p  + 
                scale_x_continuous(
                  breaks=as.numeric(unique(unlist (strsplit(input$xaxisbreaks, ","))) ),
                  labels= rep_len(unlist(strsplit(xaxislabels, ",")) ,
                                  length(as.numeric(unique(unlist (strsplit(input$xaxisbreaks, ",")))))),
                  minor_breaks = as.numeric(unique(unlist (strsplit(input$xaxisminorbreaks, ","))) ),
                  expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                     add  = c(input$xexpansion_l_add, input$xexpansion_r_add)
                  )) 
            }
            
          }#xaxisformat default
          if (input$xaxisformat=="scientificx") {
            p <- p  + 
              scale_x_continuous(labels=comma,
                                 breaks=as.numeric(unique(unlist (strsplit(input$xaxisbreaks, ","))) ),
                                 minor_breaks = as.numeric(unique(unlist (strsplit(input$xaxisminorbreaks, ","))) ),
                                 expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                                    add  = c(input$xexpansion_l_add, input$xexpansion_r_add)
                                 )
              ) 
          }
          if (input$xaxisformat=="percentx") {
            p <- p  + 
              scale_x_continuous(labels=percent,
                                 breaks=as.numeric(unique(unlist (strsplit(input$xaxisbreaks, ","))) ),
                                 minor_breaks = as.numeric(unique(unlist (strsplit(input$xaxisminorbreaks, ","))) ),
                                 expand = expansion(mult = c(input$xexpansion_l_mult,input$xexpansion_r_mult),
                                                    add  = c(input$xexpansion_l_add, input$xexpansion_r_add)
                                 )) 
          }  
        }#custom x ticks
      }
      
     
      if (!is.null(input$y) && length(input$y) >= 2 && input$ylab=="" ){
        p <- p + ylab("Y variable(s)")
      }
      if (!is.null(input$y) && length(input$y) < 2 && input$ylab=="" ){
        p <- p + ylab(input$y)
      }
      
      if (input$horizontalzero)
        p <-    p+
        geom_hline(aes(yintercept=0))
      
      if (input$customvline1)
        p <-    p+
        geom_vline(xintercept=input$vline1,color=input$vlinecol1,linetype=input$vlinetype1,size=input$vlinesize1)
      if (input$customvline2)
        p <-    p+
        geom_vline(xintercept=input$vline2,color=input$vlinecol2,linetype=input$vlinetype2,size=input$vlinesize2)      
      
      if (input$customhline1)
        p <-    p+
        geom_hline(yintercept=input$hline1,color=input$hlinecol1,linetype=input$hlinetype1,size=input$hlinesize1)
      
      if (input$customhline2)
        p <-    p+
        geom_hline(yintercept=input$hline2,color=input$hlinecol2,linetype=input$hlinetype2,size=input$hlinesize2)     
      
      if (input$identityline)
        p <-    p+ geom_abline(intercept = 0, slope = 1)
      
      
      
      if (input$customlegendtitle){
        
        colourpos<-  which0( input$legendordering=="colour")[1]
        fillpos  <-  which0( input$legendordering=="fill")[1]
        sizepos  <-  which0( input$legendordering=="size")[1]
        shapepos  <-  which0( input$legendordering=="shape")[1]
        linetypepos  <-  which0( input$legendordering=="linetype")[1]
        
        collegend <-  gsub("\\\\n", "\\\n", input$customcolourtitle)
        filllegend <- gsub("\\\\n", "\\\n", input$customfilltitle)
        sizelegend <- gsub("\\\\n", "\\\n", input$customsizetitle)
        shapelegend <- gsub("\\\\n", "\\\n", input$customshapetitle)
        linetypelegend <- gsub("\\\\n", "\\\n", input$customlinetypetitle)
        
        
        
        if (input$legendalphacol){
          gcol  <- guide_legend(collegend,
                                ncol=input$legendncolcol,
                                reverse=input$legendrevcol,
                                order= colourpos,
                                override.aes = list(alpha = 1))
        }
        if (!input$legendalphacol){
          gcol  <- guide_legend(collegend,
                                ncol=input$legendncolcol,
                                reverse=input$legendrevcol,
                                order= colourpos)
        }
        
        if (input$legendalphafill){
          gfill <- guide_legend(filllegend,
                                ncol=input$legendncolfill,
                                reverse=input$legendrevfill,
                                order = fillpos,
                                override.aes = list(alpha = 1))
        }
        if (!input$legendalphafill){
          gfill <- guide_legend(filllegend,
                                ncol=input$legendncolfill,
                                reverse=input$legendrevfill,
                                order = fillpos)
        }
        
        gsize  <- guide_legend(sizelegend,
                               ncol=input$legendncolsize,
                               reverse=input$legendrevsize,
                               order = sizepos)
        
        gshape <- guide_legend(shapelegend,
                               ncol=input$legendncolshape,
                               reverse=input$legendrevshape,
                               order = shapepos)
        
        glinetype <- guide_legend(linetypelegend,
                                  ncol=input$legendncollinetype,
                                  reverse=input$legendrevlinetype,
                                  order = linetypepos)
        
        if (input$removelegend){
          if( colourpos==0) gcol = FALSE
          if( fillpos==0) gfill = FALSE
          if( sizepos==0) gsize = FALSE
          if( shapepos==0) gshape = FALSE
          if( linetypepos==0) glinetype = FALSE
        }
        
        
        p <-  p + guides(colour = gcol,
                         size = gsize,
                         fill = gfill,
                         shape= gshape,
                         linetype = glinetype)
        
      }
      
      if(input$colorin!="None"){
        
        if (input$themecolorswitcher=="themeggplot"&&!is.numeric(plotdata[,input$colorin])){
          p <-  p + scale_colour_hue(drop=!input$themecolordrop)
        }
        

        if (input$themecolorswitcher=="themeviridis" && !is.numeric(plotdata[,input$colorin])){
          p <-  p + scale_colour_viridis_d(drop=!input$themecolordrop)
        }
        
        if (input$themecontcolorswitcher=="themeviridis" && is.numeric(plotdata[,input$colorin])){
          p <-  p + scale_colour_viridis_c()
        }
        
      }
      
      
      if(input$fillin!="None"){
        if (input$themecolorswitcher=="themeggplot"&&!is.numeric(plotdata[,input$fillin])){
          p <-  p + scale_fill_hue(drop=!input$themecolordrop)
          
        }
        

        if (input$themecolorswitcher=="themeviridis"&&!is.numeric(plotdata[,input$fillin])){
          p <-  p + scale_fill_viridis_d(drop=!input$themecolordrop)
        }
        
        if (input$themecontcolorswitcher=="themeviridis"&&is.numeric(plotdata[,input$fillin])){
          p <-  p + scale_fill_viridis_c()
        }
        
      }
      
      
      if(input$pointsizein!="None"){
        
        if(!input$scalesizearea&&is.numeric(plotdata[,input$pointsizein])){
          p <- p +  scale_size(range = c(input$scalesizearearange1[1], input$scalesizearearange1[2]))   }   
        if(input$scalesizearea&&is.numeric(plotdata[,input$pointsizein])){
          p <- p +  scale_size_area(max_size =  input$scalesizearearange2[1])   }
        
      }
      
      if(input$annotatelogticks){
        p <-  p+
          annotation_logticks(sides=paste(input$logsides,collapse="",sep="") )   
      }
      
      if (all(
        input$yaxiszoom=='noyzoom'&&
        !is.null(input$xaxiszoomin[1])&&
        is.numeric(plotdata[,input$x] )&&
        input$facetscalesin!="free_x"&&
        input$facetscalesin!="free")
      ){
        if(input$xaxiszoom=="userxzoom"){
          p <- p +
            coord_cartesian(xlim= c(input$lowerxin,input$upperxin),
                            expand=input$expand)
        }
        if(input$xaxiszoom=="automaticxzoom"){
          p <- p +
            coord_cartesian(xlim= c(input$xaxiszoomin[1],input$xaxiszoomin[2]),
                            expand=input$expand)
        }
        
      }
      
      if (all(
        input$xaxiszoom=='noxzoom'  &&
        !is.null(plotdata$yvalues) &&
        is.numeric(plotdata[,"yvalues"] ) &&
        input$facetscalesin!="free_y" &&
        input$facetscalesin!="free")
      ){
        if(input$yaxiszoom=="useryzoom" ){
          p <- p +
            coord_cartesian(ylim= c(input$loweryin,input$upperyin),
                            expand=input$expand)
        }
        if(input$yaxiszoom=="automaticyzoom"){
          
          if(!is.null(input$yaxiszoomin[1]) ){
            p <- p +
              coord_cartesian(
                ylim= c(input$yaxiszoomin[1],input$yaxiszoomin[2]),
                expand=input$expand) 
          } 
          if(is.null(input$yaxiszoomin[1]) ){
            p <- p +
              coord_cartesian(ylim= c(NA,NA),
                              expand=input$expand) 
          } 
        }
        
      }
      
      
      if (all(!is.null(input$xaxiszoomin[1])&&
              is.numeric(plotdata[,input$x] ) && !is.null(plotdata$yvalues) &&
              is.numeric(plotdata[,"yvalues"]) &&
              input$facetscalesin!="free_x"&&input$facetscalesin!="free_y"&&
              input$facetscalesin!="free")
      ){
        
        if (input$xaxiszoom=="userxzoom"&& input$yaxiszoom=="useryzoom"){
          p <- p +
            coord_cartesian(xlim= c(input$lowerxin,input$upperxin),
                            ylim= c(input$loweryin,input$upperyin),
                            expand=input$expand)
        }
        if (input$xaxiszoom=="userxzoom"&&input$yaxiszoom=="automaticyzoom"){
          if(!is.null(input$yaxiszoomin[1]) ){
            p <- p +
              coord_cartesian(xlim= c(input$lowerxin,input$upperxin),
                              ylim= c(input$yaxiszoomin[1],input$yaxiszoomin[2]),
                              expand=input$expand)
          }
          if(is.null(input$yaxiszoomin[1]) ){
            p <- p +
              coord_cartesian(xlim= c(input$lowerxin,input$upperxin),
                              ylim= c(NA,NA),
                              expand=input$expand)
          }
          
        }
        if (input$xaxiszoom=="automaticxzoom"&&input$yaxiszoom=="useryzoom"){
          p <- p +
            coord_cartesian(xlim= c(input$xaxiszoomin[1],input$xaxiszoomin[2]),
                            ylim= c(input$loweryin,input$upperyin),
                            expand=input$expand)
        }
        if (input$xaxiszoom=="automaticxzoom"&&input$yaxiszoom=="automaticyzoom"){
          if(!is.null(input$yaxiszoomin[1]) ){
            p <- p +
              coord_cartesian(xlim= c(input$xaxiszoomin[1],input$xaxiszoomin[2]),
                              ylim= c(input$yaxiszoomin[1],input$yaxiszoomin[2]),
                              expand=input$expand)
          }
          if(is.null(input$yaxiszoomin[1]) ){
            p <- p +
              coord_cartesian(xlim= c(input$xaxiszoomin[1],input$xaxiszoomin[2]),
                              ylim= c(NA,NA),
                              expand=input$expand)
          }
        }
      }
      
      if (input$showtargettext){
        targettext <-  gsub("\\\\n", "\\\n", input$targettext)
        p <- p +
          annotate("text", x=input$targettextxpos, y=input$targettextypos,
                   label=targettext, col=input$targettextcol,
                   hjust=input$targettexthjust,
                   vjust=input$targettextvjust,size=input$targettextsize)
      }
    }
    
    p <- add_plot_theme(p)
    values$prevPlot <- p
    
    p
  })
  add_plot_theme <- function(p) {
    
    xlablinebreak <- gsub("\\\\n", "\\\n", input$xlab)
    ylablinebreak <- gsub("\\\\n", "\\\n", input$ylab)
    titlelinebreak <- gsub("\\\\n", "\\\n", input$title)
    subtitlelinebreak <- gsub("\\\\n", "\\\n", input$subtitle)
    captionlinebreak <- gsub("\\\\n", "\\\n", input$caption)
    
    if (input$xlab!="") {
      p <- p + xlab(xlablinebreak)
      p <- attach_source_dep(p, "xlablinebreak")
    }
    if (input$ylab!="") {
      p <- p + ylab(ylablinebreak)
      p <- attach_source_dep(p, "ylablinebreak")
    }
    
    if (input$themebw) {
      p <- p + theme_bw(base_size=input$themebasesize)     
    }
    
    
    if (!input$themebw){
      p <- p + theme_gray(base_size=input$themebasesize)
    }
    
    plot_margin   <- c(input$margintop,input$marginright,
                    input$marginbottom,input$marginleft)
    legend_margin <- c(input$legendtop,input$legendright,
                     input$legendbottom,input$legendleft)
    legend_box_margin <- c(input$legendboxtop,input$legendboxright,
                           input$legendboxbottom,input$legendboxleft)
    plot_margin[ which(is.na(plot_margin) ) ] <- 0
    legend_margin[ which(is.na(legend_margin) ) ] <- 0
    legend_box_margin[ which(is.na(legend_box_margin) ) ] <- 0
    
    p <- attach_source_dep(p, "plot_margin")
    p <- attach_source_dep(p, "legend_margin")
    p <- attach_source_dep(p, "legend_box_margin")
    
    
    if( input$legendposition=="custom") {
      legendpositiontheme <-  c(input$legendpositionx,
                                input$legendpositiony)
      legendpositiontheme[ which(is.na(legendpositiontheme) ) ] <- 0
    }
    if( input$legendposition!="custom") {
      legendpositiontheme <-  input$legendposition
    } 
    p <- attach_source_dep(p, "legendpositiontheme")
    
    p <-    p + theme(
      panel.background = element_rect(fill=input$backgroundcol),
      panel.ontop = input$panelontop)
    p <-    p + theme(
      legend.position = legendpositiontheme,
      legend.justification = c(input$legendjustificationh,
                               input$legendjustificationv),
      legend.box=input$legendbox,
      legend.box.just = input$legendboxjust)
    p <-    p + theme(
      legend.background = element_rect(fill=input$legendbackground),
      legend.key = element_rect(fill=input$legendkey),
      legend.direction=input$legenddirection)
    p <-    p + theme(
      legend.spacing.x = ggplot2::unit(input$legendspacex*11, "pt"),
      legend.spacing.y = ggplot2::unit(input$legendspacey*11, "pt"))
    p <-    p + theme(
      legend.margin = ggplot2::margin(t = legend_margin[1],r = legend_margin[2],
                                      b = legend_margin[3],l = legend_margin[4],
                                      unit='pt'))
    p <-    p + theme(
      plot.margin =  ggplot2::margin(t = plot_margin[1],r = plot_margin[2],
                                     b = plot_margin[3],l = plot_margin[4],
                                     unit='pt'))
    p <-    p + theme(
      legend.box.margin = ggplot2::margin(t = legend_box_margin[1],r = legend_box_margin[2],
                                          b = legend_box_margin[3],l = legend_box_margin[4],
                                          unit='pt')
    )
    p <-    p + theme(
      plot.title.position =input$titleposition ,
      plot.caption.position =input$captionposition 	)
    
    if (input$labelguides)
      p <-    p +
      theme(legend.title=element_blank())
    if (input$themeaspect)
      p <-    p +
      theme(aspect.ratio=input$aspectratio)
    
    if (grepl("^\\s+$", input$ylab) ){
      p <- p + theme(
        axis.title.y=element_blank())
    }
    if (grepl("^\\s+$", input$xlab) ){
      p <- p + theme(
        axis.title.x=element_blank())
    }
    
    if (!input$rmxaxistickslabels && input$rotatexticks  ){
      if (input$xlabelsize <= 0) {
        x.axis.text <- ggplot2::element_blank()
      } else {
        x.axis.text <- ggplot2::element_text(size = input$xlabelsize,
                                             angle = input$xticksrotateangle,
                                             hjust = input$xtickshjust,
                                             vjust = input$xticksvjust)
      }
      p <-  p+
        theme(axis.text.x = x.axis.text )
      
    }
    if (!input$rmyaxistickslabels && input$rotateyticks){
      if (input$xlabelsize <= 0) {
        y.axis.text <- ggplot2::element_blank()
      } else {
        y.axis.text <- ggplot2::element_text(size = input$ylabelsize,
                                             angle = input$yticksrotateangle,
                                             hjust = input$ytickshjust,
                                             vjust = input$yticksvjust)
      }
      
      p <-  p+
        theme(axis.text.y = y.axis.text )                              
    }  
    if (input$striptextsizex <= 0) {
      x.strip.text <- ggplot2::element_blank()
    } else {
      x.strip.text <- ggplot2::element_text(size = input$striptextsizex,
                                            colour=input$striptextcolourx,
                                            angle= input$facettextxangle,
                                            face = ifelse(input$boldfacettextx,"bold","plain"),
                                            hjust = input$x_facet_text_hjust,
                                            vjust = input$x_facet_text_vjust)
    }
    if (input$striptextsizey <= 0) {
      y.strip.text <- ggplot2::element_blank()
    } else {
      y.strip.text <- ggplot2::element_text(size = input$striptextsizey,
                                            colour=input$striptextcoloury,
                                            angle= input$facettextyangle,
                                            face = ifelse(input$boldfacettexty,"bold","plain"),
                                            hjust = input$y_facet_text_hjust,
                                            vjust = input$y_facet_text_vjust)
    }
    p <- attach_source_dep(p, "y.strip.text")
    p <- attach_source_dep(p, "x.strip.text")
    
    
    p <-  p + theme(
            panel.grid.major.x = element_line(colour = input$majorgridlinescolx),
            panel.grid.minor.x = element_line(colour = input$minorgridlinescolx))
    p <-  p + theme(
            panel.grid.major.y = element_line(colour = input$majorgridlinescoly),
            panel.grid.minor.y = element_line(colour = input$minorgridlinescoly))
    p <- p + theme(
            strip.background.x = element_rect(fill=input$stripbackgroundfillx),
            strip.background.y = element_rect(fill=input$stripbackgroundfilly),
            strip.placement  = input$stripplacement)
    p <- p + theme(
            strip.switch.pad.wrap = unit(input$stripswitchpadwrap*11/ 2, "pt"),
            strip.switch.pad.grid = unit(input$stripswitchpadgrid*11/ 2, "pt"))
    p <- p + theme(
            strip.text.x =  x.strip.text,
            strip.text.y =  y.strip.text,
            strip.text.y.left =  y.strip.text)
    p <- p + theme(
            panel.spacing.x = unit(input$panelspacingx, "lines"),
            panel.spacing.y = unit(input$panelspacingy, "lines"))
    if(input$removexstrip){
    p <-  p +
      theme(strip.background.x = element_blank())
    }
    if(input$removeystrip){
    p <-  p +
      theme(strip.background.y = element_blank())
    }
    
    if(input$rmmajorgridlinesx){
      p <-  p + theme(
          panel.grid.major.x = element_blank())
    }
    if(input$rmminorgridlinesx){
      p <-  p + theme(
        panel.grid.minor.x = element_blank())
    }
    if(input$rmmajorgridlinesy){
      p <-  p + theme(
        panel.grid.major.y = element_blank())
    }
    if(input$rmminorgridlinesy){
      p <-  p+
        theme(panel.grid.minor.y = element_blank())
    }
    
    if(input$rmxaxistickslabels){
      p <-  p+
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    }
    if(input$rmyaxistickslabels){
      p <-  p+
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
    }
    
    if (input$title!="") {
      p <- p + labs(title=titlelinebreak)
      p <- attach_source_dep(p, "titlelinebreak")
    }
    if (input$subtitle!="") {
      p <- p + labs(subtitle=subtitlelinebreak)
      p <- attach_source_dep(p, "subtitlelinebreak")
    }
    if (input$caption!="") {
      p <- p + labs(caption=captionlinebreak)
      p <- attach_source_dep(p, "captionlinebreak")
    }
    p
    
  }
  
  output$plot <- renderPlot({
    plotObject()
  })
  
  output$plotly <- renderPlotly({ggplotly(plotObject(),height = input$height)})
  output$ui_plotly <-  renderUI({plotlyOutput('plotly')})
  
  output$ui_plot <-  renderUI({                 
    plotOutput('plot',  width = "100%" ,height = input$height,
               click = "plot_click",
               hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
               brush = brushOpts(id = "plot_brush"))
  })
  
  
  output$clickheader <-  renderUI({
    df <-finalplotdata()
    validate(       need(!is.null(df), "Please select a data set"))
    h4("Clicked points")
  })
  
  output$brushheader <-  renderUI({
    df <- finalplotdata()
    validate(       need(!is.null(df), "Please select a data set"))
    h4("Brushed points")
    
  })
  
  output$plot_clickedpoints <- renderTable({
    df<- finalplotdata()  
    validate(       need(!is.null(df), "Please select a data set"))
    res <- nearPoints(df, input$plot_click, input$x, "yvalues")
    if (nrow(res) == 0|is.null(res))
      return(NULL)
    res
  })
  output$plot_brushedpoints <- renderTable({
    df<- finalplotdata()  
    validate(       need(!is.null(df), "Please select a data set"))
    res <- brushedPoints(df, input$plot_brush, input$x,"yvalues")
    if (nrow(res) == 0|is.null(res))
      return(NULL)
    res
  })
  
  # ------ Save Plot button in Plot tab ------
  
  # When the save button is clicked, add the plot to a list and clear the input
  observeEvent(input$save_plot_btn, {
    plot_name <- trimws(input$save_plot_name)
    
    if (plot_name %in% names(values$plots)) {
      showModal(
        modalDialog(
          "You already have a plot saved with the same name. Saving this plot will override the existing plot.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_plot_duplicate_confirm", "OK",
                         class = "btn-primary")
          ),
          size = "m"
        )
      )
    } else {
      save_plot()
    }
  })
  observeEvent(input$save_plot_duplicate_confirm, {
    save_plot()
    removeModal()
  })
  save_plot <- function() {
    shinyjs::show("save_plot_checkmark")
    values$plots[[trimws(input$save_plot_name)]] <- plotObject()
    updateTextInput(session, "save_plot_name", value = "")
    shinyjs::delay(
      1000,
      shinyjs::hide("save_plot_checkmark", anim = TRUE, animType = "fade")
    )
  }
  
  # Disable the "save" button if the plot name input is empty
  observe({
    shinyjs::toggle("save_plot_area", condition = !is.null(values$maindata))
    shinyjs::toggleState("save_plot_btn",
                         condition = nzchar(trimws(input$save_plot_name)))
  })
  
  # Don't show the update plot options when there is no plot
  observe({
    shinyjs::toggle("update_plot_area", condition = !is.null(values$maindata))
  })
  observe({
    shinyjs::toggle("update_plot_btn",
                    condition = input$auto_update_plot == FALSE)
  })
  # Signal the app to update the plot manually
  observeEvent(input$update_plot_btn, {
    values$updatePlot <- TRUE
  })
  
  # ----- Export Plots tab -----
  source(file.path("server", "tab-export.R"), local = TRUE)$value
  
  # ----- Plot Code tab ------
  
  # Show the source code of the plot
  output$plotcode <- renderText({
    get_source_code(plotObject())
  })
  
  # for testing purposes
  #values$maindata <- read.csv("data/sample_data.csv", na.strings = c("NA","."))
  
  # ----- Descriptive Stats tab ------
  
  
  output$dstats_col_extra <- renderUI({
    df <-tabledata()
    validate(       need(!is.null(df), "Please select a data set"))
    selectInput("dstatscolextrain", "Extra Column Split:", c("None" = "."))
  })
  
  observe({
    req(tabledata())
    items=names(tabledata())
    names(items)=items
    items= items
    items= items[!is.element(items,input$x)]
    items =c(None='.',items)
    if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
      nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
      items= c(items,nameofcombinedvariables)
    }
    
    # Keep the current value selected unless it's not in the new items list
    current_value <- input$dstatscolextrain
    if (!is.null(current_value) && current_value %in% items) {
      new_value <- current_value
    } else {
      new_value <- items[1]
    }
    updateSelectInput(session, "dstatscolextrain",
                      choices = items, selected = new_value)
  })
  
  output$flipthelevels <- renderUI({
    df <-tabledata()
    validate(need(!is.null(df), "Please select a data set"))
    if(!is.null(df) && !is.null(input$dstatscolextrain) && input$dstatscolextrain!="."){
      checkboxInput('flipthelevelsin', 'Flip the Order of the Columns', value = FALSE)
    }
  })  
  
  dstatsTableData <- reactive({
    df <-tabledata()
    validate(
      need(!is.null(df), "Please select a data set") 
    )
    validate(need(!is.null(input$y), 
                  "No y variable(s) selected"))
    req(input$dstatscolextrain)
    
    tabledata <- df
    if (input$dstatscolextrain != ".") {
      tabledata <- tabledata[, c(input$x, input$dstatscolextrain, input$y)]
    } else {
      tabledata <- tabledata[, c(input$x, input$y)]
    }
    tabledata$.id <- 1:(nrow(tabledata) )
    
    
    vars <- unique(as.character(input$y))
    tabledata[sapply(tabledata, is.character)] <- lapply(tabledata[sapply(tabledata, is.character)], as.factor)
    tabledata
    
    
  })
  
  
  
  
  stats.apply.rounding <- function(x, digits=3, digits.pct=1, round.median.min.max=F) {
    r <- lapply(x, signif_pad, digits=digits)
    r[x == 0] <- prettyNum(0, nsmall=digits-1)  # Fix for special case 0
    nr <- c("N", "FREQ")
    if (!round.median.min.max) {
      nr <- c(nr, "MEDIAN", "MIN", "MAX")
    }
    nr <- nr[nr %in% names(x)]
    r[nr] <- x[nr]
    if (!is.null(x$PCT)) {
      r$PCT <- round(x$PCT, digits.pct)
    }
    r
  }
  
  dstatsRenderCont <- reactive({
    all <- input$dstats_cont_list
    all <- all[all %in% allstats]
    all <- c("None", all)
    
    stats.fun <- list(
      "None"                 = function(x) "",
      "N"                    = function(x) x$N,
      "Mean"                 = function(x) x$MEAN,
      "SD"                   = function(x) x$SD,
      "CV%"                  = function(x) x$CV,
      "Median"               = function(x) x$MEDIAN,
      "q01"                  = function(x) sprintf("%s", x$q01),
      "q02.5"                = function(x) sprintf("%s", x$q02.5),
      "q05"                  = function(x) sprintf("%s", x$q05),
      "q10"                  = function(x) sprintf("%s", x$q10),
      "q25"                  = function(x) sprintf("%s", x$q25),
      "q50"                  = function(x) sprintf("%s", x$q50),
      "q75"                  = function(x) sprintf("%s", x$q75),
      "q90"                  = function(x) sprintf("%s", x$q90),
      "q95"                  = function(x) sprintf("%s", x$q95),
      "q97.5"                = function(x) sprintf("%s", x$q97.5),
      "q99"                  = function(x) sprintf("%s", x$q99),
      "Min"                  = function(x) x$MIN,
      "Max"                  = function(x) x$MAX,
      "IQR"                  = function(x) x$IQR,
      "Q1"                  = function(x) x$Q1,      
      "Q2"                  = function(x) x$Q2,
      "Q3"                  = function(x) x$Q3,
      "T1"                  = function(x) x$T1,
      "T2"                  = function(x) x$T2,
      "Geo. Mean"            = function(x) x$GMEAN,
      "Geo. CV%"             = function(x) x$GCV,
      "Mean (SD)"            = function(x) sprintf("%s (%s)", x$MEAN, x$SD),
      "Mean (CV%)"           = function(x) sprintf("%s (%s)", x$MEAN, x$CV),
      "Mean (SD) (CV%)"      = function(x) sprintf("%s (%s) (%s)", x$MEAN, x$SD, x$CV),
      "Mean (Median)"        = function(x) sprintf("%s (%s)", x$MEAN, x$MEDIAN),
      "[Min, Max]"           = function(x) sprintf("[%s, %s]", x$MIN, x$MAX),
      "Median [Min, Max]"    = function(x) sprintf("%s [%s, %s]", x$MEDIAN, x$MIN, x$MAX),
      "Median [Q1, Q3]"      = function(x) sprintf("%s [%s, %s]", x$MEDIAN, x$Q1, x$Q3),
      "Median [IQR]"         = function(x) sprintf("%s [%s]", x$MEDIAN, x$IQR),
      "Geo. Mean (Geo. CV%)" = function(x) sprintf("%s (%s)", x$GMEAN, x$GCV))
    
    function(x) {
      s <- stats.apply.rounding(stats.default(x), digits=input$dstats_sigfig,
                                round.median.min.max=input$round_median_min_max)
      sapply(all, function(l) stats.fun[[l]](s))
    }
  })
  
  
  
  dstatsTable <- reactive({
    # Don't generate a new table if the user wants to refresh manually
    if (!input$auto_update_table) {
      if (values$updateTable == TRUE) {
        values$updateTable <- FALSE
      } else {
        return(values$prevTable)
      }
    }
    validate(
      need(!is.null(dstatsTableData()), "Please select a data set") 
    )
    
    df <- dstatsTableData() 
    
    if(!is.null(df)) {
      vars <- input$y
      names(vars) <- vars
      for (i in seq_along(vars)) {
        if (i <= quickRelabel$numTotal) {
          lab <- as.character(input[[paste0("quick_relabel_", i)]])
          if (length(lab) > 0) {
            label(df[[vars[i]]]) <- lab
          }
        }
      }
      LHS <- paste(vars, collapse=" + ")
      RHS <- input$x
      if (!is.null(df[[input$dstatscolextrain]])) {
        RHS <- paste(c(RHS, input$dstatscolextrain), collapse=" * ")
        if (!is.null(input$flipthelevelsin)&&input$flipthelevelsin )
        {
          RHS <- paste(c( input$dstatscolextrain,RHS), collapse=" * ")
          
        }
      }
      formula <- as.formula(paste("~", paste(c(LHS, RHS), collapse=" | ")))
      overall <- if (input$table_incl_overall) "Overall" else FALSE
      t <- table1(formula, data=df, overall=overall,
                  topclass=paste("Rtable1", input$table_style),
                  render.continuous=dstatsRenderCont())
      values$prevTable <- t
      t
    }
  })
  
  output$dstats <- renderUI({
    HTML(dstatsTable())
  })
  
  #observe({
  #  df <- values$maindata
  #  if (is.null(df)) return(NULL)
  #  items <- names(df)
  #  names(items) <- items
  #  quickRelabel$labels <- items
  #})
  observeEvent(values$maindata, {
    relabels <- character(0)
  })
  
  
  observe({
    yvars <- input$y
    nyvars <- length(yvars)
    
    for (i in seq_len(quickRelabel$numTotal)) {
      shinyjs::hide(paste0("quick_relabel_", i))
    }
    for (i in seq_len(nyvars)) {
      lab <- as.character(yvars[i])
      if (lab %in% names(relabels)) {
        lab <- as.character(relabels[lab])
      } else {
        relabels[lab] <- lab
      }
      if (i <= quickRelabel$numTotal) {
        updateTextInput(session, 
                        inputId=paste0("quick_relabel_", i),
                        label=if (i==1) "Quick HTML Labels" else NULL,
                        value=lab)
      } else {
        insertUI(
          selector = "#quick_relabel_placeholder", where = "beforeEnd",
          immediate = TRUE,
          div(class = "quick_relabel",
              textInput(
                inputId=paste0("quick_relabel_", i),
                label=if (i==1) "Quick HTML Labels" else NULL,
                value=lab)
          )
        )
        quickRelabel$numTotal <- quickRelabel$numTotal + 1
      }
      shinyjs::show(paste0("quick_relabel_", i))
    }
  })
  
  observe({
    yvars <- input$y
    nyvars <- length(yvars)
    for (i in seq_len(nyvars)) {
      newlab <- as.character(input[[paste0("quick_relabel_", i)]])
      if (length(newlab) == 0) {
        newlab <- as.character(yvars[i])
      }
      relabels[as.character(yvars[i])] <- newlab
    }
  })
  
  # Don't show the table options when there is no table
  observe({
    shinyjs::toggle("table_options_area", condition = !is.null(values$maindata))
  })
  observe({
    shinyjs::toggle("update_table_btn",
                    condition = input$auto_update_table == FALSE)
  })
  # Signal the app to update the table manually
  observeEvent(input$update_table_btn, {
    values$updateTable <- TRUE
  })
  
  if(exists("TESTING") && TESTING) {
    values$maindata <- read.csv("data/sample_data.csv", na.strings = c("NA","."),
                                stringsAsFactors = TRUE)
  }
}
