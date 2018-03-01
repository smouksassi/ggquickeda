function(input, output, session) {
  values <- reactiveValues(
    plots = list(),      # list of plots the user saved
    maindata = NULL,     # the data frame used throughout the app
    updatePlot = FALSE,  # whether to manually update the plot
    prevPlot = NULL,     # the last plot that was successfully plotted
    updateTable = FALSE  # whether to manually update the dstats table
  )
  
  # If this app was launched from a function that explicitly set an initial dataset
  if (exists("ggplotwithyourdata_init_data")) {
    values$maindata <- get("ggplotwithyourdata_init_data")
  }
  
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
    
    df <- recodedata3()
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
    
    if (changeLblsVals$numCurrent <= changeLblsVals$numTotal) {
      # if we already had this many sections before, no need to wire up any
      # new observers
    } else {
      num1 <- changeLblsVals$numCurrent
      changeLblsVals$numTotal <- num1
      
      output[[paste0("factor_lvl_change_labeltext_", num1)]] <- renderText({
        df <- recodedata3()
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

        df <- recodedata3()
        MODEDF <- sapply(df, is.numeric)
        
        ALLNAMES <- names(df)[!MODEDF]
        ALLNAMES <- ALLNAMES[!ALLNAMES=="custombins"]
        if (changeLblsVals$numCurrent < length(ALLNAMES)) {
          shinyjs::enable("factor_lvl_change_add")
        }
        df <- recodedata3()
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
                        edit the labels via Backspace/Enter keys. Drag and Drop the items to the desired order."),
          choices = levelsvalues,
          selected = levelsvalues,
          options = list(
            create = TRUE, createOnBlur = TRUE,
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
    values$maindata <- read.csv(file, na.strings = c("NA","."))
  })
  
  # Load sample dataset
  observeEvent(input$sample_data_btn, {
    file <- "data/sample_data.csv"
    values$maindata <- read.csv(file, na.strings = c("NA","."))
  })
  
  # when recodedata3 changes, reset the dynamic "change factor levels" boxes
  observeEvent(recodedata3(), {
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
  
  observeEvent(input$gridlinescolreset, {
      shinyjs::reset("gridlinescol")
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
  
  # If an X is selected but no Y is selected, switch to the Histograms tab
  observe({
    if (!is.null(input$x) && is.null(input$y)) {
      updateTabsetPanel(session, "graphicaltypes", "histograms_density")
    }
  })
  observe({
    if (input$yaxisscale=="lineary") {
      updateRadioButtons(session, "yaxisformat", choices = c("default" = "default",
                                                             "Comma separated" = "scientificy",
                                                             "Percent" = "percenty"))
    }
    if (input$yaxisscale!="lineary") {
      updateRadioButtons(session, "yaxisformat", choices = c("default" = "default",
                                                             "Log 10^x Format" = "logyformat"))
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
                                                             "Log 10^x Format" = "logxformat"))
    }
  })
  
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

  output$catvar2 <- renderUI({
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
  
  
  
  
  recodedata1  <- reactive({
    df <- values$maindata 
    validate(       need(!is.null(df), "Please select a data set"))
    if(!is.null(input$catvarin)&length(input$catvarin ) >=1) {
      for (i in 1:length(input$catvarin ) ) {
        varname<- input$catvarin[i]
        df[,varname] <- cut(df[,varname],input$ncuts , include.lowest = TRUE, right = FALSE, ordered_result = TRUE)
        df[,varname]   <- as.factor( df[,varname])
      }
    }
    df
  })
  
  
  recodedata2  <- reactive({
    df <- recodedata1()
    validate(       need(!is.null(df), "Please select a data set"))
    if(!is.null(input$catvar2in) ){
      if(length(input$catvar2in ) >=1) {
        for (i in 1:length(input$catvar2in ) ) {
          varname<- input$catvar2in[i]
          df[,varname]   <- as.factor( df[,varname])
        }
      }  
    }
    df
  })
  
  recodedata3  <- reactive({
    df <- recodedata2()
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
    df <- recodedata3()
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
    df <- recodedata3()
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
      
      new_labels <- unlist(strsplit(labels, ","))[1:nlevels(df[, variable_name])]
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
    selectizeInput("onerowidgroupin", "ID(s):", choices = items,multiple=TRUE,
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
  
  
  
  
  output$roundvar <- renderUI({
    df <- filterdata7()
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
    df <- filterdata7()
    validate(       need(!is.null(df), "Please select a data set"))
    if(!is.null(input$roundvarin)&length(input$roundvarin ) >=1) {
      for (i in 1:length(input$roundvarin ) ) {
        varname<- input$roundvarin[i]
        df[,varname]   <- round( df[,varname],input$rounddigits)
      }
    }
    df
  })  
  
  
  tabledata <- reactive({
    df <- rounddata() 
    df
  })
  
  stackdata <- reactive({
    
    df <- rounddata() 
    validate(       need(!is.null(df), "Please select a data set"))
    if (!is.null(df)){
      validate(  need(!is.element(input$x,input$y) ,
                      "Please select a different x variable or remove the x variable from the list of y variable(s)"))
      
      tidydata <- NULL
      if(!is.null(input$y) ){
        
        validate(need(all(input$y %in% names(df)), "Invalid y value(s)"))

        tidydata <- df %>%
          gather( "yvars", "yvalues", gather_cols=as.vector(input$y) ,factor_key = TRUE) 
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
    items= c("None",items, "yvars","yvalues") 
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
      if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
        nameofcombinedvariables<- paste(as.character(input$pastevarin),collapse="_",sep="") 
        items= c(items,nameofcombinedvariables)    
      }
    }
    if (length(input$y) > 1 ){
      items= c("yvars",None=".",items, "yvalues")    
      if (!is.null(input$pastevarin)&length(input$pastevarin) >1 ){
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
    selectInput("weightin", "Weight By:",items )
  })
  outputOptions(output, "pointsize", suspendWhenHidden=FALSE)
  outputOptions(output, "fill", suspendWhenHidden=FALSE)
  outputOptions(output, "weight", suspendWhenHidden=FALSE)
  
  
  
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
  
  
  
  plotObject <- reactive({
    # Don't generate a new plot if the user wants to refresh manually
    if (!input$auto_update_plot) {
      if (values$updatePlot == TRUE) {
        values$updatePlot <- FALSE
      } else {
        return(values$prevPlot)
      }
    }
    plotdata <- finalplotdata()
    validate(need(!is.null(plotdata), "Please select a data set") )
    
    if(!is.null(plotdata)) {
      
      if (input$themecontcolorswitcher=="RedWhiteBlue"){
        scale_colour_continuous<- function(...) 
          scale_colour_gradient2(..., low = muted("red"), mid = "white",
                                 high = muted("blue"), midpoint = 0, space = "Lab",
                                 na.value = "grey50", guide = "colourbar")
      }
      if (input$themecontcolorswitcher=="RedWhiteGreen"){
        scale_colour_continuous<- function(...) 
          scale_colour_gradient2(..., low = muted("red"), mid = "white",
                                 high = muted("darkgreen"), midpoint = 0, space = "Lab",
                                 na.value = "grey50", guide = "colourbar")
      }
      if (input$themecolorswitcher=="themetableau10"){
        scale_colour_discrete <- function(...) 
          scale_colour_manual(..., values = tableau10,drop=!input$themecolordrop)
        scale_fill_discrete <- function(...) 
          scale_fill_manual(..., values = tableau10,drop=!input$themecolordrop)
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
      
      if (!is.null(input$y) ){
        
        if(input$addcorrcoeff){
          if( input$addcorrcoeffignoregroup){
            listvarcor <- c(input$colorin,input$fillin,
                            input$facetrowin,input$facetcolin,input$facetrowextrain,input$facetcolextrain)
          }
          if( !input$addcorrcoeffignoregroup){
            listvarcor <- c(input$colorin,input$fillin,input$groupin,
                            input$facetrowin,input$facetcolin,input$facetrowextrain,input$facetcolextrain)
          }
          listvarcor <- listvarcor[!is.element(listvarcor,c("None",".")) ]
          listvarcor <- listvarcor[!duplicated(listvarcor) ]
          listvarcor <- c("yvars",listvarcor)
          if (!is.numeric(plotdata[,"yvalues"]) ){
            cors <- NULL
          }
          if (!is.numeric(plotdata[,input$x]) ){
            cors <- NULL
          }
          if (all( is.numeric(plotdata[,"yvalues"])&&is.numeric(plotdata[,input$x]) ) ){
            if (length(listvarcor)<=1){
              cors <-  plotdata %>%
                group_by(!!!syms("yvars")) %>%
                dplyr::summarize(corcoeff = round(cor(!!as.name(input$x),!!as.name("yvalues"),use="complete.obs"),2))
            }
            if (length(listvarcor)>=2){
              cors <- plotdata %>%
                group_by_at(.vars= listvarcor ) %>%
                dplyr::summarize(corcoeff = round(cor(!!as.name(input$x),!!as.name("yvalues"),use="complete.obs"),2))
            }
            cors<-as.data.frame(cors)
            
          } 
        }
        
        
        
        p <- sourceable(ggplot(plotdata, aes_string(x=input$x, y="yvalues")))
        
        if (input$showtarget)  {
          if ( is.numeric( plotdata[,input$x] ) ) {
            p <-   p   +
              annotate("rect", xmin = -Inf, xmax = Inf, ymin = input$lowerytarget,
                       ymax = input$uppertarget,fill=input$targetcol,
                       alpha =input$targetopacity)  
          } 
          
        } 
        
        
        if (input$colorin != 'None')
          p <- p + aes_string(color=input$colorin)
        if (input$fillin != 'None')
          p <- p + aes_string(fill=input$fillin)
        if (input$pointsizein != 'None')
          p <- p  + aes_string(size=input$pointsizein)
        
        # if (input$groupin != 'None' & !is.factor(plotdata[,input$x]))
        if (input$groupin != 'None')
          p <- p + aes_string(group=input$groupin)
        if (input$groupin == 'None' & !is.numeric(plotdata[,input$x]) 
            & input$colorin == 'None')
          p <- p + aes(group=1)
        
        if (input$Points=="Points"&input$pointsizein == 'None'&!input$pointignorecol)
          p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes)  
        if (input$Points=="Points"&input$pointsizein != 'None'&!input$pointignorecol)
          p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes)
        
        if (input$jitterdirection=="Vertical"){
          positionj<- position_jitter(width=0)
        }
        if (input$jitterdirection=="Horizontal"){
          positionj<-  position_jitter(height=0)
        }
        
        if (input$jitterdirection=="Both"){
          positionj<-  position_jitter()
        }
        if (input$jitterdirection=="Custom"){
          positionj<-  position_jitter(height=input$jittervertical,width=input$jitterhorizontal)
        }
        if (input$Points=="Jitter"&input$pointsizein == 'None'&!input$pointignorecol)
          p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes,
                               position=positionj)
        if (input$Points=="Jitter"&input$pointsizein != 'None'&!input$pointignorecol)
          p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes,position=positionj)
        
        
        
        if (input$Points=="Points"&input$pointsizein == 'None'&input$pointignorecol)
          p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes,colour=input$colpoint)  
        if (input$Points=="Points"&input$pointsizein != 'None'&input$pointignorecol)
          p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes,colour=input$colpoint)
        
        if (input$Points=="Jitter"&input$pointsizein == 'None'&input$pointignorecol)
          p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes,colour=input$colpoint)
        if (input$Points=="Jitter"&input$pointsizein != 'None'&input$pointignorecol)
          p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes,colour=input$colpoint)
        
        
        
        if (input$line=="Lines"&input$pointsizein == 'None'& !input$lineignorecol)
          p <- p + geom_line(,size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes)
        if (input$line=="Lines"&input$pointsizein != 'None'& !input$lineignorecol& !input$lineignoresize)
          p <- p + geom_line(,alpha=input$linestransparency,linetype=input$linetypes)
        if (input$line=="Lines"&input$pointsizein != 'None'& !input$lineignorecol& input$lineignoresize)
          p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes)
        
        if (input$line=="Lines"&input$pointsizein == 'None'&input$lineignorecol)
          p <- p + geom_line(,size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
        if (input$line=="Lines"&input$pointsizein != 'None'& input$lineignorecol& !input$lineignoresize)
          p <- p + geom_line(,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
        if (input$line=="Lines"&input$pointsizein != 'None'& input$lineignorecol & input$lineignoresize )
          p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
        
        #### Boxplot Section START
        
        if (input$boxplotaddition){
          if (input$groupin != 'None'& !input$boxplotignoregroup ){
            if (!input$boxplotignorecol){
              p <- p + aes_string(group=input$groupin)
              p <- p + geom_boxplot(varwidth = input$boxplotvarwidh,notch = input$boxplotnotch,show.legend=input$boxplotshowlegend,alpha=input$boxplotalpha)
              
            }
            if (input$boxplotignorecol){
              p <- p + aes_string(group=input$groupin)
              p <- p + geom_boxplot(varwidth = input$boxplotvarwidh,notch = input$boxplotnotch,show.legend=input$boxplotshowlegend,col=input$boxcolline,alpha=input$boxplotalpha)
              
            }
          }
          if (input$groupin == 'None'){
            if (!input$boxplotignorecol){
              p <- p + geom_boxplot(aes(group=NULL),varwidth = input$boxplotvarwidh,notch = input$boxplotnotch,show.legend=input$boxplotshowlegend,alpha=input$boxplotalpha)
            }
            if (input$boxplotignorecol){
              p <- p + geom_boxplot(aes(group=NULL),varwidth = input$boxplotvarwidh,notch = input$boxplotnotch,show.legend=input$boxplotshowlegend,col=input$boxcolline,alpha=input$boxplotalpha)
            } 
          }
          
          
          if (input$boxplotignoregroup ){
            if (!input$boxplotignorecol){
              p <- p + geom_boxplot(aes(group=NULL),varwidth = input$boxplotvarwidh,notch = input$boxplotnotch,show.legend=input$boxplotshowlegend,alpha=input$boxplotalpha)
            }
            if (input$boxplotignorecol){
              p <- p + geom_boxplot(aes(group=NULL),varwidth = input$boxplotvarwidh,notch = input$boxplotnotch,show.legend=input$boxplotshowlegend,col=input$boxcolline,alpha=input$boxplotalpha)
            }
          }
          
          
        }
        #### Boxplot Section END
        
        
        ###### Mean section  START 
        
        
        if (!input$meanignoregroup) {
          if (!input$meanignorecol) {
            
            if (input$Mean=="Mean") {
              if(input$meanlines&input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_single(mean, geom = "line")
              if(input$meanlines&input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_single(mean, geom = "line",size=input$meanlinesize)
              
              
              if(input$meanpoints)           
                p <- p + 
                  stat_sum_single(mean, geom = "point")
              
            }
            
            if (input$Mean=="Mean/CI"){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "errorbar",fun.args=list(conf.int=input$CI),width=input$errbar)
              if(input$meanlines&input$pointsizein != 'None')  
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "line")
              if(input$meanlines&input$pointsizein == 'None')  
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "line",size=input$meanlinesize)
              if(input$meanpoints)           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point")
              
            }
          }
          
          
          if (input$meanignorecol) {
            meancol <- input$colmean
            if (input$Mean=="Mean") {
              if(input$meanlines&input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_single(mean, geom = "line",col=meancol)
              
              if(input$meanlines&input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_single(mean, geom = "line",col=meancol,size=input$meanlinesize)
              
              if(input$meanpoints)           
                p <- p + 
                  stat_sum_single(mean, geom = "point",col=meancol)
              
            }
            
            if (input$Mean=="Mean/CI"){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "errorbar",fun.args=list(conf.int=input$CI),width=input$errbar, col=meancol)
              if(input$meanlines&input$pointsizein != 'None')  
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "line", col=meancol)
              if(input$meanlines&input$pointsizein == 'None')  
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "line", col=meancol,size=input$meanlinesize)
              
              if(input$meanpoints)           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point", col=meancol)
              
            }
          }
        }
        
        if (input$meanignoregroup) {
          if (!input$meanignorecol) {
            
            if (input$Mean=="Mean") {
              if(input$meanlines&input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_single(mean, geom = "line",aes(group=NULL))
              if(input$meanlines&input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_single(mean, geom = "line",aes(group=NULL),size=input$meanlinesize)
              
              if(input$meanpoints)           
                p <- p + 
                  stat_sum_single(mean, geom = "point",aes(group=NULL))
              
            }
            
            if (input$Mean=="Mean/CI"){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "errorbar",fun.args=list(conf.int=input$CI), width=input$errbar,aes(group=NULL))
              if(input$meanlines&input$pointsizein != 'None')  
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "line",aes(group=NULL))
              if(input$meanlines&input$pointsizein == 'None')  
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "line",aes(group=NULL),size=input$meanlinesize)
              if(input$meanpoints)           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",aes(group=NULL))
              
            }
          }
          
          
          if (input$meanignorecol) {
            meancol <- input$colmean
            if (input$Mean=="Mean") {
              if(input$meanlines&input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_single(mean, geom = "line",col=meancol,aes(group=NULL))
              if(input$meanlines&input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_single(mean, geom = "line",col=meancol,aes(group=NULL),size=input$meanlinesize)
              
              
              if(input$meanpoints)           
                p <- p + 
                  stat_sum_single(mean, geom = "point",col=meancol,aes(group=NULL))
              
            }
            
            if (input$Mean=="Mean/CI"){
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "errorbar",fun.args=list(conf.int=input$CI), width=input$errbar, col=meancol, aes(group=NULL))
              if(input$meanlines&input$pointsizein != 'None')  
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "line",col=meancol,aes(group=NULL))
              if(input$meanlines&input$pointsizein == 'None')  
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "line",col=meancol,aes(group=NULL),size=input$meanlinesize)
              
              if(input$meanpoints)           
                p <- p + 
                  stat_sum_df("mean_cl_normal", geom = "point",col=meancol,aes(group=NULL))
              
            }
          }
        }
        ###### Mean section  END 
        
        ###### Smoothing Section START
        if(!is.null(input$Smooth) ){
          familyargument <- input$loessfamily
          if(input$smoothmethod=="glm") {
            familyargument<- "binomial"  
          }
          methodsargument<- list(family = familyargument,degree=input$loessdegree) 
          if(input$smoothmethod=="glm"){
            methodsargument<- list(family = familyargument) 
          }
          
          spanplot <- input$loessens
          
          if ( input$ignoregroup) {
            if (!input$ignorecol) {
              if (input$Smooth=="Smooth")
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=F,span=spanplot,aes(group=NULL))
              
              if (input$Smooth=="Smooth and SE")
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=T,span=spanplot,aes(group=NULL))
              
              if (input$Smooth=="Smooth"& input$weightin != 'None')
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=F,span=spanplot,aes(group=NULL))+  
                  aes_string(weight=input$weightin)
              
              if (input$Smooth=="Smooth and SE"& input$weightin != 'None')
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=T,span=spanplot,aes(group=NULL))+  
                  aes_string(weight=input$weightin)
            }
            if (input$ignorecol) {
              colsmooth <- input$colsmooth
              if (input$Smooth=="Smooth")
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=F,span=spanplot,col=colsmooth,aes(group=NULL))
              
              if (input$Smooth=="Smooth and SE")
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=T,span=spanplot,col=colsmooth,aes(group=NULL))
              
              if (input$Smooth=="Smooth"& input$weightin != 'None')
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=F,span=spanplot,col=colsmooth,aes(group=NULL))+  
                  aes_string(weight=input$weightin)
              
              if (input$Smooth=="Smooth and SE"& input$weightin != 'None')
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=T,span=spanplot,col=colsmooth,aes(group=NULL))+  
                  aes_string(weight=input$weightin)
            }
            
          }
          
          if ( !input$ignoregroup) {
            if (!input$ignorecol) {
              if (input$Smooth=="Smooth")
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=F,span=spanplot)
              
              if (input$Smooth=="Smooth and SE")
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=T,span=spanplot)
              
              if (input$Smooth=="Smooth"& input$weightin != 'None')
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=F,span=spanplot)+  
                  aes_string(weight=input$weightin)
              
              if (input$Smooth=="Smooth and SE"& input$weightin != 'None')
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=T,span=spanplot)+  
                  aes_string(weight=input$weightin)
            }
            if (input$ignorecol) {
              colsmooth <- input$colsmooth
              if (input$Smooth=="Smooth")
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=F,span=spanplot,col=colsmooth)
              
              if (input$Smooth=="Smooth and SE")
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=T,span=spanplot,col=colsmooth)
              
              if (input$Smooth=="Smooth"& input$weightin != 'None')
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=F,span=spanplot,col=colsmooth)+  
                  aes_string(weight=input$weightin)
              
              if (input$Smooth=="Smooth and SE"& input$weightin != 'None')
                p <- p + geom_smooth(method=input$smoothmethod,
                                     method.args = methodsargument,
                                     size=1.5,se=T,span=spanplot,col=colsmooth)+  
                  aes_string(weight=input$weightin)
            }
            
          }
          
          ###### smooth Section END
        }
        
        
        ###### Median PI section  START  
        if (!input$medianignoregroup) {
          
          if (!input$medianignorecol) {
            
            if (input$Median=="Median") {
              if(input$medianlines&input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_single(median, geom = "line")
              
              if(input$medianlines&input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_single(median, geom = "line",size=input$medianlinesize)
              
              
              if(input$medianpoints)           
                p <- p + 
                  stat_sum_single(median, geom = "point")
              
            }
            
            if (input$Median=="Median/PI"&input$pointsizein == 'None'){
              p <- p + 
                stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI) ,size=input$medianlinesize,alpha=input$PItransparency,col=NA)+ 
                stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI) ,size=input$medianlinesize,alpha=0)
              
              if ( input$sepguides )
                p <-   p + 
                  guides(
                    color = guide_legend(paste("Median"),
                                         override.aes = list(shape =NA,fill=NA)),
                    fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                         override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                    ) )
              
            }
            
            if (input$Median=="Median/PI"&input$pointsizein != 'None'){
              p <- p + 
                stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI), alpha=input$PItransparency,col=NA)+
                stat_sum_df("median_hilow", geom = "smooth"  ,fun.args=list(conf.int=input$PI),alpha=0)
              
              if ( input$sepguides )
                p <-   p +
                  guides(
                    color = guide_legend(paste("Median"),
                                         override.aes = list(shape =NA,fill=NA)),
                    fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                         override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                    ) )
              
            }
            
            if (input$Median!="None" & input$medianvalues )  {
              p <-   p   +
                stat_summary(fun.data = median.n,geom = "label_repel",alpha=0.4,
                             fun.y = median, fontface = "bold",
                             show.legend=FALSE,size=6)}
            if (input$Median!="None" & input$medianN)  {
              p <-   p   +
                stat_summary(fun.data = give.n, geom = "label_repel",alpha=0.4,
                             fun.y = median, fontface = "bold", 
                             show.legend=FALSE,size=6)      
            }  
          }
          
          
          
          if (input$medianignorecol) {
            mediancol <- input$colmedian
            if (input$Median=="Median") {
              if(input$medianlines&input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_single(median, geom = "line",col=mediancol)
              
              if(input$medianlines&input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_single(median, geom = "line",col=mediancol,size=input$medianlinesize)
              
              if(input$medianpoints)           
                p <- p + 
                  stat_sum_single(median, geom = "point",col=mediancol)
              
            }
            
            if (input$Median=="Median/PI"&input$pointsizein == 'None'){
              p <- p + 
                stat_sum_df("median_hilow", geom = "ribbon", fun.args=list(conf.int=input$PI), alpha=input$PItransparency,col=NA)+
                stat_sum_df("median_hilow", geom = "smooth", fun.args=list(conf.int=input$PI), size=input$medianlinesize,col=mediancol,alpha=0)
              
              if ( input$sepguides )
                p <-   p +
                  guides(
                    color = guide_legend(paste("Median"),
                                         override.aes = list(shape =NA,fill=NA)),
                    fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                         override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                    ) )
            }
            if (input$Median=="Median/PI"&input$pointsizein != 'None'){
              p <- p + 
                stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),alpha=input$PItransparency,col=NA)+
                stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),col=mediancol,
                            alpha=0)          
              
              if ( input$sepguides )
                p <-   p +
                  guides(
                    color = guide_legend(paste("Median"),
                                         override.aes = list(shape =NA,fill=NA)),
                    fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                         override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                    ) )
            }
            if (input$Median!="None" & input$medianvalues )  {
              p <-   p   +
                stat_summary(fun.data = median.n,geom = "label_repel",alpha=0.4,
                             fun.y = median, fontface = "bold",colour=mediancol,
                             show.legend=FALSE,size=6)}
            if (input$Median!="None" & input$medianN)  {
              p <-   p   +
                stat_summary(fun.data = give.n, geom = "label_repel",alpha=0.4,
                             fun.y = median, fontface = "bold", colour=mediancol,
                             show.legend=FALSE,size=6)      
            }       
            
          }
        }
        
        
        if (input$medianignoregroup) {
          if (!input$medianignorecol) {
            if (input$Median=="Median") {
              if(input$medianlines&input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_single(median, geom = "line",aes(group=NULL))
              if(input$medianlines&input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_single(median, geom = "line",aes(group=NULL),size=input$medianlinesize)
              
              if(input$medianpoints)           
                p <- p + 
                  stat_sum_single(median, geom = "point",aes(group=NULL))
              
            }
            
            if (input$Median=="Median/PI"&input$pointsizein == 'None'){
              p <- p + 
                stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=input$PItransparency,col=NA)+ 
                stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),aes(group=NULL),size=input$medianlinesize,alpha=0)   
              if ( input$sepguides )
                p <-   p +
                  guides(
                    color = guide_legend(paste("Median"),
                                         override.aes = list(shape =NA,fill=NA)),
                    fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                         override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                    ) )
            }
            
            if (input$Median=="Median/PI"&input$pointsizein != 'None'){
              p <- p + 
                stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=input$PItransparency,col=NA)+ 
                stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=0)
              if ( input$sepguides )
                p <-   p +
                  guides(
                    color = guide_legend(paste("Median"),
                                         override.aes = list(shape =NA,fill=NA)),
                    fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                         override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                    ) )
            }
            if (input$Median!="None" & input$medianvalues )  {
              p <-   p   +
                stat_summary(fun.data = median.n, aes(group=NULL),geom = "label_repel",alpha=0.4,
                             fun.y = median, fontface = "bold",fill="white",
                             show.legend=FALSE,
                             size=6)}
            if (input$Median!="None" & input$medianN)  {
              p <-   p   +
                stat_summary(fun.data = give.n, aes(group=NULL), geom = "label_repel",alpha=0.4,
                             fun.y = median, fontface = "bold", fill="white",
                             show.legend=FALSE,size=6)      
            }
            
            
          }
          
          
          if (input$medianignorecol) {
            mediancol <- input$colmedian
            if (input$Median=="Median") {
              if(input$medianlines&input$pointsizein != 'None')           
                p <- p + 
                  stat_sum_single(median, geom = "line",col=mediancol,aes(group=NULL))
              if(input$medianlines&input$pointsizein == 'None')           
                p <- p + 
                  stat_sum_single(median, geom = "line",col=mediancol,aes(group=NULL),size=input$medianlinesize)
              
              if(input$medianpoints)           
                p <- p + 
                  stat_sum_single(median, geom = "point",col=mediancol,aes(group=NULL))
              
            }
            
            if (input$Median=="Median/PI"&input$pointsizein == 'None'){
              p <- p + 
                stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=input$PItransparency,col=NA)+ 
                stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),col=mediancol,aes(group=NULL),size=input$medianlinesize,alpha=0)
              if ( input$sepguides )
                p <-   p +
                  guides(
                    color = guide_legend(paste("Median"),
                                         override.aes = list(shape =NA,fill=NA)),
                    fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                         override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                    ) )
            }
            if (input$Median=="Median/PI"&input$pointsizein != 'None'){
              p <- p + 
                stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=input$PItransparency,col=NA)+ 
                stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),col=mediancol,aes(group=NULL),alpha=0)
              
              
              
              if ( input$sepguides )
                p <-   p +
                  guides(
                    color = guide_legend(paste("Median"),
                                         override.aes = list(shape =NA,fill=NA)),
                    fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                         override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                    ) )
            }
            
            
            if (input$Median!="None" & input$medianvalues )  {
              p <-   p   +
                stat_summary(fun.data = median.n, aes(group=NULL),geom = "label_repel",alpha=0.4,
                             fun.y = median, fontface = "bold",colour=mediancol,
                             show.legend=FALSE,size=6)}
            if (input$Median!="None" & input$medianN)  {
              p <-   p   +
                stat_summary(fun.data = give.n, aes(group=NULL), geom = "label_repel",alpha=0.4,
                             fun.y = median, fontface = "bold", colour=mediancol,
                             show.legend=FALSE,size=6)      
            }
            
          }
        }
        
        
        
        ###### Median PI section  END
        
        
        
        ###### RQSS SECTION START  
        if (!input$ignoregroupqr) {
          if (!input$ignorecolqr) {
            if (input$Tauvalue) {
              if(!input$hidedynamic){
                p <- p +  stat_quantile(method = "rqss",quantiles =input$Tau,size=1.5,
                                        linetype="solid", 
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))       
              }
              
              if (input$mid)
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.5,size=1.5,
                                        linetype="solid",
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              if (input$ninetieth)
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.90,size=1,
                                        linetype="dashed",
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              if (input$tenth)
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.1,size=1,
                                        linetype="dashed",
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$up)
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.95,size=1,
                                        linetype="dashed",
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$low) 
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.05,size=1,
                                        linetype="dashed",
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$ninetyseventh)
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.97,size=1,
                                        linetype="dashed",
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$third) 
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.03,size=1,
                                        linetype="dashed",
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              
              
            }
          }
          if (input$ignorecolqr) {
            colqr <- input$colqr
            if (input$Tauvalue) {
              if(!input$hidedynamic){
                p <- p +  stat_quantile(method = "rqss",quantiles =input$Tau,size=1.5,
                                        linetype="solid", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty)) 
              }
              
              
              if (input$mid)
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.5,size=1.5,
                                        linetype="solid", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              if (input$ninetieth)
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.90,size=1,
                                        linetype="dashed", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              if (input$tenth)
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.1,size=1,
                                        linetype="dashed", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$up)
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.95,size=1,
                                        linetype="dashed", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$low) 
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.05,size=1,
                                        linetype="dashed", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$ninetyseventh)
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.97,size=1,
                                        linetype="dashed", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$third) 
                p <- p +  stat_quantile(method = "rqss",quantiles = 0.03,size=1,
                                        linetype="dashed", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
            }
          }
        }
        
        
        if (input$ignoregroupqr) {
          if (!input$ignorecolqr) {
            if (input$Tauvalue) {
              if(!input$hidedynamic){
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles =input$Tau,size=1.5,
                                        linetype="solid",
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty)) 
              }
              
              if (input$mid)
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.5,size=1.5,
                                        linetype="solid",
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              if (input$ninetieth)
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.90,size=1,
                                        linetype="dashed", 
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              if (input$tenth)
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.1,size=1,
                                        linetype="dashed",
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$up)
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.95,size=1,
                                        linetype="dashed", 
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$low) 
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.05,size=1,
                                        linetype="dashed", 
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              if (input$ninetyseventh)
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.97,size=1,
                                        linetype="dashed", 
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              if (input$third)
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.03,size=1,
                                        linetype="dashed", 
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))     
            }
          }
          if (input$ignorecolqr) {
            colqr <- input$colqr
            if (input$Tauvalue) {
              if(!input$hidedynamic){
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles =input$Tau,size=1.5,
                                        linetype="solid",col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))     
              }
              
              
              if (input$mid)
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.5,size=1.5,
                                        linetype="solid", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              if (input$ninetieth)
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.90,size=1,
                                        linetype="dashed", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              if (input$tenth)
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.1,size=1,
                                        linetype="dashed", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$up)
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.95,size=1,
                                        linetype="dashed", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$low) 
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.05,size=1,
                                        linetype="dashed", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              if (input$ninetyseventh)
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.97,size=1,
                                        linetype="dashed", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
              
              if (input$third) 
                p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.03,size=1,
                                        linetype="dashed", col=colqr,
                                        formula=y ~ qss(x, constraint= input$Constraints,
                                                        lambda=input$Penalty))
            }
          }
        }
        
        
        ###### RQSS SECTION END
        
        #### Corr coefficient Start
        if(input$addcorrcoeff&&!is.null(cors)&&!input$addcorrcoeffignoregroup) {
          p <- p +
            geom_text_repel(data=data.frame(cors), aes(label=paste("italic(r) == ", corcoeff)), 
                            x=Inf, y=Inf, parse=TRUE,size=5)
        }
        
        if(input$addcorrcoeff&&!is.null(cors)&&input$addcorrcoeffignoregroup) {
          p <- p +
            geom_text_repel(data=data.frame(cors), aes(group=NULL,label=paste("italic(r) == ", corcoeff)), 
                            x=Inf, y=Inf, parse=TRUE,size=5)
        }
        
        
        #### Corr coefficient END
        
        
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
          if (input$groupin != 'None' & !is.factor(plotdata[,input$x]))
            p <- p + aes_string(group=input$groupin)
        }
        p <- p + xlab(input$x)
        if (input$KM=="KM/CI") {
          p <- p +
            geom_kmband(alpha=input$KMCItransparency,conf.int = input$KMCI,trans=input$KMtrans)
          }
        
        
        if (input$KM!="None") {
          p  <- p +
            geom_smooth(stat="km",trans=input$KMtrans)
        
        if (input$censoringticks) {
          p  <- p +
            geom_kmticks(trans=input$KMtrans)
        }
        }
        ###### KM SECTION END
      } 
      ###### Univariate SECTION START
      
      if (is.null(input$y) ) {
        
        if(is.numeric(plotdata[,input$x]) ){
          #validate(       need(is.numeric(plotdata[,input$x]), "Please select a numeric x variable"))
          p <- sourceable(ggplot(plotdata, aes_string(x=input$x)))
          if (input$colorin != 'None')
            p <- p + aes_string(color=input$colorin)
          if (input$fillin != 'None')
            p <- p + aes_string(fill=input$fillin)
          if (input$groupin != 'None')
            p <- p + aes_string(group=input$groupin)
          if (input$groupin == 'None' & !is.numeric(plotdata[,input$x]) 
              & input$colorin == 'None')
            p <- p + aes(group=1)
          
          if ( input$histogramaddition){
            p <- p+ aes(y=..density..)+
              geom_histogram(alpha=0.2)
          }
          if ( input$densityaddition){
            p <- p+
              geom_density(alpha=0.1)
            
          }
        }
        
        if(!is.numeric(plotdata[,input$x]) ){
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
            p <- p+ 
              geom_bar(alpha=0.2,position = eval(parse(text=input$positionbar)))+
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
            
            
            p <- p+   scale_y_continuous(labels = percent) +
              ylab("Percentage")
            if ( input$barplotflip){
              p <- p +
                coord_flip()
            }
          }
          
        }
        
        
        
        
        
      }
      
      
      ###### Univariate SECTION END
      
      
      facets <- paste(input$facetrowin,'~', input$facetcolin)
      
      if (input$facetrowextrain !="."&input$facetrowin !="."){
        facets <- paste(input$facetrowextrain ,"+", input$facetrowin, '~', input$facetcolin)
      }  
      if (input$facetrowextrain !="."&input$facetrowin =="."){
        facets <- paste( input$facetrowextrain, '~', input$facetcolin)
      }  
      
      if (input$facetcolextrain !="."){
        facets <- paste( facets, "+",input$facetcolextrain)
      }  
      ASTABLE <- ifelse( input$facetordering=="table",TRUE,FALSE)
      
      if (facets != '. ~ .')
        p <- p + facet_grid(facets,scales=input$facetscalesin,space=input$facetspace
                            ,labeller=input$facetlabeller,margins=input$facetmargin,as.table=ASTABLE )
      
      if (facets != '. ~ .' & input$facetswitch!="" )
        
        p <- p + facet_grid(facets,scales=input$facetscalesin,space=input$facetspace,
                            switch=input$facetswitch
                            , labeller=input$facetlabeller,
                            margins=input$facetmargin ,as.table=ASTABLE)
      
      if (facets != '. ~ .'&input$facetwrap) {
        multiline <-  input$facetwrapmultiline
        
        p <- p + facet_wrap(    c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain ) [
          c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain )!="."]
          ,scales=input$facetscalesin,
          labeller=label_wrap_gen(width = 25, multi_line = multiline),as.table=ASTABLE)
        
        if (input$facetwrap&input$customncolnrow) {
          multiline <-  input$facetwrapmultiline
          p <- p + facet_wrap(    c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain ) [
            c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain )!="."]
            ,scales=input$facetscalesin,ncol=input$wrapncol,nrow=input$wrapnrow,
            labeller=label_wrap_gen(width = 25, multi_line = multiline ),as.table=ASTABLE)
        }
      }
      
      
      
      if (input$yaxisscale=="logy"&& is.numeric(plotdata[,"yvalues"])&& input$yaxisformat=="logyformat")
        p <- p + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x)))
      if (input$yaxisscale=="logy"&& is.numeric(plotdata[,"yvalues"])&&input$yaxisformat!="logyformat")
        p <- p + scale_y_log10()
      
      if (input$xaxisscale=="logx"&& is.numeric(plotdata[,input$x])&& input$xaxisformat=="logxformat")
        p <- p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x)))
      if (input$xaxisscale=="logx"&& is.numeric(plotdata[,input$x])&&input$xaxisformat!="logxformat")
        p <- p + scale_x_log10()
      
      
      if (input$yaxisscale=="lineary" && !is.null(plotdata$yvalues) && is.numeric(plotdata[,"yvalues"]) && input$yaxisformat=="scientificy")
        p <- p  + 
        scale_y_continuous(labels=comma )
      if (input$xaxisscale=="linearx" && is.numeric(plotdata[,input$x]) && input$xaxisformat=="scientificx")
        p <- p  + 
        scale_x_continuous(labels=comma )
      if (input$yaxisscale=="lineary" && !is.null(plotdata$yvalues) && is.numeric(plotdata[,"yvalues"]) && input$yaxisformat=="percenty")
        p <- p  + 
        scale_y_continuous(labels=percent )
      if (input$xaxisscale=="linearx" && is.numeric(plotdata[,input$x]) && input$xaxisformat=="percentx")
        p <- p  + 
        scale_x_continuous(labels=percent )
      
      
      if (input$xaxisscale=="linearx" && input$customxticks) {
        p <- p  + 
          scale_x_continuous(breaks=as.numeric(unique(unlist (strsplit(input$xaxisbreaks, ","))) ),
                             minor_breaks = as.numeric(unique(unlist (strsplit(input$xaxisminorbreaks, ","))) ) ) 
      }
      if (input$xaxisscale=="logx" && input$customxticks) {
        p <- p  + 
          scale_x_log10(breaks=as.numeric(unique(unlist (strsplit(input$xaxisbreaks, ","))) ),
                        minor_breaks = as.numeric(unique(unlist (strsplit(input$xaxisminorbreaks, ","))) ) ) 
      }
      
      if (input$yaxisscale=="lineary" && input$customyticks) {
        p <- p  + 
          scale_y_continuous(breaks=as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ),
                             minor_breaks = as.numeric(unique(unlist (strsplit(input$yaxisminorbreaks, ","))) ) ) 
      }
      if (input$yaxisscale=="logy" && input$customyticks) {
        p <- p  + 
          scale_y_log10(breaks=as.numeric(unique(unlist (strsplit(input$yaxisbreaks, ","))) ),
                        minor_breaks = as.numeric(unique(unlist (strsplit(input$yaxisminorbreaks, ","))) ) ) 
      }
      
      if (!is.null(input$y) & length(input$y) >= 2 & input$ylab=="" ){
        p <- p + ylab("Y variable(s)")
      }
      if (!is.null(input$y) & length(input$y) < 2 & input$ylab=="" ){
        p <- p + ylab(input$y)
      }
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
      
      
      if (input$horizontalzero)
        p <-    p+
        geom_hline(aes(yintercept=0))
      
      if (input$customvline1)
        p <-    p+
        geom_vline(xintercept=input$vline1)
      if (input$customvline2)
        p <-    p+
        geom_vline(xintercept=input$vline2)      
      
      if (input$customhline1)
        p <-    p+
        geom_hline(yintercept=input$hline1)
      
      if (input$customhline2)
        p <-    p+
        geom_hline(yintercept=input$hline2)     
      
      if (input$identityline)
        p <-    p+ geom_abline(intercept = 0, slope = 1)
      
      
      
      if (input$customlegendtitle){
        colourpos<-  which( input$legendordering=="colour")
        fillpos  <-  which( input$legendordering=="fill")
        sizepos  <-  which( input$legendordering=="size")
        collegend <-  gsub("\\\\n", "\\\n", input$customcolourtitle)
        filllegend <- gsub("\\\\n", "\\\n", input$customfilltitle)
        sizelegend <- gsub("\\\\n", "\\\n", input$customsizetitle)
        # to do list by row by row etc.
        
        if (input$legendalphacol){
          gcol  <- guide_legend(collegend,ncol=input$legendncolcol,reverse=input$legendrevcol,
                                override.aes = list(alpha = 1))
          if( length(colourpos)!=0) {
            gcol  <- guide_legend(collegend,ncol=input$legendncolcol,reverse=input$legendrevcol,
                                  order= colourpos,override.aes = list(alpha = 1))
          }
        }
        if (!input$legendalphacol){
          gcol  <- guide_legend(collegend,ncol=input$legendncolcol,reverse=input$legendrevcol)
          if( length(colourpos)!=0) {
            gcol  <- guide_legend(collegend,ncol=input$legendncolcol,reverse=input$legendrevcol,
                                  order= colourpos)
          }
        }
        if (input$legendalphafill){
          gfill <- guide_legend(filllegend,ncol=input$legendncolfill,reverse=input$legendrevfill,override.aes = list(alpha = 1))
          if( length(fillpos)!=0) {
            gfill <- guide_legend(filllegend,ncol=input$legendncolfill,reverse=input$legendrevfill,
                                  order = fillpos,override.aes = list(alpha = 1))
          } 
        }
        
        if (!input$legendalphafill){
          gfill <- guide_legend(filllegend,ncol=input$legendncolfill,reverse=input$legendrevfill)
          if( length(fillpos)!=0) {
            gfill <- guide_legend(filllegend,ncol=input$legendncolfill,reverse=input$legendrevfill,
                                  order = fillpos)
          } 
        }
        
        gsize <- guide_legend(sizelegend,ncol=input$legendncolsize,reverse=input$legendrevsize)
        if( length(sizepos)!=0) {
          gsize <- guide_legend(sizelegend,ncol=input$legendncolsize,reverse=input$legendrevsize,
                                order = sizepos)
        }
        
        p <-  p + guides(colour = gcol, size = gsize, fill = gfill)
        
      }
      
      
      if (input$themebw) {
        p <-    p+
          theme_bw(base_size=input$themebasesize)     
      }
      
      
      if (!input$themebw){
        p <- p +
          theme_gray(base_size=input$themebasesize)
      }
      
      
      p <-    p+theme(
        legend.position=input$legendposition,
        legend.box=input$legendbox,
        legend.direction=input$legenddirection,
        panel.background = element_rect(fill=input$backgroundcol))
      
      if (input$labelguides)
        p <-    p+
        theme(legend.title=element_blank())
      if (input$themeaspect)
        p <-    p+
        theme(aspect.ratio=input$aspectratio)
      
      if (input$themecolorswitcher=="themeggplot"){
        p <-  p +
          scale_colour_hue(drop=!input$themecolordrop)+
          scale_fill_hue(drop=!input$themecolordrop)
      }
      
      if (grepl("^\\s+$", input$ylab) ){
        p <- p + theme(
          axis.title.y=element_blank())
      }
      if (grepl("^\\s+$", input$xlab) ){
        p <- p + theme(
          axis.title.x=element_blank())
      }
      
      if (input$rotatexticks ){
        p <-  p+
          theme(axis.text.x = element_text(angle = input$xticksrotateangle,
                                           hjust = input$xtickshjust,
                                           vjust = input$xticksvjust) )
        
      }
      if (input$rotateyticks ){
        p <-  p+
          theme(axis.text.y = element_text(angle = input$yticksrotateangle,
                                           hjust = input$ytickshjust,
                                           vjust = input$yticksvjust) )                              
      }    
      
      p <-  p+
        theme(panel.grid.major = element_line(colour = input$gridlinescol),
              panel.grid.minor = element_line(colour = input$gridlinescol) )

      if (all(
         input$yaxiszoom=='noyzoom'&&
        !is.null(input$xaxiszoomin[1])&&
          is.numeric(plotdata[,input$x] )&&
          input$facetscalesin!="free_x"&&
          input$facetscalesin!="free")
      ){
        if(input$xaxiszoom=="userxzoom"){
          p <- p +
            coord_cartesian(xlim= c(input$lowerxin,input$upperxin))
        }
        if(input$xaxiszoom=="automaticxzoom"){
          p <- p +
            coord_cartesian(xlim= c(input$xaxiszoomin[1],input$xaxiszoomin[2])  )
        }

      }

      if (all(
        input$xaxiszoom=='noxzoom' &&
        !is.null(input$yaxiszoomin[1]) &&
          !is.null(plotdata$yvalues) &&
          is.numeric(plotdata[,"yvalues"] ) &&
          input$facetscalesin!="free_y" &&
          input$facetscalesin!="free")
      ){
        if(input$yaxiszoom=="useryzoom" ){
          p <- p +
            coord_cartesian(ylim= c(input$loweryin,input$upperyin) )
        }
        if(input$yaxiszoom=="automaticyzoom"){
          p <- p +
            coord_cartesian(
              ylim= c(input$yaxiszoomin[1],input$yaxiszoomin[2]))
        }

      }


      if (all(!is.null(input$xaxiszoomin[1])&&!is.null(input$yaxiszoomin[1])&&
          is.numeric(plotdata[,input$x] ) && !is.null(plotdata$yvalues) &&
          is.numeric(plotdata[,"yvalues"]) &&
          input$facetscalesin!="free_x"&&input$facetscalesin!="free_y"&&
          input$facetscalesin!="free")
      ){

        if (input$xaxiszoom=="userxzoom"&& input$yaxiszoom=="useryzoom"){
          p <- p +
            coord_cartesian(xlim= c(input$lowerxin,input$upperxin),
                            ylim= c(input$loweryin,input$upperyin)  )
        }
        if (input$xaxiszoom=="userxzoom"&&input$yaxiszoom=="automaticyzoom"){
          p <- p +
            coord_cartesian(xlim= c(input$lowerxin,input$upperxin),
                            ylim= c(input$yaxiszoomin[1],input$yaxiszoomin[2])  )
        }
        if (input$xaxiszoom=="automaticxzoom"&&input$yaxiszoom=="useryzoom"){
          p <- p +
            coord_cartesian(xlim= c(input$xaxiszoomin[1],input$xaxiszoomin[2]),
                            ylim= c(input$loweryin,input$upperyin)  )
        }
        if (input$xaxiszoom=="automaticxzoom"&&input$yaxiszoom=="automaticyzoom"){
          p <- p +
            coord_cartesian(xlim= c(input$xaxiszoomin[1],input$xaxiszoomin[2]),
                            ylim= c(input$yaxiszoomin[1],input$yaxiszoomin[2])  )
        }
      }

      if (input$showtargettext){
        p <- p +
          annotate("text", x=input$lowerxin, y=input$lowerytarget,
                   label=input$targettext, col=input$targettextcol, hjust=0, vjust=1,size=input$targettextsize)
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
      

      # You should attach any variables (dependencies) that are used in the
      # source code
      # p <- attach_source_dep(p, c("var1", "var2", "var3"))
      
      values$prevPlot <- p
      
      p
    }
  })
  
  output$plot <- renderPlot({
    plotObject()
  })
  
  output$plotly <- renderPlotly({ggplotly(plotObject())})
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
    validate(       need(!is.null(df), "Please select a data set"))
    if(!is.null(df) && input$dstatscolextrain!="."){
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
      "Min"                  = function(x) x$MIN,
      "Max"                  = function(x) x$MAX,
      "IQR"                  = function(x) x$IQR,
      "Geo. Mean"            = function(x) x$GMEAN,
      "Geo. CV%"             = function(x) x$GCV,
      "Mean (SD)"            = function(x) sprintf("%s (%s)", x$MEAN, x$SD),
      "Mean (CV%)"           = function(x) sprintf("%s (%s)", x$MEAN, x$CV),
      "Mean (SD) (CV%)"      = function(x) sprintf("%s (%s) (%s)", x$MEAN, x$SD, x$CV),
      "Mean (Median)"        = function(x) sprintf("%s (%s)", x$MEAN, x$MEDIAN),
      "[Min, Max]"           = function(x) sprintf("[%s, %s]", x$MIN, x$MAX),
      "Median [Min, Max]"    = function(x) sprintf("%s [%s, %s]", x$MEDIAN, x$MIN, x$MAX),
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
      t <- capture.output(table1(formula, data=df, overall=overall,
                                 topclass=paste("Rtable1", input$table_style),
                                 render.continuous=dstatsRenderCont(),
                                 standalone=FALSE))
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
values$maindata <- read.csv("data/sample_data.csv", na.strings = c("NA","."))
}
}
