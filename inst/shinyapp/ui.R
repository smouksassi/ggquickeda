fluidPage(
  useShinyjs(),
  tags$link(rel = "stylesheet", href = "app.css"),
  tags$link(rel = "stylesheet", href = "table1-style.css"),
  titlePanel(paste("Welcome to ggquickeda!",utils::packageVersion("ggquickeda"))),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "sidebar_upper_menus", selected="sidebar_inputs",
        tabPanel("Inputs", value = "sidebar_inputs", 
          tags$style(".shiny-file-input-progress {margin-bottom: 0px;margin-top: 0px}"),
          tags$style(".form-group {margin-bottom: 0px;margin-top: 0px}"),
          tags$div(
            tags$strong("Click Browse to choose csv file to upload with"),
            inline_ui(radioButtons("fileseparator", NULL,
                                   choices = c("comma (,)" = ",","or semicolon (;)" = ";"),
                                   selected = ",", inline = TRUE)),
            "separators, or",actionLink("sample_data_btn", "use sample data")
          ),
          fileInput("datafile", NULL, multiple = FALSE, accept = c("csv")),
          checkboxInput("stringasfactor", "Character Variables as Factors?", TRUE),
          uiOutput("ycol"),
          checkboxInput("show_pairs", "Plot a matrix of all Y variables", value = FALSE),
          conditionalPanel(
            condition = "!input.show_pairs",
            uiOutput("xcol")
          ),
          tabsetPanel(
            id = "filtercategorize",
            type = "pills",
            tabPanel(
              "Categorize/Cut", 
              uiOutput("catvar"),
              shinyjs::hidden(
                sliderInput('ncuts',label = 'N of Cut Breaks:', min=2, max=10, value=c(2),step=1)
              ),
              uiOutput("catvarquant"),
              shinyjs::hidden(
                sliderInput('ncutsquant',label = 'N of Quantiles:', min=2, max=10, value=c(2),step=1),
                checkboxInput('zeroplacebo', 'Zero as Placebo ?', value = FALSE),
                checkboxInput('missingcategory', 'Missing as a Category ?', value = FALSE)
                
              ),
              
              uiOutput("catvar2"),
              uiOutput("contvar"),
              uiOutput("catvar3"),
              uiOutput("ncuts2"),
              uiOutput("asnumeric"),
              textOutput("bintext")
              
            ),
            tabPanel(
              "Merge factor levels",
              shinyjs::hidden(div(
                id = "factor_merge_section",
                div(id = "factor_merge_placeholder"),
                actionButton("factor_merge_add", "Merge another group", icon("plus")),
                actionButton("factor_merge_remove", "Remove last merge", icon("trash"))
              ))
            ),
            tabPanel(
              "Recode/Reorder Categories",
              shinyjs::hidden(div(
                id = "factor_lvl_change_section",
                div(id = "factor_lvl_change_placeholder"),
                actionButton("factor_lvl_change_add", "Add another variable", icon("plus")),
                actionButton("factor_lvl_change_remove", "Remove last", icon("trash"))
              ))
            ),
            tabPanel("Combine Two Variables",
                     h6("Combined variables can be used for colour, fill, group, size and facets. They cannot be used as X or Y variables."),
                     
                     uiOutput("pastevar")
            ),
            tabPanel(
              "Filters", 
              uiOutput("maxlevels"),
              uiOutput("filtervar1"),
              uiOutput("filtervar1values"),
              uiOutput("filtervar2"),
              uiOutput("filtervar2values"),
              uiOutput("filtervar3"),
              uiOutput("filtervar3values"),
              uiOutput("filtervarcont1"),
              uiOutput("fslider1"),
              uiOutput("filtervarcont2"),
              uiOutput("fslider2"),
              uiOutput("filtervarcont3"),
              uiOutput("fslider3")
            ),
            tabPanel(
              "One Row by ID(s)",
              uiOutput("onerowidgroup"),
              uiOutput("onerowidlastgroup")
              
            ),
            
            tabPanel(
              "Division/Inverse/Rounding",
              uiOutput("divideynum"),
              uiOutput("divideydenom"),
              uiOutput("divideynum2"),
              numericInput("divideyconstant",label = "Divide by",value = 1),
              uiOutput("inversenum"),
              uiOutput("roundvar"),
              numericInput("rounddigits",label = "N Digits",value = 0,min=0,max=10)
            ),
            
            tabPanel("Reorder Facets or axis Levels", value = "reorder_facet_axis",
              h6("Operations in this tab will only take effect on the plot and not the table."),
              uiOutput("reordervar"),
              conditionalPanel(
                condition = "input.reordervarin!='' " ,
                selectizeInput(
                  "functionordervariable", 'By the:',
                  choices =c("Median","Mean","Minimum","Maximum","N","N Unique","SD","Sum") ,multiple=FALSE)
              ),
              uiOutput("variabletoorderby"),
              conditionalPanel(
                condition = "input.reordervarin!='' " ,
                checkboxInput('reverseorder', 'Reverse Order ?', value = FALSE)
              ),
              
              uiOutput("reordervar2"),
              uiOutput("reordervar2values"),
              uiOutput("reordervar3"),
              uiOutput("reordervar3values"),
              selectizeInput("change_labels_stat_var", "Change levels of this variable:",
                             choices = list(), multiple = FALSE,
                             options = list(placeholder = 'Please select a variable')
              ),
              conditionalPanel(
                "input.change_labels_stat_var != ''",
                "Old labels", textOutput("change_labels_stat_old", inline = TRUE),
                textInput("change_labels_stat_levels", label = "")
              ),
              selectizeInput("change_labels_stat_var_2", "Change levels of this variable:",
                             choices = list(), multiple = FALSE,
                             options = list(placeholder = 'Please select a variable')
              ),
              conditionalPanel(
                "input.change_labels_stat_var_2 != ''",
                "Old labels", textOutput("change_labels_stat_old_2", inline = TRUE),
                textInput("change_labels_stat_levels_2", label = "")
              )
            )
          ),
          hr()
        ), # tabsetPanel
        tabPanel("Graph Options", value= "sidebar_Graph_Options",
          tabsetPanel(id = "graphicaloptions", selected = "x_y_loglabels",
            tabPanel("X/Y Axes Log/Labels", value = "x_y_loglabels",
              hr(),
              textInput('ylab', 'Y axis label', value = "") ,
              textInput('xlab', 'X axis label', value = "") ,
              textInput('title', 'Plot Title', value = "") ,
              textInput('subtitle', 'Plot Subtitle', value = "") ,
              radioButtons("titleposition", "Title Positioning:",
                           c("Panel" = "panel",
                             "Plot" = "plot"), inline = TRUE),
              textInput('caption', 'Plot Caption', value = "") ,
              textInput('plottag', 'Plot Tag', value = "") ,
              radioButtons("captionposition", "Caption Positioning:",
                           c("Panel" = "panel",
                             "Plot" = "plot"), inline = TRUE),
              hr(),
              conditionalPanel(condition = "!input.show_pairs",
              fluidRow(
                column(6,
                       radioButtons("yaxisscale", "Y Axis scale:",
                                    c("Linear" = "lineary",
                                      "Log10" = "logy"))),
                column(6,
                       radioButtons("yaxisformat", "Y Axis tick format:",
                                    c("default" = "default",
                                      "Comma separated" = "scientificy",
                                      "Percent" = "percenty"))),
                column(6,
                       radioButtons("xaxisscale", "X Axis scale:",
                                    c("Linear" = "linearx",
                                      "Log10" = "logx"))),
                column(6,
                       radioButtons("xaxisformat", "X Axis tick format:",
                                    c("default" = "default",
                                      "Comma separated" = "scientificx",
                                      "Percent" = "percentx")))
              ),
      
              checkboxInput('rotateyticks', 'Rotate/Customize Y axis Labels ?', value = FALSE),
              conditionalPanel(condition = "input.rotateyticks" ,
                               sliderInput("ylabelsize", "Y axis label size: (zero to hide)",
                                           min=0, max=100, value=c(16),step=0.5),
                               sliderInput("yticksrotateangle", "Y axis labels angle:", min=0, max=360, value=c(0),step=10),
                               sliderInput("ytickshjust", "Y axis labels horizontal justification:", min=0, max=1, value=c(0.5),step=0.1),
                               sliderInput("yticksvjust", "Y axis labels vertical justification:", min=0, max=1, value=c(0.5),step=0.1)
              ),
              checkboxInput('rotatexticks', 'Rotate/Customize X axis Labels ?', value = FALSE),
              conditionalPanel(condition = "input.rotatexticks" ,
                               sliderInput("xlabelsize", "X axis label size: (zero to hide)",
                                           min=0, max=100, value=c(16),step=0.5),
                               sliderInput("xticksrotateangle", "X axis labels angle:", min=0, max=360, value=c(20),step=10),
                               sliderInput("xtickshjust", "X axis labels horizontal justification:", min=0, max=1, value=c(1),step=0.1),
                               sliderInput("xticksvjust", "X axis labels vertical justification:", min=0, max=1, value=c(1),step=0.1)
              ),
              checkboxInput('customxticks', 'Custom X axis Ticks ?', value = FALSE),
              conditionalPanel(condition = "input.customxticks" , 
                               textInput("xaxisbreaks",label ="X axis major Breaks",
                                         value = as.character(paste(0,12,24,sep=",") )),
                               textInput("xaxisminorbreaks",label ="X axis minor Breaks",
                                         value = as.character(paste(6,18,sep=",") ))
                               ),
              conditionalPanel(condition = "input.customxticks && input.xaxisformat=='default' " , 
                               checkboxInput('customxtickslabel', 'Custom X axis labels ?', value = FALSE)
              ),
               conditionalPanel(condition = "input.customxticks && input.customxtickslabel && input.xaxisformat=='default' " , 
                               textInput("xaxislabels",label ="X axis Labels",
                                         value = as.character(paste("A","B","C" ,sep=",") )
                               )
              ),
              checkboxInput('customyticks', 'Custom Y axis Ticks ?', value = FALSE),
              conditionalPanel(condition = "input.customyticks" , 
                               textInput("yaxisbreaks",label ="Y axis major Breaks",
                                         value = as.character(paste(0,1,2,sep=",") )),
                               textInput("yaxisminorbreaks",label ="Y axis minor Breaks",
                                         value = as.character(paste(0.5,1.5,sep=",") ))
              ),
              conditionalPanel(condition = "input.customyticks && input.yaxisformat=='default' " , 
                               checkboxInput('customytickslabel', 'Custom Y axis labels ?', value = FALSE)
              ),
              conditionalPanel(condition = "input.customyticks & input.customytickslabel & input.yaxisformat=='default' " , 
                               textInput("yaxislabels",label ="Y axis Labels",
                                         value = as.character(paste("A","B","C" ,sep=",") )
                               )
              ),
              checkboxInput('rmxaxistickslabels', 'Remove X axis ticks and labels ?',value=FALSE),
              checkboxInput('rmyaxistickslabels', 'Remove Y axis ticks and labels ?',value=FALSE),
              checkboxInput('annotatelogticks', 'Add Log Tick Annotations ?', value = FALSE),
              conditionalPanel(condition = "input.annotatelogticks",
                               selectInput('logsides', label ='Log Tick Sides',
                                           choices=c("Left" = "l",
                                                     "Top" ="t ",
                                                     "Right"="r",
                                                     "Bottom"="b"),
                                           multiple=TRUE, selectize=TRUE,selected="l")
              )
              )# conditional on pairs is off
            ),
            tabPanel("Graph Size/Zoom", value = "graph_size_zoom",
              sliderInput("height", "Plot Height", min=1080/4, max=1080, value=480, animate = FALSE),
              conditionalPanel(condition = "!input.show_pairs",
              h6("X Axis Zoom is available if you have exactly one x variable and facet x scales are not set to be free. The automatic setting generates a slider has limits between your x variable min/max otherwise select User Defined to input your own."),
              fluidRow(
                column(12,
                       radioButtons("xaxiszoom", "X Axis Zoom:",
                                    c("None" = "noxzoom",
                                      "Automatic" = "automaticxzoom",
                                      "User" = "userxzoom"),inline=TRUE )
                       ,
                       conditionalPanel(condition = "input.xaxiszoom=='automaticxzoom'",
                                        uiOutput("xaxiszoom") ) ),
                column(6,
                       conditionalPanel(condition = "input.xaxiszoom=='userxzoom' ",uiOutput("lowerx")) ),
                column(6,
                       conditionalPanel(condition = "input.xaxiszoom=='userxzoom' ",uiOutput("upperx")) )
              ),# fluidrow
              
              h6("Y Axis Zoom is available if you have exactly one y variable and facet y scales are not set to be free. The automatic setting generates a slider has limits between your y variable min/max otherwise select User Defined to input your own."),
              fluidRow(
                column(12,
                       radioButtons("yaxiszoom", "Y Axis Zoom:",
                                    c("None" = "noyzoom",
                                      "Automatic" = "automaticyzoom",
                                      "User" = "useryzoom"),inline=TRUE )
                       ,
                       conditionalPanel(condition = "input.yaxiszoom=='automaticyzoom'",
                                        uiOutput("yaxiszoom") ) ),
                column(6,
                       conditionalPanel(condition = "input.yaxiszoom=='useryzoom' ",uiOutput("lowery")) ),
                column(6,
                       conditionalPanel(condition = "input.yaxiszoom=='useryzoom' ",uiOutput("uppery")) ),
                column(12,
                       inline_ui(
                         numericInput("xexpansion_l_add",label = "x expansion left additive",
                                      value = 0, min = 0, max = NA, step = 0.1, width='120px')),
                       inline_ui(
                         numericInput("xexpansion_r_add",label = "x expansion right additive",
                                      value = 0, min = 0, max = NA, step = 0.1, width='120px')),
                       inline_ui(
                         numericInput("xexpansion_l_mult",label = "x expansion left multiplicative",
                                      value = 0.05, min = 0, max = NA, step = 0.1, width='120px')),
                       inline_ui(
                         numericInput("xexpansion_r_mult",label = "x expansion right multiplicative",
                                      value = 0.05, min = 0, max = NA, step = 0.1 , width='120px'))
                       ),
                column(12,
                       inline_ui(
                         numericInput("yexpansion_l_add",label = "y expansion bottom additive",
                                      value = 0,min=0,max=NA,width='120px')),
                       inline_ui(
                         numericInput("yexpansion_r_add",label = "y expansion top additive",
                                      value = 0,min=0,max=NA,width='120px')),
                       inline_ui(
                         numericInput("yexpansion_l_mult",label = "y expansion bottom multiplicative",
                                      value = 0.05,min=0,max=NA,width='120px')),
                       inline_ui(
                         numericInput("yexpansion_r_mult",label = "y expansion top multiplicative",
                                      value = 0.05, min=0,max=NA,width='120px'))
                ),
                column(12,
                       conditionalPanel(condition = "input.yaxiszoom!='noyzoom' | input.xaxiszoom!='noxzoom' ",
                                        checkboxInput('expand', 'Allow Coordinate Expansion?', value = TRUE)
                       )
                )
              ) # fluidrow
              ) # conditional on not show pairs
            ),#tabpanel zoom
            
            tabPanel("Customization of Legend(s)", value = "custom_legends",
              selectInput('legendposition', label ='Legend Position',
                          choices=c("left", "right", "bottom", "top","none","custom"),
                          multiple=FALSE, selectize=TRUE,selected="bottom"),
              
              conditionalPanel(
                condition = "input.legendposition=='custom'",
                inline_ui(
                  numericInput("legendpositionx",label = "Legend X Position",
                               value = 0.5,min=0,max=1,width='100%')),
                inline_ui(
                  numericInput("legendpositiony",label = "Legend Y Position",
                               value = 0.5,min=0,max=1,width='100%'))),
              br(),
              selectInput('legendjustificationh', label ='Horizontal Legend Justification',
                          choices=c("left","center", "right"),
                          multiple=FALSE, selectize=TRUE,selected="center"),
              selectInput('legendjustificationv', label ='Vertical Legend Justification',
                          choices=c("bottom","center","top"),
                          multiple=FALSE, selectize=TRUE,selected="center"),
              
               selectInput('legenddirection', label ='Layout of Items in Legends',
                          choices=c("horizontal", "vertical"),
                          multiple=FALSE, selectize=TRUE,selected="horizontal"),
              selectInput('legendbox', label ='Arrangement of Multiple Legends ',
                          choices=c("horizontal", "vertical"),
                          multiple=FALSE, selectize=TRUE,selected="vertical"),
              selectInput('legendboxjust', label ='Legend Box Justification',
                          choices=c("left", "right","center", "bottom", "top"),
                          multiple=FALSE, selectize=TRUE,selected="center"),
              colourpicker::colourInput(
                "legendbackground","Legend Background Fill",value =  "white",
                showColour = "both",allowTransparent = TRUE, returnName = TRUE),
              colourpicker::colourInput(
                "legendkey","Legend Item Fill",value =  "white",
                showColour = "both",allowTransparent = TRUE,returnName=TRUE),
              checkboxInput('sepguides', 'Separate Legend Items for Median/PI ?',value = TRUE),       
              checkboxInput('labelguides', 'Hide the Names of the Legend Items ?',value = FALSE),
              sliderInput("legendspacex", "Multiplier for Space between Legend Items (x)",
                          min = 0, max = 3, step = 0.1, value = 1),
              sliderInput("legendspacey", "Multiplier for Space between Legend Items (y)",
                          min = 0, max = 3, step = 0.1, value = 1),
              checkboxInput('customlegendtitle', 'Customization of Legend(s): e.g. change legend title,
                            number of legend keys columns, override transparency, ordering of multiple legends and more',
                            value = FALSE),
              tabsetPanel(
                tabPanel(
                  "Colour Legend",
                  conditionalPanel(
                    condition = "input.customlegendtitle",
                    textInput("customcolourtitle", label ="Colour Legend Title",value="colour"),
                    numericInput("legendncolcol",label = "Colour Legend N columns", value =1, min=1, max = 10) ,
                    checkboxInput('legendrevcol', 'Reverse Colour Legend ?',value = FALSE),
                    checkboxInput('legendalphacol', 'Override Colour Transparency ?',value = TRUE)
                  )
                ),
                tabPanel(
                  "Fill Legend",
                  conditionalPanel(
                    condition = "input.customlegendtitle",
                    textInput("customfilltitle", label ="Fill Legend Title",value="fill"),
                    numericInput("legendncolfill",label = "Fill Legend N columns", value = 1, min = 1, max =10) ,
                    checkboxInput('legendrevfill', 'Reverse Fill Legend?',value = FALSE),
                    checkboxInput('legendalphafill', 'Override Fill Transparency ?',value = FALSE)
                  )
                ),
                tabPanel(
                  "Size Legend",
                  conditionalPanel(
                    condition = "input.customlegendtitle",
                    textInput("customsizetitle", label ="Size Legend Title",value="size"),
                    numericInput("legendncolsize",label = "Size Legend N columns",value =1,min=1,max =10) ,
                    checkboxInput('legendrevsize','Reverse Size Legend ?',value = FALSE)
                  )
                ),
                tabPanel(
                  "Shape Legend",
                  conditionalPanel(
                    condition = "input.customlegendtitle",
                    textInput("customshapetitle", label ="Shape Legend Title",value="shape"),
                    numericInput("legendncolshape",label = "Shape Legend N columns",value =1,min=1,max =10) ,
                    checkboxInput('legendrevshape','Reverse Shape Legend ?',value = FALSE)
                  )
                ),
                tabPanel(
                  "Linetype Legend",
                  conditionalPanel(
                    condition = "input.customlegendtitle",
                    textInput("customlinetypetitle", label ="Linetype Legend Title",value="linetype"),
                    numericInput("legendncollinetype",label = "Linetype Legend N columns",value =1,min=1,max =10) ,
                    checkboxInput('legendrevlinetype','Reverse Linetype Legend ?',value = FALSE)
                  )
                ),
                tabPanel(
                  "Legends Ordering",
                  conditionalPanel(
                    condition = "input.customlegendtitle",
                    selectizeInput(
                      'legendordering',
                      label = paste("Drag/Drop to reorder","Colour, Fill, Size, Shape, Linetype Legends"),
                      choices = c("colour","fill","size","shape","linetype"),
                      selected = c("colour","fill","size","shape","linetype"),
                      multiple=TRUE,  options = list(
                        plugins = list('drag_drop')
                      )
                    ),
                    checkboxInput('removelegend','Remove Legend if deleted from input reordering above?',value = FALSE),
                    h6("ggplot will attempt to merge legend items that share names, mappings and ordering position.")
                  )
                )
                )
  
            ),
            tabPanel("Facets Options", value = "facet_options",
              tabsetPanel(
                tabPanel("Facet Scales, Spaces, Positioning, Ordering",
                         uiOutput("facetscales"),
                         selectInput('facetspace' ,'Facet Spaces:',c("fixed","free_x","free_y","free")),
                         conditionalPanel(
                           condition = "!input.facetwrap" ,
                           selectizeInput(  "facetswitch", "Facet Switch to Near Axis:",
                                            choices = c("none","x","y","both"),
                                            options = list(  maxItems = 1 )  )
                         ),
                         selectizeInput(  "stripplacement", "Strip Placement:",
                                          choices = c("inside","outside"),
                                          options = list(  maxItems = 1 )  ),
                         
                         conditionalPanel(
                           condition = "!input.facetwrap && input.stripplacement== 'outside'" ,
                           sliderInput("stripswitchpadgrid", label = "Facets Axis Padding:",
                                       min = 0, max = 2, value = 0.25, step = 0.05)
                           ),
                         conditionalPanel(
                           condition = "input.facetwrap && input.stripplacement== 'outside'" ,
                           sliderInput("stripswitchpadwrap", label = "Facets Axis Padding:",
                                       min = 0, max = 2, value = 0.25, step = 0.05)
                         ),
                         
                         selectInput('facetordering' ,'Facet Ordering:',c(
                           "Top to Bottom, Left to Right Ordering like a Table" ="table",
                           "Bottom to Top, Left to Right Ordering like a Plot" ="plot"),
                           selected="table")
                         
                         ),
                tabPanel("Facet Grid Margins",
                         h6("The 'Show facet margins?' options are available when 'Use facet_wrap?' is not checked"),    
                         conditionalPanel(
                           condition = "!input.facetwrap" ,
                           selectInput('facetmargin', 'Show facet margins?', 
                                       choices = c("None" = "none",
                                                   "All" = "all",
                                                   "Specific variables" = "some")),
                           conditionalPanel(
                             "input.facetmargin == 'some'",
                             selectInput('facetmargin_vars', NULL, choices = c(), multiple = TRUE,)
                           )
                         ), 
                )
                
                ),
              hr(),
              tabsetPanel(
                tabPanel("Facet Text Size/Angle/Face",
                         sliderInput("striptextsizex", "X Strip Text Size: (zero to hide)",
                                     min = 0, max = 100, step = 0.5, value = 16),
                         sliderInput("facettextxangle", "Facet Text Angle X",
                                     min = 0, max = 360, step = 90, value = 0),
                         checkboxInput('boldfacettextx', "X Strip Text Bold?", value = FALSE),
                         sliderInput("striptextsizey", "Y Strip Text Size: (zero to hide)",
                                     min = 0, max = 100, step = 0.5, value = 16),
                         sliderInput("facettextyangle", "Facet Text Angle Y",
                                     min = 0, max = 360, step = 90, value = 270),
                         checkboxInput('boldfacettexty', "Y Strip Text Bold?", value = FALSE),
                ),
                tabPanel("Facet Text Justification",
                         sliderInput("x_facet_text_vjust", "Facet Text Vertical Justification X",
                                     min = 0, max = 1, step = 0.1, value = 0.5),
                         sliderInput("x_facet_text_hjust", "Facet Text Horizontal Justification X",
                                     min = 0, max = 1, step = 0.1, value = 0.5),
                         sliderInput("y_facet_text_vjust", "Facet Text Vertical Justification Y",
                                     min = 0, max = 1, step = 0.1, value = 0.5),
                         sliderInput("y_facet_text_hjust", "Facet Text Horizontal Justification Y",
                                     min = 0, max = 1, step = 0.1, value = 0.5)
                ),
                tabPanel("Facet Text/Background Colour",
                         colourpicker::colourInput("striptextcolourx",  "X Text Colour:",
                                                   value="black",
                                                   showColour = "both",allowTransparent=TRUE,returnName=TRUE),
                         colourpicker::colourInput("stripbackgroundfillx",
                                                   "X Strip Background Fill:",
                                                   value="#E5E5E5",
                                                   showColour = "both",allowTransparent=TRUE,returnName=TRUE),
                         div( actionButton("stripbackfillresetx", "Reset X Strip Background Fill"),
                              style="text-align: right"),
                         checkboxInput('removexstrip', 'Remove X Strip Background and Outline?',
                                       value=FALSE),
                         
                         colourpicker::colourInput("striptextcoloury",  "Y Text Colour:",
                                                   value="black",
                                                   showColour = "both",allowTransparent=TRUE,returnName=TRUE),
                         colourpicker::colourInput("stripbackgroundfilly",
                                                   "Y Strip Background Fill:",
                                                   value="#E5E5E5",
                                                   showColour = "both",allowTransparent=TRUE,returnName=TRUE),
                         div( actionButton("stripbackfillresety", "Reset Y Strip Background Fill"),
                              style="text-align: right"),
                         checkboxInput('removeystrip', 'Remove Y Strip Background and Outline?',
                                       value=FALSE),
                ),
                tabPanel("Facet Labels",
                         selectInput('facetlabeller' ,'Facet Label:',c(
                           "Variable(s) Name(s) and Value(s)" ="label_both",
                           "Value(s)"="label_value",
                           "Parsed Expression" ="label_parsed",
                           "Depends on Context" ="label_context",
                           "Wrap lines" ="label_wrap_gen"),
                           selected="label_both"),
                         conditionalPanel(
                           condition = "input.facetlabeller== 'label_wrap_gen'  " ,
                           sliderInput("labelwrapwidth", "N Characters to Wrap Labels:",
                                       min=5, max=100, value=c(25),step=1)
                         ),
                         checkboxInput('facetwrapmultiline',
                                       'Strip labels on multiple lines?',
                                       value = TRUE)
                ),
                tabPanel("Facet Wrap",
                         checkboxInput('facetwrap', 'Use facet_wrap?'),
                         conditionalPanel(
                           condition = "input.facetwrap" ,
                           selectInput('stripposition', label ='Strip Position',
                                       choices=c("left", "right", "bottom", "top"),
                                       multiple=FALSE, selectize=TRUE,selected="top"),
                           checkboxInput('customncolnrow', 'Control N columns an N rows?')
                         ),
                         conditionalPanel(
                           condition = "input.customncolnrow" ,
                           h6("An error (nrow*ncol >= n is not TRUE) will show up if the total number of facets/panels
                   is greater than the product of the specified  N columns x N rows. Increase the N columns and/or N rows to avoid the error.
                   The default empty values will use ggplot automatic algorithm."),        
                           numericInput("wrapncol",label = "N columns",value =NA,min=1,max =10) ,
                           numericInput("wrapnrow",label = "N rows",value = NA,min=1,max=10) 
                         )
                ),
                tabPanel("Facet Spacing",
                         sliderInput("panelspacingx", label = "Facets X Spacing:",
                                     min = 0, max = 2, value = 0.25, step = 0.05),
                         sliderInput("panelspacingy", label = "Facets Y Spacing:",
                                     min = 0, max = 2, value = 0.25, step = 0.05)
                )
              )
              ),
            
            tabPanel("Reference Lines/Target", value ="ref_line_target_options",
              checkboxInput('identityline', 'Identity Line')    ,   
              checkboxInput('horizontalzero', 'Horizontal Zero Line'),
              checkboxInput('customvline1', 'Vertical Line 1'),
              conditionalPanel(condition = "input.customvline1" , 
                               numericInput("vline1",label = "",value = 1),
                               colourpicker::colourInput("vlinecol1", "Line Color:", "gray",
                                                         showColour = "both",allowTransparent=TRUE,returnName=TRUE),
                               div( actionButton("vlinecol1reset", "Reset Line Color"), style="text-align: right"),
                               selectInput('vlinetype1','Line Type:',c("solid","dashed", "dotted", "dotdash", "longdash", "twodash","blank")),
                               sliderInput("vlinesize1", "Line Size:", min=0, max=4, value=c(1),step=0.1)
              ),
              checkboxInput('customvline2', 'Vertical Line 2'),
              conditionalPanel(condition = "input.customvline2" , 
                               numericInput("vline2",label = "",value = 1) ,
                               colourpicker::colourInput("vlinecol2", "Line Color:", "gray",
                                                         showColour = "both",allowTransparent=TRUE,returnName=TRUE),
                               div( actionButton("vlinecol2reset", "Reset Line Color"), style="text-align: right"),
                               selectInput('vlinetype2','Line Type:',c("solid","dashed", "dotted", "dotdash", "longdash", "twodash","blank")),
                               sliderInput("vlinesize2", "Line Size:", min=0, max=4, value=c(1),step=0.1) 
              ),
              checkboxInput('customhline1', 'Horizontal Line 1'),
              conditionalPanel(condition = "input.customhline1" , 
                               numericInput("hline1",label = "",value = 1),
                               colourpicker::colourInput("hlinecol1", "Line Color:", "gray",showColour = "both",allowTransparent=TRUE,returnName=TRUE),
                               div( actionButton("hlinecol1reset", "Reset Line Color"), style="text-align: right"),
                               selectInput('hlinetype1','Line Type:',c("solid","dashed", "dotted", "dotdash", "longdash", "twodash","blank")),
                               sliderInput("hlinesize1", "Line Size:", min=0, max=4, value=c(1),step=0.1)
                               
              ),
              checkboxInput('customhline2', 'Horizontal Line 2'),
              conditionalPanel(condition = "input.customhline2" , 
                               numericInput("hline2",label = "",value = 1),
                               colourpicker::colourInput("hlinecol2", "Line Color:", "gray",showColour = "both",allowTransparent=TRUE,returnName=TRUE),
                               div( actionButton("hlinecol2reset", "Reset Line Color"), style="text-align: right"),
                               selectInput('hlinetype2','Line Type:',c("solid","dashed", "dotted", "dotdash", "longdash", "twodash","blank")),
                               sliderInput("hlinesize2", "Line Size:", min=0, max=4, value=c(1),step=0.1) ),
              checkboxInput('showtarget', 'Add Target Window 1', value = FALSE) ,
              conditionalPanel(condition = "input.showtarget" , 
                               numericInput("upperytarget1",label = "Upper Target Value 1:",
                                            value = 1,min=NA,max=NA,width='50%'),
                               numericInput("lowerytarget1",label = "Lower Target Value 1:",
                                            value = 1,min=NA,max=NA,width='50%'),
                               colourpicker::colourInput("targetcol1", "Target  Color 1:", "gray",showColour = "both",returnName=TRUE),
                               sliderInput("targetopacity1", label = "Target Opacity 1:", min = 0, max = 1, value = 0.7, step = 0.05)
              ),
              checkboxInput('showtarget2', 'Add Target Window 2', value = FALSE) ,
              
              conditionalPanel(condition = "input.showtarget2" , 
                               numericInput("upperytarget2",label = "Upper Target Value 2:",
                                            value = 1,min=NA,max=NA,width='50%'),
                               numericInput("lowerytarget2",label = "Lower Target Value 2:",
                                            value = 1,min=NA,max=NA,width='50%'),
                               colourpicker::colourInput("targetcol2", "Target  Color 2:", "gray",showColour = "both",returnName=TRUE),
                               sliderInput("targetopacity2", label = "Target Opacity 2", min = 0, max = 1, value = 0.7, step = 0.05)
              ),
              checkboxInput('showtargettext', 'Add Target Text', value = FALSE),
              conditionalPanel(condition = "input.showtargettext" ,
                               textInput('targettext', 'Target Text', value = "Target: XX-XXX µg/mL"),
                               sliderInput("targettextsize", "Target Text Size:", min=1, max=10, value=c(5),step=0.1),
                               colourInput("targettextcol", "Target Text Color:", "blue",showColour = "both"),
                               sliderInput("targettextvjust", "Target Text Vertical Justification:", min=0, max=1, value=c(1),step=0.1),
                               sliderInput("targettexthjust", "Target Text Horizontal Justification:", min=0, max=1, value=c(0),step=0.1),
                               numericInput("targettextxpos",label = "Target Text X Position",
                                            value = 1,min=NA,max=NA,width='50%'),
                               numericInput("targettextypos",label = "Target Text Y Position",
                                            value = 1,min=NA,max=NA,width='50%')
              )
              
              
              
            ),
            tabPanel(
              "Themes and Color/other Scales Options", value = "themes_color_other",
              sliderInput("themebasesize", "Theme Size (affects all text except facet strip):",
                          min=1, max=100, value=c(16),step=1),
              checkboxInput('themebw', 'Use Black and White Theme ?',value=TRUE),
               colourpicker::colourInput(
                "backgroundcol","Background Color",value =  "white",
                showColour = "both",allowTransparent = TRUE,returnName=TRUE),
              checkboxInput('panelontop', 'Place the Panel gridlines and background over the data ?',value=FALSE),
              checkboxInput('themecolordrop', 'Keep All levels of Colors and Fills ?',value=TRUE),
              tabsetPanel(
                tabPanel("Discrete Color and Fill Scale",
                         radioButtons("themecolorswitcher", "Discrete Color and Fill Scale:",
                                      c("Tableau 10"  = "themetableau10",
                                        "Tableau 20"  = "themetableau20",
                                        "Color Blind" = "themecolorblind",
                                        "Color Blind 2" = "themecolorblind2",
                                        "ggplot default" = "themeggplot",
                                        "viridis"        = "themeviridis",
                                        "User defined" = "themeuser")
                                      ,inline=TRUE),
                         h6("If you get /Error: Insufficient values in manual scale. ## needed but only 10 provided.
                 Try to use Tableau 20 or ggplot default. Color Blind and Color Blind 2 Themes support up to 8 colors.
                 Contact me if you want to add your own set of colors."),
                         conditionalPanel(condition = " input.themecolorswitcher=='themeuser' " ,
                                          sliderInput("nusercol", "N of User Colors:",
                                                      min=2, max=30, value=c(10),step=1)
                         ),
                         uiOutput('userdefinedcolor'),
                         conditionalPanel(condition = " input.themecolorswitcher=='themeuser' " ,
                                          actionButton("userdefinedcolorreset", "Back to starting tableau colours", icon = icon("undo") ),
                                          actionButton("userdefinedcolorhighlight", "Highligth first colour", icon = icon("search") )
                                          
                         )
                ),
                tabPanel("Discrete Shape Scale",
                         radioButtons("scaleshapeswitcher", "Discrete Shape Scale:",
                                      c("ggplot default" = "themeggplot","User defined" = "themeuser") ,inline=TRUE),
                         conditionalPanel(condition = " input.scaleshapeswitcher=='themeuser' " ,
                                          sliderInput("nusershape", "N of User Shapes:", min=1, max=20, value=c(6),step=1)
                         ),
                         uiOutput('userdefinedshape')
                ),
                tabPanel("Discrete Linetype Scale",
                         radioButtons("scalelinetypeswitcher", "Discrete Linetype Scale:",
                                      c("ggplot default" = "themeggplot","User defined" = "themeuser") ,inline=TRUE),
                         conditionalPanel(condition = " input.scalelinetypeswitcher=='themeuser' " ,
                                          sliderInput("nuserlinetype", "N of User Linetypes:", min=1, max=10, value=c(6),step=1)
                         ),
                         uiOutput('userdefinedlinetype')
                ),
                tabPanel("Continuous Color and Fill Scale",
                         radioButtons("themecontcolorswitcher", "Continuous Color and Fill Themes:",
                                      c("Red White Blue"  = "RedWhiteBlue",
                                        "Red White Green"  = "RedWhiteGreen",
                                        "ggplot default" = "themeggplot",
                                        "viridis" = "themeviridis",
                                        "User defined" = "themeuser")
                                      ,inline=TRUE),
                         conditionalPanel(condition = " input.themecontcolorswitcher=='RedWhiteBlue' |
                                             input.themecontcolorswitcher=='RedWhiteGreen'" ,
                                          colourpicker::colourInput(
                                            "midcolor",
                                            "Midpoint Color",
                                            value ="white",
                                            showColour = "both",
                                            allowTransparent = FALSE,returnName = TRUE)
                         ),
                         conditionalPanel(condition = " input.themecontcolorswitcher=='RedWhiteBlue' |
                                             input.themecontcolorswitcher=='RedWhiteGreen'|
                                             input.themecontcolorswitcher=='themeuser'" ,
                                          numericInput("colormidpoint", "Continuous Color/Fill Midpoint Value",
                                                       value = 0)
                         ),
                         # conditionalPanel(condition = " input.themecontcolorswitcher=='themeuser' " ,
                         #                  gradientInputUI("gradientcol", "100%", "www"),
                         #                  actionButton("gradientreset", "Back to starting colours",icon = icon("undo") )
                         #                  ),
                         
                         
                         conditionalPanel(condition = " input.themecontcolorswitcher=='themeuser' " ,
                                          uiOutput('userdefinedcontcolor'),
                                          actionButton("userdefinedcontcolorreset", "Back to starting continuous colours", icon = icon("undo") )
                                          
                         )    
                         
                )
              ),
              tabsetPanel(
                tabPanel("Grid Lines",
                         div(
                           colourpicker::colourInput("majorgridlinescolx", "Major X Axis Grid Lines:",
                                                     value="#E5E5E5",
                                                     showColour = "both",
                                                     allowTransparent=TRUE,returnName=TRUE),
                           style = "display: inline-block;") ,
                         div(
                           colourpicker::colourInput("majorgridlinescoly", "Major Y Axis Grid Lines:",
                                                     value="#E5E5E5",
                                                     showColour = "both",
                                                     allowTransparent=TRUE,returnName=TRUE),
                           style = "display: inline-block;") ,
                         div( actionButton("majorgridlinescolreset", "Reset Major Grid Lines Color"),
                              style="display: inline-block;text-align: right"),br(),
                         div(colourpicker::colourInput("minorgridlinescolx", "Minor X Axis Grid Lines:",
                                                       value="#E5E5E5",
                                                       showColour = "both",
                                                       allowTransparent=TRUE,returnName=TRUE),
                             style = "display: inline-block;"),
                         div(colourpicker::colourInput("minorgridlinescoly", "Minor Y Axis Grid Lines:",
                                                       value="#E5E5E5",
                                                       showColour = "both",
                                                       allowTransparent=TRUE,returnName=TRUE),
                             style = "display: inline-block;"),
                         div( actionButton("minorgridlinescolreset", "Reset Minor Grid Lines Color"),
                              style="display: inline-block;text-align: right"),
                         inline_ui(checkboxInput('rmmajorgridlinesx',
                                                 'Remove X axis Major Grid Lines?',value=FALSE)),
                         inline_ui(checkboxInput('rmmajorgridlinesy',
                                                 'Remove Y axis Major Grid Lines?',value=FALSE)),
                         inline_ui(checkboxInput('rmminorgridlinesx',
                                                 'Remove X axis Minor Grid Lines?',value=FALSE)),
                         inline_ui(checkboxInput('rmminorgridlinesy',
                                                 'Remove Y axis Minor Grid Lines?',value=FALSE))
                ),
                tabPanel("Margins",
                         inline_ui(
                           numericInput("margintop",label = "Plot Top Margin",
                                        value = 0,min=0,max=NA,width='120px')),
                         inline_ui(
                           numericInput("marginleft",label = "Plot Left Margin",
                                        value = 5.5,min=0,max=NA,width='120px')),
                         inline_ui(
                           numericInput("marginright",label = "Plot Right Margin",
                                        value = 5.5,min=0,max=NA,width='120px')),
                         inline_ui(
                           numericInput("marginbottom",label = "Plot Bottom Margin",
                                        value = 0,min=0,max=NA,width='120px')),
                         inline_ui(
                           numericInput("legendtop",label = "Legend Top Margin",
                                        value = 0,min=0,max=NA,width='120px')),
                         inline_ui(
                           numericInput("legendleft",label = "Legend Left Margin",
                                        value = 5.5,min=0,max=NA,width='120px')),
                         inline_ui(
                           numericInput("legendright",label = "Legend Right Margin",
                                        value = 5.5,min=0,max=NA,width='120px')),
                         inline_ui(
                           numericInput("legendbottom",label = "Legend Bottom Margin",
                                        value = 0,min=0,max=NA,width='120px')),
                         inline_ui(
                           numericInput("legendboxtop",label = "Legend Box Top Margin",
                                        value = 0,min=0,max=NA,width='120px')),
                         inline_ui(
                           numericInput("legendboxleft",label = "Legend Box Left Margin",
                                        value = 5.5,min=0,max=NA,width='120px')),
                         inline_ui(
                           numericInput("legendboxright",label = "Legend Box Right Margin",
                                        value = 5.5,min=0,max=NA,width='120px')),
                         inline_ui(
                           numericInput("legendboxbottom",label = "Legend Box Bottom Margin",
                                        value = 0,min=0,max=NA,width='120px'))
                )
              ),
              checkboxInput('themeaspect', 'Use custom aspect ratio ?'),
              h6("Setting aspect ratio does not work when facets spacing x or y is free."),
              conditionalPanel(condition = "input.themeaspect" , 
                               numericInput("aspectratio",label = "Y/X ratio",
                                            value = 1,min=0.1,max=10,step=0.01)) 

              ) #tabpanel
            )#tabsetpanel
      ), # tabpanel
      #) ,#tabsetPanel(),
      
      tabPanel("How To",value = "sidebar_How_To", 
        includeMarkdown(file.path("text", "howto.md"))
      )# tabpanel 
      )
    ), #sidebarPanel
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "X/Y Plot"  , 
          uiOutput('ui_plot'),
          shinyjs::hidden(div(
            id = "update_plot_area",
            inline_ui(
              checkboxInput("auto_update_plot",
                            "Update plot automatically", value = TRUE)
            ),
            actionButton("update_plot_btn", "Update plot",
                         icon = icon("refresh"))
          )),
          shinyjs::hidden(div(
            id = "save_plot_area",
            inline_ui(
              textInput("save_plot_name", NULL, "",
                        placeholder = "Enter plot name to save")
            ),
            actionButton("save_plot_btn", "Save plot", icon = icon("star")),
            shinyjs::hidden(
              span(
                id = "save_plot_checkmark",
                icon("check")
              )
            )
          )),
          hr(),
          uiOutput("clickheader"),
          tableOutput("plot_clickedpoints"),
          uiOutput("brushheader"),
          tableOutput("plot_brushedpoints"),
          
          tabPanel(
            "Types of Graphs",
            tabsetPanel(
              id = "graphicaltypes",selected = "color_aes_mappings",
              tabPanel("Pairs Plot Options",
                       value = "pairs_plot",
                       fluidRow(
                         column (3, uiOutput("colourpairs"))
                       )
              ),
              tabPanel(
                "Points, Lines",
                value = "points_lines",
                fluidRow(
                  column (
                    3,
                    radioButtons("Points", "Points:",
                                 c("Points" = "Points",
                                   "None" = "None"),inline=TRUE),
                    
                    conditionalPanel(
                      " input.Points!= 'None' ",
                      sliderInput("pointsizes", "Points Size:", min=0, max=6, value=c(1),step=0.1),
                      checkboxInput('pointignoresize', 'Ignore Mapped Size'),
                      radioButtons("jitterdirection", "Add noise/adjust points:",
                                   c("None" = "None",
                                     "Jitter Vertical"  = "Vertical",
                                     "Jitter Horizontal"  = "Horizontal",
                                     "Jitter Both" = "Default",
                                     "Jitter Custom" = "Custom",
                                     "Side By Side"= "dodge",
                                     "Top on Top"= "dodgev"
                                   ),selected="None"
                                   ,inline=TRUE),
                      conditionalPanel(
                        " input.jitterdirection== 'Custom' ",
                        inline_ui(
                        numericInput("jittervertical",label = "Vertical Jitter Height",
                                     value =0.1,min=0,width='120px')) ,
                        inline_ui(numericInput("jitterhorizontal",
                                               label = "Horizontal Jitter Width",value =0.1,min=0,width='120px'))
                      ),
                      conditionalPanel(
                        " input.jitterdirection== 'dodge' ",
                        inline_ui(
                          numericInput("pointdodgewidth",label = "Points dodge width",
                                       value =0.75,min=0,width='120px'))
                      ),
                      conditionalPanel(
                        " input.jitterdirection== 'dodgev' ",
                        inline_ui(
                          numericInput("pointdodgeheight",label = "Points dodge height",
                                       value =0.75,min=0,width='120px'))
                      )
                    )
                  ),
                  column(
                    3,
                    conditionalPanel(
                      " input.Points!= 'None' ",
                      checkboxInput('pointignorecol', 'Ignore Mapped Color'),
                      conditionalPanel(
                        " input.pointignorecol ",
                        colourpicker::colourInput("colpoint", "Points Color", value="black",
                                                  showColour = "both",allowTransparent=FALSE,
                                                  returnName=TRUE),
                        div( actionButton("colpointreset", "Reset Points Color"),
                             style="text-align: right")
                        
                        
                      ),
                      sliderInput("pointstransparency", "Points Transparency:",
                                  min=0, max=1, value=c(0.5),step=0.01),
                      checkboxInput('pointignoreshape', 'Ignore Mapped Shape'),
                      selectInput('pointshapes','Points Shape:',c(
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
                        "triangle down filled"  ,
                        "blank"
                      ),selected="circle small")
                    )
                  ),
                  column(
                    3,
                    radioButtons("line", "Lines:",
                                 c("Lines" = "Lines",
                                   "None" = "None"),selected="None",inline = TRUE),
                    conditionalPanel(
                      " input.line== 'Lines' ",
                      sliderInput("linesize", "Lines Size:", min=0, max=4, value=c(1),step=0.1),
                      checkboxInput('lineignoresize', 'Ignore Mapped Size')
                    )
                  ),
                  column(
                    3,
                    conditionalPanel(
                      " input.line== 'Lines' ",
                      checkboxInput('lineignorecol', 'Ignore Mapped Color'),
                      
                      conditionalPanel(
                        " input.lineignorecol ",
                        colourpicker::colourInput("colline", "Lines Color", value="black",
                                                  showColour = "both",allowTransparent=FALSE,returnName=TRUE),
                        div( actionButton("collinereset", "Reset Lines Color"),
                             style="text-align: right")
                        
                      ),
                      sliderInput("linestransparency", "Lines Transparency:", min=0, max=1, value=c(0.5),step=0.01),
                      checkboxInput('lineignorelinetype', 'Ignore Mapped Linetype'),
                      selectInput('linetypes','Lines Type:',c("solid","dashed", "dotted", "dotdash", "longdash", "twodash","blank"))
                      
                    )
                  ),
                  column (12,
                          h6("Points and Lines Size will apply only if Size By: in the Color Group Split Size Fill Mappings are set to None"))
                  
                )#fluidrow
              ), # tabpanel
              tabPanel(
                "Color/Group/Split/Size/Fill Mappings",
                value = "color_aes_mappings",
                fluidRow(
                  column (3, uiOutput("colour"),uiOutput("group"),uiOutput("facet_col_extra")),
                  column (3, uiOutput("facet_col"),uiOutput("facet_row"),uiOutput("facet_row_extra")),
                  column (3, uiOutput("pointshape") ,uiOutput("linetype")),
                  column (3, uiOutput("pointsize"),uiOutput("fill")),
                  column (12, h6("Make sure not to choose a variable that is in the y variable(s) list otherwise you will get an error Variable not found. These variables are stacked and become yvars and yvalues.This ensures that colour/group/etc. are kept intact when you apply a new filter or recode a variable. When you combine variables all mappings will be updated so you can choose the newly formed variable and as such the previous state will be lost." ))
                  
                )
              ),#tabpanel
              tabPanel(
                "Boxplots",
                value = "box_plots",
                fluidRow(
                  column (
                    4,
                    checkboxInput('boxplotaddition', 'Add a Boxplot ? (makes sense if x variable is categorical and
                                  you Group By a sensible choice. By default the x variable is used for grouping)'),
                    checkboxInput('boxplotignoregroup', 'Ignore Mapped Group ? (can be helpful to superpose a loess or median on top of the boxplot)',value = TRUE)
                  ),
                  column (
                    4,
                    checkboxInput('boxplotvarwidh', "Boxes proportional to the square-roots of the number of observations ?" ),
                    checkboxInput('boxplotnotch', "Notched Boxes ?.
                                  Notches are used to compare groups; if the notches of two boxes do not overlap, this suggests that the medians are significantly different." ),
                    checkboxInput('boxplotshowlegend', "Show Legend ?", value=TRUE)
                    ),
                  
                  column(
                    4,
                    checkboxInput('boxplotignorecol', 'Ignore Mapped Color'),
                    conditionalPanel(
                      " input.boxplotignorecol " ,
                      colourpicker::colourInput('boxcolline','Box Outlines Color',value="black",
                                                showColour = "both",allowTransparent=TRUE,
                                                returnName=TRUE)
                    ),
                    sliderInput("boxplotalpha", "Boxplot Fill Transparency:", min=0, max=1, value=c(0.2),step=0.01),
                    sliderInput("boxplotoutlieralpha", "Outlier Transparency:", min=0, max=1, value=c(0.5),step=0.01),
                    sliderInput("boxplotoutliersize", "Outliers Size:", min=0, max=6, value=c(1),step=0.1)
                  )
                  
                )#fluidrow 
                
                  ),
              tabPanel(
                "Histograms/Density",
                value = "histograms_density",
                fluidRow(
                  column (3,
                    radioButtons("histogramaddition", "Add a Histogram ?",
                                 c("Density" = "Density",
                                   "Counts" = "Counts",
                                   "Scaled Counts" = "ncounts",
                                   "None" = "None") ,
                                 selected="None", inline = TRUE) ,
                    conditionalPanel("input.histogramaddition!='None' ",
                    radioButtons("histogrambinwidth", "Bin Width: ",
                                 c("User Specified" = "userbinwidth",
                                   "Automatic Binwidth" = "autobinwidth",
                                   "N of Bins" = "None") ,
                                   selected = "None", inline = TRUE) ,
                    conditionalPanel(
                      " input.histogrambinwidth== 'userbinwidth' ", 
                      numericInput("histobinwidth","Bin Width Value",
                                   value = 1, min = 0, step = 0.1)
                      
                    ),
                    conditionalPanel(" input.histogrambinwidth== 'None' ",
                       numericInput("histonbins","Number of Bins",
                                    value = 30,min = 0, step = 0.1)
                    )
                    )  
                  ),
                  column (3,
                          conditionalPanel("input.histogramaddition!='None' ",
                                           sliderInput(
                                             "histogramalpha","Histogram Fill Transparency:",
                                             min = 0, max = 1, value = c(0.2), step = 0.01
                                           ),
                          inline_ui(selectInput("positionhistogram",
                                      label = "Histogram positioning for overlap:",
                                      choices = c("Default"="identity",
                                                  "Side By Side"="dodge",
                                                  "Stacked"="stack"),selected = "stack")
                          )
                          )
                  ),
                  column (3,
                    radioButtons("densityaddition", "Add a Density Curve ?",
                                 c("Density" = "Density",
                                   "Counts" = "Counts",
                                   "Match Histo Count"="histocount",
                                   "Scaled Density" = "Scaled Density",
                                   "None" = "None") ,
                                 selected="Density", inline = TRUE),
                    conditionalPanel("input.densityaddition!='None' ",
                    sliderInput(
                      "densityadjust",
                      "Density Binwidth Adjustment:",
                      min = 0.01, max = 10, value = c(1), step = 0.01
                    )
                    )
                    
                  ),
                  column (3,
                          conditionalPanel("input.densityaddition!='None' ",
                                           sliderInput(
                                             "densityalpha",
                                             "Density Fill Transparency:",
                                             min = 0 ,max = 1, value = c(0.2), step = 0.01
                                           )
                          )
                  )
                )),
              tabPanel("Barplots",
                value = "barplots",
                fluidRow(
                  column (3,
                    checkboxInput('barplotaddition', 'Add a Barplot ?', value = TRUE),
                    conditionalPanel("input.barplotaddition",
                                     inline_ui(selectInput("positionbar", label = "Bar positioning:",
                                choices = c("Stacked"="position_stack(vjust = 0.5)",
                                            "Side By Side"="position_dodge(width = 0.9)",
                                            "Sum to 100%"="position_fill(vjust = 0.5)"),
                                selected = "position_stack(vjust = 0.5)") ),
                    checkboxInput('barplotflip', 'Flip the Barplot ?',value = FALSE)
                    )
                  ),
                  column(3,
                         conditionalPanel("input.barplotaddition",
                                          sliderInput("barplotfillalpha",
                                                      "Barplot Fill Transparency:",
                                                      min=0, max=1, value=c(0.2),step=0.01),
                                          radioButtons("barplotorder", "Bar Ordering:",
                                                       c("Default" = "default",
                                                         "By Frequency" = "frequency",
                                                         "By Reverse Frequency" = "revfrequency"),
                                                       inline=TRUE )
                         )
                         ),
                  column (3,
                          conditionalPanel("input.barplotaddition",
                          checkboxInput('barplotlabel', 'Show Counts/Percentages ?', value = FALSE),
                          conditionalPanel(" input.barplotaddition && input.barplotlabel",
                                           checkboxInput('barplotpercent',
                                                         'Compute Percentages instead of Counts ?',
                                                         value = FALSE)
                          ),
                          conditionalPanel(" input.barplotaddition &&
                                           input.barplotlabel && input.barplotpercent ",
                                           numericInput("nroundbarplotpercentdigits",
                                                        label = "Accuracy of Percent Digits",
                                                        value = 1, min=0,max=10) 
                          ),
                          conditionalPanel('input.barplotaddition && input.barplotlabel',
                                           inline_ui(sliderInput("barplotlabelhjust",
                                                                 "Text horizontal justification:",
                                                                 min=-0.2, max=1.2, value=c(0.5),step=0.1,
                                                                 width='120px')),
                                           inline_ui(sliderInput("barplotlabelvjust",
                                                                 "Text vertical justification:",
                                                                 min=-0.2, max=1.2, value=c(0.5),step=0.1,
                                                                 width='120px'))
                          )
                  )
                  ),
                  column(3,
                         conditionalPanel('input.barplotaddition && input.barplotlabel',
                         checkboxInput('ignorebarplotlabelcolor', 'Ignore Mapped Color for labels?'),
                         conditionalPanel(
                           " input.ignorebarplotlabelcolor ",
                           colourpicker::colourInput("barplotlabelcolor", "Text Color", value="black",
                                                     showColour = "both",
                                                     allowTransparent=FALSE,
                                                     returnName=TRUE)
                           ),
                         conditionalPanel('input.barplotaddition && input.barplotlabel',
                                          inline_ui(sliderInput("barplotlabelsize", "Text Size:",
                                                                min=0, max=12, value=c(5), step=0.01)),
                         checkboxInput('barplotlabellegend', "Show Text Legend ?",
                                       value = TRUE)
                         )
                         )
                         ),
                  column (12,
                          conditionalPanel('input.barplotaddition &&
                          input.barplotlabel && input.barplotpercent && input.positionbar == "position_fill(vjust = 0.5)" ',
                  h6("Currently there is ggplot2 text label limitations when Percentages and position 'Sum to 100%' are selected as such `ggally` `stat_prop` computed by the x or y axis values is used.")
                          )
                  )
                  )#fluidrow
              ),
              
              #rqss quantile regression
              tabPanel("Quantile Regression",
                       value = "quantile_regression",
                       fluidRow(
                         column(
                           3,
                           radioButtons("qr","Quantile Regression:",
                                        c("Slider and Predefined Quantile(s)" = "dynamicquantile",
                                          "None" = "None") ,
                                        selected = "None"
                           ),
                           checkboxInput('ignoregroupqr', 'Ignore Mapped Group',value = TRUE),
                           
                           conditionalPanel(
                             " input.qr!= 'None' ",
                             checkboxGroupInput("predefquantiles", "Predefined Quantiles" ,
                                                c("97%" = 0.97,
                                                  "95%" = 0.95,
                                                  "90%" = 0.9,
                                                  "75%" = 0.75,
                                                  "50%" = 0.5,
                                                  "25%" = 0.25,
                                                  "10%" = 0.1,
                                                  "5%" = 0.05,
                                                  "3%" = 0.03
                                                ),inline = TRUE),
                             selectInput('predefquantileslinetype','Predefined Quantiles Line Type:',
                                         c("solid","dashed", "dotted", "dotdash", "longdash", "twodash","1F","blank"),
                                         selected="dashed")
                           )
                           
                           
                         ),
                         column(3,
                                conditionalPanel(
                                  " input.qr!= 'None' ",
                                  sliderInput("Tau", label = "Quantile Slider:",min = 0, max = 1, value = 0.5, step = 0.01)  ,
                                  checkboxInput('hidedynamic', 'Hide Quantile Slider?'),
                                  sliderInput("Penalty", label = "Spline sensitivity adjustment:",min = 0, max = 100, value = 1, step = 0.1),
                                  selectInput("Constraints", label = "Spline constraints:",
                                              choices = c("None"="N","Increasing"="I","Decreasing"="D","Convex"="V","Concave"="C",
                                                          "Convex and Increasing"="VI", "Convex and Decreasing"= "VD",
                                                          "Concave and Increasing"="CI","Concave and Decreasing"= "CD"),
                                              selected = "N")
                                )
                                
                         ),
                         
                         column(
                           3,
                           conditionalPanel(
                             " input.qr!= 'None' ",
                             sliderInput(
                               "qrlinesize",
                               "QR Line(s) Size:",
                               min = 0,
                               max = 4,
                               value = 1.5,
                               step = 0.1
                             ),
                             sliderInput(
                               "qrlinealpha",
                               "QR Line(s) Transparency:",
                               min = 0,
                               max = 1,
                               value = c(0.5),
                               step = 0.01
                             )
                           )
                         ),
                         column(3,
                                checkboxInput('ignorecolqr', 'Ignore Mapped Color'),
                                conditionalPanel(
                                  " input.ignorecolqr ",
                                  colourpicker::colourInput("colqr", "QR Color", value="black",
                                                            showColour = "both",allowTransparent=FALSE,returnName=TRUE))
                                
                         )
                         
                       )#fluidrow
              ),
              
              tabPanel(
                "Smooth/Linear/Logistic Regressions",
                value = "smooth_regression",
                fluidRow(
                  column (2, 
                    radioButtons("Smooth", "Smooth:",
                                 c("Smooth" = "Smooth",
                                   "Smooth and SE" = "Smooth and SE",
                                   "None" = "None"),selected="None", inline = TRUE),
                    conditionalPanel(
                      " input.Smooth!= 'None' ",
                      checkboxInput('ignoregroup', 'Ignore Mapped Group',value = TRUE),
                      uiOutput("weight")
                    ) 
                  ),
                  column (2,
                          conditionalPanel(
                            " input.Smooth== 'Smooth and SE' ",
                            sliderInput("smoothselevel", "CI %:", min=0.5, max=0.99, value=c(0.95),step=0.01),
                            sliderInput("smoothCItransparency", "CI Transparency:", min=0, max=1, value=c(0.2),step=0.01)
                          )    
                  ),
                  column (2, 
                          conditionalPanel( " input.Smooth!= 'None' ",                    
                            sliderInput("smoothlinesize", "Smooth Line(s) Size:",
                                        min=0, max=4,value=c(1.5),step=0.1),
                            sliderInput("smoothlinealpha", "Smooth Line(s) Transparency:",
                                        min=0, max=1, value=c(0.5),step=0.01)
                          )
                  ),
                  column(2, 
                    conditionalPanel(
                      " input.Smooth!= 'None' ",
                      selectInput('smoothmethod', label ='Smoothing Method',
                                  choices=c("Loess" ="loess","Linear Fit"="lm",
                                            "Logistic"="glm1",
                                            "Poisson"="glm2",
                                            "Emax"="emax"),
                                  multiple=FALSE, selectize=TRUE,selected="loess")
                    ),
                    conditionalPanel(
                      "input.Smooth== 'Smooth and SE' & input.smoothmethod== 'emax' ",
                    h5("The emax fit is based on a nonlinear `nls` fit which does not support providing standard errors."),
                    )
                  ),
                 column (2,
                          conditionalPanel(" input.Smooth!= 'None' ",
                                           
                          conditionalPanel(" input.smoothmethod== 'lm' ",
                                           checkboxInput('showslopepvalue', 'Show Slope p-value ?',value = FALSE),
                                           checkboxInput('showadjrsquared', HTML('Show R<sup>2</sup><sub>adj</sub> ?'),value = FALSE),
                                           checkboxInput('showlmequation', HTML('Show Int/Slope values &plusmn SE ?'),value = FALSE)
                          ),
                          conditionalPanel(" input.smoothmethod== 'emax' ",
                                           checkboxInput('shownlsparams', HTML('Show Fitted values &plusmn SE ?'),
                                                         value = FALSE),
                                           checkboxInput('customemaxstart', HTML('Specify Starting values ?'),
                                                         value = FALSE),
                                           checkboxInput('e0fit', 'E0 Fit ?',value = FALSE)
                          ),
                          conditionalPanel(" input.smoothmethod== 'emax' & input.customemaxstart ",
                                           inline_ui(
                                             numericInput("emaxstart",label = "Emax start",
                                                          value = 1, width='80px')
                                             ),
                                           inline_ui(
                                             numericInput("ec50start",label = "EC50 start",
                                                          value = 1, min=0, width='80px')
                                             )
                          ),
                          
                          conditionalPanel(" input.customemaxstart & input.e0fit ",
                                           inline_ui(numericInput("e0start",label = "E0 start",
                                                                  value = 1 , width='80px')
                                                     )
                          ),
                          conditionalPanel(" input.smoothmethod== 'loess' ",
                                           sliderInput("loessens", "Loess Span:", min=0, max=1, value=c(0.75),step=0.05),
                                           selectInput('loessfamily', label ='Loess Family:',
                                                       choices=c("Gaussian" ="gaussian","Symmetric"="symmetric"),
                                                       multiple=FALSE, selectize=TRUE,selected="gaussian"),
                                           sliderInput("loessdegree", "Loess Degree:", min=0, max=2, value=c(1),step=1)
                          ),
                          conditionalPanel(" input.smoothmethod== 'emax' | input.smoothmethod== 'lm' ",
                                           sliderInput("smoothtextsize", "Text Size:", min=0, max=10,
                                                       value=c(3.88),step=0.01)
                                           
                          )
                  )
                  ),
                  column (2,
                          conditionalPanel( " input.Smooth!= 'None' ",
                                            checkboxInput('smoothignorecol', 'Ignore Mapped Color'),
                                            conditionalPanel(
                                              " input.smoothignorecol ",
                                              colourpicker::colourInput("colsmooth", "Smooth Line Color", value="black",
                                                                        showColour = "both",allowTransparent=FALSE,returnName=TRUE),
                                              div( actionButton("colsmoothreset", "Reset Smooth Color"),
                                                   style="text-align: right")
                                            )
                          )
                  )
                  
                )#fluidrow
              )
              ,
              ### Mean CI section
              tabPanel("Mean (CI)",
                       value = "mean_ci",
                       fluidRow(
                         column (2,
                                 radioButtons(
                                   "Mean",
                                   "Mean:",
                                   c(
                                     "Mean" = "Mean",
                                     "Mean/CI" = "Mean/CI",
                                     "None" = "None"
                                   ) ,
                                   selected = "None",
                                   inline = TRUE
                                 ),
                                 conditionalPanel(
                                   " input.Mean!= 'None' ",
                                   checkboxInput('meanignoregroup', 'Ignore Mapped Group', value = TRUE),
                                   selectInput("positionmean", label = "Mean positioning for overlap:",
                                               choices = c(
                                                 "Default"="position_identity",
                                                 "Side By Side"="position_dodge"
                                               ),selected = "position_identity")
                                   
                                 ),
                                 conditionalPanel(
                                   " input.Mean== 'Mean/CI' && (
                      input.geommeanCI == 'errorbar' | input.positionmean =='position_dodge')  ",
                                   numericInput(
                                     inputId = "errbar",
                                     label = "CI errorbar/dodge width:",
                                     value = 0.75,
                                     min = 0.1,
                                     max = NA
                                   )
                                 )
                                 
                         ),#first column
                         column (2,
                                conditionalPanel(
                                   " input.Mean== 'Mean/CI' ",
                                   radioButtons("geommeanCI", "CI Geom:",
                                                c("errorbar" = "errorbar",
                                                  "ribbon"= "ribbon"),
                                                selected = "errorbar",
                                                inline = TRUE),
                                   sliderInput("CI","CI %:",
                                     min = 0, max = 1, value = c(0.95), step = 0.01),
                                   sliderInput("meancitransparency", "CI Transparency:",
                                               min=0, max=1, value=c(0.2),step=0.01),
                                   conditionalPanel(" input.geommeanCI == 'errorbar' ",
                                    sliderInput("meancierrorbarsize","Errorbar(s) Line(s) Size:",
                                                min=0, max=4, value=c(1),step=0.1)
                                   )
                                )       
                                 
                         ),# ci column options
                         column (2,
                                 conditionalPanel(
                                   " input.Mean!= 'None' ",
                                   checkboxInput('meanlines', 'Show lines', value =TRUE)
                                 ),
                                 conditionalPanel(
                                   " input.Mean!= 'None'&& input.meanlines ",
                                   sliderInput(
                                     "meanlinesize",
                                     "Mean(s) Line(s) Size:",
                                     min = 0,
                                     max = 4,
                                     value = 1.5,
                                     step = 0.1
                                   ),
                                   sliderInput(
                                     "alphameanl",
                                     "Mean(s) Line(s) Transparency:",
                                     min = 0,
                                     max = 1,
                                     value = c(0.5),
                                     step = 0.01
                                   )
                                 )
                         ),#second column
                         column (2,
                                 conditionalPanel(
                                   " input.Mean!= 'None' ",
                                   checkboxInput('meanpoints', 'Show points')
                                 ),
                                 conditionalPanel(
                                   " input.Mean!= 'None' & input.meanpoints ",
                                   sliderInput(
                                     "meanpointsize",
                                     "Mean(s) Point(s) Size:",
                                     min = 0,
                                     max = 6,
                                     value = 1,
                                     step = 1
                                   ),
                                   sliderInput(
                                     "alphameanp",
                                     "Mean(s) Point(s) Transparency:",
                                     min = 0,
                                     max = 1,
                                     value = c(0.5),
                                     step = 0.01
                                   ),
                                   checkboxInput('forcemeanshape', 'Force Mean(s) Shape',
                                                 value = FALSE),
                                   conditionalPanel(
                                     " input.Mean!= 'None'&input.meanpoints & input.forcemeanshape ",
                                     selectInput('meanshapes','Mean(s) Point(s) Shape:',
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
                                                   "triangle down filled"  ,
                                                   "blank"
                                                 ),
                                                 selected = "diamond"
                                     )
                                   )
                                 )
                                 
                         ),#third column
                         column(2,
                                conditionalPanel(
                                  " input.Mean!= 'None' ",
                                  checkboxInput('meanvalues', 'Label Values?') ,
                                  checkboxInput('meanN', 'Label N?') ),
                                
                                conditionalPanel(
                                  "input.meanvalues | input.meanN ",
                                  sliderInput("alphameanlabel", "Labels(s) Transparency:",
                                              min=0, max=1, value=c(0.5),step=0.01),
                                  radioButtons("geommeanlabel", "Mean Label Geom:",
                                               c("text" = "text","label"= "label",
                                                 "auto text repel" = "text_repel",
                                                 "auto label repel" = "label_repel"),selected = "text_repel",
                                               inline = TRUE)
                                )
                                
                                ), # fourth column
                         column(2,
                                conditionalPanel(
                                  " input.Mean!= 'None' ",
                                  checkboxInput('meanignorecol', 'Ignore Mapped Color') ,
                                  conditionalPanel(" input.meanignorecol ",
                                                   conditionalPanel(
                                                     " input.meanlines ",
                                                     colourpicker::colourInput(
                                                       "colmeanl",
                                                       "Mean(s) Line(s) Color",
                                                       value = "black",
                                                       showColour = "both",
                                                       allowTransparent = FALSE,
                                                       returnName = TRUE)
                                                   ),
                                                   conditionalPanel(" input.meanpoints ",
                                                                    colourpicker::colourInput(
                                                                      "colmeanp",
                                                                      "Mean(s) Points(s) Color",
                                                                      value = "black",
                                                                      showColour = "both",
                                                                      allowTransparent = FALSE,
                                                                      returnName = TRUE)
                                                   )
                                  )

                                )
                         )#column 5
                       ) #fluidrow
              ), # tab panel for mean
              
              
              ### median PI section
              tabPanel("Median (PIs)",
                       value = "median_pi",
               fluidRow(
                  column (2,
                    radioButtons("Median", "Median:",
                                 c("Median" = "Median",
                                   "Median/PI" = "Median/PI",
                                   "None" = "None") ,selected="None",
                                 inline = TRUE),
                    conditionalPanel( " input.Median!= 'None' ",
                    checkboxInput('medianignoregroup', 'Ignore Mapped Group',value = TRUE),
                    selectInput("positionmedian", label = "Median positioning for overlap:",
                                choices = c(
                                  "Default"="position_identity",
                                  "Side By Side"="position_dodge"
                                ),selected = "position_identity"),
                    conditionalPanel(
                      " input.Median== 'Median/PI' && (
                      input.geommedianPI == 'errorbar' | input.positionmedian =='position_dodge')  ",
                      numericInput(
                        inputId = "medianerrbar",
                        label = "PI errorbar/dodge width:",
                        value = 0.75,
                        min = 0.1,
                        max = NA
                      )
                    )
                    
                    )
                  ),#first column
                column (2,
                  conditionalPanel( " input.Median== 'Median/PI' ",
                                    radioButtons("geommedianPI", "PI Geom:",
                                                 c("errorbar" = "errorbar",
                                                   "ribbon"= "ribbon"),
                                                 selected = "ribbon",
                                                 inline = TRUE),
                                    sliderInput("PI", "PI %:", min=0, max=1, value=c(0.95),step=0.01),
                                    sliderInput("PItransparency", "PI Transparency:",
                                                min=0, max=1, value=c(0.2),step=0.01),
                                    conditionalPanel(" input.geommedianPI == 'errorbar' ",
                                    sliderInput("PIerrorbarsize","Errorbar(s) Line(s) Size:",
                                                min=0, max=4, value=c(1),step=0.1)
                                    )
                                    
                  ) 
                  
                ), # Pi color options
                
                  column (2,
                    conditionalPanel( " input.Median!= 'None' ",
                                      checkboxInput('medianlines', 'Show lines',value=TRUE)
                                      ),
                    conditionalPanel(
                      " input.Median!= 'None' && input.medianlines && (input.pointsizein == 'None' ) ",
                                      sliderInput("medianlinesize", "Median(s) Line(s) Size:",
                                                  min=0, max=4, value=c(1.5),step=0.1)
                      ),
                    conditionalPanel(
                      " input.Median!= 'None'&& input.medianlines",
                      sliderInput("alphamedianl", "Median(s) Line(s) Transparency:",
                                  min=0, max=1, value=c(0.5),step=0.01)
                    )
                  ), # lines options
                  column (2,
                    conditionalPanel( " input.Median!= 'None' ",
                                      checkboxInput('medianpoints', 'Show points')
                    ),
                    conditionalPanel( " input.Median!= 'None' & input.medianpoints ",
                                      sliderInput("medianpointsize", "Median(s) Point(s) Size:", min=0, max=6, value=c(1),step=0.1),
                                      sliderInput("alphamedianp", "Median(s) Point(s) Transparency:", min=0, max=1, value=c(0.5),step=0.01),
                                      checkboxInput('forcemedianshape', 'Force Median(s) Shape',value = FALSE)
                    ),
                    
                    conditionalPanel( " input.Median!= 'None'&input.medianpoints & input.forcemedianshape ",
                                      selectInput('medianshapes','Median(s) Point(s) Shape:',c(
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
                                        "triangle down filled"  ,
                                        "blank"
                                      ),selected="square")  
                                      
                    )
                    
                  ), # points options

                column(2,
                       conditionalPanel(" input.Median!= 'None' ",
                       checkboxInput('medianvalues', 'Label Values?') ,
                       checkboxInput('medianN', 'Label N?') ),
                       conditionalPanel(
                         "input.medianvalues | input.medianN ",
                         sliderInput("alphamedianlabel", "Labels(s) Transparency:",
                                     min=0, max=1, value=c(0.5),step=0.01),
                         radioButtons("geommedianlabel", "Median Label Geom:",
                                      c("text" = "text","label"= "label",
                                        "auto text repel" = "text_repel",
                                        "auto label repel" = "label_repel"),selected = "text_repel",
                                      inline = TRUE)
                       )
                       
                ), # fourth column
                
                column (2,
                       conditionalPanel(" input.Median!= 'None' ",
                       checkboxInput('medianignorecol', 'Ignore Mapped Color'),
                       conditionalPanel(" input.medianignorecol ",
                       conditionalPanel( " input.Median!= 'None' ",
                                        colourpicker::colourInput("colmedianl",
                                                                  "Median(s) Line(s) Color",
                                                                  value="black",
                                                                  showColour = "both",
                                                                  allowTransparent=FALSE,
                                                                  returnName=TRUE)              
                                                       ),
                       conditionalPanel( " input.medianpoints ",
                                         colourpicker::colourInput("colmedianp",
                                                                   "Median(s) Point(s) Color",
                                                                   value="black",
                                                                   showColour = "both",
                                                                   allowTransparent=FALSE,
                                                                   returnName=TRUE)
                                                       )
                                      )
                    )#main condition
                  )# column #5
                  
                )#fluidrow
              ),
              ### median PI section
              
              ### KM section
              
              
              tabPanel("Kaplan-Meier (CI)",
                       value = "kaplan_meier",
                       fluidRow(
                         column (2,
                                 radioButtons("KM", "KM:",
                                              c("KM" = "KM","KM/CI" = "KM/CI","None" = "None"),
                                              selected="None",
                                              inline = TRUE) ,
                                 conditionalPanel( " input.KM!= 'None' ",
                                                   checkboxInput('KMignoregroup', 'Ignore Mapped Group',value = TRUE)
                                 )#kmnotnotnone
                         ) ,#first km column
                         column (2,
                                 
                                 conditionalPanel( " input.KM== 'KM/CI' ",
                                                   sliderInput("KMCI", "KM CI:", min=0, max=1, value=c(0.95),step=0.01),
                                                   sliderInput("KMCItransparency", "KM CI Transparency:", min=0, max=1, value=c(0.2),step=0.01),
                                                   radioButtons("kmconftype", "KM CI type:",
                                                                c("none" = "none",
                                                                  "plain" = "plain",
                                                                  "log" = "log",
                                                                  "log-log" = "log-log",
                                                                  "logit" = "logit"),
                                                                selected = "log",
                                                                inline = TRUE),
                                                   radioButtons("kmconflower", "KM lower CI type:",
                                                                c("usual" = "usual",
                                                                  "peto" = "peto",
                                                                  "modified" = "modified"),
                                                                selected = "log",
                                                                inline = TRUE)
                                 )
                         ),#second km colonne
                         column (2,
                                 conditionalPanel( " input.KM!= 'None' ",
                                                   sliderInput("kmlinesize", "KM Line(s) Size:", min=0, max=6, value=c(1),step=0.1),
                                                   sliderInput("kmlinealpha", "KM Line(s) Transparency:", min=0, max=1, value=c(0.5),step=0.01),
                                                   radioButtons("addmediansurv", "Add Median Survival:",
                                                                c("Median" ="addmediansurvival"  ,
                                                                  "Median/95%CI" = "addmediancisurvival",
                                                                  "None" = "None")
                                                                ,selected="None"),
                                                   conditionalPanel( " input.addmediansurv!= 'None' ",
                                                                     checkboxInput('arrowmedian', 'Draw Arrow?')
                                                   )#addmedian
                                                   
                                 )#kmnotnotnone
                         ),#3 km column
                         column (2,
                                 conditionalPanel(" input.KM!= 'None' ",
                                                  selectInput('KMtrans', label ='KM Transformation',
                                                              choices=c("None" ="identity","event"="event",
                                                                        "cumhaz"="cumhaz","cloglog"="cloglog"),
                                                              multiple=FALSE, selectize=TRUE,selected="identity"),
                                                  checkboxInput('censoringticks', 'Show Censoring Ticks?'),
                                                  checkboxInput('reversecenstoevent', 'Status is Censoring Flag ?') 
                                 )#kmnotnotnone
                         ),#4 km column
                         column (2,
                                 conditionalPanel(" input.KM!= 'None' ",
                                checkboxInput('addrisktable', 'Add Risk Table',value = FALSE)),
                                 conditionalPanel( " input.KM!= 'None' & input.addrisktable ",
                                                   selectizeInput("risktablevariables", 'Numbers to Show:',
                                                                  choices = c("n.risk","n.event","n.censor",
                                                                              "pct.risk","cum.n.event",
                                                                              "cum.n.censor") ,multiple=TRUE,
                                                                  selected = c("n.risk","n.censor")),
                                                   checkboxInput('addhorizontallines', 'Draw Horizontal lines',value = TRUE),
                                                   numericInput(inputId = "breaktimeby",
                                                                label = "Show Numbers Every x time unit:",value="", min = 0,max = NA),
                                                   sliderInput("nriskpositionscaler", "Numbers position scaler:", min=0.1, max=1, value=c(0.2),step=0.01),
                                                   sliderInput("nriskpositiondodge", "Numbers vertical dodge scaler:", min=-1, max=1, value=c(0.2),step=0.01)
                                 )#risktable
                         ),
                         
                         column (2,
                                 conditionalPanel(" input.KM!= 'None' ",
                                                  checkboxInput('kmignorecol', 'Ignore Mapped Color'),
                                                  conditionalPanel(" input.kmignorecol ",
                                                                   colourpicker::colourInput(
                                                                     "colkml",
                                                                     "KM Line(s) Color",
                                                                     value = "black",
                                                                     showColour = "both",
                                                                     allowTransparent = FALSE,
                                                                     returnName = TRUE
                                                                   ),
                                                                   conditionalPanel(
                                                                     " input.censoringticks ",
                                                                     colourpicker::colourInput(
                                                                       "colkmticks",
                                                                       "Censoring Tick(s) Color",
                                                                       value = "black",
                                                                       showColour = "both",
                                                                       allowTransparent =FALSE,
                                                                       returnName = TRUE
                                                                     )
                                                                   )#censorticks 
                                                  )#ignorecolor
                                 )#kmnotnone
                         ),#column 4
                         column (12, h6("KM curves support is now expanded. When a KM curve is added nothing else will be plotted (e.g. points, lines etc.).Color/Fill/Linetype/Group/Facets work." ))
                         
                         
                       )#fluidrow
              ), #tabpanel km
              ### KM section
              tabPanel(
                "Correlation Coefficient",
                value = "corr_coeff",
                fluidRow(
                  column(3,
                         checkboxInput('addcorrcoeff',
                                       "Add Correlation Coefficient to the plot ?"),
                         conditionalPanel(
                           " input.addcorrcoeff ",
                         checkboxInput('addcorrcoeffignoregroup',"Ignore Mapped Group ?", value=TRUE),
                         radioButtons("geomcorr", "Corr Label Geom:",
                                      c("text" = "text",
                                        "auto text repel" = "text_repel"),selected = "text_repel",
                                      inline = TRUE)
                         )
                  ),
                  column(3,
                         conditionalPanel(
                           " input.addcorrcoeff ",
                           conditionalPanel( condition = "input.geomcorr=='text'" ,
                                             inline_ui(numericInput("cortextxpos",label = "Correlation x position",
                                                          value =0,width='120px') ),
                                             inline_ui(numericInput("cortextypos",label = "Correlation y position",
                                                          value =0,width='120px') )
                                             ),
                           sliderInput("corrlabelsize", "Text Size:", min=0, max=6, value=c(3.88), step=0.01),
                           
                         )
                  ),
                  column(3,
                         conditionalPanel(
                           " input.addcorrcoeff ",
                           selectInput("corrtype", label = "Correlation Method:",
                                       choices = c("pearson"="pearson",
                                                   "kendall"= "kendall",
                                                   "spearman"= "spearman"
                                       ) ,
                                       selected = "pearson"),
                           checkboxInput('addcorrcoeffpvalue',"Show R p-value?", value=FALSE)
                         )
                  ),
                  column(3,
                         conditionalPanel(
                           " input.addcorrcoeff ",
                           checkboxInput('corrignorecol', 'Ignore Mapped Color'),
                           conditionalPanel(
                             " input.corrignorecol ",
                             conditionalPanel(
                               " input.addcorrcoeff ",
                               colourpicker::colourInput(
                                 "corrcol",
                                 "Correlation Color",
                                 value =
                                   "black",
                                 showColour = "both",
                                 allowTransparent =
                                   FALSE,
                                 returnName = TRUE
                               )
                             )
                             
                           ),
                           checkboxInput('correlationshowlegend', "Show Legend ?", value=TRUE)
                         )
                  )
                  
                )#fluidrow
              ),##tabpanel corr
              
              tabPanel("Text Labels",
                       value ="text_labels",
                fluidRow(
                  column (3,
                    checkboxInput('addcustomlabel',
                                  "Add Text Labels from Data?"),
                    checkboxInput('addcustomlabelignoregroup',"Ignore Mapped Group ?", value=TRUE),
                    checkboxInput('customlabellegend',"Show Legend ?", value=FALSE),
                    radioButtons("geomlabel", "Label Geom:",
                                 c("text" = "text","label" = "label",
                                   "auto text repel" = "text_repel",
                                   "auto label repel" = "label_repel"), inline = TRUE)
                  ),
                  column(3,
                         conditionalPanel(
                           " input.addcustomlabel ",
                           uiOutput("labeltext")
                         ),
                         checkboxInput('scalesizearea',"Scale Size by Area ?", value=FALSE),
                         conditionalPanel(
                           " !input.scalesizearea ",
                           sliderInput("scalesizearearange1", "Label Size Range:", min=0, max=10, value=c(1,6))
                         ),
                         conditionalPanel(
                           " input.scalesizearea ",
                           sliderInput("scalesizearearange2", "Label Size Range:", min=0, max=10, value=c(6))
                         )
                         
                  ),
                  column(3,
                         conditionalPanel(
                           " input.addcustomlabel ",
                           checkboxInput('labelignoresize', 'Ignore Mapped Size')),
                         conditionalPanel(
                           " input.addcustomlabel && input.labelignoresize  ",
                           sliderInput("labelsize", "Label Size:", min=0, max=6, value=c(1),step=0.1)
                         ),
                         conditionalPanel(" input.addcustomlabel ",
                                            checkboxInput('roundlabeldigits', 'Round the numeric labels digits?')),
                         conditionalPanel(" input.roundlabeldigits && input.roundlabeldigits",
                                          numericInput("nroundlabeldigits",label = "N Digits",
                                                       value = 0,min=0,max=10) )
                  ),
                  
                  column(3,
                         conditionalPanel(
                           " input.addcustomlabel ",
                           checkboxInput('customlabelignorecol', 'Ignore Mapped Color'),
                           conditionalPanel(
                             " input.customlabelignorecol ",
                             conditionalPanel(
                               " input.addcustomlabel ",
                               colourpicker::colourInput(
                                 "customlabelcol",
                                 "Text Labels Color",
                                 value =
                                   "black",
                                 showColour = "both",
                                 allowTransparent =
                                   FALSE,
                                 returnName = TRUE
                               )
                             )
                             
                           )
                         )
                  ),
                  
                  column (12, h6("Custom Label Size applies if no Size is applied or if explicitly ignored. Text repelling can take time. Size scales applies to all geoms that use continuous scale size." ))
                  
                )
              ),
              tabPanel("Rug Marks",
                       value ="rug_marks",
                       fluidRow(
                         column(3,
                                 checkboxInput('addrugmarks', 'Add X/Y rug marks?',
                                               value = FALSE),
                                 conditionalPanel(condition = "input.addrugmarks",
                                                  selectInput('rugsides',
                                                              label ='X/Y rug marks sides',
                                                              choices=c("Left" = "l",
                                                                        "Top" ="t ",
                                                                        "Right"="r",
                                                                        "Bottom"="b"),
                                                              multiple=TRUE, selectize=TRUE,selected="b")
                                                  
                                 )
                         ),
                         column(3,
                                checkboxInput('addextrarugmarks',
                                              'Add rug marks to addtional variables?',
                                              value = FALSE),
                                conditionalPanel(condition = "input.addextrarugmarks",
                                                 selectInput('extrarugsides',
                                                             label ='Extra Variables rug marks sides',
                                                             choices=c("Left" = "l",
                                                                       "Top" ="t ",
                                                                       "Right"="r",
                                                                       "Bottom"="b"),
                                                             multiple=TRUE, selectize=TRUE,selected="b"),
                                 uiOutput("xcolrug"))
                         ),
                         column(3,
                                conditionalPanel(condition = "input.addrugmarks | input.addextrarugmarks",
                                sliderInput("ruglinelength", "rug line length (percentage of the plot area):", min=0, max=1,value=c(0.03),step=0.005),
                                sliderInput("ruglinealpha", "rug line transparency:", min=0, max=1, value=c(0.5),step=0.01)
                                )
                                ),
                         column(3,
                         conditionalPanel(condition = "input.addrugmarks | input.addextrarugmarks",
                           checkboxInput('rugignorecol', 'Ignore Mapped Color'),
                           conditionalPanel(condition = " input.rugignorecol ",
                             colourpicker::colourInput("colrug", "rug Color", value="black",
                                                       showColour = "both",
                                                       allowTransparent=FALSE, returnName=TRUE)
                           )
                         )
                         )
                       )#fluidrow
              )#tabpanel
              )#tabsetPanel
          )#tabPanel
          
      ),#tabPanel1
      tabPanel(
        "Export Plots", 
        conditionalPanel(
          condition = "!output.saved_plots_exist",
          h2("You do not have any saved plots to export")
        ),
        conditionalPanel(
          condition = "output.saved_plots_exist",
          fluidRow(
            column(
              4,
              h2("Export Options"),
              div(
                id = "exporting_plots_options",
                selectInput("export_file_type", "File type",
                            c("PDF" = "pdf", "JPEG" = "jpeg", "PNG" = "png")),
                conditionalPanel(
                  condition = "input.export_file_type == 'pdf'",
                  selectInput("export_pdf_orientation", "Page orientation",
                              c("Portrait (8.5\" x 11\")" = "portrait",
                                "Landscape (11\" x 8.5\")" = "landscape",
                                "Custom dimensions" = "custom")
                  ),
                  conditionalPanel(
                    condition = "input.export_pdf_orientation == 'custom'",
                    numericInput("export_pdf_width", "Page width (inches)",
                                 value = 8.5, min = 1, max = 50, step = 0.5),
                    numericInput("export_pdf_height", "Page height (inches)",
                                 value = 11, min = 1, max = 50, step = 0.5)
                  )
                ),
                conditionalPanel(
                  condition = "input.export_file_type != 'pdf'",
                  numericInput("export_file_width", "Image width (pixels)",
                               value = 480, min = 100, max = 2000),
                  numericInput("export_file_height", "Image height (pixels)",
                               value = 480, min = 100, max = 2000)
                ),
                checkboxInput("export_multiple", "Multiple plots per page"),
                conditionalPanel(
                  condition = "input.export_multiple",
                  selectInput("export_arrangement", NULL,
                              c("Arrange plots by row" = "byrow",
                                "Arrange plots by column" = "bycol")),
                  numericInput("export_nrow", "Rows per page",
                               value = 1, min = 1, max = 20),
                  numericInput("export_ncol", "Columns per page",
                               value = 1, min = 1, max = 20)
                  
                ),
                uiOutput("export_btn_ui")
              )
            ),
            column(
              8,
              h2("Preview"),
              strong("Remove plot"), br(),
              inline_ui(uiOutput("plots_remove_ui")),
              actionButton("remove_plot_btn", "Remove"),
              uiOutput("plots_order_ui"),
              div(
                id = "preview_plots_options",
                uiOutput("plots_select_page_ui"),
                plotOutput("plot_preview", height = "auto")
              )
            )
          )
        )
      ),
      
      tabPanel("Experimental Plotly",
               p("Note: This is experimental and does not work all the time due to ploty::ggploty limitations."),
               uiOutput('ui_plotly')),
      tabPanel("Descriptive Stats",
               p("Note: use y for variables of interest (rows) and x for stratification (columns). Drag and Drop the y variable(s) list on the left to the order of your liking. When more than one x is selected the first will be used."),
               htmlOutput("dstats"),
               shinyjs::hidden(div(
                 id = "table_options_area",
                 inline_ui(
                   checkboxInput("auto_update_table",
                                 "Update table automatically", value = TRUE)
                 ),
                 actionButton("update_table_btn", "Update table",
                              icon = icon("refresh")),
                 fluidRow(
                   column(3,
                          div(id="quick_relabel_placeholder"),
                          uiOutput("dstats_col_extra"),
                          uiOutput("flipthelevels")
                   ),
                   column(3,
                          div(id="quick_reorder_placeholder"),
                          textInput('tablecaption', 'Table title', value = "") ,
                          textInput('tablefootnote', 'Table caption', value = "") ,
                          
                   ),
                   column(6,
                          checkboxInput("table_incl_overall",
                                        label="Include Overall column?",
                                        value=TRUE),
                          selectInput("table_style",
                                      label="Style",
                                      choices=c("Default"="t1default",
                                                "Zebra"="t1zebra",
                                                "Grid"="t1grid")),
                          selectizeInput("dstats_cont_list",
                                         label="Statistics to display for continuous variables (per line)",
                                         choices=allstats,
                                         selected=c("Mean (SD)", "Median [Min, Max]"),
                                         multiple=TRUE,
                                         options=list(plugins=list('drag_drop','remove_button'))),
                          checkboxInput("table_suppress_missing",
                                        label="Suppress Missing Stats?",
                                        value=FALSE),
                          numericInput("dstats_sigfig",
                                       label="Number of significant figures (for Mean, SD, ...)",
                                       value=3, min=1, max=10, step=1),
                          checkboxInput("round_median_min_max",
                                        label="Also round median, min, max?",
                                        value=TRUE),
                          checkboxInput("table_na_is_category",
                                        label="Missing as a Category?",
                                        value=FALSE)
                   )
                 )
               ))
      ),
      
      tabPanel(
        'Data',
        dataTableOutput("mytablex") 
      ),#tabPanel2
      
      tabPanel(
        'Plot Code',
        h5("Plot code generation: to reproduce a plot, in the data tab show all rows and save the plotdata into a csv, read back to R naming it plotdata then copy paste the code below. If you face inputs that are not yet supported, please file a github issue."),
        verbatimTextOutput("plotcode")
      )
      )#tabsetPanel
      )#mainPanel
  )#sidebarLayout
)#fluidPage
