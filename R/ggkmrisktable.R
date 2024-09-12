#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom rlang syms
#' @importFrom survival survfit
#' @importFrom survival Surv
#' @import tidyr

tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD",
               "#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

tableau20 <- c("#1F77B4","#AEC7E8", "#FF7F0E","#FFBB78"  ,"#2CA02C",
               "#98DF8A" ,"#D62728","#FF9896" ,"#9467BD","#C5B0D5" ,
               "#8C564B","#C49C94" ,"#E377C2","#F7B6D2" ,"#7F7F7F",
               "#C7C7C7" ,"#BCBD22","#DBDB8D" ,"#17BECF","#9EDAE5")

certara_palette<- c(
  "#4682ac", "#ee3124", "#fdbb2f", "#6d405d", "#093b6d", "#2f71fd", "#336343", "#803333",
  "#279594", "#ef761b", "#29398c", "#32a17e", "#d89a17", "#d64d20", "#9da1bd", "#9c8777",
  "#7059a6", "#e07070", "#475c6b", "#75604D", "#067f97", "#b7a148", "#f98068", "#72cbed",
  "#b8a394", "#b35d1b", "#a52f43", "#113df2", "#f2c611", "#52ccbb"
) 


lung_long <-  survival::lung |>
 dplyr::mutate(status = ifelse(status==1,0,1)) |>
 tidyr::gather(Endpoint,DV,status) |>
 dplyr::filter(!is.na(ph.karno))|>
 dplyr::filter(!is.na(pat.karno))|>
 dplyr::filter(!is.na(ph.ecog))
lung_long$ph.ecog <- ifelse(lung_long$ph.ecog>1,2,lung_long$ph.ecog)
lung_long$ph.ecog <- as.factor(lung_long$ph.ecog )
lung_long$ph.ecog <- as.factor(lung_long$ph.ecog )
lung_long$facetdum <- "(all)"

# from survminer
.clean_strata <- function(strata, fit){
  is_dollar_sign <- grepl("$", as.character(strata)[1], fixed=TRUE)
  if(is_dollar_sign) {
    strata <- as.character(strata)
    data_name <- unlist(strsplit(strata[1], "$", fixed =TRUE))[1]
    strata <- gsub(paste0(data_name, "$"), "", strata, fixed=TRUE)
    strata <- as.factor(strata)
  }
  else if(!missing(fit)) strata <- factor(strata, levels = names(fit$strata))
  return(strata)
}

.get_variables <- function(strata, fit, data = NULL){
  variables <- sapply(as.vector(strata),
                      function(x){
                        x <- unlist(strsplit(x, "=|,\\s+", perl=TRUE))
                        x[seq(1, length(x), 2)]
                      })
  #variables <- unique(as.vector(variables))
  variables <- unique(as.vector(unlist(variables)))
  variables <- intersect(variables, colnames(.get_data(fit, data) ))
  variables
}

.get_data <- function(fit, data = NULL, complain = TRUE) {
  if(is.null(data)){
    if (complain)
      warning ("The `data` argument is not provided. Data will be extracted from model fit.")
    data <- eval(fit$call$data)
    if (is.null(data))
      stop("The `data` argument should be provided either to ggsurvfit or survfit.")
  }
  data
}

.get_variable_value <- function(variable, strata, fit, data = NULL){
  res <- sapply(as.vector(strata), function(x){
    x <- unlist(strsplit(x, "=|(\\s+)?,\\s+", perl=TRUE))
    # When a factor name is the same as one of its level, index is of length 2
    index <- grep(paste0("^", variable, "$"), x)[1]
    .trim(x[index+1])
  })
  res <- as.vector(res)
  var_levels <- levels(.get_data(fit, data)[, variable])
  if(!is.null(var_levels)) res <- factor(res, levels = var_levels)
  else res <- as.factor(res)
  res
}

.get_choice_items <- function(data, x = NULL, y = NULL, pastevarin = NULL) {
  items <- names(data)
  names(items) <- items
  items <- c("None",items)
  if ( !is.null(x) ){
    items <- c(items, "yvars","yvalues") 
  }
  if ( !is.null(y) ){
    items <- c(items, "xvars","xvalues") 
  }
  if (!is.null(pastevarin) && length(pastevarin) > 1 ){
    nameofcombinedvariables<- paste(as.character(pastevarin),collapse="_",sep="") 
    items <- c(items,nameofcombinedvariables)
  }
  return(items)
}

.get_choice_items_char <- function(data) {
  MODEDF <- sapply(data, function(x) is.numeric(x))
  NAMESTOKEEP2<- names(data)  [ !MODEDF ]
  items <- NAMESTOKEEP2
  names(items) <- items
  items <- c("None",items)
  return(items)
}

.get_choice_items_num <- function(data) {
  MODEDF <- sapply(data, function(x) is.numeric(x))
  NAMESTOKEEP2<- names(data)[MODEDF]
  items <- c("None",NAMESTOKEEP2, "yvalues") 
  return(items)
}

.get_choice_facet_scales <- function(x = NULL, y = NULL) {
  items <- c("fixed","free_x","free_y","free")   
  if (is.null(x) && !is.null(y) && length(y) > 1 ){
    items <- c("free_y","fixed","free_x","free")    
  }
  if (is.null(y) && !is.null(x) && length(x) > 1 ){
    items <- c("free_x","fixed","free_y","free")    
  }
  if (!is.null(x) && !is.null(y) && (length(y) > 1  || 
                                     length(x) > 1)  ){
    items <- c("free","fixed","free_x","free_y")    
  }
  return(items)
}

.trim <- function(x){gsub("^\\s+|\\s+$", "", x)}
# from survminer

#' Create a Kaplan-Meier plot with risk table 
#'
#' Produces a km plot with a facettable risk table in ggplot2
#' 
#' @param data Data to use with multiple endpoints stacked into time, status, endpoint name
#' @param time name of the column holding the time to event information default to `time`
#' @param status name of the column holding the event information default to `DV`
#' @param endpoint name of the column holding the name/key of the endpoint default to `Endpoint`
#' @param groupvar1 name of the column to group by, default `Endpoint`
#' @param groupvar2 name of the column to group by in addition to groupvar1, default `expname`
#' @param groupvar3 name of the column to group by in addition to groupvar1 and groupvar2, default "none"
#' @param exposure_metrics name(s) of the column(s) to be stacked into `expname` `exptile` and split into `exposure_metric_split`
#' @param exposure_metric_split one of "median", "tertile", "quartile", "none"
#' @param exposure_metric_soc_value  special exposure code for standard of care default -99 
#' @param exposure_metric_plac_value special exposure code for placebo default 0
#' @param show_exptile_values FALSE
#' @param show_exptile_values_pos "left" or "right"
#' @param show_exptile_values_textsize default to 5
#' @param show_exptile_values_order the order of the entries "default" or "reverse"
#' @param color_fill name of the column to be used for color/fill default to `exptile`
#' @param linetype name of the column to be used for linetype default to `exptile`
#' @param xlab text to be used as x axis label
#' @param ylab text to be used as y axis label
#' @param nrisk_table_plot TRUE
#' @param nrisk_table_variables one or more from: "n.risk", "pct.risk", "n.event, "cum.n.event, "n.censor"
#' @param nrisk_table_breaktimeby NULL
#' @param nrisk_table_textsize 4
#' @param nrisk_position_scaler 0.2
#' @param nrisk_position_dodge 0.2, negative values will reverse the order
#' @param nrisk_offset 0
#' @param nrisk_filterout0 FALSE
#' @param km_logrank_pvalue FALSE
#' @param km_logrank_pvalue_pos "left" or "right"
#' @param km_logrank_pvalue_textsize pvalue text size default to 5
#' @param km_trans one of "identity","event","cumhaz","cloglog"
#' @param km_ticks TRUE
#' @param km_band TRUE
#' @param km_conf_int 0.95
#' @param km_conf_type default one of "log", "plain", "log-log", "logit", "none"
#' @param km_conf_lower one of "usual", "peto", "modified"
#' @param km_median add median survival information one of "none", "median", "medianci", "table"
#' @param km_median_table_pos when table is chosen where to put it  "left" or "right
#' @param km_median_table_order when table is chosen the order of the entries "default" or "reverse"
#' @param km_yaxis_position where to put y axis on "left" or "right
#' @param facet_formula facet formula to be used otherwise ~ groupvar1 + groupvar2 + groupvar3
#' @param facet_ncol NULL if not specified the automatic waiver will be used
#' @param facet_strip_position position in sequence for the variable used in faceting default to c("top","top","top","top")
#' @param theme_certara apply certara colors and format for strips and default colour/fill
#' @param return_list What to return if True a list of the datasets and plot is returned instead of only the plot
#' @examples
#' library(tidyr)
#' # Example 1
#'lung_long <-  survival::lung |>
#'  dplyr::mutate(status = ifelse(status==1,0,1)) |>
#'  tidyr::gather(Endpoint,DV,status) |>
#'  dplyr::filter(!is.na(ph.karno))|>
#'  dplyr::filter(!is.na(pat.karno))|>
#'  dplyr::filter(!is.na(ph.ecog))
#' lung_long$ph.ecog <- ifelse(lung_long$ph.ecog>1,2,lung_long$ph.ecog)
#' lung_long$ph.ecog <- as.factor(lung_long$ph.ecog )
#' lung_long$ph.ecog <- as.factor(lung_long$ph.ecog )
#' lung_long$facetdum <- "(all)"
#' 
#' ggkmrisktable(data = lung_long, time= "time", status ="DV",
#'              exposure_metrics =c("age","ph.karno"),
#'              exposure_metric_split = "tertile",
#'              color_fill = "exptile",
#'              linetype = "expname",
#'              groupvar1 = "Endpoint",
#'              groupvar2 = "exptile",
#'              xlab = "Time of follow_up",
#'              ylab ="Overall survival probability",
#'              nrisk_table_variables = c("n.risk","n.event"),
#'              km_median = "medianci",
#'              km_band = FALSE,
#'              nrisk_table_breaktimeby = 200,
#'              facet_ncol = 3)
#' #Example 2
#' ggkmrisktable(data = lung_long, time= "time", status ="DV",
#'              exposure_metrics =c("age","ph.karno"),
#'              exposure_metric_split = "quartile",
#'              color_fill = "exptile",
#'              linetype = "none",
#'              groupvar1 = "Endpoint",
#'              groupvar2 = "exptile",
#'              xlab = "Time of follow_up",
#'              ylab ="Overall survival probability",
#'              nrisk_table_variables = c("cum.n.event","pct.risk","n.censor"),
#'              km_median = "medianci",
#'              km_band = TRUE,
#'              km_trans = "event",
#'              show_exptile_values = TRUE,
#'              show_exptile_values_pos = "right",
#'              nrisk_table_breaktimeby = 200,
#'              facet_ncol = 3,
#'              facet_formula = ~expname)
#'\dontrun{
#' #Example 3
#' ggkmrisktable(data = lung_long, time = "time", status = "DV",
#'              exposure_metrics =c("ph.karno","pat.karno"),
#'              exposure_metric_split = "median",
#'              color_fill = "exptile",
#'              linetype = "exptile",
#'              groupvar1 = "Endpoint",
#'              groupvar2 = "expname",
#'              xlab = "Time of follow_up",
#'              ylab ="Overall survival probability",
#'              nrisk_table_variables = c("n.event"),
#'              km_trans = "event",
#'              km_median = "table",
#'              km_median_table_pos = "right",
#'              km_logrank_pvalue = TRUE,
#'              km_band = TRUE,
#'              nrisk_table_breaktimeby = 200,
#'              facet_ncol = 3,
#'              facet_formula = ~expname)
#' #Example 4
#'ggkmrisktable(data=lung_long,
#'              exposure_metrics = c("ph.karno","age"),
#'              exposure_metric_split = "median",
#'              time = "time",
#'              status ="DV",
#'              color_fill = "ph.ecog",
#'              linetype = "ph.ecog",
#'              groupvar1 = "Endpoint",
#'              groupvar2 = "expname",
#'              groupvar3 = "exptile",
#'              nrisk_filterout0 = FALSE,
#'              nrisk_table_breaktimeby = 200,
#'              km_logrank_pvalue = TRUE,
#'              km_median = "table",
#'              km_median_table_pos = "left",
#'              facet_formula = ~expname+exptile)
#' #Example 5
#' 
#' ggkmrisktable(data=lung_long,
#'              exposure_metrics = c("ph.karno","age"),
#'              exposure_metric_split = "none",
#'               color_fill = "none",
#'              linetype = "none",
#'              nrisk_table_variables = c("n.risk", "pct.risk", "n.event", "cum.n.event", "n.censor"),
#'               km_median = "table",
#'               nrisk_position_scaler = 0.1
#'              )             
#'              
#'}
#' @export
ggkmrisktable <- function(data = lung_long, # long format filter to Endpoint of choice
                         time = "time"   , # long format filter to Endpoint of choice
                         status = "DV",
                         endpoint ="Endpoint",
                         groupvar1 = "Endpoint", 
                         groupvar2 ="expname", 
                         groupvar3 ="none",
                         exposure_metrics = c("age","ph.karno"),
                         exposure_metric_split = c("median","tertile","quartile","none"),
                         exposure_metric_soc_value = -99,
                         exposure_metric_plac_value = 0,
                         show_exptile_values = FALSE,
                         show_exptile_values_pos = c("left","right"),
                         show_exptile_values_textsize = 5,
                         show_exptile_values_order = c("default","reverse"),
                         color_fill = "exptile",
                         linetype = "exptile",
                         xlab = "Time of follow_up",
                         ylab ="Overall survival probability",
                         nrisk_table_plot = TRUE,
                         nrisk_table_variables = c("n.risk", "pct.risk", "n.event", "cum.n.event", "n.censor"), 
                         nrisk_table_breaktimeby = NULL,
                         nrisk_table_textsize = 4,
                         nrisk_position_scaler = 0.2,
                         nrisk_position_dodge = 0.2,
                         nrisk_offset = 0,
                         nrisk_filterout0 = FALSE,
                         km_logrank_pvalue = FALSE,
                         km_logrank_pvalue_pos = c("left","right"),
                         km_logrank_pvalue_textsize = 5,
                         km_trans = c("identity","event","cumhaz","cloglog"),
                         km_ticks = TRUE,
                         km_band  = TRUE,
                         km_conf_int = 0.95,
                         km_conf_type  = c("log" , "plain", "log" ,"log-log","logit","none"),
                         km_conf_lower = c("usual","peto" , "modified"),
                         km_median = c("none","median","medianci","table"),
                         km_median_table_pos = c("left","right"),
                         km_median_table_order = c("default","reverse"),
                         km_yaxis_position = c("left","right"),
                         facet_formula = NULL,
                         facet_ncol = NULL,
                         facet_strip_position = c("top","top","top","top"),
                         theme_certara = TRUE,
                         return_list = FALSE
) {
  none = NULL
  timevar          <- time
  statusvar        <- status
  endpointinputvar <- endpoint
  groupvar1inputvar <- groupvar1
  groupvar2inputvar <- groupvar2
  groupvar3inputvar <- groupvar3
  colorinputvar    <-  if (color_fill !="none") color_fill else NULL
  fillinputvar     <-  if (color_fill !="none") color_fill else NULL
  linetypeinputvar <-  if (linetype   !="none") linetype   else NULL
  survformula      <-  paste( "Surv","(",timevar,",",statusvar,")",sep="")
  
  exposure_metric_split <- match.arg(exposure_metric_split, several.ok = FALSE)
  nrisk_table_variables  <- match.arg(nrisk_table_variables, several.ok = TRUE)
  km_logrank_pvalue_pos <- match.arg(km_logrank_pvalue_pos, several.ok = FALSE)
  
  km_trans <- match.arg(km_trans, several.ok = FALSE)
  km_conf_type <- match.arg(km_conf_type, several.ok = FALSE)
  km_conf_lower <- match.arg(km_conf_lower, several.ok = FALSE)
  km_median <- match.arg(km_median, several.ok = FALSE)
  km_median_table_pos <- match.arg(km_median_table_pos, several.ok = FALSE)
  km_median_table_order <- match.arg(km_median_table_order, several.ok = FALSE)
  km_yaxis_position <- match.arg(km_yaxis_position, several.ok = FALSE)
  show_exptile_values_pos <- match.arg(show_exptile_values_pos, several.ok = FALSE)
  show_exptile_values_order <- match.arg(show_exptile_values_order, several.ok = FALSE)
  
  pval.txt = expname = expvalue = x1lower = x1upper = x1 = y2 = keynumeric = key = n.risk = value = loopvariable = NULL
  exprange = exptile = exptile2 = NULL
  facetvars <- unique(c(groupvar1inputvar,groupvar2inputvar,groupvar3inputvar))
  facetvars <- c(groupvar1inputvar,groupvar2inputvar,groupvar3inputvar) [
    c(groupvar1inputvar,groupvar2inputvar,groupvar3inputvar)!= "."]
  facetvars <- c(groupvar1inputvar,groupvar2inputvar,groupvar3inputvar) [
    c(groupvar1inputvar,groupvar2inputvar,groupvar3inputvar)!= "none"]
  
  
  facet_formula <- if (is.null(facet_formula) ) stats::as.formula( paste("~",paste(facetvars, collapse=" + "))) else
      stats::as.formula(facet_formula)

  exposure_metric_split <- match.arg(exposure_metric_split)
 

  if(all(exposure_metrics%in% c("none",""))) {
    data <- data |> 
      dplyr::mutate(none = "(all)")  # needed when no metric are chosen
    exposure_metric_split <- "none"
    colorinputvar    <-  if (color_fill !="none") color_fill else NULL
    fillinputvar     <-  if (color_fill !="none") color_fill else NULL
    linetypeinputvar <-  if (linetype   !="none") linetype   else NULL
    
    
    data.long <- data |>
      tidyr::gather(expname,expvalue,none) |> 
      dplyr::group_by(expname,!!endpoint)    
  }
  
  if(!all(exposure_metrics%in% c("none",""))) {
    data.long <- data |> 
      tidyr::gather(expname,expvalue,!!!exposure_metrics) |> 
      dplyr::group_by(expname,!!endpoint)
  }

  if(exposure_metric_split=="none") {
    data.long <- data.long |> 
      dplyr::mutate(exptile = dplyr::case_when(
    expvalue == exposure_metric_soc_value  ~ NA,
    expvalue == exposure_metric_plac_value ~ NA,
    expvalue  > exposure_metric_plac_value ~ expvalue))
  }
   if(exposure_metric_split=="quartile") {
     data.long <- data.long |> 
       dplyr::mutate(
        Q25 = stats::quantile(expvalue[!expvalue %in% c(exposure_metric_soc_value,
                                                 exposure_metric_plac_value)], 0.25, na.rm=TRUE),
        Q50 = stats::quantile(expvalue[!expvalue %in% c(exposure_metric_soc_value,
                                                 exposure_metric_plac_value)], 0.50, na.rm=TRUE), 
        Q75 = stats::quantile(expvalue[!expvalue %in% c(exposure_metric_soc_value,
                                                 exposure_metric_plac_value)], 0.75, na.rm=TRUE)) |> 
       dplyr::mutate(exptile = dplyr::case_when(
         expvalue == exposure_metric_soc_value  ~ "SOC",
         expvalue == exposure_metric_plac_value ~ "Placebo",
         expvalue  > exposure_metric_plac_value &
                           expvalue <= Q25      ~ "Q1",
         expvalue > Q25  & expvalue <= Q50      ~ "Q2",
         expvalue > Q50  & expvalue <= Q75      ~ "Q3",
         expvalue > Q75                         ~ "Q4"))
   }
  if(exposure_metric_split=="tertile") {
    data.long <- data.long |> 
      dplyr::mutate(
        Q33 = stats::quantile(expvalue[!expvalue %in% c(exposure_metric_soc_value,
                                                 exposure_metric_plac_value)], 1/3, na.rm=TRUE),
        Q66 = stats::quantile(expvalue[!expvalue %in% c(exposure_metric_soc_value,
                                                 exposure_metric_plac_value)], 2/3, na.rm=TRUE)) |> 
      dplyr::mutate(exptile = dplyr::case_when(
        expvalue == exposure_metric_soc_value  ~ "SOC",
        expvalue == exposure_metric_plac_value ~" Placebo",
        expvalue  > exposure_metric_plac_value &
                          expvalue <= Q33      ~ "T1",
        expvalue > Q33  & expvalue <= Q66      ~ "T2",
        expvalue > Q66                         ~ "T3"))
  }
  if(exposure_metric_split=="median") {
    data.long <- data.long |> 
      dplyr::mutate(
        Q50 = stats::quantile(expvalue[!expvalue %in% c(exposure_metric_soc_value,
                                                 exposure_metric_plac_value)], 0.5, na.rm=TRUE)) |> 
      dplyr::mutate(exptile = dplyr::case_when(
        expvalue == exposure_metric_soc_value  ~"SOC",
        expvalue == exposure_metric_plac_value ~"Placebo",
        expvalue > 0   &  expvalue <= Q50      ~ "M1",
        expvalue > Q50                         ~ "M2"))
  }
  data.long$exptile2 <- data.long$exptile
  
  listvars <- unique(c(endpointinputvar,colorinputvar,fillinputvar,linetypeinputvar))
  listvars <- listvars[!is.element(listvars,c("none",".")) ]
  listvars <- c(listvars,"expname","exptile","exptile2")
  listvars <- listvars[!duplicated(listvars) ]
  

  if(!all(exposure_metrics%in% c("none",""))) {
    data.long.quantiles <- data.long |>
      dplyr::group_by(!!!syms(listvars))|>
      #dplyr::group_by(Endpoint,exptile,expname,exptile2)|>
      dplyr::summarize(exprange = paste0("(",round(min(expvalue),2),"-",round(max(expvalue),2),"]"))
    
  }
  if(all(exposure_metrics%in% c("none",""))) {
    data.long.quantiles <- data.long |>
      dplyr::group_by(!!!syms(listvars))|>
      dplyr::summarize(exprange = "none")

  }
  #print(data.long.quantiles)
  #sprintf("%#.3g (%#.3g-%#.3g]",min(expvalue),max(expvalue))
  
  #we generate a curve by the combination of all these inputs removing duplicates and none
  listvars <- unique(c(endpointinputvar,colorinputvar,fillinputvar,linetypeinputvar,
                       groupvar1inputvar,
                       groupvar2inputvar,groupvar3inputvar))
  listvars <- listvars[!is.element(listvars,c("none",".")) ]
  listvars <- listvars[!duplicated(listvars) ]
  #if(exposure_metric_split == "none") listvars <- c(listvars,"expvalue")
  
  if ( length(listvars) ==0 ){
    f <- stats::as.formula(paste(survformula, "1", sep = " ~ "))
  }
  if ( length(listvars) >0 ){
    f <- stats::as.formula(paste(survformula, paste(listvars, collapse = " + "), sep = " ~ "))
  }
  
  surv_object <- eval(bquote( survival::survfit( .(f)  , data.long) ))
  
  if (is.null(nrisk_table_breaktimeby) ||
      nrisk_table_breaktimeby == ''    ||
      is.na(nrisk_table_breaktimeby)){
    ggsurv <- survminer::ggsurvplot(surv_object,
                                    data.long,risk.table = TRUE,
                                    ggtheme = ggplot2::theme_bw())
  } else {
    ggsurv <- survminer::ggsurvplot(surv_object,
                                    data.long,risk.table = TRUE,
                                    break.time.by = nrisk_table_breaktimeby,
                                    ggtheme = ggplot2::theme_bw())
  }
  if(km_logrank_pvalue){ #log rank does not group by color_fill, linetype exptile
    loopvariables <- unique(c(endpointinputvar,"expname",groupvar1inputvar,groupvar2inputvar,groupvar3inputvar))
    #loopvariables <- loopvariables[!loopvariables%in% "exptile"]
    loopvariables <- loopvariables[!loopvariables%in% "none"]
    listvars2 <- listvars[!listvars%in% loopvariables]
    if(exposure_metric_split == "none") listvars2 <- c(listvars2,"expvalue")
    if ( length(listvars2) ==0 ){
      f2 <- stats::as.formula(paste(survformula, "1", sep = " ~ "))
    }
    if ( length(listvars2) >0 ){
      f2 <- stats::as.formula(paste(survformula, paste(listvars2, collapse = " + "), sep = " ~ "))
    }
   
    survfit_by_endpoint <- list()
    logrank_test_by_endpoint <- list()
    loopvariables <- loopvariables[loopvariables!="none"]
    data.long <- tidyr::unite(data.long,"loopvariable", !!!loopvariables, remove = FALSE)
    
    for (i in unique(data.long[,"loopvariable"]) |>
         dplyr::pull() |>
         as.character() ) {
      survregdata<- data.long |>
        dplyr::filter(.data[["loopvariable"]] ==i)
      survfit_by_endpoint_fit <- eval(bquote( survival::survfit( .(f2)  , survregdata) ))
      survfit_by_endpoint[[i]] <- survfit_by_endpoint_fit
      logrank_test_by_endpoint_fit <- survminer::surv_pvalue(survfit_by_endpoint_fit, method = "1",
                                                             data=survregdata)
      logrank_test_by_endpoint_fit[,"loopvariable"] <- i
      logrank_test_by_endpoint[[i]] <- logrank_test_by_endpoint_fit
    }
    
    logrank_test_by_endpoint <- data.table::rbindlist(logrank_test_by_endpoint)
    logrank_test_by_endpoint <- logrank_test_by_endpoint |> 
      tidyr::separate(loopvariable, into = loopvariables,
               sep="_",extra = "merge"
      )
  }
  risktabledata <- ggsurv$table$data
  if(!is.null(surv_object$strata)){
    variables <- .get_variables(risktabledata$strata, surv_object, data.long)
    for(variable in variables) {
      risktabledata[[variable]] <- .get_variable_value(variable,risktabledata$strata, surv_object, data.long)
    }
  }
  if(nrisk_filterout0){
    risktabledata <- risktabledata |> 
      dplyr::filter(n.risk > 0)
  }
  if(!is.null(nrisk_table_variables) && (length(as.vector(nrisk_table_variables)) > 0) &&
     all(nrisk_table_variables != "")){
    risktabledatag<- tidyr::gather(risktabledata,key,value, !!!nrisk_table_variables , factor_key = TRUE)
    risktabledatag$keynumeric<- - nrisk_position_scaler* as.numeric(as.factor(risktabledatag$key)) + nrisk_offset
  }
  if(is.null(nrisk_table_variables) || all(nrisk_table_variables == "") ) {
    risktabledatag<- tidyr::gather(risktabledata,key,value, n.risk, factor_key = TRUE)
    risktabledatag$keynumeric<- - nrisk_position_scaler*as.numeric(as.factor(risktabledatag$key)) + nrisk_offset
  }
  
  if (km_median!="none"){
    if(!is.null(surv_object$strata) || is.matrix(surv_object$surv))  {
    .table <- as.data.frame(summary(surv_object)$table)
  } else {
    .table <- t(as.data.frame(summary(surv_object)$table))
    rownames(.table) <- "(all)"
  }
  surv_median <- as.vector(.table[,"median"])
  dfmedian <- data.frame(x1 = surv_median,
                         x2 = surv_median,
                         x1lower =  as.vector(.table[,"0.95LCL"]),
                         x1upper =  as.vector(.table[,"0.95UCL"]),
                         y1 = rep(0, length(surv_median)),
                         y2 = rep(0.5, length(surv_median)),
                         strata = .clean_strata(rownames(.table)))
  
  if(!is.null(surv_object$strata)){
    variables <- .get_variables(dfmedian$strata, surv_object, data.long)
    for(variable in variables) {
      dfmedian[[variable]] <- .get_variable_value(variable, dfmedian$strata, surv_object, data.long)
    }
  }
  }
  
  plotkm0 <-   ggplot2::ggplot(data.long,ggplot2::aes_string(time = time, status = status,
                                            color = colorinputvar, fill = fillinputvar,
                                            linetype = linetypeinputvar))+
    ggplot2::geom_line(stat = "km",trans = km_trans)
  if(km_band){
    plotkm00 <-  plotkm0 +
      ggplot2::geom_ribbon(stat = "kmband", alpha=0.2, color = "transparent",
                  conf.int   = km_conf_int,
                  conf.type  = km_conf_type,
                  conf.lower = km_conf_lower,
                  error = "greenwood",
                  trans = km_trans) 
  }
  if(!km_band){
    plotkm00 <-  plotkm0 
  }
  
  if(km_ticks){
    plotkm000 <-  plotkm00 +
      geom_kmticks(trans = km_trans)
  }
  if(!km_ticks){
    plotkm000 <-  plotkm00 
  }
  if(nrisk_table_plot) {
      plotkm1 <-  plotkm000 +
        ggplot2::geom_text(data=risktabledatag,
                           ggplot2::aes(x=time,label=value,y=keynumeric,time=NULL,status=NULL),
                show.legend = FALSE,
                size = nrisk_table_textsize, 
                position = ggstance::position_dodgev(height =nrisk_position_dodge)
      )+
        ggplot2::geom_hline(yintercept = - nrisk_position_scaler *(
        unique(c(seq(min(as.numeric(as.factor(risktabledatag$key))),max(as.numeric(as.factor(risktabledatag$key)))+1,1)))
      )+nrisk_position_scaler/2 + nrisk_offset
      )
    }

  if(!nrisk_table_plot) {
    plotkm1 <-  plotkm0 
  }
  
  
  if(km_median=="table"){
    km_median_table_pos_x <- ifelse(km_median_table_pos == "left",-Inf,Inf)
    km_median_table_pos_hjust <- ifelse(km_median_table_pos == "left",0,1)
     
    plotkm1m  <-  plotkm1 +
      ggplot2::geom_text(data = dfmedian |> 
                           dplyr::mutate(none = "(all)"), 
                         ggplot2::aes(x     = km_median_table_pos_x,
                                      y     = (max(as.numeric(as.factor(get(!!color_fill))))+1)*0.09,
                                      label = "Med. Surv. Time:"), 
                hjust = km_median_table_pos_hjust, show.legend = FALSE, 
                color="gray30",inherit.aes = FALSE)
    
    if(km_median_table_order == "reverse"){
      plotkm1mt  <-  plotkm1m +
        ggplot2::geom_text(data = dfmedian |>
                             dplyr::mutate(none = "(all)",
                                           "{timevar}" := NA,
                                           "{statusvar}" := NA), 
                           ggplot2::aes(    x = km_median_table_pos_x, y = 0.09*rev(as.numeric(as.factor(get(!!color_fill)))),
                                            label = paste0(get(!!color_fill), ": ",
                                                           sprintf("%#.3g (%#.3g, %#.3g)",x1,x1lower,x1upper)
                                            )), 
                           hjust = km_median_table_pos_hjust,
                           show.legend = FALSE,inherit.aes = TRUE)
    }
    
    if(km_median_table_order == "default"){
      plotkm1mt  <-  plotkm1m +
        ggplot2::geom_text(data=dfmedian |>
                             dplyr::mutate(none = "(all)",
                                           "{timevar}" := NA,
                                           "{statusvar}" := NA), 
                           ggplot2::aes(    x = km_median_table_pos_x, y = 0.09*(as.numeric(as.factor(get(!!color_fill)))),
                                            label = paste0(get(!!color_fill), ": ",
                                                           sprintf("%#.3g (%#.3g, %#.3g)",x1,x1lower,x1upper)
                                            )), 
                           hjust = km_median_table_pos_hjust,
                           show.legend = FALSE,inherit.aes = TRUE)
    }

  }     
  if(km_median=="medianci"){
    plotkm1mt  <-  plotkm1 +
      ggrepel::geom_label_repel(data = dfmedian, ggplot2::aes(x= x1 , y= y2 ,
                                                              label =sprintf("%#.3g (%#.3g, %#.3g)",x1,x1lower,x1upper),
                                            status=NULL,time=NULL),show.legend = FALSE,
                       label.size = NA, direction="both",fill="white",
                       segment.color="black",nudge_y = -0.1,segment.size = 0.5,
                       alpha = 0.5,label.padding=.1, force = 5,
                       na.rm=TRUE,
                       seed = 1234) +
      ggrepel::geom_label_repel(data = dfmedian, ggplot2::aes(x= x1 , y= y2 ,label =sprintf("%#.3g (%#.3g, %#.3g)",x1,x1lower,x1upper),
                                            status=NULL,time=NULL),show.legend = FALSE,
                       label.size = NA,direction="both",
                       nudge_y = -0.1,segment.size = 0.5,
                       arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc"), type = "closed", ends = "first"),
                       alpha = 1,label.padding=.1, force = 5,
                       na.rm=TRUE,
                       fill = NA,
                       seed = 1234)
  }
  if(km_median=="median"){
    plotkm1mt  <-  plotkm1 +
      ggrepel::geom_label_repel(data = dfmedian, ggplot2::aes(x= x1 , y= y2 ,label = sprintf("%#.3g",x1), status=NULL,time=NULL),show.legend = FALSE,
                         label.size = NA, direction="both",fill="white",
                         segment.color="black",nudge_y = -0.1,segment.size = 0.5,
                         alpha = 0.5,label.padding=.1, force = 5,
                         na.rm=TRUE,
                         seed = 1234) +
      ggrepel::geom_label_repel(data = dfmedian, ggplot2::aes(x= x1 , y= y2 ,label =sprintf("%#.3g",x1),
                                              status=NULL,time=NULL),show.legend = FALSE,
                         label.size = NA,direction="both",
                         nudge_y = -0.1, segment.size = 0.5,
                         arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc"), type = "closed", ends = "first"),
                         alpha = 1,label.padding=.1, force = 5,
                         na.rm=TRUE,
                         fill = NA,
                         seed = 1234)
  }
  if(km_median=="none"){
    plotkm1mt <- plotkm1
  }
 if (all(exposure_metrics%in% c("none","")) )  show_exptile_values <- FALSE
 if (all(exposure_metric_split%in% c("none")) )  show_exptile_values <- FALSE

  if(show_exptile_values){
    exptile_values_pos_x       <- ifelse(show_exptile_values_pos == "left",-Inf,Inf)
    exptile_values_pos_x_hjust <- ifelse(show_exptile_values_pos == "left",0,1)

    if(show_exptile_values_order=="reverse") {
      plotkm1mtexp <- plotkm1mt +
        ggplot2::geom_text(data = data.long.quantiles |>
                             dplyr::mutate(none = "(all)",
                                           "{timevar}" := NA,
                                           "{statusvar}" := NA), 
                           ggplot2::aes(x = exptile_values_pos_x,
                                        y = 0.09*rev(as.numeric(as.factor(exptile))),
                                        label = paste0(exptile, ": ",exprange
                                        )), 
                           hjust = exptile_values_pos_x_hjust,
                           size = show_exptile_values_textsize,
                           show.legend = FALSE,inherit.aes = TRUE)
    }
    if(show_exptile_values_order=="default") {
      plotkm1mtexp <- plotkm1mt +
        ggplot2::geom_text(data = data.long.quantiles |>
                             dplyr::mutate(none = "(all)",
                                           "{timevar}" := NA,
                                           "{statusvar}" := NA), 
                           ggplot2::aes(x = exptile_values_pos_x,
                                        y = 0.09*(as.numeric(as.factor(exptile))),
                                        label = paste0(exptile, ": ",exprange
                                        )), 
                           hjust = exptile_values_pos_x_hjust,
                           show.legend = FALSE,inherit.aes = TRUE)
    }
   
  }
  if(!show_exptile_values){
    plotkm1mtexp <- plotkm1mt
  }
  
  
  plotkm2 <- plotkm1mtexp +
    ggh4x::facet_nested_wrap(facet_formula,ncol= facet_ncol ,
                      strip = ggh4x::strip_split(position=facet_strip_position))+
    ggplot2::scale_y_continuous(position = km_yaxis_position,
                       breaks =c(unique(risktabledatag$keynumeric),
                                 c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) ), 
                       labels= c(as.vector(unique(risktabledatag$key)),
                                 c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1") ),
                       expand = ggplot2::expansion(mult=c(0.01,0.01),
                                          add =c(0, 0)))+
    ggplot2::scale_x_continuous( breaks =c(unique(risktabledatag$time))) +
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "top",strip.placement = "outside")+
    ggplot2::labs(color="",fill="",linetype="",
         x = xlab,y = ylab) 
  if(km_logrank_pvalue){
    
    km_logrank_pvalue_x <- ifelse(km_logrank_pvalue_pos == "left", -Inf,Inf)
    km_logrank_pvalue_x_hjust <- ifelse(km_logrank_pvalue_pos == "left",0,1)
    
    plotkm3 <-  plotkm2 +
      ggplot2::geom_text(data=logrank_test_by_endpoint,
                         ggplot2::aes(x = km_logrank_pvalue_x,
                                      y = Inf,label = pval.txt),
                         vjust = 1, hjust = km_logrank_pvalue_x_hjust, color = "gray30",
                         inherit.aes = FALSE,
                         size = km_logrank_pvalue_textsize)
    plotkm <- plotkm3
  } 
  if(!km_logrank_pvalue) {
    plotkm <- plotkm2
  }
  if(!theme_certara){
  pf <-  plotkm +
      ggplot2::scale_colour_manual( values = tableau10,drop=FALSE,na.value = "grey50")+
      ggplot2::scale_fill_manual(   values = tableau10,drop=FALSE,na.value = "grey50")
  }
  if(theme_certara){
    pf <-  plotkm +
      ggplot2::scale_colour_manual(values = c( "#4682AC","#FDBB2F","#EE3124" ,"#336343","#7059a6", "#803333"),
                                   drop=FALSE,na.value = "grey50")+
      ggplot2::scale_fill_manual(  values = c( "#4682AC","#FDBB2F","#EE3124" ,"#336343","#7059a6", "#803333"),
                                   drop=FALSE,na.value = "grey50")+
      ggplot2::theme(strip.background = ggplot2::element_rect(fill="#475c6b"),
                     strip.text =  ggplot2::element_text(face = "bold",color = "white"))
    
  }
  if(!return_list){
    pf }
  if(return_list){
    pf <- list(data.long,
               risktabledatag,
               dfmedian,
               data.long.quantiles,
               logrank_test_by_endpoint,
               - nrisk_position_scaler *(
                 unique(c(seq(min(as.numeric(as.factor(risktabledatag$key))),
                              max(as.numeric(as.factor(risktabledatag$key)))+1,1)))
               )+nrisk_position_scaler/2 + nrisk_offset,
               pf)
  }
  pf
}

