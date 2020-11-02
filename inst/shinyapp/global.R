
###########################
#### ARIDHIA ADDITIONS ####
# Sys.setenv(PGHOST = "10.0.0.4")
#.libPaths("../R/3.6.3")

# unloadNamespace("xaputils")
# unloadNamespace("dbplyr")
# unloadNamespace("dplyr")
# unloadNamespace("tibble")

# unloadNamespace("tidyselect")
# unloadNamespace("purrr")
# unloadNamespace("pillar")
#### ARIDHIA ADDITIONS ####
###########################

suppressMessages({
  library(zoo)
  library(shiny)
  library(shinyjs)
  library(colourpicker)
  library(ggplot2)
  library(ggpubr)
  library(scales)
  library(DT)
  library(tidyr)
  library(dplyr)
  library(Hmisc)
  library(quantreg)
  library(markdown)
  library(rlang)
  library(lazyeval)
  library(ggrepel)
  library(plotly)
  library(ggpmisc)
  library(ggquickeda)
  library(table1)
  library(survminer)
  library(ggstance)
  library(GGally)
})

###########################
#### ARIDHIA ADDITIONS ####
suppressMessages({
  library(shinyFiles)
  library(RPostgres)
})

DATABASE_CONN <- NULL
PGDATABASE <- Sys.getenv("PGDATABASE")
PGHOST <- Sys.getenv("PGHOST")
PORT <- Sys.getenv("PORT")
PGUSER <- Sys.getenv("PGUSER")
PGPASSWORD <- Sys.getenv("PGPASSWORD")
if (PGDATABASE != "" && PGHOST != "" && PORT != "" && PGUSER != "" && PGPASSWORD != "") {
  if (dbCanConnect(RPostgres::Postgres(),
    dbname = PGDATABASE,
    host = PGHOST,
    port = PORT,
    user = PGUSER,
    password = PGPASSWORD)
  ) {
    # get connection to database
    DATABASE_CONN <- dbConnect(RPostgres::Postgres(),
      dbname = PGDATABASE,
      host = PGHOST,
      port = PORT,
      user = PGUSER,
      password = PGPASSWORD)
  }
}

#### ARIDHIA ADDITIONS ####
###########################

#source("gradientInput.R") pending a shinyjqui fix

options(shiny.maxRequestSize = 250*1024^2)

stat_sum_df <- function(fun, geom = "point", ...) {
  stat_summary(fun.data = fun,  geom = geom,  ...)
}
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun = fun,  geom = geom,  ...)
}

median.n <- function(x){
  return(c(y = ifelse(median(x)<0,median(x),median(x)),
           label = round(median(x),2))) 
}
give.n <- function(x){
  return(c(y = min(x)*1,  label = length(x))) 
}

mean.n <- function(x){
  return(c(y = ifelse(mean(x)<0,mean(x),mean(x)),
           label = round(mean(x),2))) 
}


tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728","#9467BD",
               "#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

tableau20 <- c("#1F77B4","#AEC7E8", "#FF7F0E","#FFBB78"  ,"#2CA02C",
               "#98DF8A" ,"#D62728","#FF9896" ,"#9467BD","#C5B0D5" ,
               "#8C564B","#C49C94" ,"#E377C2","#F7B6D2" ,"#7F7F7F",
               "#C7C7C7" ,"#BCBD22","#DBDB8D" ,"#17BECF","#9EDAE5")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")


manual_scale <- function(aesthetic, values = NULL, ...) {
  if (rlang::is_missing(values)) {
    values <- NULL
  } else {
    force(values)
  }
  pal <- function(n) {
    if (n > length(values)) {
      stop("Insufficient values in manual scale. ", n, " needed but only ",
           length(values), " provided.", call. = FALSE)
    }
    values
  }
  discrete_scale(aesthetic, "manual", pal, ...)
}

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
  variables <- unique(as.vector(variables))
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
    index <- grep(paste0("^", variable, "$"), x)
    .trim(x[index+1])
  })
  res <- as.vector(res)
  var_levels <- levels(.get_data(fit, data)[, variable])
  if(!is.null(var_levels)) res <- factor(res, levels = var_levels)
  else res <- as.factor(res)
  res
}
.trim <- function(x){gsub("^\\s+|\\s+$", "", x)}
# from survminer


which0 <- function(x) {
  result <- which(x)
  if (length(result) == 0) {
    result <- 0
  }
  result
}

# All stats that can be displayed for continuous variables
allstats <- c("N",
              "Mean",
              "SD",
              "CV%",
              "Median",
              "q01",
              "q02.5",
              "q05",
              "q10",
              "q25",
              "q50",
              "q75",
              "q90",
              "q95",
              "q97.5",
              "q99",
              "Min",
              "Max",
              "IQR",
              "Q1","Q2","Q3","T1","T2",
              "Geo. Mean",
              "Geo. CV%",
              "Mean (SD)",
              "Mean (CV%)",
              "Mean (SD) (CV%)",
              "Median [Min, Max]","[Min, Max]",
              "Median [Q1, Q3]",
              "Median [IQR]",
              "Geo. Mean (Geo. CV%)")

inline_ui <- function(tag) {
  div(style = "display: inline-block", tag)
}

