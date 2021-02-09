suppressMessages({
  library(colourpicker)
  library(dplyr)
  library(DT)
  library(GGally)
  library(ggplot2)
  library(ggpmisc)
  library(ggpubr)
  library(ggrepel)
  library(ggquickeda)
  library(Hmisc)
  library(markdown)
  library(plotly)
  library(quantreg)
  library(rlang)
  library(shiny)
  library(shinyjs)
  library(survminer)
  library(table1)
  library(tidyr)
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

options(shiny.maxRequestSize=250*1024^2) 

stat_sum_df <- function(fun, geom="point", ...) {
  stat_summary(fun.data = fun,  geom=geom,  ...)
}
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun = fun,  geom=geom,  ...)
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

label_wrap <- function(width) {
  force(width)
  function(x) {
    unlist(lapply(strwrap(x, width = width, simplify = FALSE), 
                  paste0, collapse = "\n"))
  }
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
              "N Missing",
              "Mean",
              "SD",
              "CV%",
              "Sum",
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

translate_shape_string <- function(shape_string) {
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }
  pch_table <- c(
    "square open"           = 0,
    "circle open"           = 1,
    "triangle open"         = 2,
    "plus"                  = 3,
    "cross"                 = 4,
    "diamond open"          = 5,
    "triangle down open"    = 6,
    "square cross"          = 7,
    "asterisk"              = 8,
    "diamond plus"          = 9,
    "circle plus"           = 10,
    "star"                  = 11,
    "square plus"           = 12,
    "circle cross"          = 13,
    "square triangle"       = 14,
    "triangle square"       = 14,
    "square"                = 15,
    "circle small"          = 16,
    "triangle"              = 17,
    "diamond"               = 18,
    "circle"                = 19,
    "bullet"                = 20,
    "circle filled"         = 21,
    "square filled"         = 22,
    "diamond filled"        = 23,
    "triangle filled"       = 24,
    "triangle down filled"  = 25,
    "blank"                 = NA
  )
  
  shape_match <- charmatch(shape_string, names(pch_table))
  
  invalid_strings <- is.na(shape_match)
  nonunique_strings <- shape_match == 0
  
  if (any(invalid_strings)) {
    bad_string <- unique(shape_string[invalid_strings])
    n_bad <- length(bad_string)
    
    collapsed_names <- sprintf("\n* '%s'", bad_string[1:min(5, n_bad)])
    
    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    } else {
      ""
    }
    
    abort(glue("Can't find shape name:", collapsed_names, more_problems))
  }
  
  if (any(nonunique_strings)) {
    bad_string <- unique(shape_string[nonunique_strings])
    n_bad <- length(bad_string)
    
    n_matches <- vapply(
      bad_string[1:min(5, n_bad)],
      function(shape_string) sum(grepl(paste0("^", shape_string), names(pch_table))),
      integer(1)
    )
    
    collapsed_names <- sprintf(
      "\n* '%s' partially matches %d shape names",
      bad_string[1:min(5, n_bad)], n_matches
    )
    
    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    } else {
      ""
    }
    
    abort(glue("Shape names must be unambiguous:", collapsed_names, more_problems))
  }
  
  unname(pch_table[shape_match])
}

my.render.cat <- function (x, ..., na.is.category = FALSE) 
{
  c("", sapply(stats.apply.rounding(stats.default(x, ...), 
                                    ...), function(y) with(y, sprintf("%s (%s%%)", FREQ, 
                                   if (na.is.category) PCT else PCTnoNA))))
}

