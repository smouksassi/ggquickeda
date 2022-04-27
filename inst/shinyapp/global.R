suppressMessages({
  library(colourpicker)
  library(dplyr)
  library(DT)
  library(GGally)
  library(ggplot2)
  library(ggbeeswarm)
  library(ggpmisc)
  library(ggstance)
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
PGUSER <- Sys.getenv("PGUSER")
PGPASSWORD <- Sys.getenv("PGPASSWORD")

if (dbCanConnect(RPostgres::Postgres(),
                 dbname = PGDATABASE,
                 host = PGHOST,
                 user = PGUSER,
                 password = PGPASSWORD)
) {
  # get connection to database
  DATABASE_CONN <- dbConnect(RPostgres::Postgres(),
                             dbname = PGDATABASE,
                             host = PGHOST,
                             user = PGUSER,
                             password = PGPASSWORD)
}


#### ARIDHIA ADDITIONS ####
###########################
source("gradientInput.R") 

options(shiny.maxRequestSize=250*1024^2)  

stat_sum_df <- function(fun, geom="point", ...) {
  stat_summary(fun.data = fun,  geom=geom,  ...)
}
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun = fun,  geom=geom,  ...)
}


median.n <- function(x, nroundlabel = 2, labeltrans =c("none","exp") ){
  medianxvalue <-  median(x, na.rm = TRUE)
  if(labeltrans=="none")  medianxlabel <-  round(medianxvalue,nroundlabel)
  if(labeltrans=="exp")   medianxlabel <-  round(10^(medianxvalue),nroundlabel)
  return(c(y = medianxvalue, label = medianxlabel )) 
}

mean.n <- function(x, nroundlabel = 2, labeltrans =c("none","exp") ){
  meanxvalue <-  mean(x, na.rm = TRUE)
  if(labeltrans=="none")  meanxlabel <-  round(meanxvalue,nroundlabel)
  if(labeltrans=="exp")   meanxlabel <-  round(10^(meanxvalue),nroundlabel)
  return(c(y = meanxvalue, label = meanxlabel )) 
}

give.n <- function(x, nposition = c("min","max","below","up"),
                   mult = 1, add = 0
){
  if ( nposition == "below"){
    yposition <- -Inf
  } else if (nposition == "up")  {
    yposition <- Inf
  } else if (nposition == "min"){
    yposition <- min(x)*mult + add 
  } else 
    yposition <- max(x)*mult + add 
  return(c(y = yposition,  label = length(x))) 
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

draw_key_errorbar <- function (data, params, size) {
    data$linetype[is.na(data$linetype)] <- 0
    grid::segmentsGrob(c(0.2, 0.2, 0.5),
                       c(0.2, 0.8, 0.2),
                       c(0.8, 0.8, 0.5),
                       c(0.2, 0.8, 0.8),
                 gp = grid::gpar(col = alpha(data$colour,data$alpha),
                           lwd = data$size * ggplot2::.pt,
                           lty = data$linetype,
                           lineend = "butt"),
                           arrow = params$arrow)
}
draw_key_errorbarh <- function (data, params, size) {
  data$linetype[is.na(data$linetype)] <- 0
  grid::segmentsGrob(y0=c(0.2, 0.2, 0.5),
                     x0=c(0.2, 0.8, 0.2),
                     y1=c(0.8, 0.8, 0.5),
                     x1=c(0.2, 0.8, 0.8),
                     gp = grid::gpar(col = alpha(data$colour,data$alpha),
                                     lwd = data$size * ggplot2::.pt,
                                     lty = data$linetype,
                                     lineend = "butt"),
                     arrow = params$arrow)
}

draw_key_boxploth <- function (data, params, size) {
  grid::grobTree(grid::linesGrob(c(0.1, 0.25), 0.5),
                 grid::linesGrob(c(0.75,0.9), 0.5),
                 grid::rectGrob(height = 0.75, width = 0.5),
                 grid::linesGrob(0.5,c(0.125, 0.875)),
                 gp = grid::gpar(col = data$colour,
                     fill = alpha(data$fill, data$alpha),
                     lwd = data$size * ggplot2::.pt,
                     lty = data$linetype))
}

position_dodgev <- function(height = NULL, preserve = c("total", "single")) {
  ggproto(NULL, PositionDodgev,
          height = height,
          preserve = match.arg(preserve)
  )
}

PositionDodgev <- ggproto("PositionDodgev", Position,
                          height = NULL,
                          preserve = "total",
                          setup_params = function(self, data) {
                            if (is.null(data$ymin) && is.null(data$ymax) && is.null(self$height)) {
                              warning("Height not defined. Set with `position_dodge(height = ?)`",
                                      call. = FALSE)
                            }
                            
                            if (identical(self$preserve, "total")) {
                              n <- NULL
                            } else {
                              panels <- unname(split(data, data$PANEL))
                              ns <- vapply(panels, function(panel) max(table(panel$ymin)), double(1))
                              n <- max(ns)
                            }
                            
                            list(
                              height = self$height,
                              n = n
                            )
                          },
                          
                          setup_data = function(self, data, params) {
                            if (!"y" %in% names(data) && all(c("ymin", "ymax") %in% names(data))) {
                              data$y <- (data$ymin + data$ymax) / 2
                            }
                            data
                          },
                          
                          compute_panel = function(data, params, scales) {
                            collidev(
                              data,
                              params$height,
                              name = "position_dodgev",
                              strategy = pos_dodgev,
                              n = params$n,
                              check.height = FALSE
                            )
                          }
)


pos_dodgev <- function(df, height, n = NULL) {
  if (is.null(n)) {
    n <- length(unique(df$group))
  }
  
  if (n == 1)
    return(df)
  
  if (!all(c("ymin", "ymax") %in% names(df))) {
    df$ymin <- df$y
    df$ymax <- df$y
  }
  
  d_height <- max(df$ymax - df$ymin)
  
  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidy <- match(df$group, sort(unique(df$group)))
  
  # Find the center for each group, then use that to calculate ymin and ymax
  df$y <- df$y + height * ((groupidy - 0.5) / n - .5)
  df$ymin <- df$y - d_height / n / 2
  df$ymax <- df$y + d_height / n / 2
  
  df
}

position_dodge2v <- function(height = NULL, preserve = c("single", "total"),
                             padding = 0.1, reverse = TRUE) {
  ggproto(NULL, PositionDodge2v,
          height = height,
          preserve = match.arg(preserve),
          padding = padding,
          reverse = reverse
  )
}


PositionDodge2v <- ggproto("PositionDodge2v", PositionDodgev,
                           preserve = "total",
                           padding = 0.1,
                           reverse = TRUE,
                           
                           setup_params = function(self, data) {
                             if (is.null(data$ymin) && is.null(data$ymax) && is.null(self$height)) {
                               warning("Height not defined. Set with `position_dodge2v(height = ?)`",
                                       call. = FALSE)
                             }
                             
                             if (identical(self$preserve, "total")) {
                               n <- NULL
                             } else {
                               panels <- unname(split(data, data$PANEL))
                               if ("y" %in% names(data)) {
                                 # Point geom
                                 groups <- lapply(panels, function(panel) table(panel$y))
                               } else {
                                 # Interval geom
                                 groups <- lapply(panels, find_y_overlaps)
                               }
                               n_groups <- vapply(groups, max, double(1))
                               n <- max(n_groups)
                             }
                             
                             list(
                               height = self$height,
                               n = n,
                               padding = self$padding,
                               reverse = self$reverse
                             )
                           },
                           
                           compute_panel = function(data, params, scales) {
                             collide2v(
                               data,
                               params$height,
                               name = "position_dodge2v",
                               strategy = pos_dodge2v,
                               n = params$n,
                               padding = params$padding,
                               check.height = FALSE,
                               reverse = params$reverse
                             )
                           }
)

pos_dodge2v <- function(df, height, n = NULL, padding = 0.1) {
  if (!all(c("ymin", "ymax") %in% names(df))) {
    df$ymin <- df$y
    df$ymax <- df$y
  }
  
  # yid represents groups of boxes that share the same position
  df$yid <- find_y_overlaps(df)
  
  # based on yid find newy, i.e. the center of each group of overlapping
  # elements. for boxes, bars, etc. this should be the same as original y, but
  # for arbitrary rects it may not be
  newy <- (tapply(df$ymin, df$yid, min) + tapply(df$ymax, df$yid, max)) / 2
  df$newy <- newy[df$yid]
  
  if (is.null(n)) {
    # If n is null, preserve total widths of elements at each position by
    # dividing widths by the number of elements at that position
    n <- table(df$yid)
    df$new_height <- (df$ymax - df$ymin) / as.numeric(n[df$yid])
  } else {
    df$new_height <- (df$ymax - df$ymin) / n
  }
  
  # Find the total height of each group of elements
  group_sizes <- stats::aggregate(
    list(size = df$new_height),
    list(newy = df$newy),
    sum
  )
  
  # Starting ymin for each group of elements
  starts <- group_sizes$newy - (group_sizes$size / 2)
  
  # Set the elements in place
  for (i in seq_along(starts)) {
    divisions <- cumsum(c(starts[i], df[df$yid == i, "new_height"]))
    df[df$yid == i, "ymin"] <- divisions[-length(divisions)]
    df[df$yid == i, "ymax"] <- divisions[-1]
  }
  
  # y values get moved to between ymin and ymax
  df$y <- (df$ymin + df$ymax) / 2
  
  # If no elements occupy the same position, there is no need to add padding
  if (!any(duplicated(df$yid))) {
    return(df)
  }
  
  # Shrink elements to add space between them
  df$pad_height <- df$new_height * (1 - padding)
  df$ymin <- df$y - (df$pad_height / 2)
  df$ymax <- df$y + (df$pad_height / 2)
  
  df$yid <- NULL
  df$newy <- NULL
  df$new_height <- NULL
  df$pad_height <- NULL
  
  df
}

find_y_overlaps <- function(df) {
  overlaps <- numeric(nrow(df))
  overlaps[1] <- counter <- 1
  
  for (i in seq_asc(2, nrow(df))) {
    if (df$ymin[i] >= df$ymax[i - 1]) {
      counter <- counter + 1
    }
    overlaps[i] <- counter
  }
  overlaps
}

seq_asc <- function(to, from) {
  if (to > from) {
    integer()
  } else {
    to:from
  }
}

