suppressMessages ({
  library(shiny)
  library(shinyjs)
  library(colourpicker)
  library(ggplot2)
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
})

options(shiny.maxRequestSize=250*1024^2) 

stat_sum_df <- function(fun, geom="point", ...) {
  stat_summary(fun.data=fun,  geom=geom,  ...)
}
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun,  geom=geom,  ...)
}

median.n <- function(x){
  return(c(y = ifelse(median(x)<0,median(x),median(x)),
           label = round(median(x),2))) 
}
give.n <- function(x){
  return(c(y = min(x)*1,  label = length(x))) 
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

# eqcut <- function(x, ngroups, withhold=NULL, xlab=NULL, labeling=c("verbose", "Qn", "none"), interval=TRUE) {
#   labeling <- match.arg(labeling)
#   if (!is.null(withhold)) {
#     if (!is.list(withhold) || is.null(names(withhold)) || !(all(sapply(withhold, is.logical)))) {
#       stop("withhold must be a named list of logicals")
#     }
#     for (i in seq_len(length(withhold))) {
#       x[withhold[[i]]] <- NA
#     }
#   }
#   is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
#     abs(x - round(x)) < tol
#   }
#   if (!is.numeric(x)) stop("x must be numeric")
#   if (!is.numeric(ngroups)) stop("ngroups must be a single integer value")
#   if (length(ngroups) != 1) stop("ngroups must be a single integer value")
#   if (!is.wholenumber(ngroups)) stop("ngroups must be a single integer value")
#   if (ngroups < 2) stop("ngroups must be at least 2")
#   if (labeling == "none") interval <- TRUE  # Can't have no labels
#   q <- quantile(x, probs=seq.int(ngroups-1)/ngroups, na.rm=TRUE)
#   xcat <- cut(x, breaks=c(min(x, na.rm=T), q, max(x, na.rm=T)), right=F, include.lowest=T)
#   interv <- levels(xcat)
#   if (labeling == "verbose") {
#     if (ngroups == 2) {
#       if (!is.null(xlab)) {
#         lab <- paste0(" ", xlab)
#       } else {
#         lab <- ""
#       }
#       lab <- c("Below median", "Above median")
#       if (!is.null(xlab)) {
#         lab <- paste(lab, xlab)
#       }
#     } else {
#       th <- paste0(1:ngroups, c("st", "nd", "rd", rep("th", ngroups - 3)))
#       tile <- switch(as.character(ngroups), 
#                      "3" = "tertile",
#                      "4" = "quartile",
#                      "5" = "quintile",
#                      "6" = "sextile",
#                      "7" = "septile",
#                      "8" = "octile",
#                      "10" = "decile",
#                      "16" = "hexadecile",
#                      "100" = "percentile",
#                      paste0(ngroups, "-tile"))
#       lab <- paste(th, tile)
#       if (!is.null(xlab)) {
#         lab <- paste(lab, "of", xlab)
#       }
#     }
#   } else if (labeling == "Qn") {
#     lab <- paste0("Q", 1:ngroups)
#     if (!is.null(xlab)) {
#       lab <- paste(lab, "of", xlab)
#     }
#   } else {
#     lab <- ""
#   }
#   if (interval) {
#     if (labeling != "none") {
#       lab <- paste0(lab, ": ")
#     }
#     levels(xcat) <- paste0(lab, interv)
#   } else {
#     levels(xcat) <- lab
#   }
#   if (!is.null(withhold)) {
#     xcat <- factor(xcat, levels=c(levels(xcat), names(withhold)))
#     for (i in seq_len(length(withhold))) {
#       xcat[withhold[[i]]] <- names(withhold)[i]
#     }
#   }
#   xcat
# }
#sample_data <- read.csv("inst/shinyapp/data/sample_data.csv")
#x2<- unlist(sample_data["Weight"])
#withhold<- list(Placebo=(x2==0))
#table(eqcut(x2, 3, "AUC",withhold=list(Placebo=(x2==0)) )