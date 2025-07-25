% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggcontinuousexpdist.R
\name{ggcontinuousexpdist}
\alias{ggcontinuousexpdist}
\title{Create a continuous fit vs exposure(s) plot}
\usage{
ggcontinuousexpdist(
  data = effICGI,
  response = "response",
  endpoint = "Endpoint",
  model_type = c("linear", "loess", "none"),
  DOSE = "DOSE",
  color_fill = "DOSE",
  fit_by_color_fill = FALSE,
  exposure_metrics = c("AUC", "CMAX"),
  exposure_metric_split = c("median", "tertile", "quartile", "none"),
  exposure_metric_soc_value = -99,
  exposure_metric_plac_value = 0,
  exposure_distribution = c("distributions", "lineranges", "boxplots", "none"),
  exposure_distribution_percent = TRUE,
  exposure_distribution_percent_text_size = 5,
  dose_plac_value = "Placebo",
  xlab = "Exposure Values",
  ylab = "Response",
  points_alpha = 0.2,
  points_show = TRUE,
  mean_obs_byexptile = TRUE,
  mean_obs_byexptile_plac = TRUE,
  mean_obs_byexptile_group = "none",
  mean_text_size = 5,
  mean_obs_bydose = TRUE,
  mean_obs_bydose_plac = FALSE,
  N_text_show = TRUE,
  N_text_size = 5,
  N_text_ypos = c("with means", "top"),
  N_text_sep = "\\n",
  binlimits_show = TRUE,
  binlimits_text_size = 5,
  binlimits_ypos = -Inf,
  binlimits_color = "#B3B3B380",
  dist_position_scaler = 0.2,
  dist_offset = 0,
  dist_scale = 0.9,
  lineranges_ypos = -1,
  lineranges_dodge = 1,
  lineranges_doselabel = FALSE,
  proj_bydose = TRUE,
  yproj = TRUE,
  yproj_xpos = 0,
  yproj_dodge = 0.2,
  yaxis_position = c("left", "right"),
  facet_formula = NULL,
  theme_certara = TRUE,
  return_list = FALSE
)
}
\arguments{
\item{data}{Data to use with multiple endpoints stacked into response (values), Endpoint(endpoint name)}

\item{response}{name of the column holding the response values}

\item{endpoint}{name of the column holding the name/key of the endpoint default to \code{Endpoint}}

\item{model_type}{type of the trend fit one of "linear", "loess", "none"}

\item{DOSE}{name of the column holding the DOSE values default to \code{DOSE}}

\item{color_fill}{name of the column to be used for color/fill default to DOSE column}

\item{fit_by_color_fill}{fit split by color? default \code{FALSE}}

\item{exposure_metrics}{name(s) of the column(s) to be stacked into \code{expname} \code{exptile} and split into \code{exposure_metric_split}}

\item{exposure_metric_split}{one of "median", "tertile", "quartile", "none"}

\item{exposure_metric_soc_value}{special exposure code for standard of care default -99}

\item{exposure_metric_plac_value}{special exposure code for placebo default 0}

\item{exposure_distribution}{one of distributions, lineranges, boxplots or none}

\item{exposure_distribution_percent}{show percent of distribution between binlimits \code{TRUE}/\code{FALSE}}

\item{exposure_distribution_percent_text_size}{distribution percentages text size default to 5}

\item{dose_plac_value}{string identifying placebo in DOSE column}

\item{xlab}{text to be used as x axis label}

\item{ylab}{text to be used as y axis label}

\item{points_alpha}{alpha transparency for points}

\item{points_show}{show the observations \code{TRUE}/\code{FALSE}}

\item{mean_obs_byexptile}{observed mean by exptile \code{TRUE}/\code{FALSE}}

\item{mean_obs_byexptile_plac}{observed mean by exptile placebo \code{TRUE}/\code{FALSE}}

\item{mean_obs_byexptile_group}{additional grouping for exptile probabilities default \code{none}}

\item{mean_text_size}{mean text size default to 5}

\item{mean_obs_bydose}{observed mean by dose \code{TRUE}/\code{FALSE}}

\item{mean_obs_bydose_plac}{observed mean by placebo dose \code{TRUE}/\code{FALSE}}

\item{N_text_show}{show the N by exptile \code{TRUE}/\code{FALSE}}

\item{N_text_size}{N by exposure text size default to 5}

\item{N_text_ypos}{y position for N two text elements the first for by exptile and the second
for by dose/color options include \verb{with means} \code{top} \code{bottom}}

\item{N_text_sep}{character string to separat N from mean default \verb{\\n}}

\item{binlimits_show}{show the binlimits vertical lines \code{TRUE}/\code{FALSE}}

\item{binlimits_text_size}{binlimits text size default to 5}

\item{binlimits_ypos}{binlimits y position default to -Inf}

\item{binlimits_color}{binlimits text color default to alpha("gray70",0.5)}

\item{dist_position_scaler}{space occupied by the distribution default to 0.2}

\item{dist_offset}{offset where the distribution position starts default to 0}

\item{dist_scale}{scaling parameter for ggridges default to 0.9}

\item{lineranges_ypos}{where to put the lineranges -1}

\item{lineranges_dodge}{lineranges vertical dodge value 1}

\item{lineranges_doselabel}{\code{TRUE}/\code{FALSE}}

\item{proj_bydose}{project the probabilities on logistic curve \code{TRUE}/\code{FALSE}}

\item{yproj}{project the probabilities on y axis \code{TRUE}/\code{FALSE}}

\item{yproj_xpos}{y projection x position 0}

\item{yproj_dodge}{y projection dodge value 0.2}

\item{yaxis_position}{where to put y axis "left" or "right"}

\item{facet_formula}{facet formula to be use otherwise \code{endpoint ~ expname}}

\item{theme_certara}{apply certara colors and format for strips and default colour/fill}

\item{return_list}{What to return if True a list of the datasets and plot is returned instead of only the plot}
}
\description{
Produces a logistic fit plot with a facettable exposures/quantiles/distributions in ggplot2
}
\examples{
# Example 1
library(ggplot2)
library(patchwork)
effICGI <- logistic_data |>
dplyr::filter(!is.na(ICGI7))|>
dplyr::filter(!is.na(AUC))
effICGI$DOSE <- factor(effICGI$DOSE,
                      levels=c("0", "600", "1200","1800","2400"),
                      labels=c("Placebo", "600 mg", "1200 mg","1800 mg","2400 mg"))
effICGI$STUDY <- factor(effICGI$STUDY)    
effICGI <- tidyr::gather(effICGI,Endpoint,response,ICGI7,BRLS)
a <- ggcontinuousexpdist(data = effICGI |> dplyr::filter(Endpoint =="ICGI7"),
                 response = "response",
                 endpoint = "Endpoint",
                 exposure_metrics = c("AUC"),
                 exposure_metric_split = c("quartile"),
                 exposure_metric_soc_value = -99,
                 exposure_metric_plac_value = 0,
                 dist_position_scaler = 1, dist_offset = -1 ,
                 yproj_xpos =  -20 ,
                 yproj_dodge = 20 ,
                 exposure_distribution ="distributions",
                 mean_obs_bydose_plac = TRUE,
                 mean_obs_byexptile_plac=FALSE,
                 return_list = FALSE
                 )

b <- ggcontinuousexpdist(data = effICGI |> dplyr::filter(Endpoint =="BRLS"),
                 response = "response",
                 endpoint = "Endpoint",
                 exposure_metrics = c("AUC"),
                 exposure_metric_split = c("quartile"),
                 exposure_metric_soc_value = -99,
                 exposure_metric_plac_value = 0,
                 dist_position_scaler = 4.2, dist_offset = 5 ,
                 yproj_xpos =  -20 ,
                 yproj_dodge = 20 ,
                 exposure_distribution ="distributions",
                 mean_obs_bydose_plac = TRUE,
                 mean_obs_byexptile_plac=FALSE,
                 return_list = FALSE)            
(a / b)  +
plot_layout(guides = "collect") &
 theme(legend.position = "top")

#Example 2 loess fit
effICGI$SEX <- as.factor(effICGI$SEX)
ggcontinuousexpdist(data = effICGI |>
  dplyr::filter(Endpoint =="ICGI7"),
                 model_type = "loess",
                 response = "response",
                 endpoint = "Endpoint",
                 color_fill = "SEX",
                 exposure_metrics = c("AUC"),
                 exposure_metric_split = c("quartile"),
                 exposure_metric_soc_value = -99,
                 exposure_metric_plac_value = 0,
                 dist_position_scaler = 1, dist_offset = -1 ,
                 yproj_xpos =  -20 ,
                 yproj_dodge = 20 ,
                 exposure_distribution ="lineranges",
                 lineranges_ypos = -0.2,
                 lineranges_dodge = 0.2,
                 lineranges_doselabel = TRUE)
\dontrun{
#Example 3
library(ggplot2)
 library(patchwork)
  effICGI <- logistic_data |>
  dplyr::filter(!is.na(ICGI7))|>
  dplyr::filter(!is.na(AUC))
  effICGI$DOSE <- factor(effICGI$DOSE,
                       levels=c("0", "600", "1200","1800","2400"),
                       labels=c("Placebo", "600 mg", "1200 mg","1800 mg","2400 mg"))
effICGI$STUDY <- factor(effICGI$STUDY)
effICGI <- tidyr::gather(effICGI,Endpoint,response,PRLS,BRLS)
effICGI$Endpoint2 <- effICGI$Endpoint
ggcontinuousexpdist(data = effICGI ,
                    model_type = "loess",
                    response = "response",
                    endpoint = "Endpoint",
                    color_fill = "Endpoint2",
                    fit_by_color_fill = TRUE,
                    exposure_metrics = c("AUC"),
                    exposure_metric_split = c("quartile"),
                    exposure_metric_soc_value = -99,
                    exposure_metric_plac_value = 0,
                    yproj = FALSE, points_show = FALSE,
                    exposure_distribution ="none",
                    N_text_sep = " | N = ",
                    N_text_ypos = c("bottom","with means"),
                    binlimits_color = "red",
                    binlimits_ypos = 10,
                    facet_formula = ~expname)
}
}
