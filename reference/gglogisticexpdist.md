# Create a logistic fit vs exposure(s)plot

Produces a logistic fit plot with a facettable
exposures/quantiles/distributions in ggplot2

## Usage

``` r
gglogisticexpdist(
  data = effICGI,
  response = "response",
  endpoint = "Endpoint",
  model_type = c("logistic"),
  DOSE = "DOSE",
  color_fill = "DOSE",
  logistic_by_color_fill = FALSE,
  exposure_metrics = c("AUC", "CMAX"),
  exposure_metric_split = c("median", "tertile", "quartile", "none"),
  exposure_metric_soc_value = -99,
  exposure_metric_plac_value = 0,
  exposure_distribution = c("distributions", "lineranges", "boxplots", "none"),
  exposure_distribution_percent = TRUE,
  exposure_distribution_percent_text_size = 5,
  dose_plac_value = "Placebo",
  xlab = "Exposure Values",
  ylab = "Probability of Response",
  points_alpha = 0.2,
  points_show = TRUE,
  prob_obs_byexptile = TRUE,
  prob_obs_byexptile_plac = TRUE,
  prob_obs_byexptile_group = "none",
  prob_text_size = 5,
  prob_obs_bydose = TRUE,
  prob_obs_bydose_plac = FALSE,
  Nresp_Ntot_show = TRUE,
  Nresp_Ntot_size = 5,
  Nresp_Ntot_ypos = c("with percentages", "top"),
  Nresp_Ntot_sep = "/",
  binlimits_show = TRUE,
  binlimits_text_size = 5,
  binlimits_ypos = 0,
  binlimits_color = "#B3B3B380",
  dist_position_scaler = 0.2,
  dist_offset = 0,
  dist_scale = 0.9,
  lineranges_ypos = 0.2,
  lineranges_dodge = 0.15,
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
```

## Arguments

- data:

  Data to use with multiple endpoints stacked into response (values),
  Endpoint(endpoint name)

- response:

  name of the column holding the response values

- endpoint:

  name of the column holding the name/key of the endpoint default to
  `Endpoint`

- model_type:

  type of the trend fit one of "logistic", "none"

- DOSE:

  name of the column holding the DOSE values default to `DOSE`

- color_fill:

  name of the column to be used for color/fill default to DOSE column

- logistic_by_color_fill:

  fit split by color? default `FALSE`

- exposure_metrics:

  name(s) of the column(s) to be stacked into `expname` `exptile` and
  split into `exposure_metric_split`

- exposure_metric_split:

  one of "median", "tertile", "quartile", "none"

- exposure_metric_soc_value:

  special exposure code for standard of care default -99

- exposure_metric_plac_value:

  special exposure code for placebo default 0

- exposure_distribution:

  one of distributions, lineranges, boxplots or none

- exposure_distribution_percent:

  show percent of distribution between binlimits `TRUE`/`FALSE`

- exposure_distribution_percent_text_size:

  distribution percentages text size default to 5

- dose_plac_value:

  string identifying placebo in DOSE column

- xlab:

  text to be used as x axis label

- ylab:

  text to be used as y axis label

- points_alpha:

  alpha transparency for points

- points_show:

  show the observations `TRUE`/`FALSE`

- prob_obs_byexptile:

  observed probability by exptile `TRUE`/`FALSE`

- prob_obs_byexptile_plac:

  observed probability by exptile placebo `TRUE`/`FALSE`

- prob_obs_byexptile_group:

  additional grouping for exptile probabilities default `none`

- prob_text_size:

  probability text size default to 5

- prob_obs_bydose:

  observed probability by dose `TRUE`/`FALSE`

- prob_obs_bydose_plac:

  observed probability by placebo dose `TRUE`/`FALSE`

- Nresp_Ntot_show:

  show N responders/Ntotal ? `TRUE`/`FALSE`

- Nresp_Ntot_size:

  N responders/Ntotal text size default to 5

- Nresp_Ntot_ypos:

  y position for N responders/Ntotal two text elements the first for by
  exptile and the second for by dose/color options include
  `with percentages` `top` `bottom`

- Nresp_Ntot_sep:

  character string to separate N responders/ Ntotal default `/`

- binlimits_show:

  show the binlimits vertical lines `TRUE`/`FALSE`

- binlimits_text_size:

  binlimits text size default to 5

- binlimits_ypos:

  binlimits y position default to 0

- binlimits_color:

  binlimits text color default to alpha("gray70",0.5)

- dist_position_scaler:

  space occupied by the distribution default to 0.2

- dist_offset:

  offset where the distribution position starts default to 0

- dist_scale:

  scaling parameter for ggridges default to 0.9

- lineranges_ypos:

  where to put the lineranges -1

- lineranges_dodge:

  lineranges vertical dodge value 1

- lineranges_doselabel:

  `TRUE`/`FALSE`

- proj_bydose:

  project the probabilities on logistic curve `TRUE`/`FALSE`

- yproj:

  project the probabilities on y axis `TRUE`/`FALSE`

- yproj_xpos:

  y projection x position 0

- yproj_dodge:

  y projection dodge value 0.2

- yaxis_position:

  where to put y axis "left" or "right"

- facet_formula:

  facet formula to be use otherwise `endpoint ~ expname`

- theme_certara:

  apply certara colors and format for strips and default colour/fill

- return_list:

  What to return if True a list of the datasets and plot is returned
  instead of only the plot

## Examples

``` r
# Example 1
library(ggplot2)
effICGI <- logistic_data |>
dplyr::filter(!is.na(ICGI))|>
dplyr::filter(!is.na(AUC))
effICGI$DOSE <- factor(effICGI$DOSE,
                      levels=c("0", "600", "1200","1800","2400"),
                      labels=c("Placebo", "600 mg", "1200 mg","1800 mg","2400 mg"))
effICGI$STUDY <- factor(effICGI$STUDY)    
effICGI$ICGI2 <- effICGI$ICGI
effICGI <- tidyr::gather(effICGI,Endpoint,response,ICGI,ICGI2)
gglogisticexpdist(data = effICGI |>
                 dplyr::filter(Endpoint=="ICGI"),
                 response = "response",
                 endpoint = "Endpoint",
                 exposure_metrics = c("AUC"),
                 exposure_metric_split = c("quartile"),
                 exposure_metric_soc_value = -99,
                 exposure_metric_plac_value = 0,
                 exposure_distribution ="distributions",
                 yproj_xpos = -15,
                 yproj_dodge = 10,
                 dist_position_scaler = 0.1,
                 dist_offset = -0.1,
                 Nresp_Ntot_ypos = c("with percentages","bottom"),
                 prob_obs_bydose_plac = TRUE,
                 prob_obs_byexptile_plac = FALSE,
                 prob_obs_byexptile_group = "none",
                 binlimits_ypos = 0.3,
                 binlimits_color = "#475c6b",
                 points_alpha= 0.8)
#> Joining with `by = join_by(loopvariable, DOSE, quant_10)`
#> Joining with `by = join_by(loopvariable, DOSE, quant_90)`
#> Joining with `by = join_by(loopvariable, DOSE, quant_25)`
#> Joining with `by = join_by(loopvariable, DOSE, quant_75)`
#> Joining with `by = join_by(loopvariable, DOSE, medexp)`
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'
#> Picking joint bandwidth of 11.7
#> Warning: Removed 244 rows containing non-finite outside the scale range
#> (`stat_density_ridges()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_pointrange()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_text_repel()`).

                 
# Example 2                
gglogisticexpdist(data = effICGI |>
                 dplyr::filter(Endpoint=="ICGI"),
                 response = "response",
                 endpoint = "Endpoint",
                 exposure_metrics = c("CMAX"),
                 exposure_metric_split = c("tertile"),
                 exposure_metric_soc_value = -99,
                 exposure_metric_plac_value = 0,
                 exposure_distribution ="lineranges",
                 lineranges_ypos = -0.2,
                 lineranges_dodge = 0.2,
                 lineranges_doselabel = TRUE,
                 Nresp_Ntot_ypos = c("with percentages","bottom"),
                 prob_obs_bydose_plac = TRUE,
                 prob_obs_byexptile_plac = FALSE,
                 prob_obs_byexptile_group = "none",yproj_xpos = -1,
                 yproj_dodge = 2,
                 binlimits_color = "#475c6b",
                 dist_position_scaler = 0.1)
#> Joining with `by = join_by(loopvariable, DOSE, quant_10)`
#> Joining with `by = join_by(loopvariable, DOSE, quant_90)`
#> Joining with `by = join_by(loopvariable, DOSE, quant_25)`
#> Joining with `by = join_by(loopvariable, DOSE, quant_75)`
#> Joining with `by = join_by(loopvariable, DOSE, medexp)`
#> `geom_smooth()` using formula = 'y ~ x'
#> `geom_smooth()` using formula = 'y ~ x'
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_pointrange()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_text_repel()`).



if (FALSE) { # \dontrun{
#' # Example 3                
library(ggh4x)
gglogisticexpdist(data = effICGI |>
                 dplyr::filter(Endpoint=="ICGI"), 
                 response = "response",
                 endpoint = "Endpoint",
                 DOSE = "DOSE",
                 exposure_metrics = c("AUC"),
                 exposure_metric_split = c("quartile"),
                 exposure_distribution ="distributions",
                 exposure_metric_soc_value = -99,
                 exposure_metric_plac_value = 0,
                 dist_position_scaler = 0.15)+
 facet_grid2(Endpoint~expname+DOSE2,scales="free",
 margins = "DOSE2",strip = strip_nested())
# Example 4  
effICGI$SEX <- as.factor(effICGI$SEX)               
gglogisticexpdist(data = effICGI  |>
                 dplyr::filter(Endpoint=="ICGI"), 
                 response = "response",
                 endpoint = "Endpoint",
                 DOSE = "DOSE",
                 color_fill = "SEX",
                 exposure_metrics = c("AUC"),
                 exposure_metric_split = c("quartile"),
                 exposure_distribution ="distributions",
                 exposure_metric_soc_value = -99,
                 exposure_metric_plac_value = 0,
                 lineranges_ypos = -0.2,
                 yproj_xpos = -10,
                 yproj_dodge = 20,
                 prob_text_size = 6,
                 binlimits_text_size = 6,
                 Nresp_Ntot_show = TRUE,
                 dist_position_scaler = 0.15)+
                 ggplot2::scale_x_continuous(breaks = seq(0,350,50),
                 expand = ggplot2::expansion(add= c(0,0),mult=c(0,0)))+
                 ggplot2::coord_cartesian(xlim = c(-30,355))+
                 ggplot2::facet_grid(Endpoint~expname+color_fill2, margins ="color_fill2" )

#Example 4b
  effICGI$SEX <- as.factor(effICGI$SEX)
 gglogisticexpdist(data = effICGI |>
  dplyr::filter(Endpoint =="ICGI"),
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
                 lineranges_doselabel = TRUE)
                 
#Example 5
gglogisticexpdist(data = effICGI |> dplyr::filter(Endpoint=="ICGI"), 
                  response = "response",
                  endpoint = "Endpoint",
                  DOSE = "DOSE",
                  exposure_metrics = c("AUC"),
                  exposure_metric_split = c("quartile"),
                  exposure_distribution ="distributions",
                  exposure_metric_soc_value = -99,
                  exposure_metric_plac_value = 0,
                  dist_position_scaler = 0.15)+
                 facet_grid(Endpoint~expname+exptile,scales="free",
                 margins = "exptile")
#Example 6
a <- gglogisticexpdist(data = effICGI, # 
                  response = "response",
                  endpoint = "Endpoint",
                  DOSE = "DOSE",yproj_dodge = 36,
                  exposure_metrics = c("AUC"),
                  exposure_metric_split = c("quartile"),
                  exposure_distribution ="lineranges",
                  exposure_metric_soc_value = -99,
                  exposure_metric_plac_value = 0) +
  facet_grid(Endpoint~expname,switch = "both")
b <-   gglogisticexpdist(data = effICGI, # 
                    response = "response",
                    endpoint = "Endpoint",
                    DOSE = "DOSE",yproj_dodge = 2,
                    exposure_metrics = c("CMAX"),
                    exposure_metric_split = c("quartile"),
                    exposure_distribution ="lineranges",
                    exposure_metric_soc_value = -99,
                    exposure_metric_plac_value = 0,
                    yaxis_position = "right")+
  facet_grid(Endpoint~expname,switch = "x")+
  theme(strip.text.y.right = element_blank(),
        strip.background.y = element_blank())
library(patchwork)
(a | b ) +
  plot_layout(guides = "collect", axes = "collect_x")&
  theme(legend.position = "top")

#Example 7
effICGI <- logistic_data |>
dplyr::filter(!is.na(ICGI))|>
dplyr::filter(!is.na(AUC))
effICGI$DOSE <- factor(effICGI$DOSE,
                      levels=c("0", "600", "1200","1800","2400"),
                      labels=c("Placebo", "600 mg", "1200 mg","1800 mg","2400 mg"))
effICGI$STUDY <- factor(effICGI$STUDY)
effICGI$ICGI2 <- ifelse(effICGI$ICGI7 < 4,1,0)
effICGI$ICGI3 <- ifelse(effICGI$ICGI7 < 5,1,0)

effICGI <- tidyr::gather(effICGI,Endpoint,response,ICGI,ICGI2,ICGI3)
effICGI$endpointcol2 <- effICGI$Endpoint
effICGI$endpointcol3 <- effICGI$Endpoint
gglogisticexpdist(data = effICGI,
                  response = "response",
                 endpoint = "Endpoint",
                  exposure_metrics = c("AUC"),
                  exposure_metric_split = c("tertile"),
                  exposure_metric_soc_value = -99,
                  exposure_metric_plac_value = 0,
                  color_fill = "endpointcol2",                   
                  logistic_by_color_fill = TRUE,
                  Nresp_Ntot_show = TRUE,
                  Nresp_Ntot_ypos = c("with percentages","bottom"),
                  prob_obs_byexptile = FALSE,
                  prob_obs_byexptile_group="endpointcol3",
                  prob_obs_byexptile_plac = FALSE,
                  prob_obs_bydose_plac = TRUE,
                  binlimits_color = "#475c6b",
                  exposure_distribution ="distributions",
                  prob_obs_bydose = TRUE,
                  proj_bydose = FALSE,
                  yproj = FALSE,
                  dist_position_scaler = 0.1,
                  dist_offset = -0.1)+
  facet_grid(expname~Endpoint,scales="free_x")
#Example 8  
gglogisticexpdist(data = effICGI,
                  response = "response",
                 endpoint = "endpointcol2",
                  exposure_metrics = c("AUC"),
                  exposure_metric_split = c("quartile"),
                  exposure_metric_soc_value = -99,
                  exposure_metric_plac_value = 0,
                  color_fill = "Endpoint",                   
                  logistic_by_color_fill = TRUE,
                  Nresp_Ntot_show = FALSE, points_show = FALSE,
                  prob_obs_byexptile = TRUE,
                  prob_obs_byexptile_group="endpointcol3",
                  prob_obs_byexptile_plac = TRUE,
                  prob_obs_bydose = FALSE,
                  prob_obs_bydose_plac = FALSE,
                  binlimits_color = "#475c6b",
                  exposure_distribution ="distributions",
                  proj_bydose = FALSE,
                  yproj = FALSE,
                  dist_position_scaler = 0.1,
                  dist_offset = -0.1)+
  facet_grid(expname~.,scales="free_x")
} # }
```
