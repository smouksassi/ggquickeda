#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom rlang sym
#' @import tidyr
#' @importFrom stats median
#' @importFrom stats quantile

summary_df <- function(x,y, probs = c(0.25,0.75,0.90,0.10)) {
  N = Ntot = prob = NULL
 tibble::tibble(
    minexp = min(x),
    maxexp = max(x),
    medexp = stats::median(x),
    meanexp = mean(x),
    N = sum(y,na.rm=TRUE),
    Nmiss = length(x[is.na(x)]),
    Ntot = dplyr::n(),
    prob = N/Ntot,
    SE = sqrt(prob*(1-prob)/Ntot ),
    values = stats::quantile(x, probs, na.rm = TRUE),
    quant = probs
  )
}
plogis <- function(x) exp(x)/(1+exp(x))

#' Create a logistic fit vs exposure(s)plot
#'
#' Produces a logistic fit plot with a facettable exposures/quantiles/distributions in ggplot2
#' @param data Data to use with multiple endpoints stacked into response (values), Endpoint(endpoint name)
#' @param response name of the column holding the response values
#' @param endpoint name of the column holding the name/key of the endpoint default to `Endpoint`
#' @param model_type type of the trend fit one of "logistic", "none" 
#' @param DOSE name of the column holding the DOSE values default to `DOSE`
#' @param color_fill name of the column to be used for color/fill default to DOSE column
#' @param logistic_by_color_fill fit split by color? default `FALSE`
#' @param exposure_metrics name(s) of the column(s) to be stacked into `expname` `exptile` and split into `exposure_metric_split`
#' @param exposure_metric_split one of "median", "tertile", "quartile", "none"
#' @param exposure_metric_soc_value  special exposure code for standard of care default -99 
#' @param exposure_metric_plac_value special exposure code for placebo default 0
#' @param exposure_distribution one of distributions, lineranges, boxplots or none
#' @param exposure_distribution_percent show percent of distribution between binlimits `TRUE`/`FALSE`
#' @param exposure_distribution_percent_text_size  distribution percentages text size default to 5
#' @param dose_plac_value string identifying placebo in DOSE column
#' @param xlab text to be used as x axis label
#' @param ylab text to be used as y axis label
#' @param points_alpha alpha transparency for points
#' @param points_show show the observations `TRUE`/`FALSE`
#' @param prob_obs_byexptile observed probability by exptile `TRUE`/`FALSE`
#' @param prob_obs_byexptile_plac observed probability by exptile placebo `TRUE`/`FALSE` 
#' @param prob_obs_byexptile_group additional grouping for exptile probabilities default `none`
#' @param prob_text_size probability text size default to 5
#' @param prob_obs_bydose observed probability by dose `TRUE`/`FALSE`
#' @param prob_obs_bydose_plac observed probability by placebo dose `TRUE`/`FALSE`
#' @param Nresp_Ntot_show show N responders/Ntotal ? `TRUE`/`FALSE`
#' @param Nresp_Ntot_size N responders/Ntotal text size default to 5
#' @param Nresp_Ntot_ypos y position for N responders/Ntotal two text elements the first for by exptile and the second
#'  for by dose/color options include `with percentages` `top` `bottom`
#' @param Nresp_Ntot_sep character string to separat N responders/ Ntotal default `/`
#' @param binlimits_show show the binlimits vertical lines `TRUE`/`FALSE`
#' @param binlimits_text_size binlimits text size default to 5
#' @param binlimits_ypos binlimits y position default to 0 
#' @param binlimits_color binlimits text color default to alpha("gray70",0.5)
#' @param dist_position_scaler space occupied by the distribution default to 0.2 
#' @param dist_offset offset where the distribution position starts default to 0
#' @param dist_scale scaling parameter for ggridges default to 0.9
#' @param lineranges_ypos where to put the lineranges -1
#' @param lineranges_dodge lineranges vertical dodge value 1
#' @param lineranges_doselabel `TRUE`/`FALSE`
#' @param proj_bydose project the probabilities on logistic curve `TRUE`/`FALSE`
#' @param yproj project the probabilities on y axis `TRUE`/`FALSE`
#' @param yproj_xpos y projection x position 0
#' @param yproj_dodge  y projection dodge value 0.2
#' @param yaxis_position where to put y axis "left" or "right"
#' @param facet_formula facet formula to be use otherwise `endpoint ~ expname`
#' @param theme_certara apply certara colors and format for strips and default colour/fill
#' @param return_list What to return if True a list of the datasets and plot is returned instead of only the plot
#' @examples
#' # Example 1
#' library(ggplot2)
#' effICGI <- logistic_data |>
#' dplyr::filter(!is.na(ICGI))|>
#' dplyr::filter(!is.na(AUC))
#'effICGI$DOSE <- factor(effICGI$DOSE,
#'                       levels=c("0", "600", "1200","1800","2400"),
#'                       labels=c("Placebo", "600 mg", "1200 mg","1800 mg","2400 mg"))
#'effICGI$STUDY <- factor(effICGI$STUDY)    
#'effICGI$ICGI2 <- effICGI$ICGI
#'effICGI <- tidyr::gather(effICGI,Endpoint,response,ICGI,ICGI2)
#' gglogisticexpdist(data = effICGI |>
#'                  dplyr::filter(Endpoint=="ICGI"),
#'                  response = "response",
#'                  endpoint = "Endpoint",
#'                  exposure_metrics = c("AUC"),
#'                  exposure_metric_split = c("quartile"),
#'                  exposure_metric_soc_value = -99,
#'                  exposure_metric_plac_value = 0,
#'                  exposure_distribution ="distributions",
#'                  yproj_xpos = -15,
#'                  yproj_dodge = 10,
#'                  dist_position_scaler = 0.1,
#'                  dist_offset = -0.1,
#'                  Nresp_Ntot_ypos = c("with percentages","bottom"),
#'                  prob_obs_bydose_plac = TRUE,
#'                  prob_obs_byexptile_plac = FALSE,
#'                  prob_obs_byexptile_group = "none",
#'                  binlimits_ypos = 0.3,
#'                  binlimits_color = "#475c6b",
#'                  points_alpha= 0.8)
#'                  
#' # Example 2                
#' gglogisticexpdist(data = effICGI |>
#'                  dplyr::filter(Endpoint=="ICGI"),
#'                  response = "response",
#'                  endpoint = "Endpoint",
#'                  exposure_metrics = c("CMAX"),
#'                  exposure_metric_split = c("tertile"),
#'                  exposure_metric_soc_value = -99,
#'                  exposure_metric_plac_value = 0,
#'                  exposure_distribution ="lineranges",
#'                  lineranges_ypos = -0.2,
#'                  lineranges_dodge = 0.2,
#'                  lineranges_doselabel = TRUE,
#'                  Nresp_Ntot_ypos = c("with percentages","bottom"),
#'                  prob_obs_bydose_plac = TRUE,
#'                  prob_obs_byexptile_plac = FALSE,
#'                  prob_obs_byexptile_group = "none",yproj_xpos = -1,
#'                  yproj_dodge = 2,
#'                  binlimits_color = "#475c6b",
#'                  dist_position_scaler = 0.1)
#'
#'
#'\dontrun{
#'#' # Example 3                
#'library(ggh4x)
#'gglogisticexpdist(data = effICGI |>
#'                  dplyr::filter(Endpoint=="ICGI"), 
#'                  response = "response",
#'                  endpoint = "Endpoint",
#'                  DOSE = "DOSE",
#'                  exposure_metrics = c("AUC"),
#'                  exposure_metric_split = c("quartile"),
#'                  exposure_distribution ="distributions",
#'                  exposure_metric_soc_value = -99,
#'                  exposure_metric_plac_value = 0,
#'                  dist_position_scaler = 0.15)+
#'  facet_grid2(Endpoint~expname+DOSE2,scales="free",
#'  margins = "DOSE2",strip = strip_nested())
#' # Example 4  
#' effICGI$SEX <- as.factor(effICGI$SEX)               
#' gglogisticexpdist(data = effICGI  |>
#'                  dplyr::filter(Endpoint=="ICGI"), 
#'                  response = "response",
#'                  endpoint = "Endpoint",
#'                  DOSE = "DOSE",
#'                  color_fill = "SEX",
#'                  exposure_metrics = c("AUC"),
#'                  exposure_metric_split = c("quartile"),
#'                  exposure_distribution ="distributions",
#'                  exposure_metric_soc_value = -99,
#'                  exposure_metric_plac_value = 0,
#'                  lineranges_ypos = -0.2,
#'                  yproj_xpos = -10,
#'                  yproj_dodge = 20,
#'                  prob_text_size = 6,
#'                  binlimits_text_size = 6,
#'                  Nresp_Ntot_show = TRUE,
#'                  dist_position_scaler = 0.15)+
#'                  ggplot2::scale_x_continuous(breaks = seq(0,350,50),
#'                  expand = ggplot2::expansion(add= c(0,0),mult=c(0,0)))+
#'                  ggplot2::coord_cartesian(xlim = c(-30,355))+
#'                  ggplot2::facet_grid(Endpoint~expname+color_fill2, margins ="color_fill2" )
#'
#' #Example 4b
#'   effICGI$SEX <- as.factor(effICGI$SEX)
#'  gglogisticexpdist(data = effICGI |>
#'   dplyr::filter(Endpoint =="ICGI"),
#'                  response = "response",
#'                  endpoint = "Endpoint",
#'                  color_fill = "SEX",
#'                  exposure_metrics = c("AUC"),
#'                  exposure_metric_split = c("quartile"),
#'                  exposure_metric_soc_value = -99,
#'                  exposure_metric_plac_value = 0,
#'                  dist_position_scaler = 1, dist_offset = -1 ,
#'                  yproj_xpos =  -20 ,
#'                  yproj_dodge = 20 ,
#'                  exposure_distribution ="lineranges",
#'                  lineranges_doselabel = TRUE)
#'                  
#' #Example 5
#' gglogisticexpdist(data = effICGI |> dplyr::filter(Endpoint=="ICGI"), 
#'                   response = "response",
#'                   endpoint = "Endpoint",
#'                   DOSE = "DOSE",
#'                   exposure_metrics = c("AUC"),
#'                   exposure_metric_split = c("quartile"),
#'                   exposure_distribution ="distributions",
#'                   exposure_metric_soc_value = -99,
#'                   exposure_metric_plac_value = 0,
#'                   dist_position_scaler = 0.15)+
#'                  facet_grid(Endpoint~expname+exptile,scales="free",
#'                  margins = "exptile")
#' #Example 6
#' a <- gglogisticexpdist(data = effICGI, # 
#'                   response = "response",
#'                   endpoint = "Endpoint",
#'                   DOSE = "DOSE",yproj_dodge = 36,
#'                   exposure_metrics = c("AUC"),
#'                   exposure_metric_split = c("quartile"),
#'                   exposure_distribution ="lineranges",
#'                   exposure_metric_soc_value = -99,
#'                   exposure_metric_plac_value = 0) +
#'   facet_grid(Endpoint~expname,switch = "both")
#' b <-   gglogisticexpdist(data = effICGI, # 
#'                     response = "response",
#'                     endpoint = "Endpoint",
#'                     DOSE = "DOSE",yproj_dodge = 2,
#'                     exposure_metrics = c("CMAX"),
#'                     exposure_metric_split = c("quartile"),
#'                     exposure_distribution ="lineranges",
#'                     exposure_metric_soc_value = -99,
#'                     exposure_metric_plac_value = 0,
#'                     yaxis_position = "right")+
#'   facet_grid(Endpoint~expname,switch = "x")+
#'   theme(strip.text.y.right = element_blank(),
#'         strip.background.y = element_blank())
#' library(patchwork)
#' (a | b ) +
#'   plot_layout(guides = "collect", axes = "collect_x")&
#'   theme(legend.position = "top")
#'
#' #Example 7
#' effICGI <- logistic_data |>
#' dplyr::filter(!is.na(ICGI))|>
#' dplyr::filter(!is.na(AUC))
#' effICGI$DOSE <- factor(effICGI$DOSE,
#'                       levels=c("0", "600", "1200","1800","2400"),
#'                       labels=c("Placebo", "600 mg", "1200 mg","1800 mg","2400 mg"))
#' effICGI$STUDY <- factor(effICGI$STUDY)
#' effICGI$ICGI2 <- ifelse(effICGI$ICGI7 < 4,1,0)
#' effICGI$ICGI3 <- ifelse(effICGI$ICGI7 < 5,1,0)
#' 
#' effICGI <- tidyr::gather(effICGI,Endpoint,response,ICGI,ICGI2,ICGI3)
#' effICGI$endpointcol2 <- effICGI$Endpoint
#' effICGI$endpointcol3 <- effICGI$Endpoint

#' gglogisticexpdist(data = effICGI,
#'                   response = "response",
#'                  endpoint = "Endpoint",
#'                   exposure_metrics = c("AUC"),
#'                   exposure_metric_split = c("tertile"),
#'                   exposure_metric_soc_value = -99,
#'                   exposure_metric_plac_value = 0,
#'                   color_fill = "endpointcol2",                   
#'                   logistic_by_color_fill = TRUE,
#'                   Nresp_Ntot_show = TRUE,
#'                   Nresp_Ntot_ypos = c("with percentages","bottom"),
#'                   prob_obs_byexptile = FALSE,
#'                   prob_obs_byexptile_group="endpointcol3",
#'                   prob_obs_byexptile_plac = FALSE,
#'                   prob_obs_bydose_plac = TRUE,
#'                   binlimits_color = "#475c6b",
#'                   exposure_distribution ="distributions",
#'                   prob_obs_bydose = TRUE,
#'                   proj_bydose = FALSE,
#'                   yproj = FALSE,
#'                   dist_position_scaler = 0.1,
#'                   dist_offset = -0.1)+
#'   facet_grid(expname~Endpoint,scales="free_x")
#' #Example 8  
#' gglogisticexpdist(data = effICGI,
#'                   response = "response",
#'                  endpoint = "endpointcol2",
#'                   exposure_metrics = c("AUC"),
#'                   exposure_metric_split = c("quartile"),
#'                   exposure_metric_soc_value = -99,
#'                   exposure_metric_plac_value = 0,
#'                   color_fill = "Endpoint",                   
#'                   logistic_by_color_fill = TRUE,
#'                   Nresp_Ntot_show = FALSE, points_show = FALSE,
#'                   prob_obs_byexptile = TRUE,
#'                   prob_obs_byexptile_group="endpointcol3",
#'                   prob_obs_byexptile_plac = TRUE,
#'                   prob_obs_bydose = FALSE,
#'                   prob_obs_bydose_plac = FALSE,
#'                   binlimits_color = "#475c6b",
#'                   exposure_distribution ="distributions",
#'                   proj_bydose = FALSE,
#'                   yproj = FALSE,
#'                   dist_position_scaler = 0.1,
#'                   dist_offset = -0.1)+
#'   facet_grid(expname~.,scales="free_x")
#'}
#' @export               
gglogisticexpdist <- function(data = effICGI, 
                              response = "response",
                              endpoint = "Endpoint",
                              model_type = c("logistic"),
                              DOSE = "DOSE",
                              color_fill = "DOSE",
                              logistic_by_color_fill = FALSE,
                              exposure_metrics = c("AUC","CMAX"),
                              exposure_metric_split = c("median","tertile","quartile","none"),
                              exposure_metric_soc_value = -99,
                              exposure_metric_plac_value = 0,
                              exposure_distribution = c("distributions","lineranges","boxplots","none"),
                              exposure_distribution_percent = TRUE,
                              exposure_distribution_percent_text_size = 5,
                              dose_plac_value = "Placebo",
                              xlab = "Exposure Values",
                              ylab ="Probability of Response",
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
                              Nresp_Ntot_ypos = c("with percentages","top"),
                              Nresp_Ntot_sep = "/",
                              binlimits_show = TRUE,
                              binlimits_text_size = 5,
                              binlimits_ypos = 0,
                              binlimits_color= "#B3B3B380",
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
                              yaxis_position = c("left","right"),
                              facet_formula = NULL,
                              theme_certara = TRUE,
                              return_list = FALSE
) {
  
  responseinputvar  <-  response
  endpointinputvar  <- endpoint
  DOSEinputvar  <- DOSE
  colorinputvar    <-  if (color_fill !="none") color_fill else NULL
  model_type <- match.arg(model_type, several.ok = FALSE)
  exptilegroupvar <-  prob_obs_byexptile_group
  if( all(exposure_metrics == "none") )   stop("exposure_metrics should specify at least one exposure", call. = FALSE)
  exposure_metrics <-  exposure_metrics[exposure_metrics!="none"]
  exposure_metric_split <- match.arg(exposure_metric_split, several.ok = FALSE)
  exposure_distribution <- match.arg(exposure_distribution, several.ok = FALSE)
  yaxis_position <- match.arg(yaxis_position, several.ok = FALSE)

  effICGI = expname = expvalue = DOSE2 = quant = values = Ncat = Ntot = xmed = percentage = exptile = keynumeric = NULL
  intercept = medexp = prob = SE = N = ndensity = Endpoint = color_fill2 = NULL
  
  data.long <- data |> 
    tidyr::gather(expname,expvalue,!!!exposure_metrics, factor_key = TRUE) |> 
    dplyr::group_by(expname,!!sym(endpointinputvar) ) 
  
  if(exposure_metric_split=="none") {
    data.long <- data.long |> 
      dplyr::mutate(exptile = dplyr::case_when(
        expvalue == exposure_metric_soc_value  ~ "SOC",
        expvalue == exposure_metric_plac_value ~ "Placebo",
        expvalue  > exposure_metric_plac_value ~ "(all)"))
    data.long$keynumeric <- - dist_position_scaler*as.numeric(forcats::fct_rev(as.factor(dplyr::pull(data.long[,DOSEinputvar])))) + dist_offset
    xintercepts <- data.long|> 
      dplyr::group_by(expname,!!sym(endpointinputvar) )|> 
      dplyr::reframe(intercept = stats::quantile(expvalue[!expvalue %in%
                                                            c(exposure_metric_soc_value, exposure_metric_plac_value)],
                                                 c(0,1), na.rm=TRUE),
                     quant = c(0,1) ) 
    
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
    
    data.long$keynumeric <- - dist_position_scaler*as.numeric(forcats::fct_rev(as.factor(dplyr::pull(data.long[,DOSEinputvar])))) + dist_offset
    xintercepts <- data.long|> 
      dplyr::group_by(expname,!!sym(endpointinputvar) )|> 
      dplyr::reframe(intercept = stats::quantile(expvalue[!expvalue %in%
                                                            c(exposure_metric_soc_value, exposure_metric_plac_value)],
                                                 c(0,0.25,0.5,0.75,1), na.rm=TRUE), quant = c(0,0.25,0.5,0.75,1) ) 
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
    data.long$keynumeric <- - dist_position_scaler*as.numeric(forcats::fct_rev(as.factor(dplyr::pull(data.long[,DOSEinputvar])))) + dist_offset
    xintercepts <- data.long|> 
      dplyr::group_by(expname,!!sym(endpointinputvar) )|> 
      dplyr::reframe(intercept = stats::quantile(expvalue[!expvalue %in%
                                                            c(exposure_metric_soc_value, exposure_metric_plac_value)],
                                                 c(0,1/3,2/3,1), na.rm=TRUE), quant = c(0,1/3,2/3,1)) 
    
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
    data.long$keynumeric <- - dist_position_scaler*as.numeric(forcats::fct_rev(as.factor(dplyr::pull(data.long[,DOSEinputvar])))) + dist_offset
    xintercepts <- data.long|> 
      dplyr::group_by(expname,!!sym(endpointinputvar) )|> 
      dplyr::reframe(intercept = stats::quantile(expvalue[!expvalue %in%
                                                            c(exposure_metric_soc_value, exposure_metric_plac_value)],
                                                 c(0,0.5,1), na.rm=TRUE), quant = c(0,0.5,1) ) 
    
  }
  data.long$exptile2 <- data.long$exptile
  data.long[,"DOSE2"]    <- data.long[,DOSEinputvar]
  data.long[,"color_fill2"]    <- data.long[,colorinputvar]

  if(exptilegroupvar=="none"){
    if(color_fill != DOSEinputvar) {
      data.long.summaries.dose <- data.long |>
        dplyr::group_by(!!sym(endpointinputvar),expname,!!sym(color_fill),color_fill2,!!sym(DOSEinputvar),DOSE2)|>
        dplyr::reframe(
          summary_df(expvalue,!!sym(responseinputvar))) |>
        tidyr::pivot_wider(names_from= quant,values_from = values,names_glue = "quant_{100*quant}")
    }
    
    if(color_fill == DOSEinputvar) {
      data.long.summaries.dose <- data.long |>
        dplyr::group_by(!!sym(endpointinputvar),expname,!!sym(DOSEinputvar),DOSE2)|>
        dplyr::reframe(
          summary_df(expvalue,!!sym(responseinputvar))) |>
        tidyr::pivot_wider(names_from= quant,values_from = values,names_glue = "quant_{100*quant}")
    }
  }
  
  if(exptilegroupvar!="none"){
    if(color_fill != DOSEinputvar) {
      data.long.summaries.dose <- data.long |>
        dplyr::group_by(!!sym(endpointinputvar),expname,!!sym(color_fill),color_fill2,
                        !!sym(DOSEinputvar),DOSE2,
                        !!sym(exptilegroupvar))|>
        dplyr::reframe(
          summary_df(expvalue,!!sym(responseinputvar))) |>
        tidyr::pivot_wider(names_from= quant,values_from = values,names_glue = "quant_{100*quant}")
    }
    if(color_fill == DOSEinputvar) {
      data.long.summaries.dose <- data.long |>
        dplyr::group_by(!!sym(endpointinputvar),expname,
                        !!sym(DOSEinputvar),DOSE2,
                        !!sym(exptilegroupvar))|>
        dplyr::reframe(
          summary_df(expvalue,!!sym(responseinputvar))) |>
        tidyr::pivot_wider(names_from= quant,values_from = values,names_glue = "quant_{100*quant}")
    }
  }
  
  loopvariables <- unique(c(endpointinputvar,"expname",exptilegroupvar))
  loopvariables <- loopvariables[loopvariables!="none"]
  data.long.summaries.dose <- tidyr::unite(data.long.summaries.dose,"loopvariable",
                                           !!!loopvariables, remove = FALSE)
  data.long.summaries.dose$meanresprlower <- data.long.summaries.dose$meanresp-
    1.959*data.long.summaries.dose$SE
  data.long.summaries.dose$meanresprupper <- data.long.summaries.dose$meanresp+
    1.959*data.long.summaries.dose$SE
  
  
  data.long <- tidyr::unite(data.long,"loopvariable", !!!loopvariables, remove = FALSE)
  
  logisticfit_by_endpoint <- list()
  predict_by_endpoint_expname <- list()
  predict_by_endpoint_expname_dose <- list()
  predict_by_endpoint_expname_dose2 <- list()
  
  for (i in unique(data.long[,"loopvariable"]) |>
       dplyr::pull() |>
       as.character() ) {

    logisticregdata<- data.long |>
      dplyr::filter(.data[["loopvariable"]] ==i)
    d <- rms::datadist(logisticregdata[, c(endpointinputvar,responseinputvar,DOSEinputvar,"expname","expvalue","exptile")])           
    options(datadist= d)
    logisticfit_by_endpoint_fit <- eval(bquote( rms::lrm( as.formula(paste(responseinputvar,"~","expvalue")) ,
                                                          data=logisticregdata,x=TRUE,y=TRUE) ))
    logisticfit_by_endpoint[[i]] <- logisticfit_by_endpoint_fit
    
    data.long.summaries.dose.loop <- data.long.summaries.dose |>
      dplyr::filter(.data[["loopvariable"]] ==i)
    pred10exp <- as.data.frame(rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                            expvalue= data.long.summaries.dose.loop$quant_10))
    names(pred10exp)<- c("quant_10" ,  "ymid10",  "ylow10", "yup10")
    pred10exp$loopvariable <- i
    pred90exp <- as.data.frame(rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                            expvalue= data.long.summaries.dose.loop$quant_90))
    names(pred90exp)<- c("quant_90" ,  "ymid90",  "ylow90", "yup90")
    pred90exp$loopvariable <- i
    pred25exp <- as.data.frame(rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                            expvalue= data.long.summaries.dose.loop$quant_25))
    names(pred25exp)<- c("quant_25" ,  "ymid25",  "ylow25", "yup25")
    pred25exp$loopvariable <- i 
    pred75exp <- as.data.frame(rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                            expvalue= data.long.summaries.dose.loop$quant_75))
    names(pred75exp)<- c("quant_75" ,  "ymid75",  "ylow75", "yup75")
    pred75exp$loopvariable <- i 
    pred50exp<- as.data.frame (rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                            expvalue=data.long.summaries.dose.loop$medexp))
    names(pred50exp)<- c("medexp" ,  "ymid50",  "ylow50", "yup50")
    pred50exp$loopvariable <- i 
    
    pred10exp[,DOSEinputvar] <- pred90exp[,DOSEinputvar] <- pred25exp[,DOSEinputvar] <- 
    pred75exp[,DOSEinputvar] <- pred50exp[,DOSEinputvar] <- data.long.summaries.dose.loop[,DOSEinputvar]
    if(color_fill != DOSEinputvar) {
    pred10exp[,color_fill] <- pred90exp[,color_fill] <- pred25exp[,color_fill] <- 
    pred75exp[,color_fill] <- pred50exp[,color_fill] <- data.long.summaries.dose.loop[,color_fill]
    pred10exp[,"color_fill2"] <- pred90exp[,"color_fill2"] <- pred25exp[,"color_fill2"] <- 
    pred75exp[,"color_fill2"] <- pred50exp[,"color_fill2"] <- data.long.summaries.dose.loop[,"color_fill2"]
    }
    data.long.summaries.dose.loop<- dplyr::left_join(data.long.summaries.dose.loop,pred10exp)
    data.long.summaries.dose.loop<- dplyr::left_join(data.long.summaries.dose.loop,pred90exp)
    data.long.summaries.dose.loop<- dplyr::left_join(data.long.summaries.dose.loop,pred25exp)
    data.long.summaries.dose.loop<- dplyr::left_join(data.long.summaries.dose.loop,pred75exp)
    data.long.summaries.dose.loop<- dplyr::left_join(data.long.summaries.dose.loop,pred50exp)
    predict_by_endpoint_expname[[i]] <- data.long.summaries.dose.loop
    data.long.summaries.dose.loop <- data.long.summaries.dose.loop |>
    dplyr::ungroup()

    if(exptilegroupvar=="none"){
      if(color_fill != DOSEinputvar) {
        predictionsbydose<- data.long.summaries.dose.loop |>
          dplyr::group_by(!!sym(DOSEinputvar),!!sym(endpointinputvar),expname,DOSE2,!!sym(color_fill),color_fill2) |>
          dplyr::do(as.data.frame(rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                               expvalue=seq(.data$quant_10,.data$quant_90,length.out=100))))
        predict_by_endpoint_expname_dose[[i]] <- predictionsbydose
        
        predictionsbydose2<- data.long.summaries.dose.loop |>
          dplyr::group_by(!!sym(DOSEinputvar),!!sym(endpointinputvar),expname,DOSE2,!!sym(color_fill),color_fill2) |>
          dplyr::do(as.data.frame(rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                               expvalue=seq(.data$quant_25,.data$quant_75,length.out=100))))
        predict_by_endpoint_expname_dose2[[i]] <- predictionsbydose2
      }
      if(color_fill == DOSEinputvar) {
        predictionsbydose<- data.long.summaries.dose.loop |>
          dplyr::group_by(!!sym(DOSEinputvar),!!sym(endpointinputvar),expname,DOSE2) |>
          dplyr::do(as.data.frame(rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                               expvalue=seq(.data$quant_10,.data$quant_90,length.out=100))))
        predict_by_endpoint_expname_dose[[i]] <- predictionsbydose
        
        predictionsbydose2<- data.long.summaries.dose.loop |>
          dplyr::group_by(!!sym(DOSEinputvar),!!sym(endpointinputvar),expname,DOSE2) |>
          dplyr::do(as.data.frame(rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                               expvalue=seq(.data$quant_25,.data$quant_75,length.out=100))))
        predict_by_endpoint_expname_dose2[[i]] <- predictionsbydose2
      }
    }
    if(exptilegroupvar!="none"){
      if(color_fill != DOSEinputvar) {
        predictionsbydose<- data.long.summaries.dose.loop |>
          dplyr::group_by(!!sym(DOSEinputvar),!!sym(endpointinputvar),expname,DOSE2,
                          !!sym(color_fill),color_fill2,
                          !!sym(exptilegroupvar)) |>
          dplyr::do(as.data.frame(rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                               expvalue=seq(.data$quant_10,.data$quant_90,length.out=100))))
        predict_by_endpoint_expname_dose[[i]] <- predictionsbydose
        
        predictionsbydose2<- data.long.summaries.dose.loop |>
          dplyr::group_by(!!sym(DOSEinputvar),!!sym(endpointinputvar),expname,DOSE2,
                          !!sym(color_fill),color_fill2,
                          !!sym(exptilegroupvar)) |>
          dplyr::do(as.data.frame(rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                               expvalue=seq(.data$quant_25,.data$quant_75,length.out=100))))
        predict_by_endpoint_expname_dose2[[i]] <- predictionsbydose2
      }
      if(color_fill == DOSEinputvar) {
        predictionsbydose<- data.long.summaries.dose.loop |>
          dplyr::group_by(!!sym(DOSEinputvar),!!sym(endpointinputvar),expname,DOSE2,
                          !!sym(exptilegroupvar)) |>
          dplyr::do(as.data.frame(rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                               expvalue=seq(.data$quant_10,.data$quant_90,length.out=100))))
        predict_by_endpoint_expname_dose[[i]] <- predictionsbydose
        
        predictionsbydose2<- data.long.summaries.dose.loop |>
          dplyr::group_by(!!sym(DOSEinputvar),!!sym(endpointinputvar),expname,DOSE2,
                          !!sym(exptilegroupvar)) |>
          dplyr::do(as.data.frame(rms::Predict(logisticfit_by_endpoint_fit,fun=plogis,
                                               expvalue=seq(.data$quant_25,.data$quant_75,length.out=100))))
        predict_by_endpoint_expname_dose2[[i]] <- predictionsbydose2
      }
    }
  }
  predict_by_endpoint_expname <- data.table::rbindlist(predict_by_endpoint_expname)
  predict_by_endpoint_expname_dose <- data.table::rbindlist(predict_by_endpoint_expname_dose)
  predict_by_endpoint_expname_dose2 <- data.table::rbindlist(predict_by_endpoint_expname_dose2)

  if (exptilegroupvar=="none") {
    data.long.summaries.exposure <- data.long |>
      dplyr::ungroup()|>
      dplyr::group_by(!!sym(endpointinputvar),expname,exptile)|>
      dplyr::reframe(
        summary_df(expvalue,!!sym(responseinputvar))) |>
      tidyr::pivot_wider(names_from= quant,values_from =values,names_glue = "quant_{100*quant}")
  }
  if (exptilegroupvar !="none") {
    data.long.summaries.exposure <- data.long |>
      dplyr::ungroup()|>
      dplyr::group_by(!!sym(endpointinputvar),expname,exptile,
                      !!sym(exptilegroupvar))|>
      dplyr::reframe(
        summary_df(expvalue,!!sym(responseinputvar))) |>
      tidyr::pivot_wider(names_from= quant,values_from =values,names_glue = "quant_{100*quant}")
  }
  
  data.long.summaries.exposure$meanresprlower <- data.long.summaries.exposure$meanresp-
    1.959*data.long.summaries.exposure$SE
  data.long.summaries.exposure$meanresprupper <- data.long.summaries.exposure$meanresp+
    1.959*data.long.summaries.exposure$SE
  

  if(color_fill == DOSEinputvar) {
    if (exptilegroupvar =="none") {
      percentineachbreakcategory <- data.long |>
        dplyr::filter(!!sym(DOSEinputvar)!= dose_plac_value) |>
        dplyr::select(  !!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2,
                        expvalue,exptile)|>
        dplyr::group_by( !!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2) |> 
        dplyr::mutate(Ntot = dplyr::n())|> 
        dplyr::group_by( !!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2,exptile) |> 
        dplyr::mutate(Ncat = dplyr::n(),xmed=median(expvalue))|> 
        dplyr::mutate(percentage=Ncat/Ntot)|> 
        dplyr::distinct(expname,!!sym(DOSEinputvar),exptile,xmed,percentage,keynumeric,DOSE2)|> 
        dplyr::arrange(expname,!!sym(DOSEinputvar),exptile)
    }
    if (exptilegroupvar !="none") {
      percentineachbreakcategory <- data.long |>
        dplyr::filter(!!sym(DOSEinputvar)!= dose_plac_value) |>
        dplyr::select(  !!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2,!!sym(exptilegroupvar),
                        expvalue,exptile)|>
        dplyr::group_by( !!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2,!!sym(exptilegroupvar)) |> 
        dplyr::mutate(Ntot = dplyr::n())|>
        dplyr::group_by( !!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2,exptile,!!sym(exptilegroupvar)) |>  
        dplyr::mutate(Ncat = dplyr::n(),xmed=median(expvalue))|> 
        dplyr::mutate(percentage=Ncat/Ntot)|> 
        dplyr::distinct(expname,!!sym(DOSEinputvar),exptile,xmed,percentage,keynumeric,DOSE2,!!sym(exptilegroupvar))|> 
        dplyr::arrange(expname,!!sym(DOSEinputvar),exptile)
    }
  }
  
  if(color_fill != DOSEinputvar) {
    if (exptilegroupvar =="none") {
      percentineachbreakcategory <- data.long |>
        dplyr::filter(!!sym(DOSEinputvar)!= dose_plac_value) |>
        dplyr::select(  !!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2,!!sym(color_fill),color_fill2,
                        expvalue,exptile)|>
        dplyr::group_by( !!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2,!!sym(color_fill),color_fill2) |> 
        dplyr::mutate(Ntot = dplyr::n())|> 
        dplyr::group_by( !!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2,exptile,!!sym(color_fill),color_fill2) |> 
        dplyr::mutate(Ncat = dplyr::n(),xmed=median(expvalue))|> 
        dplyr::mutate(percentage=Ncat/Ntot)|> 
        dplyr::distinct(expname,!!sym(DOSEinputvar),exptile,xmed,percentage,keynumeric,DOSE2,!!sym(color_fill),color_fill2)|> 
        dplyr::arrange(expname,!!sym(DOSEinputvar),!!sym(color_fill),color_fill2,exptile)
    }
    if (exptilegroupvar !="none") {
      percentineachbreakcategory <- data.long |>
        dplyr::filter(!!sym(DOSEinputvar)!= dose_plac_value) |>
        dplyr::select(  !!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2,!!sym(color_fill),color_fill2,!!sym(exptilegroupvar),
                        expvalue,exptile)|>
        dplyr::group_by( !!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2,!!sym(color_fill),color_fill2,!!sym(exptilegroupvar)) |> 
        dplyr::mutate(Ntot = dplyr::n())|>
        dplyr::group_by( !!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2,exptile,!!sym(color_fill),color_fill2,!!sym(exptilegroupvar)) |>  
        dplyr::mutate(Ncat = dplyr::n(),xmed=median(expvalue))|> 
        dplyr::mutate(percentage=Ncat/Ntot)|> 
        dplyr::distinct(expname,!!sym(DOSEinputvar),exptile,xmed,percentage,keynumeric,DOSE2,!!sym(color_fill),color_fill2,!!sym(exptilegroupvar))|> 
        dplyr::arrange(expname,!!sym(DOSEinputvar),!!sym(color_fill),color_fill2,exptile)
    }
  }
  
  facet_formula <- if (is.null(facet_formula) ) stats::as.formula( paste(endpointinputvar,"~","expname")) else
    stats::as.formula(facet_formula)

  p1 <-  ggplot2::ggplot(data.long,
                         ggplot2::aes_string("expvalue", responseinputvar))+
    ggplot2::facet_grid(facet_formula, scales = "free")

  if(points_show){
    p1p <- p1 + 
      ggplot2::geom_point(ggplot2::aes_string(col = color_fill),
                          alpha = points_alpha,
                          position = ggplot2::position_jitter(width = 0 , height = 0.025))+
      ggplot2::geom_hline(yintercept = c(0,1)) 
  }
  if(!points_show){
    p1p <- p1 + 
      ggplot2::geom_hline(yintercept = c(0,1)) 
    }
    
  if(binlimits_show){
    if(logistic_by_color_fill){
      xintercepts <- xintercepts %>% 
        dplyr::distinct(expname,intercept,quant)
    }
    p1pl <- p1p  +
    ggplot2::geom_vline(data = xintercepts, ggplot2::aes(xintercept = intercept),
                        color = binlimits_color, alpha = 0.5 )
  }
  if(!binlimits_show){
    p1pl <- p1p 
  }
  
 if (model_type != "none") {
  model_type_method <- "glm"
  filltext    <- "Logistic Fit 95% CI"
  linetypetext <- "Logistic Fit 95% CI"
  
    if(!logistic_by_color_fill) {
     p1lo <- p1pl +
    ggplot2::geom_ribbon(data = data.long |>
                         dplyr::mutate( DOSEinputvar := NULL, DOSE2 = NULL, exptile = NULL, color_fill := NULL, color_fill2 = NULL),
                         stat="smooth",
                         method = model_type_method,
                         method.args = list(family = "binomial"), 
                         color="transparent", linetype = 0, alpha = 0.3,
                         ggplot2::aes(fill = filltext))+
    ggplot2::geom_line(data = data.long |>
                       dplyr::mutate( DOSEinputvar := NULL, DOSE2 = NULL, exptile = NULL, color_fill := NULL, color_fill2 = NULL),
                        stat="smooth",
                        method = model_type_method, method.args = list(family = "binomial"), 
                        color="black", alpha = 0.5,
                       ggplot2::aes(linetype = linetypetext))
    }
  if(logistic_by_color_fill) {
    p1lo <- p1pl +
    ggplot2::geom_ribbon(data = data.long,
                         stat="smooth",
                         method = model_type_method, method.args = list(family = "binomial"), 
                         color="transparent", linetype = 0, alpha = 0.3,
                        ggplot2::aes_string(fill = color_fill))+
    ggplot2::geom_line(data = data.long,
                        stat="smooth",
                        method = model_type_method, method.args = list(family = "binomial"), 
                         alpha = 0.5,
                       ggplot2::aes_string(linetype = color_fill,
                                           color=color_fill))
  }
  }
  if(model_type == "none"){
    p1lo <- p1pl 
  }

    if(proj_bydose){
      p1proj <- p1lo +
        ggplot2::geom_line(data = predict_by_endpoint_expname_dose,
                           ggplot2::aes_string(y = "yhat", col = color_fill),
                           alpha = 0.4, linewidth = 2)+
        ggplot2::geom_line(data = predict_by_endpoint_expname_dose2,
                           ggplot2::aes_string(y = "yhat", col = color_fill),
                           alpha = 0.4, linewidth = 2.5)+
        ggplot2::geom_point(data = predict_by_endpoint_expname,
                            ggplot2::aes_string(x = "medexp", y = "ymid50", col = color_fill),
                            alpha = 0.4, size = 5)
    }
  if(!proj_bydose){
    p1proj <- p1lo
  }
  
  if(exposure_distribution=="lineranges") {
    lineranges_ypos <- as.character(lineranges_ypos)
    p1l <- p1proj +
      ggplot2::geom_linerange(data = data.long.summaries.dose, linewidth = 2, alpha = 0.4,
                              ggplot2::aes_string(xmin = "quant_10", xmax = "quant_90",y = lineranges_ypos,
                                                  col = color_fill,
                                                  group=   paste0("interaction(",paste(as.character(c(DOSEinputvar,color_fill)) ,collapse=",",sep=""),")")
                              ),
      position = ggstance::position_dodgev(height = lineranges_dodge),inherit.aes = FALSE)+
      ggplot2::geom_linerange(data = data.long.summaries.dose, linewidth = 2.5, alpha = 0.4,
                              ggplot2::aes_string(xmin = "quant_25", xmax=  "quant_75", y = lineranges_ypos, 
                                                  col = color_fill,
                                                  group=   paste0("interaction(",paste(as.character(c(DOSEinputvar,color_fill)) ,collapse=",",sep=""),")")
                              ),
                              position = ggstance::position_dodgev(height = lineranges_dodge), inherit.aes = FALSE)+
      ggplot2::geom_point(data=data.long.summaries.dose, size = 5, alpha = 0.2,
                          ggplot2::aes_string(x="medexp",y = lineranges_ypos,
                                              col = color_fill,
                                              group=   paste0("interaction(",paste(as.character(c(DOSEinputvar,color_fill)) ,collapse=",",sep=""),")")
                          ),
                          position = ggstance::position_dodgev(height = lineranges_dodge))

    if(lineranges_doselabel){
      p1lt <-  p1l +
        ggplot2::geom_text(data=data.long.summaries.dose, vjust = 1, size = 5, show.legend = FALSE,
                           ggplot2::aes_string(x="quant_90",y = lineranges_ypos,
                                               label = "DOSE",
                                               col = color_fill,
                                               group=   paste0("interaction(",paste(as.character(c(DOSEinputvar,color_fill)) ,collapse=",",sep=""),")")
                           ),
                           position = ggstance::position_dodgev(height = lineranges_dodge))
    }
    if(!lineranges_doselabel){
      p1lt <- p1l
    }
      
  }
  if(exposure_distribution!="lineranges") {
    p1lt <- p1proj 
  }

if(!prob_obs_byexptile_plac){
    data.long.summaries.exposure[data.long.summaries.exposure[,"meanexp"]==exposure_metric_plac_value,"prob"] <- NA
    data.long.summaries.exposure[data.long.summaries.exposure[,"meanexp"]==exposure_metric_plac_value,"N"] <- NA
    data.long.summaries.exposure[data.long.summaries.exposure[,"meanexp"]==exposure_metric_plac_value,"Ntot"] <- NA
  }  
  if(!logistic_by_color_fill){
    if (prob_obs_byexptile){
      p2e <- p1lt +
      ggplot2::geom_pointrange(data = data.long.summaries.exposure,
                               size = 1, alpha = 0.5,
                               ggplot2::aes(shape = paste("Observed probability by exposure split:",
                                                            exposure_metric_split),
                                              x = medexp,
                                              y = prob,
                                            ymin = meanresprlower,
                                            ymax = meanresprupper))
    }
    if (!prob_obs_byexptile){
      p2e <- p1lt
    }
  }
  if(logistic_by_color_fill){
    data.long.summaries.exposure <- data.long.summaries.exposure %>% 
      dplyr::mutate({{endpointinputvar}} := !!sym(endpointinputvar))
    
  if (prob_obs_byexptile){
    if(exptilegroupvar!="none"){
      p2e <- p1lt +
        ggplot2::geom_pointrange(data = data.long.summaries.exposure,
                                 size = 1, alpha = 0.5,
                                 ggplot2::aes(shape = paste("Observed probability by exposure split:",
                                                            exposure_metric_split),
                                            x = medexp,
                                            y = prob,
                                            ymin = meanresprlower,
                                            ymax = meanresprupper,
                                            col = !!sym(exptilegroupvar)))
    }
    if(exptilegroupvar=="none"){
      data.long.summaries.exposure <- data.long.summaries.exposure %>% 
        dplyr::mutate({{endpointinputvar}} := !!sym(endpointinputvar))
      p2e <- p1lt +
        ggplot2::geom_pointrange(data = data.long.summaries.exposure,
                                 size = 1, alpha = 0.5,
                                 ggplot2::aes(shape = paste("Observed probability by exposure split:",
                                                            exposure_metric_split),
                                            x = medexp,
                                            y = prob,
                                            ymin = meanresprlower,
                                            ymax = meanresprupper,
                                            col = !!sym(endpointinputvar)))
    }
  }
    if (!prob_obs_byexptile){
      p2e <- p1lt 
    }
  }
  
  if(prob_obs_bydose){
    data.long.summaries.dose.plot <- as.data.frame(data.long.summaries.dose)
    if(!prob_obs_bydose_plac){
    data.long.summaries.dose.plot[data.long.summaries.dose.plot[,DOSEinputvar]==dose_plac_value,"N"] <- NA
    data.long.summaries.dose.plot[data.long.summaries.dose.plot[,DOSEinputvar]==dose_plac_value,"Ntot"] <- NA
    data.long.summaries.dose.plot[data.long.summaries.dose.plot[,DOSEinputvar]==dose_plac_value,"prob"] <- NA
    }

    if(Nresp_Ntot_show){
      p2d <- p2e +
        ggplot2::geom_pointrange(data = data.long.summaries.dose.plot,
         alpha = 0.5, size = 1,
         ggplot2::aes(
         x = medexp,
         y = prob,
         ymin = meanresprlower,
         ymax = meanresprupper,
         col = !!sym(color_fill),
         shape = paste("Observed probability by",colorinputvar,"split")))
      
      if(Nresp_Ntot_ypos[2] == "with percentages"){
        p2dntot<- p2d +
          ggrepel::geom_text_repel(data=data.long.summaries.dose.plot %>% 
                                dplyr::filter(!is.na(N),!is.na(Ntot)),
                                vjust = 1,
                                size = prob_text_size,
                                show.legend = FALSE,
                             ggplot2::aes(x = medexp,
                                          y = prob*0.85,                                        ,
                                          col = !!sym(color_fill),
                                          label = paste(
                                            paste(100*round(prob,2),"%",sep=""),
                                                        "\n",N,Nresp_Ntot_sep,Ntot,sep="")
                             ))
      }
      if(Nresp_Ntot_ypos[2] == "top"){
        p2dntot<- p2d +
          ggrepel::geom_text_repel(data=data.long.summaries.dose.plot %>% 
                                dplyr::filter(!is.na(N),!is.na(Ntot)),
                             vjust = 1,
                             size = prob_text_size, show.legend = FALSE,
                             ggplot2::aes(x = medexp, y = prob*0.85,                                        ,
                                          col = !!sym(color_fill),
                                          label = paste(100*round(prob,2),"%",sep="")
                             ))+
          ggrepel::geom_text_repel(data=data.long.summaries.dose.plot %>% 
                                dplyr::filter(!is.na(N),!is.na(Ntot)),
                             vjust = 1,
                             size = prob_text_size, show.legend = FALSE,
                             ggplot2::aes(x = medexp, y = Inf,                                        ,
                                          col = !!sym(color_fill),
                                          label = paste(N,Nresp_Ntot_sep,Ntot,sep="")
                             ))
      }
        if(Nresp_Ntot_ypos[2] == "bottom"){
          p2dntot <- p2d +
            ggrepel::geom_text_repel(data=data.long.summaries.dose.plot %>% 
                                 dplyr::filter(!is.na(N),!is.na(Ntot)),
                               vjust = 1,
                               size = prob_text_size, show.legend = FALSE,
                               ggplot2::aes(x = medexp, y = prob*0.85,                                        ,
                                            col = !!sym(color_fill),
                                            label = paste(100*round(prob,2),"%",sep="")
                               ))+
            ggrepel::geom_text_repel(data=data.long.summaries.dose.plot %>% 
                                 dplyr::filter(!is.na(N),!is.na(Ntot)),
                               vjust = 1,
                               size = prob_text_size, show.legend = FALSE,
                               ggplot2::aes(x = medexp, y = 0.05,                                        ,
                                            col = !!sym(color_fill),
                                            label = paste(N,Nresp_Ntot_sep,Ntot,sep="")
                               ))
      }

    }
    if(!Nresp_Ntot_show){
      p2dntot <- p2e +
        ggplot2::geom_pointrange(data = data.long.summaries.dose.plot, alpha = 0.5, size = 1,
                                 ggplot2::aes(x = medexp, y = prob, col = !!sym(color_fill),
                                              ymin = meanresprlower,
                                              ymax = meanresprupper,
                                              shape = paste("Observed probability by",colorinputvar,"split")))+
        ggplot2::geom_text(data=data.long.summaries.dose.plot,
                           vjust = 1,
                           size = prob_text_size, show.legend = FALSE,
                           ggplot2::aes(x = medexp,y = prob*0.85,
                                        col = !!sym(color_fill),
                                        label = paste(100*round(prob,2),"%",sep="")
                           ))
    }
    
  }
  
  if(!prob_obs_bydose){
    p2dntot <- p2e
  }
    
  if(prob_obs_byexptile && !logistic_by_color_fill) {
    if(Nresp_Ntot_show){
      if(Nresp_Ntot_ypos[1] == "with percentages"){
      p2 <- p2dntot +
        ggrepel::geom_text_repel(data=data.long.summaries.exposure,
                           vjust = 1,
                           size = prob_text_size, show.legend = FALSE,
                           ggplot2::aes(x = medexp,
                                        y = prob*1.05,
                                        label = paste(paste(100*round(prob,2),"%",sep=""),
                                                      "\n",ifelse(is.na(N),"",N),
                                                      Nresp_Ntot_sep,
                                                      ifelse(is.na(Ntot),"",Ntot),
                                                      sep="")
                           ))
      }
      if(Nresp_Ntot_ypos[1] == "top"){
        p2 <- p2dntot +
          ggrepel::geom_text_repel(data=data.long.summaries.exposure,
                             vjust = 1,
                             size = prob_text_size, show.legend = FALSE,
                             ggplot2::aes(x = medexp, y = prob*1.05,                                        ,
                                          label = paste(100*round(prob,2),"%",sep="")
                             ))+
          ggrepel::geom_text_repel(data=data.long.summaries.exposure %>% 
                               dplyr::filter(!is.na(N),!is.na(Ntot)),
                               vjust = 1,
                             size = prob_text_size, show.legend = FALSE,
                             ggplot2::aes(x = medexp, y = Inf,
                                          label = paste(N,Nresp_Ntot_sep,Ntot,sep="")
                             ))
      }
      if(Nresp_Ntot_ypos[1] == "bottom"){
        p2 <- p2dntot +
          ggrepel::geom_text_repel(data=data.long.summaries.exposure,
                             vjust = 1,
                             size = prob_text_size, show.legend = FALSE,
                             ggplot2::aes(x = medexp, y = prob*1.05,                                        ,
                                          label = paste(100*round(prob,2),"%",sep="")
                             ))+
          ggrepel::geom_text_repel(data=data.long.summaries.exposure,
                             vjust = 1,
                             size = prob_text_size, show.legend = FALSE,
                             ggplot2::aes(x = medexp, y = 0.05,
                                          label = paste(N,Nresp_Ntot_sep,Ntot,sep="")
                             ))
      }
    }
    
    if(!Nresp_Ntot_show){
      p2 <- p2dntot +
        ggplot2::geom_text(data=data.long.summaries.exposure,
                           vjust = 1,
                           size = prob_text_size, show.legend = FALSE,
                           ggplot2::aes(x = as.double(as.character(medexp)),
                                        y = prob*1.1,
                                        label = paste(100*round(prob,2),"%",sep="")
                           ))
    }
  }
  
  if(prob_obs_byexptile && logistic_by_color_fill) {
    if(Nresp_Ntot_show){
      if(Nresp_Ntot_ypos[1] == "with percentages"){
      p2 <-   p2dntot +
        ggrepel::geom_text_repel(data=data.long.summaries.exposure %>% 
                             dplyr::mutate({{endpointinputvar}} :=!!sym(endpointinputvar)),
                           vjust = 1,
                           size = prob_text_size, show.legend = FALSE,
                           ggplot2::aes(x = medexp,y = prob*1.15,
                                        col= .data[[endpointinputvar]],
                                        label = paste(paste(100*round(prob,2),"%",sep=""),
                                                      "\n",N,Nresp_Ntot_sep,Ntot,sep="")
                           ))
      }
      if(Nresp_Ntot_ypos[1] == "top"){
        p2 <- p2dntot +
          ggplot2::geom_text(data=data.long.summaries.exposure,
                             vjust = 1,
                             size = prob_text_size, show.legend = FALSE,
                             ggplot2::aes(x = medexp, y = prob*1.15,  
                                          col= .data[[endpointinputvar]],
                                          label = paste(100*round(prob,2),"%",sep="")
                             ))+
          ggplot2::geom_text(data=data.long.summaries.exposure,
                             vjust = 1,
                             size = prob_text_size, show.legend = FALSE,
                             ggplot2::aes(x = medexp, y = Inf,
                                          col= .data[[endpointinputvar]],
                                          label = paste(N,Nresp_Ntot_sep,Ntot,sep="")
                             ))
      }
      if(Nresp_Ntot_ypos[1] == "bottom"){
        p2 <- p2dntot +
          ggrepel::geom_text_repel(data=data.long.summaries.exposure,
                             vjust = 1,
                             size = prob_text_size, show.legend = FALSE,
                             ggplot2::aes(col= .data[[endpointinputvar]],
                                          x = medexp,
                                          y = prob*1.15,  
                                          label = paste(100*round(prob,2),"%",sep="")
                             ))+
          ggrepel::geom_text_repel(data=data.long.summaries.exposure,
                             vjust = 1,
                             size = prob_text_size, show.legend = FALSE,
                             ggplot2::aes(x   = medexp, y = 0.05,
                                          col = .data[[endpointinputvar]],
                                          label = paste(N,Nresp_Ntot_sep,Ntot,sep="")
                             ))
      }
    }
    if(!Nresp_Ntot_show){
      p2 <-   p2dntot +
        ggrepel::geom_text_repel(data=data.long.summaries.exposure %>% 
                             dplyr::mutate({{endpointinputvar}} :=!!sym(endpointinputvar)),
                           vjust = 1,
                           size = prob_text_size, show.legend = FALSE,
                           ggplot2::aes(x = as.double(as.character(medexp)),
                                        y = prob*1.1,
                                        col= .data[[endpointinputvar]],
                                        label = paste(100*round(prob,2),"%",sep="")
                           ))
    }
  }

  if(!prob_obs_byexptile) {
    p2 <- p2dntot
  }
   if(binlimits_show){
     if(logistic_by_color_fill){
       xintercepts <- xintercepts %>% 
         dplyr::distinct(expname,intercept,quant)
     }
     p2t <- p2 +
       ggrepel::geom_text_repel(data = xintercepts,
                          ggplot2::aes(label=round(intercept,1), 
                                       x = intercept, y = binlimits_ypos) ,
                          vjust = 0, size = binlimits_text_size,color = binlimits_color,
                          direction = "y")
   }
    if(!binlimits_show){
      p2t <- p2 
    }

  if(exposure_distribution%in%c("distributions","boxplots")) {
    data.long.ridges <- data.long 
    data.long.ridges[data.long.ridges[,DOSEinputvar]==dose_plac_value,"expvalue"] <- NA
    if(exposure_distribution%in%c("distributions")){
      p2d <- p2t +
        ggridges::geom_density_ridges(data = data.long.ridges,
                                      ggplot2::aes(x = expvalue, y = keynumeric,
                                                   group = interaction(!!sym(color_fill),!!sym(DOSEinputvar)),
                                                   col = !!sym(color_fill),
                                                   height = ggplot2::after_stat(ndensity)),
                                      rel_min_height = 0.05, alpha = 0.1, scale = dist_scale,
                                      quantile_lines = TRUE, quantiles = c(0.1,0.25, 0.5, 0.75,0.9)) 
    }
    if(exposure_distribution%in%c("boxplots")){
      p2d <- p2t +
        ggplot2::geom_boxplot(data = data.long.ridges,
                                      ggplot2::aes(x = expvalue, y = keynumeric,
                                                   group = interaction(!!sym(color_fill),!!sym(DOSEinputvar)),
                                                   col = !!sym(color_fill)),
                     alpha = 0.1, orientation="y", width = dist_position_scaler*0.90) 
    }
    
if(!exposure_distribution_percent){
    p2dn <- p2d
    }
    if(exposure_distribution_percent){
    p2dn <- p2d +
        ggrepel::geom_label_repel(data = percentineachbreakcategory,
                                  ggplot2::aes(color = !!rlang::sym(color_fill),
                                               group = interaction(!!sym(color_fill),!!sym(DOSEinputvar)),
                                               y = keynumeric, x= xmed, label = round(100*percentage,0) ),
                                  size= exposure_distribution_percent_text_size,
                                  alpha = 0.5, show.legend = FALSE)
    }
  }
  if(!exposure_distribution%in%c("distributions","boxplots")) {
    p2dn <- p2t
  }
  if(yproj) {
    yproj_xpos <- as.character(yproj_xpos)
    
    p2df <- p2dn +
      ggplot2::geom_linerange(data = predict_by_endpoint_expname, alpha = 0.4, linewidth = 2,
                              ggplot2::aes_string(x = yproj_xpos, ymin = "ymid10", ymax = "ymid90",
                                                  col = color_fill,
                                                  group=   paste0("interaction(",paste(as.character(c(DOSEinputvar,color_fill)) ,collapse=",",sep=""),")")
                              ),
                              position = ggplot2::position_dodge(width = yproj_dodge), inherit.aes = FALSE)+
      ggplot2::geom_linerange(data = predict_by_endpoint_expname, alpha = 0.4, linewidth = 2.5,
                              ggplot2::aes_string(x = yproj_xpos, ymin = "ymid25", ymax = "ymid75", col= color_fill,
                                                  group=   paste0("interaction(",paste(as.character(c(DOSEinputvar,color_fill)) ,collapse=",",sep=""),")")
                              ),
                              position = ggplot2::position_dodge(width = yproj_dodge), inherit.aes = FALSE)+
      ggplot2::geom_point(data=predict_by_endpoint_expname, alpha = 0.4, size = 3,
                          ggplot2::aes_string(x = yproj_xpos, y = "ymid50", color = color_fill,
                                              group=   paste0("interaction(",paste(as.character(c(DOSEinputvar,color_fill)) ,collapse=",",sep=""),")")
                              ),
                              position = ggplot2::position_dodge(width = yproj_dodge), inherit.aes = FALSE)
  }
  if(!yproj) {
    p2df <- p2dn 
  }
  if(exposure_distribution %in%c("distributions","boxplots")){
    
    if( length (levels(data.long[DOSEinputvar] |> dplyr::pull() )) ==
        length (sort(unique(data.long$keynumeric)))
    ){
      p2df2 <- p2df +
        ggplot2::scale_y_continuous(position = yaxis_position,
                                    breaks =c(sort(unique(data.long$keynumeric)),
                                              c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) ), 
                                    labels= c(levels(data.long[DOSEinputvar] |> dplyr::pull() ),
                                              c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1") ),
                                    expand = ggplot2::expansion(mult=c(0.01,0.01), add =c(0, 0)))
    }
    if( length (levels(data.long[DOSEinputvar] |> dplyr::pull() )) !=
        length (sort(unique(data.long$keynumeric)))
    ){
      p2df2 <- p2df +
        ggplot2::scale_y_continuous(position = yaxis_position,
                                    breaks =c(
                                              c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) ), 
                                    labels= c(
                                              c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1") ),
                                    expand = ggplot2::expansion(mult=c(0.01,0.01), add =c(0, 0)))
    }
    
  }
  if(exposure_distribution =="lineranges"){
    p2df2 <- p2df +
      ggplot2::scale_y_continuous(position = yaxis_position,
                                  breaks =c(
                                    c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) ), 
                                  labels= c(
                                    c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1") ),
                                  expand = ggplot2::expansion(mult=c(0.01,0.01), add =c(0, 0)))
  }
  if(exposure_distribution =="none"){
    p2df2 <- p2df
  }

  pf1 <- p2df2 +
    ggplot2::labs(fill="", linetype="", shape="", x = xlab, y = ylab) +
    ggplot2::theme_bw(base_size = 18)+
    ggplot2::theme(legend.position = "top",strip.placement = "outside")
  
  if(!theme_certara && !logistic_by_color_fill){
    pf <-  pf1 +
      ggplot2::scale_colour_manual( values = tableau10,drop=FALSE,na.value = "grey50")+
      ggplot2::scale_fill_manual(   values = c("gray80",tableau10),drop=FALSE,na.value = "grey50")
  }
  if( theme_certara && !logistic_by_color_fill){
    pf <-  pf1 +
      ggplot2::scale_colour_manual(values = c( "#4682AC","#FDBB2F","#EE3124" ,"#336343","#7059a6", "#803333"),
                                   drop=FALSE,na.value = "grey50")+
      ggplot2::scale_fill_manual(  values = c("gray80","#4682AC","#FDBB2F","#EE3124" ,"#336343","#7059a6", "#803333"),
                                   drop=FALSE,na.value = "grey50")+
      ggplot2::theme(strip.background = ggplot2::element_rect(fill="#475c6b"),
                     strip.text =  ggplot2::element_text(face = "bold",color = "white"))
    
  }
  
  if(!theme_certara && logistic_by_color_fill){
    pf <-  pf1 +
      ggplot2::scale_colour_manual( values = tableau10,drop=FALSE,na.value = "grey50")+
      ggplot2::scale_fill_manual(   values = c(tableau10),drop=FALSE,na.value = "grey50")
  }
  if( theme_certara && logistic_by_color_fill){
    pf <-  pf1 +
      ggplot2::scale_colour_manual(values = c("#4682AC","#FDBB2F","#EE3124" ,"#336343","#7059a6", "#803333"),
                                   drop=FALSE,na.value = "grey50")+
      ggplot2::scale_fill_manual(  values = c("#4682AC","#FDBB2F","#EE3124" ,"#336343","#7059a6", "#803333"),
                                   drop=FALSE,na.value = "grey50")+
      ggplot2::theme(strip.background = ggplot2::element_rect(fill="#475c6b"),
                     strip.text =  ggplot2::element_text(face = "bold",color = "white"))
    
  }
  
  if(!return_list){
    pf }
  
  if(return_list){
    pf <-   list(data.long,xintercepts,
         data.long.summaries.dose,
         predict_by_endpoint_expname,
         predict_by_endpoint_expname_dose,
         predict_by_endpoint_expname_dose2,
         data.long.summaries.exposure,
         percentineachbreakcategory,pf)
  }
  pf
}

