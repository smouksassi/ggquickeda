#' @importFrom rlang `:=`
#' @importFrom rlang .data
#' @importFrom rlang sym
#' @import tidyr
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats sd

summary_df_cont <- function(x,y, probs = c(0.25,0.75,0.90,0.10)) {
  N = Ntot = prob = NULL
 tibble::tibble(
    minexp = min(x),
    maxexp = max(x),
    medexp = stats::median(x),
    meanexp = mean(x),
    Nmiss = length(x[is.na(x)]),
    Ntot = dplyr::n(),
    mean = mean(y,na.rm=TRUE),
    SE = sd(y,na.rm=TRUE)/sqrt(Ntot) ,
    values = stats::quantile(x, probs, na.rm = TRUE),
    quant = probs
  )
}
plogis <- function(x) exp(x)/(1+exp(x))

#' ggcontinuousexpdist
#'
#' Produces a logistic fit plot with a facettable exposures/quantiles/distributions in ggplot2
#' @param data Data to use with multiple endpoints stacked into Endpoint(endpoint name), response 0/1
#' @param response name of the column holding the valuesresponse 0/1
#' @param endpoint name of the column holding the name/key of the endpoint default to `Endpoint`
#' @param DOSE name of the column holding the DOSE values default to `DOSE`
#' @param exposure_metrics name(s) of the column(s) to be stacked into `expname` `exptile` and split into `exposure_metric_split`
#' @param exposure_metric_split one of "median", "tertile", "quartile", "none"
#' @param exposure_metric_soc_value  special exposure code for standard of care default -99 
#' @param exposure_metric_plac_value special exposure code for placebo default 0
#' @param exposure_distribution one of distributions, lineranges or none
#' @param dose_plac_value string identifying placebo in DOSE column
#' @param xlab text to be used as x axis label
#' @param ylab text to be used as y axis label
#' @param mean_text_size mean text size default to 5
#' @param mean_obs_bydose observed mean by dose TRUE/FALSE
#' @param N_text_size N responders/Ntotal by exposure bin text size default to 5
#' @param binlimits_text_size 5 binlimits text size
#' @param binlimits_ypos binlimits y position default to 0 
#' @param binlimits_color binlimits text color default to "gray70"
#' @param dist_position_scaler space occupied by the distribution default to 0.2 
#' @param dist_offset offset where the distribution position starts 0
#' @param lineranges_ypos where to put the lineranges -1
#' @param lineranges_dodge lineranges vertical dodge value 1
#' @param yproj project the probabilities on y axis TRUE/FALSE
#' @param yproj_xpos y projection x position 0
#' @param yproj_dodge  y projection dodge value 0.2
#' @param yaxis_position where to put y axis "left" or "right"
#' @param facet_formula facet formula to be use otherwise endpoint ~ expname
#' @param theme_certara apply certara colors and format for strips and default colour/fill
#' @examples
#' # Example 1
#' library(ggplot2)
#' library(patchwork)
#' effICGI <- logistic_data |>
#' dplyr::filter(!is.na(ICGI7))|>
#' dplyr::filter(!is.na(AUC))
#'effICGI$DOSE <- factor(effICGI$DOSE,
#'                       levels=c("0", "600", "1200","1800","2400"),
#'                       labels=c("Placebo", "600 mg", "1200 mg","1800 mg","2400 mg"))
#'effICGI$STUDY <- factor(effICGI$STUDY)    
#'effICGI <- tidyr::gather(effICGI,Endpoint,response,ICGI7,BRLS)
#' a <- ggcontinuousexpdist(data = effICGI |> dplyr::filter(Endpoint =="ICGI7"),
#'                  response = "response",
#'                  endpoint = "Endpoint",
#'                  exposure_metrics = c("AUC"),
#'                  exposure_metric_split = c("quartile"),
#'                  exposure_metric_soc_value = -99,
#'                  exposure_metric_plac_value = 0,
#'                  dist_position_scaler = 1, dist_offset = -1 ,
#'                  yproj_xpos =  -20 ,
#'                  yproj_dodge = 20 ,
#'                  exposure_distribution ="distributions")
#'
#' b <- ggcontinuousexpdist(data = effICGI |> dplyr::filter(Endpoint =="BRLS"),
#'                  response = "response",
#'                  endpoint = "Endpoint",
#'                  exposure_metrics = c("AUC"),
#'                  exposure_metric_split = c("quartile"),
#'                  exposure_metric_soc_value = -99,
#'                  exposure_metric_plac_value = 0,
#'                  dist_position_scaler = 4.2, dist_offset = 5 ,
#'                  yproj_xpos =  -20 ,
#'                  yproj_dodge = 20 ,
#'                  exposure_distribution ="distributions")            
#' a / b +
#'plot_layout(guides = "collect") &
#'  theme(legend.position = "top")
#'\dontrun{
#' #Example 5
#'}
#' @export               
ggcontinuousexpdist <- function(data = effICGI, 
                              response = "response",
                              endpoint = "Endpoint",
                              DOSE = "DOSE",
                              exposure_metrics = c("AUC","CMAX"),
                              exposure_metric_split = c("median","tertile","quartile","none"),
                              exposure_metric_soc_value = -99,
                              exposure_metric_plac_value = 0,
                              exposure_distribution = c("distributions","lineranges","none"),
                              dose_plac_value = "Placebo",
                              xlab = "Exposure Values",
                              ylab ="Probability of Response",
                              mean_text_size = 5,
                              mean_obs_bydose = TRUE,
                              N_text_size = 5,
                              binlimits_text_size = 5,
                              binlimits_ypos = -Inf,
                              binlimits_color= "gray70",
                              dist_position_scaler = 0.2,
                              dist_offset = 0,
                              lineranges_ypos = -1,
                              lineranges_dodge = 1,
                              yproj = TRUE,
                              yproj_xpos = 0,
                              yproj_dodge = 0.2,
                              yaxis_position = c("left","right"),
                              facet_formula = NULL,
                              theme_certara = TRUE
) {
  
  responseinputvar  <-  response
  endpointinputvar  <- endpoint
  DOSEinputvar  <- DOSE 
  exposure_metric_split <- match.arg(exposure_metric_split, several.ok = FALSE)
  exposure_distribution <- match.arg(exposure_distribution, several.ok = FALSE)
  yaxis_position <- match.arg(yaxis_position, several.ok = FALSE)
 
  effICGI = expname = expvalue = DOSE2 = quant = values = Ncat = Ntot = xmed = percentage = exptile = keynumeric = NULL
  intercept = medexp = prob = SE = N = ndensity = Endpoint = NULL
  
  data <- data |> 
    dplyr::mutate(none = "(all)")  # needed when no metric are chosen
  
  data.long <- data |> 
    tidyr::gather(expname,expvalue,!!!exposure_metrics, factor_key = TRUE) |> 
    dplyr::group_by(expname,!!sym(endpointinputvar) ) 
  
  if(exposure_metric_split=="none") {
    data.long <- data.long |> 
      dplyr::mutate(exptile = dplyr::case_when(
        expvalue == exposure_metric_soc_value  ~ NA,
        expvalue == exposure_metric_plac_value ~ NA,
        expvalue  > exposure_metric_plac_value ~ expvalue))
    data.long$keynumeric<- - dist_position_scaler* as.numeric(forcats::fct_rev(as.factor(dplyr::pull(data.long[,DOSEinputvar])))) + dist_offset
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

  data.long.summaries.dose <- data.long |>
    dplyr::group_by(!!sym(endpointinputvar),expname,!!sym(DOSEinputvar),DOSE2)|>
    dplyr::reframe(
      summary_df_cont(expvalue,!!sym(responseinputvar))) |> 
    tidyr::pivot_wider(names_from= quant,values_from = values,names_glue = "quant_{100*quant}")
  
  
  loopvariables <- unique(c(endpointinputvar,"expname"))
  data.long.summaries.dose <- tidyr::unite(data.long.summaries.dose,"loopvariable", !!!loopvariables, remove = FALSE)
  data.long <- tidyr::unite(data.long,"loopvariable", !!!loopvariables, remove = FALSE)
  
  olsfit_by_endpoint <- list()
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
    olsfit_by_endpoint_fit <- eval(bquote( rms::ols( as.formula(paste(responseinputvar,"~","expvalue")) , data=logisticregdata,x=TRUE,y=TRUE) ))
    olsfit_by_endpoint[[i]] <- olsfit_by_endpoint_fit
    
    data.long.summaries.dose.loop <- data.long.summaries.dose |>
      dplyr::filter(.data[["loopvariable"]] ==i)
    pred10exp <- as.data.frame(rms::Predict(olsfit_by_endpoint_fit,
                                            expvalue= data.long.summaries.dose.loop$quant_10))
    names(pred10exp)<- c("quant_10" ,  "ymid10",  "ylow10", "yup10")
    pred10exp$loopvariable <- i
    pred90exp <- as.data.frame(rms::Predict(olsfit_by_endpoint_fit,
                                            expvalue= data.long.summaries.dose.loop$quant_90))
    names(pred90exp)<- c("quant_90" ,  "ymid90",  "ylow90", "yup90")
    pred90exp$loopvariable <- i
    pred25exp <- as.data.frame(rms::Predict(olsfit_by_endpoint_fit,
                                            expvalue= data.long.summaries.dose.loop$quant_25))
    names(pred25exp)<- c("quant_25" ,  "ymid25",  "ylow25", "yup25")
    pred25exp$loopvariable <- i 
    pred75exp <- as.data.frame(rms::Predict(olsfit_by_endpoint_fit,
                                            expvalue= data.long.summaries.dose.loop$quant_75))
    names(pred75exp)<- c("quant_75" ,  "ymid75",  "ylow75", "yup75")
    pred75exp$loopvariable <- i 
    pred50exp<- as.data.frame (rms::Predict(olsfit_by_endpoint_fit,
                                            expvalue=data.long.summaries.dose.loop$medexp))
    names(pred50exp)<- c("medexp" ,  "ymid50",  "ylow50", "yup50")
    pred50exp$loopvariable <- i 
    
    data.long.summaries.dose.loop<- dplyr::left_join(data.long.summaries.dose.loop,pred10exp)
    data.long.summaries.dose.loop<- dplyr::left_join(data.long.summaries.dose.loop,pred90exp)
    data.long.summaries.dose.loop<- dplyr::left_join(data.long.summaries.dose.loop,pred25exp)
    data.long.summaries.dose.loop<- dplyr::left_join(data.long.summaries.dose.loop,pred75exp)
    data.long.summaries.dose.loop<- dplyr::left_join(data.long.summaries.dose.loop,pred50exp)
    predict_by_endpoint_expname[[i]] <- data.long.summaries.dose.loop
    
    predictionsbydose<- data.long.summaries.dose.loop |>
      dplyr::group_by(!!sym(DOSEinputvar),!!sym(endpointinputvar),expname,DOSE2) |>
      dplyr::do(as.data.frame(rms::Predict(olsfit_by_endpoint_fit,
                                      expvalue=seq(.data$quant_10,.data$quant_90,0.1))))
    predict_by_endpoint_expname_dose[[i]] <- predictionsbydose
    
    predictionsbydose2<- data.long.summaries.dose.loop |>
      dplyr::group_by(!!sym(DOSEinputvar),!!sym(endpointinputvar),expname,DOSE2) |>
      dplyr::do(as.data.frame(rms::Predict(olsfit_by_endpoint_fit,
                                      expvalue=seq(.data$quant_25,.data$quant_75,0.1))))
    predict_by_endpoint_expname_dose2[[i]] <- predictionsbydose2
    
  }
  predict_by_endpoint_expname <- data.table::rbindlist(predict_by_endpoint_expname)
  predict_by_endpoint_expname_dose <- data.table::rbindlist(predict_by_endpoint_expname_dose)
  predict_by_endpoint_expname_dose2 <- data.table::rbindlist(predict_by_endpoint_expname_dose2)
  
  
  data.long.summaries.exposure <- data.long |>
    dplyr::ungroup()|>
    dplyr::group_by(!!sym(endpointinputvar),expname,exptile)|>
    dplyr::reframe(
      summary_df_cont(expvalue,!!sym(responseinputvar))) |> 
    tidyr::pivot_wider(names_from= quant,values_from =values,names_glue = "quant_{100*quant}")
  
  percentineachbreakcategory <- data.long |>
    dplyr::group_by(!!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,DOSE2)|>
    dplyr::select(!!sym(endpointinputvar),expname,!!sym(DOSEinputvar),keynumeric,expvalue,exptile,DOSE2)|>
    dplyr::group_by(!!sym(DOSEinputvar),keynumeric,expname,DOSE2) |> 
    dplyr::mutate(Ntot = dplyr::n())|> 
    dplyr::group_by(!!sym(DOSEinputvar),expname,exptile,keynumeric,DOSE2) |> 
    dplyr::mutate(Ncat = dplyr::n(),xmed=median(expvalue))|> 
    dplyr::mutate(percentage=Ncat/Ntot)|> 
    dplyr::distinct(!!sym(DOSEinputvar),xmed,exptile,expname,percentage,keynumeric,DOSE2)|> 
    dplyr::arrange(!!sym(DOSEinputvar))
  
  facet_formula <- if (is.null(facet_formula) ) stats::as.formula( paste(endpointinputvar,"~","expname")) else
    stats::as.formula(facet_formula)
  #facet_formula <- Endpoint ~ expname 
  
  p1 <-  ggplot2::ggplot(data.long,
                         ggplot2::aes_string("expvalue", responseinputvar))+
    ggplot2::facet_grid(facet_formula, scales = "free")+
    #facet_nested(Endpoint~expname+DOSE2,scales="free",margins = "DOSE2")+
    ggplot2::geom_point(ggplot2::aes_string(col = DOSEinputvar),
                        alpha = 0.2, position = ggplot2::position_jitter(width = 0 , height = 0.05))+
    #ggplot2::geom_hline(yintercept = c(0,1))+
    ggplot2::geom_vline(data = xintercepts, ggplot2::aes(xintercept = intercept), color = "gray70" )+
    ggplot2::geom_ribbon(data = data.long |> dplyr::mutate( DOSEinputvar := NULL, DOSE2 = NULL, exptile = NULL),
                         stat="smooth",
                         method = "glm", 
                         color="transparent",linetype=0, alpha = 0.5,
                         ggplot2::aes(fill = "Linear Fit 95% CI"))+
    ggplot2::geom_line(data = data.long |> dplyr::mutate( DOSEinputvar := NULL, DOSE2 = NULL, exptile = NULL),
                       stat="smooth",
                       method = "glm", 
                       color="black", alpha = 0.5,
                       ggplot2::aes(linetype = "Linear Fit 95% CI"))+
    ggplot2::geom_line(data = predict_by_endpoint_expname_dose,
                       ggplot2::aes_string(y = "yhat", col = DOSEinputvar),
                       alpha = 0.4, size = 2)+
    ggplot2::geom_line(data = predict_by_endpoint_expname_dose2,
                       ggplot2::aes_string(y = "yhat", col = DOSEinputvar),
                       alpha = 0.4, size = 2.5)+
    ggplot2::geom_point(data = predict_by_endpoint_expname,
                        ggplot2::aes_string(x = "medexp", y = "ymid50", col = DOSEinputvar),
                        alpha = 0.4, size = 5)
  
  
  if(exposure_distribution=="lineranges") {
    lineranges_ypos <- as.character(lineranges_ypos)
    p1l <- p1 +
      ggplot2::geom_linerange(data = data.long.summaries.dose, size = 2, alpha = 0.4,
                              ggplot2::aes_string(xmin = "quant_10", xmax = "quant_90",y = lineranges_ypos,
                                                  col = DOSEinputvar, group = DOSEinputvar),
                              position = ggstance::position_dodgev(height = lineranges_dodge),inherit.aes = FALSE)+
      ggplot2::geom_linerange(data = data.long.summaries.dose, size = 2.5, alpha = 0.4,
                              ggplot2::aes_string(xmin = "quant_25", xmax=  "quant_75", y = lineranges_ypos, 
                                                  col = DOSEinputvar, group = DOSEinputvar),
                              position = ggstance::position_dodgev(height = lineranges_dodge), inherit.aes = FALSE)+
      ggplot2::geom_point(data=data.long.summaries.dose, size = 5, alpha = 0.2,
                          ggplot2::aes_string(x="medexp",y = lineranges_ypos,
                                              col = DOSEinputvar),
                          position = ggstance::position_dodgev(height = 0.15))
  }
  if(exposure_distribution!="lineranges") {
    p1l <- p1 
  }
  
  p2e <- p1l +
    ggplot2::geom_pointrange(data = data.long.summaries.exposure, size = 1,
                             ggplot2::aes(shape = "Observed probability by exposure split",
                                          x = medexp, y = mean,
                                          ymin = mean+1.959*SE,
                                          ymax=mean-1.959*SE),
                             alpha = 0.5)
  if(mean_obs_bydose){
    data.long.summaries.dose.plot <- data.long.summaries.dose 
    data.long.summaries.dose.plot[data.long.summaries.dose.plot[,DOSEinputvar]==dose_plac_value,"Ntot"] <- NA
    data.long.summaries.dose.plot[data.long.summaries.dose.plot[,DOSEinputvar]==dose_plac_value,"mean"] <- NA
    
    p2d <- p2e +
      ggplot2::geom_pointrange(data = data.long.summaries.dose.plot, alpha = 0.5, size = 1,
                               ggplot2::aes(x = medexp, y = mean, col = !!sym(DOSEinputvar),
                                            ymin = mean+1.959*SE, ymax=mean-1.959*SE,
                                            shape = "Observed probability by dose split"),
                               show.legend = FALSE) +
      ggplot2::geom_text(data=data.long.summaries.dose.plot, vjust = 1, size = mean_text_size, show.legend = FALSE,
                         ggplot2::aes(x = medexp, y = mean, col = !!sym(DOSEinputvar),
                                      label = paste(
                                        paste("\n",round(mean,2),sep=""),"\n",
                                        Ntot,sep="")
                         ))
    
  }
  if(!mean_obs_bydose){
    p2d <- p2e
  }
  p2 <- p2d +
    ggplot2::geom_text(data=data.long.summaries.exposure, vjust = 0, size = mean_text_size, show.legend = FALSE,
                       ggplot2::aes(x = medexp, y = mean, label = paste(round(mean,2),"\n",sep="")))+
    ggplot2::geom_text(data = xintercepts, ggplot2::aes(label=round(intercept,1), x = intercept, y = binlimits_ypos) ,
                       vjust = 0, size = binlimits_text_size,color = binlimits_color)+
    ggplot2::geom_text(data = data.long.summaries.exposure, y = Inf, vjust = 1, size = N_text_size, 
                       ggplot2::aes(x = as.double(as.character(medexp)), label=paste(Ntot,sep="")))
  
  if(exposure_distribution=="distributions") {
    data.long.ridges <- data.long 
    data.long.ridges[data.long.ridges[,DOSEinputvar]==dose_plac_value,"expvalue"] <- NA
    p2d <- p2 +
      ggridges::geom_density_ridges(data = data.long.ridges,
                                    ggplot2::aes(x = expvalue, y = keynumeric,
                                                 group = !!rlang::sym(DOSEinputvar),
                                                 col = !!rlang::sym(DOSEinputvar),
                                        height = ggplot2::after_stat(ndensity)),
                                    rel_min_height = 0.05, alpha = 0.1, scale = 0.9,
                                    quantile_lines = TRUE, quantiles = c(0.1,0.25, 0.5, 0.75,0.9))+
      ggplot2::geom_label(data = percentineachbreakcategory,
                          ggplot2::aes(color = !!rlang::sym(DOSEinputvar), y = keynumeric, x= xmed, label = round(100*percentage,0) ),
                          alpha = 0.5, show.legend = FALSE)
  }
  if(exposure_distribution!="distributions") {
    p2d <- p2 
  }
  
  if(yproj) {
    yproj_xpos <- as.character(yproj_xpos)
    
    p2df <- p2d +
      ggplot2::geom_linerange(data = predict_by_endpoint_expname, alpha = 0.4, size = 2,
                              ggplot2::aes_string(x = yproj_xpos, ymin = "ymid10", ymax = "ymid90", col = DOSEinputvar, group = DOSEinputvar),
                              position = ggplot2::position_dodge(width = yproj_dodge), inherit.aes = FALSE)+
      ggplot2::geom_linerange(data = predict_by_endpoint_expname, alpha = 0.4, size = 2.5,
                              ggplot2::aes_string(x = yproj_xpos, ymin = "ymid25", ymax = "ymid75", col= DOSEinputvar, group= DOSEinputvar),
                              position = ggplot2::position_dodge(width = yproj_dodge), inherit.aes = FALSE)+
      ggplot2::geom_point(data=predict_by_endpoint_expname, alpha = 0.4, size = 3,
                          ggplot2::aes_string(x = yproj_xpos, y = "ymid50", col = DOSEinputvar),
                          position = ggplot2::position_dodge(width = yproj_dodge), inherit.aes = FALSE)
  }
  if(!yproj) {
    p2df <- p2d 
  }
  if(exposure_distribution =="distributions"){

    breaksendpoints<- data.long |> 
      dplyr::group_by(Endpoint) |>
      dplyr::reframe(breaks= pretty(response))
    
    p2df2 <- p2df +
      ggplot2::scale_y_continuous(position = yaxis_position,
                                  breaks = (c(breaksendpoints$breaks,unique(data.long$keynumeric))),
                                  labels = c(breaksendpoints$breaks,as.vector(unique(data.long$DOSE)) ),
                                  expand = ggplot2::expansion(mult=c(0.01,0.01), add =c(0, 0)))
    # p2df2 <- p2df +
    #   ggplot2::scale_y_continuous(position = yaxis_position,
    #                               breaks =c(unique(data.long$keynumeric) ), 
    #                               labels= c(as.vector(unique(data.long$DOSE)) ),
    #                               expand = ggplot2::expansion(mult=c(0.01,0.01), add =c(0, 0)))
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
  p2df2 +
    ggplot2::labs(fill="", linetype="", shape="", x = xlab, y = ylab) +
    ggplot2::theme_bw(base_size = 18)+
    ggplot2::theme(legend.position = "top",strip.placement = "outside",
                   axis.title.y = ggplot2::element_blank())+
    ggplot2::scale_colour_manual(values = c( "#4682AC","#FDBB2F","#EE3124" ,"#336343","#7059a6", "#803333"),
                                 drop=FALSE,na.value = "grey50")+
    ggplot2::scale_fill_manual(  values = c("gray80",
                                            "#4682AC","#FDBB2F","#EE3124" ,"#336343","#7059a6", "#803333"),
                                 drop=FALSE,na.value = "grey50")+
    ggplot2::theme(strip.background = ggplot2::element_rect(fill="#475c6b"),
                   strip.text =  ggplot2::element_text(face = "bold",color = "white"))
  
}

