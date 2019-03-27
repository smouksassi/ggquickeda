# Code taken from the https://github.com/sachsmc/ggkm/ R package commit ce24953df839d27e4576ac1cde1df05a61698417

#' Display Kaplan Meier Curve
#'
#' Kaplan Meier Curve
#'
#' @export
#' @keywords internal
#' @rdname geom_km


GeomKm <- ggplot2::ggproto("GeomKm", ggplot2::Geom,
                           
                           draw_group = function(data, scales, coordinates, ...) {
                             
                             path <- transform(data, alpha = NA)
                             GeomPath$draw_panel(path, scales, coordinates)
                             
                           },
                           
                           required_aes = c("x", "y"),
                           default_aes = ggplot2::aes(colour="black", fill="grey60", size=.75,
                                                      linetype=1, weight=1, alpha = 1),
                           draw_key = ggplot2::draw_key_path
                           
                           
)


#' Display Kaplan Meier Curve
#'
#' Kaplan Meier Curve
#'
#' @export
#' @keywords internal
#' @rdname geom_kmband


GeomKmband <- ggplot2::ggproto("GeomKmband", ggplot2::Geom,
                               
                               draw_group = function(data, scales, coordinates, ...) {
                                 
                                 ribbon <- transform(data, colour = NA)
                                 path <- transform(data, alpha = NA)
                                 
                                 GeomRibbon$draw_group(ribbon, scales, coordinates)
                                 
                               },
                               
                               required_aes = c("x", "ymin", "ymax"),
                               default_aes = ggplot2::aes(colour="black", fill="grey60", size=.75,
                                                          linetype=1, weight=1, alpha=0.4),
                               
                               draw_key = ggplot2::draw_key_smooth
                               
)


#' Display tick marks on a Kaplan Meier curve
#'
#' Ticks are plotted at censoring times that are also not event times
#'
#' @export
#' @keywords internal
#' @rdname geom_kmticks

GeomKmticks <- ggplot2::ggproto("GeomKmticks", ggplot2::Geom,
                                
                                draw_group = function(data, scales, coordinates, ...) {
                                  
                                  showpoints <- data$n.censor > 0 & data$n.event == 0
                                  
                                  coordsp <- coordinates$transform(data, scales)[showpoints, , drop = FALSE]
                                  
                                  if(nrow(coordsp) == 0){
                                    grid::nullGrob()
                                  } else {
                                    grid::pointsGrob(
                                      coordsp$x, coordsp$y,
                                      pch = coordsp$shape,
                                      size = grid::unit(coordsp$size, "char"),
                                      gp = grid::gpar(
                                        col = coordsp$colour,
                                        fill = coordsp$fill,
                                        alpha = coordsp$alpha
                                      )
                                    )
                                  }
                                  
                                },
                                
                                required_aes = c("x", "y"),
                                non_missing_aes = c("size", "shape"),
                                default_aes = ggplot2::aes(
                                  shape = 3, colour = "black", size = .75,
                                  alpha = 1, stroke = 0.5, fill = "black"
                                ),
                                draw_key = ggplot2::draw_key_point
                                
)


#' Add a Kaplan-Meier survival curve
#'
#' @section Aesthetics:
#' \code{geom_km} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}} The survival/censoring times. This is automatically mapped by \link{stat_km}
#'   \item \strong{\code{y}} The survival probability estimates. This is automatically mapped by \link{stat_km}
#'   smallest level in sort order is assumed to be 0, with a warning
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @inheritParams ggplot2::geom_point
#' @seealso The default stat for this geom is \code{\link{stat_km}} see
#'   that documentation for more options to control the underlying statistical transformation.
#' @export
#' @keywords internal
#' @examples
#' library(ggplot2)
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(time = time, status = status, color = factor(sex))) + geom_km()

geom_km <- function(mapping = NULL, data = NULL, stat = "km",
                    position = "identity", show.legend = NA,
                    inherit.aes = TRUE, na.rm = TRUE, ...) {
  ggplot2::layer(
    geom = GeomKm, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' Add confidence bands to a Kaplan-Meier survival curve
#'
#' @section Aesthetics:
#' \code{geom_km} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}} The survival/censoring times. This is automatically mapped by \link{stat_km}
#'   \item \strong{\code{y}} The survival probability estimates. This is automatically mapped by \link{stat_km}
#'   smallest level in sort order is assumed to be 0, with a warning
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @inheritParams ggplot2::geom_point
#' @seealso The default stat for this geom is \code{\link{stat_kmband}} see
#'   that documentation for more options to control the underlying statistical transformation.
#' @export
#' @keywords internal
#' @examples
#' library(ggplot2)
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(time = time, status = status, color = factor(sex), fill =factor(sex))) + geom_km() + geom_kmband()

geom_kmband <- function(mapping = NULL, data = NULL, stat = "kmband",
                        position = "identity", show.legend = NA,
                        inherit.aes = TRUE, na.rm = TRUE, ...) {
  ggplot2::layer(
    geom = GeomKmband, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' Add tick marks to a Kaplan-Meier survival curve
#'
#' Adds tickmarks at the times when there are censored observations but no events
#'
#' @section Aesthetics:
#' \code{geom_kmticks} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}} The survival/censoring times. This is automatically mapped by \link{stat_km}
#'   \item \strong{\code{y}} The survival probability estimates. This is automatically mapped by \link{stat_km}
#'   smallest level in sort order is assumed to be 0, with a warning
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @inheritParams ggplot2::geom_point
#' @seealso The default stat for this geom is \code{\link{stat_kmticks}} see
#'   that documentation for more options to control the underlying statistical transformation.
#' @export
#' @keywords internal
#' @rdname geom_kmticks
#' @examples
#' library(ggplot2)
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(time = time, status = status, color = factor(sex), group = factor(sex))) + geom_km() + geom_kmticks(col="black")

geom_kmticks <- function(mapping = NULL, data = NULL, stat = "kmticks",
                         position = "identity", show.legend = NA,
                         inherit.aes = TRUE, na.rm = TRUE, ...) {
  ggplot2::layer(
    geom = GeomKmticks, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}





#' @importFrom ggplot2 layer aes ggproto
#' @import scales
#' @importFrom survival Surv survfit.formula
#' @importFrom grid pointsGrob nullGrob unit gpar gList

#' @rdname stat_km
#' @export
#' @keywords internal

StatKm <- ggplot2::ggproto("StatKm", ggplot2::Stat,
                           
                           compute_group = function(data, scales, trans = "identity", firstx = 0, firsty = 1,
                                                    type = "kaplan-meier", start.time = 0) {
                             
                             sf <- survival::survfit.formula(survival::Surv(data$time, data$status) ~ 1, se.fit = FALSE,
                                                             type = type, start.time = start.time)
                             
                             transloc <- scales::as.trans(trans)$trans
                             
                             if(is.null(sf$surv)) {
                               x <- rep(sf$time, 2)
                               sf$surv <- rep(1, length(x))
                             }
                             
                             x <- c(firstx, sf$time)
                             y <- transloc(c(firsty, sf$surv))
                             y[y == -Inf] <- min(y[is.finite(y)])
                             y[y == Inf] <- max(y[is.finite(y)])
                             
                             step <- dostep(x, y)
                             df.out <- data.frame(time = step$x, survival = step$y)
                             
                             df.out
                             
                           },
                           
                           default_aes = ggplot2::aes(y = ..survival.., x = ..time..),
                           required_aes = c("time", "status")
                           
                           
)

## need to create a different stat for kmticks



#' @importFrom ggplot2 layer aes ggproto
#' @import scales
#' @importFrom survival Surv survfit.formula
#' @importFrom grid pointsGrob nullGrob unit gpar gList

#' @rdname stat_kmband
#' @export
#' @keywords internal

StatKmband <- ggplot2::ggproto("StatKmband", ggplot2::Stat,
                               
                               compute_group = function(data, scales, trans = "identity", firstx = 0, firsty = 1,
                                                        type = "kaplan-meier", error = "greenwood", conf.type = "log",
                                                        conf.lower = "usual", start.time = 0, conf.int = 0.95) {
                                 
                                 sf <- survival::survfit.formula(survival::Surv(data$time, data$status) ~ 1, se.fit = TRUE,
                                                                 type = type, error = error, conf.type = conf.type,
                                                                 conf.lower = conf.lower, start.time = start.time, conf.int = conf.int)
                                 
                                 transloc <- scales::as.trans(trans)$trans
                                 if(is.null(sf$surv)) {
                                   x <- rep(sf$time, 2)
                                   sf$surv <- rep(1, length(x))
                                 }
                                 
                                 x <- c(firstx, sf$time)
                                 y <- transloc(c(firsty, sf$surv))
                                 y[y == -Inf] <- min(y[is.finite(y)])
                                 y[y == Inf] <- max(y[is.finite(y)])
                                 
                                 ymin <- transloc(c(firsty, sf$lower))
                                 ymax <- transloc(c(firsty, sf$upper))
                                 ymin[ymin == -Inf] <- min(ymin[is.finite(ymin)])
                                 ymin[ymin == Inf] <- max(ymin[is.finite(ymin)])
                                 ymax[ymax == -Inf] <- min(ymax[is.finite(ymax)])
                                 ymax[ymax == Inf] <- max(ymax[is.finite(ymax)])
                                 ymax <- zoo::na.locf(ymax)
                                 ymin <- zoo::na.locf(ymin)
                                 dfout<-   stairstepn(data=data.frame(x=x,ymin=ymin,ymax=ymax),  yvars=c("ymin", "ymax"))
                                 names(dfout)<- c("time","lower","upper")
                                 dfout
                               },
                               
                               default_aes = ggplot2::aes(ymin = ..lower.., ymax = ..upper.., x = ..time..),
                               required_aes = c("time", "status")
                               
                               
)


#' Adds a Kaplan Meier Estimate of Survival
#'
#' @section Aesthetics:
#' \code{stat_km} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{time}} The survival times
#'   \item \strong{\code{status}} The censoring indicator, see \link[survival]{Surv} for more information.
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @inheritParams ggplot2::stat_identity
#' @param trans Transformation to apply to the survival probabilities. Defaults
#'   to "identity". Other options include "event", "cumhaz", "cloglog", or
#'   define your own using \link{trans_new}.
#' @param firstx,firsty the starting point for the survival curves. By default,
#'   the plot program obeys tradition by having the plot start at (0,1).
#' @param ... Other arguments passed to \code{survival::survfit.formula}
#' @return a data.frame with additional columns: \item{x}{x in data}
#'   \item{y}{Kaplan-Meier Survival Estimate at x}
#' @export
#' @keywords internal
#'
#' @rdname stat_km
#' @details
#'
#' This stat is for computing the confidence intervals for the Kaplan-Meier survival estimate for
#' right-censored data. It requires the aesthetic mapping \code{x} for the
#' observation times and \code{status} which indicates the event status,
#' 0=alive, 1=dead or 1/2 (2=death). Logical status is not supported.
#'
#'
#' @examples
#' library(ggplot2)
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(time = time, status = status, color = factor(sex))) +
#'  stat_km()
#'
#' ## Examples illustrating the options passed to survfit.formula
#'
#' p1 <- ggplot(df, aes(time = time, status = status))
#' p1 + stat_km()
#' p1 + stat_km(trans = "cumhaz")
#' # cloglog plots also log transform the time axis
#' p1 + stat_km(trans = "cloglog") + scale_x_log10()
#' p1 + stat_km(type = "fleming-harrington")
#' p1 + stat_km(start.time = 5)
#'

stat_km <- function(mapping = NULL, data = NULL, geom = "km",
                    position = "identity", show.legend = NA, inherit.aes = TRUE,
                    se = TRUE, trans = "identity", firstx = 0, firsty = 1,
                    type = "kaplan-meier", start.time = 0, ...) {
  ggplot2::layer(
    stat = StatKm,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(trans = trans, firstx = firstx, firsty = firsty,
                  type = type, start.time = start.time, ...)
  )
  
}



#' Adds confidence bands to a Kaplan Meier Estimate of Survival
#'
#' @section Aesthetics:
#' \code{stat_kmband} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{time}} The survival times
#'   \item \strong{\code{status}} The censoring indicator, see \link[survival]{Surv} for more information.
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @inheritParams ggplot2::stat_identity
#' @param \code{conf.int} to control significance level which is 0.95 by default
#' @param trans Transformation to apply to the survival probabilities. Defaults
#'   to "identity". Other options include "event", "cumhaz", "cloglog", or
#'   define your own using \link{trans_new}.
#' @param firstx,firsty the starting point for the survival curves. By default,
#'   the plot program obeys tradition by having the plot start at (0,1).
#' @param ... Other arguments passed to \code{survival::survfit.formula}
#' @return a data.frame with additional columns: \item{x}{x in data}
#'   \item{ymin}{Lower confidence
#'   limit of KM curve, if \code{se = TRUE}} \item{ymax}{Upper confidence limit
#'   of KM curve}
#' @export
#' @keywords internal
#'
#' @rdname stat_kmband
#' @details
#'
#' This stat is for computing the confidence intervals for the Kaplan-Meier survival estimate for
#' right-censored data. It requires the aesthetic mapping \code{x} for the
#' observation times and \code{status} which indicates the event status,
#' 0=alive, 1=dead or 1/2 (2=death). Logical status is not supported.
#'
#' @examples
#' library(ggplot2)
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(time = time, status = status, color = factor(sex))) +
#'  stat_km()
#'
#' ## Examples illustrating the options passed to survfit.formula
#'
#' p1 <- ggplot(df, aes(time = time, status = status))
#' p1 + stat_km() + stat_kmband(conf.int = .99)
#' p1 + stat_kmband(error = "greenwood",fill="red",alpha=0.2) + stat_kmband(error = "tsiatis",alpha=1)+ stat_km()
#' p1 + stat_km() + stat_kmband(conf.type = "log-log")+ stat_kmband(conf.type = "log")
#'

stat_kmband <- function(mapping = NULL, data = NULL, geom = "kmband",
                        position = "identity", show.legend = NA, inherit.aes = TRUE,
                        trans = "identity", firstx = 0, firsty = 1,
                        type = "kaplan-meier", error = "greenwood", conf.type = "log",
                        conf.lower = "usual", start.time = 0, conf.int = 0.95, ...) {
  ggplot2::layer(
    stat = StatKmband,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(trans = trans, firstx = firstx, firsty = firsty,
                  type = type, error = error, conf.type = conf.type,
                  conf.lower = conf.lower, start.time = start.time, conf.int = conf.int, ...)
  )
  
}



#' Compute locations for tick marks
#'
#' Tick marks are plotted where there are censoring times that are also not event times
#'
#' @rdname stat_kmticks
#' @export
#' @keywords internal

StatKmticks <- ggplot2::ggproto("StatKmticks", ggplot2::Stat,
                                
                                compute_group = function(data, scales, trans = "identity", ...) {
                                  
                                  sf <- survival::survfit.formula(survival::Surv(data$time, data$status) ~ 1, se.fit = FALSE, ...)
                                  trans <- scales::as.trans(trans)$trans
                                  
                                  sf.df <- data.frame(time = sf$time,
                                                      survival = trans(sf$surv),
                                                      n.risk = sf$n.risk,
                                                      n.censor = sf$n.censor,
                                                      n.event = sf$n.event)
                                  
                                  sf.df
                                  
                                },
                                
                                default_aes = ggplot2::aes(y = ..survival.., x = ..time..),
                                required_aes = c("time", "status")
                                
                                
)



#' Adds tick marks to a Kaplan Meier Estimate of Survival
#'
#' @section Aesthetics:
#' \code{stat_kmticks} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{time}} The survival times
#'   \item \strong{\code{status}} The censoring indicator, see \link[survival]{Surv} for more information.
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @seealso \link{stat_km}
#' @inheritParams ggplot2::stat_identity
#' @param trans Transformation to apply to the survival probabilities. Defaults
#'   to "identity". Other options include "event", "cumhaz", "cloglog", or
#'   define your own using \link{trans_new}.
#' @param ... Other arguments passed to \code{survival::survfit.formula}
#' @return a data.frame with additional columns: \item{x}{x in data}
#'   \item{y}{Kaplan-Meier Survival Estimate at x}
#' @export
#' @keywords internal
#' @rdname stat_kmticks
#'
#' @details
#'
#' This stat is for computing the tick marks for a Kaplan-Meier survival estimate for
#' right-censored data. The tick marks will appear at each censoring time which is also
#' not a death time, which is the default for \link{plot.survfit}.
#' It requires the aesthetic mapping \code{x} for the
#' observation times and \code{status} which indicates the event status,
#' normally 0=alive, 1=dead. Other choices are TRUE/FALSE (TRUE = death) or 1/2
#' (2=death).
#'
#' @examples
#' library(ggplot2)
#' sex <- rbinom(250, 1, .5)
#' df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
#' ggplot(df, aes(time = time, status = status, color = factor(sex))) +
#'  stat_km() + stat_kmticks()
#'


stat_kmticks <- function(mapping = NULL, data = NULL, geom = "kmticks",
                         position = "identity", show.legend = NA, inherit.aes = TRUE, trans, ...) {
  ggplot2::layer(
    stat = StatKmticks,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
  
}







cumhaz_trans <- function(){
  
  trans <- function(x){
    -log(x)
  }
  
  inv <- function(x){
    exp(-x)
  }
  
  scales::trans_new("cumhaz",
                    trans,
                    inv,
                    scales::log_breaks(base = exp(1)),
                    domain = c(0, Inf) ## The domain over which the transformation is valued
  )
}


event_trans <- function(){
  
  trans <- function(x){
    1-x
  }
  
  inv <- function(x){
    1-x
  }
  
  trans_new("event",
            trans,
            inv,
            scales::pretty_breaks(),
            domain = c(0, 1) ## The domain over which the transformation is valued
  )
}


cloglog_trans <- function(){
  
  trans <- function(x){
    log(-log(x))
  }
  
  inv <- function(x){
    exp(-exp(x))
  }
  
  trans_new("cloglog",
            trans,
            inv,
            scales::pretty_breaks(),
            domain = c(-Inf, Inf) ## The domain over which the transformation is valued
  )
}


dostep <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  if (!any(keep))
    return()
  if (!all(keep)) {
    x <- x[keep]
    y <- y[keep]
  }
  n <- length(x)
  if (n == 1)
    list(x = x, y = y)
  else if (n == 2)
    list(x = x[c(1, 2, 2)], y = y[c(1, 1, 2)])
  else {
    temp <- rle(y)$lengths
    drops <- 1 + cumsum(temp[-length(temp)])
    if (n %in% drops) {
      xrep <- c(x[1], rep(x[drops], each = 2))
      yrep <- rep(y[c(1, drops)], c(rep(2, length(drops)),
                                    1))
    }
    else {
      xrep <- c(x[1], rep(x[drops], each = 2), x[n])
      yrep <- c(rep(y[c(1, drops)], each = 2))
    }
    list(x = xrep, y = yrep)
  }
}



merge_steps <- function(s1, s2) {
  
  n2 <- s1$x[vapply(s1$x, function(x) !x %in% s2$x, TRUE)]
  
  ns2 <- s2
  wats <- vapply(n2, function(x){
    
    t1 <- s2$x[x > s2$x]
    if(length(t1) < 1) {
      return(NA)
    } else {
      max(which(s2$x == max(t1, na.rm = TRUE)))
    }
    
  }, integer(1))
  wats <- wats[!is.na(wats)]
  
  ns2$x <- append(ns2$x, n2)
  ns2$y <- append(ns2$y, ns2$y[wats])
  
  res2 <- list(x = sort(ns2$x), y = ns2$y[order(ns2$x)])
  
  n1 <- res2$x[vapply(res2$x, function(x) !x %in% s1$x, TRUE)]
  
  ns1 <- s1
  wats <- vapply(n1, function(x) {
    t1 <- s1$x[x > s1$x]
    if(length(t1) < 1) {
      return(NA)
    } else {
      max(which(s1$x == max(t1, na.rm = TRUE)))
    }
  }, integer(1))
  
  ns1$x <- append(ns1$x, n1)
  ns1$y <- append(ns1$y, ns1$y[wats])
  
  
  res1 <- list(x = sort(ns1$x), y = ns1$y[order(ns1$x)])
  
  list(s1 = res1, s2 = res2)
  
  
}

## from ggalt
stairstepn <- function(data,yvars="y") {
  data <- as.data.frame(data)[order(data$x),]
  
    n <- nrow(data)
    ys <- rep(1:n, each=2)[-2*n]
    xs <- c(1, rep(2:n, each=2))
  data.frame(
    x=data$x[xs],
    data[ys, yvars, drop=FALSE],
    data[xs, setdiff(names(data), c("x", yvars)), drop=FALSE]
  )
}
