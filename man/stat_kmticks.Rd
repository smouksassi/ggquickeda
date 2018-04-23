% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggkm.R
\docType{data}
\name{StatKmticks}
\alias{StatKmticks}
\alias{stat_kmticks}
\title{Compute locations for tick marks}
\format{An object of class \code{StatKmticks} (inherits from \code{Stat}, \code{ggproto}) of length 4.}
\usage{
StatKmticks

stat_kmticks(mapping = NULL, data = NULL, geom = "kmticks",
  position = "identity", show.legend = NA, inherit.aes = TRUE, trans, ...)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link{aes}} or
\code{\link{aes_}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three
   options:

   If \code{NULL}, the default, the data is inherited from the plot
   data as specified in the call to \code{\link{ggplot}}.

   A \code{data.frame}, or other object, will override the plot
   data. All objects will be fortified to produce a data frame. See
   \code{\link{fortify}} for which variables will be created.

   A \code{function} will be called with a single argument,
   the plot data. The return value must be a \code{data.frame.}, and
   will be used as the layer data.}

\item{geom}{The geometric object to use display the data}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link{borders}}.}

\item{trans}{Transformation to apply to the survival probabilities. Defaults
to "identity". Other options include "event", "cumhaz", "cloglog", or
define your own using \link{trans_new}.}

\item{...}{Other arguments passed to \code{survival::survfit.formula}}
}
\value{
a data.frame with additional columns: \item{x}{x in data}
  \item{y}{Kaplan-Meier Survival Estimate at x}
}
\description{
Tick marks are plotted where there are censoring times that are also not event times

Adds tick marks to a Kaplan Meier Estimate of Survival
}
\details{
This stat is for computing the tick marks for a Kaplan-Meier survival estimate for
right-censored data. The tick marks will appear at each censoring time which is also
not a death time, which is the default for \link{plot.survfit}.
It requires the aesthetic mapping \code{x} for the
observation times and \code{status} which indicates the event status,
normally 0=alive, 1=dead. Other choices are TRUE/FALSE (TRUE = death) or 1/2
(2=death).
}
\section{Aesthetics}{

\code{stat_kmticks} understands the following aesthetics (required aesthetics
are in bold):
\itemize{
  \item \strong{\code{time}} The survival times
  \item \strong{\code{status}} The censoring indicator, see \link[survival]{Surv} for more information.
  \item \code{alpha}
  \item \code{color}
  \item \code{linetype}
  \item \code{size}
}
}

\examples{
library(ggplot2)
sex <- rbinom(250, 1, .5)
df <- data.frame(time = exp(rnorm(250, mean = sex)), status = rbinom(250, 1, .75), sex = sex)
ggplot(df, aes(time = time, status = status, color = factor(sex))) +
 stat_km() + stat_kmticks()

}
\seealso{
\link{stat_km}
}
\keyword{internal}