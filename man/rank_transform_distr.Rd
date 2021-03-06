% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rank_transform_distr_function.R
\name{rank_transform_distr}
\alias{rank_transform_distr}
\title{Rank transformation based on a distribution function}
\usage{
rank_transform_distr(x, distr = "norm", ...)
}
\arguments{
\item{x}{Input vector}

\item{distr}{Character naming output distribution function (e.g. "norm",
"lnorm", "unif", "exp", "gamma",...). Use "trunc" for truncated distributions
as provided by the truncdist package. See examples below for parameter
settings.}

\item{...}{Distribution parameters (see examples below)}
}
\value{
Vector of transformed values
}
\description{
Rank transform a dataset, i.e., for each input value provide an output value
from a given probability distribution, with the order of values (ranks)
being preserved.
}
\examples{
# Transform values from 1 to 100 into a normal distribution
test.vec <- 1:100
norm.vec <- rank_transform_distr(x=test.vec, distr="norm", mean=55, sd=5)
hist(test.vec)
hist(norm.vec)
# Transform into a gamma distribution
gamma.vec <- rank_transform_distr(x=norm.vec, distr="gamma",
                                  shape=0.5, rate=1)
hist(gamma.vec)
# Transform into a truncated lognormal distribution
require(truncdist)
trunc.lnorm.vec <- rank_transform_distr(x=gamma.vec, distr="trunc",
                                        meanlog=3, sdlog=0.5,
                                        spec="lnorm", a=10, b=40)
hist(trunc.lnorm.vec)
}
\author{
Nikolai Knapp, nikolai.knapp@ufz.de
}
\keyword{distribution}
\keyword{normalization}
\keyword{order}
\keyword{rank}
\keyword{transformation}
