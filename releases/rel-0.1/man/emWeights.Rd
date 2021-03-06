\name{emWeights}
\alias{emWeights}

\title{Calculate weights}
\description{
  Calculates weights for Record Linkage based on the EM algorithm.
}
\usage{
emWeights(rpairs, cutoff = 0.95, ...)
}
\arguments{
  \item{rpairs}{Record pairs as \code{\link{RecLinkData}} object}
  \item{cutoff}{cutoff value for string comparator}
  \item{\dots}{additional arguments for \code{\link{mygllm}}}
}
\details{
  The weight of a record pair is calculated by \eqn{\log_{2}\frac{M}{U}}{
  \code{log(M/U,base=2)}}, where \eqn{M} and \eqn{U} are estimated m- and u-probabilities
  for the present comparison pattern. If a string comparator is used, weights
  are first calculated based on a binary table where all comparison 
  values greater or equal \code{cutoff} are set to one, all other to zero.
  The resulting weight is adjusted by adding for every pair
  \eqn{\log_{2}\left(\prod_{j:s^{i}_{j}\geq \textit{cutoff }}s^{i}_{j}\right)}{
  log(prod(s[i,s[i,]>=cutoff]), base = 2)}, where
  \eqn{s^{i}_{j}}{s[i,j]} is the value of the string metric for attribute j in 
  data pair i.
  
  Estimation of \eqn{M} and \eqn{U} is done by \code{\link{mygllm}},
  an adoption of the EM algorithm in package \code{gllm}. For every comparison
  pattern, the estimated numbers of matches and non-matches are used to compute
  the corresponding probabilities. An estimation based on the average 
  frequencies of values and fixed error rates is taken as initial values.
  Our experience has shown that this increases stability and performance of the
  EM algorithm while the results are more or less independent of the chosen
  values.
}

\value{
  A \code{\link{RecLinkData}} object containing all components of \code{rpairs}
  plus the following:
  \item{M}{Estimated m-probabilities}
  \item{U}{Estimated u-probabilities}
  \item{W,Wdata}{Calculated weights}
  
  \code{M}, \code{U} and \code{W} correspond to a list of all binary comparison
  patterns, sorted ascending from all zeroes to all ones. \code{Wdata} 
  corresponds directly to the record pairs in \code{rpairs$valid}.  
}
\references{William E. Winkler: Using the EM Algorithm for Weight Computation 
  in the Fellegi-Sunter Model of Record Linkage, in: Proceedings of the Section 
  on Survey Research Methods, American Statistical Association 1988, 
  pp. 667--671}
\author{Andreas Borg}
\seealso{\code{\link{emClassify}} for classification of weighted pairs.}

\keyword{classif}
