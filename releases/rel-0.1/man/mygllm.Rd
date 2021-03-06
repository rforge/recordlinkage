\name{mygllm}
\alias{mygllm}

\title{Extended emgllm}

\description{
   Adoption and Extension of \code{\link[gllm]{emgllm}}, originally algorithm AS 207 
   (see references).
}
\usage{
mygllm(y, s, X, maxit = 1000, tol = 1e-05, E = rep(1, length(s)))
}

\arguments{
  See \code{\link[gllm]{emgllm}} in package \code{gllm} for detailed description.
 \item{y}{Vector of observed cell frequencies.}
 \item{s}{Scatter matrix. s[i] is the cell in the observed array that
          corresponds to cell i in the full array.}
 \item{X}{Design matrix.}
 \item{maxit}{Maximum number of iterations.}
 \item{tol}{Convergence parameter.}
 \item{E}{Full contingency table. Should be initialized with either ones or
          a priori estimates.}
}

\details{Works like \code{\link[gllm]{emgllm}}. In addition,
        a priori estimates of the full contingency table can be given as
        start values for argument \code{E}. This extension can 
        reduce execution time significantly.        
        }

\value{Estimated full contingency table.}


\references{Michael Haber, Algorithm AS 207: Fitting a General Log-Linear
            Model, in: Applied Statistics 33 Vol. 33 No. 3 (1984),  S. 358-362.
            
            David Duffy (2006). gllm: Generalised log-linear model. R package
            version 0.31.}

\author{Andreas Borg (R interface and translation to C)}

\seealso{\code{\link[gllm]{emgllm}}}
