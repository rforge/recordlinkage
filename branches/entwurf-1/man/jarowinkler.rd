\name{jarowinkler}
\alias{jarowinkler}
\alias{jaro}
\alias{winkler}
\title{Compute Jaro-Winkler string metric}
\description{
   Computes Jaro-Winkler string metric for pairs of strings.
   	}
\usage{jarowinkler(str1, str2, W_1=1/3, W_2=1/3, W_3=1/3, r=0.5,
                        use_transpose_radius=FALSE)}
\arguments{
   \item{str1,str2}{The strings to compare.}
   \item{W_1,W_2,W_3}{Adjustable weights. Default values are usually fine.}
   \item{r}{Maximum radius of transpositions, as fraction of the length of the 
            shorter string.}
   \item{use_transpose_radius}{Switch for an experimental addition.}
   }
\value{Similarity between the strings as a real number in [0..1]}
\references{Winkler, W.E.: String Comparator Metrics and Enhanced Decision
Rules in the Fellegi-Sunter Model of Record Linkage. In: Proceedings
of the Section on Survey Research Methods, American Statistical Association
(1990), S. 354�369}
\keyword{misc}

