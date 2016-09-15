\name{GRS.optimal}
\alias{GRS.optimal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimal Level of Significance for the GRS test
}
\description{
The optimal level is calculated by minimizing expected loss from hypothesis testing

Under the assumption of equal prior and identical losses from Type I and II errors

}
\usage{
GRS.optimal(T, N, K, theta, ratio, Graph = "TRUE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T}{ sample size }
  \item{N}{ the number of portfolio returns}
  \item{K}{ the number of risk factors }
  \item{theta}{ maximum Sharpe ratio of the K factor portfolios}
  \item{ratio}{ theta/thetas, proportion of the potential efficiency }
  \item{Graph}{ show graph if TRUE. No graph otherwise }
}
\details{
Based on the power calculation of the GRS test, as in GRS (1989) <DOI:10.2307/1913625>.

The blue square is the point where the expected loss is mimimized.

The red horizontal line indicate the point of the covnentional level of significance (alpha = 0.05).
}
\value{

\item{opt.sig }{Optimal level of significance}
\item{opt.crit }{Critical value corresponding to opt.sig}
\item{opt.beta }{Type II error probability corresponding to opt.sig}
}
\references{
Leamer, E. 1978, Specification Searches: Ad Hoc Inference with Nonexperimental Data, Wiley, New York.

Kim, JH and Ji, P. 2015, Significance Testing in Empirical Finance: A Critical Review and Assessment, Journal of Empirical Finance 34, 1-14.
<DOI:http://dx.doi.org/10.1016/j.jempfin.2015.08.006>

Gibbons, Ross, Shanken, 1989. A test of the efficiency of a given portfolio, Econometrica, 57,1121-1152.
<DOI:10.2307/1913625>

Kim and Shamsuddin, 2016, Reapparaising Empirical Validity of Asset-Pricing Models with consideration of Statistical Power. Working Paper}
\author{
Jae H. Kim
}
\note{
ratio = theta/thetas

thetas = maximum Sharpe ratio of the K factor portfolios: GRS (1989) <DOI:10.2307/1913625>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Kim and Ji (2015)
}
\examples{
GRS.optimal(T=90, N=25, K=3, theta=0.25, ratio=0.4) # Figure 3 of Kim and Shamsuddin (2016)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

