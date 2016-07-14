\name{panelperf}
\alias{panelperf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{panel performance for descriptive Sensory Tests
%%  ~~function to do ... ~~
}
\description{
Compute attribute~Product*Panelist two-way ANOVAs 

If p-value> 0.05,The panel was not discriminant.This can be due to a lack of difference among the products on this particular attribute. If it'S a key attribute, re-train the panel.the bars will be red.

If p-value<= 0.05,The panel was discriminant for the given attribute.the bars will be blue.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
panelperf(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  a dataframe, it's structure as same as the 'chocolate',colomns are Product,Panelist,Session,attribute01,attribute02......
%%     ~~Describe \code{data} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(chocolate)
panelperf(chocolate)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
