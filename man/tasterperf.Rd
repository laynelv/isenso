\name{tasterperf}
\alias{tasterperf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{individual performance for descriptive Sensory Tests
%%  ~~function to do ... ~~
}
\description{
F :Compute attribute~Product "one-way ANOVAs" 

to asses Individual discrimination  ability,The ratio of variance "between" and "within" products is called "F-ratio" (F-ratio=Between/Within). The better the panelist discriminates, the larger this F-ratio. For this analysis,  F>=2.0 is recommended: Any taster with an F-ratio>= 2.0 will be considered as discriminant on that attribute.

r :Compute "Pearson correlations" 

to asses Individual agreement with the rest of the panel.Panelists are in agreement if they rate products in the same order and with similar scoring patterns. This can be characterized by a correlation coefficient r.Regard the agreement as satisfactory if r>= 0.8.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tasterperf(data = NULL, ntaster)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a dataframe, it's structure as same as the 'chocolate',colnames are Product,Panelist,Session,attribute01,attribute02......
%%     ~~Describe \code{data} here~~
}
  \item{ntaster}{the name or number of a taster.
%%     ~~Describe \code{ntaster} here~~
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
## compute taster11's discrimination ability and aggreement
data(chocolate)
tasterperf(chocolate,"taster11")

##another way

tasterperf(chocolate,11)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
