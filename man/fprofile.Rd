\name{fprofile}
\alias{fprofile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
flash profiling
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
a rapid descriptive sensory profiling technique where panellists generate their own individual list of sensory attributes on which they evaluate the products. No attempt is made to obtain a common list of attributes for the entire panel. This is a rapid variant of the free-choice profiling technique.

use PCA variables plot to map a set of products rapidly.

use PCA biplot to visualize the glossories that differant panelists used to study glossories.
}
\usage{
fprofile(data = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
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
ggplot2
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(fprofile_data)
fprofile(fprofile_data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
