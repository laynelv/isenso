\name{comprofile}
\alias{comprofile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 comparative profiling
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Comparative Profiling is a descriptive sensory technique used to measure the relative difference between two products for a set of sensory attributes, one product being chosen as the reference sample. The product and the reference sample are presented at the same time, side by side.

Calculate each sensory attribute's mean score and 0.95 confidence interval, use a bar plot with error bar to visualize the results.
}
\usage{
comprofile(data = NULL, firsta = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
a data frame, contains product,panelists names in rows and attributes in columns.
}
  \item{firsta}{
%%     ~~Describe \code{firstatri} here~~
the  column number of the first sensory attribute.default is 4.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{mean }{the mean score of attributes}
\item{sd }{standard deviation}
\item{ci }{confidence interval of mean, default level is 0.95}
\item{significant }{'Yes' means the sample is significant different from REF in this attribute.}

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
Layne lv (layne.lv@outlook.com)
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
data(comprofile_data)
comprofile(comprofile_data)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~profiling }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
