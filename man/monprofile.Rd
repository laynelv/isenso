\name{monprofile}
\alias{monprofile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
monodic profiling
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
descriptive sensory profiling technique used to measure the intensity of each sensory attribute related to the products. Products are evaluated one at a time for all the attributes.

Calculate each sensory attribute's mean score , use a line plot and PCA biplot to visualize the relations between products and attributes.
}
\usage{
monprofile(data = NULL, prod = 1, firsta = 4, ncluster = 3, graph = c("point", "text"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
a data frame, contains product/panelists names in rows and attributes in columns.
}
  \item{prod}{
%%     ~~Describe \code{prod} here~~
the column number which contains products names.default is 1.
}
  \item{firsta}{
%%     ~~Describe \code{firsta} here~~
the column number of the first sensory attribute.default is 4.
}
  \item{ncluster}{
%%     ~~Describe \code{ncluster} here~~
the number of clusters you want to have when calculating kmeans.
}
  \item{graph}{
%%     ~~Describe \code{graph} here~~
the content you want to plot,"point" only plot points,"text" only plot texts.  plot both in default.
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
data(monprofile_data)
monprofile(monprofile_data,graph = "text",ncluster = 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
