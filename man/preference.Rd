\name{preference}
\alias{preference}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ consumer preference

%%  ~~function to do ... ~~
}
\description{
1. Draw a barplot to show which product is the faverate of consummers.compute the means of consummers liking scores,then do LSD multiple comparations to identify whether the means are significantly different. Visualize the results by a barplot with means and different colors.

2.Draw a biplot to show the relations of products and consummers and if there are  sperate groups of consummers by the method of K-means.

Preference Mapping is a set of statistical techniques that can be used to analyze data collected during a Product Guidance or Product Optimization test.
The Preference Mapping analysis provides additional understanding of consumer preference data, providing greater actionability to the consumer test.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
preference(data=NULL,ncluster=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame, contains consummers in rows and products in columns.
%%     ~~Describe \code{x} here~~
}
 \item{ncluster}{How many clusters you want to have? default is 3
%%     ~~Describe \code{x} here~~
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
\author{Layne lv (layne.lv@outlook.com)
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
data(choc_preference)
preference(choc_preference)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~preference }
\keyword{ ~cluster }% __ONLY ONE__ keyword per line
