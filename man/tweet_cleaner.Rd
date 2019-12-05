\name{tweet_cleaner}
\alias{tweet_cleaner}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clean tweet text
}
\description{
This function takes an input collection of tweets in text form and remove garbage.
}
\usage{
tweet_cleaner(text)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
text of tweets in vector form
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

function (text)
{
tweet_cleaner <- function(text) {
    temp1 <- str_replace_all(text, "&amp", "") %>%
      str_replace_all(., "https://t+", "") %>%
      str_replace_all(.,"@[a-z,A-Z]*","") %>%
      str_replace_all(., "[[:punct:]]", "")  %>%
      str_replace_all(., "[[:digit:]]", "") %>%
      #    str_replace_all(., "[ \t]{2,}", "") %>%
      #    str_replace_all(., "^\\s+|\\s+$", "")  %>%
      #    str_replace_all(., " "," ") %>%
      #    str_replace_all(., "http://t.co/[a-z,A-Z,0-9]*{8}","")
      #    str_replace_all(.,"RT @[a-z,A-Z]*: ","") %>%
      str_replace_all(.,"#[a-z,A-Z]*","")
    return(temp1)
}

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{twitter}% use one of  RShowDoc("KEYWORDS")
