\name{Search2Cloud}
\alias{Search2Cloud}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Input a quoted hashtag or search term and return a wordcloud
}
\description{
Takes a quoted search term or hashtag and produces a wordcloud2
}
\usage{
Search2Cloud(searchT, n.enter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{searchT}{
  A character string search term or hashtag to retrieve from Twitter
}
  \item{n.enter}{
  Number of matching tweets to return [defaults to 5000]
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A wordcloud2
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Robert W. Walker
}
\note{
Not for CRAN
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (searchT, n.enter=5000) {
  local.tw <- search_tweets(searchT, n=n.enter)
  save(local.tw, file="TW-temp.RData")
  load("TW-temp.RData")
  TweetDF <- local.tw %>% filter(is_retweet==FALSE)
  TDF <- TweetDF %>% select(text)
  # TDF contains the text of tweets.
  clean_tweets <- data.frame(text=sapply(1:dim(TDF)[[1]], function(x) {tweet_cleaner(TDF[x,"text"])}))
  clean_tweets$text <- as.character(clean_tweets$text)
  Tweet.Words <- clean_tweets %>% unnest_tokens(., word, text) %>% anti_join(stop_words, "word")
  TTW <- table(Tweet.Words)
  TTW <- TTW[order(TTW, decreasing = T)]
  TTW <- data.frame(TTW)
  names(TTW) <- c("word","freq")
  wordcloud2(TTW, color="random-light", backgroundColor = "black")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Twitter}% use one of  RShowDoc("KEYWORDS")
\keyword{Hashtag}% __ONLY ONE__ keyword per line
