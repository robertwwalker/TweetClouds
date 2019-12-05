\name{Tweeter2Cloud}
\alias{Tweeter2Cloud}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A twitter handle as input produces a wordcloud
}
\description{
Takes a quoted twitter handle and produces a wordcloud2
}
\usage{
Tweeter2Cloud(handle, n.enter=3200)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{handle}{
  A character string handle for the timeline to retrieve from Twitter
}
  \item{n.enter}{
  Number of matching tweets to return [defaults to 3200]
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Not run
## Tweeter2Cloud("realDonaldTrump")

## The function is currently defined as
function (handle, n.enter=3200) {
local.tw <- get_timeline(handle, n = n.enter)
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
\keyword{wordcloud2}% __ONLY ONE__ keyword per line
