#' @import rtweet
#' @import dplyr
#' @import wordcloud2
#' @import tidytext
#' @import stringr
#' @export
Search2Cloud <- function(searchT, n.enter = 3500) {
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
