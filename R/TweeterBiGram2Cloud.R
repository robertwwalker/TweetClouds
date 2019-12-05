##' Create wordcloud by wordcloud2.js from Tweets by timeline
##'
##' @description
##' Function for Creating wordcloud from a twitter handle through the Twitter API
##'
##' @usage
##' TweeterBiGram2Cloud(handle, n.tweets=5000, Bigrams = 500)
##'
##' @param handle   A twitter handle with the timeline to examine
##' @param n.tweets   The number of tweets to examine, default is 5000.
##' @param Bigrams   The number of bigrams to plot, default is 500.
##'
##' @examples
##'library(TweetClouds)
##'# Global variables can go here
##'
##'
##'
##' TweeterBiGram2Cloud("realDonaldTrump")
##' TweeterBiGram2Cloud("realDonaldTrump", n.tweets=9000, Bigrams=350)
##'
#' @import rtweet
#' @import dplyr
#' @import wordcloud2
#' @import tidytext
#' @import stringr
#' @import tidyr
#' @export

TweeterBiGram2Cloud <- function(handle, n.tweets=5000, Bigrams=500) {
  TDF <- get_timeline(handle, n = n.tweets) %>% filter(is_retweet==FALSE) %>% select(text)
  # TDF contains the text of tweets.
  TDF %>% mutate_if(is.factor, as.character) -> TDF
  clean_tweets <- data.frame(text=sapply(1:dim(TDF)[[1]], function(x) {tweet_cleaner(TDF[x,"text"])}))
  Base2G <- clean_tweets %>% mutate(tweetno= row_number(), text = as.character(text)) %>% unnest_tokens(bigram, text, token = "ngrams", n=2)
  bigrams_separated <- Base2G %>% separate(bigram, c("word1", "word2"), sep = " ")
  bigrams_filtered <- bigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
  bigram_counts <- bigrams_filtered %>% count(word1, word2, sort = TRUE)
  bigrams_united <- bigram_counts %>% unite(bigram, word1, word2, sep = " ")
  my.df <- bigrams_united[order(bigrams_united$n, decreasing=TRUE),]
  my.df <- my.df[c(1:Bigrams),]
  wordcloud2(my.df, color="random-light", backgroundColor = "black", size = 0.5)
}
