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
