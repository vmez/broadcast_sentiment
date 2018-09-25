#data from: https://www.datacamp.com/courses/sentiment-analysis-in-r-the-tidy-way 

library(tidytext)
library(tidyverse)
library(stringr)
pacman::p_load(wordcloud)
library(reshape2)


# Open file TV.text in Projects ------------------------
glimpse(climate_text)

# Most common words per Show ----------------------
climate_text %>%
  group_by(show) %>%
  unnest_tokens(word, text) %>% anti_join(stop_words)  %>% count(word, sort = T) %>%
  filter(n > 30) %>%
  ggplot(aes(word, n, fill = show)) +  geom_col() + xlab("") + coord_flip() + theme_minimal() + 
  ggtitle("Most Common Words Used per Show", subtitle = "Filtering word count larger than 30")


# Most common words per Broadcast:
climate_text %>%
  group_by(station) %>%
  unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(word, sort = T) %>%
  filter(n > 30) %>%
  ggplot(aes(reorder(word, n), n, fill = station)) +  geom_col() + xlab("") + coord_flip() + theme_minimal() + 
  ggtitle("Most Common Words User per Broadcast", subtitle = "Filtering word count larger than 30")


# Get sentiment:
get_sentiments("nrc")

negative <- get_sentiments("nrc") %>% filter(sentiment == "negative")

climate_text %>%
  group_by(station) %>% 
  unnest_tokens(word, text) %>% anti_join(stop_words) %>% 
  inner_join(negative) %>%  count(word, sort = T) 

# Sentiment per Show: Worring with diff dictionaries
climate_text %>%
  group_by(station, show) %>%
  unnest_tokens(word, text) %>% anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% #bing has only positive/negative counts
  count(show, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot() + geom_bar(aes(sentiment, fill = station)) +
  facet_wrap(~ station, scales = "free_x") + coord_flip() +
  ggtitle("Popular TV broadcast opinion over Climate Temperatures", 
          subtitle = "net sentiment calculated per show using 'bing' dictionary") + 
  theme_minimal() 

climate_text %>%
  group_by(station, show) %>%
  unnest_tokens(word, text) %>% anti_join(stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>% # accounts for 10 types of sentiment
  count(word, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot() + geom_bar(aes(sentiment, fill = station)) +
  facet_wrap(~ station, scales = "free_x") + coord_flip() +
  ggtitle("Popular TV broadcast opinion over Climate Temperatures", 
          subtitle = "net sentiment calculated per show using 'nrc' dictionary") + 
  theme_minimal() 

climate_text %>%
  group_by(station, show) %>%
  unnest_tokens(word, text) %>% anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn")) %>% # returns a score
  count(word, score) %>%
  ggplot() + geom_bar(aes(score, fill = station)) +
  facet_wrap(~ station, scales = "free_x") + coord_flip() +
  ggtitle("Popular TV broadcast opinion over Climate Temperatures", 
          subtitle = "net sentiment calculated per show using 'afinn' dictionary") + 
  theme_minimal()

 # Most common positive and negative words:
words <- climate_text %>%
  group_by(station, show) %>%
  unnest_tokens(word, text) %>% anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% # returns a score
  count(word, sentiment, sort = T)  

words %>% group_by(sentiment) %>% ungroup() %>% top_n(10) %>%
  ggplot(aes(reorder(word, n),n, fill = sentiment)) +
  geom_col() + facet_wrap(~ station, scales = "free_y") +
  coord_flip() + theme_minimal() + 
  ggtitle("Top10 word count describing Sentiment", 
          subtitle = "using the bing dictionary") + labs(y = "word count", x = "")

# Building a Word Count:
climate_text %>%
  unnest_tokens(word, text) %>% anti_join(stop_words) %>%
  count(word, sort = T) %>%
  filter(n > 20) %>%
  with(wordcloud(word, n, colors = "#de2d26", ordered.colors = F))


# Cloud Comparing Positive vs Negative:
climate_text %>%
  unnest_tokens(word, text) %>% anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#a6bddb", "#1c9099"), max.words = 100,
                   title.size = 1.5)

# LEXICON & WORDS ..............................................................
# https://www.datacamp.com/community/tutorials/sentiment-analysis-R?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com
#................................................................................
library(formattable) # for table graph

# Customize a table graph for consistency in HTLM formatting
style_table <- function(dat, caption) {
  kableExtra::kable(dat, "html", escape = F, caption = caption) %>%
    kableExtra::kable_styling(bootstrap_options =  c("striped", "condensed", "bordered"),
                  full_width = F)
}


# Tidytext has a dataset called sentiments which provides with lexicons: AFINN, Bing, NRC
glimpse(sentiments)
unique(sentiments$lexicon)
unique(sentiments$score)
unique(sentiments$sentiment)

# Examine lexicons: 
new_sentiments <- 
  sentiments %>% 
  filter(lexicon != "loughran") %>% # exclude financial lexicon
  mutate(sentiment = ifelse(lexicon == "AFINN" & score >= 0, "positive",
                            ifelse(lexicon == "AFINN" & score < 0, "negative",
                                   sentiment))) %>% 
  group_by(lexicon) %>% 
  mutate(words_in_lexicon = n_distinct(word)) %>%
  ungroup() # Why use ungroup(): https://community.rstudio.com/t/is-ungroup-recommended-after-every-group-by/5296/4

new_sentiments %>% 
  group_by(lexicon, sentiment, words_in_lexicon) %>%
  summarise(distinct_words = n_distinct(word)) %>%
  ungroup() %>%
  spread(sentiment, distinct_words) %>%
  mutate(lexicon = color_tile("lightgreen", "lightgreen")(lexicon),
         words_in_lexicon = color_bar("orange")(words_in_lexicon)) %>%
  style_table(caption = "Count of Words Associated to Sentiment per Lexicon")
        #nrc appears to have a wider variety of words and sentiments 

  
# How many words are used per Broadcast:
climate_text %>% 
  unnest_tokens(word, text) %>% anti_join(stop_words) %>%
  group_by(show) %>% summarise(total_words = n(),
                               unique_words = n_distinct(word))


# Deciding which Lexicon to use based on how many words match those of the Lexicon:
broadcast_tidy <- 
  climate_text %>%
  unnest_tokens(word, text) %>% anti_join(stop_words)


broadcast_tidy %>%
  mutate(words_in_broadcast = n_distinct(word)) %>% # column denotes total number of words (repeated value)
  inner_join(new_sentiments) %>% # attach new_sentiment df to match broadcast_tidy words 
  group_by(lexicon, words_in_broadcast, words_in_lexicon) %>%
  summarise(match_with_lexicon = n_distinct(word)) %>%
  ungroup() %>%
  mutate(match_ratio = match_with_lexicon / words_in_broadcast,
         total_match = sum(match_with_lexicon)) %>%
  select(lexicon, words_in_broadcast, match_with_lexicon, match_ratio) %>%
  mutate(lexicon = color_tile("lightgreen", "lightgreen")(lexicon),
         match_with_lexicon = color_bar("lightblue")(match_with_lexicon)) %>%
  style_table(caption = "Match of Words Used in the Broadcasts to the Types of Lexicon")
      # Confirmed: Lexicon NRC has the highest match of words to lexicon words.


# Trying tables:
library(kableExtra)

kable_table <- function(dataset, number_of_rows){
  dataset %>%
    head(number_of_rows) %>%
    kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

}

climate_text %>% kable_table(number_of_rows = 10)

library(DT)

datatable(climate_text)
