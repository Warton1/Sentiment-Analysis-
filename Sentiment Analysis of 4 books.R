library(ggplot2)
library(tidytext)
library(tidyverse)
library(stm)
library(topicmodels)
library(stringr)
library(gutenbergr)
library(janitor)
library(wordcloud)
library(tidytext)
library(gutenbergr)
library(janeaustenr)

#	Extract the full dataset for all four books together
gutenberg_download
full_collection <- gutenberg_download(c(219,43,844,4300),
                                        meta_fields = "title")
#	Tokenize the dataset of all four books together using the unnest_tokens command and create a tidy dataset
full_collection %>%
  unnest_tokens(word, text)

# Tidying Pride and Prejudice

library(gutenbergr)

head(full_collection)
class(full_collection)
dim(full_collection)
tidy_book <- full_collection %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text)
glimpse(tidy_book)
count_tidy_book <- tidy_book %>%
  count(word, sort = TRUE)
head(count_tidy_book)

#  Remove stop words
get_stopwords()
get_stopwords(language = "es")
get_stopwords(language = "pt")
get_stopwords(source = "smart")

# Plot the top 20 common words in the collection of books
tidy_book %>%
  anti_join(get_stopwords(source = "smart")) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(fct_reorder(word, n), n)) +
  geom_col() +
  coord_flip()
tidy_book


##############For the next set of analysis you have to create a tidy set for each separately#######################
# Create Tidy dataset for 219
Joseph_Conrad <- gutenberg_download(c(219),
                                      meta_fields = "title")
Joseph_Conrad %>%
  unnest_tokens(word, text)

# Create Tidy dataset for 43
Robert_Louis_Stevenson <- gutenberg_download(c(43),
                                   meta_fields = "title")
Robert_Louis_Stevenson %>%
  unnest_tokens(word, text)

# Create Tidy dataset for 844
Oscar_Wilde <- gutenberg_download(c(844),
                                   meta_fields = "title")
Oscar_Wilde %>%
  unnest_tokens(word, text)

# Create Tidy dataset for 4300
James_Joyce <- gutenberg_download(c(4300),
                                   meta_fields = "title")
James_Joyce %>%
  unnest_tokens(word, text)

## 	Tokenize each book and create tidy datasets making sure that you use stop words. Create tidy data sets for each book by the author making sure that you use the unnest_tokens command and stop words
joseph_conrad <- gutenberg_download(c(219))
tidy_joseph_conrad <- joseph_conrad %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_joseph_conrad %>% 
  count(word, sort = TRUE)

robert_louis_stevenson <- gutenberg_download(c(43))
tidy_robert_louis_stevenson <- robert_louis_stevenson %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_robert_louis_stevenson %>% 
  count(word, sort = TRUE)

oscar_wilde <- gutenberg_download(c(844))
tidy_oscar_wilde<- oscar_wilde %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_oscar_wilde %>% 
  count(word, sort = TRUE)

james_joyce <- gutenberg_download(c(4300))
tidy_james_joyce <- james_joyce %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_james_joyce %>% 
  count(word, sort = TRUE)

# 	Use bind_rows to stack the four datasets and create frequency counts of the word distributions after calculating proportions.
frequency <- bind_rows(mutate(tidy_joseph_conrad, author = "Joseph Conrad"),
                       mutate(tidy_robert_louis_stevenson, author = "Robert Louis Stevenson"),
                       mutate(tidy_oscar_wilde, author = "Oscar Wilde"),
                       mutate(tidy_james_joyce, author = "James Joyce"))%>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%     #Compute proportions
  select(-n) %>%                          #Drop n
  spread(author, proportion) %>%          #Reshape long dataset into wide
  gather(author, proportion, `James Joyce`)     #

class(frequency)
dim(frequency)
names(frequency)
tabyl(frequency$author)
head(frequency)

# Create word frequency plots for each of the three authors using James Joyce as the standard as we demonstrated using Jane Austen in the Chapter 1 code
library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Joseph Conrad`, 
                      color = abs(`Joseph Conrad` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Joseph Conrad", x = NULL)
```
ggplot(frequency, aes(x = proportion, y = `Robert Louis Stevenson`, 
                      color = abs(`Robert Louis Stevenson` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Robert Louis Stevenson", x = NULL)
```
ggplot(frequency, aes(x = proportion, y = `Oscar Wilde`, 
                      color = abs(`Oscar Wilde` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Oscar Wilde", x = NULL)
```

#	Compute correlations between James Joyce against each of the three other authors. Use the code examples in Chapter 1
cor.test(data = frequency[frequency$author == "James Joyce",], 
         ~ proportion + `Joseph Conrad`)
cor.test(data = frequency[frequency$author == "James Joyce",], 
         ~ proportion + `Robert Louis Stevenson`)
cor.test(data = frequency[frequency$author == "James Joyce",], 
         ~ proportion + `Oscar Wilde`)


####################### Part 2####################################
#Invoke the required library packages
library(ggplot2)
library(tidytext)
library(tidyverse)
library(stm)
library(topicmodels)
library(stringr)
library(gutenbergr)
library(janitor)
library(wordcloud)
library(janeaustenr)
library(dplyr)
library(stringr)

install.packages("textdata")
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# tidy books

tidy_james_joyce

# positive word
bing_word_counts <- tidy_james_joyce %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

# 	Calculate the tf_idf for all the tokens in the combined dataset. Use the bind_tf-idf function. 

full_collection <- gutenberg_download(c(219,43,844,4300),
                                      meta_fields = "title")
full_collection 

# tidy_full_collection  
tidy_collection <- full_collection %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text)

tidy_collection%>% 
  count(word, sort = TRUE)

# Set up the book_words, only left word, book name, and number of word)
book_words <- full_collection %>%
  unnest_tokens(word, text) %>%
  count(title, word, sort = TRUE)

total_words <- book_words %>%
  group_by(title)%>%
  summarize(total = sum(n))

book_words

# The bind_tf_idf function
book_tf_idf <- book_words %>%
  bind_tf_idf(word, title, n)

book_tf_idf

# show top 10 word
book_tf_idf %>%
  arrange(desc(tf_idf))

book_tf_idf


#	Plot the tf_idf for each book separately using the combined dataset as column plots. 

book_tf_idf %>%
  group_by(title) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

book_tf_idf %>%
  group_by(title) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
  