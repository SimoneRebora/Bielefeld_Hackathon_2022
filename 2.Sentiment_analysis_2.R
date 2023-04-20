
# Where do I start? Create your corpus and set up your data with R Studio -----



# This is an R script file, created by Giulia (reads like English "Julia")

# Everything written after an hashtag is a comment (normally appears in green). If you don't want to type the hash manually every time, you can type your comments normally and after you finish, with the cursor on the sentence, press ctrl+shift+c. it will turn text into a comment and vice versa.

# Everything else is R code. To execute the code, place the cursor on the corresponding line and press Ctrl+Enter (windows)


## load packages ----------

# Ok, let's start! Before you begin you will need to load some packages. These allow you to execute specific operations.
# If you have not done so already, you have to install them first: it might take a few minutes and you only have to do it once. If R asks you whether you want to install dependencies for the packages, say yes.
# 
# install.packages("tidyverse")
# install.packages("readr")
# install.packages("data.table")
# install.packages("tm")
# install.packages("tidytext")
# install.packages("syuzhet")
# install.packages("sjPlot")
# install.packages("wordcloud")
# install.packages("textdata")

# Once you have installed the packeges you can comment the installation code like this:

#   install.packages("blablabla")

# so this operation will not be execute again in the future.


library(tidyverse)
library(readr)
library(data.table)
library(syuzhet)
library(tm)
library(tidytext)
library(sjPlot)
library(wordcloud)
library(textdata)
library(readtext)



## set options --------------------

# we can start setting up a few options for our project

options(stringsAsFactors = F, # do not convert to factor upon loading
        scipen = 999, # do not convert numbers to e-values
        max.print = 200, # stop printing after 200 values
        warn = -1) # as many warnings in R are useless (and annoying), you might want to disable them


theme_set(theme_sjplot2()) # set default ggplot theme to light
fs = 12 # default plot font size


## files import ----------------- 

# in this case, the files are consistently saved as author_title_year.txt, where we use one word only for the author and for the title, and the format YYYY for the year of first publication.

# It is important to be consistent! It can make your life much easier when you deal with many texts.

# this tells R to look only for txt files in the working directory. (that's why we had to change it. we will set it back to the previous WD later)


corpus_source <- readtext("corpus/*.txt", encoding = "UTF-8") %>%
  mutate(text = gsub("\\s+"," ", text))  %>%
  as_tibble() %>%
  mutate(doc_id = stringr::str_remove(doc_id, ".txt")) %>%
  left_join(readtext("corpus/ELTeC-eng_metadata.tsv",
                     docid_field = "filename") %>%
              rename(author = `author.name`, # i want to rename wome variables
                     author_gender = `author.gender`,
                     year = `first.edition`) %>%
              select(doc_id, # let's just preserve a few metadata infos
                     author,
                     title,
                     author_gender,
                     year,
                     -text) %>%
              as_tibble()
  ) %>%
  group_by(author_gender) %>%
  sample_n(5) %>% # for this session, let's limit the corpus to 10 texts (5 for each gender)
  ungroup() 



# let's have a look at out dataset now

head(corpus_source)


corpus_sentences <- corpus_source %>%
  unnest_sentences(input = text, output = sentence, drop = T, to_lower = F) %>%
  group_by(doc_id) %>%
  mutate(sentence_id = seq_along(sentence)) %>%
  ungroup() %>%
  mutate(unique_setence_id = seq_along(sentence))


corpus_tokens <- corpus_sentences %>%
  unnest_tokens(input = sentence, output = token, drop = T, to_lower = F) %>%
  group_by(doc_id, sentence_id) %>%
  mutate(token_id = seq_along(token)) %>%
  ungroup() %>%
  mutate(unique_token_id = seq_along(token))

head(corpus_tokens)



# Corpus overview --------
## plot word frequency ---------------------- 

# now we can have a first look at our corpus and see which words are most frequent in the novels

corpus_tokens %>%
  group_by(title, token) %>%
  anti_join(as_tibble(stopwords("en")), by = c("token"="value")) %>% # delete stopwords
  count() %>% # summarize count per word per title
  arrange(desc(n)) %>% # highest freq on top
  group_by(title) %>% # 
  mutate(top = seq_along(token)) %>% # identify rank within group
  filter(top <= 15) %>% # retain top 15 frequent words
  # create barplot
  ggplot(aes(x = -top, fill = title)) + 
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure words are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = token), size = fs/3, hjust = "left") +
  theme(legend.position = 'none', # get rid of legend
        text = element_text(size = fs), # determine fs
        axis.text.x = element_text(angle = 45, hjust = 1, size = fs/1.5), # rotate x text
        axis.ticks.y = element_blank(), # remove y ticks
        axis.text.y = element_blank()) + # remove y text
  labs(y = "Word count", x = "", # add labels
       title = "Most frequent words throughout the novels") +
  facet_grid(. ~ title) + # separate plot for each title
  coord_flip() + # flip axes
  scale_fill_sjplot()

# relatively unsurprisingly, names of characters are generally the most frequent tokens. To see what other tokens are highly frequent, we can for example import a list of first and last names, so that we can exclude them from the plot.


first_names <- read_table("scripts/first_names.txt") %>%
  rename(token = word)

last_names <- read_table("scripts/last_names.txt")  %>%
  rename(token = word)


# let's see then how it looks without those

corpus_tokens %>%
  anti_join(first_names) %>%
  anti_join(last_names) %>%
  group_by(title, token) %>%
  anti_join(as_tibble(stopwords()), by = c("token"="value")) %>% # delete stopwords
  count() %>% # summarize count per word per title
  arrange(desc(n)) %>% # highest freq on top
  group_by(title) %>% # 
  mutate(top = seq_along(token)) %>% # identify rank within group
  filter(top <= 15) %>% # retain top 15 frequent words
  # create barplot
  ggplot(aes(x = -top, fill = title)) + 
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure words are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = token), size = fs/3, hjust = "left") +
  theme(legend.position = 'none', # get rid of legend
        text = element_text(size = fs), # determine fs
        axis.text.x = element_text(angle = 45, hjust = 1, size = fs/1.5), # rotate x text
        axis.ticks.y = element_blank(), # remove y ticks
        axis.text.y = element_blank()) + # remove y text
  labs(y = "Word count", x = "", # add labels
       title = "Austen: Most frequent words throughout the novels") +
  facet_grid(. ~ title) + # separate plot for each title
  coord_flip() + # flip axes
  scale_fill_sjplot()

# can you see any interesting pattern now?



# Sentiment analysis --------------

# "so, now, what about the sentiments?", you might ask?



## lexicons -----------

# first we need to decide which lexicons we can use for sentiment analysis

# in this example, we will use three popular lexicons, namely the AFINN, NRC and BING. For the sake of simplicity, we will simply use the versions provided by the syuzhet package.
# we can therefore match these onto our corpus directly with the function called get_sentiments, which is included in the syuzhet package. Rather than loading the sentiment lexicons, it applies it directly to the corpus.

novels_SA <- bind_rows(
  # 1 AFINN 
  novels_corpus %>% 
    inner_join(get_sentiments("afinn"), by = "word")  %>%
    # filter(value != 0) %>% # delete neutral words
    mutate(sentiment = ifelse(value < 0, 'negative', 'positive')) %>% # identify sentiment
    mutate(value = sqrt(value ^ 2)) %>% # all values to positive
    group_by(title, sentiment) %>% 
    mutate(dictionary = 'afinn'), # create dictionary identifier
  
  # 2 BING 
  novels_corpus %>% 
    inner_join(get_sentiments("bing"), by = "word") %>%
    group_by(title, sentiment) %>%
    mutate(dictionary = 'bing'), # create dictionary identifier
  
  # 3 NRC 
  novels_corpus %>% 
    inner_join(get_sentiments("nrc"), by = "word") %>%
    group_by(title, sentiment) %>%
    mutate(dictionary = 'nrc'),
  
)

# in this case, we have performed an "inner_join" function from the package tidyverse. this means that the combination of our corpus and the lexicons will only preserve the words for which a match exist, i.e. only words with a sentiment value.

# if you want to preserve the whole corpus, with NA values for "empty" matches, you can use "letf_join" instead. give it a try and see how it changes!


## let's have a look at our corpus

novels_SA %>% head()



# we can have a look at the most frequent words with a sentiment value using a wordcloud. we can look at these all together, or select one specific sentiment for the wordcloud.

## wordclouds by sentiment ---------------

# positive sentiments

novels_SA %>%
  anti_join(stop_words, by = "word") %>% # delete stopwords
  filter(sentiment=="positive") %>% # here is where we can select what to look at
  group_by(word) %>%
  count() %>% # summarize count per word
  mutate(log_n = sqrt(n)) %>% # take root to decrease outlier impact
  with(wordcloud(word, 
                 log_n,
                 max.words = 50, 
                 colors=brewer.pal(5, "Dark2"),
                 random.order = F,
                 ))

# negative sentiments

novels_SA %>%
  anti_join(stop_words, by = "word") %>% # delete stopwords
  filter(sentiment=="negative") %>% # here is where we can select what to look at
  group_by(word) %>%
  count() %>% # summarize count per word
  mutate(log_n = sqrt(n)) %>% # take root to decrease outlier impact
  with(wordcloud(word, 
                 log_n,
                 max.words = 50, 
                 colors=brewer.pal(5, "Dark2"),
                 random.order = F,
  ))


# it looks pretty correct, right? a lot of "love" for positive sentiments and quite some doubt and death for negative ones
# You might have noticed that "miss" is by far the biggest (and therefore more frequent) term in the "negative cloud. With some sense, we can probably understand that there is a chance this is a "mistake": "to miss" as a verb might be negative, but it is possible that "miss" would not really be a negative term in Austen's novels when referring to a young woman.
# we can see how the graph looks like without it.

novels_SA %>%
  anti_join(stop_words, by = "word") %>% # delete stopwords
  filter(word != "miss") %>%
  filter(sentiment=="negative") %>% # here is where we can select what to look at
  group_by(word) %>%
  count() %>% # summarize count per word
  mutate(log_n = sqrt(n)) %>% # take root to decrease outlier impact
  with(wordcloud(word, 
                 log_n,
                 max.words = 50, 
                 colors=brewer.pal(5, "Dark2"),
                 random.order = F,
  ))

# better, right?





## dictionaries comparison -----------------
# we might want to see if the different lexicons perform differntly.
# the three elxicons share the negative/positive value, so let's focus on that

novels_SA %>%
  filter(sentiment == "negative" | sentiment == "positive") %>%
  group_by(word, sentiment, dictionary) %>%
  count() %>% # summarize count per word per sentiment
  group_by(sentiment) %>%
  arrange(sentiment, desc(n)) %>% # most frequent on top
  mutate(top = seq_along(word)) %>% # identify rank within group
  filter(top <= 15) %>% # keep top 15 frequent words
  ggplot(aes(x = -top, fill = factor(sentiment))) + 
  # create barplot
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure words are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = word), size = fs/3, hjust = "left") +
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs), # determine fs
        axis.text.x = element_text(angle = 45, hjust = 1), # rotate x text
        axis.ticks.y = element_blank(), # remove y ticks
        axis.text.y = element_blank()) + # remove y text
  labs(y = "Word count", x = "", # add manual labels
       title = "Frequency of words carrying sentiment",
       subtitle = "Using tidytext and the AFINN, bing, and nrc sentiment dictionaries") +
  facet_grid(sentiment ~ dictionary) + # separate plot for each sentiment
  coord_flip()  + # flip axes
  scale_fill_sjplot()


## NER also has discrete emotions, we might want to focus on them separately

novels_SA %>%
  filter(dictionary == "nrc") %>%
  filter(!sentiment %in% c('negative','positive')) %>%
  group_by(word, sentiment, dictionary) %>%
  count() %>% # summarize count per word per sentiment
  group_by(sentiment) %>%
  arrange(sentiment, desc(n)) %>% # most frequent on top
  mutate(top = seq_along(word)) %>% # identify rank within group
  filter(top <= 15) %>% # keep top 15 frequent words
  ggplot(aes(x = -top, fill = factor(sentiment))) + 
  # create barplot
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure words are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = word), size = fs/3, hjust = "left") +
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs), # determine fs
        axis.text.x = element_text(angle = 45, hjust = 1), # rotate x text
        axis.ticks.y = element_blank(), # remove y ticks
        axis.text.y = element_blank()) + # remove y text
  labs(y = "Word count", x = "", # add manual labels
       title = "Frequency of words carrying sentiment",
       subtitle = "nrc sentiment dictionary") +
  facet_grid(. ~ sentiment) + # separate plot for each sentiment
  coord_flip()  + # flip axes
  scale_fill_sjplot()



# these explorations may reveal interesting patterns in the novels, as well as flows in the sentiment lexicons. A "perfect" lexicons does not exist, but more and more researchers are working to develop better and more reliable ones.




# sentiment across chapters --------

# another thing you might want to do is to have a look at how sentiment evolves acrosss a narrative.
# we can do so with some graphs:


plot_sentiments <- novels_SA %>%
  group_by(dictionary, sentiment, title, chapter) %>%
  summarize(value = sum(value), # summarize AFINN values
            count = n(), # summarize bing and nrc counts
            # move bing and nrc counts to value 
            value = ifelse(is.na(value), count, value))  %>%
  filter(sentiment %in% c('positive','negative')) %>%   # only retain bipolar sentiment
  mutate(value = ifelse(sentiment == 'negative', -value, value)) %>% # reverse negative values
  # create area plot
  ggplot(aes(x = as.numeric(chapter), y = value)) +    
  geom_area(aes(fill = value > 0),stat = 'identity') +
  # add black smoothed line without standard error
  geom_smooth(method = "loess", se = F, col = "black") + 
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs)) + # change font size
  labs(x = "Chapter", y = "Sentiment value", # add labels
       title = "Sentiment across novels chapters",
       subtitle = "Using tidytext and the AFINN, bing, and nrc sentiment dictionaries") +
  # separate plot per title and dictionary and free up x-axes
  facet_grid(title ~ dictionary, scale = "free_x") +
  scale_fill_sjplot()


plot_sentiments # let's see the plot

# we can also zoom in the plot

plot_sentiments + coord_cartesian(ylim = c(-100,200)) 



## nrc emotions across novels ----------------------------


novels_SA %>% 
  filter(dictionary == "nrc") %>%
  filter(!sentiment %in% c('negative','positive')) %>%
  group_by(sentiment, title, chapter) %>%
  count() %>% # summarize count
  # create area plot
  ggplot(aes(x = as.numeric(chapter), y = n)) +
  geom_area(aes(fill = sentiment), stat = 'identity') + 
  # add black smoothing line without standard error
  geom_smooth(aes(fill = sentiment), method = "loess", se = F, col = 'black') + 
  theme(legend.position = 'none', # remove legend
        text = element_text(size = fs)) + # change font size
  labs(x = "Chapter", y = "Emotion value", # add labels
       title = "Jane Austen: Emotions in the novels",
       subtitle = "Using tidytext and the nrc sentiment dictionary") +
  # separate plots per sentiment and title and free up x-axes
  facet_grid(title ~ sentiment, scale = "free_x") +
  scale_fill_sjplot()

