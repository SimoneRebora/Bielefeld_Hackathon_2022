

# ok! you tried with the data provided, but of course every corpus is different!

# can you do the same with yours?


# 1. create a folder in the directory Day1_practice_GG and add your txt files there. Call it "my_corpus".
# if you do not have a corpus yet, that's ok, too. 
# Just go to project gutenberg and download 6 or 7 novels of your interst in a txt file format.

# remember: if you want to use the same script, your texts must be saved with the filename
# "surname_title_year.txt"

# 2. use the scripts from practice_1A to repeat the analysis with your data.
# Paste the scripts here, and change the name of the directories paths 
# (whenever the practice 1 script featured "corpus", it is going to become "my_corpus. 
# Also, subsitute "austen_corpus" and"austen_SA" with names of your choice.

# 3. remember that we split the novels arbitrarily into 15 chapters.
# If you need more or less "fake chapters", change the number in the appropriate section.

# 4. don't forget to comment! you want to be able to recall what you did at all times, so introduce your code with some comment, and add comment for the results, if you want,

recepcion_files <- list.files(pattern = ".*.txt")
recepcion_fuente <- recepcion_files %>%
  set_names(.) %>%
  map_df(read.delim, fileEncoding = "utf-8", .id = "FileName", header = F)%>%
 rename(text = V1)
recepcion_cortado <- recepcion_fuente %>%
  separate(FileName, into = c("author", "title", "year"), sep = "_", remove = T) %>% 
  mutate(year = str_remove(str_trim(year, side = "both"), ".txt"))
recepcion_cortado <- recepcion_cortado %>% 
  group_by(title) %>%
  mutate(sentence_id = seq_along(text)) %>% ungroup() %>%
  select(author, title, year,sentence_id,
         text) %>%
  unnest_tokens(word, text, to_lower = T) 
recepcion_cortado <- recepcion_cortado %>% group_by(title, sentence_id) %>% mutate(word_id = seq_along(word)) %>% ungroup() %>%
  mutate(unique_word_id = seq_along(word))

fs=12 #Olvidé poner esto la primera vez. Define el tamaño de la fuente. Es importante indicarlo en el "environment" para que luegop se ejecuten correctamente las gráficas 

test <- recepcion_cortado %>%
  ungroup() %>%
  group_split(title)

test2 = list()

for (i in 1:length(test)) {
  avg_ch_lenght <- nrow(test[[i]])/15
  r  <- rep(1:ceiling(nrow(test[[i]])/avg_ch_lenght),each=avg_ch_lenght)[1:nrow(test[[i]])]
  test2[[i]] <- split(test[[i]],r)
}


for (i in 1:length(test2)) {
  for (j in 1:length(test2[[i]])) {
    test2[[i]][[j]]$chapter <- paste0(j)
  }
}

test = list()

for (i in 1:length(test2)) {
  test[[i]] <- data.table::rbindlist(test2[[i]])
}

recepcion_cortado <- data.table(rbindlist(test))


remove(test, test2, j,i,r,avg_ch_lenght)



recepcion_cortado %>%
  group_by(title, word) %>%
  anti_join(stop_words, by = "word") %>% 
  count() %>% 
  arrange(desc(n)) %>% # highest freq on top
  group_by(title) %>% #
  mutate(top = seq_along(word)) %>% # identify rank within group filter(top <= 15) %>% # retain top 15 frequent words
  
  ggplot(aes(x = -top, fill = title)) +
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure words are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = word), size = fs/3, hjust = "left") + theme(legend.position = 'none', # get rid of legend
                                                                    text = element_text(size = fs), # determine fs
                                                                    axis.text.x = element_text(angle = 45, hjust = 1, size = fs/1.5), # rotate x text axis.ticks.y = element_blank(), # remove y ticks
                                                                    axis.text.y = element_blank()) + # remove y text
  labs(y = "Word count", x = "", # add labels
       title = "Austen: Most frequent words throughout the novels") +
  facet_grid(. ~ title) + # separate plot for each title coord_flip() + # flip axes
  scale_fill_sjplot()


#Intento ahora meter la lista de stopwords española
recepcion_cortado %>%
  group_by(title, word) %>%
  anti_join("C:/Home/mallet-2.0.8/stoplists/es.txt") %>% # delete stopwords
  count() %>% # summarize count per word per title
  arrange(desc(n)) %>% # highest freq on top
  group_by(title) %>% # 
  mutate(top = seq_along(word)) %>% # identify rank within group
  filter(top <= 15) %>% # retain top 15 frequent words
  # create barplot
  ggplot(aes(x = -top, fill = title)) + 
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure words are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = word), size = fs/3, hjust = "left") +
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
