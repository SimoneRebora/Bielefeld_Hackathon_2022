# Where do I start? Create your corpus and set up your data with R Studio -----



# This is an R script file, created by Giulia (reads like English "Julia")

# Everything written after an hashtag is a comment (normally appears in green). If you don't want to type the hash manually every time, you can type your comments normally, and after you finish, with the cursor on the sentence, press ctrl+shift+c. it will turn text into a comment and vice versa.

# Everything else is R code. To execute the code, place the cursor on the corresponding line and press Ctrl+Enter (windows)

# If you are in the beginners' group, for today's practice you will not need much knowledge of R: the scripts are provided for you. You will be guided through a simple case exploratory Sentiment Analysis, and then use those same scripts to experiment with data in your possess or of your choice.
# If you are unfamiliar with R language and basic operations and want to learn more about it, there is plenty of tutorials online. Have a look at the resources at the end of this script for a few recommendations.

# before you start, check the working directory!
# you can click on the Files panel, go to the Bielefeld_Hackaton_2022 folder, and once you are inside click on the little arrow near the "More" button, and select "Set as working directory"


# now we're ready to start!




# PS: Have you noticed that there is a little symbol on the top right of this panel, made of little horizontal lines? It shows and hide the document outline. if you write a comment and put a series of --------- (any number, more than 4) after it, the comment will become the header of a section, which you can then see in the outline for an easier navigation of the script.



# Creating your dataset ----------

# Often one of the factors that prevents us humanists from doing computational analysis is that tutorials sometimes assume that a certain amount of knowledge is somehow pre-existing. Unfortunately, it is often not.
# So it happens that right when you want to finally try to adapt someone else's existing scripts to your lovely literary texts (yes, thas's how we often do, and it's ok!), you are not really sure how to put those books into a shape that you can use.

# Here we will try and show how different text formats can be imported in R and made ready for some analysis.

## packages -----


# Before you begin you will need to load some packages. These allow you to execute specific operations.
# If you have not done so already, you have to install them first: it might take a few minutes and you only have to do it once. If R asks you whether you want to install dependencies for the packages, say yes.

install.packages("tidyverse")
install.packages("readr")
install.packages("data.table")
install.packages("tm")
install.packages("tidytext")
install.packages("syuzhet")
install.packages("sjPlot")
install.packages("wordcloud")
install.packages("readxl")

# Once you have installed the packeges you can comment the installation code like this:

#   install.packages("blablabla")

# so this operation will not be execute again in the future.


library(tidyverse)
library(readxl)
library(readr)
library(data.table)
library(syuzhet)
library(tm)
library(tidytext)
library(sjPlot)
library(wordcloud)


# Importing data ----

## txt ----

# One easy way to import texts into R is to start from txt files.

# You might have more than one, so it is important that you store them all together in one folder, and ideally with a consistent filename. Information in the filename can be used later on to add metadata to your dataset. The format "surname_title_year.txt" could be a good option, for example, where the surname and the title have to be one word.

# In order to import a txt file, you can use the "read.delim" function from base R (which means you do not need to install extra packages). 

# let's try it out. As you can see in the files panels, there is a folder called "samples", where some texts in different formats are stored.

# before you execute the code, make sure the working directory is set to 


pride <- read.delim("samples/austen_pride_1813.txt", # this is the url to your file
                    fileEncoding = "utf-8",  # we want to read it as unicode text
                    header = F) %>% # we do not want the first line as a header 
  rename(text = V1) # we can name the column text

# your file has been imported! 
# Have a look at the first rows to see how it looks
# execute the next code cunk or click on the "pride" element in your environment

head(pride)




# when importing a txt file, the paragraphs are automatically converted into new rows. if you want to have a single string instead, you can transform it, telling R to combine the rows, and to add "\n " (a conventional code for a new line) between one piece of string and the next, as follows:

pride_whole <- paste(unlist(pride), collapse ="\n")

head(as_tibble(pride_whole))

pride_whole_tibble <- as_tibble(pride_whole)

# you can then split it into sentences, for instance with packages syuzhet (the result will be a list of strings)

pride_sentences <- get_sentences(pride_whole)

head(as_tibble(pride_sentences))

# or with the package tidytext (this will turn into a dataframe)

pride_sentences <- unnest_sentences(pride, 
                                    input = text,
                                    output = "sentence_text",
                                    to_lower = F)

head(pride_sentences)

## multiple txt files ----------

# if you have more than one text, you probably won't want to repeat this operations manually several times.
# you can then proceed as follows:
# (this is just one way but there are many out there)

# - create a silt of the files inside the folder that match the criteria (txt)

corpus_files <- list.files(path = "samples", pattern = "*.txt", full.names = T)


corpus_source <- corpus_files %>%  
  set_names(.) %>% 
  map_df(read.delim, fileEncoding = "utf-8", 
         .id = "FileName", 
         header = F) %>%
  rename(text = V1)


head(corpus_source)


# let's see which files are in our corpus:

corpus_source %>% 
  select(FileName) %>%
  distinct()


# now, as we mentioned you might want to use the information in the filename to create more variables (that's how "columns" are called in R) in our corpus

corpus <- corpus_source %>%
  mutate(FileName = str_remove_all(FileName, "samples/")) %>% # let's remove the directory from the filename
  separate(FileName, into = c("author", "title", "year"), sep = "_", remove = T) %>% # and separate the metadata
  mutate(year = str_remove(str_trim(year, side = "both"), ".txt")) # and make sure there are no extra spaces before/after the words

corpus$year <- as.numeric(corpus$year)

# click on corpus and see how it looks. Neat, right?

# you might also want to add an identification number for the sentences, which can be useful for later analysis

corpus <- corpus %>%
  group_by(title) %>%
  mutate(sentence_id = seq_along(text)) %>%
  ungroup()



## csv and xslx ----

# another common format for texts is csv or xlsx. Importing a this is very easy, because R understands the csv and xslx formats well. You can either use code, or click directly on the file you want to import in the files panel.
# R studio will ask if you want to import it, and you will be able to determine options with a friendly interface.

# navigate into the samples folder and click on the file small_corpus.xlsx. or 
# execute the following code

pride_excel <- read_excel("samples/pride.xlsx")

# have a look at it

head(pride_excel)


## multiple files ----

# the procedure similar to the one we saw for the txt files, except it has read_excel as function, and it does not need to add a header or other variables

corpus_files <- list.files(path = "samples", pattern = "*.xlsx",  full.names = T)


corpus_source <- corpus_files %>%  
  set_names(.) %>% 
  map_df(read_excel)

head(corpus_source)

## epubs ----------

## another format that is quite popular today, especially for books, is the epub
## for these files you will need a specific package, calle "epubr"

install.packages("epubr")

library(epubr)

kafka_all <- epubr::epub("samples/kafka.epub")

# have a look a the dataset "kafka_all": epubs often have a more complex internal structure, and you might need to modify your dataset according to your needs

kafka_werke <- kafka_all[[9]][[1]]

# Importing sentiment lexicons --------

# Another thing you might want to do is to import the lexicons you will use for your sentiment analysis. Lexicons are often available as part of several packages.

# for example, you may use the syuzhet package to import the bing, nrc, afinn lexicons


syuzhet_bing <- get_sentiment_dictionary("bing")
syuzhet_afinn <- get_sentiment_dictionary("afinn")
syuzhet_nrc <- get_sentiment_dictionary("nrc")



# Dictionaries within packages, however, could be updated or change over time (or become unavailable). 
# You can then decide that you prefer to store them locally, so that you can be sure they do not change and that you will be able to access them whether you are online or not.

# for example, if you go to the official source of the NRC lexicon developed by Saif Mohammad, https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm , you will be able to download the lexicon in several formats.

# A reduced version (only english) of that file has been downloaded for you in the "lexicon" folder. Import it and have a look at it.

NRC_2017_english <- read_excel("lexicons/NRC_2017_english.xlsx")

# you can already see that the two nrc lexicons have differences, so be careful when you pick one and make an informed decision.


# Now you can create your dataset with your own literary texts, and be ready to start an exploration of sentiments!


