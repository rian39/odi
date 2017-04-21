library(knitr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(readr)
options(tibble.width = Inf) 
data('stop_words')

odi  <-  read_csv('open-data-certificates.csv')
colnames(odi)

## url analysis

odi$documentation_url

## titles analysis

odi_words  <- odi %>%
    select(dataset_title, publisher) %>%
    group_by(publisher) %>%
    unnest_tokens(word, dataset_title) %>% 
    mutate(word = str_extract(word, "[a-zA-Z']+")) %>% 
    anti_join(stop_words)

odi_words

odi_words %>% count(word, sort=TRUE) %>%
    filter(str_detect(publisher, pattern = 'Ministry of Defence'), n >= 5) %>%
    mutate(word=reorder(word,n)) %>%
    ggplot(aes(word, n)) +
    geom_col() + 
    xlab(NULL) + 
    coord_flip()

## publisher analysis 

odi %>% select(publisher) %>% 
    count(publisher, sort=TRUE) %>%
    print(n=50)

odi %>% select(publisher, dataset_title) %>%
    filter(str_detect(publisher, pattern='Ministry of Defence')) %>%
    print(n=50)

odi %>% select(publisher, dataset_title) %>%
    filter(str_detect(publisher, pattern='Food Standards Agency')) %>%
    print(n=70)


odi %>% select(publisher, dataset_title, dataset_url) %>%
    filter(str_detect(publisher, pattern='Home Office')) %>%
    print(n=70)

## when are things published
odi_dates  <- odi %>%
    select(title, publisher, created_at) %>% 
    group_by(publisher) %>% 
    mutate(created_at = as.Date(created_at)) %>%
    count(created_at, sort=FALSE) 

odi_dates

odi_dates %>% ggplot(aes(x= created_at, y=n, colour = publisher )) + 
   geom_jitter(alpha=0.5)  + 
   scale_y_log10() + 
   theme(legend.position='none')

