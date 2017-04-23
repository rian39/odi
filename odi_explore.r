library(dplyr)
library(ggplot2)
library(knitr)
library(readr)
library(stringr)
library(tidyr)
library(tidytext)

options(tibble.width = Inf) 
data('stop_words')

odi  <-  read_csv('open-data-certificates.csv')
colnames(odi)
#  [1] "title"              "url"                "jurisdiction"       "status"             "level"              "created_at"         "certification_type"
#  [8] "badge_javascript"   "badge_html"         "badge_image"        "dataset_title"      "dataset_url"        "publisher"          "data_license"      
# [15] "content_license"    "documentation_url" 

# status of datasets

odi %>% select(status) %>% count(status, sort=TRUE)
#   status      n
# 1   beta 140194
# 2  final  11542
# 3  alpha     98

odi %>% select(publisher, status, title) %>%
    filter(status=='final') %>%
    group_by(publisher) %>% 
    count(publisher, sort=TRUE) %>%
    print(n=70)

## jurisdiction or country

odi %>% select(jurisdiction) %>% count(jurisdiction, sort=TRUE)

#    jurisdiction      n
# 1            US 140128
# 2            GB  11565
# 3            AU     75
# 4            GR     31
# 5            CZ     12
# 6            CH      6
# 7            IT      6
# 8            FR      2
# 9            KR      2
# 10           CA      1
# 11           CO      1
# 12           ES      1
# 13           HR      1
# 14           RU      1
# 15           SK      1
# 16           TN      1

## url analysis

odi %>% select(title, documentation_url) %>%
    filter(str_detect(documentation_url, pattern="data.gov")) %>%
    count() %>%
    print(n=10)

## titles word analysis

odi_words  <- odi %>%
    select(dataset_title, publisher) %>%
    group_by(publisher) %>%
    unnest_tokens(word, dataset_title) %>% 
    mutate(word = str_extract(word, "[a-zA-Z']+")) %>% 
    anti_join(stop_words)

odi_words
#                    publisher      word
# 1      Food Standards Agency  chickens
# 2      Food Standards Agency  helpline
# 3      Food Standards Agency    yammer
# 4  Runnymede Borough Council runnymede
# 5  Runnymede Borough Council   stroude
# 6  Runnymede Borough Council     meads
# 7  Runnymede Borough Council  chertsey
# 8  Runnymede Borough Council       wey
# 9  Runnymede Borough Council       sho
# 10 Runnymede Borough Council       sho
# # ... with 1,363,374 more rows

odi_words %>% count(word, sort=TRUE) %>%
    filter(str_detect(publisher, pattern = 'Food Standards Agency'), n >= 5) %>%
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


odi %>% select(publisher, dataset_title) %>%
    filter(str_detect(publisher, pattern='Home Office')) %>%
    print(n=70)

odi %>% select(publisher, dataset_title) %>%
    filter(str_detect(publisher, pattern='Environment, Food and Rural')) %>%
    print(n=70)

odi %>% select(publisher, dataset_title) %>%
    filter(str_detect(publisher, pattern='Health and Social Care Information')) %>%
    print(n=70)

odi %>% select(publisher, dataset_title) %>%
    filter(str_detect(publisher, pattern='Culture, Arts')) %>%
    print(n=70)

culture  <- odi %>% select(publisher, dataset_title) %>%
    filter(str_detect(publisher, pattern='Culture, Arts')) %>%
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

