#! /usr/bin/env Rscript
library("tidyverse")
#install.packages("tm")
#install.packages("wordcloud")
library(wordcloud)
list.files("../")
dataTypes<-read_csv("../altdata.csv") %>% 
  select(DataType) %>% 
  mutate(words=DataType) %>% 
  group_by(words) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) 
write.csv(dataTypes, "../altdata_freq.csv")
wc_dataTypes<-read_csv("../altdata_freq_rus.csv")
png(filename="altDataTypes.png")
wordcloud(wc_dataTypes$words, wc_dataTypes$n)
dev.off()
dataTypes
dataVendors<-read_csv("../altdata.csv") %>% 
  select(DataVendor) %>% 
  group_by(DataVendor) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) 
dataVendors
  
png(filename="altDataVendorTypes.png")
wordcloud(words = dataTypes$words, freq = dataTypes$n)
dev.off()  
