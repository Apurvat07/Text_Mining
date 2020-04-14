library("readtext")
library(ggplot2)
setwd("./Fixed_Judgements/")
getwd()
list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE)

#install.packages("tidyverse", 
#                 dependencies = TRUE, repos = "https://cran.rstudio.com")
library(tidyverse)

# Read all the files and create a FileName column to store filenames
df <- list_of_files %>%
  set_names(.) %>%
  map_df(readtext, .id = "FileName")

df<-df[,-1]
colnames(df)<-c("Judgements","Text")

setwd("C:/Users/Apurva Tawde/Downloads/Rule14/Sample Problem/")

Interview_Mapping<-read.csv("Interview_Mapping.csv")

df[,1]<-str_replace(df[,1], ".txt", "")

#Final Data Frame 
df_1<-merge(Interview_Mapping,df,by="Judgements")

#Exploratory analysis

summary(df_1$Area.of.Law,max=100)
# Inside bars
library(dplyr)
library(forcats)
data <- df_1 %>%
  group_by(Area.of.Law) %>%
  summarise(counts = n())

# Create the bar plot. 
data %>%
mutate(Area.of.Law = fct_reorder(Area.of.Law, counts)) %>%
ggplot(aes(x = Area.of.Law, y = counts)) +
  geom_bar(stat="identity", fill="#0073C2FF", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +geom_text(aes(label = counts))
  theme_bw()
  
  
write.csv(df_1,file = "Merged_data.csv",fileEncoding = "utf-8")
