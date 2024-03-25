#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(jsonlite)
library(ggplot2)

#Data Import and Cleaning
rstats<-fromJSON("https://www.reddit.com/r/rstats.json")

rstats_tbl<-data.frame(post=rstats$data$children$data$title,
                       upvotes=rstats$data$children$data$ups,
                       comments=rstats$data$children$data$num_comments)

#Visualization
plot1<-ggplot(rstats_tbl, aes(upvotes, comments))+
  geom_point()+
  labs(x="The number of upvotes", 
       y="The number of comments", 
       titled="The relationship between upvotes and comments")
plot1

#Analysis
cor_upvotes_comment<-cor(rstats_tbl$upvotes, rstats_tbl$comment)
cor_upvotes_comment

p_upvotes_comment<-cor.test(rstats_tbl$upvotes, rstats_tbl$comments)
p_upvotes_comment$p.value

#Publication
#"The correlation between upvotes and comments was r(23) = .21, p = .32. This test was not statistically significant."
df<-p_upvotes_comment$parameter
cor<-sub("^(-?)0.", "\\1.", sprintf("%.2f", cor_upvotes_comment))
p<-sub("^(-?)0.", "\\1.", sprintf("%.2f", p_upvotes_comment$p.value))
significant<- if_else(p_upvotes_comment$p.value < 0.05, "was","was not")

sprintf("The correlation between upvotes and comments was r(%d) = %s, p = %s. This test %s statistically significant.", df, cor, p, significant)

