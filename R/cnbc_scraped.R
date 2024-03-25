#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rvest)

#Data Import and Cleaning
urls <- c(
  "Business" = "https://www.cnbc.com/business/",
  "Investing" = "https://www.cnbc.com/investing/",
  "Tech" = "https://www.cnbc.com/technology/",
  "Politics pages" = "https://www.cnbc.com/politics/"
)

cnbc_tbl <- NULL  # Empty table for storing the result


for (source in names(urls)) {
  page <- read_html(urls[source]) #read each urls

  headlines <- page %>%
    html_nodes(".Card-title") %>% #scrap headlines
    html_text(trim = TRUE)
  
  length <- str_count(headlines, "\\S+") #count the number of words("\\S+")
  
  result <- tibble(
    headline = headlines,
    length = length,
    source = source
  )#create a table result for each source 
  
  cnbc_tbl <- bind_rows(cnbc_tbl, result)  #combine 4 source result
}

#display the table result
head(cnbc_tbl)

# Visualization
ggplot(cnbc_tbl, aes(x = source, y = length)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average Headline Length by Source",
       x = "Source",
       y = "Average Headline Length") 

#Analysis
anova_result<-anova(aov(length~source, data=cnbc_tbl))

#Publication
#The results of an ANOVA comparing lengths across sources was F(3, 130) = 0.80, p = .50. This test was not statistically significant.

f_stat<-sprintf("%.2f", anova_result$`F value`[1])
dfn <- anova_result$Df[1]  
dfd <- anova_result$Df[2] 
p<-sub("^(-?)0.", "\\1.",sprintf("%.2f", anova_result$`Pr(>F)`[1]))
significant<- if_else(anova_result$`Pr(>F)`[1] < 0.05, "was","was not")

cat(sprintf("The results of an ANOVA comparing lengths across sources was F(%d, %d) = %s, p = %s. This test %s statistically significant.", dfn, dfd, f_stat, p, significant))

