---
title: "Text mining: model Bag of Words"
author: ""
date: ""
output:
   html_document:
     df_print: paged
     theme: cerulean
     highlight: default
     toc: yes
     toc_depth: 3
     toc_float:
        collapsed: false
        smooth_scroll: true
     code_fold: show
---


``` r
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)


# install.packages(c("tm", "tidytext", "stringr", "wordcloud", "RColorBrewer", "ggplot2", "SnowballC", "SentimentAnalysis", "ggthemes",  "tidyverse", "rmarkdown"))
# install.packages("knitr")
```

# Wymagane pakiety


``` r
# Wymagane pakiety ----
library(tm)
library(tidytext)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(SnowballC)
library(SentimentAnalysis)
library(ggthemes)
library(tidyverse)
library(rmarkdown)
library(knitr)
```

# 0. Funkcja do przetwarzania tekstu z apostrofami, stemmingiem i stemCompletion















