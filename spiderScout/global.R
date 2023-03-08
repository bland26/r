library(plotly)
library(htmltools)
library(jsonlite)
library(httr)
library(tidyverse)
library(shiny)

scoutData <- read.csv("file")
rangeAM  <- 3
rangeANS <- max(scoutData[10])
rangeTNS <- max(scoutData[14])
rangeL   <- max(scoutData[15])
rangeCST <- 2
rangeDS  <- 5
rangeC   <- 5

selection <- c(6,10,14,15,17,18,19)

ranges <- c(angeAM,rangeANS,rangeTNS,rangeL,rangeCST,rangeDS,rangeC)

for(i in 1:length(ranges)){
  x <- selection[i]
  scoutData[x] <- scoutData[x] / ranges[i]
  }
