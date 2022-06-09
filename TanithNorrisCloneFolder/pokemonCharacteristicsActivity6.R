---
  title: "Pokemon Assessment"
author: "Tanith Norris"
date: "6/8/2022"
output: html_document
---
  
```{r}

## Load in the relevant packages to read data
library(tidyverse)
library(readxl)
library(boxplotdbl)
library(skimr)

## Load in Pokemon Databases

pokemonDatabase <- read_excel("GitHub/BPWorkshop2022/PokemonDatabaseOFFICIAL.xlsx")
pokemonDatabase <- pokemonDatabase %>%
  select(number, pokemon_name, main_type, 
                  attack, defense, stamina,
                  "height(m)", "weight (kg)", 
                  capture_rate)
## GETTING STARTED ::::::::::::::::::::::::::::::::

## 1ST TASK: DETERMINING POWERFUL POKEMON

    ## FIRST STEP: Defining Pokemon Strengths

## Create Vector by which we measure strengths 
## and ratings for each facet of power

strengths <- pokemonDatabase %>%
  select(attack, defense, stamina)
                        
## Create Boxplot for Strength Ratings

boxplot(strengths,
        xlab = "Strengths",
        ylab = "Strength Ratings",
        ylim = c(0,500),
        main = "Strengths Rating for Pokemon")

    ## SECOND STEP: Creating a Powerful 
    ## True/False Column based on Box Plot
      
## Run a Summary to Determine Boxplot Statistics
## and Print to prove

pokemonDatabase_summary <- pokemonDatabase %>% 
  select(attack, defense, stamina) %>%
  summary(pokemonDatabase)
  
pokemonDatabase_summary

## Establish Vectors that filter responses for
## each strength based on 4th Quartile Value

attackFourthQuart <- pokemonDatabase$attack >= 204
defenseFourthQuart <- pokemonDatabase$defense >= 176
staminaFourthQuart <- pokemonDatabase$stamina >= 190
   
## Create Powerful TRUE/FALSE Column Check
pokemonDatabase <- pokemonDatabase %>%
  mutate(powerful = attackFourthQuart & 
                    defenseFourthQuart & 
                    staminaFourthQuart)

## Print when in Doubt
pokemonDatabase

## Find way to separate which pokemon classify as
## powerful? How do I sort??

print("The most powerful pokemon are as follows:")
filter(pokemonDatabase, powerful == TRUE)

## 2ND TASK: TYPES OF POKEMON WITH 
## HIGHEST CAPTURE RATE

      ## FIRST STEP: Select relevant categories
      ## and Arrange database in descending
      ## order by capture_rate, for convenience
      ## of reading data. and Print

pokemonCapture <- pokemonDatabase %>%
  select(main_type, capture_rate)
  arrange(pokemonCapture, desc(capture_rate))
  summary(pokemonCapture)
  
## Create Barplot to Record Main Types vs. Capture
  
barplot(pokemonCapture$capture_rate)
         #main = "Capture Rate of Pokemon Types",
          #x = main_type,
          #y = capture_rate)#,
          #xlab = "Pokemon Types",
          #ylab = "Capture Rate",
          #ylim = c(0, 1))

## !??!?!?!??!

## 3RD TASK: BOX PLOT OF POKEMON TYPE AND HEIGHT/WEIGHT/BMI

    ## FIRST STEP: Print to create height and 
    ## weight environment
## print summary to identify scale of the graph

height <- pokemonDatabase$"height(m)"
weight <- pokemonDatabase$"weight (kg)"
hwVector <- data.frame(height, weight)

summary(hwVector)

## Create scatter plot of height/weights
plot(hwVector,
        xlab = "Height",
        ylab = "Weight",
        xlim = c(0,15),
        ylim = c(0,1000),
        main = "Pokemon Height vs. Weight Chart")

## STEP TWO: Create new vector that calculates
## BMI using kg / m^2



## STEP THREE: Create new column defining elements
## "Uderweight," "Healthy," "Overweight,"
## & "Obese" for pokemon. 

```