## 1. Plot the 30-day mortality rates for heart attack

## Set working directory

>setwd("~/Desktop/ProgrammingAssignment3")

## Read the outcome data into R via the read.csv function and look at the first few rows.

> outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## To make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset), run:

> outcome[, 11] < as.numeric(outcome[, 11])
> hist(outcome[, 11])

## 2. Finding the best hospital in a state

## best.R

## The best function inputs two arguments, an abbreviated state and an outcome, then returns a character vector with the name of a hospital with the best outcome (lowest) for a 30-day mortality.

best <- function(state, outcome) {

    ## Read the outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (!state %in% unique(data[, 7])) {
        stop("invalid state")
    }
    switch(outcome, `heart attack` = {
        col = 11
    }, `heart failure` = {
        col = 17
    }, pneumonia = {
        col = 23
    }, stop("invalid outcome"))
    
    ## Return hospital name in that state with lowest 30-day death rate
    rslt = data[data$State == state, c(2, col)]
    rslt[which.min(rslt[, 2]), 1]
}
best("MD", "pneumonia")