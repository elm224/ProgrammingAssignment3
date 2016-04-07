## 3. Ranking hospitals by outcome in a state

## rankhospital.R

## rankhospital takes three arguments: the abbreviated name of a state (state), an ourcome (outcome), and the ranking of a hospital in that state for that outcome (num = "best")

rankhospital <- function(state, outcome, num = "best") {

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
    data[, col] = as.numeric(data[, col])
    rslt = data[data[, 7] == state, c(2, col)]
    rslt = na.omit(rslt)
    nhospital = nrow(rslt)
    switch(num, best = {
        num = 1
    }, worst = {
        num = nhospital
    })
    if (num > nhospital) {
        return(NA)
    }
    ## Return hospital name in that state with the given rank 30-day death rate

    o = order(rslt[, 2], rslt[, 1])
    rslt[o, ][num, 1]
}
rankhospital("MD", "heart attack", "worst")