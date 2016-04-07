## 4. Ranking hospitals in all states

## rankall.R

## rankall takes two arguments: an outcome name (outcome) and a hospital raning number (num = "best"). The function then returns a 2-column data frame with the hospital in each state that has the ranking specified in num.


rankall <- function(outcome, num = "best") {
    ## Read the outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    states = unique(data[, 7])
    switch(outcome, `heart attack` = {
        col = 11
    }, `heart failure` = {
        col = 17
    }, pneumonia = {
        col = 23
    }, stop("invalid outcome"))

    ## Return hospital name in that state with the given rank 30-day death rate
    data[, col] = as.numeric(data[, col])
    data = data[, c(2, 7, col)]  # leave only name, state, and death rate
    data = na.omit(data)
        rank_in_state <- function(state) {
        rslt = data[data[, 2] == state, ]
        nhospital = nrow(rslt)
        switch(num, best = {
            num = 1
        }, worst = {
            num = nhospital
        })
        if (num > nhospital) {
            result = NA
        }
        o = order(rslt[, 3], rslt[, 1])
        result = rslt[o, ][num, 1]
        c(result, state)
    }
    output = do.call(rbind, lapply(states, rank_in_state))
    output = output[order(output[, 2]), ]
    rownames(output) = output[, 2]
    colnames(output) = c("hospital", "state")
    data.frame(output)
}
tail(rankall("heart failure"), 10)