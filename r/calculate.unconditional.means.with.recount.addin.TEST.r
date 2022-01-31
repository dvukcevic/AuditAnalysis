#!/usr/bin/env Rscript

# This is a test version of the following...

# Calculate the mean sample size that includes a full count ('unconditional
# mean with recount') but where ballots that are already sampled do NOT get
# counted again.  Previously, we assumed *all* ballots got recounted, separate
# to the sampling phase of the audit, so the ballots that were sampled
# originally were eventually double-counted.

# For example, with n = 20,000 and m = 2000, an audit that reaches m (= 2000)
# ballots during sampling and proceeds to a full count will be treated as
# having a sample size of n (= 20,000).  Previously these would be treated as
# having a sample size of m + n (= 22,000).

# Sample size relationships:
#
# uncondmean                    = power * condmean + (1 - power) *  m
# uncondmean_with_recount       = power * condmean + (1 - power) * (m + n)
# uncondmean_with_recount_addin = power * condmean + (1 - power) *      n

# The calculations below are for the scenario:
#
# n = 20,000
# m =  2,000
# Sampling without replacement
# Sampling increment ('step') = 1
# True proportion in favour of winner (p_T) = 0.5


# =============================================================================
# Parameter settings.

n <- 20000
m <-  2000

datadir1 <- file.path("..", "tables",
                      "n=020000_m=02000_p=0.500_replacement=False_step=1")

files.to.load <- c("power.csv",
                   "conditionalmean.csv",
                   "unconditional_mean.csv",
                   "unconditional_mean_with_recount.csv")

results.names <- gsub(".csv", "", files.to.load)


# =============================================================================
# Functions.

# Load a single results file, and convert to a standard matrix format.
load.results <- function(infile) {
    data0 <- read.csv(infile, check.names = FALSE)

    method.labels  <- data0[,  1]
    actual.results <- data0[, -1]

    data1 <- t(as.matrix(actual.results))
    colnames(data1) <- method.labels

    return(data1)
}

# Check that all datasets refer to the same auditing methods.
check.methods <- function(datalist) {
    method.labels <- colnames(datalist[[1]])
    for (d in datalist)
        stopifnot(identical(method.labels, colnames(d)))
}


# =============================================================================
# Load and munge data.

datafiles <- file.path(datadir1, files.to.load)

dd <- Map(load.results, datafiles)

names(dd) <- results.names

check.methods(dd)


# =============================================================================
# Main calculations.

ee <- list(dd$power * dd$conditionalmean + (1 - dd$power) *  m     ,
           dd$power * dd$conditionalmean + (1 - dd$power) * (m + n),
           dd$power * dd$conditionalmean + (1 - dd$power) *      n )

# Check that our new calculations match the olds ones.
stopifnot(isTRUE(all.equal(ee[[1]], dd[[3]])))
stopifnot(isTRUE(all.equal(ee[[2]], dd[[4]])))
