#!/usr/bin/env Rscript

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

results.subdirs <- c("tables",
                     "minstop_calibrate_full_tables",
                     "uncalibrated_tables",
                     "xtreme_prior_full_tables")

results.subsubdir <- "n=020000_m=02000_p=0.500_replacement=False_step=1"

infilenames <- c("power.csv",
                 "conditionalmean.csv",
                 "unconditional_mean.csv")

results.names <- gsub(".csv", "", infilenames)

# Filenames for saving the main results.
outfilename <- "unconditional_mean_with_recount_addin.csv"
outfiles <- file.path("..", results.subdirs, results.subsubdir, outfilename)

# Filename for saving combined results files.
outfile0 <- file.path("..", "combined_tables", results.subsubdir, outfilename)
outfile1 <- file.path("..", "combined_tables", results.subsubdir, infilenames[1])
outfile2 <- file.path("..", "combined_tables", results.subsubdir, infilenames[2])
outfile3 <- file.path("..", "combined_tables", results.subsubdir, infilenames[3])


# =============================================================================
# Functions.

# Get path to all required files for a given results subdirectory.
filenames.for.subdir <- function(subdir)
    file.path("..", subdir, results.subsubdir, infilenames)

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

# Load and munge data for a single subdirectory.
load.and.munge <- function(subdir) {
    infiles <- filenames.for.subdir(subdir)
    d <- Map(load.results, infiles)
    names(d) <- results.names
    check.methods(d)
    return(d)
}

# Calculate desired mean sample size.
calc.sampsize <- function(results) {
    results$power * results$conditionalmean + (1 - results$power) * n
}

# Save a results matrix to a file.
save.results <- function(resultsmat, ...) {
    write.csv(t(resultsmat), ...)
}


# =============================================================================
# Do it...

# Load and munge data.
dd <- Map(load.and.munge, results.subdirs)

# Main calculations.
ee <- Map(calc.sampsize, dd)

# Write output to files.
Map(save.results, ee, outfiles)

# Create a single file with all new results.
ee2 <- do.call(cbind, ee)
save.results(ee2, outfile0)

# Also extract the previously published results and combine them together.
dd2.1 <- do.call(cbind, Map('[[', dd, "power"))
dd2.2 <- do.call(cbind, Map('[[', dd, "conditionalmean"))
dd2.3 <- do.call(cbind, Map('[[', dd, "unconditional_mean"))
save.results(dd2.1, outfile1)
save.results(dd2.2, outfile2)
save.results(dd2.3, outfile3)
