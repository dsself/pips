#!/usr/bin/env Rscript
#
# IRT Model R Script
#
# Prior to this point we expect a per variable object with the raw
# codings split between coder-reported confidences (conf_mat) and
# ratings (wdata), each transformed from long to wide, interpolated,
# and reduced, with the appropriate vignette data appended. We also
# expect three logical matrices matching the dimensions of our
# confidence and code matrices indicating for each coder-country-date
# observation whether s/he is a historical, lateral, or new coder
# (coded only >= 2005 for countries existing prior to 2005). Lastly,
# we want a vector of original country-dates prior to reduction so
# that we can expand our results to the full time series, a numeric
# vector of main country coded per coder, and a vector of missing
# country-dates so that we can preserve missingness in later stages
# oif the DS construction.
#
# This script then generates offset priors depending on whether a
# coder is historical or new, while ignoring lateral coders, and runs
# the data + priors with the listed stan files.
#
# We save 50% draws plus summarised Z, OSP, and ORDs at country-date
# and country-year level.
###

#setwd("vparty")
sessionInfo()

options(scipen = 10)
library(dplyr)
library(rstan)
library(tools)
library(vutils)

sessionInfo()

rstan_options(auto_write = FALSE)

VARIABLE <- "v2panom"
ITER   <- as.numeric(100000)
OUTDIR <- "out/"
INFILE <- file.path("", VARIABLE %^% "_original.rds")
R_SEED <- 42
STAN_SEED <- 42
output <- list()

# Grab or set seeds for R and STAN
if (STAN_SEED == "0")
    STAN_SEED <- sample(1000000, 1)
if (R_SEED == "0")
    R_SEED <- sample(1000000, 1)

R_SEED <- as.integer(R_SEED)
STAN_SEED <- as.integer(STAN_SEED)

OUTDIR_ <- file.path(OUTDIR, as.character(ITER/1000) %^% "k")
dir.create(OUTDIR_, recursive = T)

stopifnot(file.exists(INFILE), file.access(INFILE, mode = 4) == 0)
stopifnot(dir.exists(OUTDIR_), file.access(OUTDIR_, mode = 2) == 0)
stopifnot(is.numeric(ITER), ITER > 0, length(ITER) == 1)

NAME   <- VARIABLE
WARMUP <- 1000 * (ITER / 10000)
THIN   <- 20 * (ITER / 10000)
CHAINS <- 4
CORES  <- CHAINS




set.seed(R_SEED)

info("START: " %^% Sys.time())
info("NAME: " %^% NAME)
info("INFILE: " %^% INFILE)
info("OUTDIR: " %^% OUTDIR_)
info("ITER: " %^% ITER)
info("STAN_SEED: " %^% STAN_SEED)
info("R_SEED: " %^% R_SEED)
info(sprintf("WARMUP: %d THIN: %d CHAINS: %d", WARMUP, THIN, CHAINS))

###
# Let's start with loading some data and excluding vignettes from our
# initial priors.
input.data  <- readRDS("v2panom_original.rds")[["v2panom"]]

utable <- input.data$utable
normal_vignettes.b <- is_vignette(rownames(input.data$wdata)) &
        !is_country_date(gsub("^A_", "", rownames(input.data$wdata)), party = TRUE)
lateral_vignettes.b <- is_vignette(rownames(input.data$wdata)) &
        is_country_date(gsub("^A_", "", rownames(input.data$wdata)), party = TRUE)

vignettes.b <- normal_vignettes.b | lateral_vignettes.b
# Matrices for generating priors
wdata       <- input.data$wdata[!vignettes.b, ]
conf_mat    <- input.data$conf_mat[!vignettes.b, ]

historical  <- input.data$historical[!vignettes.b, ]
lateral     <- input.data$lateral[!vignettes.b, ]
new_coder   <- input.data$new_coder[!vignettes.b, ]

# Lateral coders not used for priors
is.na(wdata)    <- lateral
is.na(conf_mat) <- lateral

###
# If we have a v3.* variables, no need for offsets; otherwise, apply
# offsets based on per mean difference between normal contemporary and
# new/historical per country.
if (substring(NAME, 1, 2) == "v2") {
    countries <- rownames(wdata) %>% substr(1,3)

    wdata.split <- split.data.frame(wdata, countries)
    conf_mat.split <- split.data.frame(conf_mat, countries)

    if (any(new_coder)) {
        new_coder.split <- split.data.frame(new_coder, countries)

        # min = 4 meaning that when generating offsets we only take
        # into account country-dates where there are at least 4 or
        # more of each group (normal and new)
        new_offsets <- Map(offset_diff, wdata.split, new_coder.split, conf_mat.split, min = 4)
        wdata.split <- Map(add_to_matrix, wdata.split, new_offsets, by = new_coder.split)
    }

    if (any(historical)) {
        hist.split <- split.data.frame(historical, countries)

        hist_offsets <- Map(offset_diff, wdata.split, hist.split, conf_mat.split)

        # Apply our offsets from successor states to extinct ones (ex
        # Germany -> Saxony)
        hist_offsets <- lapply(names(hist_offsets), function(s) {
            if (s %in% names(input.data$successor))
                hist_offsets[[input.data$successor[s]]]
            else
                hist_offsets[[s]]
        })

        wdata.split <- Map(add_to_matrix, wdata.split, hist_offsets, by = hist.split)
    }

    wdata <- do.call(rbind, wdata.split)
}

# Make sure we're not below 1 or above K
wavg <- pmin(wdata, input.data$K) %>% pmax(1) %>% weighted.rowMeans(conf_mat)

priors <- vector(mode = "numeric", length = nrow(input.data$wdata))
# names(priors) <- names(wavg)

# Normalize! Finally!
priors[1:nrow(wdata)] <- (wavg - mean(wavg, na.rm = T)) / sd(wavg, na.rm = T)
names(priors)[1:nrow(wdata)] <- names(wavg)

###
# Vignette priors are set from -1.5 to 1.5 depending on their order
# (dummy country_text_id)
if (any(normal_vignettes.b)) {
    v <- rownames(input.data$wdata)[normal_vignettes.b]
    thresholds <- sub(".*?__(\\d)$", "\\1", v) %>% type.convert

    if (length(v) > 1)
        stopifnot(diff(sort(thresholds)) %in% c(0, 1))

    priors[normal_vignettes.b] <- to_seq(thresholds)
    names(priors)[normal_vignettes.b] <- v
}

if (any(lateral_vignettes.b)) {
    # m_wdata <- input.data$wdata[lateral_vignettes.b, ]
    # m_conf <- input.data$conf[lateral_vignettes.b, ]
    priors[lateral_vignettes.b] <- 0
    names(priors)[lateral_vignettes.b] <-
        rownames(input.data$wdata)[lateral_vignettes.b]
}

###
# Transform wdata to long format and remove missing values
stopifnot(sapply(dimnames(input.data$wdata), Negate(is.null)))

# For debugging purposes
input.data$wdata <- input.data$wdata[sort(rownames(input.data$wdata)), ]
input.data$wdata <- input.data$wdata[, as.character(sort(as.numeric(colnames(input.data$wdata))))]

# Transform our matrix with country-dates and rownames and coder_ids as columns
# to a long form data.frame with country_date and coder_id as factors.
wdata.df <- do.call(expand.grid, dimnames(input.data$wdata)) %>%
    setNames(c("country_date", "coder_id")) %>%
    transform(code = as.vector(input.data$wdata)) %>%
    subset(!is.na(code))

# For index access in the stan file we need ordered index sequences
# representing our coders, countries, and country-dates that can be
# translated back to the appropriate IDs after.
#this is where Darin's team can recode
wdata.df <- transform(wdata.df,
                     rater_id = as.numeric(coder_id),
                     cdata = as.factor(input.data$cdata[match(coder_id, names(input.data$cdata))]),
                     country_date_id = as.numeric(country_date))

##Shari edit -> make the new _mid data. 2 becomes 1, 3 becomes 2, and 1 becomes 3
wdata.df$code <- recode(wdata.df$code, '2' = 1, '3' = 2, '1' = 3)
  
# Main country coded for each rater_id
cdata_id <- wdata.df$cdata[match(1:input.data$J, wdata.df$rater_id)] %>%
    as.numeric

# We should never hit this, but stop if we completely delete a coder
stopifnot(diff(wdata.df$rater_id) < 2)

# Order priors to match index order of country-dates
priors <- priors[levels(wdata.df$country_date)]
stopifnot(!anyNA(priors))

# Let's do some final (redundant) checks.
stopifnot(!is.na(wdata.df))
stopifnot(input.data$J == length(unique(wdata.df$coder_id)))
stopifnot(input.data$N == length(unique(wdata.df$country_date)))
stopifnot(nrow(wdata.df) == length(input.data$wdata[!is.na(input.data$wdata)]))
stopifnot(as.character(wdata.df$country_date) == names(priors)[wdata.df$country_date_id])
stopifnot(cdata_id[wdata.df$rater_id] == as.factor(wdata.df$cdata) %>% as.numeric)

# For binary vars, transform data to 0, 1 & load matching stan code
if (input.data$K == 2) {
    stanfile <- "bin_mcc.stan"

    data <- list(K = input.data$K,                           # Answer categories
            J = input.data$J,                           # Number of coders
            C = length(unique(input.data$cdata)),       # Number of countries
            N = input.data$N,                           # Number of unique country-dates
            n_obs = nrow(wdata.df),                     # Total number of long-format observations
            cdata_id = cdata_id,                        # Index of main country coded
            wdata = wdata.df$code,                      # Coder-submitted ratings
            rater_id = wdata.df$rater_id,               # Index of coder_id's
            country_date_id = wdata.df$country_date_id,
            gsigmasq = 0.2,
            gsigmasqc = 0.2,
            mc = priors)

    data$wdata <- data$wdata - 1
} else {
    stanfile <- "quasilda4.stan"

    data <- list(K = input.data$K,                           # Answer categories
                 J = input.data$J,                           # Number of coders
                 C = length(unique(input.data$cdata)),       # Number of countries
                 N = input.data$N,                           # Number of unique country-dates
                 n_obs = nrow(wdata.df),                     # Total number of long-format observations
                 rater_state = cdata_id,                        # Index of main country coded
                 y = wdata.df$code,                      # Coder-submitted ratings
                 j_id = wdata.df$rater_id,               # Index of coder_id's
                 sy_id = wdata.df$country_date_id,
                 gsigmasq = 0.2,
                 gsigmasqc = 0.2,
                 mc = priors)
}

posterior.sim <- stan(file = file.path("/Users/sharifranke/Desktop/elements/vparty", stanfile),
                      data = data,
                      iter = ITER,
                      thin = THIN,
                      chains = CHAINS,
                      cores = CORES,
                      warmup = WARMUP,
                      init_r = 0.1,
                      control = list(max_treedepth = 2e1),
                      pars = "Z_star",
                      include = F,
                      seed = STAN_SEED)

get_elapsed_time(posterior.sim)

output$posterior <- posterior.sim
output$model_input <- data
output$script_input <- input.data
output$country_dates <- input.data$country_dates

# For debugging:
# write_file(output, file.path(OUTDIR_, "posterior_" %^% VARIABLE %^% ".rds"))

pars <- c("Z",        # Estimated latent variable
          "gamma_mu", # Universial thresholds
          "beta",     # Reliability scores
          "gamma",    # Coder-specific thresholds
          "gamma_c")  # Country-specific thresholds

###
# Before we start summarising and creating our different variable
# versions, check convergence (mean(Rhat > 1.1) <= 0.01)
for (p in pars) {
    Rhat <- summary(posterior.sim, pars = p)$summary[, "Rhat"]

    if (!mean(Rhat > 1.01) <= 0.05) {
        warn("Convergence failed for: " %^% p)
    }
}

###
# Extract each parameter as a matrix for later use.
pars.matrices <- lapply(pars, function(p) {
    # Keep this for debugging purposes. Seriously, you really want
    # this info in your log files.
    sprintf("Extracting %s", p) %>% info

    m <- as.matrix(posterior.sim, pars = p)
    if (p == "gamma_mu")
        return(m)

    colnames(m) <- switch(p,
                         "gamma" = lapply(1:(input.data$K - 1), paste,
                                          levels(wdata.df$coder_id), sep = ".") %>% unlist,
                         "gamma_c" = lapply(1:(input.data$K - 1), paste,
                                            levels(wdata.df$cdata), sep = ".") %>% unlist,
                         "beta" = levels(wdata.df$coder_id),
                         levels(wdata.df$country_date))

    return(m)
})

names(pars.matrices) <- pars
output$post.sample <- list()

###
# Save half of our draws as samples to be publically released
for (p in names(pars.matrices)) {

    m <- pars.matrices[[p]]
    out <- t(m[sample(nrow(m), nrow(m) / 2) %>% sort, ])

    # Add the missing country-dates so that we can keep track of them
    # for the BFAs
    if (p == "Z")
        out <- add_empty_rows(out, input.data$missing)

    output$post.sample[[tolower(p)]] <- out
}


###
# Finally, let's create the different variable versions that go into
# the public DS. For each var type, create both a country-date and
# aggregated country-year.
#
# Variable versions:
#     1. post.summary -> Latent trait estimates
#     2. osp -> Linearized ordinal scale
#     3. ord -> Ordinal estimates
summarise_t <- partial(dist_summary, expanded.names = input.data$country_dates,
            utable = utable, party = TRUE, rule_366 = FALSE)

for (type in c("post.summary", "osp", "ord")) {
    sprintf("Generating %s", type) %>% info

    cd <- switch(type,
               "post.summary" = summarise_t(pars.matrices$Z),
               "osp" = osp(pars.matrices$Z, pars.matrices$gamma_mu) %>% summarise_t,
               "ord" = ord(pars.matrices$Z, pars.matrices$gamma_mu) %>% summarise_t)

    # If creating ordinal estimates, truncate decimals
    if (type == "ord")
        cd$median <- floor(cd$median)

    # OBS! For ords, aggregate by taking the last observation,
    # otherwise day-weighted mean
    if (type != "ord") {
        cy <- cy.day_mean(cd, historical_date, country_text_id)
    } else {
        cy <- transform(cd, year = to_year(historical_date), historical_date = NULL) %>%
            aggregate(. ~ country_text_id + year, ., partial(tail, n = 1))
    }

    if (substr(VARIABLE, 3, 4) == "pa") {
      cy$party_id <- sub("^\\S{3} ", "", cy$country_text_id) %>% as.numeric
      cd$party_id <- sub("^\\S{3} ", "", cd$country_text_id) %>% as.numeric

      cy$country_text_id <- sub(" \\d{6}$", "", cy$country_text_id)
      cd$country_text_id <- sub(" \\d{6}$", "", cd$country_text_id)
    }

    output[[type]] <- list(cd = cd, cy = cy)
}


# Generate beta summaries for the disaggregated dataset
####
output$b.summary <- b_summary(pars.matrices$beta)

# Fix columns
###
output$cd <- Reduce(
    partial(full_join,
            by = c("country_text_id", "party_id", "historical_date")),
    list(fix_stat_columns(output$post.summary$cd, VARIABLE),
         fix_stat_columns(output$osp$cd, VARIABLE %^% "_osp"),
         fix_stat_columns(output$ord$cd, VARIABLE %^% "_ord")))

#Darin trying to fix year type
output$osp$cy$year <- as.numeric(output$osp$cy$year)
output$post.summary$cy$year <- as.numeric(output$post.summary$cy$year)

output$cy <- Reduce(
  partial(full_join,
          by = c("country_text_id", "party_id", "year")),
  list(fix_stat_columns(output$post.summary$cy, VARIABLE),
       fix_stat_columns(output$osp$cy, VARIABLE %^% "_osp"),
       fix_stat_columns(output$ord$cy, VARIABLE %^% "_ord")))




#end of the Vparty code
# Write file
<<<<<<< HEAD
#outout <- list()
#outout[[VARIABLE]] <- output
#write_file(outout, file.path(OUTDIR_, VARIABLE %^% ".rds"), dir_create = T)
#info("Done with " %^% NAME %^% " at " %^% ITER %^% " iterations.")
#info("END: " %^% Sys.time())

#extract the vector we want and make it a df, then save
library(readr)

#as.data.frame(output$cd) %>% 
# select(country_text_id, historical_date, party_id, v2panom ) %>% 
#write_rds("v2panom_replication.rds")
=======
outout <- list()
outout[[VARIABLE]] <- output
write_file(outout, file.path(OUTDIR_, VARIABLE %^% ".rds"), dir_create = T)
info("Done with " %^% NAME %^% " at " %^% ITER %^% " iterations.")
info("END: " %^% Sys.time())

#new data
data1 <- readRDS("~/Desktop/elements/out/10k/v2panom.rds")
#old data
data2 <- readRDS("~/Desktop/elements/data/vparty.rds")
data3 <- subset(data2, select = c("v2panom", "historical_date", "country_text_id"))
#compare old data with new data, and the v2panom values are about the same, so it did work correctly. 

#Now, we need to make the new data not export as a list, and instead export as a data frame



>>>>>>> b709960b7ae7138d952755f4463a90e8db34f8e0
