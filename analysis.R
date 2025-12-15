library(stringr)
library(tidyverse)
library(dplyr)
library(MatchIt)  # For propensity matching
library(readr)
library(cobalt) # For post-match balance checks
library(sandwich)
library(lmtest)
library(dplyr)
library(purrr)
library(tibble)
library(estimatr)


###### Data filename #########

fname <- "final_data_merged/cleaned.csv"

###### Read in the data ######

raw_df <- read.csv(fname)
df <- raw_df

###### Create a data summary table #######

transform_analysis_vars <- function(df) {

  # Optional safety check (highly recommended)
  required_cols <- c(
    "minority", "sex", "teachingstatus", "verificationlevel",
    "trach", "gastro", "icpparench", "icpevdrain",
    "withdrawallst", "tbimidlineshift", "ich_category",
    "statedesignation", "hospdischargedisposition"
  )

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  df %>%
    mutate(
      # Core demographics
      minority = factor(minority, levels = c(0, 1),
                        labels = c("White", "Minority")),
      sex = factor(sex, levels = c(1, 2),
                   labels = c("Male", "Female")),

      # Hospital characteristics
      teachingstatus = factor(teachingstatus),
      verificationlevel = factor(verificationlevel),

      # Procedures
      trach  = factor(trach, levels = c(0, 1), labels = c("No", "Yes")),
      gastro = factor(gastro, levels = c(0, 1), labels = c("No", "Yes")),
      icpparench = factor(icpparench, levels = c(0, 1), labels = c("No", "Yes")),
      icpevdrain = factor(icpevdrain, levels = c(0, 1), labels = c("No", "Yes")),

      # Outcomes / clinical decisions
      withdrawallst = factor(withdrawallst,
                             levels = c(1, 2),
                             labels = c("Yes", "No")),

      # Injury characteristics
      tbimidlineshift = case_when(
        tbimidlineshift == 1 ~ "Yes",
        tbimidlineshift == 2 ~ "No",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Yes", "No")),

      ich_category = factor(ich_category),
      statedesignation = factor(statedesignation),
      hospdischargedisposition = factor(hospdischargedisposition)
    )
}

df <- transform_analysis_vars(df)


max_levels_to_print <- 10  # threshold for printing categories

summary_df <- data.frame(
  column = character(),
  type = character(),
  na_count = integer(),
  value_summary = character(),
  stringsAsFactors = FALSE
)

for (col_name in names(df)) {
  x <- df[[col_name]]
  n_na <- sum(is.na(x))
  x_no_na <- x[!is.na(x)]

  # Numerical
  if (is.numeric(x)) {
    type <- "numerical"
    if (length(x_no_na) == 0) {
      value_summary <- "all values are NA"
    } else {
      value_summary <- paste0(
        min(x_no_na), " to ", max(x_no_na)
      )
    }

  # Categorical
  } else {
    type <- "categorical"
    levels <- unique(x_no_na)
    n_levels <- length(levels)

    if (n_levels <= max_levels_to_print) {
      value_summary <- paste(levels, collapse = ", ")
    } else {
      value_summary <- paste(n_levels, "categories")
    }
  }

  summary_df <- rbind(
    summary_df,
    data.frame(
      column = col_name,
      type = type,
      na_count = n_na,
      value_summary = value_summary,
      stringsAsFactors = FALSE
    )
  )
}

####### Print out the data summary table #######

options(width = 300)
print(summary_df[order(summary_df$type, decreasing=TRUE),], row.names = FALSE)
cat("Rows:", nrow(df), "\nColumns:", ncol(df), "\n")

####### Filter the data based on criteria of excluding missing/bad vaules ########

data <- raw_df

apply_analytic_filters <- function(data_frame) {
  data_frame %>%
  filter(!is.na(minority))%>%
  filter(!is.na(sex), sex %in% c(1,2))%>%
  filter(!is.na(teachingstatus))%>%
  filter(!is.na(verificationlevel))%>%
  filter(!is.na(totalgcs))%>%
  filter(!is.na(iss))%>%
  filter(!is.na(tbimidlineshift), tbimidlineshift %in% c(1,2))%>%
  filter(!is.na(statedesignation))%>%
  # filter(!is.na(hospdischargedisposition))%>%   # Don't exclude upfront (avoid selection bias)
  # filter(!is.na(withdrawallst))%>%              # Don't exclude upfront
  filter(!is.na(ich_category))
}

data_analytic <- apply_analytic_filters(data)
data_analytic_mod <- transform_analysis_vars(data_analytic) # Simply change things from numerical to things like Yes/No Male/Female etc.

######## Run statistical analysis to compare groups ########

group_var <- "minority"

exclude_vars <- character(0)

# ---- Variable lists from summary_df ----
num_vars <- summary_df %>%
  filter(type == "numerical") %>%
  pull(column) %>%
  setdiff(c("inc_key")) %>%
  setdiff(exclude_vars)

cat_vars <- summary_df %>%
  filter(type == "categorical") %>%
  pull(column) %>%
  setdiff(c(group_var)) %>%
  setdiff(exclude_vars)

# ---- Test helpers ----
test_numeric_wilcox <- function(df, var, group) {
  x <- df[[var]]
  g <- df[[group]]
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- g[ok]
  if (length(unique(g)) != 2) return(NA_real_)
  tryCatch(wilcox.test(x ~ g)$p.value, error = function(e) NA_real_)
}

test_categorical_chisq <- function(df, var, group) {
  x <- df[[var]]
  g <- df[[group]]
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- g[ok]
  if (length(unique(g)) != 2) return(NA_real_)
  tbl <- table(x, g)
  if (any(dim(tbl) < 2)) return(NA_real_)
  tryCatch(chisq.test(tbl)$p.value, error = function(e) NA_real_)
}

# ---- Run tests ----
results_num <- map_dfr(num_vars, function(v) {
  tibble(
    variable = v,
    type = "continuous",
    n_nonmissing = sum(!is.na(data_analytic_mod[[v]]) & !is.na(data_analytic_mod[[group_var]])),
    p_value = test_numeric_wilcox(data_analytic_mod, v, group_var)
  )
})

results_cat <- map_dfr(cat_vars, function(v) {
  # also record table size so you can spot "ICD-like" variables if you didn't exclude them
  x <- data_analytic_mod[[v]]
  g <- data_analytic_mod[[group_var]]
  ok <- !is.na(x) & !is.na(g)
  tbl <- table(x[ok], g[ok])
  tibble(
    variable = v,
    type = "categorical",
    n_nonmissing = sum(ok),
    n_levels = nrow(tbl),
    table_cells = prod(dim(tbl)),
    p_value = test_categorical_chisq(data_analytic_mod, v, group_var)
  )
})

table1_pvals <- bind_rows(results_num, results_cat) %>%
  mutate(
    p_value = ifelse(is.na(p_value), NA, signif(p_value, 3)),
    note = case_when(
      type == "categorical" & !is.na(table_cells) & table_cells > 20000 ~ "very large contingency table",
      TRUE ~ ""
    )
  ) %>%
  arrange(type, p_value)

print(table1_pvals, n = Inf)


######## Make some new data variables and factorize some existing ones ########


data_analytic <- data_analytic %>%
  mutate(
    icpparench = ifelse(is.na(icpparench), 0, icpparench),
    icpevdrain = ifelse(is.na(icpevdrain), 0, icpevdrain),
    trach      = ifelse(is.na(trach), 0, trach),
    gastro     = ifelse(is.na(gastro), 0, gastro),
    neurosurg_any = ifelse(icpparench == 1 | icpevdrain == 1, 1, 0),
  )

data_analytic <- data_analytic %>%
  mutate(
    sex              = factor(sex),                      # 0/1 or M/F -> factor
    minority         = as.integer(minority),             # 1 = minority, 0 = NHW
    verificationlevel = factor(verificationlevel),
    teachingstatus   = factor(teachingstatus)
  )

data_analytic <- data_analytic %>%
  mutate(
    mort_inhospital = case_when(
      hospdischargedisposition == 5  ~ 1L,
      hospdischargedisposition %in% c(1:99) ~ 0L,  # other valid codes
      TRUE ~ NA_integer_
    ),
    ltc_inhospital = case_when(
      hospdischargedisposition == 12 ~ 1L,
      hospdischargedisposition %in% c(1:99) ~ 0L,
      TRUE ~ NA_integer_
    ),
    withdrawallst_bin = case_when(
      withdrawallst == 2 ~ 1L,
      withdrawallst == 1 ~ 0L,
      TRUE ~ NA_integer_
    )
)

######## Propensity matching ########


ps_formula <- minority ~ ageyears + sex + iss +
  verificationlevel + teachingstatus +
  ich_category + statedesignation + tbimidlineshift

data_ps <- data_analytic %>%
  mutate(
    gcs_cat = case_when(
      totalgcs <= 8  ~ "Severe",
      totalgcs <= 12 ~ "Moderate",
      TRUE ~ "Mild"
    )
  )
m.out <- matchit(
  ps_formula,
  data = data_ps,
  method = "nearest",
  ratio = 1,
  caliper = 0.2,
  exact = ~ gcs_cat     # <-- THIS IS NEEDED TO PROPENSITY MATCH GCS
)


######## Results of propensity matching ########

summary(m.out)
love.plot(m.out)


####### Regression analyses #########

# Helper function: clustered-robust SE by subclass
cluster_se <- function(model, cluster) {
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M / (M - 1)) * ((N - 1) / (N - K))  # finite-sample correction
  
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
  vcovCL <- dfc * sandwich(model, meat. = crossprod(uj) / N)
  list(coef = coef(model), vcov = vcovCL)
}

run_logit_cluster <- function(formula, data, cluster) {
  fit <- glm(formula, data = data, family = binomial)
  cl  <- cluster_se(fit, cluster)
  coefs <- coeftest(fit, vcov = cl$vcov)
  return(coefs)
}

matched <- match.data(m.out)
#crani_out<- run_logit_cluster(crani ~ minority, matched, matched$subclass)
#crani_out
parench_out <- run_logit_cluster(icpparench ~ minority, matched, matched$subclass)
parench_out
evd_out      <- run_logit_cluster(icpevdrain ~ minority, matched, matched$subclass)
evd_out
trach_out <- run_logit_cluster(trach ~ minority, matched, matched$subclass)
trach_out
gastro_out <- run_logit_cluster(gastro ~ minority, matched, matched$subclass)
gastro_out              

# For regressions where we're now going to exclude NA's

matched_mort <- matched %>%
  filter(!is.na(mort_inhospital))

mort_out <- run_logit_cluster(
  mort_inhospital ~ minority,
  matched_mort,
  matched_mort$subclass
)
mort_out

matched_ltc <- matched %>%
  filter(!is.na(ltc_inhospital))

ltc_out <- run_logit_cluster(
  ltc_inhospital ~ minority,
  matched_ltc,
  matched_ltc$subclass
)
ltc_out


matched_wlt <- matched %>%
  filter(!is.na(withdrawallst_bin))

wlt_out <- run_logit_cluster(
  withdrawallst_bin ~ minority,
  matched_wlt,
  matched_wlt$subclass
)
wlt_out


# Linear regression with clustered SE
los_out <- lm_robust(finaldischargedays ~ minority, data = matched, clusters = subclass)
icu_out <- lm_robust(totaliculos ~ minority, data = matched, clusters = subclass)
vent_out <- lm_robust(totalventdays ~ minority, data = matched, clusters = subclass)

los_out
icu_out
vent_out

# Cerebral monitoring days
matched_monitor <- matched %>% filter(!is.na(tbicerebralmonitordays))
monitor_days_out <- lm_robust(
  tbicerebralmonitordays ~ minority,
  data = matched_monitor,
  clusters = subclass
)
monitor_days_out



# Midline shift
#matched_mid <- matched_mid %>%
#  mutate(tbimidlineshift_bin = ifelse(tbimidlineshift == 2, 1, 0))  # assuming 2 = shift, 1 = no shift

# Then run logistic with clustered SE
#fit_mid <- glm(tbimidlineshift_bin ~ minority, data = matched_mid, family = binomial)
#vcov_cluster <- vcovCL(fit_mid, cluster = matched_mid$subclass)
#coeftest(fit_mid, vcov = vcov_cluster)




