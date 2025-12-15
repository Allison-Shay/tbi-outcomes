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


###### File paths #########

fname <- "final_data_merged/cleaned.csv"
output_dir <- "analysis"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

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

write.csv(summary_df[order(summary_df$type, decreasing=TRUE),], file.path(output_dir, "raw_data_summary.csv"), row.names=FALSE)
cat("Rows:", nrow(df), "\nColumns:", ncol(df), "\n")

####### Filter the data based on criteria of excluding missing/bad vaules ########

# This will store number of encounters (note: not patients, because TQIP only stores encounter IDs) at each filtering step
filter_log <- data.frame(
  step = character(),
  n = integer(),
  stringsAsFactors = FALSE
)
log_step <- function(df, name, log_df) {
  rbind(
    log_df,
    data.frame(step = name, n = nrow(df), stringsAsFactors = FALSE)
  )
}
filter_log <- log_step(raw_df, "Raw", filter_log)


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

filter_log <- log_step(data_analytic, "Filtered (removed missing/bad values)", filter_log)
write_csv(filter_log, paste(output_dir, "/", "filtering_summary.csv", sep=""))

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

write.csv(table1_pvals, file.path(output_dir, "raw_data_statistics.csv"), row.names=FALSE)


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


out <- capture.output(summary(m.out))
writeLines(out, file.path(output_dir, "matchit_summary.txt"))
pdf(file.path(output_dir, "love_plot.pdf"), width = 7, height = 5)
love.plot(
  m.out,
  stats = "mean.diffs",
  abs = TRUE,
  threshold = 0.1,
  var.order = "unadjusted",
  standardize = TRUE,
  binary = "std",
  colors = c("grey60", "black")
)
dev.off()



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


# ---- helpers ----

fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) "<0.001" else sprintf("%.3f", p)
}

fmt_ci <- function(lo, hi, digits = 2) {
  paste0(sprintf(paste0("%.", digits, "f"), lo),
         " to ",
         sprintf(paste0("%.", digits, "f"), hi))
}

# Logistic: input is coeftest matrix from run_logit_cluster()
summ_logit <- function(coeftest_mat, outcome, n_used, term = "minority") {
  # coeftest_mat rows are (Intercept), minority, etc.
  if (!(term %in% rownames(coeftest_mat))) {
    return(tibble(
      outcome = outcome, model = "Logistic", n = n_used,
      estimate = NA_character_, ci_95 = NA_character_, p_value = NA_character_
    ))
  }

  b  <- coeftest_mat[term, "Estimate"]
  se <- coeftest_mat[term, "Std. Error"]
  p  <- coeftest_mat[term, "Pr(>|z|)"]

  # OR + Wald CI on log-odds scale
  or  <- exp(b)
  lo  <- exp(b - 1.96 * se)
  hi  <- exp(b + 1.96 * se)

  tibble(
    outcome = outcome,
    model = "Logistic (OR)",
    n = n_used,
    estimate = sprintf("%.2f", or),
    ci_95 = fmt_ci(lo, hi, digits = 2),
    p_value = fmt_p(p)
  )
}

# Linear: input is lm_robust object
summ_lm <- function(lmrob, outcome, n_used, term = "minority") {
  s <- summary(lmrob)
  ct <- s$coefficients
  if (!(term %in% rownames(ct))) {
    return(tibble(
      outcome = outcome, model = "Linear (Δ days)", n = n_used,
      estimate = NA_character_, ci_95 = NA_character_, p_value = NA_character_
    ))
  }

  b  <- ct[term, "Estimate"]
  lo <- ct[term, "CI Lower"]
  hi <- ct[term, "CI Upper"]
  p  <- ct[term, "Pr(>|t|)"]

  tibble(
    outcome = outcome,
    model = "Linear (Δ days)",
    n = n_used,
    estimate = sprintf("%.2f", b),
    ci_95 = fmt_ci(lo, hi, digits = 2),
    p_value = fmt_p(p)
  )
}

# ---- build the paper table ----
# NOTE: if minority is a factor, the term might be "minorityMinority"
# If your coefficient rowname isn't exactly "minority", change term= below accordingly.

term_name <- "minority"
# term_name <- "minorityMinority"   # <- uncomment if needed

results_table <- bind_rows(
  summ_logit(parench_out, "ICP parenchymal monitor", nrow(matched), term = term_name),
  summ_logit(evd_out,     "EVD placed",             nrow(matched), term = term_name),
  summ_logit(trach_out,   "Tracheostomy",           nrow(matched), term = term_name),
  summ_logit(gastro_out,  "Gastrostomy",            nrow(matched), term = term_name),

  summ_logit(mort_out,    "In-hospital mortality",  nrow(matched_mort), term = term_name),
  summ_logit(ltc_out,     "Discharge to LTC",       nrow(matched_ltc),  term = term_name),

  # WARNING: your WLST glm had convergence/separation issues earlier.
  # If you used logistf (Firth), DON'T summarize it with summ_logit().
  # Replace this row with a separate Firth summary.
  summ_logit(wlt_out,     "Withdrawal of LST",      nrow(matched_wlt),  term = term_name),

  summ_lm(los_out,        "Hospital LOS",           nrow(matched), term = term_name),
  summ_lm(icu_out,        "ICU LOS",                nrow(matched), term = term_name),
  summ_lm(vent_out,       "Ventilator days",        nrow(matched), term = term_name),

  summ_lm(monitor_days_out, "Cerebral monitoring days", nrow(matched_monitor), term = term_name)
) %>%
  mutate(
    contrast = "Minority vs White"
  ) %>%
  select(outcome, model, contrast, n, estimate, ci_95, p_value)


write.csv(results_table, file.path(output_dir, "final_analysis.csv"), row.names = FALSE)



              


