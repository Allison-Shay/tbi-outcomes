library(stringr)
library(tidyverse)
library(dplyr)
library(MatchIt)  # For propensity matching
library(readr)
library(cobalt) # For post-match balance checks
library(sandwich)
library(lmtest)

###### Read in the data ######

df <- read.csv("final_data_merged/cleaned.csv")

df$minority <- factor(df$minority, levels=c(0,1), labels=c("White","Minority"))
df$sex <- factor(df$sex, levels=c(1,2), labels=c("Male","Female"))
df$teachingstatus <- factor(df$teachingstatus)
df$verificationlevel <- factor(df$verificationlevel)

df$trach  <- factor(df$trach, levels=c(0,1), labels=c("No","Yes"))
df$gastro <- factor(df$gastro, levels=c(0,1), labels=c("No","Yes"))

df$icpparench <- factor(df$icpparench, levels=c(0,1), labels=c("No","Yes"))
df$icpevdrain <- factor(df$icpevdrain, levels=c(0,1), labels=c("No","Yes"))

df <- df %>%
  mutate(
    withdrawallst = factor(withdrawallst, levels = c(1,2), labels = c("Yes","No")),
    tbimidlineshift = case_when(
      is.na(tbimidlineshift) ~ NA_character_,
      tbimidlineshift == 1 ~ "Yes",
      tbimidlineshift == 2 ~ "No",
      tbimidlineshift == 3 ~ NA_character_,
      TRUE ~ NA_character_   # optional: make unexpected codes NA (or change to "Unknown")
    ) %>% factor(levels = c("Yes","No")),
    ich_category = factor(ich_category),
    statedesignation = factor(statedesignation),
    hospdischargedisposition = factor(hospdischargedisposition)
  )



###### Summarize the data ######

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

options(width = 300)
print(summary_df[order(summary_df$type, decreasing=TRUE),], row.names = FALSE)
cat("Rows:", nrow(df), "\nColumns:", ncol(df), "\n")


##########


cont_vars <- c("ageyears","totalgcs",
               "iss","totalventdays","totaliculos","finaldischargedays",
               "tbicerebralmonitordays",
               "hospitalprocedurestartdays")

cat_vars <- c("sex","minority","verificationlevel","teachingstatus",
              "trach","gastro","icpparench","icpevdrain",
              "hospdischargedisposition","ich_category",
              "withdrawallst","withdrawallstdays","statedesignation", "tbimidlineshift")


cont_results <- lapply(cont_vars, function(v) {
  x_white    <- df %>% filter(minority == "White")    %>% pull(!!sym(v))
  x_minority <- df %>% filter(minority == "Minority") %>% pull(!!sym(v))
  data.frame(
    Variable = v,
    White_Mean = mean(x_white, na.rm = TRUE),
    White_SD   = sd(x_white, na.rm = TRUE),
    Minority_Mean = mean(x_minority, na.rm = TRUE),
    Minority_SD   = sd(x_minority, na.rm = TRUE),
    P_value = wilcox.test(x_white, x_minority, exact = FALSE)$p.value,
    row.names = NULL
  )
})

cont_results <- bind_rows(cont_results)

print(cont_results)

cat_results <- lapply(cat_vars, function(v) {
  # Drop rows with NA in this variable or minority
  tab <- table(df[[v]][!is.na(df[[v]])], df$minority[!is.na(df[[v]])])
  chi <- suppressWarnings(chisq.test(tab))
  data.frame(
    Variable = v,
    White_n = tab[, "White"],
    White_pct = round(prop.table(tab, 2)[, "White"] * 100, 1),
    Minority_n = tab[, "Minority"],
    Minority_pct = round(prop.table(tab, 2)[, "Minority"] * 100, 1),
    P_value = chi$p.value,
    row.names = NULL
  )
})

# Combine the continuous results into a single data frame
cont_results_df <- bind_rows(cont_results) %>% 
  mutate(Type = "Continuous")

# Combine the categorical results into a single data frame
cat_results_df <- bind_rows(cat_results) %>% 
  mutate(Type = "Categorical")

# Combine both
final_table <- bind_rows(cont_results_df, cat_results_df)
final_table

# Build full table 
cat_table <- bind_rows(lapply(cat_vars, cat_summary))
cont_table <- bind_rows(lapply(cont_vars, cont_summary))
baseline_table <- bind_rows(cat_table, cont_table)
baseline_table

##################
#Delaney




###### Read in the data ######

df <- read.csv("final_data_merged/cleaned.csv")

df$minority <- factor(df$minority, levels=c(0,1), labels=c("White","Minority"))
df$sex <- factor(df$sex, levels=c(1,2), labels=c("Male","Female"))
df$teachingstatus <- factor(df$teachingstatus)
df$verificationlevel <- factor(df$verificationlevel)

df$trach  <- factor(df$trach, levels=c(0,1), labels=c("No","Yes"))
df$gastro <- factor(df$gastro, levels=c(0,1), labels=c("No","Yes"))

df$icpparench <- factor(df$icpparench, levels=c(0,1), labels=c("No","Yes"))
df$icpevdrain <- factor(df$icpevdrain, levels=c(0,1), labels=c("No","Yes"))

df <- df %>%
  mutate(
    withdrawallst = factor(withdrawallst, levels = c(1,2), labels = c("Yes","No")),
    tbimidlineshift = case_when(
      is.na(tbimidlineshift) ~ NA_character_,
      tbimidlineshift == 1 ~ "Yes",
      tbimidlineshift == 2 ~ "No",
      tbimidlineshift == 3 ~ NA_character_,
      TRUE ~ NA_character_   # optional: make unexpected codes NA (or change to "Unknown")
    ) %>% factor(levels = c("Yes","No")),
    ich_category = factor(ich_category),
    statedesignation = factor(statedesignation),
    hospdischargedisposition = factor(hospdischargedisposition)
  )



###### Summarize the data ######

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

options(width = 300)
print(summary_df[order(summary_df$type, decreasing=TRUE),], row.names = FALSE)
cat("Rows:", nrow(df), "\nColumns:", ncol(df), "\n")

data <- read.csv("final_data_merged/cleaned.csv")

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

summary(m.out)

love.plot(m.out)

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



table(matched$minority)


###########
library(estimatr)


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




