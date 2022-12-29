library(TMB)
library(tidyverse)
library(sf)
library(dfertility)

prev_data <- read.csv("~/Imperial College London/Key population data - WP - General/Age prevalence.csv")

region <- read.csv("~/Documents/GitHub/fertility_orderly/global/region.csv")

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode::countrycode(ssa_names, "country.name", "iso3c")

prev_df <- prev_data %>%
  separate(age, into=c("lower", "upper"), sep = "-") %>%
  mutate(upper = ifelse(is.na(upper), 49,upper),
         upper = ifelse(str_detect(lower, "<"), lower, upper),
         lower = ifelse(str_detect(lower, "<"), 15, lower),
         upper = ifelse(str_detect(upper, "<"), str_remove(upper, "<"), upper),
         lower = ifelse(str_detect(lower, ">"), str_remove(lower, ">"), lower),
         lower = ifelse(str_detect(lower, "\\+"), str_remove(lower, "\\+"), lower)
  ) %>%
  mutate(idx = row_number()) %>%
  type.convert(as.is = TRUE) %>%
  mutate(age_group = paste0(lower, "-", upper),
         # lower = ifelse(lower < 15, 15, lower),
         upper = ifelse(upper > 49, 49, upper),
         upper = ifelse(upper %% 5 == 0, upper - 1, upper),
         lower = ifelse(lower == 16, 15, lower)) %>%
  select(iso3, kp, area_name, year, method, lower, upper, prev, denominator, ref) %>%
  filter(lower < upper, ## Fix these weird ones later. Mostly from "50+" or "50-55", but not all
         !(lower == 18 & upper == 49),
         !(lower == 15 & upper == 49)) 

mf <- crossing(
  # age_group = naomi::get_age_groups() %>%
  #   filter(age_group_sort_order %in% 18:24) %>%
  #   pull(age_group),
  age_group = 15:49,
  iso3 = ssa_iso3
) %>%
  arrange(iso3) %>%
  group_by(age_group) %>%
  mutate(id.age = group_indices())

prev_df <- prev_df %>%
  filter(kp == "FSW") %>%
  group_by(iso3, kp, year, ref) %>%
  mutate(id.ref = group_indices()) %>%
  ungroup() %>%
  group_by(iso3) %>%
  mutate(id.iso3 = group_indices()) %>%
  select(-ref) %>%
  arrange(id.iso3) %>%
  ungroup() %>%
  group_by(area_name, id.ref) %>%
  mutate(area_ref = group_indices())

remove_denominator <- prev_df %>%
  filter(is.na(denominator)) %>%
  distinct(area_ref)
  
prev_df <- prev_df %>%
  filter(!area_ref %in% remove_denominator$area_ref) %>%
  mutate(prop_denominator = denominator/sum(denominator))

incomplete_studies <- prev_df %>%
  group_by(area_ref) %>%
  filter(lower == min(lower) | upper == max(upper),
         lower != 15, upper != 49)

complete_studies <- prev_df %>%
  filter(!area_ref %in% incomplete_studies$area_ref) %>%
  group_by(area_ref) %>%
  mutate(total_denominator = sum(denominator))

age_matrix <- matrix(data = c(complete_studies$lower, complete_studies$upper), nrow = nrow(complete_studies))

age_matrix <- apply(age_matrix, 1, function(x) {
  lower <- x[1]
  upper <- x[2]
  ages <- lower:upper
  lagging <- lower-15
  leading <- 49-upper
  mat_row <- c(rep(0, lagging), rep(1, length(ages)), rep(0, leading))
}) %>%
  unlist()

A_age <- as(matrix(data = age_matrix, nrow = nrow(complete_studies), byrow = TRUE), "dgTMatrix")

# Z_age <- sparse.model.matrix(~0 + id.age, mf$observations$full_obs)
R_age <- make_rw_structure_matrix(max(mf$id.age), 2, adjust_diagonal = TRUE)

five_year_age_matrix <- matrix(data = c(seq(15,45,5), (seq(15,45,5))+4), nrow = length(seq(15,45,5)))

five_year_age_matrix <- apply(five_year_age_matrix, 1, function(x) {
  lower <- x[1]
  upper <- x[2]
  ages <- lower:upper
  lagging <- lower-15
  leading <- 49-upper
  mat_row <- c(rep(0, lagging), rep(1, length(ages)), rep(0, leading))
}) %>%
  unlist()

five_year_A_age <- as(matrix(data = five_year_age_matrix, nrow = length(seq(15,45,5)), byrow = TRUE), "dgTMatrix")

TMB::compile("R/kp_age.cpp")
dyn.load(dynlib("R/kp_age"))

tmb_int <- list()

tmb_int$data <- list(
  A_age = A_age,
  five_year_A_age = five_year_A_age,
  Z_age = as(diag(1, 35), "dgTMatrix"),
  R_age = R_age,
  denominator = complete_studies$denominator,
  log_offset = log(complete_studies$total_denominator),
  age_span = apply(A_age, 1, sum)
  
)

tmb_int$par <- list(
  beta_0 = 0,
  u_age = rep(0, 35),
  log_prec_age = 0
  # logit_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_age"
)


f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                          parameters = tmb_int$par,
                                          DLL = "kp_age",
                                          silent=0,
                                          checkParameterOrder=FALSE)
})

if(is.null(parallel::mccollect(f)[[1]])) {
  stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
}

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "kp_age",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
fit$sdreport <- sdreport(fit$obj, fit$par)

class(fit) <- "naomi_fit"  # this is hacky...

fit <- naomi::sample_tmb(fit, random_only=TRUE)

plot(apply(fit$sample$lambda, 1, median))

vec <- c(2,3,5,7,8)
(sum(1/vec)/length(vec))^-1

vec <- log(vec)
(sum(1/exp(vec))/length(vec))^-1

five_year_A_age %*% apply(fit$sample$lambda, 1, median)

dfertility::tmb_outputs
         
