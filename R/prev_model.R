library(INLA)

get_mod_results_test <- function(mod, prev_inla) {
  
  
  prev <- prev_inla %>%
    filter(!is.na(iso3))
  
  print("Sampling..")
  samples <- inla.posterior.sample(1000, mod)
  print("Done sampling")
  contents = mod$misc$configs$contents
  effect = "Predictor"
  id.effect = which(contents$tag==effect)
  ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])
  
  ind.effect <- 1:(nrow(prev_inla) - nrow(prev))
  
  samples.effect = lapply(samples, function(x) x$latent[ind.effect] %>% exp)
  
  ident <- prev_inla[ind.effect, ]
  
  qtls <- apply(sapply(samples.effect, cbind), 1, quantile, c(0.025, 0.5, 0.975))
  
  samples_ident <- ident %>%
    mutate(lower = qtls[1,],
           median = qtls[2,],
           upper = qtls[3,]
    )
  
  return(samples_ident)
  
}

names(id_list) <- iso3_vec
id_list <- id_list %>%
  compact()

names(prev_dat) <- names(id_list)

prev_df <- prev_dat %>%
  bind_rows(.id = "iso3") %>%
  filter(value != 0) %>%
  select(iso3, year, kp, value, provincial_value) %>%
  group_by(iso3, year, kp) %>%
  mutate(study_idx = cur_group_id(),
         log_kp_prev = log(value),
         log_gen_prev = log(provincial_value)
  )

prev_inla <- crossing(log_gen_prev = log(seq(0.01, 0.4, 0.01))) %>%
  bind_rows(prev_df %>%
              filter(kp == "FSW") %>%
              ungroup) %>%
  mutate(idx = row_number())


formula <- log_kp_prev ~ log_gen_prev + f(study_idx, model = "iid")

fit <- INLA::inla(formula,
                         data = prev_inla,
                         family = "gaussian", 
                         control.compute = list(config = TRUE),
                  control.predictor=list(compute=TRUE),
                  verbose = TRUE)

fitted_val <- get_mod_results_test(fit, prev_inla)

fitted_val <- fitted_val %>%
  mutate(gen_prev = exp(log_gen_prev))

prev_inla %>%
  filter(!is.na(iso3)) %>%
  ggplot(aes(x=provincial_value, y=value)) +
    geom_line(data = fitted_val, aes(x=gen_prev, y=median)) +
    geom_ribbon(data = fitted_val, aes(x=gen_prev, ymin = lower, ymax=upper), alpha=0.3) + 
    geom_point() +
    lims(x=c(0,1), y=c(0,1))
    
