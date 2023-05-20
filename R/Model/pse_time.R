
fit_pse_time_model <- function(kp_id) {
  
  pse_inla <- df_logit_prev %>%
    filter(!is.na(area_id),
           kp == "FSW") %>%
    distinct(iso3, area_id) %>%
    crossing(year = c(2010:2020)) %>%
    # pse_inla <- data.frame(iso3 = ssa_iso3) %>%  
    bind_rows(pse_dat %>%
                filter(kp == "FSW") %>%
                group_by(ref) %>%
                mutate(id.ref = cur_group_id(),
                       id.ref = ifelse(is.na(ref), NA, id.ref)) %>%
                ungroup %>%
                arrange(fe_method) %>%
                mutate(id.method = as.numeric(fct_inorder(method)))) %>%
    left_join(areas %>% st_drop_geometry()) %>%
    mutate(area_id = ifelse(is.na(area_id), iso3, area_id)) %>%
    dplyr::select(iso3, area_id, id.area, year, logit_proportion, fe_method, id.method, method, id.ref)
  # 
  nat_level_obs <- pse_inla %>%
    filter(area_id == iso3) %>%
    ungroup() %>%
    group_by(id.ref) %>%
    mutate(id.ref.nat = cur_group_id(),
           id.ref = NA)
  
  pse_inla <- pse_inla %>%
    filter(area_id != iso3
           # | is.na(area_id) ## get rid of this
    ) %>%
    bind_rows(nat_level_obs) %>%
    left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(id.year = cur_group_id())
  
  pse_formula <- logit_proportion ~ 
    f(id.area, model = "besag", scale.model = TRUE, graph = "admin1_level_adj.adj", hyper=prec.prior) +
    # f(id.iso3, model = "besag", scale.model = TRUE, graph = "geog.adj", hyper=prec.prior) +
    fe_method +
    # year +
    f(id.year, model = "ar1", group=id.iso3, control.group=list(model='iid')) +
    f(id.method, model = "iid", hyper = prec.prior) +
    f(id.ref, model = "iid", hyper = prec.prior) +
    # f(id.iso3, model = "iid", hyper = prec.prior) +
    f(id.ref.nat, model = "iid", hyper = prec.prior)
  
  pse_fit <- INLA::inla(pse_formula,
                        data = pse_inla,
                        family = "gaussian", 
                        control.compute = list(config = TRUE),
                        control.predictor=list(compute=TRUE),
                        verbose = FALSE)
  
  # fitted_val <- get_mod_results_test(fit, pse_inla, "logit_proportion")
  
  df <- pse_inla %>%
    filter(across(all_of("logit_proportion"), ~!is.na(.x)))
  
  print("Sampling..")
  samples <- inla.posterior.sample(1000, pse_fit)
  print("Done sampling")
  contents = pse_fit$misc$configs$contents
  effect = "Predictor"
  id.effect = which(contents$tag==effect)
  ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])
  
  ind.effect <- 1:(nrow(pse_inla) - nrow(df))
  
  samples.effect = lapply(samples, function(x) x$latent[ind.effect])
  
  pse_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)
  
  ident <- pse_inla[ind.effect, ]
  
  qtls <- apply(pse_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
  pse <- ident %>%
    ungroup() %>%
    mutate(
      lower = qtls[1,],
      median = qtls[2,],
      upper = qtls[3,],
      indicator = "pse"
    )
  
  extract_df <- pse_inla %>% 
    distinct(id.method, method, fe_method) %>%
    filter(!is.na(method))
  
  sm <- function(x,y) {
    random_marginal <- eval(parse(text = paste0("pse_fit$marginals.random$id.method$index.", x)))
    random_samples <- inla.rmarginal(1000, random_marginal)
    
    if(y != "empirical") {
      fixed_marginal <- eval(parse(text = paste0("pse_fit$marginals.fixed$`fe_method", y, "`")))
      fixed_samples <- inla.rmarginal(1000, fixed_marginal)
    } else {
      fixed_samples <- rep(0, 1000)
    }
    
    out <- random_samples + fixed_samples
  }
  
  samples <- map2(extract_df$id.method, as.character(extract_df$fe_method), ~sm(.x, .y))
  samples <- matrix(unlist(samples), nrow = nrow(extract_df), byrow = TRUE)
  
  qtls <- t(apply(samples, 1, quantile, c(0.025, 0.5, 0.975)))
  
  pse_fixed <- extract_df %>%
    ungroup() %>%
    mutate(lower = qtls[,1],
           median = qtls[,2],
           upper = qtls[,3])
  
  # pse_fixed <- data.frame(pse_fit$summary.fixed) %>%
  #   rownames_to_column() %>%
  #   dplyr::select(rowname, starts_with("X")) %>%
  #   type.convert(as.is = FALSE) %>%
  #   mutate(kp = kp_id)
  
  out <- list()
  out$pse <- pse
  out$pse_samples <- pse_samples
  out$pse_fixed <- pse_fixed
  out
  
}

mod <- fit_pse_time_model("FSW")

pse_s <- mod$pse_samples
ids <- mod$pse %>% pull(area_id)
pop <- pop_l$FSW
pop <- pop[match(ids, pop$area_id), ]

urban_ntl_pop <- mod$pse %>%
  select(iso3, area_id, year) %>%
  cbind(invlogit(pse_s) * urban_proportion$urban_proportion * pop$mean,
        urban_pop = urban_proportion$urban_proportion * pop$mean) %>%
  group_by(iso3, year) %>%
  summarise(across(as.character(1:1000), sum),
            urban_pop = sum(urban_pop)) %>%
  select(iso3, urban_pop, year, as.character(1:1000))

urban_ntl_pop <- urban_ntl_pop %>%
  mutate(across(as.character(1:1000), ~.x/urban_pop))

urban_pse_proportion <- urban_ntl_pop %>%
  select(iso3, year) %>%
  cbind(t(apply(urban_ntl_pop[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))))

colnames(urban_pse_proportion) <- c("iso3", "year", "lower", "median", "upper")

urban_pse_proportion %>%
  left_join(region) %>%
  filter(region == "WCA") %>%
  ggplot(aes(x=year, y=median)) +
    # geom_point(size=2, show.legend = F) +
    geom_line(size=1, show.legend = F) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    geom_point(data = pse_dat %>% 
                 left_join(region) %>% 
                 filter(region == "WCA",
                        prop_estimate < 0.1,
                        kp == "FSW"),
               aes(y=prop_estimate), alpha = 0.5, show.legend = F) +
    standard_theme() +
    scale_percent() +
    scale_x_continuous(labels = scales::label_number(big.mark = "", accuracy = 1)) +
    facet_wrap(~iso3)
