df <- long_nd %>%
  filter(indicator == "art_coverage",
         iso3 %in% adjusted_art$iso3,
         age_group_label == "15-49",
         sex != "both") %>%
  select(iso3, area_id, sex, mean) %>%
  pivot_wider(names_from = sex, values_from = mean) %>%
  rename(female_cov = female,
         male_cov = male) %>%
  mutate(logit_female = logit(female_cov),
         logit_male = logit(male_cov),
  ) %>%
  left_join(long_nd %>%
              filter(indicator %in% c("prevalence", "population"), 
                     iso3 %in% adjusted_art$iso3,
                     age_group_label == "15-49",
                     sex != "both") %>%
              select(iso3, area_id, indicator, sex, mean) %>%
              pivot_wider(names_from = indicator, values_from = "mean") %>%
              mutate(hivpop = prevalence*population) %>%
              select(-c(prevalence, population)) %>%
              pivot_wider(names_from = sex, values_from = hivpop) %>%
              rename(female_hivpop = female, male_hivpop = male)) %>%
  mutate(male_num = male_cov * male_hivpop,
         female_num = female_cov * female_hivpop)

female_scalar <- 0.001
# target_diff <- 0.72
curr_diff <- 6

int <- lapply(df %>% group_by(iso3) %>% group_split(), function(df){
  
  while(logit_diff > 0.72) {
    # 
    # df <- df %>%
    #   mutate(female_additional = female_hivpop * female_scalar,
    #          male_hivpop = male_hivpop - female_additional,
    #          female_hivpop = female_hivpop + female_additional,
    #          est_cov = (male_num + female_num)/(male_hivpop + female_hivpop),
    #          est_male_cov = male_num/male_hivpop,
    #          est_female_cov = female_num/female_hivpop,
    #          est_logit_female = logit(est_female_cov),
    #          est_logit_male = logit(est_male_cov),
    #          logit_diff = est_logit_female - est_logit_male
    #   )
    # 
    # df %>% select(iso3, both_cov, est_cov, est_logit_female, est_logit_male, logit_diff)
    # 
    # 
    # curr_diff <- df$est_logit_female - df$est_logit_male
    # female_scalar <- female_scalar + 0.0000001
    
    int <- df %>%
      mutate(female_additional = female_hivpop * female_scalar,
             male_hivpop = male_hivpop - female_additional,
             female_hivpop = female_hivpop + female_additional) %>%
      group_by(iso3) %>%
      mutate(est_male_cov_nat = sum(male_num)/sum(male_hivpop),
             est_female_cov_nat = sum(female_num)/sum(female_hivpop),
             est_male_cov_prov = male_num/male_hivpop,
             est_female_cov_prov = female_num/female_hivpop)
    
    
    logit_diff <- logit(unique(int$est_female_cov_nat)) - logit(unique(int$est_male_cov_nat))
    female_scalar <- female_scalar + 0.0001
    
    print(logit_diff)
    
  }
  
  int
  
  
})

write_csv(int %>% bind_rows(), "adjusted_art.csv")