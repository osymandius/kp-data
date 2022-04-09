pop <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

pop <- lapply(file.path("archive/aaa_scale_pop", pop, "interpolated_population.csv"), read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  filter(year == 2020,
         str_length(area_id) == 3) %>%
  five_year_to_15to49("population")

pop <- pop %>%
  bind_rows(
    pop %>%
      group_by(area_id, year, age_group) %>%
      summarise(population = sum(population)) %>%
      mutate(sex = "both")
  ) %>%
  mutate(iso3 = area_id)

unaids_plhiv <- bind_rows(
  read.csv("~/Downloads/People living with HIV_People living with HIV - Adults (15-49)_Population All adults (15-49).csv")
) %>%
  select(!contains(c("lower", "upper", "Footnote"))) %>%
  mutate(iso3 = countrycode::countrycode(Country, "country.name", "iso3c")) %>%
  filter(iso3 %in% ssa_iso3) %>%
  select(-Country) %>%
  pivot_longer(-iso3) %>%
  mutate(year = str_remove(name, "X"),
         value = str_remove_all(value, " ")) %>%
  filter(year == "2020") %>%
  type_convert() %>%
  select(-name)

# pse <- read.csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_estimates.csv")
# 
# prev <- read.csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV Prevalence/prev_national_matched_estimates.csv")


kplhiv <- foo %>%
  mutate(sex = case_when(
    kp == "FSW" ~ "female",
    kp == "MSM" ~ "male",
    kp == 'PWID' ~ "both"
  )) %>%
  left_join(pop) %>%
  mutate(
    kplhiv_lower = lower * population,
    kplhiv_median = median * population,
    kplhiv_upper = upper * population
  )

remaining_plhiv <- kplhiv %>%
  group_by(iso3) %>%
  summarise(kplhiv_median = sum(kplhiv_median)) %>%
  left_join(unaids_plhiv) %>%
  mutate(kplhiv_median = value - kplhiv_median,
         kp = "Remainder") %>%
  select(iso3, kp, kplhiv_median)

kplhiv %>%
  bind_rows(remaining_plhiv) %>%
  ggplot(aes(x=iso3, y=kplhiv_median, fill=fct_rev(kp))) +
    geom_col(position = "fill") +
    standard_theme() +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_fill_manual(values = 
                        c(
                          wesanderson::wes_palette("Zissou1")[1],
                          wesanderson::wes_palette("Moonrise2")[2],
                          wesanderson::wes_palette("Zissou1")[4],
                          wesanderson::wes_palette("Rushmore1")[3]
                          
                        )) +
    labs(x=element_blank(), y="Proportion of total PLHIV", fill=element_blank())

kplhiv %>%
  ggplot(aes(x=iso3, y=kplhiv_median, group=kp, fill=kp)) +
    geom_col(position = position_dodge(.9)) +
    geom_linerange(aes(ymin=kplhiv_lower, ymax = kplhiv_upper), position=position_dodge(.9)) +
    scale_y_log10(labels = scales::label_number()) +
    standard_theme() +
    labs(x=element_blank(), y=element_blank())
