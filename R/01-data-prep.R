

pacman::p_load(malariasimulation, site, dplyr, purrr, tidyr, ggplot2, patchwork,
               stringr)

ken_site <- readRDS("data/raw/ken-site.rds")

# for plotting:
#- line: continuous-continuous (date and PfPr)
#- scatter: continuous-continuous (intvn coverage and PfPr)
#- bar/column: epi visualization or population visualization
#- map: of some risk thing e.g. EIR

# line plot
pfpr_data <- ken_site$prevalence |>
  select(county = name_1, residence = urban_rural, year, pfpr)

itn_data <- ken_site$interventions |>
  select(county = name_1, residence = urban_rural, year, itn_coverage = itn_use)

pfpr_intvn <- merge(pfpr_data, itn_data, by = c('county', 'residence', 'year'),
                    all = T)

# Population at risk from Plasmodium falciparum, Plasmodium vivax or both are estimated by masking the total population raster by areas with active transmission (prevalence >0%) in year 2000
malaria_risk <- ken_site$population$population_by_age |>
  select(county = name_1, residence = urban_rural, year, age = age_lower, total_pop = pop, at_risk = par_pf) |>
  mutate(
    age_group = case_when(
      age %in% 0:4   ~ '0-4',
      age %in% 5:9   ~ '5-9',
      age %in% 10:14 ~ '10-14',
      age %in% 15:19 ~ '15-19',
      age %in% 20:24 ~ '20-24',
      age %in% 25:29 ~ '25-29',
      age %in% 30:34 ~ '30-34',
      age %in% 35:39 ~ '35-39',
      age %in% 40:44 ~ '40-44',
      age %in% 45:49 ~ '45-49',
      age %in% 50:54 ~ '50-54',
      age %in% 55:59 ~ '55-59',
      age %in% 60:64 ~ '60-64',
      age %in% 65:69 ~ '65-69',
      age %in% 70:74 ~ '70-74',
      age %in% 75:79 ~ '75-79',
      age %in% 80:84 ~ '80-84',
      age %in% 85:89 ~ '85-89',
      age %in% 90:94 ~ '90-94',
      age %in% 95:100~ '95+'
    )
  ) |>
  group_by(county, year, age_group) |>
  reframe(across(c(total_pop, at_risk), sum))

malaria_risk <- malaria_risk[sample(nrow(malaria_risk)), ]
write.csv(malaria_risk, 'data/interim/malaria-risk.csv', row.names = F)

# The Entomological Inoculation Rate (EIR) is a key measure of malaria transmission intensity, representing the number of infectious mosquito bites a person receives per year. It's calculated by multiplying the human biting rate (HBR) (how many mosquitoes bite a person) by the sporozoite rate (the proportion of mosquitoes carrying malaria parasites). A high EIR indicates a greater risk of malaria infection in a specific area,
eir_data <- ken_site$eir |>
  group_by(name_1) |>
  reframe(eir = mean(eir))
write.csv(eir_data, 'data/interim/eir_data.csv', row.names = F)


incidence_deaths <- ken_site$cases_deaths |>
  select(year, starts_with('wmr_incidence'), starts_with('wmr_mortality'))
# plotting confidence bands, plotting smooth trends, plotting ablines/hlines

rainfall_data <- ken_site$seasonality$monthly_rainfall |>
  select(county = name_1, residence = urban_rural, year, month = month_name, rainfall)
write.csv(rainfall_data, 'data/interim/rainfall_data.csv', row.names = F)

st_write(ken_site$shape$level_1, 'data/interim/shp/kenyan_counties.shp')

# -------------------------------------------------------------------------

write.csv(pfpr_intvn, 'data/interim/pfpr_intvn.csv', row.names = F)

pfpr_intvn |> filter(county == 'Busia') |>
  ggplot() + 
  geom_line(aes(x = year, y = pfpr, col = residence))


