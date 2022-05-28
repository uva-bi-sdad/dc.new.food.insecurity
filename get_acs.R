# Adapted from Joanna's code

# Food Insecurity - new measure
#
# Get ACS variables for FAMILY size and median income - all in table S1903
#
# Getting data from ACS5 for VA, MD, and DC. Subsetting to NCR. 
# Years: 2015-2020 
# Geographies: counties and tracts. (NOT available at block groups.)


library(tidycensus)
library(dplyr)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

# geographies and years to pull from ACS

states <- c("VA", "MD", "DC")
geographies <- c("county", "tract")  
years <- c(2018:2020) 

# DATA PULL AND WRANGLING ------------------------------------ 

fam_num <- NULL
fam_num_wide <- NULL
fam_med_income <- NULL
fam_med_income_wide <- NULL

for(state in states)
{
  for(year in years)
  {
    for(geography in geographies)
    {
      # pull count of each family size ---------------------------
      
      vars = c("S1903_C01_024", "S1903_C01_025", "S1903_C01_026", "S1903_C01_027",
               "S1903_C01_028", "S1903_C01_029", "S1903_C01_036", "S1903_C01_039")
      
      fam_num <- get_acs(geography = geography, variables = vars, state = state, year = year, 
                         geometry = FALSE, survey = "acs5", cache_table = TRUE, output = "wide") %>% 
      transmute(
        geoid = GEOID,
        region_type = as.character(geography),
        region_name = NAME,
        year = year,
        fam1ppl_cnt = rowSums(select(., S1903_C01_036E, S1903_C01_039E), na.rm = TRUE),
        fam2ppl_cnt = S1903_C01_024E,
        fam3ppl_cnt = S1903_C01_025E,
        fam4ppl_cnt = S1903_C01_026E,  
        fam5ppl_cnt = S1903_C01_027E,
        fam6ppl_cnt = S1903_C01_028E,
        fam7ppl_cnt = S1903_C01_029E,
      ) %>%
        arrange(geoid)
    
      fam_num_wide <- rbind(fam_num_wide, fam_num)
      
      # pull median income of each family size -----------------------------
     
      vars = c("S1903_C03_024", "S1903_C03_025", "S1903_C03_026", "S1903_C03_027",
               "S1903_C03_028", "S1903_C03_029", "B19019_002")
      
      fam_med_income <- get_acs(geography = geography, variables = vars, state = state, year = year, 
                                geometry = FALSE, survey = "acs5", cache_table = TRUE, output = "wide") %>% 
        transmute(
          geoid = GEOID,
          region_type = as.character(geography),
          region_name = NAME,
          year = year,
          fam1ppl_med_income = B19019_002E,
          fam2ppl_med_income = S1903_C03_024E,
          fam3ppl_med_income = S1903_C03_025E,
          fam4ppl_med_income = S1903_C03_026E,  
          fam5ppl_med_income = S1903_C03_027E,
          fam6ppl_med_income = S1903_C03_028E,
          fam7ppl_med_income = S1903_C03_029E,
        ) %>%
        arrange(geoid)
      
      fam_med_income_wide <- rbind(fam_med_income_wide, fam_med_income)
      
    }
  }
}


# FORMAT CHANGE TO DATA COMMONS STANDARD -------------------------------
# I will update this part once we have decisions made on FAMILY vs HOUSEHOLD
# 
# # Format change to long and add measure_type column
# 
# ed_att_long <- gather(ed_att_wide, measure, value, less_than_hs_degree_18plus_cnt:bachelors_degree_or_above_18plus_pct)
# ed_att_long$measure_type <- "count"
# ed_att_long[grepl("pct", ed_att_long$measure, fixed = TRUE), ]$measure_type <- "percent"
# 
# 
# # NCR ------------------------------------------
# 
# ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")
# 
# # filter to NCR
# 
# ncr_ed_att <- ed_att_long %>% dplyr::filter(str_detect(geoid, ncr_counties))
# 
# con <- get_db_conn()
# dc_dbWriteTable(con, "dc_education_training", "vadcmd_cttrbg_acs5_2013_2019_educational_attainment", ed_att_long)
# dc_dbWriteTable(con, "dc_education_training", "ncr_cttrbg_acs5_2013_2019_educational_attainment", ncr_ed_att)
# DBI::dbDisconnect(con)