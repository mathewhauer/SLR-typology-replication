###------Hammer Data Gather-----
## @knitr Hammer-Gather

# Historic census data pulled.
historic <- read.xlsx("DATA/historicaldat.xlsx", sheet =1) %>%
  mutate_all(funs(replace(., is.na(.), 0)))


# download.file("https://www2.census.gov/geo/docs/reference/codes/national_county.txt", "DATA/national_county.txt")
fips <- read_csv("DATA/national_county.txt", col_names=FALSE) %>%
  filter(X2<=56) # getting rid of PR, Guam, etc.

# Downloading the county-level population projections
# download.file("https://osf.io/uh5sj/download/", destfile = "DATA/SSP_asrc.zip", mode = 'wb')
# unzip(zipfile='DATA/SSP_asrc.zip', exdir = "DATA") 
controls <- read_csv("DATA/SSP_asrc.csv") %>%
  group_by(YEAR, STATE, COUNTY, GEOID) %>%
  dplyr::summarise(SSP1 = sum(SSP1),
                   SSP2 = sum(SSP2),
                   SSP3 = sum(SSP3),
                   SSP4 = sum(SSP4),
                   SSP5 = sum(SSP5)) %>%
  dplyr::select(FIPS = GEOID, everything())

### Getting the Decennial Data ###
dat_decen <- foreach(i = 1:nrow(fips), .combine = rbind, .errorhandling = "stop", .packages = c("data.table", "doParallel", "foreach", "reshape2", "stringi", "stringr", "zoo", "censusapi", "tidyverse", "tidycensus")) %dopar% {  
  decennial <- get_decennial(geography = "block", variables = c("H001001", "P001001", "P043001"), year = 2010,
                             output = "wide", state = paste0(fips$X2[i]), county = paste0(fips$X3[i]), geometry = FALSE) %>%
    mutate(GEOID = substr(GEOID,1,12)) %>%
    group_by(GEOID) %>%
    dplyr::summarise(H001001 = sum(H001001),
                     P001001 = sum(P001001),
                     P043001 = sum(P043001)) %>%
    mutate(pphu = ((P001001-P043001) / H001001), # estimating the PPHU as the Total Population minus the GQ divided by the # of HU's.
           FIPS = substr(GEOID, 1,5)) %>% # setting the FIPS code
    ungroup() %>%
    dplyr::select(FIPS, GEOID, pphu,P043001, H001001)
  
  
}

write_rds(dat_decen, "./R/DATA/census2010dat.RDS")

### Setting the variables to get from the ACS ###
var <- c("B25034_001E","B25034_002E", "B25034_003E", "B25034_004E", "B25034_005E", "B25034_006E",
         "B25034_007E", "B25034_008E", "B25034_009E", "B25034_010E") 

### Getting the ACS data
dat_acs <- foreach(i = 1:2,#nrow(fips), 
                   .combine = rbind, 
                   .errorhandling = "stop", 
                   .packages = c("data.table", "doParallel", "foreach", "reshape2", "stringi", "stringr", "zoo", "censusapi", "tidyverse", "tidycensus")) %dopar% {  
                     
                     dat_acs <- get_acs(geography = "block group", variables = var, year = 2013,
                                        output = "tidy", state = paste0(fips$X2[i]), county = paste0(fips$X3[i]), geometry = FALSE,
                                        key = "0206e3f2924a424be8722887fd0a49cea6308a7e") %>%
                       mutate(year = case_when( # renaming this set of variables
                         variable == "B25034_001" ~  "TOTALa", 
                         variable == "B25034_003" ~  "a2009",
                         variable == "B25034_004" ~  "a1999",
                         variable == "B25034_005" ~  "a1989",
                         variable == "B25034_006" ~  "a1979",
                         variable == "B25034_007" ~  "a1969",
                         variable == "B25034_008" ~  "a1959",
                         variable == "B25034_009" ~  "a1949",
                         variable =="B25034_010" ~  "a1939", 
                         TRUE ~ "a2019"),
                         FIPS = paste0(fips$X2[i], fips$X3[i])) %>% ## Creating a FIPS code from the first 5 digits of the GEOID
                       dplyr::select(GEOID, FIPS, estimate, year) %>%
                       spread(year, estimate)  ## Going from tall/skinny to short/fat
                     write_rds(dat_acs, paste0("./R/DATA/ACS2013/", fips$X2[i], fips$X3[i], ".RDS"))
                   }  
# 