
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

files <- paste0("DATA-PROCESSED/ACS2013//", list.files(path = "./DATA-PROCESSED/ACS2013/"))
temp <- lapply(files, read_rds)
dat_acs <- rbindlist( temp ) 

dat_decen <- read_rds("DATA-PROCESSED/census2010dat.RDS")

data_sum <-  group_by(dat_acs, FIPS) %>% # grouping the data by FIPS code
  summarise(t1939 = sum(a1939), 
            t1949 = sum(a1949),
            t1959 = sum(a1959),
            t1969 = sum(a1969),
            t1979 = sum(a1979),
            t1989 = sum(a1989),
            t1999 = sum(a1999),
            t2009 = sum(a2009)) %>% # summing the total number of housing units in each decade
  left_join(historic) %>% # Joining the summed ACS with the historic Census data
  mutate(adj_1940 = h1940 / t1939,
         adj_1950 = h1950 / (t1939 + t1949),
         adj_1960 = h1960 / (t1939 + t1949 + t1959),
         adj_1970 = h1970 / (t1939 + t1949 + t1959  + t1969),
         adj_1980 = h1980 / (t1939 + t1949 + t1959  + t1969 + t1979),
         adj_1990 = h1990 / (t1939 + t1949 + t1959  + t1969 + t1979 + t1989),
         adj_2000 = h2000 / (t1939 + t1949 + t1959  + t1969 + t1979 + t1989 + t1999),
         adj_2010 = h2010 / (t1939 + t1949 + t1959  + t1969 + t1979 + t1989 + t1999 + t2009)) # Creates adjustment factors for each decade

joined <- full_join(dat_acs, data_sum) %>% # Joining the unsummed ACS data with the summed ACS housing units
  mutate(hat_1940 = (h1940 / t1939) * a1939, # This block of code creates the housing units from the Hammer Method
         hat_1950 = (h1950 / (t1939 + t1949)) * (a1939 + a1949),
         hat_1960 = (h1960 / (t1939 + t1949 + t1959)) * (a1939 + a1949 + a1959),
         hat_1970 = (h1970 / (t1939 + t1949 + t1959 + t1969))  * (a1939 + a1949 + a1959 + a1969),
         hat_1980 = (h1980 / (t1939 + t1949 + t1959 + t1969 + t1979)) * (a1939 + a1949 + a1959 + a1969 + a1979),
         hat_1990 = (h1990 / (t1939 + t1949 + t1959 + t1969 + t1979 + t1989)) * (a1939 + a1949 + a1959 + a1969 + a1979 + a1989),
         hat_2000 = (h2000 / (t1939 + t1949 + t1959 + t1969 + t1979 + t1989 + t1999)) * (a1939 + a1949 + a1959 + a1969 + a1979 + a1989 + a1999),
         hat_2010 = (h2010 / (t1939 + t1949 + t1959 + t1969 + t1979 + t1989 + t1999 + t2009)) * (a1939 + a1949 + a1959 + a1969 + a1979 + a1989 + a1999 + a2009),
         GROWTH = if_else(hat_2010 >= hat_1940, "grow", "decline")) %>%
  dplyr::select(GEOID, FIPS, hat_1940:GROWTH) %>%
  left_join(., dat_decen)

joinedt <- joined %>% # creating a tall dataset for the modeling
  gather(YEAR, HU, hat_1940:hat_2010) %>%
  mutate(YEAR = as.numeric(substr(YEAR, 5,9)))

fitted_models_decline <- joinedt %>%
  filter(GROWTH == "decline") %>%
  group_by(GEOID) %>%
  do(model = lm(log(HU) ~ YEAR, data = .)) %>% #exponential regression for areas projected to decline.
  tidy(model) %>%
  dplyr::select(-p.value, -statistic, -std.error) %>% # selecting just the coefficients
  spread(term, estimate) %>% # putting them on the same line
  left_join(., joined, by = "GEOID") %>% # rejoining with the estimates
  mutate(GROWTH = "decline", # making the projections.
         h2010 = exp(YEAR*2010 +  `(Intercept)`),
         adjfactor =  hat_2010 - h2010,
         p2020 = (exp(YEAR*2020 +  `(Intercept)`) + adjfactor)*pphu + P043001,
         p2030 = (exp(YEAR*2030 +  `(Intercept)`) + adjfactor)*pphu + P043001,
         p2040 = (exp(YEAR*2040 +  `(Intercept)`) + adjfactor)*pphu + P043001,
         p2050 = (exp(YEAR*2050 +  `(Intercept)`) + adjfactor)*pphu + P043001,
         p2060 = (exp(YEAR*2060 +  `(Intercept)`) + adjfactor)*pphu + P043001,
         p2070 = (exp(YEAR*2070 +  `(Intercept)`) + adjfactor)*pphu + P043001,
         p2080 = (exp(YEAR*2080 +  `(Intercept)`) + adjfactor)*pphu + P043001,
         p2090 = (exp(YEAR*2090 +  `(Intercept)`) + adjfactor)*pphu + P043001,
         p2100 = (exp(YEAR*2100 +  `(Intercept)`) + adjfactor)*pphu + P043001)

fitted_models_growth <- joinedt %>%
  filter(GROWTH == "grow") %>%
  group_by(GEOID) %>%
  do(model = lm(HU ~ YEAR, data = .)) %>%
  tidy(model) %>%
  dplyr::select(-p.value, -statistic, -std.error) %>%
  spread(term, estimate) %>%
  left_join(., joined, by = "GEOID")%>%
  mutate(GROWTH = "grow",
         h2010 = YEAR*2010 +  `(Intercept)`,
         adjfactor =  hat_2010 - h2010,
         p2020 = (YEAR*2020 +  `(Intercept)` + adjfactor)*pphu + P043001,
         p2030 = (YEAR*2030 +  `(Intercept)` + adjfactor)*pphu + P043001,
         p2040 = (YEAR*2040 +  `(Intercept)` + adjfactor)*pphu + P043001,
         p2050 = (YEAR*2050 +  `(Intercept)` + adjfactor)*pphu + P043001,
         p2060 = (YEAR*2060 +  `(Intercept)` + adjfactor)*pphu + P043001,
         p2070 = (YEAR*2070 +  `(Intercept)` + adjfactor)*pphu + P043001,
         p2080 = (YEAR*2080 +  `(Intercept)` + adjfactor)*pphu + P043001,
         p2090 = (YEAR*2090 +  `(Intercept)` + adjfactor)*pphu + P043001,
         p2100 = (YEAR*2100 +  `(Intercept)` + adjfactor)*pphu + P043001)

# Combining all the data
fitted_models <- rbind(fitted_models_decline, fitted_models_growth) %>%
  mutate(p2010 = (hat_2010+ adjfactor)*pphu + P043001,
         p2000 = (hat_2000+ adjfactor)*pphu + P043001) %>%
  dplyr::select(GEOID, p2000, p2010, p2020:p2100)

areas <- read_csv("DATA/County_Population.csv") %>%
  separate(ID, c("State", "Drop", "GEOID"), sep = "_")%>%
  dplyr::select(State, PLACE_NAME, FIPS=GEOID)%>%
  left_join(., historic)

areas$GEOID<-  str_pad(areas$GEOID, 12, pad = "0")
areas$FIPS<-  str_pad(areas$FIPS, 5, pad = "0")
# subsetting to just coastal counties
fin_dat <- fitted_models

# 0 block groups produce NaN. Setting them to 0.
fin_dat[is.na(fin_dat)]<- 0

tots <- fin_dat %>%
  mutate(FIPS = substr(GEOID,0,5)) %>%
  group_by(FIPS) %>%
  mutate(p2020u = p2020/sum(p2020),
         p2030u = p2030/sum(p2030),
         p2040u = p2040/sum(p2040),
         p2050u = p2050/sum(p2050),
         p2060u = p2060/sum(p2060),
         p2070u = p2070/sum(p2070),
         p2080u = p2080/sum(p2080),
         p2090u = p2090/sum(p2090),
         p2100u = p2100/sum(p2100))
controlled <- tots %>%
  dplyr::select(FIPS, GEOID, p2000, p2010, p2020u:p2100u) %>%
  gather(Year, Uncontrolled, p2000:p2100u) %>%
  mutate(YEAR = as.integer(substr(Year,2,5))) %>%
  dplyr::select(-Year) %>%
  left_join(., controls) %>%
  mutate(SSP1 = Uncontrolled*SSP1,
         SSP2 = Uncontrolled*SSP2,
         SSP3 = Uncontrolled*SSP3,
         SSP4 = Uncontrolled*SSP4,
         SSP5 = Uncontrolled*SSP5)
controlled$SSP1[is.na(controlled$SSP1)] <- controlled$Uncontrolled
controlled$SSP2[is.na(controlled$SSP2)] <- controlled$Uncontrolled
controlled$SSP3[is.na(controlled$SSP3)] <- controlled$Uncontrolled
controlled$SSP4[is.na(controlled$SSP4)] <- controlled$Uncontrolled
controlled$SSP5[is.na(controlled$SSP5)] <- controlled$Uncontrolled
 
controlled <- controlled %>%
  dplyr::select(-Uncontrolled) %>%
  mutate(STATE = substr(GEOID,1,2),
         COUNTY = substr(GEOID,3,5)) %>%
  filter(FIPS %in% unique(areas$FIPS))

z <- controlled %>%
  group_by(YEAR) %>%
  dplyr::summarise(SSP2 = sum(SSP2))

z2 <- controlled %>%
  filter(FIPS == "15001")


write_csv(controlled, "DATA-PROCESSED/blkgrp_projections_20002100_controlled.csv")