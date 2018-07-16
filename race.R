## Race by Age Table
## This program generates a race by age table using the ACS 5  yr estimates.
## The program aggregates age into two groups, under 18 and 18 and over.
## You can see the original table from American Fact Finder here: https://bit.ly/2KZZDGd
### Limitations- the B01001 does not break race out into non-Latino categories. Also, the table does not break-out estimates between household vs group quarter (e.g. inmates) residents. 
### As an alternative, consider the P.L. 94-171 Redistricting Data file https://www.census.gov/rdo

# set up environment 

library("tidycensus")
library("reshape2")

census_api_key("your API here",install = TRUE, overwrite = TRUE)

#view ACS API variables
v15 <- load_variables(2016, "acs5", cache = TRUE)

View(v15)

# download data for Queens, NY. Summary table B01001
Queens_long <- get_acs(
  geography = "county", 
  variables = c(
# White race    
    white_total = "B01001A_001E"
    
      ,white_m_U5="B01001A_003E"
      ,white_m_5t9="B01001A_004E"
      ,white_m_10t14="B01001A_005E"
      ,white_m_15t17="B01001A_006E"
      
      ,white_f_u5="B01001A_018E"
      ,white_f_5t9="B01001A_019E"
      ,white_f_10t14="B01001A_020E"
      ,white_f_15t17="B01001A_021E"
 # Black race   
    ,black_total = "B01001B_001E"
    
      ,black_m_U5="B01001B_003E"
      ,black_m_5t9="B01001B_004E"
      ,black_m_10t14="B01001B_005E"
      ,black_m_15t17="B01001B_006E"
      
      ,black_f_u5="B01001B_018E"
      ,black_f_5t9="B01001B_019E"
      ,black_f_10t14="B01001B_020E"
      ,black_f_15t17="B01001B_021E"
# American Indian and Alaska Native race    
    ,aian_total = "B01001C_001E"
    
      ,aian_m_U5="B01001C_003E"
      ,aian_m_5t9="B01001C_004E"
      ,aian_m_10t14="B01001C_005E"
      ,black_m_15t17="B01001C_006E"
      
      ,aian_f_u5="B01001C_018E"
      ,aian_f_5t9="B01001C_019E"
      ,aian_f_10t14="B01001C_020E"
      ,aian_f_15t17="B01001C_021E"
  # Asian race    
    ,asian_total = "B01001D_001E"
    
      ,asian_m_U5="B01001D_003E"
      ,asian_m_5t9="B01001D_004E"
      ,asian_m_10t14="B01001D_005E"
      ,asian_15t17="B01001D_006E"
      
      ,asian_f_u5="B01001D_018E"
      ,asian_f_5t9="B01001D_019E"
      ,asian_f_10t14="B01001D_020E"
      ,asian_f_15t17="B01001D_021E"
# Native. Hawaiian and Other Pacific. Islander Race    
    ,nhopi_total = "B01001E_001E"
    
      ,nhopi_m_U5="B01001E_003E"
      ,nhopi_m_5t9="B01001E_004E"
      ,nhopi_m_10t14="B01001E_005E"
      ,asian_15t17="B01001E_006E"
      
      ,nhopi_f_u5="B01001E_018E"
      ,nhopi_f_5t9="B01001E_019E"
      ,nhopi_f_10t14="B01001E_020E"
      ,nhopi_f_15t17="B01001E_021E"
# Some other race   
    ,sor_total = "B01001F_001E"
    
      ,sor_m_U5="B01001F_003E"
      ,sor_m_5t9="B01001F_004E"
      ,sor_m_10t14="B01001F_005E"
      ,sor_15t17="B01001F_006E"
      
      ,sor_f_u5="B01001F_018E"
      ,sor_f_5t9="B01001F_019E"
      ,sor_f_10t14="B01001F_020E"
      ,sor_f_15t17="B01001F_021E"
# Two or more races    
    ,tom_total = "B01001G_001E"
    
      ,tom_m_U5="B01001G_003E"
      ,tom_m_5t9="B01001G_004E"
      ,tom_m_10t14="B01001G_005E"
      ,tom_15t17="B01001G_006E"
      
      ,tom_f_u5="B01001G_018E"
      ,tom_f_5t9="B01001G_019E"
      ,tom_f_10t14="B01001G_020E"
      ,tom_f_15t17="B01001G_021E"
  )
  ,state = "36"
  ,county="081"
)

# reshape the file from long (an estimate for each row) to wide (a row for each county)
Queens_wide <- dcast(Queens_long , GEOID + NAME ~variable, value.var = "estimate")

# sum under 18 for White race 
Queens_wide$white_U18<-(                           Queens_wide$B01001A_003
                                                   +Queens_wide$B01001A_004
                                                   +Queens_wide$B01001A_005
                                                   +Queens_wide$B01001A_006
                                                   +Queens_wide$B01001A_018
                                                   +Queens_wide$B01001A_019
                                                   +Queens_wide$B01001A_020
                                                   +Queens_wide$B01001A_021)
# subtract sum under 18 for White race from total to generate 18+
Queens_wide$white_adults<-
  (Queens_wide$B01001A_001
   -Queens_wide$white_U18)       
# sum under 18 for Black race 
Queens_wide$black_U18<-(                           Queens_wide$B01001B_003
                                                   +Queens_wide$B01001B_004
                                                   +Queens_wide$B01001B_005
                                                   +Queens_wide$B01001B_006
                                                   +Queens_wide$B01001B_018
                                                   +Queens_wide$B01001B_019
                                                   +Queens_wide$B01001B_020
                                                   +Queens_wide$B01001B_021)
# subtract sum under 18 for Black race from total to generate 18+
Queens_wide$black_adults<-
  (Queens_wide$B01001B_001
   -Queens_wide$black_U18)  
# sum under 18 for American Indian and Alaska Native race 
Queens_wide$aian_U18<-(                           Queens_wide$B01001C_003
                                                  +Queens_wide$B01001C_004
                                                  +Queens_wide$B01001C_005
                                                  +Queens_wide$B01001C_006
                                                  +Queens_wide$B01001C_018
                                                  +Queens_wide$B01001C_019
                                                  +Queens_wide$B01001C_020
                                                  +Queens_wide$B01001C_021)
# subtract sum under 18 for American Indian and Alaska Native race from total to generate 18+
Queens_wide$aian_adults<-
  (Queens_wide$B01001C_001
   -Queens_wide$aian_U18)  
# sum under 18 for Asian race 
Queens_wide$asian_U18<-(                           Queens_wide$B01001D_003
                                                   +Queens_wide$B01001D_004
                                                   +Queens_wide$B01001D_005
                                                   +Queens_wide$B01001D_006
                                                   +Queens_wide$B01001D_018
                                                   +Queens_wide$B01001D_019
                                                   +Queens_wide$B01001D_020
                                                   +Queens_wide$B01001D_021)
# subtract sum under 18 for Asian race from total to generate 18+

Queens_wide$asian_adults<-
  (Queens_wide$B01001D_001
   -Queens_wide$asian_U18) 
# sum under 18 for Native. Hawaiian and Other Pacific. Islander Race    
Queens_wide$nhopi_U18<-(                           Queens_wide$B01001E_003
                                                   +Queens_wide$B01001E_004
                                                   +Queens_wide$B01001E_005
                                                   +Queens_wide$B01001E_006
                                                   +Queens_wide$B01001E_018
                                                   +Queens_wide$B01001E_019
                                                   +Queens_wide$B01001E_020
                                                   +Queens_wide$B01001E_021)
# subtract sum under 18 for Native. Hawaiian and Other Pacific. Islander race from total to generate 18+
Queens_wide$nhopi_adults<-
  (Queens_wide$B01001E_001
   -Queens_wide$nhopi_U18)  
# sum under 18 for some other race
Queens_wide$sor_U18<-(                           Queens_wide$B01001F_003
                                                 +Queens_wide$B01001F_004
                                                 +Queens_wide$B01001F_005
                                                 +Queens_wide$B01001F_006
                                                 +Queens_wide$B01001F_018
                                                 +Queens_wide$B01001F_019
                                                 +Queens_wide$B01001F_020
                                                 +Queens_wide$B01001F_021)
# subtract sum under 18 for some other race from total to generate 18+
Queens_wide$sor_adults<-
  (Queens_wide$B01001F_001
   -Queens_wide$sor_U18)  
# sum under 18 for two or more races
Queens_wide$tom_U18<-(                           Queens_wide$B01001G_003
                                                 +Queens_wide$B01001G_004
                                                 +Queens_wide$B01001G_005
                                                 +Queens_wide$B01001G_006
                                                 +Queens_wide$B01001G_018
                                                 +Queens_wide$B01001G_019
                                                 +Queens_wide$B01001G_020
                                                 +Queens_wide$B01001G_021)
# subtract sum under 18 for two or more races from total to generate 18+
Queens_wide$tom_adults<-
  (Queens_wide$B01001G_001
   -Queens_wide$tom_U18)  