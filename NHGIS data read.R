#Read in NHGIS data and do basic variable construction
#time-series from NHGIS at smallest available level and county level stored rowwise
#https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums.html


library(ipumsr)
library(tidyverse)
library(sf)

wd<-getwd()


##########################################
#########Block-Group Data################
#########################################

d.bg<-read_nhgis(paste0(wd,"/data/nhgis0015_ts_geog2010_blck_grp.csv")) #read in data

cdbk.bg<-read_ipums_codebook(paste0(wd,"/data/nhgis0015_ts_geog2010_blck_grp_codebook.txt")) #read in codebook

var_names<-cdbk.bg$var_info$var_label #extract variable names from codebook

colnames(d.bg)<-var_names #assign variable names to columns

head(d.bg)

d.bg<-d.bg[!duplicated(names(d.bg), fromLast=TRUE)] #remove duplicated columns based on colname

d.bg2<-d.bg %>% select(!(starts_with('Upper')|starts_with('Lower'))) #now remove columns with upper or lower bound

####Now Prep the data

d.bg3<-d.bg2 %>%  mutate(bg_pop = `Persons: Total`,
                        dependents = rowSums(cbind(`Persons: Under 5 years`, 
                          `Persons: 5 to 9 years`,
                          `Persons: 10 to 14 years`,
                          `Persons: 15 to 17 years`), na.rm=T),
                        seniors = rowSums(cbind(`Persons: 65 to 69 years`+
                          `Persons: 70 to 74 years`+
                          `Persons: 75 to 79 years`+
                          `Persons: 80 to 84 years`+
                          `Persons: 85 years and over`), na.rm=T),
                        working_age = rowSums(cbind(bg_pop, -dependents, -seniors), na.rm=T),
                        other = rowSums(cbind(`Persons: American Indian and Alaska Native (single race)`,
                          `Persons: Asian (single race)`,
                          `Persons: Native Hawaiian and Other Pacific Islander (single race)`,
                          `Persons: Some Other Race (single race)`,
                          `Persons: Two or More Races`), na.rm=T),
                        white = `Persons: White (single race)`,
                        black = `Persons: Black or African American (single race)`, 
                        hisp = `Persons: Hispanic or Latino`,
                        housing_units = `Housing units: Total`,
                        occ_housing = rowSums(cbind(`Housing units: Owner occupied`,
                          `Housing units: Renter occupied`), na.rm=T),
                        rural = `Housing units: Rural`,
                        urban = `Housing units: Urban`,
                        urban_center = `Housing units: Urban--Inside urbanized areas`,
                        urban_outside = `Housing units: Urban--Outside urbanized areas (in urban clusters)`,
                        home_own = `Housing units: Owner occupied`,
                        rent = `Housing units: Renter occupied`,
                        white_ho = `Housing units: Owner occupied ~ Householder is White (single race)`,
                        white_rent = `Housing units: Renter occupied ~ Householder is White (single race)`,
                        black_ho = `Housing units: Owner occupied ~ Householder is Black or African American (single race)`,
                        black_rent = `Housing units: Renter occupied ~ Householder is Black or African American (single race)`,
                        other_ho = rowSums(cbind(`Housing units: Owner occupied ~ Householder is Asian and Pacific Islander (single race)`,
                          `Housing units: Owner occupied ~ Householder is American Indian and Alaska Native (single race)`,
                          `Housing units: Owner occupied ~ Householder is Some Other Race (single race)`, 
                          `Housing units: Owner occupied ~ Householder is Two or More Races`), na.rm=T),
                        other_rent = rowSums(cbind(`Housing units: Renter occupied ~ Householder is American Indian and Alaska Native (single race)`,
                          `Housing units: Renter occupied ~ Householder is Asian and Pacific Islander (single race)`,
                          `Housing units: Renter occupied ~ Householder is Some Other Race (single race)` , 
                          `Housing units: Renter occupied ~ Householder is Two or More Races`), na.rm=T),
                        hisp_ho = `Housing units: Owner occupied ~ Hispanic or Latino householder`,
                        hisp_rent = `Housing units: Renter occupied ~ Hispanic or Latino householder`
                        ) %>% #build combined variables
                    mutate_at(vars(86:ncol(.)), ~ifelse (.<1,0,.))



d.bg3 <- d.bg3 %>% mutate(p.dependents = dependents/bg_pop*100,
                          p.seniors = seniors/bg_pop*100,
                          p.working = working_age/bg_pop*100,
                          p.white = white/bg_pop*100,
                          p.black = black/bg_pop*100,
                          p.other = other/bg_pop*100,
                          p.hisp = hisp/bg_pop*100,
                          p.rural = rural/housing_units*100,
                          p.urban = urban/housing_units*100,
                          p.urban_center = urban_center/housing_units*100,
                          p.urban_outside = urban_outside/housing_units*100,
                          p.home_own = home_own/occ_housing*100,
                          p.rent = rent/occ_housing*100,
                          p.white_ho = white_ho/home_own*100,
                          p.white_rent = white_rent/rent*100,
                          p.black_ho = black_ho/home_own*100,
                          p.black_rent = black_rent/rent*100,
                          p.other_ho = other_ho/home_own*100,
                          p.other_rent=other_rent/rent*100,
                          p.hisp_ho = hisp_ho/home_own*100,
                          p.hisp_rent = hisp_rent/rent*100)

d.bg <- d.bg3 %>% select(c(1:9,86:ncol(.))) 

saveRDS(d.bg, paste0(wd,"/data/", "processed_bg.rds"))

############################
###Census tract data########
############################

d.ct<-read_nhgis(paste0(wd,"/data/nhgis0015_ts_nominal_tract.csv"))

cdbk.ct<-read_ipums_codebook(paste0(wd,"/data/nhgis0015_ts_nominal_tract_codebook.txt")) #read in codebook

var_names<-cdbk.ct$var_info$var_label #extract variable names from codebook

colnames(d.ct)<-var_names #assign variable names to columns

head(d.ct)

d.ct<-d.ct[!duplicated(names(d.ct), fromLast=TRUE)] #remove duplicated columns based on colname

d.ct2<-d.ct %>% select(!(starts_with('Upper')|starts_with('Lower'))) #now remove columns with upper or lower bound

d.ct2$`Row Source Year`[d.ct2$`Row Source Year`=="2008-2012"]<-"2010"
d.ct2$`Row Source Year`<-as.numeric(d.ct2$`Row Source Year`)
d.ct2 <-d.ct2[d.ct2$'Row Source Year' >= 1990,]

####Now Prep the data

d.ct3<-d.ct2 %>% mutate(ct_pop = rowSums(cbind(`Persons: Native`+`Persons: Foreign born`),na.rm=T), 
                        pci = `Per capita income in previous year`,
                        foreign = `Persons: Foreign born`,
                        pop_18 = rowSums(cbind(`Persons: 18 years and over ~ Less than 9th grade`,
                          `Persons: 18 years and over ~ 9th to 12th grade, no diploma`, 
                          `Persons: 18 years and over ~ High school graduate, GED, or alternative`, 
                          `Persons: 18 years and over ~ Some college, no degree`,
                          `Persons: 18 years and over ~ Associate degree`,
                          `Persons: 18 years and over ~ Bachelor's degree` ,
                          `Persons: 18 years and over ~ Graduate or professional degree`), na.rm=T),
                        low_ed = rowSums(cbind(`Persons: 18 years and over ~ Less than 9th grade`,
                          `Persons: 18 years and over ~ 9th to 12th grade, no diploma`), na.rm=T),
                        high_ed = rowSums(cbind(`Persons: 18 years and over ~ Bachelor's degree`,
                          `Persons: 18 years and over ~ Graduate or professional degree`), na.rm=T)) %>% #build combined variables
                  mutate_at(vars(32:ncol(.)), ~ifelse (.<1,0,.))

d.ct3 <- d.ct3 %>% mutate(p.foreign = foreign/ct_pop*100,
                          p.low_ed = low_ed/pop_18*100,
                          p.high_ed = high_ed/pop_18*100
                          )

d.ct <- d.ct3 %>% select(c(1:11,32:ncol(.)))

saveRDS(d.ct, paste0(wd,"/data/", "processed_ct.rds"))

#############################
###County data ##############
#############################


d.cnty<-read_nhgis(paste0(wd,"/data/nhgis0015_ts_geog2010_county.csv")) #read in standardized data

cdbk.cnty<-read_ipums_codebook(paste0(wd,"/data/nhgis0015_ts_geog2010_county_codebook.txt")) #read in codebook

var_names<-cdbk.cnty$var_info$var_label #extract variable names from codebook

colnames(d.cnty)<-var_names #assign variable names to columns

d.cnty.n<-read_nhgis(paste0(wd,"/data/nhgis0015_ts_nominal_county.csv")) #read in nominal data

cdbk.cnty.n<-read_ipums_codebook(paste0(wd,"/data/nhgis0015_ts_nominal_county_codebook.txt")) #read in codebook

var_names<-cdbk.cnty.n$var_info$var_label #extract variable names from codebook

colnames(d.cnty.n)<-var_names #assign variable names to columns

d.cnty<-d.cnty[!duplicated(names(d.cnty), fromLast=TRUE)] #remove duplicated columns based on colname
d.cnty.n<-d.cnty.n[!duplicated(names(d.cnty.n), fromLast=TRUE)] #remove duplicated columns based on colname

d.cnty2<-d.cnty %>% select(!(starts_with('Upper')|starts_with('Lower'))) #now remove columns with upper or lower bound
d.cnty.n2<-d.cnty.n %>% select(!(starts_with('Upper')|starts_with('Lower')))  #now remove columns with upper or lower bound

# d.cnty2$`Row Source Year`[d.cnty2$`Row Source Year`=="2008-2012"]<-"2010"
d.cnty.n2$`Row Source Year`[d.cnty.n2$`Row Source Year`=="2008-2012"]<-"2010"
d.cnty.n2$`Row Source Year`<-as.numeric(d.cnty.n2$`Row Source Year`)
d.cnty.n2 <-d.cnty.n2[d.cnty.n2$'Row Source Year' >= 1990,]
d.cnty.f<-left_join(d.cnty2, d.cnty.n2, by=c("GIS Join Match Code","Data Measurement Year" = "Row Source Year"))

####Now Prep the data

d.cnty3<-d.cnty.f %>% mutate(cnty_pop = `Persons: Total`,
                            dependents = rowSums(cbind(`Persons: Under 5 years`, 
                                                       `Persons: 5 to 9 years`,
                                                       `Persons: 10 to 14 years`,
                                                       `Persons: 15 to 17 years`), na.rm=T),
                            seniors = rowSums(cbind(`Persons: 65 to 69 years`+
                                                      `Persons: 70 to 74 years`+
                                                      `Persons: 75 to 79 years`+
                                                      `Persons: 80 to 84 years`+
                                                      `Persons: 85 years and over`), na.rm=T),
                            working_age = rowSums(cbind(cnty_pop, -dependents, -seniors), na.rm=T),
                            other = rowSums(cbind(`Persons: American Indian and Alaska Native (single race)`,
                                                  `Persons: Asian (single race)`,
                                                  `Persons: Native Hawaiian and Other Pacific Islander (single race)`,
                                                  `Persons: Some Other Race (single race)`,
                                                  `Persons: Two or More Races`), na.rm=T),
                            white = `Persons: White (single race)`,
                            black = `Persons: Black or African American (single race)`, 
                            hisp = `Persons: Hispanic or Latino`,
                            housing_units = `Housing units: Total`,
                            occ_housing = rowSums(cbind(`Housing units: Owner occupied`,
                                                        `Housing units: Renter occupied`), na.rm=T),
                            rural = `Housing units: Rural`,
                            urban = `Housing units: Urban`,
                            urban_center = `Housing units: Urban--Inside urbanized areas`,
                            urban_outside = `Housing units: Urban--Outside urbanized areas (in urban clusters)`,
                            home_own = `Housing units: Owner occupied`,
                            rent = `Housing units: Renter occupied`,
                            white_ho = `Housing units: Owner occupied ~ Householder is White (single race)`,
                            white_rent = `Housing units: Renter occupied ~ Householder is White (single race)`,
                            black_ho = `Housing units: Owner occupied ~ Householder is Black or African American (single race)`,
                            black_rent = `Housing units: Renter occupied ~ Householder is Black or African American (single race)`,
                            other_ho = rowSums(cbind(`Housing units: Owner occupied ~ Householder is Asian and Pacific Islander (single race)`,
                                                     `Housing units: Owner occupied ~ Householder is American Indian and Alaska Native (single race)`,
                                                     `Housing units: Owner occupied ~ Householder is Some Other Race (single race)`, 
                                                     `Housing units: Owner occupied ~ Householder is Two or More Races`), na.rm=T),
                            other_rent = rowSums(cbind(`Housing units: Renter occupied ~ Householder is American Indian and Alaska Native (single race)`,
                                                       `Housing units: Renter occupied ~ Householder is Asian and Pacific Islander (single race)`,
                                                       `Housing units: Renter occupied ~ Householder is Some Other Race (single race)` , 
                                                       `Housing units: Renter occupied ~ Householder is Two or More Races`), na.rm=T),
                            hisp_ho = `Housing units: Owner occupied ~ Hispanic or Latino householder`,
                            hisp_rent = `Housing units: Renter occupied ~ Hispanic or Latino householder`,
                            pci = `Per capita income in previous year`,
                            foreign = `Persons: Foreign born`,
                            pop_18 = rowSums(cbind(`Persons: 18 years and over ~ Less than 9th grade`,
                                               `Persons: 18 years and over ~ 9th to 12th grade, no diploma`, 
                                               `Persons: 18 years and over ~ High school graduate, GED, or alternative`, 
                                               `Persons: 18 years and over ~ Some college, no degree`,
                                               `Persons: 18 years and over ~ Associate degree`,
                                               `Persons: 18 years and over ~ Bachelor's degree` ,
                                               `Persons: 18 years and over ~ Graduate or professional degree`), na.rm=T),
                            low_ed = rowSums(cbind(`Persons: 18 years and over ~ Less than 9th grade`,
                                               `Persons: 18 years and over ~ 9th to 12th grade, no diploma`), na.rm=T),
                            high_ed = rowSums(cbind(`Persons: 18 years and over ~ Bachelor's degree`,
                                                `Persons: 18 years and over ~ Graduate or professional degree`), na.rm=T)) %>% #build combined variables
                       mutate_at(vars(111:ncol(.)), ~ifelse (.<1,0,.))

d.cnty3 <- d.cnty3 %>% mutate(p.dependents = dependents/cnty_pop*100,
                          p.seniors = seniors/cnty_pop*100,
                          p.working = working_age/cnty_pop*100,
                          p.white = white/cnty_pop*100,
                          p.black = black/cnty_pop*100,
                          p.other = other/cnty_pop*100,
                          p.hisp = hisp/cnty_pop*100,
                          p.rural = rural/housing_units*100,
                          p.urban = urban/housing_units*100,
                          p.urban_center = urban_center/housing_units*100,
                          p.urban_outside = urban_outside/housing_units*100,
                          p.home_own = home_own/occ_housing*100,
                          p.rent = rent/occ_housing*100,
                          p.white_ho = white_ho/home_own*100,
                          p.white_rent = white_rent/rent*100,
                          p.black_ho = black_ho/home_own*100,
                          p.black_rent = black_rent/rent*100,
                          p.other_ho = other_ho/home_own*100,
                          p.other_rent=other_rent/rent*100,
                          p.hisp_ho = hisp_ho/home_own*100,
                          p.hisp_rent = hisp_rent/rent*100,
                          p.foreign = foreign/cnty_pop*100,
                          p.low_ed = low_ed/pop_18*100,
                          p.high_ed = high_ed/pop_18*100)


d.cnty <- d.cnty3 %>% select(c(1:7,111:ncol(.)))

saveRDS(d.cnty, paste0(wd,"/data/", "processed_cnty_0922.rds"))
