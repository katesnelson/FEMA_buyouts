#Combine the NHGIS boundary information with the NHGIS demographic tables
#Then join to FEMA buyout locations


library(ipumsr)
library(tidyverse)
library(sf)

wd<-getwd()

##########################################
#########Block-Group Data################
#########################################

bg<- st_read(paste0(wd,"/data/US_blck_grp_2010.shp")) #read in block group boundary shapefile

d.bg<-readRDS(paste0(wd,"/data/","processed_bg.rds")) #read in block group census data

bg$GISJOIN <- as.character(bg$GISJOIN)
d.bg$GISJOIN <- d.bg$`GIS Join Match Code`

sf.bg<-left_join(d.bg,bg, by="GISJOIN") %>% st_as_sf(.)

sf.bg <- sf.bg %>% mutate(bg_pop_density = bg_pop/(Shape_area * 1e-6), bg_housing_density = housing_units/(Shape_area* 1e-6)) #calculate densities in terms of #/area in sq km


##########################################
#########Census Tract Data################
#########################################


d.ct<-readRDS(paste0(wd,"/data/","processed_ct.rds")) #read in census tract census data
d.ct$GISJOIN <- d.ct$`Year-Specific GIS Join Match Code`

#because data is nominally integrated (not normalized like the block group data) need to pull each decade boundary file
ct10<- st_read(paste0(wd,"/data/US_tract_2010.shp")) %>% #read in tract boundary shapefile
        dplyr::select(.,GISJOIN,geometry) %>%
        mutate(.,GISJOIN = as.character(GISJOIN)) %>%
        st_transform(.,st_crs(sf.bg))
        
ct00<- st_read(paste0(wd,"/data/US_tract_2000.shp")) %>% #read in tract boundary shapefile
        dplyr::select(.,GISJOIN,geometry) %>%
        mutate(.,GISJOIN = as.character(GISJOIN)) %>%
        st_transform(.,st_crs(sf.bg))

ct90<- st_read(paste0(wd,"/data/US_tract_1990.shp")) %>% #read in tract boundary shapefile
        dplyr::select(.,GISJOIN,geometry) %>%
        mutate(.,GISJOIN = as.character(GISJOIN)) %>%
        st_transform(.,st_crs(sf.bg))

ct90_10<-list(ct90,ct00,ct10) #combine into a list for mapping

d.ct<- d.ct %>% group_by(`Row Source Year`) %>% group_split() #split into list for mapping

sf.ct<- map2_df(d.ct, ct90_10, ~left_join(.x,.y, by="GISJOIN"))  %>% st_as_sf(.) %>% #join census tract data to geometries by decade
          st_set_crs(.,st_crs(sf.bg)) #reset crs which is lost in mapping to dataframe


##########################################
#########County Data################
#########################################

cnty<- st_read(paste0(wd,"/data/US_county_2010.shp")) #read in block group boundary shapefile

d.cnty<-readRDS(paste0(wd,"/data/","processed_cnty_0922.rds")) #read in county census data

cnty$GISJOIN <- as.character(cnty$GISJOIN)
d.cnty$GISJOIN <- d.cnty$`GIS Join Match Code`

sf.cnty<-left_join(d.cnty,cnty, by="GISJOIN") %>% st_as_sf(.)

sf.cnty <- sf.cnty %>% mutate(cnty_pop_density = cnty_pop/(Shape_area * 1e-6), cnty_housing_density = housing_units/(Shape_area* 1e-6)) #calculate densities in terms of #/area in sq km


##############################################################################################
### Join the BlockGroup and Census Tract "Neighborhood" Data by Common Census Tract#########
############################################################################################

# Step 1: Join by shared tract GIS code (2010 variety)
  sf.bg <- sf.bg %>% mutate(NHGIS_CT_Code = substr(`GIS Join Match Code`, 1, 14))
  
  sf.nb <- left_join(sf.bg, sf.ct %>% st_drop_geometry(.), by=c("NHGIS_CT_Code"="GISJOIN", "Data Measurement Year" = "Row Source Year")) #this method fails for some locations where census tracts were split or eliminated, etc... between 1990 and 2010

  sf.nb_joined<- sf.nb %>% filter(., !is.na(`Year-Specific GIS Join Match Code`)) #filter to block groups with postive match with census tract

# Step 2: Pull out block groups not successfully joined to a tract and use a spatial intersect to identify candidate tracts for joining 
  sf.bg_missed<- sf.bg %>% filter(., !(paste0(`GIS Join Match Code`,`Data Measurement Year`) %in% 
                                         paste0(sf.nb_joined$`GIS Join Match Code`,sf.nb_joined$`Data Measurement Year`))) #get block groups not successfully joined by GEOID-Year combo
      
        sf.bg_missed2<- sf.bg_missed %>% group_by(`Data Measurement Year`) %>% group_split() #split into list for mapping
  
  sf.ct2<- sf.ct  %>% group_by(`Row Source Year`) %>% group_split() #split into list for mapping
      

  sf.nb_intersect<- map2(sf.bg_missed2, sf.ct2, ~st_join(.x,.y, join=st_intersects) %>% #join census tract data to block groups by spatial intersect by decade
                      group_by(GISJOIN.x) %>% 
                      filter(.,substr(`Year-Specific GIS Join Match Code`, 1,12)==substr(`GIS Join Match Code`, 1, 12) ) %>% #filter to places where we at least are getting close to the base tract (switch from 12 digit to 14 digit between 1990-2010)
                      filter(.,!duplicated(GISJOIN.x)) %>%  #now get rid of any duplicates by first record
                      ungroup(.) %>% as.data.frame(.)) %>% bind_rows() %>%
                      st_as_sf(.) %>% st_set_crs(.,st_crs(sf.bg)) %>%
                      rename(., GISJOIN = GISJOIN.x) %>% select(-GISJOIN.y)

# Step 3: Pull out block groups still not joined to a tract and spatial intersect with tracts that haven't been joined to a block group
  sf.bg_stillmissed<- sf.bg_missed %>% filter(., !(paste0(`GIS Join Match Code`,`Data Measurement Year`) %in% 
                                         paste0(sf.nb_intersect$`GIS Join Match Code`,sf.nb_intersect$`Data Measurement Year`))) %>%  #get block groups not successfully joined by GEOID-Year combo
                       group_by(`Data Measurement Year`) %>% group_split()
  
  sf.ct_missed<- sf.ct %>% filter(., !(`Year-Specific GIS Join Match Code` %in% sf.nb_intersect$`Year-Specific GIS Join Match Code` | 
                                         `Year-Specific GIS Join Match Code` %in% sf.nb_joined$`Year-Specific GIS Join Match Code`)) %>% #get tracts not successfully joined
                      group_by(`Row Source Year`) %>% group_split()
  
  sf.nb_intersect2<- map2(sf.bg_stillmissed, sf.ct_missed, ~st_join(.x,.y, join=st_intersects) %>% #join census tract data not previously joined to block groups not previously joined by spatial intersect by decade
                           group_by(GISJOIN.x) %>% 
                           filter(.,!duplicated(GISJOIN.x)) %>%  #now get rid of any duplicates by first record
                           ungroup(.) %>% as.data.frame(.)) %>% bind_rows()%>%
                           st_as_sf(.) %>% st_set_crs(.,st_crs(sf.bg)) %>%
                           rename(., GISJOIN = GISJOIN.x) %>% select(.,-GISJOIN.y)

# Step 4: put all block groups joined to tract data in a single dataframe (make sure we got all block groups with no duplication)
  sf.nb_full<-bind_rows(as.data.frame(sf.nb_joined),
                        as.data.frame(sf.nb_intersect), 
                        as.data.frame(sf.nb_intersect2)) %>% 
    filter(.,!duplicated(paste0(`GIS Join Match Code`,`Data Measurement Year`))) %>% #create complete block group data with joined tract info
    st_as_sf(.) %>% st_set_crs(.,st_crs(sf.bg))
  
  #plot(sf.nb_full[sf.nb_full$`Data Measurement Year`==1990 & sf.nb_full$`State Code`=="01","bg_pop"]) #quick test plot
  
#Check for missing tract info
  sf.ct_missed<- sf.ct %>% filter(., !(`NHGIS Integrated Geographic Unit Code` %in% sf.nb$`NHGIS Integrated Geographic Unit Code`)) #35560
  sf.ct_missed<- sf.ct %>% filter(., !(`NHGIS Integrated Geographic Unit Code` %in% sf.nb_full$`NHGIS Integrated Geographic Unit Code`)) #7812 --> not perfect, but much better
  sf.ct_missed<- sf.ct %>% filter(., !(`NHGIS Integrated Geographic Unit Code` %in% sf.nb_full$`NHGIS Integrated Geographic Unit Code`) & `Row Source Year` == 1990) #4547 --> as expected, most for earliest year
  sf.ct_missed<- sf.ct %>% filter(., !(`NHGIS Integrated Geographic Unit Code` %in% sf.nb_full$`NHGIS Integrated Geographic Unit Code`) & `Row Source Year` == 2000) #2309
  sf.ct_missed<- sf.ct %>% filter(., !(`NHGIS Integrated Geographic Unit Code` %in% sf.nb_full$`NHGIS Integrated Geographic Unit Code`) & `Row Source Year` == 2010) #956 --> unclear why some 2010 records don't match (should match by GEOID column)

#########################################################################
### ADD FEMA Buyout Data Summary to Neighborhood Data ###################
######################################################################

bo<-st_read(paste0(wd,"/data/","fema_add_shp.shp"))
bo<-st_transform(bo,st_crs(sf.nb_full)) #set the buyout point spatial ref system to the same as the census boundaries
bo<-bo[,c(1:5,70:82)] %>% filter (!is.na(Loc_name)) #keep only basic geocoding match info and FEMA info, get rid of records with no geometry match
bo2<-bo[sf.nb_full,] #buyouts that intersect with block groups --> just one that is lost

bo <- bo %>% mutate(decade = ifelse(USER_Fisca < 2000, "1990", 
                                    ifelse(USER_Fisca < 2010 & USER_Fisca >= 2000, "2000", 
                                           ifelse(USER_Fisca >= 2010,"2010", "NA"))))


test_bo<- bo %>% select(.,decade, USER_Price) %>% group_by(decade) %>% group_split() 
test_nb<-sf.nb_full %>% group_by(`Data Measurement Year`) %>% group_split()
neighborhood<-map2(test_nb, test_bo,
           ~ st_join(.x, .y,join=st_intersects) %>% #spatially join buyouts to block group geometries
             group_by(GISJOIN) %>%
             add_tally(.) %>%
             mutate(AvePrice=mean(as.numeric(as.character(USER_Price)), na.rm=T), count=ifelse(!is.na(decade),n, n-1)) %>% #count ifelse distinguishes between block groups without and buyouts and block groups that intersect with just one buyout (rendering an n of 1 in both cases)
             summarise_at(vars(-c(geometry)),first) %>%
             left_join(.,bg[,c("GISJOIN","geometry")], by="GISJOIN")) %>%   #reattach the blk geometries
             bind_rows() %>% 
             st_as_sf()

saveRDS(neighborhood,paste0(wd,"/data/neighborhood_with_bo_summary_0930.rds"))

#plot(neighborhood[neighborhood$`Data Measurement Year`==1990 & neighborhood$`State Code`=="01","count"]) #quick test plot
       

#######################################################################
### ADD FEMA Buyout Data Summary to Community Data ###################
######################################################################


sf.cnty<-st_transform(sf.cnty,st_crs(sf.nb)) #set the county spatial ref system to the same as the buyout points

test_cnty<-sf.cnty %>% group_by(`Data Measurement Year`) %>% group_split()
community<-map2(test_cnty, test_bo,
           ~ st_join(.x, .y,join=st_intersects) %>% #spatially join buyouts to county geometries
             group_by(GISJOIN) %>%
             add_tally(.) %>%
             mutate(AvePrice=mean(as.numeric(as.character(USER_Price)), na.rm=T), count=ifelse(!is.na(decade),n, n-1)) %>%
             summarise_at(vars(-c(geometry)),first) %>%
             left_join(.,cnty[,c("GISJOIN","geometry")], by="GISJOIN")) %>%   #reattach the blk geometries
             bind_rows() %>% 
             st_as_sf()

saveRDS(community,paste0(wd,"/data/community_with_bo_summary_0922.rds"))

###################################################################
### Now add Neighborhood data to Individual Buyout Records ########
##################################################################


test_bo<- bo %>% group_by(decade) %>% group_split() 

individuals <- map2(test_bo, test_nb,
                    ~ st_join(.x, .y, join=st_intersects)) %>% #spatially join block group data to buyout points
                    do.call("rbind",.)

saveRDS(individuals,paste0(wd,"/data/individual_bo_with_neighborhood_0930.rds"))



