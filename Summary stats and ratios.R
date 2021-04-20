#Extract summary statistics for nation, state, county, block-groups with and without buyouts
#Calculate ratios comparing values across these scales


library(tidyverse)
library(sf)
library(magrittr)
library(flextable)
library(stringr)

wd<-getwd()

#####################
### Read in Data ###
####################

community <- readRDS(paste0(wd,"/data/community_with_bo_summary_0922.rds"))

neighborhood <- readRDS(paste0(wd,"/data/neighborhood_with_bo_summary_0930.rds"))

#inflation adjust pci to 2019 dollars
library(blscrapeR)
df<-inflation_adjust(2019)
head(df)
df$year<-as.numeric(df$year)
community<-community %>% mutate(pciyr = ifelse(`Data Measurement Year` != 2010, `Data Measurement Year`-1, 2012))
community_2<-left_join(community,df[,c(1,3)], by=c("pciyr"="year"))
community_2$pci<-community_2$pci/community_2$adj_value

neighborhood<-neighborhood %>% mutate(pciyr = ifelse(`Data Measurement Year` != 2010, `Data Measurement Year`-1, 2012))
neighborhood_2<-left_join(neighborhood,df[,c(1,3)], by=c("pciyr"="year"))
neighborhood_2$pci<-neighborhood_2$pci/neighborhood_2$adj_value

community<-community_2
neighborhood<-neighborhood_2

# saveRDS(community, paste0(wd,"/data/community_with_bo_summary_020521.rds"))
# saveRDS(neighborhood, paste0(wd,"/data/neighborhood_with_bo_summary_020521.rds"))
###########################################################################
### Calculate Summary Stats at Different Scales using Neighborhood Data ###
###########################################################################

keep<-c("GISJOIN","GIS Join Match Code" ,"Geography Year" ,"Data Measurement Year" ,"State Name", "State Code","County Name",
        "County Code","Census Tract Code" ,"Block Group Code","p_dependents","p_seniors" ,"p_working" , "p_white" , "p_black","p_other",  "p_hisp",
        "p_rural", "p_urban" , "p_urban_center" ,"p_urban_outside" , "p_home_own" , "p_rent","p_white_ho" , "p_white_rent" ,"p_black_ho",
        "p_black_rent" ,"p_other_ho", "p_other_rent" , "p_hisp_ho","p_hisp_rent","bg_pop_density" , "bg_housing_density", "p_foreign", 
        "p_low_ed", "p_high_ed", "pci", "AvePrice" , "count" )

aggregate<-c("p_dependents","p_seniors" ,"p_working" , "p_white" , "p_black","p_other",  "p_hisp",
             "p_rural", "p_urban" , "p_urban_center" ,"p_urban_outside" , "p_home_own" , "p_rent","p_white_ho" , "p_white_rent" ,"p_black_ho",
             "p_black_rent" ,"p_other_ho", "p_other_rent" , "p_hisp_ho","p_hisp_rent","bg_pop_density" , "bg_housing_density", "p_foreign", 
             "p_low_ed", "p_high_ed", "pci", "AvePrice" , "count")

dat<-neighborhood %>% st_drop_geometry(.) %>% 
    group_by(`Data Measurement Year`) %>% 
    rename_all(~str_replace_all(.,"\\.","_")) %>% #the periods in the original dataframe cause headaches later on so replace with underscores
    select (all_of(keep)) %>%  
    group_split() #split neighborhood data by decade for mapping functions

  ## National Average of Neighborhood Stats for full time period (for summary table)
      national_summ<-bind_rows(dat) %>% summarise_at(.,vars(all_of(aggregate)), funs(mean=mean, sd=sd, min=min, max=max), na.rm=T)

      tbl<-regulartable(as.data.frame(t(national_summ))%>% rownames_to_column()) 
      tbl<-autofit(tbl)
      tbl<-colformat_num(tbl,col_keys=colnames(tbl), digits=2)
      print(tbl, preview="docx")
      
  ##NATIONAL AVERAGE of Neighborhood Stats by decade
  
    national<-purrr::map_df(dat, ~ summarise_at(.,vars(all_of(aggregate)), funs(mean=mean, sd=sd, min=min, max=max), na.rm=T)) 
    # saveRDS(national, "national.rds") #produces a dataframe with national averages of all census stats across block groups and decades
    
    nationalbo<-purrr::map_df(dat, ~filter(.,count > 0) %>% 
                                 summarise_at(.,vars(all_of(aggregate)), mean, na.rm=T)) %>% 
                                  set_rownames(c("1990","2000","2010")) 
    # saveRDS(nationalbo, "nationalbo.rds") 
    
    
    nationalnobo<-purrr::map_df(dat, ~filter(.,count == 0) %>% 
                                   summarise_at(.,vars(all_of(aggregate)), mean, na.rm=T)) %>% 
                                   set_rownames(c("1990","2000","2010"))
    # saveRDS(nationalnobo, "nationalnobo.rds")
    
    
    nat_ratio<- nationalbo/nationalnobo #within the nation, the propensity for buyout block group characteristics to vary from non-buyout block groups
    
    nat_ratio<-as_tibble(t(nat_ratio),rownames="Variable") %>%  tibble::rownames_to_column()
    nationalbo<-as_tibble(t(nationalbo),rownames="Variable") %>%  tibble::rownames_to_column()
    nationalnobo<-as_tibble(t(nationalnobo),rownames="Variable") %>%  tibble::rownames_to_column()
    
    #Build a summary table
    nat_summary<-cbind(nationalbo,nationalnobo,nat_ratio) %>% 
      set_colnames(c("RN1","Variable", "Buyout Areas '90","Buyout Areas '00","Buyout Areas '10",
                     "RN2","Variable2","Non-Buyout Areas '90", "Non-Buyout Areas '00", "Non-Buyout Areas '10", 
                     "RN3","Variable3","Buyout:Non-Buyout Ratio '90","Buyout:Non-Buyout Ratio '00","Buyout:Non-Buyout Ratio '10")) %>% 
      select(.,c(2:5,8:10,13:15)) 

        tbl<-regulartable(as.data.frame(nat_summary)) 
        tbl<-autofit(tbl)
        tbl<-colformat_num(tbl,col_keys=colnames(nat_summary), digits=2)
        print(tbl, preview="docx")
    
    
  ##STATE AVERAGES of Neighborhood Stats
  
    state<-purrr::map(dat, ~.x %>% group_by(`State Code`,`Data Measurement Year`) %>% 
                         summarise_at(.,vars(all_of(aggregate)), mean, na.rm=T) %>% rename_all(~str_replace_all(.,"\\.","_"))) 
    saveRDS(state, "state.rds") 
    
    statebo<-purrr::map(dat, ~.x %>% filter(.,count > 0) %>% group_by(`State Code`,`Data Measurement Year`) %>% 
                          summarise_at(.,vars(all_of(aggregate)), mean, na.rm=T) %>% rename_all(~str_replace_all(.,"\\.","_")))
    saveRDS(statebo, "statebo.rds") 
    
    statenobo<-purrr::map(dat, ~.x %>% filter(.,count == 0) %>% group_by(`State Code`,`Data Measurement Year`) %>% 
                            summarise_at(.,vars(all_of(aggregate)), mean, na.rm=T) %>% rename_all(~str_replace_all(.,"\\.","_")))
    saveRDS(statenobo, "statenobo.rds") 
    
    state_ratio<- purrr::map2_df(statenobo, statebo, ~left_join(.x,.y, by=c("State Code", "Data Measurement Year")) %>% #join the state summaries for the no buyout block groups and buyout blockgroups
      gather(var, val, -c(`State Code`, `Data Measurement Year`)) %>%  #make the census variable names a variable so there is only one column for all census record values
      separate(var, c('var', 'case'), sep = "\\.") %>% #create a column that describes whether a record belongs to the no buyouts block group case (x) or buyout block groups case (y)
      spread(case, val) %>% #break the census variable values into 2 columns one for all census variables in no buyouts block groups and one for all census variables in buyouts block groups
      mutate(ratio = y / x)) #take the ratio of each census variable under the 2 cases (no buyout and buyouts)
    
    state_ratio_w<-state_ratio[,c("State Code", "Data Measurement Year","ratio","var")] %>% spread(var,ratio)#convert back to wide format 
    #within each state, how do buyout block group characteristics tend to vary from non-buyout block groups
    
    
  ##COUNTY AVERAGES of Neighborhood Stats
  
    cnty<-purrr::map(dat, ~.x %>% group_by(`State Code`, `County Code`,`Data Measurement Year`) %>% 
                         summarise_at(.,vars(all_of(aggregate)), mean, na.rm=T) %>% 
                        mutate(COUNTYFP = paste0(`State Code`,`County Code`)))
    saveRDS(cnty, "cnty.rds") #average of all neighborhood stats at county level
    
    cntybo<-purrr::map(dat, ~.x %>% filter(.,count > 0) %>% group_by(`State Code`,`County Code`,`Data Measurement Year`) %>% 
                          summarise_at(.,vars(all_of(aggregate)), mean, na.rm=T) %>% 
                          mutate(COUNTYFP = paste0(`State Code`,`County Code`)))  #change order here to create COUNTYFP first then group?
    
    cntybo<-purrr::map(dat, ~.x %>% filter(.,count > 0) %>% mutate(COUNTYFP = paste0(`State Code`,`County Code`)) %>%
                        group_by(COUNTYFP, `Data Measurement Year`) %>% 
                         summarise_at(.,vars(all_of(aggregate)), mean, na.rm=T))  #change order here to create COUNTYFP first then group? No difference
    saveRDS(cntybo, "cntybo.rds") #average of all BUYOUT neighborhoods at county level
    
    cntynobo<-purrr::map(dat, ~.x %>% filter(.,count == 0) %>% group_by(`State Code`,`County Code`,`Data Measurement Year`) %>% 
                            summarise_at(.,vars(all_of(aggregate)), mean, na.rm=T) %>% 
                           mutate(COUNTYFP = paste0(`State Code`,`County Code`))) 
    saveRDS(cntynobo, "cntynobo.rds") #average of all NON-BUYOUT neighborhoods at county level
    
    cnty_ratio<- purrr::map2_df(cntynobo, cntybo, ~left_join(.x,.y, by=c("Data Measurement Year", "COUNTYFP")) %>% #join the cnty summaries for the no buyout block groups and buyout blockgroups
                                   gather(var, val, -c(`State Code`,`County Code`, `Data Measurement Year`,COUNTYFP)) %>%  #make the census variable names a variable so there is only one column for all census record values
                                   separate(var, c('var', 'case'), sep = "\\.") %>% #create a column that describes whether a record belongs to the no buyouts block group case (x) or buyout block groups case (y)
                                   spread(case, val) %>% #break the census variable values into 2 columns one for all census variables in no buyouts block groups and one for all census variables in buyouts block groups
                                   mutate(ratio = y / x)) #take the ratio of each census variable under the 2 cases (no buyout and buyouts)
    
    cnty_ratio_w<-cnty_ratio[,c("State Code", "County Code", "Data Measurement Year","COUNTYFP", "ratio","var")] %>% spread(var,ratio)#convert back to wide format 
    #within each county, how do buyout block group characteristics tend to vary from non-buyout block groups
    
#########################################################################
### Calculate Summary Stats at Different Scales using Community Data ###
########################################################################
    
keep2<-c("GISJOIN","GIS Join Match Code" ,"Geography Year" ,"Data Measurement Year" ,"State Name", "State Code","County Name",
         "County Code","p_dependents","p_seniors" ,"p_working" , "p_white" , "p_black","p_other",  "p_hisp",
         "p_rural", "p_urban" , "p_urban_center" ,"p_urban_outside" , "p_home_own" , "p_rent","p_white_ho" , "p_white_rent" ,"p_black_ho",
         "p_black_rent" ,"p_other_ho", "p_other_rent" , "p_hisp_ho","p_hisp_rent", "p_foreign", 
         "p_low_ed", "p_high_ed", "pci", "AvePrice" , "count", "cnty_pop_density", "cnty_housing_density" )

aggregate2<-c("p_dependents","p_seniors" ,"p_working" , "p_white" , "p_black","p_other",  "p_hisp",
              "p_rural", "p_urban" , "p_urban_center" ,"p_urban_outside" , "p_home_own" , "p_rent","p_white_ho" , "p_white_rent" ,"p_black_ho",
              "p_black_rent" ,"p_other_ho", "p_other_rent" , "p_hisp_ho","p_hisp_rent", "p_foreign", 
              "p_low_ed", "p_high_ed", "pci", "AvePrice" , "count", "cnty_pop_density", "cnty_housing_density" )

dat2<-community %>% st_drop_geometry(.) %>% 
  group_by(`Data Measurement Year`) %>% 
  rename_all(~str_replace_all(.,"\\.","_")) %>% #the periods in the original dataframe cause headaches later on so replace with underscores
  select (all_of(keep2)) %>%  
  group_split() #split neighborhood data by decade for mapping functions
    

    ##NATIONAL AVERAGE of Community Stats 
    
    national2<-purrr::map_df(dat2, ~ summarise_at(.,vars(all_of(aggregate2)), mean, na.rm=T)) 
    saveRDS(national2, "national2.rds") #produces a dataframe with national averages of all census stats across block groups and decades
    
    national2bo<-purrr::map_df(dat2, ~filter(.,count > 0) %>% 
                                 summarise_at(.,vars(all_of(aggregate2)), mean, na.rm=T)) %>% 
      set_rownames(c("1990","2000","2010")) 
    saveRDS(national2bo, "national2bo.rds") 
    
    
    national2nobo<-purrr::map_df(dat2, ~filter(.,count == 0) %>% 
                                   summarise_at(.,vars(all_of(aggregate2)), mean, na.rm=T)) %>% 
      set_rownames(c("1990","2000","2010"))
    saveRDS(national2nobo, "national2nobo.rds")
   
    
    nat2_ratio<-national2bo/national2nobo #within the nation, the propensity for buyout block group characteristics to vary from non-buyout block groups
    
    nat2_ratio<-as_tibble(t(nat2_ratio),rownames="Variable") %>%  tibble::rownames_to_column()
    national2bo<-as_tibble(t(national2bo),rownames="Variable") %>%  tibble::rownames_to_column()
    national2nobo<-as_tibble(t(national2nobo),rownames="Variable") %>%  tibble::rownames_to_column()
     
    #Build a summary table
    nat2_summary<-cbind(national2bo,national2nobo,nat2_ratio) %>% 
      set_colnames(c("RN1","Variable", "Buyout Areas '90","Buyout Areas '00","Buyout Areas '10",
                     "RN2","Variable2","Non-Buyout Areas '90", "Non-Buyout Areas '00", "Non-Buyout Areas '10", 
                     "RN3","Variable3","Buyout:Non-Buyout Ratio '90","Buyout:Non-Buyout Ratio '00","Buyout:Non-Buyout Ratio '10")) %>% 
      select(.,c(2:5,8:10,13:15)) 
    
    tbl<-regulartable(as.data.frame(nat2_summary)) 
    tbl<-autofit(tbl)
    tbl<-colformat_num(tbl,col_keys=colnames(nat2_summary), digits=2)
    print(tbl, preview="docx")
    
    
    ##STATE AVERAGES of Community Stats
    
    state2<-purrr::map(dat2, ~.x %>% group_by(`State Code`,`Data Measurement Year`) %>% 
                         summarise_at(.,vars(all_of(aggregate2)), mean, na.rm=T) %>% rename_all(~str_replace_all(.,"\\.","_"))) 
    saveRDS(state2, "state2.rds") 
    
    state2bo<-purrr::map(dat2, ~.x %>% filter(.,count > 0) %>% group_by(`State Code`,`Data Measurement Year`) %>% 
                          summarise_at(.,vars(all_of(aggregate2)), mean, na.rm=T) %>% rename_all(~str_replace_all(.,"\\.","_")))
    saveRDS(statebo, "state2bo.rds") 
    
    state2nobo<-purrr::map(dat2, ~.x %>% filter(.,count == 0) %>% group_by(`State Code`,`Data Measurement Year`) %>% 
                            summarise_at(.,vars(all_of(aggregate2)), mean, na.rm=T) %>% rename_all(~str_replace_all(.,"\\.","_")))
    saveRDS(statenobo, "state2nobo.rds") 
    
    state2_ratio<- purrr::map2_df(state2nobo, state2bo, ~left_join(.x,.y, by=c("State Code", "Data Measurement Year")) %>% #join the state summaries for the no buyout block groups and buyout blockgroups
                                   gather(var, val, -c(`State Code`, `Data Measurement Year`)) %>%  #make the census variable names a variable so there is only one column for all census record values
                                   separate(var, c('var', 'case'), sep = "\\.") %>% #create a column that describes whether a record belongs to the no buyouts block group case (x) or buyout block groups case (y)
                                   spread(case, val) %>% #break the census variable values into 2 columns one for all census variables in no buyouts block groups and one for all census variables in buyouts block groups
                                   mutate(ratio = y / x)) #take the ratio of each census variable under the 2 cases (no buyout and buyouts)
    
    state2_ratio_w<-state2_ratio[,c("State Code", "Data Measurement Year","ratio","var")] %>% spread(var,ratio)#convert back to wide format 
    #within each state, how do buyout block group characteristics tend to vary from non-buyout block groups
    
    
    

##########################################################################################
### Calculate Comparisons of Block Group Stats to other Scales using Neighborhood Data ###
###########################################################################################
#This set of calculations uses the block-group/census-tract (neighborhood) scale data extracted from IPUMS & aggregated to communities/states/nation


  ## County--> compares each buyout block group in a county to the average for all non-buyout block groups in the same county
     bo_bg<-purrr::map(dat, ~.x %>% filter(.,count > 0) %>% select(.,all_of(c(aggregate,"State Code","County Code", "Data Measurement Year", "GISJOIN"))))
     bg_cnty_ratio<-purrr::map2_df(bo_bg, cntynobo, ~ left_join(.x,.y, by=c("State Code","County Code", "Data Measurement Year")) %>% #join the county summaries for the no buyout block groups to buyout block groups
                                     gather(var, val, -c(`State Code`,`County Code`, `Data Measurement Year`,GISJOIN,COUNTYFP)) %>%  #make the census variable names a variable so there is only one column for all census record values
                                     separate(var, c('var', 'case'), sep = "\\.") %>% #create a column that describes whether a record belongs to the no buyouts block group case (x) or buyout block groups case (y)
                                     spread(case, val) %>% #break the census variable values into 2 columns one for all census variables in no buyouts block groups and one for all census variables in buyouts block groups
                                     mutate(ratio = x / y))#take the ratio of each census variable under the 2 cases (average of no buyout bgs in county and buyout bgs)
  
    #convert back to wide format 
    bg_cnty_ratio_w<-bg_cnty_ratio[,c("GISJOIN","County Code","State Code", "Data Measurement Year", "ratio","var")] %>% spread(var,ratio)
    write.csv(bg_cnty_ratio_w, file = "bgcntyratio.csv")
    
        nrow(bg_cnty_ratio_w %>% filter(.,(`Data Measurement Year`==1990 & p_white_rent > 1)))/nrow(bg_cnty_ratio_w %>% filter(.,`Data Measurement Year`==1990)) #for pulling average stats for paper
        nrow(bg_cnty_ratio_w %>% filter(.,(`Data Measurement Year`==2000 & p_hisp_ho > 1)))/nrow(bg_cnty_ratio_w %>% filter(.,`Data Measurement Year`==2000)) #for pulling average stats for paper
        nrow(bg_cnty_ratio_w %>% filter(.,(`Data Measurement Year`==2010 & p_white_rent > 1)))/nrow(bg_cnty_ratio_w %>% filter(.,`Data Measurement Year`==2010)) #for pulling average stats for paper
        
    
        ### Build Plots ###
        
        w<- ggplot(bg_cnty_ratio_w %>% select(p_white, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_white, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) + #try adding boxplots
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative % White") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=700, x=p_white, group = Decade, fill=Decade), width=500, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        b<- ggplot(bg_cnty_ratio_w %>% select(p_black, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_black, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) + #try adding boxplots
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative % Black") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=300, x=p_black, group = Decade, fill=Decade), width=200, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        o<- ggplot(bg_cnty_ratio_w %>% select(p_other, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_other, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) + #try adding boxplots
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative % Other Race") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=150, x=p_other, group=Decade, fill=Decade), width=100, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        hl<- ggplot(bg_cnty_ratio_w %>% select(p_hisp, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_hisp, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) + #try adding boxplots
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative % Hispanic/Latinx") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=150, x=p_hisp, group=Decade, fill=Decade), width=100, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        
        figure1<-ggpubr::ggarrange(w,
                                   b + theme( axis.title.y = element_blank()),
                                   o,
                                   hl + theme( axis.title.y = element_blank()),
                                   ncol=2,nrow=2, common.legend = TRUE, legend = "bottom")
        figure1
        ggsave("neigh_community_demo_0930.jpeg", device="jpeg", width = 6, height = 6, units = "in")
        
        
        pci<- ggplot(bg_cnty_ratio_w %>% select(pci, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=pci, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1, adjust=2) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative Per Capita Income") + ylab ("Count") + xlim(0,6) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=250, x=pci, group=Decade, fill=Decade), width=250, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        pop<- ggplot(bg_cnty_ratio_w %>% select(bg_pop_density, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=bg_pop_density, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1, adjust=2) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative Population Density") + ylab ("Count") + xlim(0,6) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=150, x=bg_pop_density, group=Decade, fill=Decade), width=125, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        figure1<-ggpubr::ggarrange(pop,  
                                   pci + theme( axis.title.y = element_blank()),
                                   ncol=2, nrow=1,
                                   common.legend = TRUE, legend = "bottom")
        figure1
        ggsave("neigh_community_pop_pci_020521.jpeg", device="jpeg", width = 6, height = 3, units = "in")
        
        ed<- ggplot(bg_cnty_ratio_w %>% select(p_low_ed, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_low_ed, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1, adjust=2) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative Low Education Population") + ylab ("Count") + xlim(0,6) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=300, x=p_low_ed, group=Decade, fill=Decade), width=200, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        dep<- ggplot(bg_cnty_ratio_w %>% select(p_dependents, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_dependents, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative Dependent Population") + ylab ("Count") + xlim(0,3) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=400, x=p_dependents, group=Decade, fill=Decade), width=400, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        sen<- ggplot(bg_cnty_ratio_w %>% select(p_seniors, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_seniors, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative Senior Population") + ylab ("Count") + xlim(0,3) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=250, x=p_seniors, group=Decade, fill=Decade), width=200, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        wrk<- ggplot(bg_cnty_ratio_w %>% select(p_working, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_working, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative Working Age Population") + ylab ("Count") + xlim(0,3) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=800, x=p_working, grou=Decade, fill=Decade), width=700, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        figure1<-ggpubr::ggarrange(ed,
                                   dep + theme( axis.title.y = element_blank()),
                                   sen,
                                   wrk + theme( axis.title.y = element_blank()),
                                   ncol=2,nrow=2, common.legend = TRUE, legend = "bottom")
        figure1
        ggsave("neigh_community_ed_age_0930.jpeg", device="jpeg", width = 6, height = 6, units = "in")
        
        w<- ggplot(bg_cnty_ratio_w %>% select(p_white_ho, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_white_ho, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Realtive % White") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=750, x=p_white_ho, group=Decade, fill=Decade), width=600, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        b<- ggplot(bg_cnty_ratio_w %>% select(p_black_ho, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_black_ho, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Realtive % Black") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=350, x=p_black_ho, group=Decade, fill=Decade), width=300, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        
        o<- ggplot(bg_cnty_ratio_w %>% select(p_other_ho, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_other_ho, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Realtive % Other Race") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=300, x=p_other_ho, group=Decade, fill=Decade), width=175, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        
        hl<- ggplot(bg_cnty_ratio_w %>% select(p_hisp_ho, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_hisp_ho, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Realtive % Hispanic/Latinx") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=300, x=p_hisp_ho, group=Decade, fill=Decade), width=175, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        figure1<-ggpubr::ggarrange(w,
                                   b + theme( axis.title.y = element_blank()),
                                   o,
                                   hl + theme( axis.title.y = element_blank()),
                                   ncol=2,nrow=2, common.legend = TRUE, legend = "bottom")
        figure1
        ggsave("neigh_community_homeowner_0930.jpeg", device="jpeg", width = 6, height = 6, units = "in")
      
  ## STATE--> compares each buyout block group in a state to the average for all non-buyout block groups in the same state
    bg_st_ratio<-purrr::map2_df(bo_bg, statenobo, ~ left_join(.x,.y, by=c("State Code","Data Measurement Year")) %>% #join the county summaries for the no buyout block groups to buyout block groups
                                  gather(var, val, -c(`State Code`,`County Code`, `Data Measurement Year`,GISJOIN)) %>%  #make the census variable names a variable so there is only one column for all census record values
                                  separate(var, c('var', 'case'), sep = "\\.") %>% #create a column that describes whether a record belongs to the no buyouts block group case (x) or buyout block groups case (y)
                                  spread(case, val) %>% #break the census variable values into 2 columns one for all census variables in no buyouts block groups and one for all census variables in buyouts block groups
                                  mutate(ratio = x / y))#take the ratio of each census variable under the 2 cases (average of no buyout bgs in county and buyout bgs)
  
    #convert back to wide format 
    bg_st_ratio_w<-bg_st_ratio[,c("GISJOIN","County Code","State Code", "Data Measurement Year", "ratio","var")] %>% spread(var,ratio)
    write.csv(bg_st_ratio_w, file = "bgstratio.csv")
    
   
  ## NATIONAL--> compares each buyout block group  to the average for all non-buyout block groups in the nation
    # bo_blks<-dat[dat$cnt > 0 ,c(1,2,3,17,19,23:55)]
    # bo_blks$Nation<-"US"
    # national_nobo$Nation<-"US"
    # blckgrp_n<-left_join(bo_blks,national_nobo[,-1], by="Nation")
    # blckgrp_n_ratio<-left_join(bo_blks,national_nobo, by="Nation") %>% #join the state summaries for the no buyout block groups to buyout block groups
    #   gather(var, val, -c(GISJOIN, STATEFP10, STATE, COUNTYFP10, COUNTY, Nation)) %>%  #make the census variable names a variable so there is only one column for all census record values
    #   separate(var, c('var', 'case'), sep = "\\.") %>% #create a column that describes whether a record belongs to the no buyouts block group case (x) or buyout block groups case (y)
    #   spread(case, val) %>% #break the census variable values into 2 columns one for all census variables in no buyouts block groups and one for all census variables in buyouts block groups
    #   mutate(ratio = x / y) #take the ratio of each census variable under the 2 cases (average of no buyout bgs in state and buyout bgs)
    # 
    # #convert back to wide format and quick plot
    # blckgrp_n_ratio_w<-blckgrp_n_ratio[,c("GISJOIN","STATE","COUNTY", "ratio","var")] %>% spread(var,ratio)
    # write.csv(blckgrp_n_ratio_w, file = "blknratio90.csv")
    
#########################################################################################
### Calculate Comparisons of Community Stats to other Scales using Neighborhood Data ###
########################################################################################
#This set of calculations uses the county aggregated block-group/census-tract (neighborhood) scale data extracted from IPUMS & neighborhood data aggregated to states/nation


  ## NATIONAL--> uses the average for ALL block groups in a county-WITH-buyouts and compares to the average of ALL block groups in counties_WITHOUT-buyouts across the nation
    bo_cntys<-purrr::map2(cnty, cntybo, ~.x %>% filter(COUNTYFP %in% .y$COUNTYFP) %>% ungroup(.)%>%
                            select(., all_of(c(aggregate, "COUNTYFP", "Data Measurement Year")))) #get average census values for all block groups in a county for counties with buyouts
    nobo_cntys<-purrr::map2(cnty, cntybo, ~.x %>% filter(!(COUNTYFP %in% .y$COUNTYFP)) %>% ungroup(.) %>%
                              select(., all_of(c(aggregate, "COUNTYFP","Data Measurement Year")))) #reverse of above
    nat_nobocnty<-purrr::map(nobo_cntys, ~.x %>% group_by(`Data Measurement Year`) %>% 
                               summarise_at(vars(all_of(aggregate)), mean, na.rm=T)) #get average across cnty values for all counties WITHOUT buyouts in the nation
    
   cnty_n_ratio<-purrr::map2_df(bo_cntys , nat_nobocnty, 
                                 ~left_join(.x,.y, by=c("Data Measurement Year")) %>% #join the county summaries for the no buyout block groups to buyout block groups
                                  gather(var, val, -c(`Data Measurement Year`,COUNTYFP)) %>%  #make the census variable names a variable so there is only one column for all census record values
                                  separate(var, c('var', 'case'), sep = "\\.") %>% #create a column that describes whether a record belongs to the no buyouts block group case (x) or buyout block groups case (y)
                                  spread(case, val) %>% #break the census variable values into 2 columns one for all census variables in no buyouts block groups and one for all census variables in buyouts block groups
                                  mutate(ratio = x / y))#take the ratio of each census variable under the 2 cases (average of no buyout bgs in county and buyout bgs)
    
    #convert back to wide format 
    cnty_n_ratio_w<-cnty_n_ratio[,c("COUNTYFP","Data Measurement Year", "ratio","var")] %>% spread(var,ratio)
    write.csv(cnty_n_ratio_w, file = "cntynratio.csv")
    
    
          ### Build Plots ###
          
          w<- ggplot(cnty_n_ratio_w %>% select(p_white, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=p_white, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) + #try adding boxplots
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Relative % White") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=200, x=p_white, group = Decade, fill=Decade), width=200, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
          b<- ggplot(cnty_n_ratio_w %>% select(p_black, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=p_black, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) + #try adding boxplots
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Relative % Black") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=50, x=p_black, group = Decade, fill=Decade), width=75, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
          o<- ggplot(cnty_n_ratio_w %>% select(p_other, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=p_other, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) + #try adding boxplots
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Relative % Other Race") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=75, x=p_other, group=Decade, fill=Decade), width=100, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
          hl<- ggplot(cnty_n_ratio_w %>% select(p_hisp, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=p_hisp, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) + #try adding boxplots
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Relative % Hispanic/Latinx") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=75, x=p_hisp, group=Decade, fill=Decade), width=100, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
          
              figure1<-ggpubr::ggarrange(w,
                                         b + theme( axis.title.y = element_blank()),
                                         o,
                                         hl + theme( axis.title.y = element_blank()),
                                         ncol=2,nrow=2, common.legend = TRUE, legend = "bottom")
              figure1
              ggsave("community_nation_demo_0930.jpeg", device="jpeg", width = 6, height = 6, units = "in")
          
          
          pci<- ggplot(cnty_n_ratio_w %>% select(pci, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=pci, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Relative Per Capita Income") + ylab ("Count") + xlim(0,6) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=100, x=pci, group=Decade, fill=Decade), width=100, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
          pop<- ggplot(cnty_n_ratio_w %>% select(bg_pop_density, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=bg_pop_density, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1, adjust=2) +
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Relative Population Density") + ylab ("Count") + xlim(0,6) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=25, x=bg_pop_density, group=Decade, fill=Decade), width=30, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
              figure1<-ggpubr::ggarrange(pop,  
                                         pci + theme( axis.title.y = element_blank()),
                                         ncol=2, nrow=1,
                                         common.legend = TRUE, legend = "bottom")
              figure1
              ggsave("community_nation_pop_pci_020521.jpeg", device="jpeg", width = 6, height = 3, units = "in")
          
          ed<- ggplot(cnty_n_ratio_w %>% select(p_low_ed, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=p_low_ed, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Relative Low Education Population") + ylab ("Count") + xlim(0,3) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=50, x=p_low_ed, group=Decade, fill=Decade), width=50, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
          dep<- ggplot(cnty_n_ratio_w %>% select(p_dependents, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=p_dependents, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Relative Dependent Population") + ylab ("Count") + xlim(0,3) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=200, x=p_dependents, group=Decade, fill=Decade), width=200, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
          sen<- ggplot(cnty_n_ratio_w %>% select(p_seniors, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=p_seniors, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Relative Senior Population") + ylab ("Count") + xlim(0,3) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=100, x=p_seniors, grou=Decade, fill=Decade), width=100, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
          wrk<- ggplot(cnty_n_ratio_w %>% select(p_working, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=p_working, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Relative Working Age Population") + ylab ("Count") + xlim(0,3) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=250, x=p_working, grou=Decade, fill=Decade), width=250, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
              figure1<-ggpubr::ggarrange(ed,
                                         dep + theme( axis.title.y = element_blank()),
                                         sen,
                                         wrk + theme( axis.title.y = element_blank()),
                                         ncol=2,nrow=2, common.legend = TRUE, legend = "bottom")
              figure1
              
              ggsave("community_nation_ed_age_0930.jpeg", device="jpeg", width = 6, height = 6, units = "in")
          
          w<- ggplot(cnty_n_ratio_w %>% select(p_white_ho, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=p_white_ho, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Realtive % White") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=150, x=p_white_ho, group=Decade, fill=Decade), width=150, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
          b<- ggplot(cnty_n_ratio_w %>% select(p_black_ho, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=p_black_ho, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Realtive % Black") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=50, x=p_black_ho, group=Decade, fill=Decade), width=75, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
          
          o<- ggplot(cnty_n_ratio_w %>% select(p_other_ho, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=p_other_ho, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Realtive % Other Race") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=75, x=p_other_ho, group=Decade, fill=Decade), width=100, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
          
          hl<- ggplot(cnty_n_ratio_w %>% select(p_hisp_ho, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
            geom_density(aes(x=p_hisp_ho, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
            theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
            xlab ("Realtive % Hispanic/Latinx") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
            ggstance::geom_boxploth(aes(y=75, x=p_hisp_ho, group=Decade, fill=Decade), width=100, outlier.alpha = 0) +
            scale_fill_brewer(palette="RdBu")
          
              figure1<-ggpubr::ggarrange(w,
                                         b + theme( axis.title.y = element_blank()),
                                         o,
                                         hl + theme( axis.title.y = element_blank()),
                                         ncol=2,nrow=2, common.legend = TRUE, legend = "bottom")
              figure1
              
              ggsave("community_nation_homeowner_0930.jpeg", device="jpeg", width = 6, height = 6, units = "in")
              

  ## STATE--> uses the average for ALL block groups in a county and compares across counties with and without buyouts
  # bo_cntys2<-left_join(bo_cntys[,-34],unique(dat[,c("COUNTYFP10","STATEFP10")]), by="COUNTYFP10")
  # 
  # cnty_st_ratio<-left_join(bo_cntys2,state_nobo, by="STATEFP10") %>% #join the county summaries for the no buyout block groups to buyout block groups
  #   gather(var, val, -c(COUNTYFP10, STATEFP10, Nation, Loc_name)) %>%  #make the census variable names a variable so there is only one column for all census record values
  #   separate(var, c('var', 'case'), sep = "\\.") %>% #create a column that describes whether a record belongs to the no buyouts block group case (x) or buyout block groups case (y)
  #   spread(case, val) %>% #break the census variable values into 2 columns one for all census variables in no buyouts block groups and one for all census variables in buyouts block groups
  #   mutate(ratio = x / y) #take the ratio of each census variable under the 2 cases (average of no buyout bgs in county and buyout bgs)
  # 
  #   #convert back to wide format 
  #   
  #   cnty_st_ratio_w<-cnty_st_ratio[,c("COUNTYFP10","ratio","var")] %>% spread(var,ratio)
  #   write.csv(cnty_st_ratio_w, file = "cntystratio90.csv")

    
##############################################################
### Calculate Scale Comparison Stats using Community Data ###
#############################################################

# This set of calculations uses the county (community) scale data extracted from IPUMS
# (tests to confirm that there is no fallacy in comparing the average of community  neighborhoods instead of the census community values)


              
  ##COUNTY (community) COMPARISON TO NATIONAL AVERAGES - compares each county value from IPUMS to the national average of values for counties with and without buyouts 
    bo_cntys2<-purrr::map(dat2, ~.x %>% filter(count > 0) %>% select (all_of(c(aggregate2, "Data Measurement Year", "GISJOIN")))) #get  census values for counties with any buyouts
    nobo_cntys2<-purrr::map(dat2, ~.x %>% filter(count == 0)) #get  census values for counties with NO buyouts
    nat_nobocnty2<-purrr::map(nobo_cntys2, ~.x %>% group_by(`Data Measurement Year`) %>% 
                               summarise_at(vars(all_of(aggregate2)), mean, na.rm=T)) #get average across cnty values for all counties WITHOUT buyouts in the nation
    
    cnty_n_ratio2<-purrr::map2_df(bo_cntys2, nat_nobocnty2, 
                                 ~left_join(.x,.y, by=c("Data Measurement Year")) %>% #join the county summaries for the no buyout block groups to buyout block groups
                                   gather(var, val, -c(`Data Measurement Year`,GISJOIN)) %>%  #make the census variable names a variable so there is only one column for all census record values
                                   separate(var, c('var', 'case'), sep = "\\.") %>% #create a column that describes whether a record belongs to the no buyouts block group case (x) or buyout block groups case (y)
                                   spread(case, val) %>% #break the census variable values into 2 columns one for all census variables in no buyouts block groups and one for all census variables in buyouts block groups
                                   mutate(ratio = x / y))#take the ratio of each census variable under the 2 cases (average of no buyout bgs in county and buyout bgs)
    
    #convert back to wide format 
    cnty_n_ratio_w2<-cnty_n_ratio2[,c("GISJOIN","Data Measurement Year", "ratio","var")] %>% spread(var,ratio)
    write.csv(cnty_n_ratio_w2, file = "cntynratio2.csv")
    
       nrow(cnty_n_ratio_w2 %>% filter(.,(`Data Measurement Year`==1990 & pci > 1)))/nrow(cnty_n_ratio_w2 %>% filter(.,`Data Measurement Year`==1990)) #for pulling average stats for paper
       nrow(cnty_n_ratio_w2 %>% filter(.,(`Data Measurement Year`==2000 & pci > 1)))/nrow(cnty_n_ratio_w2 %>% filter(.,`Data Measurement Year`==2000)) #for pulling average stats for paper
       nrow(cnty_n_ratio_w2 %>% filter(.,(`Data Measurement Year`==2010 & pci > 1)))/nrow(cnty_n_ratio_w2 %>% filter(.,`Data Measurement Year`==2010)) #for pulling average stats for paper
       
        ### Build Plots ###
        
        w<- ggplot(cnty_n_ratio_w2 %>% select(p_white, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_white, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) + #try adding boxplots
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative % White") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=200, x=p_white, group = Decade, fill=Decade), width=200, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        b<- ggplot(cnty_n_ratio_w2 %>% select(p_black, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_black, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) + #try adding boxplots
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative % Black") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=50, x=p_black, group = Decade, fill=Decade), width=75, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        o<- ggplot(cnty_n_ratio_w2 %>% select(p_other, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_other, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) + #try adding boxplots
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative % Other Race") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=75, x=p_other, group=Decade, fill=Decade), width=100, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        hl<- ggplot(cnty_n_ratio_w2 %>% select(p_hisp, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_hisp, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) + #try adding boxplots
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative % Hispanic/Latinx") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=75, x=p_hisp, group=Decade, fill=Decade), width=100, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        
        figure1<-ggpubr::ggarrange(w,
                                   b + theme( axis.title.y = element_blank()),
                                   o,
                                   hl + theme( axis.title.y = element_blank()),
                                   ncol=2,nrow=2, common.legend = TRUE, legend = "bottom")
        figure1
        ggsave("community_nation_demo_020521_b.jpeg", device="jpeg", width = 6, height = 6, units = "in")
        
        
        pci<- ggplot(cnty_n_ratio_w2 %>% select(pci, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=pci, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative Per Capita Income") + ylab ("Count") + xlim(0,3) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=100, x=pci, group=Decade, fill=Decade), width=100, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        pop<- ggplot(cnty_n_ratio_w2 %>% select(cnty_pop_density, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=cnty_pop_density, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1, adjust=2) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative Population Density") + ylab ("Count") + xlim(0,6) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=75, x=cnty_pop_density, group=Decade, fill=Decade), width=50, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        figure1<-ggpubr::ggarrange(pop,  
                                   pci + theme( axis.title.y = element_blank()),
                                   ncol=2, nrow=1,
                                   common.legend = TRUE, legend = "bottom")
        figure1
        ggsave("community_nation_pop_pci_020521_b.jpeg", device="jpeg", width = 6, height = 3, units = "in")
        
        ed<- ggplot(cnty_n_ratio_w2 %>% select(p_low_ed, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_low_ed, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative Low Education Population") + ylab ("Count") + xlim(0,3) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=50, x=p_low_ed, group=Decade, fill=Decade), width=50, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        dep<- ggplot(cnty_n_ratio_w2 %>% select(p_dependents, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_dependents, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative Dependent Population") + ylab ("Count") + xlim(0,3) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=200, x=p_dependents, group=Decade, fill=Decade), width=200, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        sen<- ggplot(cnty_n_ratio_w2 %>% select(p_seniors, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_seniors, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative Senior Population") + ylab ("Count") + xlim(0,3) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=100, x=p_seniors, grou=Decade, fill=Decade), width=100, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        wrk<- ggplot(cnty_n_ratio_w2 %>% select(p_working, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_working, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Relative Working Age Population") + ylab ("Count") + xlim(0,3) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=250, x=p_working, group=Decade, fill=Decade), width=250, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        figure1<-ggpubr::ggarrange(ed,
                                   dep + theme( axis.title.y = element_blank()),
                                   sen,
                                   wrk + theme( axis.title.y = element_blank()),
                                   ncol=2,nrow=2, common.legend = TRUE, legend = "bottom")
        figure1
        ggsave("community_nation_ed_age_020521_b.jpeg", device="jpeg", width = 6, height = 6, units = "in")
        
        w<- ggplot(cnty_n_ratio_w2 %>% select(p_white_ho, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_white_ho, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Realtive % White") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=150, x=p_white_ho, group=Decade, fill=Decade), width=150, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        b<- ggplot(cnty_n_ratio_w2 %>% select(p_black_ho, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_black_ho, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Realtive % Black") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=50, x=p_black_ho, group=Decade, fill=Decade), width=75, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        
        o<- ggplot(cnty_n_ratio_w2 %>% select(p_other_ho, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_other_ho, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Realtive % Other Race") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=75, x=p_other_ho, group=Decade, fill=Decade), width=100, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        
        hl<- ggplot(cnty_n_ratio_w2 %>% select(p_hisp_ho, `Data Measurement Year`) %>% mutate(Decade = as.character(`Data Measurement Year`))) + 
          geom_density(aes(x=p_hisp_ho, color=NULL, group = Decade, fill=Decade, alpha=0.9, stat(count)), trim=TRUE, bw=0.1) +
          theme_minimal()  + theme(legend.position="bottom") +   guides(alpha=FALSE) +
          xlab ("Realtive % Hispanic/Latinx") + ylab ("Count") + xlim(0,4) + labs(fill=NULL) + 
          ggstance::geom_boxploth(aes(y=75, x=p_hisp_ho, group=Decade, fill=Decade), width=100, outlier.alpha = 0) +
          scale_fill_brewer(palette="RdBu")
        
        figure1<-ggpubr::ggarrange(w,
                                   b + theme( axis.title.y = element_blank()),
                                   o,
                                   hl + theme( axis.title.y = element_blank()),
                                   ncol=2,nrow=2, common.legend = TRUE, legend = "bottom")
        figure1
        ggsave("community_nation_homeowner_0922_b.jpeg", device="jpeg", width = 6, height = 6, units = "in") 
  
  # #BLOCK GROUP (neighborhood) COMPARISON TO COUNTY (community) - compares each block group value to each county value from IPUMS for block group with and without buyouts 
  # 
  # bo_blks<-dat[dat$cnt > 0 ,c(1,3,19,23:54)] 
  # blckgrp_cnty_ratio<-left_join(dat[dat$cnt > 0 ,c(1,3,19,23:54)],cnty_nobo, by="COUNTYFP10") %>% #join the county summaries for the no buyout block groups to buyout block groups
  #   gather(var, val, -c(COUNTYFP10, COUNTY, GISJOIN, Loc_name)) %>%  #make the census variable names a variable so there is only one column for all census record values
  #   separate(var, c('var', 'case'), sep = "\\.") %>% #create a column that describes whether a record belongs to the no buyouts block group case (x) or buyout block groups case (y)
  #   spread(case, val) %>% #break the census variable values into 2 columns one for all census variables in no buyouts block groups and one for all census variables in buyouts block groups
  #   mutate(ratio = x / y) #take the ratio of each census variable under the 2 cases (average of no buyout bgs in county and buyout bgs)
  # 
  # 
  # #convert back to wide format and quick plot
  # 
  # blckgrp_cnty_ratio_w<-blckgrp_cnty_ratio[,c("GISJOIN","COUNTY","ratio","var")] %>% spread(var,ratio)
  # write.csv(blckgrp_cnty_ratio_w, file = "blkcntyratio90.csv")
