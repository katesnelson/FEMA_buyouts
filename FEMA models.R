#Clean data for regression
#Run linear fixed effects models for:
  # likelihood of buyout in a neighborhood, 
  # density of buyout in a neighborhood, 
  # compensation for buyouts, 



library(sf)
library(tidyverse)
library(reshape2)
library(lme4)
library(lfe)
library(INLA)

wd<-getwd()

#####################
### Read in Data ###
####################

community <- readRDS(paste0(wd,"/data/community_with_bo_summary_0922.rds"))

neighborhood <- readRDS(paste0(wd,"/data/neighborhood_with_bo_summary_0930.rds"))

individual <- readRDS(paste0(wd,"/data/individual_bo_with_neighborhood_0930.rds"))


#############################################
###Clean Neighborhood Data for Regression###
############################################


keep<-c("GISJOIN","GIS Join Match Code" ,"Geography Year" ,"Data Measurement Year" ,"State Name", "State Code","County Name",
        "County Code","Census Tract Code" ,"Block Group Code","p_dependents","p_seniors" ,"p_working" , "p_white" , "p_black","p_other",  "p_hisp",
        "p_rural", "p_urban" , "p_urban_center" ,"p_urban_outside" , "p_home_own" , "p_rent","p_white_ho" , "p_white_rent" ,"p_black_ho",
        "p_black_rent" ,"p_other_ho", "p_other_rent" , "p_hisp_ho","p_hisp_rent","bg_pop_density" , "bg_housing_density", "p_foreign", 
        "p_low_ed", "p_high_ed", "pci", "AvePrice" , "count", "density", "buyout", "StateDecade","CNTY", "CntyDecade")

dat_sf<-neighborhood %>% 
        rename_all(~str_replace_all(.,"\\.","_")) %>%  #the periods in the original dataframe cause headaches later on so replace with underscores
        mutate(density = count/Shape_area * 1e-6) %>% #add a buyout density per block group variable (count per sq km)
        mutate(buyout = ifelse(count >=1, 1,0)) %>% #add a buyout binary indicator for each block group
        mutate(StateDecade = paste0(`State Code`,"_", `Data Measurement Year`)) %>% #create State-decade fixed effect identifiers
        mutate(CNTY = paste0(`State Code`,`County Code`)) %>% #create unique County fixed effect identifiers
        mutate(CntyDecade = paste0(`State Code`,`County Code`,"_", `Data Measurement Year`)) %>% #create County-decade fixed effect identifiers
        select(all_of(c(keep,"geometry"))) 

dat<- dat_sf %>% st_drop_geometry(.) #same dataframe as above but without the geometries
            
            
  p_vars<-c("p_dependents","p_seniors" ,"p_working" , "p_white" , "p_black","p_other",  "p_hisp",
          "p_rural", "p_urban" , "p_urban_center" ,"p_urban_outside" , "p_home_own" , "p_rent",
          "p_white_ho" , "p_white_rent" ,"p_black_ho","p_black_rent" ,"p_other_ho", "p_other_rent" , 
          "p_hisp_ho","p_hisp_rent", "p_foreign", "p_low_ed", "p_high_ed") #percent value variables
  
  t_vars <-c("p_dependents","p_seniors" ,"p_working" , "p_black","p_other",  "p_hisp",
            "p_black_ho", "p_black_rent" ,"p_other_ho", "p_other_rent" , "p_hisp_ho","p_hisp_rent",
            "bg_pop_density" , "bg_housing_density", "p_foreign", 
            "p_low_ed", "p_high_ed", "pci", "AvePrice") #indep vars for transformation
 
  #t_vars<-c()
  
  #Possible transformations
  
  df_plm<-dat %>% 
    filter_at(tidyselect::all_of(p_vars), any_vars(.< 101)) %>% #get rid of outliers from poor data quality and incomplete records (no percent vars greater than 100%)
    mutate_at(vars(all_of(t_vars)), ~log(. + invoke(min,na_if(.,0),na.rm=TRUE))) %>% #log transform to reduce skew, add small value so that measured values of zero don't become -Inf
    mutate_if(is.numeric,~ifelse(abs(.) == Inf,NA,.))#remove produced infinite values
  
  df_plm<-dat %>% 
    filter_at(tidyselect::all_of(p_vars), any_vars(.< 101)) %>% #get rid of outliers from poor data quality and incomplete records (no percent vars greater than 100%)
    mutate_at(vars(all_of(t_vars)), ~log(. + 1)) %>% #log transform to reduce skew, add small value so that measured values of zero don't become -Inf
    mutate_if(is.numeric,~ifelse(abs(.) == Inf,NA,.))#remove produced infinite values
  
  df_plm<-dat %>% 
    filter_at(tidyselect::all_of(p_vars), any_vars(.< 101)) %>% #get rid of outliers from poor data quality and incomplete records (no percent vars greater than 100%)
    mutate_at(vars(all_of(t_vars)), ~log(.)) %>% #log transform to reduce skew
    mutate_if(is.numeric,~ifelse(abs(.) == Inf,NA,.))#remove produced infinite values
  
  df_plm<-dat %>% 
    filter_at(tidyselect::all_of(p_vars), any_vars(.< 101)) %>% #get rid of outliers from poor data quality and incomplete records (no percent vars greater than 100%)
    mutate_at(vars(all_of(t_vars)), ~scale(.)) %>% #standardize data
    mutate_if(is.numeric,~ifelse(abs(.) == Inf,NA,.))#remove produced infinite values
  
      #Check for consistency and accuracy
      nrow(df_plm %>% filter(`Data Measurement Year`==2010))
      nrow(df_plm %>% filter(`Data Measurement Year`==2000))
      nrow(df_plm %>% filter(`Data Measurement Year`==1990))
      
      nrow(df_plm %>% filter(`Data Measurement Year`==2010 & buyout ==1))
      nrow(df_plm %>% filter(`Data Measurement Year`==2000 & buyout ==1))
      nrow(df_plm %>% filter(`Data Measurement Year`==1990 & buyout ==1))
      
  
  saveRDS(df_plm, "plmdata0930.rds") #going with scaled/standardized data

      #Check distributions and correlations
    
      n_vars<-c("p_dependents","p_seniors" ,"p_working" , "p_white" , "p_black","p_other",  "p_hisp",
                "p_rural", "p_urban" , "p_urban_center" ,"p_urban_outside" , "p_home_own" , "p_rent","p_white_ho" , "p_white_rent" ,"p_black_ho",
                "p_black_rent" ,"p_other_ho", "p_other_rent" , "p_hisp_ho","p_hisp_rent","bg_pop_density" , "bg_housing_density", "p_foreign", 
                "p_low_ed", "p_high_ed", "pci", "AvePrice" , "count", "density") #numeric vars
      
      ggplot(data=melt(df_plm %>% select(all_of(n_vars)))) +
        stat_density(aes(x=value))+
        facet_wrap(~variable, scales="free")
    
      C<-cor(df_plm %>% select(all_of(c(n_vars, "Data Measurement Year"))), use="complete.obs")
      corrplot::corrplot(C)


###################################################################
###BUYOUT LIKELIHOOD (Neighborhoods) Linear Fixed Effects Models###
###################################################################

# These are linear probability models (LPM) at a blockgroup (neighborhood) scale 
# Evaluates the likelihood of at least one buyout occurring in a neighborhood (results in percentage points) as a function of the independent variables
# As the indep vars used are standardized interpret beta as a 1 standard deviation increase in X leads to a beta percentage point increase in Y, where Y is the likelihood of receiving a buyout.

# National models --> Only decade fixed effects
    n1<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`), #controls
             data=df_plm, na.action=na.omit) 
      summary(n1, robust=T)
      saveRDS(n1,"n1.rds")
      car::vif(n1) #VIF all below 3
      nat<- cbind (n1$coefficients, as.data.frame(n1$rpval))%>% rownames_to_column()#build a dataframe of results
    
    n2<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_housing_density + factor(`Data Measurement Year`), #controls --> check housing instead of pop density
             data=df_plm, na.action=na.omit) 
      summary(n2, robust=T) #not significantly different from n1
      saveRDS(n2,"n2.rds") 
      
    n3<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #basic demographic breakdown
               p_home_own +  #homeownership
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`), #controls
              data=df_plm, na.action=na.omit)
      summary(n3, robust=T) 
      car::vif(n3) #VIF all below 3
      saveRDS(n3,"n3.rds") 
    
    n4<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #basic demographic breakdown
               p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`), #controls
             data=df_plm, na.action=na.omit)
      summary(n4, robust=T) 
      car::vif(n4) #high VIF values --> drop this model formulation due to collinearity of demographics and homeownership demographics
    
    n5<-felm(buyout ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`), #controls
             data=df_plm, na.action=na.omit)
      summary(n5, robust=T) 
      car::vif(n5) #all below 3
      saveRDS(n5,"n5.rds")
    
    n6<-felm(buyout ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`), #controls
             data=df_plm, na.action=na.omit)
      summary(n6, robust=T) 
      car::vif(n6) 
      saveRDS(n6,"n6.rds")
    
    n7<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + bg_pop_density + factor(`Data Measurement Year`), #remove education control
             data=df_plm, na.action=na.omit) 
      summary(n7, robust=T)
      saveRDS(n7,"n7.rds")
    
    n8<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                p_low_ed + bg_pop_density + factor(`Data Measurement Year`), #remove pci control
             data=df_plm, na.action=na.omit) 
      summary(n8, robust=T)
      saveRDS(n8,"n8.rds")
    
    n9<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + factor(`Data Measurement Year`), #remove population density
             data=df_plm, na.action=na.omit) 
      summary(n9, robust=T)
      saveRDS(n9,"n9.rds")
    
    n10<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_pop_density, #remove temporal control
             data=df_plm, na.action=na.omit) 
      summary(n10, robust=T)
      saveRDS(n10,"n10.rds")
    
#State Fixed Effects Models --> State and Decade fixed effects
    s1<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`) | `State Code` , data=df_plm, na.action=na.omit)
      summary(s1, robust=T) 
      saveRDS(s1,"s1.rds")
      sfe<- cbind (s1$coefficients, as.data.frame(s1$rpval))%>% rownames_to_column()
      
    s2<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_housing_density + factor(`Data Measurement Year`) | `State Code` , data=df_plm, na.action=na.omit)
      summary(s2, robust=T) 
      saveRDS(s2,"s2.rds")
      
    s3<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #basic demographic breakdown
               p_home_own +  #homeownership
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)  | `State Code` , data=df_plm, na.action=na.omit)
      summary(s3, robust=T)
      saveRDS(s3,"s3.rds")
    
    s5<-felm(buyout ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)  | `State Code` , data=df_plm, na.action=na.omit)
      summary(s5, robust=T) 
      saveRDS(s5,"s5.rds")
    
    s6<-felm(buyout ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`) | `State Code` , data=df_plm, na.action=na.omit)
      summary(s6, robust=T) 
      saveRDS(s6,"s6.rds")
    
    s10<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                pci + p_low_ed + bg_pop_density | `State Code` , data=df_plm, na.action=na.omit) #remove temporal control
      summary(s10, robust=T)
      saveRDS(s10,"s10.rds")

#State-Decade fixed effects
    sd1<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                pci + p_low_ed + bg_pop_density | StateDecade, data=df_plm, na.action=na.omit)
      summary(sd1, robust=T)
      saveRDS(sd1,"sd1.rds")
      sdfe<- cbind (sd1$coefficients, as.data.frame(sd1$rpval))%>% rownames_to_column()
    
    sd5<-felm(buyout ~ p_dependents + p_seniors +  #basic demographic breakdown
                p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
                pci + p_low_ed + bg_pop_density | StateDecade, data=df_plm, na.action=na.omit)
      summary(sd5, robust=T)
      saveRDS(sd5,"sd5.rds")
    
    sd6<-felm(buyout ~ p_dependents + p_seniors +  #basic demographic breakdown
                p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
                pci + p_low_ed + bg_pop_density | StateDecade, data=df_plm, na.action=na.omit)
      summary(sd6, robust=T)
      saveRDS(sd6,"sd6.rds")
    
    
#County Fixed Effects Models
    c1<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)| CNTY , data=df_plm, na.action=na.omit)
      summary(c1, robust=T) 
      saveRDS(c1,"c1.rds")
      cfe<-cbind (c1$coefficients, as.data.frame(c1$rpval))%>% rownames_to_column()
    
    c2<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_housing_density + factor(`Data Measurement Year`) | CNTY , data=df_plm, na.action=na.omit)
      summary(c2, robust=T) 
      saveRDS(c2,"c2.rds")
    
    c3<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #basic demographic breakdown
               p_home_own +  #homeownership
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)  | CNTY , data=df_plm, na.action=na.omit)
      summary(c3, robust=T)
      saveRDS(c3,"c3.rds")
    
    c5<-felm(buyout ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)  | CNTY , data=df_plm, na.action=na.omit)
      summary(c5, robust=T) 
      saveRDS(c5,"c5.rds")
    
    c6<-felm(buyout ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`) | CNTY , data=df_plm, na.action=na.omit)
      summary(c6, robust=T) 
      saveRDS(c6,"c6.rds")
    
    c10<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                pci + p_low_ed + bg_pop_density | CNTY , data=df_plm, na.action=na.omit) #remove temporal control
      summary(c10, robust=T)
      saveRDS(c10,"c10.rds")
    
    #County-decade fixed effects
      cd1<-felm(buyout ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                  pci + p_low_ed + bg_pop_density | CntyDecade, data=df_plm, na.action=na.omit)
      summary(cd1, robust=T)
      saveRDS(cd1,"cd1.rds")
      cdfe<- cbind (cd1$coefficients, as.data.frame(cd1$rpval))%>% rownames_to_column()
      
      cd5<-felm(buyout ~ p_dependents + p_seniors +  #basic demographic breakdown
                  p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
                  pci + p_low_ed + bg_pop_density | CntyDecade, data=df_plm, na.action=na.omit)
      summary(cd5, robust=T)
      saveRDS(cd5,"cd5.rds")
      
      cd6<-felm(buyout ~ p_dependents + p_seniors +  #basic demographic breakdown
                  p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
                  pci + p_low_ed + bg_pop_density | CntyDecade, data=df_plm, na.action=na.omit)
      summary(cd6, robust=T)
      saveRDS(cd6,"cd6.rds")
    
    
    #Build a table with the primary results to report
    lpm_table<-left_join(nat, sfe, by="rowname") %>% left_join(.,sdfe, by="rowname") %>% left_join(.,cfe,by="rowname") %>% left_join(.,cdfe, by="rowname")
    colnames(lpm_table)<-c("Coeff", "National", "Nat p", "State FE","St p", "State-Decade FE", "St-D p", "County FE", "Cnty p", "County-Decade FE", "Cnty-D p")
    lpm_table<-lpm_table %>% mutate_if(is.numeric,~formatC(.,format="e"))
    tbl<-flextable::regulartable(lpm_table)
    colkeys<-colnames(lpm_table)[-1]
    tbl<-flextable::colformat_num(tbl, col_keys = colkeys,
                                  big.mark = ",", digits = 5, na_str = "NA")
    print(tbl, preview="docx")
    

#############################################
###Clean Individual Data for Regression###
############################################
  
  
  keep2<-c("GISJOIN","GIS Join Match Code" ,"Geography Year" ,"Data Measurement Year" ,"State Name", "State Code","County Name",
          "County Code","Census Tract Code" ,"Block Group Code","p_dependents","p_seniors" ,"p_working" , "p_white" , "p_black","p_other",  "p_hisp",
          "p_rural", "p_urban" , "p_urban_center" ,"p_urban_outside" , "p_home_own" , "p_rent","p_white_ho" , "p_white_rent" ,"p_black_ho",
          "p_black_rent" ,"p_other_ho", "p_other_rent" , "p_hisp_ho","p_hisp_rent","bg_pop_density" , "bg_housing_density", "p_foreign", 
          "p_low_ed", "p_high_ed", "pci", "StateDecade","CNTY", "CntyDecade","USER_Fisca","USER_Disas","USER_Resid","USER_Struc","USER_Price", "decade")
  
  dat2_sf<-individual %>% 
    rename_at(vars(p.dependents:p.hisp_rent,p.foreign:p.high_ed),~str_replace_all(.,"\\.","_")) %>%  #the periods in the original dataframe cause headaches later on so replace with underscores
    mutate(StateDecade = paste0(`State Code`,"_", `Data Measurement Year`)) %>% #create State-decade fixed effect identifiers
    mutate(CNTY = paste0(`State Code`,`County Code`)) %>% #create unique County fixed effect identifiers
    mutate(CntyDecade = paste0(`State Code`,`County Code`,"_", `Data Measurement Year`)) %>% #create County-decade fixed effect identifiers
    mutate_at(., vars(USER_Price),~as.numeric(as.character(.))) %>%
    select(all_of(c(keep2,"geometry"))) 
  
  #inflation adjust to 2019 dollars
  library(blscrapeR)
  df<-inflation_adjust(2019)
  head(df)
  df$year<-as.numeric(df$year)
  dat2_sf<-left_join(dat2_sf,df[,c(1,3)], by=c("USER_Fisca"="year"))
  dat2_sf$Value_adj<-dat2_sf$USER_Price/dat2_sf$adj_value

  dat2<- dat2_sf %>% st_drop_geometry(.) #same dataframe as above but without the geometries
  
  
  p_vars<-c("p_dependents","p_seniors" ,"p_working" , "p_white" , "p_black","p_other",  "p_hisp",
            "p_rural", "p_urban" , "p_urban_center" ,"p_urban_outside" , "p_home_own" , "p_rent",
            "p_white_ho" , "p_white_rent" ,"p_black_ho","p_black_rent" ,"p_other_ho", "p_other_rent" , 
            "p_hisp_ho","p_hisp_rent", "p_foreign", "p_low_ed", "p_high_ed") #percent value variables
  
  t_vars <-c("p_dependents","p_seniors" ,"p_working" , "p_black","p_other",  "p_hisp",
             "p_black_ho", "p_black_rent" ,"p_other_ho", "p_other_rent" , "p_hisp_ho","p_hisp_rent",
             "bg_pop_density" , "bg_housing_density", "p_foreign", 
             "p_low_ed", "p_high_ed", "pci") #indep vars for transformation
  
  #t_vars<-c()
  
  #Transformation

  df_comp<-dat2 %>% 
    filter_at(tidyselect::all_of(p_vars), any_vars(.< 101)) %>% #get rid of outliers from poor data quality and incomplete records (no percent vars greater than 100%)
    mutate_at(vars(all_of(t_vars)), ~scale(.)) %>% #standardize data
    mutate_if(is.numeric,~ifelse(abs(.) == Inf,NA,.))  #remove produced infinite values
    
    
  #Check for consistency and accuracy
  nrow(df_comp %>% filter(`Data Measurement Year`==2010))
  nrow(df_comp %>% filter(`Data Measurement Year`==2000))
  nrow(df_comp %>% filter(`Data Measurement Year`==1990))
 
  saveRDS(df_comp, "compdata0930.rds") #going with scaled/standardized data
  
  #Check distributions and correlations
  
  n_vars<-c("p_dependents","p_seniors" ,"p_working" , "p_white" , "p_black","p_other",  "p_hisp",
            "p_rural", "p_urban" , "p_urban_center" ,"p_urban_outside" , "p_home_own" , "p_rent","p_white_ho" , "p_white_rent" ,"p_black_ho",
            "p_black_rent" ,"p_other_ho", "p_other_rent" , "p_hisp_ho","p_hisp_rent","bg_pop_density" , "bg_housing_density", "p_foreign", 
            "p_low_ed", "p_high_ed", "pci", "USER_Fisca","USER_Price", "Value_adj") #numeric vars
  
  ggplot(data=melt(df_comp %>% select(all_of(n_vars)))) +
    stat_density(aes(x=value))+
    facet_wrap(~variable, scales="free")
  
  C<-cor(df_comp %>% select(all_of(c(n_vars, "Data Measurement Year"))), use="complete.obs")
  corrplot::corrplot(C)
  
######################################################
###BUYOUT Compensation Linear Fixed Effects Models###
#####################################################

# These are linear models evaluating the effect of blockgroup (neighborhood) characteristics on buyout (individual scale) compensation
# Indep vars are standardizes so interpret beta as a 1 standard deviation increase in X leads to a $beta increase in compensation

#National Model --> Time and structure type fixed effects
    nb1<-felm(Value_adj ~  p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)| USER_Struc, data=df_comp) #controls
      summary(nb1, robust=T)
      saveRDS(nb1,"nb1.rds")
      getfe(nb1)
      nat<- cbind (nb1$coefficients, as.data.frame(nb1$rpval))%>% rownames_to_column()
    
    nb2<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_housing_density + factor(`Data Measurement Year`)| USER_Struc, data=df_comp) #controls --> check housing instead of pop density
      summary(nb2, robust=T) #not significantly different from n1
      saveRDS(nb2,"nb2.rds") 
    
    nb3<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #basic demographic breakdown
               p_home_own +  #homeownership
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)| USER_Struc, data=df_comp) #controls
      summary(nb3, robust=T) 
      saveRDS(nb3,"nb3.rds") 
    
    nb5<-felm(Value_adj ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)| USER_Struc, data=df_comp) #controls
      summary(nb5, robust=T) 
      saveRDS(nb5,"nb5.rds")
    
    nb6<-felm(Value_adj ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)| USER_Struc, data=df_comp) #controls
      summary(nb6, robust=T) 
      saveRDS(nb6,"nb6.rds")
    
    nb7<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + bg_pop_density + factor(`Data Measurement Year`)| USER_Struc, data=df_comp) #controls #remove education control
      summary(nb7, robust=T)
      saveRDS(nb7,"nb7.rds")
    
    nb8<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               p_low_ed + bg_pop_density + factor(`Data Measurement Year`)| USER_Struc, data=df_comp) #remove pci control
      summary(nb8, robust=T)
      saveRDS(nb8,"nb8.rds")
    
    nb9<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + factor(`Data Measurement Year`)| USER_Struc, data=df_comp) #remove population density
      summary(nb9, robust=T)
      saveRDS(nb9,"nb9.rds")
      nat_nopop<- cbind (nb9$coefficients, as.data.frame(nb9$rpval)) %>% rownames_to_column()
    
    nb10<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                pci + p_low_ed + bg_pop_density| USER_Struc, data=df_comp) #remove temporal control
      summary(nb10, robust=T)
      saveRDS(nb10,"nb10.rds")
    
    nb11<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                 pci + p_low_ed + bg_pop_density  + factor(USER_Fisca)| USER_Struc, data=df_comp) #year control instead of decade time control
      summary(nb11, robust=T)
      saveRDS(nb11,"nb11.rds")
      nat_yr<- cbind (nb11$coefficients, as.data.frame(nb11$rpval)) %>% rownames_to_column()
    
      
    nb12<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                 pci + p_low_ed + bg_pop_density  + factor(`Data Measurement Year`), data=df_comp) #no structure type control
      summary(nb12, robust=T)
      saveRDS(nb12,"nb12.rds")
      nat_nohousing<- cbind (nb12$coefficients, as.data.frame(nb12$rpval)) %>% rownames_to_column()
    
    nb13<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                 pci + p_low_ed + bg_pop_density  + factor(`Data Measurement Year`)+ USER_Resid | USER_Struc, data=df_comp) #add residence type control
      summary(nb13, robust=T)
      saveRDS(nb13,"nb13.rds")
      nat_res<- cbind (nb13$coefficients, as.data.frame(nb13$rpval)) %>% rownames_to_column()
    
    nb14<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                 pci + p_low_ed + bg_pop_density  + factor(`Data Measurement Year`)+ USER_Disas | USER_Struc, data=df_comp) #add disaster type control
      summary(nb14, robust=T)
      saveRDS(nb14,"nb14.rds")
      nat_dis<- cbind (nb14$coefficients, as.data.frame(nb14$rpval)) %>% rownames_to_column()
    
    
#Models with State FE
    
    nb_s1<-felm(Value_adj ~  p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                  pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`) | USER_Struc + `State Code`, data=df_comp)
      summary(nb_s1, robust=T)
      saveRDS(nb_s1,"nb_s1.rds")
      sfe<- cbind (nb_s1$coefficients, as.data.frame(nb_s1$rpval)) %>% rownames_to_column()
    
    nb_s2<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                  pci + p_low_ed + bg_housing_density + factor(`Data Measurement Year`)| USER_Struc + `State Code`, data=df_comp) #housing density control
      summary(nb_s2, robust=T)
      saveRDS(nb_s2,"nb_s2.rds")
    
    nb_s5<-felm(Value_adj ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)  | USER_Struc + `State Code`, data=df_comp)
      summary(nb_s5, robust=T) 
      saveRDS(nb_s5,"nb_s5.rds")
    
    nb_s6<-felm(Value_adj ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`) | USER_Struc + `State Code`, data=df_comp)
      summary(nb_s6, robust=T) 
      saveRDS(nb_s6,"nb_s6.rds")
    
    nb_s10<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                pci + p_low_ed + bg_pop_density | USER_Struc + `State Code`, data=df_comp) #remove temporal control
      summary(nb_s10, robust=T)
      saveRDS(nb_s10,"nb_s10.rds")
    
    
            # lin<-felm(Value_adj ~  Dependents + Over_65  + Race_Other + Race_Black + Hisp_Lat_90  + PCI_90_2 + P18_LSHE90  + HO_W_90 + HO_B_90 + HO_O_90 + HO_HL_90 + total.pop + USER_Fisca + USER_Struc| STATEFP10.x, data=d_full_bo_new)
            # summary(lin, robust=T) #trends don't change when use decade factor control instead of annual linear control so use of decades supported
            # sfe_yr<- cbind (lin$coefficients, as.data.frame(lin$rpval)) %>% rownames_to_column()
            # 
            # lin<-felm(Value_adj ~  Dependents + Over_65  + Race_Other + Race_Black + Hisp_Lat_90  + PCI_90_2 + P18_LSHE90  + HO_W_90 + HO_B_90 + HO_O_90 + HO_HL_90  + factor(decade) + USER_Struc| STATEFP10.x, data=d_full_bo_new)
            # summary(lin, robust=T) #removing popula<- cbind (lin$coefficients, as.data.frame(lin$rpval)) %>% rownames_to_column()tion density does not substantively change estimates, so trends idependent of population density
            # sfe_nopop<- cbind (lin$coefficients, as.data.frame(lin$rpval)) %>% rownames_to_column()
            # 
            # lin<-felm(Value_adj ~  Dependents + Over_65  + Race_Other + Race_Black + Hisp_Lat_90  + PCI_90_2 + P18_LSHE90  + HO_W_90 + HO_B_90 + HO_O_90 + HO_HL_90 + total.pop + factor(decade)| STATEFP10.x, data=d_full_bo_new)
            # summary(lin, robust=T) #if we don't account for housing type then differences by Home Owner Race don't matter
            # sfe_nohousing<- cbind (lin$coefficients, as.data.frame(lin$rpval)) %>% rownames_to_column()
            # 
            # lin<-felm(Value_adj ~  Dependents + Over_65  + Race_Other + Race_Black + Hisp_Lat_90  + PCI_90_2 + P18_LSHE90  + HO_W_90 + HO_B_90 + HO_O_90 + HO_HL_90 + total.pop + factor(decade) + USER_Resid + USER_Struc| STATEFP10.x, data=d_full_bo_new)
            # summary(lin, robust=T) #use type of residence (rental vs owner occupied) makes not significant due to large number of missing entries, but does not change trends
            # sfe_res<- cbind (lin$coefficients, as.data.frame(lin$rpval)) %>% rownames_to_column()
            # 
            # lin<-felm(Value_adj ~  Dependents + Over_65  + Race_Other + Race_Black + Hisp_Lat_90   + P18_LSHE90  + HO_W_90 + HO_B_90 + HO_O_90 + HO_HL_90 + PCI_90_2 + total.pop + factor(decade) + USER_Disas + USER_Struc | STATEFP10.x, data=d_full_bo_new)
            # summary(lin, robust=T) #controlling for the disaster also reduces significnace due to large number of factors, but does not change trends
            # sfe_dis<- cbind (lin$coefficients, as.data.frame(lin$rpval)) %>% rownames_to_column()
            # 
            # lin<-felm(Value_adj ~   Over_65  + Race_Other + Race_Black + Hisp_Lat_90   + P18_LSHE90  + HO_W_90 + HO_B_90 + HO_O_90 + HO_HL_90 + PCI_90_2 + total.pop + factor(decade) + USER_Struc| STATEFP10.x, data=d_full_bo_new)
            # summary(lin, robust=T) #removing dependents makes OVer_65 significant, but removes significance by HO race suggesting correlation between the two not acocunted for by the State FE
            # 
            # lin<-felm(Value_adj ~   Dependents + Over_65  + Race_Other + Race_Black + Hisp_Lat_90   + HO_W_90 + HO_B_90 + HO_O_90 + HO_HL_90 + PCI_90 + total.pop + factor(decade) + USER_Struc| STATEFP10.x, data=d_full_bo_new)
            # summary(lin, robust=T) #removing low education status control makes Home Owner by race more significant, suggesting that part of the difference in $ by race is associated with educational disparities by race (also makes popualtion density significnat suggesting that slight difference in $ by population density also associated with education)
            # sfe_noed<- cbind (lin$coefficients, as.data.frame(lin$rpval))%>% rownames_to_column()
    
    
  #State-Decade fixed effects
    
    nb_sd1<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                pci + p_low_ed + bg_pop_density | USER_Struc + StateDecade, data=df_comp)
      summary(nb_sd1, robust=T)
      saveRDS(nb_sd1,"nb_sd1.rds")
      sdfe<- cbind (nb_sd1$coefficients, as.data.frame(nb_sd1$rpval))%>% rownames_to_column()
    
    nb_sd5<-felm(Value_adj ~ p_dependents + p_seniors +  #basic demographic breakdown
                p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
                pci + p_low_ed + bg_pop_density | USER_Struc + StateDecade, data=df_comp)
      summary(nb_sd5, robust=T)
      saveRDS(nb_sd5,"nb_sd5.rds")
    
    nb_sd6<-felm(Value_adj ~ p_dependents + p_seniors +  #basic demographic breakdown
                p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
                pci + p_low_ed + bg_pop_density | USER_Struc + StateDecade, data=df_comp)
      summary(nb_sd6, robust=T)
      saveRDS(nb_sd6,"nb_sd6.rds")
    

#County FE Models
    
    nb_c1<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)| USER_Struc + CNTY, data=df_comp)
      summary(nb_c1, robust=T) 
      saveRDS(nb_c1,"nb_c1.rds")
      cfe<-cbind (nb_c1$coefficients, as.data.frame(nb_c1$rpval))%>% rownames_to_column()
    
    nb_c2<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_housing_density + factor(`Data Measurement Year`) | USER_Struc + CNTY, data=df_comp)
      summary(nb_c2, robust=T) 
      saveRDS(nb_c2,"nb_c2.rds")
    
    nb_c3<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #basic demographic breakdown
               p_home_own +  #homeownership
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)  | USER_Struc + CNTY, data=df_comp)
      summary(nb_c3, robust=T)
      saveRDS(nb_c3,"nb_c3.rds")
    
    nb_c5<-felm(Value_adj ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)  | USER_Struc + CNTY, data=df_comp)
      summary(nb_c5, robust=T) 
      saveRDS(nb_c5,"nb_c5.rds")
    
    nb_c6<-felm(Value_adj ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`) | USER_Struc + CNTY, data=df_comp)
      summary(nb_c6, robust=T) 
      saveRDS(nb_c6,"nb_c6.rds")
    
    nb_c10<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                pci + p_low_ed + bg_pop_density | USER_Struc + CNTY, data=df_comp) #remove temporal control
      summary(nb_c10, robust=T)
      saveRDS(nb_c10,"nb_c10.rds")
    
    
        # lin<-felm(Value_adj ~  Dependents + Over_65  + Race_Other + Race_Black + Hisp_Lat_90   + P18_LSHE90  + PCI_90 +
        #             total.pop + USER_Struc| USER_Fisca  + CNTY, data=d_full_bo_new2)
        # summary(lin, robust=T)#trends don't change when use decade factor control instead of annual linear control so use of decades supported
        # 
        # lin<-felm(Value_adj ~  Dependents + Over_65  + Race_Other + Race_Black + Hisp_Lat_90 +  PCI_90 + P18_LSHE90    + 
        #             USER_Struc| CNTY + factor(decade), data=d_full_bo_new2)
        # summary(lin, robust=T) #removing population density does not substantively change estimates, so trends idependent of population density
        # 
        # lin<-felm(Value_adj ~  Dependents + Over_65  + Race_Other + Race_Black + Hisp_Lat_90  + PCI_90 + 
        #             P18_LSHE90    | CNTY + factor(decade), data=d_full_bo_new2)
        # summary(lin, robust=T) #removing housing type control does not substantively change estimates, so trends within counties independent of housing type
        # 
        # lin<-felm(Value_adj ~  Dependents + Over_65  + Race_Other + Race_Black + Hisp_Lat_90 + PCI_90 + P18_LSHE90  + 
        #             total.pop  + USER_Resid + USER_Struc| CNTY+ factor(decade), data=d_full_bo_new2)
        # summary(lin, robust=T) #use type of residence (rental vs owner occupied) reduces significnace due to large number of missing entries, but does not change trends
        # 
        # lin<-felm(Value_adj ~  Dependents + Over_65  + Race_Other + Race_Black + Hisp_Lat_90   + P18_LSHE90  + PCI_90 +
        #             total.pop  + USER_Disas + USER_Struc | CNTY + factor(decade), data=d_full_bo_new2)
        # summary(lin, robust=T) #controlling for the disaster also reduces significnace due to large number of factors, but does not change trends
        # 
        # lin<-felm(Value_adj ~  Dependents + Over_65  +  HO_W_90 + HO_B_90 + HO_O_90 + HO_HL_90   + PCI_90   + 
        #             total.pop + USER_Struc| factor(decade)  + CNTY, data=d_full_bo_new2)
        # summary(lin, robust=T) 
        # cfe_noed<- cbind (lin$coefficients, as.data.frame(lin$rpval))%>% rownames_to_column()
    
 #County-decade fixed effects
    
    nb_cd1<-felm(Value_adj ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                pci + p_low_ed + bg_pop_density | USER_Struc + CntyDecade, data=df_comp) 
    summary(nb_cd1, robust=T)
    saveRDS(nb_cd1,"nb_cd1.rds")
    cdfe<- cbind (nb_cd1$coefficients, as.data.frame(nb_cd1$rpval))%>% rownames_to_column()
    
    nb_cd5<-felm(Value_adj ~ p_dependents + p_seniors +  #basic demographic breakdown
                p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
                pci + p_low_ed + bg_pop_density | USER_Struc + CntyDecade, data=df_comp) 
    summary(nb_cd5, robust=T)
    saveRDS(nb_cd5,"nb_cd5.rds")
    
    nb_cd6<-felm(Value_adj ~ p_dependents + p_seniors +  #basic demographic breakdown
                p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
                pci + p_low_ed + bg_pop_density | USER_Struc + CntyDecade, data=df_comp) 
    summary(nb_cd6, robust=T)
    saveRDS(nb_cd6,"nb_cd6.rds")
    
    
      # lin<-felm(Value_adj ~  Dependents + Over_65  + Race_Other + Race_Black + Hisp_Lat_90   + PCI_90   + 
      #             total.pop  |USER_Struc + CNTYDecade, data=d_full_bo_new2)
      # summary(lin, robust=T)
      # cdfe_noed<- cbind (lin$coefficients, as.data.frame(lin$rpval))%>% rownames_to_column()
    
    
    
#Build a table with the primary results to report
    val_table<-left_join(nat, sfe, by="rowname") %>% left_join(.,sdfe, by="rowname") %>% 
      left_join(.,cfe, by="rowname") %>% left_join(.,cdfe, by="rowname") 
    colnames(val_table)<-c("Coeff", "National", "Nat p", "State FE","St p", "State-Decade FE", "St-D p", "County FE", "Cnty p", "County-Decade FE", "Cnty-D p")
    tbl<-flextable::regulartable(val_table)
    
    tbl <- flextable::colformat_num(
      x = tbl, col_keys = col_keys,
      big.mark=",", digits = 0, na_str = "N/A")
    col_keys=c("Nat p", "St p", "St-D p", "Cnty p", "Cnty-D p")
    tbl <- flextable::colformat_num(
      x = tbl, col_keys = col_keys,
      big.mark=",", digits = 3, na_str = "N/A")
    print(tbl, preview="docx")
    
#Build a table with supplementary results to report
    val_table<-full_join(nat, nat_yr, by="rowname") %>% full_join(.,nat_res, by="rowname") %>% full_join(.,nat_dis, by="rowname") %>%
      full_join(.,nat_nopop, by="rowname") %>% full_join(.,nat_nohousing, by="rowname") 
    colnames(val_table)<-c("Coeff", "National", "p", "National with Purchase Year","p2", "National with Residence", "p3","National with Disaster", "p4", "National without Population Density", "p5", "National without Structure Type", "p6")
    tbl<-flextable::regulartable(val_table)
    col_keys=c("National", "National with Purchase Year", "National with Residence", "National with Disaster", "National without Population Density", "National without Structure Type")
    tbl <- flextable::colformat_num(
      x = tbl, col_keys = col_keys,
      big.mark=",", digits = 0, na_str = "N/A")
    col_keys=c("p", "p2", "p3", "p4", "p5", "p6")
    tbl <- flextable::colformat_num(
      x = tbl, col_keys = col_keys,
      big.mark=",", digits = 3, na_str = "N/A")
    print(tbl, preview="docx")
    
    
   
#####################
###BUYOUT DENSITY###
####################
    #can we say anything about where buyouts are more likely to be accepted? 
    #Have more buyouts where you have more resources and more at risk homes so need to control for that. --> STATE FE and CNTY FE should help
    #Dataset should only inlcude places that have had at least 1 buyout.
    #Have controlled for population density so effects should account for fewer people and homes in the area.
    
# National models --> Only decade fixed effects
    d_n1<-felm(density ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`), #controls
             data=df_plm, na.action=na.omit) 
      summary(d_n1, robust=T)
      saveRDS(d_n1,"d_n1.rds")
      car::vif(d_n1) #VIF all below 3
      nat<- cbind (d_n1$coefficients, as.data.frame(d_n1$rpval))%>% rownames_to_column()#build a dataframe of results
    
    d_n2<-felm(density ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_housing_density + factor(`Data Measurement Year`), #controls --> check housing instead of pop density
             data=df_plm, na.action=na.omit) 
      summary(d_n2, robust=T) #not significantly different from n1
      saveRDS(d_n2,"d_n2.rds") 
    
    d_n3<-felm(density ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #basic demographic breakdown
               p_home_own +  #homeownership
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`), #controls
             data=df_plm, na.action=na.omit)
    summary(d_n3, robust=T) 
    car::vif(d_n3) #VIF all below 3
    saveRDS(d_n3,"d_n3.rds") 
    
    d_n5<-felm(density ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`), #controls
             data=df_plm, na.action=na.omit)
    summary(d_n5, robust=T) 
    car::vif(d_n5) #all below 3
    saveRDS(d_n5,"d_n5.rds")
    
    d_n6<-felm(density ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`), #controls
             data=df_plm, na.action=na.omit)
    summary(d_n6, robust=T) 
    car::vif(d_n6) 
    saveRDS(d_n6,"d_n6.rds")
    
#State Fixed Effects Models --> State and Decade fixed effects
    d_s1<-felm(density ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`) | `State Code` , data=df_plm, na.action=na.omit)
    summary(d_s1, robust=T) 
    saveRDS(d_s1,"d_s1.rds")
    sfe<- cbind (d_s1$coefficients, as.data.frame(d_s1$rpval))%>% rownames_to_column()
    
    d_s3<-felm(density ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #basic demographic breakdown
               p_home_own +  #homeownership
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)  | `State Code` , data=df_plm, na.action=na.omit)
    summary(d_s3, robust=T)
    saveRDS(d_s3,"d_s3.rds")
    
    d_s5<-felm(density ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)  | `State Code` , data=df_plm, na.action=na.omit)
    summary(d_s5, robust=T) 
    saveRDS(d_s5,"d_s5.rds")
    
    d_s6<-felm(density ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`) | `State Code` , data=df_plm, na.action=na.omit)
    summary(d_s6, robust=T) 
    saveRDS(d_s6,"d_s6.rds")
    
    
  #State-Decade fixed effects
    d_sd1<-felm(density ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                pci + p_low_ed + bg_pop_density | StateDecade, data=df_plm, na.action=na.omit)
    summary(d_sd1, robust=T)
    saveRDS(d_sd1,"sd1.rds")
    sdfe<- cbind (d_sd1$coefficients, as.data.frame(d_sd1$rpval))%>% rownames_to_column()
    
    d_sd5<-felm(density ~ p_dependents + p_seniors +  #basic demographic breakdown
                p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
                pci + p_low_ed + bg_pop_density | StateDecade, data=df_plm, na.action=na.omit)
    summary(d_sd5, robust=T)
    saveRDS(d_sd5,"d_sd5.rds")
    
    d_sd6<-felm(density ~ p_dependents + p_seniors +  #basic demographic breakdown
                p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
                pci + p_low_ed + bg_pop_density | StateDecade, data=df_plm, na.action=na.omit)
    summary(d_sd6, robust=T)
    saveRDS(d_sd6,"d_sd6.rds")
    
    
#County Fixed Effects Models
    d_c1<-felm(density ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)| CNTY , data=df_plm, na.action=na.omit)
    summary(d_c1, robust=T) 
    saveRDS(d_c1,"d_c1.rds")
    cfe<-cbind (d_c1$coefficients, as.data.frame(d_c1$rpval))%>% rownames_to_column()
    
    d_c3<-felm(density ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #basic demographic breakdown
               p_home_own +  #homeownership
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)  | CNTY , data=df_plm, na.action=na.omit)
    summary(d_c3, robust=T)
    saveRDS(d_c3,"d_c3.rds")
    
    d_c5<-felm(density ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)  | CNTY , data=df_plm, na.action=na.omit)
    summary(d_c5, robust=T) 
    saveRDS(d_c5,"d_c5.rds")
    
    d_c6<-felm(density ~ p_dependents + p_seniors +  #basic demographic breakdown
               p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`) | CNTY , data=df_plm, na.action=na.omit)
    summary(d_c6, robust=T) 
    saveRDS(d_c6,"d_c6.rds")
    
  #County-decade fixed effects
    d_cd1<-felm(density ~ p_dependents + p_seniors + p_other + p_black +  p_hisp + #only basic demographic breakdown
                pci + p_low_ed + bg_pop_density | CntyDecade, data=df_plm, na.action=na.omit)
    summary(d_cd1, robust=T)
    saveRDS(d_cd1,"d_cd1.rds")
    cdfe<- cbind (d_cd1$coefficients, as.data.frame(d_cd1$rpval))%>% rownames_to_column()
    
    d_cd5<-felm(density ~ p_dependents + p_seniors +  #basic demographic breakdown
                p_black_ho + p_other_ho + p_hisp_ho +  #homeownership demographics
                pci + p_low_ed + bg_pop_density | CntyDecade, data=df_plm, na.action=na.omit)
    summary(d_cd5, robust=T)
    saveRDS(d_cd5,"d_cd5.rds")
    
    d_cd6<-felm(density ~ p_dependents + p_seniors +  #basic demographic breakdown
                p_black_rent + p_other_rent + p_hisp_rent +  #rentership demographics
                pci + p_low_ed + bg_pop_density | CntyDecade, data=df_plm, na.action=na.omit)
    summary(d_cd6, robust=T)
    saveRDS(d_cd6,"d_cd6.rds")
    
    
    #Build a table with the primary results to report
    lpm_table<-left_join(nat, sfe, by="rowname") %>% left_join(.,sdfe, by="rowname") %>% left_join(.,cfe,by="rowname") %>% left_join(.,cdfe, by="rowname")
    colnames(lpm_table)<-c("Coeff", "National", "Nat p", "State FE","St p", "State-Decade FE", "St-D p", "County FE", "Cnty p", "County-Decade FE", "Cnty-D p")
    lpm_table<-lpm_table %>% mutate_if(is.numeric,~formatC(.,format="e"))
    tbl<-flextable::regulartable(lpm_table)
    colkeys<-colnames(lpm_table)[-1]
    tbl<-flextable::colformat_num(tbl, col_keys = colkeys,
                                  big.mark = ",", digits = 5, format= "e",na_str = "NA")
    print(tbl, preview="docx")
    
 
######################################################
### Population, Homeowner, Renter Comparison Plots ### 
######################################################
    

#For likelihood models
    
 d1<- cbind (n1$coefficients, as.data.frame(n1$rse)) %>% 
      rownames_to_column() %>% mutate(model = "National Population") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(4:6,8) %>% arrange(., Variable) #build a dataframe of results

  d2<- cbind (n5$coefficients, as.data.frame(n5$rse)) %>% 
      rownames_to_column() %>% mutate(model = "National Homeowners") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
    slice(4:6,8) %>% arrange(., Variable) #build a dataframe of results
  
  d3<- cbind (n6$coefficients, as.data.frame(n6$rse)) %>% 
      rownames_to_column() %>% mutate(model = "National Renters") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
    slice(4:6,8) %>% arrange(., Variable)#build a dataframe of results
  
  d4<- cbind (c1$coefficients, as.data.frame(c1$rse)) %>% 
    rownames_to_column() %>% mutate(model = "County Population") %>%
    setNames(., c("Variable","Effect","Standard Error","Model")) %>%
    slice(3:5,7) %>% arrange(., Variable) #build a dataframe of results
  
  d5<- cbind (c5$coefficients, as.data.frame(c5$rse)) %>% 
    rownames_to_column() %>% mutate(model = "County Homeowners") %>%
    setNames(., c("Variable","Effect","Standard Error","Model")) %>%
    slice(3:5,7) %>% arrange(., Variable) #build a dataframe of results
  
  d6<- cbind (c6$coefficients, as.data.frame(c6$rse)) %>% 
    rownames_to_column() %>% mutate(model = "County Renters") %>%
    setNames(., c("Variable","Effect","Standard Error","Model")) %>%
    slice(3:5,7) %>% arrange(., Variable)#build a dataframe of results
  
  

    home_rent <- bind_rows(d2,d3,d5,d6) %>% 
      mutate_at(.,vars(Variable),~str_replace(.,"_ho","")) %>% 
      mutate_at(.,vars(Variable),~str_replace(.,"_rent","")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"p_","")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"black","Black")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"hisp","Hispanic/Latinx")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"other","Other Race")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"low_ed","Low Education"))
    
    ggplot(home_rent) +
      geom_errorbarh(height = 0, aes(xmin = Effect - `Standard Error`, 
                                     xmax = Effect + `Standard Error`, 
                                     y = reorder(Variable, desc(Effect)),
                                     color= Model, group=Variable)) +
      geom_point(aes(x = Effect, y = reorder(Variable, desc(Effect)), color=Model), size = 2) +
      geom_vline(xintercept = 0) +
      theme_minimal()+
      labs(x = "Effect on Likelihood",
           y = "")+
      scale_color_brewer(palette="RdBu")
    ggsave("home_rent_likelihood.jpeg", device="jpeg", width = 6, height = 4, units = "in")
  
  #For compensation models
    
    d1<- cbind (nb1$coefficients, as.data.frame(nb1$rse)) %>% 
      rownames_to_column() %>% mutate(model = "National Population") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(3:5,7) %>% arrange(., Variable) #build a dataframe of results
    
    d2<- cbind (nb5$coefficients, as.data.frame(nb5$rse)) %>% 
      rownames_to_column() %>% mutate(model = "National Homeowners") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(3:5,7) %>% arrange(., Variable) #build a dataframe of results
    
    d3<- cbind (nb6$coefficients, as.data.frame(nb6$rse)) %>% 
      rownames_to_column() %>% mutate(model = "National Renters") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(3:5,7) %>% arrange(., Variable)#build a dataframe of results
    
    d4<- cbind (nb_c1$coefficients, as.data.frame(nb_c1$rse)) %>% 
      rownames_to_column() %>% mutate(model = "County Population") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(3:5,7) %>% arrange(., Variable) #build a dataframe of results
    
    d5<- cbind (nb_c5$coefficients, as.data.frame(nb_c5$rse)) %>% 
      rownames_to_column() %>% mutate(model = "County Homeowners") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(3:5,7) %>% arrange(., Variable) #build a dataframe of results
    
    d6<- cbind (nb_c6$coefficients, as.data.frame(nb_c6$rse)) %>% 
      rownames_to_column() %>% mutate(model = "County Renters") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(3:5,7) %>% arrange(., Variable)#build a dataframe of results
    
    d7<- cbind (nb_cd5$coefficients, as.data.frame(nb_cd5$rse)) %>% 
      rownames_to_column() %>% mutate(model = "County-Decade Homeowners") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(3:5,7) %>% arrange(., Variable) #build a dataframe of results
    
    d8<- cbind (nb_cd6$coefficients, as.data.frame(nb_cd6$rse)) %>% 
      rownames_to_column() %>% mutate(model = "County-Decade Renters") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(3:5,7) %>% arrange(., Variable)#build a dataframe of results
    
    home_rent <- bind_rows(d2,d3,d5,d6) %>% 
      mutate_at(.,vars(Variable),~str_replace(.,"_ho","")) %>% 
      mutate_at(.,vars(Variable),~str_replace(.,"_rent","")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"p_","")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"black","Black")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"hisp","Hispanic/Latinx")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"other","Other Race")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"low_ed","Low Education"))
    
    ggplot(home_rent) +
      geom_errorbarh(height = 0, aes(xmin = Effect - `Standard Error`, 
                                     xmax = Effect + `Standard Error`, 
                                     y = reorder(Variable, desc(Effect)),
                                     color= Model, group=Variable)) +
      geom_point(aes(x = Effect, y = reorder(Variable, desc(Effect)), color=Model), size = 2) +
      geom_vline(xintercept = 0) +
      theme_minimal()+
      labs(x = "Effect on Compensation (2019 $USD)",
           y = "")+
      scale_color_brewer(palette="RdBu")
    ggsave("home_rent_compensation.jpeg", device="jpeg", width = 6, height = 4, units = "in")
    
    
  # For density models
    
    d1<- cbind (d_n1$coefficients, as.data.frame(d_n1$rse)) %>% 
      rownames_to_column() %>% mutate(model = "National Population") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(4:6,8) %>% arrange(., Variable) #build a dataframe of results
    
    d2<- cbind (d_n5$coefficients, as.data.frame(d_n5$rse)) %>% 
      rownames_to_column() %>% mutate(model = "National Homeowners") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(4:6,8) %>% arrange(., Variable) #build a dataframe of results
    
    d3<- cbind (d_n6$coefficients, as.data.frame(d_n6$rse)) %>% 
      rownames_to_column() %>% mutate(model = "National Renters") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(4:6,8) %>% arrange(., Variable)#build a dataframe of results
    
    d4<- cbind (d_c1$coefficients, as.data.frame(d_c1$rse)) %>% 
      rownames_to_column() %>% mutate(model = "County Population") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(3:5,7) %>% arrange(., Variable) #build a dataframe of results
    
    d5<- cbind (d_c5$coefficients, as.data.frame(d_c5$rse)) %>% 
      rownames_to_column() %>% mutate(model = "County Homeowners") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(3:5,7) %>% arrange(., Variable) #build a dataframe of results
    
    d6<- cbind (d_c6$coefficients, as.data.frame(d_c6$rse)) %>% 
      rownames_to_column() %>% mutate(model = "County Renters") %>%
      setNames(., c("Variable","Effect","Standard Error","Model")) %>%
      slice(3:5,7) %>% arrange(., Variable)#build a dataframe of results
    
 
    
    home_rent <- bind_rows(d2,d3,d5,d6) %>% 
      mutate_at(.,vars(Variable),~str_replace(.,"_ho","")) %>% 
      mutate_at(.,vars(Variable),~str_replace(.,"_rent","")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"p_","")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"black","Black")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"hisp","Hispanic/Latinx")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"other","Other Race")) %>%
      mutate_at(.,vars(Variable),~str_replace(.,"low_ed","Low Education"))
    
    ggplot(home_rent) +
      geom_errorbarh(height = 0, aes(xmin = Effect - `Standard Error`, 
                                     xmax = Effect + `Standard Error`, 
                                     y = reorder(Variable, desc(Effect)),
                                     color= Model, group=Variable)) +
      geom_point(aes(x = Effect, y = reorder(Variable, desc(Effect)), color=Model), size = 2) +
      geom_vline(xintercept = 0) +
      theme_minimal()+
      labs(x = "Effect on Buyout Density (count/sqkm)",
           y = "")+
      scale_color_brewer(palette="RdBu")
    ggsave("home_rent_density.jpeg", device="jpeg", width = 6, height = 4, units = "in")
    
    
###########################################
### Time interaction model exploration ###
##########################################
    
    df_plm$decade<-df_plm$`Data Measurement Year`
    
    # National models 
    nt1<-felm(buyout ~ p_dependents + p_seniors + p_other*decade + p_black*decade +  p_hisp*decade + #only basic demographic breakdown
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`), #controls
             data=df_plm, na.action=na.omit) 
    summary(nt1, robust=T)
    saveRDS(nt1,"nt1.rds")
   
    st1<-felm(buyout ~ p_dependents + p_seniors + p_other*decade + p_black*decade +  p_hisp*decade + #only basic demographic breakdown
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`) | `State Code` , data=df_plm, na.action=na.omit)
    summary(st1, robust=T) 
    saveRDS(st1,"st1.rds")
    
    ct1<-felm(buyout ~ p_dependents + p_seniors + p_other*decade + p_black*decade +  p_hisp*decade + #only basic demographic breakdown
               pci + p_low_ed + bg_pop_density + factor(`Data Measurement Year`)| CNTY , data=df_plm, na.action=na.omit)
    summary(ct1, robust=T) 
    saveRDS(ct1,"ct1.rds")
    
   
 #######################################   
        #inla logit model

    # library(INLA)
    # 
    # df_plm$decade<-df_plm$`Data Measurement Year`
    # formula<- density ~  p_dependents  + p_other + p_black +  p_hisp + #only basic demographic breakdown
    #   pci + p_low_ed + bg_pop_density 
    # 
    # OUT<-inla(formula, data=df_plm, family = "gaussian", verbose=T,control.inla = list(int.strategy = "eb")) #run the inla model --> fails to converge
    # #saveRDS(OUT, file=paste0(wd,"/inla_logit.rds")) #save the model
    
    
    
    formula<- density ~ 1 + f(CNTY,model="iid") + f(decade, model="iid")

    OUT<-inla(formula, data=df_plm, family = "gaussian", verbose=T) #run the inla model --> fails to converge
    #saveRDS(OUT, file=paste0(wd,"/inla_logit.rds")) #save the model
    
    