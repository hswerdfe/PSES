library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(cansim)


library(CANSIM2R)
library(Hmisc)

library(pastecs)
library(psych)
library(skimr)
library(purrr)
library(janitor)
library(scales)
library(igraph)
library(colorspace)
library(gganimate)
library(plotly)
library(glue)
library(tidyverse)
getwd()
#setwd(file.path(path.expand('~'), '..', 'projects', 'PSES'))

x <- cansim::search_cansim_cubes('Labour force characteristics, annual')

x$cansim_table_number |>
  map(cansim::get_cansim) |> setNames(x$cansim_table_number )
x <- cansim::search_cansim_cubes('Labour force characteristics')
x |> select(cansim_table_number , cubeTitleEn,subjectEn   , surveyEn    ) |> View()

x |> filter(str_detect(cubeTitleEn  ,'minority')) |> select(cansim_table_number , cubeTitleEn,subjectEn   , surveyEn    )

minorities_in_PS <-function(regex_for_question ='^(D121).*',
                            fn = file.path('data', '2020-pses-employment-equity-derived-variable-dataset-saff-2020-ensemble-de-donnees-variables-der.csv') ){
  # fn = file.path('data','subset-3-sous-ensemble-3.csv')# question  1:97 by demographics (minority/sexual orientation)
  # regex_for_question ='^(Q121).*'
  dat <- 
    fn |> 
    read_csv()  
  
  
  dat$ANSCOUNT |> max(na.rm = TRUE)
  dept_e <- 'Canada School of Public Service'
  dat |> distinct(DESCRIP_E) |> pull() |> str_subset('chool')
  
  dat |>
    filter(DESCRIP_E == dept_e)
  
  
  dat2 <- 
    dat |> 
    filter(str_detect(BYCOND, regex_for_question)) |>
    filter(LEVEL1ID == '00') |>
    group_by(BYCOND, DESCRIP_E, DESCRIP_F) |>
    summarise(ANSCOUNT_MAX = max(ANSCOUNT, na.rm = TRUE)) |>
    separate(BYCOND, c('Q_NUM', 'Q_VAL')) |>
    group_by(Q_NUM) |> 
    mutate(f = ANSCOUNT_MAX/sum(ANSCOUNT_MAX)) 
  
  desc <- 
    dat2 |>
    filter(Q_VAL == 1) |> 
    select(Q_NUM, DESCRIP_E,DESCRIP_F)
  #  ps2<-
  dat2 |>
    select(-DESCRIP_E, -DESCRIP_F, -ANSCOUNT_MAX) |> 
    pivot_wider(names_from = c('Q_VAL'), values_from = f, names_prefix = 'val_') |>
    left_join(desc,  by = "Q_NUM") |> 
    arrange(desc(val_1))  |>
    ungroup()
  
}



# minorities_in_CSPS <- function((regex_for_question ='^(D121).*',
#                                 fn = file.path('data', '2020-pses-employment-equity-derived-variable-dataset-saff-2020-ensemble-de-donnees-variables-der.csv')){
#   dept_e <- 'Canada School of Public Service'
#   
#   
#   
#   dat <- 
#     fn |> 
#     read_csv()  
#   
#   
#   dat$ANSCOUNT |> max(na.rm = TRUE)
#   dept_e <- 'Canada School of Public Service'
#   dat |> distinct(DESCRIP_E) |> pull() |> str_subset('chool')
#   
#   dat |>
#     
#   
#   
#   dat2 <- 
#     dat |> 
#     filter(DESCRIP_E == dept_e) |>
#     filter(str_detect(BYCOND, regex_for_question)) |>
#     
#     #filter(LEVEL1ID == '00') |>
#     group_by(BYCOND, DESCRIP_E, DESCRIP_F) |>
#     summarise(ANSCOUNT_MAX = max(ANSCOUNT, na.rm = TRUE)) |>
#     separate(BYCOND, c('Q_NUM', 'Q_VAL')) |>
#     group_by(Q_NUM) |> 
#     mutate(f = ANSCOUNT_MAX/sum(ANSCOUNT_MAX)) 
#   
#   desc <- 
#     dat2 |>
#     filter(Q_VAL == 1) |> 
#     select(Q_NUM, DESCRIP_E,DESCRIP_F)
#   #  ps2<-
#   dat2 |>
#     select(-DESCRIP_E, -DESCRIP_F, -ANSCOUNT_MAX) |> 
#     pivot_wider(names_from = c('Q_VAL'), values_from = f, names_prefix = 'val_') |>
#     left_join(desc,  by = "Q_NUM") |> 
#     arrange(desc(val_1))  |>
#     ungroup()
#   
# }

#########################
#Labour force characteristics by visible minority group, three-month moving averages, monthly, unadjusted for seasonality
##########################
tbl_nm = '14-10-0373'
dat_raw <-get_cansim(tbl_nm) |> clean_names()
dat_raw |> 
  filter(geo == 'Canada' & uom == 'Persons' & ref_date == '2022-09') |>
  filter(str_detect(sex, 'Both sexes')) |> 
  select(age_group, labour_force_characteristics,  population_group, value) |>
  write_csv('minorities_by_age_and_group.csv')



######################################
#  13-10-0380
#Visible minority group of persons with and without disabilities aged 15 years and over, by age group and sex, Canada
#
######################################
minorities_in_canada <- function(tbl_nm = '13-10-0380'){
  #tbl_nm = '14-10-0373'
  dat_raw <- get_cansim(tbl_nm) |> clean_names()
  
  
 # |> count(age_group)  |> View()
 #  filter(!str_detect(age_group, '65 years and over'))
  
  tmp <- 
    dat_raw |>
    filter(geo == 'Canada' & uom == 'Number' & ref_date == '2017') |>
  #  filter(age_group %in% c('25 to 44 years','45 to 64 years')) |>
    filter(str_detect(sex, 'Total, both sexes'))|>
    filter(str_detect(disability, 'Total population')) |>
    select(visible_minority, age_group, value) |>
    pivot_wider(names_from = age_group, values_from = value) |>
    clean_names()
  
  
  tmp |> write_csv('minority_by_age.csv')
    #View()count(visible_minority)
    # group_by(visible_minority) |>
    # summarise(value = sum(value, na.rm = TRUE)) |> arrange(desc(value))
    
  
  tmp |>
    select(-total_15_years_and_over) |>
    pivot_longer(cols = -visible_minority, values_to = 'n') |>
    group_by(visible_minority) |>
    summarise(n = sum(n)) |>
    arrange(desc(n)) |>
    #filter(visible_minority !=  'Not a visible minority') |>
    filter( ! visible_minority %in% c('Total, by visible minority group', 'Total visible minority population')) |>
    mutate(f = n/sum(n, na.rm = TRUE))

}











ps |> pull(DESCRIP_E)
ps
ps2
can |> pull(visible_minority)



ps <- minorities_in_PS()


can <- 
  minorities_in_canada() |>
  rename_all(~{paste0(.x, '_can')})
  

mapping <- 
list("Black" = "Black" ,
     "Filipino" = "Filipino",
     "Korean" = "Korean", 
     "Japanese" = "Japanese", 
     "South Asian" = "South Asian/East Indian" ,
     "Southeast Asian" = "Southeast Asian",
     "Chinese" = "Chinese",
     'Latin American' = 'Non-White Latin American' ,
     'Visible minority, not included elsewhere' ='Other visible minority group',
     'Multiple visible minorities'='Person of mixed origin',
     'Not a visible minority'='Not a visible minority'
     ) |> as_tibble() |>
  pivot_longer(cols = everything(), values_to = 'visible_minority_ps', names_to =  'visible_minority_can')

Not a visible minority




ps_official_total_minority <- 0.189
20.5-18.9

ps  |>
  select(DESCRIP_E, val_1) |>
  set_names('visible_minority_ps', 'f_ps') |>
  mutate(f_minority_ps = sum(f_ps)) |>
  left_join(mapping) |>
  full_join(can) |>
  mutate(delta = f_ps  - f_can) |>
  filter(!is.na(delta)) |>
  arrange(desc(f_can)) |>
  mutate(lbl_cat = fct_reorder(paste0(visible_minority_ps, ' (', format(round(f_can*100, 1)), '%)'), f_can ) ) |>
  ggplot(aes(x = delta, y = lbl_cat)) +
  geom_col()
  
  mutate(x = delta*250000)
  



  
  
  
  
  
  ps  |>
    select(DESCRIP_E, val_1) |>
    set_names('visible_minority_ps', 'f_ps') |>
    mutate(f_minority_ps = sum(f_ps)) |>
    left_join(mapping) |>
    full_join(can) |>
    mutate(delta = f_ps  - f_can) |>
    filter(!is.na(delta)) |>
    select(visible_minority_ps, f_ps,f_can) |>
    pivot_longer(cols = matches('^f_')) |>
    mutate(lbl_cat = fct_reorder(visible_minority_ps,  value ) ) |>
    ggplot(aes(y = lbl_cat,x = value, fill = name  )) +
    geom_col(position =  position_dodge(width = 0.9))
  
  
  
  
  ps  |>
    select(DESCRIP_E, val_1) |>
    set_names('visible_minority_ps', 'f_ps') |>
    mutate(f_minority_ps = sum(f_ps)) |>
    left_join(mapping) |>
    full_join(can) |>
    mutate(delta = f_ps  - f_can) |>
    filter(!is.na(delta))  
  
  
