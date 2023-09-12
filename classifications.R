
library(tidyr)
library(readr)
library(tibble)
library(janitor)
library(stringr)
library(forcats)
library(dplyr)
library(ggplot2)
library(writexl)
library(stringi)
library(glue)
fn = file.path('data','subset-4-sous-ensemble-4.csv') # question  1:97 by ocupational group i.e. 'PC-2'
getwd()
#setwd(file.path(path.expand('~'), '..', 'projects', 'PSES'))

dat <- read_csv(fn)  
08
116116

#ec6 <- 
dat |> 
  #filter(DESCRIP_E == 'EC-07') |>
  group_by(BYCOND, DESCRIP_E, `DEPT_E`) |>
  summarise(N = max(ANSCOUNT, na.rm = TRUE)) |>
  ungroup() |>
  filter(is.finite(N)) |>
  filter(str_detect(BYCOND, '^OCCLEVEL') ) |> 
  group_by(DEPT_E) |>
  mutate(tot_emp = sum(N)) |>
  mutate(f = 100*N/tot_emp) |>
  inner_join(
  pay  |> 
  select(DESCRIP_E , CLASS_MAJOR , CLASS_MINOR  , last_step  ), by = "DESCRIP_E") |>
  filter(last_step > 116116) |>
  filter(! CLASS_MAJOR %in% c('EX', 'EN', 'LP'  )) |> 
  group_by(DEPT_E) |> 
  mutate(sum_f = sum(f)) |>
  mutate(sum_N = sum(N)) |> 
  arrange(desc(sum_f), desc(f)) |>
  View()
  
  #filter(DESCRIP_E =='EC-06')
  arrange(desc(N))

dat |> distinct( BYCOND, DESCRIP_E, LEVEL1ID, LEVEL2ID, DEPT_E )
dat |> distinct(  LEVEL1ID,  DEPT_E ) |> View()

dat |> filter(LEVEL1ID == '08') |> sample_n(1000) |> View()

pses <- 
dat |> 
  filter(LEVEL1ID == '08') |>
  filter(str_detect(BYCOND, 'OCCLEVEL')) |>
  #distinct(BYCOND, DESCRIP_E) |>
  group_by(BYCOND, DESCRIP_E) |>
  summarise(N = max(ANSCOUNT, na.rm = TRUE)) |>
  mutate(N = if_else(is.infinite(N), 0, N)) |> 
  separate(BYCOND, c('Q_NUM', 'Q_VAL')) |>
  group_by(Q_NUM) |> 
  mutate(F = N/sum(N))  |>
  ungroup() |>
  mutate(DESC_REV =stri_reverse(DESCRIP_E) ) |>
  separate(col = DESC_REV, into = c('CLASS_MINOR', 'CLASS_MAJOR'), remove = TRUE, sep = '-', convert  = FALSE, extra='merge') |> 
  mutate(CLASS_MINOR = as.integer(stri_reverse(CLASS_MINOR)), CLASS_MAJOR =stri_reverse(CLASS_MAJOR) ) |>  
  group_by(CLASS_MAJOR) |>
  mutate(CLASS_MAJOR_N = sum(N), CLASS_MAJOR_F = sum(F)) |>
  ungroup() |> 
  arrange(desc(CLASS_MAJOR_N), desc(N)) |>
  mutate(CLASS_MAJOR = fct_reorder(CLASS_MAJOR, CLASS_MAJOR_N ))# |> 
  #write_xlsx('classification_counts.xlsx')



library(readxl)


pses <- read_xlsx(path = 'classification_counts.xlsx', sheet = 'PSES')
pay <- read_xlsx(path = 'classification_counts.xlsx', sheet = 'pay') |>
  filter(! is.na(last_step))
  
dat_p <- 
    inner_join(
        pay  |> 
          select(CLASS_MAJOR , CLASS_MINOR  , last_step  )
        ,
        pses |> 
          select(CLASS_MAJOR , CLASS_MINOR, CLASS_MAJOR_N   , N ),
        by = c("CLASS_MAJOR", "CLASS_MINOR")
    ) |>
  group_by(CLASS_MAJOR) |>
  mutate(last_step_weighted_mean = weighted.mean(last_step , N)) #|>
  #mutate(N = N/CLASS_MAJOR_N)
  
dat_p |>
  ggplot(aes(x = last_step, y = N, color = CLASS_MAJOR )) + 
  geom_point() +
  geom_path(
    #arrow = arrow(type= 'closed', length = unit(0.15, "inches"))
    )
  

major_classes <- c('CR','SP','AS','PM', 'CS', 'EG', 'FB', 'AU', 'EC', 'EN', 'EX', 'LP')
major_classes <- c('EC', 'CS', 'BI', 'CH', 'PC', 'SE', 'SG')
dat_lbls <- 
bind_rows(
      dat_p |> 
        filter(CLASS_MAJOR %in% major_classes) |># View()
        group_by(CLASS_MAJOR) |>
        slice_max(N) |>
        ungroup() |>
        slice_max(N, n=7)  
      ,
      dat_p |> 
        filter(CLASS_MAJOR %in% major_classes) |>
        group_by(CLASS_MAJOR) |>
        slice_max(N) |>
        ungroup() |>
        slice_max(CLASS_MAJOR_N, n=7)  
      ,
      dat_p |>
        filter(CLASS_MAJOR %in% major_classes) |>
        slice_max(last_step) |>
        ungroup() |>
        slice_max(last_step, n=7)
) |>
  distinct(CLASS_MAJOR) |> 
  left_join(dat_p, by = "CLASS_MAJOR") |> 
  mutate(lbl = glue('{CLASS_MAJOR}\nTot Emp = {format(CLASS_MAJOR_N, big.mark=",")}\nAvg Last Step ${format(round(last_step_weighted_mean, 0), big.mark=",")}')) |>
  group_by(CLASS_MAJOR) |>
    slice_max(N, n =1) |> 
  ungroup() |> 
  filter(! is.na(last_step_weighted_mean ))


library(ggrepel)


dat_p |>
  filter(CLASS_MAJOR %in% dat_lbls$CLASS_MAJOR) |> 
  ungroup() |>
  ggplot(aes(x = last_step, y = N, color = CLASS_MAJOR )) + 
  #geom_point() +
  geom_path(linetype = "solid", size = 0.1
    #arrow = arrow(type= 'closed', length = unit(0.15, "inches"))
  ) + 
  geom_hline(yintercept = 0) +
  #geom_vline(xintercept = 54630) +
  geom_label(mapping = aes(label = CLASS_MINOR), alpha = 0.5) +
  geom_label_repel(data = dat_lbls, mapping = aes(label = lbl), 
                   #position = position_nudge_repel(x = 5000, y = 500), 
                   alpha = 0.75) +
  guides(color = 'none') +
  theme_minimal() +
  scale_x_continuous(labels=scales::dollar_format(), breaks = seq(10000, 1000000, 10000)) +
  #scale_y_continuous(breaks = seq(0, 10000, 500)) +
  labs(x = 'Income of Last Step (Upper End)', y = 'Number of Survey Responses (Employees)', title = 'Approximate Number of Employees', subtitle = 'by Upper End Salary and Employee Group') +
  theme(#panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #axis.text.x=element_blank(),
        axis.title=element_text(size=15, hjust=0.5, face="bold", colour="grey", vjust=-1),
        axis.text=element_text(size=12, face="bold", colour="grey"),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle = element_text(size=15, hjust=0.5, face="bold", colour="grey", vjust=-1),        
        plot.caption =element_text(size=18, hjust=0.5, face="italic", color="black")
  )

bounceRate_mean = weighted.mean(bounceRate,pageviews))







#filter(CLASS_MINOR == 16)
  #count(CLASS_MINOR, sort = T)
  ggplot(aes(x = N, y = CLASS_MAJOR, fill = CLASS_MINOR  )) +
  geom_col(CLASS_MAJOR , CLASS_MINOR  , last_step  )
  
  filter(CLASS_MINOR %in% c('MCO', 'FIN', 'VFM', 'TCO', 'IMA',''))
  distinct(CLASS_MINOR) |> pull()
  distinct(CLASS_MAJOR, CLASS_MAJOR_N) |> 
  


dat |> 
  #filter(LEVEL1ID == '00') |> 
  filter(str_detect(BYCOND, 'OCCLEVEL = 104')) |> 
  count(ANSCOUNT )

