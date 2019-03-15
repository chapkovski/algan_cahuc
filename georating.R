library(foreign)
library(tidyr)
file.choose()
dataset = read.spss(
  "/Users/chapkovski/Google Drive/regional_project/IBeLab_Surveys/_Georating/Георейтинг_ВШЭ.sav",
  to.data.frame = TRUE
)

attr(dataset, "variable.labels") %>% kable() %>% kable_styling()
?attr
names_to_convert <- names(dataset[, 600:608])
objs <- lapply(names_to_convert, (function(x)
  (deparse((x)))))

dataset %>% mutate_at(names_to_convert, as.character) %>% mutate(
  region =  coalesce(
    Q104,Q105,Q106,Q107,Q108,Q109,Q110,Q111
  ) )-> t

# Q1
t %>% group_by(region, Q1) %>%summarise(n=n())%>%mutate(prop=n/sum(n))%>%
  subset(select=c("region","Q1","prop"))%>%  spread(Q1, prop) %>%subset(select=c('region','большинству людей можно доверять')) %>%rename(trust=`большинству людей можно доверять`)%>%arrange(trust)%>% kable() %>% kable_styling()

# Q2
drop.cols<-c('не изменился','затрудняюсь ответить','Нет ответа')
t %>% group_by(region, Q2) %>%summarise(n=n())%>%mutate(prop=round(n / sum(n)*100,1))%>%
  subset(select=c("region","Q2","prop"))%>%  spread(Q2, prop) %>%arrange(`безусловно увеличился`)%>%select(-one_of(drop.cols))%>%
  kable() %>% kable_styling()

# Q3
drop.cols<-c('затрудняюсь ответить','Нет ответа')
t %>% group_by(region, Q3) %>%summarise(n=n())%>%mutate(prop=round(n / sum(n)*100,1))%>%
  subset(select=c("region","Q3","prop"))%>%  spread(Q3, prop) %>%arrange(`безусловно согласия, сплоченности`)%>%select(-one_of(drop.cols))%>%
  kable() %>% kable_styling()

# Q4
drop.cols<-c('затрудняюсь ответить','Нет ответа')
t %>% group_by(region, Q4) %>%summarise(n=n())%>%mutate(prop=round(n / sum(n)*100,1))%>%
  subset(select=c("region","Q4","prop"))%>%  spread(Q4, prop) %>%arrange(`безусловно согласия, сплоченности`)%>%select(-one_of(drop.cols))%>%
  kable() %>% kable_styling()


# Q5
drop.cols<-c('затрудняюсь ответить','Нет ответа')
t %>% group_by(region, Q5) %>%summarise(n=n())%>%mutate(prop=round(n / sum(n)*100,1))%>%
  subset(select=c("region","Q5","prop"))%>%  spread(Q5, prop)  %>%arrange(`безусловно к первым`)%>%select(-one_of(drop.cols))%>%
  kable() %>% kable_styling()
