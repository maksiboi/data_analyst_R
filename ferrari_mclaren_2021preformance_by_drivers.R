library(ggplot2)
library(dplyr)
library(ggthemes)
library("readxl")
library(plotly)
library(scales)

df <- readxl::read_xlsx("C:/Users/Maks/Desktop/sezona2021.xlsx")

View(df)

#Ferrari vs McLaren 2021 preformance

data_ferr_mcl <- df%>%filter(Drivers %in% c("Carlos Sainz Jnr","Lando Norris","Charles Leclerc","Daniel Ricciardo"))

data_ferr_mcl <- data_ferr_mcl %>% 
  group_by(Drivers) %>%  
  mutate(accumulated=cumsum(Points),dates=as.Date(Datum))%>%
  select(Drivers,Races,dates,Points,accumulated)

View(data_ferr_mcl)

graf <- data_ferr_mcl %>%
  ggplot(aes(x=dates,y=accumulated,color=Drivers))+
  geom_line(size=1,alpha=1)+
  geom_point(size=0.5)+
  theme_fivethirtyeight()+
  scale_color_brewer(name="",palette='Set1')+
  labs(title="Ferrari's vs McLarens's driver preformance\n in 2021 season")+
  theme(axis.text.x = element_text(angle=90))+
  ylim(0,250)+
  scale_x_date(limit=c(as.Date("2021-03-15"),as.Date("2021-12-31")),date_labels = "%b",date_breaks = "1 month")


ggplotly(graf)




