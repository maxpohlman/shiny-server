data<-read.csv('hw/mydata.csv', header=TRUE)
rstr <- 'Round'
tstr <- 'Treatment'
fil <- c('Certainty', 'Ambiguity')
rd <- c('1','2')

adata<-data[data[[tstr]] %in% fil & data[[rstr]] %in% rd,]


p<-adata %>%
  group_by(Treatment,Round) %>%
  summarize(tm = mean(efficiency)) %>%
  ggplot(aes(x = Treatment, y =tm )) +
  geom_bar(aes(fill = as.factor(Round)), position = 'dodge', stat="identity")
p

test <-adata %>%
  group_by(as.name(tstr)), Round) %>%
  summarize(tm = mean(efficiency))

testas <- 5
testtwo<- as.name('testas') + 2