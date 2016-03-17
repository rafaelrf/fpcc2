library("dplyr")
library("plyr")
library("ggplot2")
data <- read.csv("AnoAtual.csv")

#TXT NOME
freqParlamentares <- count(data, c('txNomeParlamentar'))    
mean_parlamentares <- mean(freqParlamentares$freq)
median_parlamentares <- median(freqParlamentares$freq)

freqParlamentares %>%
  ggplot(aes(freq)) +
  labs(title= "Frequência dos parlamentares na lista de gastos",
       y="Quantidade de Parlamentares",x="Frequência")+
  geom_vline(xintercept = mean_global, colour = "red") +
  geom_vline(xintercept = median_global, colour = "blue") +
  geom_histogram(colour= "black", fill= "light green", binwidth = 3)

ggplot(freqParlamentares,aes(x=factor(0),y=freq))+
  labs(title= "Frequência dos parlamentares na lista de gastos",
        x="",y="Frequência")+
  geom_smooth(stat = "identity") +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.3,size=1,width = .7) 

freqParlamentares %>%
  summary()
##############TXTNOME

####SGUF
freq_uf <- count(data, c('sgUF'))    
mean_uf <- mean(freq_uf$freq)
median_uf <- median(freq_uf$freq)

freq_uf %>%
  summary()

freq_uf %>%
  ggplot(aes(x=sgUF,y=freq)) +
  labs(title= "Frequência de gastos por estado",
       x="UF",y="Frequência")+
  geom_bar(stat = "identity",colour= "black",fill= "light blue")+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))+
  geom_hline(yintercept = mean_uf, colour = "red") +
  geom_hline(yintercept = median_uf, colour = "blue") 

ggplot(freq_uf,aes(x=factor(0),y=freq))+
  labs(title= "Frequência de gastos por estado.",
       x="",y="Frequência")+
  geom_smooth(stat = "identity") +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.7,size=1,width = .7) 
#######SGUF

####SGPARTIDO
freq_partido <- count(data, c('sgPartido'))    
mean_partido <- mean(freq_partido$freq)
median_partido <- median(freq_partido$freq)

freq_partido %>%
  summary()

freq_partido %>%
  ggplot(aes(x=sgPartido,y=freq)) +
  labs(title= "Frequência de gastos por partido.",
       x="Partido",y="Frequência")+
  geom_bar(stat = "identity",colour= "black",fill= "orange")+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))+
  geom_hline(yintercept = mean_uf, colour = "red") +
  geom_hline(yintercept = median_uf, colour = "blue") 

ggplot(freq_partido,aes(x=factor(0),y=freq))+
  labs(title= "Frequência de gastos por partido.",
       x="",y="Frequência")+
  geom_smooth(stat = "identity") +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.7,size=1,width = .7) 
#######SGPartido

####VlrLiquido
vlr_liquido <- select(data, vlrLiquido)
mean_liquido <- mean(vlr_liquido$vlrLiquido)
median_liquido <- median(vlr_liquido$vlrLiquido)

vlr_liquido %>%
  summary()

freq_partido %>%
  ggplot(aes(x=sgPartido,y=freq)) +
  labs(title= "Frequência de gastos por partido.",
       x="Partido",y="Frequência")+
  geom_bar(stat = "identity",colour= "black",fill= "orange")+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))+
  geom_hline(yintercept = mean_uf, colour = "red") +
  geom_hline(yintercept = median_uf, colour = "blue") 

ggplot(freq_partido,aes(x=factor(0),y=freq))+
  labs(title= "Frequência de gastos por partido.",
       x="",y="Frequência")+
  geom_smooth(stat = "identity") +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.7,size=1,width = .7) 
#######VlrLiquido


####TOTAL
data %>%
  select(txNomeParlamentar,sgUF,sgPartido,vlrLiquido,
         txtBeneficiario,txtDescricao,txtNumero,datEmissao) %>%
  summary()

data %>%
    select(txtDescricao,vlrDocumento) %>%
    group_by(txtDescricao='COMBUSTÍVEIS E LUBRIFICANTES.',sum(vlrDocumento))
    

