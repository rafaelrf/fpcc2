library("plyr")
library("dplyr")
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
  geom_vline(xintercept = mean_parlamentares, colour = "red") +
  geom_vline(xintercept = median_parlamentares, colour = "blue") +
  geom_histogram(colour= "black", fill= "light green", binwidth = 3)+
  geom_rug()

ggplot(freqParlamentares,aes(x=factor(0),y=freq))+
  labs(title= "Frequência dos parlamentares na lista de gastos",
        x="",y="Frequência")+
  geom_smooth(stat = "identity",colour="black") +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.7,size=1.5,width = .7,colour="light green") 

freqParlamentares %>%
  summarise(mean(freq),median(freq))
##############TXTNOME

####SGUF
freq_uf <- count(data, c('sgUF'))    
mean_uf <- mean(freq_uf$freq)
median_uf <- median(freq_uf$freq)

freq_uf %>%
  summarise(Media = mean(freq),Mediana = median(freq))

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
  geom_smooth(stat = "identity",colour= "black") +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.7,size=3,width = .7,colour="blue") 
#######SGUF

####SGPARTIDO
freq_partido <- count(data, c('sgPartido'))    
mean_partido <- mean(freq_partido$freq)
median_partido <- median(freq_partido$freq)

freq_partido %>%
  summarise(Media= mean(freq), Mediana= median(freq))

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
  geom_smooth(stat = "identity",colour= "black") +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.9,size=3,width = .7,colour="orange") 
#######SGPartido

###TXTBeneficiario
beneficiarios <- count(data, c('txtBeneficiario'))    
mean_beneficiario <- mean(beneficiarios$freq)
median_beneficiario <- median(beneficiarios$freq)

beneficiarios %>%
  summary()

beneficiarios %>%
  ggplot(aes(x=freq)) +
  labs(title= "Frequência de gastos por partido.",
       x="Partido",y="Frequência")+
  geom_histogram(binwidth = .9,colour= "black",fill= "orange")+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))+
  geom_hline(yintercept = mean_beneficiario, colour = "red") +
  geom_hline(yintercept = median_beneficiario, colour = "blue") 

ggplot(beneficiarios,aes(x=factor(0),y=freq))+
  labs(title= "Frequência de gastos por partido.",
       x="",y="Frequência")+
  geom_smooth(stat = "identity") +
  geom_boxplot(outlier.shape = NA)+
  scale_y_log10() + 
  geom_jitter(stat = "identity",alpha=0.7,size=1,width = .7,colour="red") 
#######TXTBeneficiario

###txtDescricao
descricao <- count(data, c('txtDescricao'))    
mean_descricao <- mean(descricao$freq)
median_descricao <- median(descricao$freq)

descricao %>%
  summary()

descricao %>%
  ggplot(aes(y=freq,x=txtDescricao)) +
  labs(title= "Frequência dos tipos de gastos.",
       x="Tipo de Gasto",y="Frequência")+
  geom_bar(stat = 'identity',colour= "black",fill= "dark blue")+
  coord_flip()

ggplot(beneficiarios,aes(x=factor(0), y=freq))+
  labs(title= "Frequência de gastos por partido.",
       x="",y="Frequência")+
  geom_smooth(stat = "identity",colour="black") +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.7,size=1,width = .7,colour="dark blue") 
#######txtDescricao


####VlrLiquido
vlr_liquido <- select(data, vlrLiquido)
mean_liquido <- mean(vlr_liquido$vlrLiquido)
median_liquido <- median(vlr_liquido$vlrLiquido)

vlr_liquido %>%
  summary() 

quantile(vlr_liquido$vlrLiquido ,c(.80, .90, .99))

vlr_liquido %>%
  ggplot(aes(x=vlrLiquido)) +
  labs(title= "Valor Liquido",
       x="Valor",y="Frequência")+
  geom_histogram(binwidth = 5000,colour= "black",fill= "red")+
  scale_y_log10() + 
  geom_rug()

ggplot(vlr_liquido,aes(x=factor(0),y=vlrLiquido))+
  labs(title= "Valor Liquido",
       x="",y="Frequência")+
  geom_smooth(stat = "identity",colour= 'black') +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.4,size=.4,width = .7,colour="red")
#######VlrLiquido

####TOTAL
data %>%
  select(txNomeParlamentar,sgUF,sgPartido,vlrLiquido,
         txtBeneficiario,txtDescricao,txtNumero,datEmissao) %>%
  summary()

data %>%
  select(txtDescricao,vlrDocumento) %>%
  group_by(txtDescricao)


###PLOTS PRIMEIRA PERGUNTA
propPartido <- data %>% 
  select(sgPartido,vlrLiquido,txNomeParlamentar) %>%
  group_by(Partido=sgPartido) %>%
  dplyr::summarise(freq= n_distinct(txNomeParlamentar),total=sum(vlrLiquido))

propPartido %>%
  ggplot(aes(x=Partido,y=total/freq)) +
  labs(title= "Proporção dos Gastos de cada Partido",
       x="Partido",y="Total (milhares de R$)")+
  geom_bar(stat = "identity",colour= "black",fill= "orange")+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

propPartido %>%
  ggplot(aes(x=Partido,y=freq)) +
  labs(title= "Parlamentares por partido",
       x="Partido",y="Nº de Parlamentares")+
  geom_bar(stat = "identity",colour= "black",fill= "orange")+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

propPartido %>%
  #dplyr::filter(Partido %in% c("PSL","PT")) %>%
  dplyr::summarise(Proporcao= total/freq)

propPartido %>%
  ggplot(aes(x=factor(0),y=total/freq))+
  labs(title= "Gastos por Despesas",
       x="",y="Total (milhares de R$)")+
  geom_smooth(stat = "identity",colour= 'black') +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.4,size=3,width = .7,colour="red") 
###

####PLOTS SEGUNDA PERGUNTA
propUF <- data %>% 
  select(sgUF,vlrLiquido,txNomeParlamentar) %>%
  group_by(UF=sgUF) %>%
  dplyr::summarise(freq= n_distinct(txNomeParlamentar),total=sum(vlrLiquido))

propUF %>%
  ggplot(aes(x=UF,y=total/freq)) +
  labs(title= "Proporção dos Gastos de cada UF",
       x="UF",y="Total (milhares de R$)")+
  geom_bar(stat = "identity",colour= "black",fill= "orange")+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

propUF %>%
  ggplot(aes(x=UF,y=freq)) +
  labs(title= "Parlamentares por UF",
       x="UF",y="Nº de Parlamentares")+
  geom_bar(stat = "identity",colour= "black",fill= "orange")+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

propUF %>%
  dplyr::filter(UF %in% c("SE")) %>%
  summary()

propUF %>%
  ggplot(aes(x=factor(0),y=total/freq))+
  labs(title= "Gastos por Despesas",
       x="",y="Total (milhares de R$)")+
  geom_smooth(stat = "identity",colour= 'black') +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.4,size=3,width = .7,colour="red") 
###

###PLOTS TERCEIRA PERGUNTA
parlamentares <- data %>% 
  select(txNomeParlamentar,vlrLiquido) %>%
  group_by(Parlamentar= txNomeParlamentar) %>% 
  dplyr::summarise(total = sum(vlrLiquido))

parlamentares %>%
  dplyr::filter(total > 100000) %>%
  dplyr::arrange(desc(total))

parlamentares %>%
  dplyr::filter(total > 100000) %>%
  dplyr::arrange(desc(total)) %>%
  ggplot(aes(x=Parlamentar,y=total)) +
  labs(title= "Gastos por Despesas",
       x="Tipo de Despesa",y="Total (milhares de R$)")+
  geom_bar(stat = "identity",colour= "black",fill= "orange")+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

despesas <- data %>% 
  dplyr::filter(txNomeParlamentar %in% c("VALADARES FILHO","JOSÉ OTÁVIO GERMANO","ROBERTO BALESTRA")) %>%
  select(txtDescricao,vlrLiquido) %>%
  group_by(Tipo= txtDescricao) %>% 
  dplyr::summarise(total = sum(vlrLiquido))
  
  
despesas %>%
  ggplot(aes(x=Tipo,y=total,fill=Parlamentar)) +
  labs(title= "Gastos por Despesas",
       x="Tipo de Despesa",y="Total (milhares de R$)")+
  geom_bar(stat = "identity",colour= "black",fill= "orange",position = "stack")+
  coord_flip()
  

###

###PLOTS QUARTA PERGUNTA
beneficiarios <- data %>% 
  select(txtBeneficiario,vlrLiquido) %>%
  group_by(Beneficiarios=txtBeneficiario) %>% 
  dplyr::summarise(freq= n(),total = sum(vlrLiquido)) %>%
  dplyr::filter(total > 150000) 

beneficiarios %>%
  ggplot(aes(x=Beneficiarios,y=total)) +
  labs(title= "Gastos por Despesas",
       x="Tipo de Despesa",y="Total (milhares de R$)")+
  geom_bar(stat = "identity",colour= "black",fill= "orange")+
  coord_flip()

beneficiarios %>%
  summary()

beneficiarios %>%
  ggplot(aes(x=factor(0),y=total/1e3))+
  labs(title= "Gastos por Despesas",
       x="",y="Total (milhares de R$)")+
  geom_smooth(stat = "identity",colour= 'black') +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.4,size=3,width = .7,colour="red") 
###

###PLOTS QUINTA PERGUNTA
valores <- data %>% 
  select(txtDescricao,vlrLiquido) %>%
  group_by(Despesas=txtDescricao) %>% 
  dplyr::summarise(total = sum(vlrLiquido))

valores %>%
  ggplot(aes(x=Despesas,y=total/1e3)) +
  labs(title= "Gastos por Despesas",
       x="Tipo de Despesa",y="Total (milhares de R$)")+
  geom_bar(stat = "identity",colour= "black",fill= "orange")+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))+
  coord_flip()

valores %>%
  summary()

valores %>%
  ggplot(aes(x=factor(0),y=total/1e3))+
  labs(title= "Gastos por Despesas",
       x="",y="Total (milhares de R$)")+
  geom_smooth(stat = "identity",colour= 'black') +
  geom_boxplot(outlier.shape = NA, width= .7)+
  geom_jitter(stat = "identity",alpha=0.4,size=3,width = .7,colour="red") 
###
