#########################
### SCRIPT DE TABELAS E GRAFICOS
### SRAG (SIVEP-GRIPE)
### MRJ: jan a 25jul de 2022
### Previamente manipulada em aula
### Natalia S Paiva IESC UFRJ

# Enconding: UTF-8


# Vamos trabalhar com essas variáveis: 
# Data de notificacao, 
# Data dos 1os sintomas,
# sexo, 
# Raca, 
# Fator de risco, 
# Idade e tipo de idade, 
# Classificação final do caso e 
# Evolução

# criamos "Fator_Risco_count" baseado em: "CARDIOPATI", "HEMATOLOGI", "SIND_DOWN",  "HEPATICA", "ASMA", "DIABETES", "RENAL"

#####
# 1 - instalar e ativar pacotes
#####

# instalar (caso nao tenha na sua maquina) e chamar na biblioteca os pacotes necessarios


library(dplyr) # manipular base usando %>%
library(tidyr) # replace_na
library(gtsummary) # tabelas bem bonitas
library(ggplot2) # graficos bonitos
library(scales) # usar comando percent
library(lubridate) # trabalhando daras, criando mes, ano, semana epi

#####
# 2 - mudar diretorio
#####

# Session >> Set working directory >> choose...


#####
# 3 - importar base de dados (esta como RData - R workspace)
#####
load("srag_MRJ_jan-jul22.RData")

# olhando base de dados  no R - vai abrir uma "nova aba"
View(srag)

# mostrar as 6as linhas da base de dados 
head(srag) 

# olhando o nome das variaveis no R: names(base_de_dados)
names(srag)

# olhar o numero de linhas da base de dados
nrow(srag)

# olhar numero de variaveis / colunas
ncol(srag)

#######
# 4 - Olhar estrutura da base de dados e verificar classificacao das variaveis
#######

# estrutura da base
glimpse(srag)


# evolucao
srag$EVOLUCAO # olhando as respostas da variavel evolucao da base de dados srag

class(srag$EVOLUCAO) # classificacao

# fator de risco
srag$FATOR_RISC # olhando as respostas da variavel fator de risco da base de dados srag

class(srag$FATOR_RISC) # classificacao

# sexo
srag$CS_SEXO # olhando as respostas da variavel sexo da base de dados srag

class(srag$CS_SEXO) # classificacao

# data de notificacao

srag$DT_NOTIFIC # olhando as respostas da variavel dt notif da base de dados srag

class(srag$DT_NOTIFIC) # classificacao

###########################
################### TABELAS
###########################


# usando pacote gtsummary
# veja mais em
# https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html


names(srag)

# colocando virgula na casa decimal
options(OutDec = ",")


######
# TABELA DE FREQ SIMPLES
#####
srag %>%
  select(Sexo , "Cor da Pele",  Idade.anos,  fx_etaria,
         FATOR_RISC , Fator_Risco_count, EVOLUCAO, CLASSI_FIN) %>% 
  tbl_summary(digits = all_categorical() ~ c(0,1),
              label = list(Idade.anos ~ "Idade (em anos)",
                           fx_etaria ~ "Faixa etaria (em anos)",
                           Fator_Risco_count ~ "Número de fatores de risco",
                           FATOR_RISC ~ "Presença de fator de risco",
                           EVOLUCAO ~ "Evolução do paciente",
                           CLASSI_FIN ~ "Classificação final do caso"),
              missing_text = "Sem preenchimento",
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "**Variaveis**") %>%
  bold_labels() %>%
  add_n()



#########
# TABELA DE CONTINGENCIA: Desfecho: Evolucao.novo
########

# Evolucao.novo: Se obito por SRAG e não óbito

srag %>% 
  select(Evolucao.novo, Sexo , RACA.novo,  Idade.anos,  fx_etaria,
         FATOR_RISC , Fator_Risco_count, CLASSI_FIN) %>% 
  tbl_summary(by =  Evolucao.novo, # add variavel na coluna / desfecho
              digits = all_categorical() ~ c(0,1), # add casa decimal na %
              label = list(Idade.anos ~ "Idade (em anos)", # mudando nome das variaveis
                           fx_etaria ~ "Faixa etaria (em anos)",
                           Fator_Risco_count ~ "Número de fatores de risco",
                           RACA.novo ~ "Cor da pele",
                           FATOR_RISC ~ "Presença de fator de risco",
                           CLASSI_FIN ~ "Classificação final do caso"),
              missing_text = "Sem preenchimento",
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_overall(col_label = "**Total**")   %>% # add coluna d total e muda nome p "total"
  modify_header(label ~ "**Variaveis**") %>% # muda nome da coluna das covariaveis
  bold_labels() %>% # variaveis em negrito
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Evolução do paciente**") %>% # modifica nome do titutlo do desfecho (variavel da coluna)
  add_p() # add p valor 


##################
### Gráficos
##################



# cores basicas: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# cores brewer: https://r-graph-gallery.com/38-rcolorbrewers-palettes.html

# 1 - Grafico de colunas para Evolucao

ggplot(srag, aes(x= EVOLUCAO)) +  
  geom_bar(fill = "navyblue") + # cor azul escura
  theme_minimal()

# 1.1 - Grafico de barras para Evolucao

ggplot(srag, aes(x= EVOLUCAO)) +  
  geom_bar(fill = "navyblue")+ 
  coord_flip() + # coluna vira barra
  theme_minimal()

# 2 - Grafico de colunas para Evolucao MAIS BACANA

ggplot(srag, aes(x= EVOLUCAO)) +  
  geom_bar(fill = "navyblue")+ # cor azul escura
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = 1.5, colour = "red", size= 3.5)+ # inserindo freq. absoluta, posicao= 1.5 (dentro da coluna), cor vermelha e tamanho 3.5
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  labs(x = "Evolução do paciente", y= "Nº de casos notificados",
       title= "Figura 1: Número de casos de SRAG segundo Evolução do paciente",
       subtitle = "MRJ, jan-jul de 2022",
       caption= "Fonte de dados: SIVEP-GRIPE")+
  theme_minimal()+ # minimal com linhas de grades suaves
  theme(text = element_text(size= 12))  # tamanho das letras = 12

# EXERCICIO: FAZER UM GRAFICO DE COLUNA BACANA PARA FATOR_RISCO_CONT

ggplot(srag, aes(x= Fator_Risco_count)) +  
  geom_bar(fill = "darkorchid1")+ # cor meio roxa
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = -0.5, colour = "black", size= 4.5)+ # inserindo freq. absoluta, posicao= 1.5 (dentro da coluna), cor vermelha e tamanho 3.5
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  labs(x = "Num. de comorbidades", y= "Nº de casos notificados",
       title= "Figura 1: Número de casos de SRAG segundo Num. de comorbidades",
       subtitle = "MRJ, jan-jul de 2022",
       caption= "Fonte de dados: SIVEP-GRIPE")+
  theme_classic()+ # classic
  theme(plot.title = element_text(hjust = 0.5), # centraliza titulo
        plot.subtitle = element_text(hjust = 0.5), # centraliza subtitulo
        text = element_text(size= 14))  # tamanho das letras = 14


# 3 - EVOLUCAO: GRAFICO DE COLUNA Colocando N e % no topo da coluna

ggplot(srag, aes(x= EVOLUCAO)) +  
geom_bar(fill = "lightcoral")+ # cor coral
  geom_text(aes(label = paste0(..count.., " (", percent(..count../sum(..count..)),")")), 
            stat = "count", vjust = - 0.75, colour = "black", size= 3.75)+ # inserindo freq. absoluta e (%), posicao= - 0.75 (negativo = acima da coluna), cor preta e tamanho 3.75
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  labs(x = "Evolução do paciente", y= "Nº de casos notificados",
       title= "Figura 1: Número de casos de SRAG
       segundo Evolução do paciente",
       subtitle = "MRJ, jan-jul de 2022",
       caption= "Fonte de dados: SIVEP-GRIPE")+
  theme_classic()+ # tipo classico - bem clean!
  theme(text = element_text(size=12))  # tamanho das letras = 12



# 4 - Grafico de colunas para Evolucao (eixo Y em %)

# criando dataframe auxiliar p/ gerar %
tab.aux <- srag %>% group_by(EVOLUCAO) %>% 
  tally() %>% mutate(porcent = n/sum(n)*100) %>% print()

ggplot(tab.aux, aes(x= EVOLUCAO, y = porcent)) +  
  geom_bar(stat= "identity", fill= "violetred2")+ # cor violeta
  geom_text(label= paste0(round(tab.aux$porcent, 1), "%"), # inserindo porcent arredondado p/ 1 casa e o simbolo %
            nudge_x = 0, nudge_y = 0.85, size= 3.5) + # no eixo x = meio da barra, no eixo y = 0.85 acima da coluna, tamanho 3
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # inserindo eixo Y em %
  labs(x = "Evolução do paciente", y= "",
       title= "Figura 1: Porcentagem de casos de SRAG
       segundo Evolução do paciente",
       subtitle = "MRJ, jan-jul de 2022",
       caption= "Fonte de dados: SIVEP-GRIPE")+
  theme_bw() +
  theme(text = element_text(size=12))  # tamanho das letras = 12


# 5- Grafico de colunas para Class final segundo sexo - empilhada
# filtro: diferente de NA para fator de risco 

# Nota que voltamos para base de dados original

names(srag)

srag %>%
  filter(!is.na(FATOR_RISC)) %>% 
 ggplot()+
 aes(x= FATOR_RISC) +   
 geom_bar(aes(fill= Sexo)) + # colunas empilhadas
 scale_fill_brewer(palette = "Pastel1") + #Pastel2, Dark1, Set2...
 scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
 labs(x = "Presença de Fator de risco", y= "Nº de casos notificados",
       title= "Figura 1: Número de casos de SRAG
       segundo Presença de Fator de risco e sexo",
       subtitle = "MRJ, jan-jul de 2022",
       fill = "Sexo do paciente", # titulo para legenda
       caption= "Fonte de dados: SIVEP-GRIPE")+
 theme_minimal() + # tema minimal
 theme(legend.position = "bottom") # legenda embaixo


# veja mais cores brewer
library(RColorBrewer)
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,
                   colorblindFriendly=FALSE)

# 6 - Fator de risco segundo sexo, barras lado a lado
# filtro: diferente de NA para fator de risco E diferente de ignorado para Sexo

srag %>%
  filter(!is.na(FATOR_RISC), Sexo != "Ignorado") %>% 
  ggplot()+
  aes(x= FATOR_RISC, group = Sexo,  fill= Sexo) +   # reorder ; positivo = crescente
  geom_bar(aes(y= ..count..), stat="count", position=position_dodge()) + # position = "dodge" colunas lado a lado
  scale_fill_brewer(palette = "Set2", direction = -1) + # se remover ", direction = -1", Fem fica verde e Masc laranja
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  geom_text(aes(label = paste0(..count.., " (", percent(..count../sum(..count..)),")")),
            stat="count", vjust= -.5, size = 3, position=position_dodge(.9)) +
  theme_minimal() + # tema minimal
  labs(x= "Fator de Risco", y = "Nº de casos de SRAG",
       title = "Número de casos de SRAG por Presença de fator de risco segundo sexo",
       subtitle = "MRJ, jan-jul 2022",
       caption = "Fonte de dados: SIVEP-Gripe",
       fill = "Sexo do paciente") +
  theme(legend.position = "top") # legenda no topo

srag %>%
  filter(!is.na(FATOR_RISC), Sexo != "Ignorado") %>% 
  ggplot()+
  aes(x= FATOR_RISC, group = Sexo,  fill= Sexo) +   # reorder ; positivo = crescente
  geom_bar(aes(y= ..count..), stat="count", position=position_dodge()) + # position = "dodge" colunas lado a lado
  scale_fill_brewer(palette = "Set2", direction = -1) + # se remover ", direction = -1", Fem fica verde e Masc laranja
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  geom_text(aes(label= ..count.., 
                y=..count..), stat="count", vjust= -.5, size = 3, position=position_dodge(.9)) +
  theme_minimal() + # tema minimal
  labs(x= "Fator de Risco", y = "Nº de casos de SRAG",
       title = "Número de casos de SRAG por Presença de fator de risco segundo sexo",
       subtitle = "MRJ, jan-jul 2022",
       caption = "Fonte de dados: SIVEP-Gripe",
       fill = "Sexo do paciente") +
  theme(legend.position = "top") # legenda no topo


# 7 - serie temporal: data notif

dados.dtnot <- srag %>% 
  group_by(Data = DT_NOTIFIC) %>%
  tally()

dados.dtnot

# botando limite p datas 01 d jan 22 ate 01 de jul de 222
ggplot(dados.dtnot, aes(x= Data, y= n)) + 
  geom_line(colour= "Blue") +
  scale_x_date(limits = as.Date(c("2022-01-01", "2022-07-01")), # limitando datas ate 01 de julho
               date_breaks = "1 weeks",  # mostrar de 1 em 1 semanas
               date_labels = "%d %b") + # "%d/%m", "%d/%m/%y"
  theme_minimal(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # deixa datas do eixo x em 45 graus

# sem limite p datas
ggplot(dados.dtnot, aes(x= Data, y= n)) + 
  geom_line(colour= "Blue") +
  scale_x_date(date_breaks = "2 weeks",  # 2 em 2 semanas
               date_labels = "%d/%m/%y") + # "%d/%m", "%d/%m/%y"
  theme_minimal(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # deixa datas do eixo x em 45 graus


ggplot(dados.dtnot, aes(x= Data, y= n)) + 
  geom_line(colour= "Blue") +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d/%m/%y") + # "%d/%m", "%d/%m/%y"
  theme_minimal(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # deixa datas do eixo x em 45 graus


ggplot(dados.dtnot, aes(x= Data, y= n)) + 
  geom_line(colour= "Blue") +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%d/%b/%Y") + # "%d/%m", "%d/%m/%y"
  theme_minimal(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # deixa datas do eixo x em 45 graus


ggplot(dados.dtnot, aes(x= Data, y= n)) + 
  geom_line(colour= "Blue") +
  scale_x_date(date_breaks = "2 weeks", 
               date_labels = "%d/%b/%Y") + # "%d/%m", "%d/%m/%y"
  theme_minimal(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # deixa datas do eixo x em 45 graus

# filtrando para meses de data de notif menor que 7 (jan-jun)
dados.dtnot %>% filter(month(Data) < 7) %>%
ggplot(aes(x= Data, y= n)) + 
  geom_line(colour= "Blue") +
  scale_x_date(date_breaks = "2 weeks", 
               date_labels = "%d/%b/%Y") + # "%d/%m", "%d/%m/%y"
  theme_minimal(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # deixa datas do eixo x em 45 graus



# EXERCICIO: fazer serie temporal para data de inicio de sintmas
# colocar labs x,y, titulo, fonte de dados...


# 7.1 - serie temporal: data notif segundo classi fin
dados.dtnot <- srag %>% filter( !is.na(CLASSI_FIN)) %>%
  group_by(CLASSI_FIN, Data = DT_NOTIFIC) %>%
  tally()  %>% 
  print()

ggplot(dados.dtnot, aes(x= Data, y= n, color = CLASSI_FIN)) + 
  geom_line(size= 1.0) +
  scale_color_brewer(palette = "Accent", direction = -1) +  # inserindo eixo Y em %
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d/%m") +
  theme_minimal(base_size = 12) + # tema minimal
  labs(x= "Data de notificação", 
       y = "Num. de casos",
       color= "Classificação final do caso")+
  theme(legend.position = "bottom") # legenda no topo

# 8 - Boxplot

ggplot(srag, aes(x = "", y = Idade.anos, fill = 1))+
  geom_boxplot(fill = "pink") +  
  labs(x = "", y= "Idade (em anos)",
                          title= "Figura 1: Distribuição da Idade (em anos) dos casos de SRAG",
                          subtitle = "MRJ, jan-jul de 2022",
                          caption= "Fonte de dados: SIVEP-GRIPE")+
  theme_minimal() + # tema minimal
  theme(legend.position = "none")

# 9 - Boxplot idade segundo evolucao

ggplot(srag, aes(x=EVOLUCAO, y = Idade.anos, fill=EVOLUCAO)) + 
  geom_boxplot(alpha=0.3) +
  scale_fill_brewer(palette="Dark2") +
  theme_classic() + 
  theme(legend.position = "bottom")

# 10 - Piramide etaria

srag <- srag %>%
  mutate(fx_etaria2 = cut(Idade.anos, breaks = c(0, 10, 20, 40, 60, 80, Inf), 
                          right = FALSE,
                          labels = c("0 a 9", "10 a 19", "20 a 39", "40 a 59", "60 a 79",
                                     "80 ou mais")))

table(srag$fx_etaria2)

piramide <- srag %>% filter(Sexo != "Ignorado") %>%
  group_by(fx_etaria2, Sexo) %>%
   tally() %>%
    print()

ggplot(piramide) + 
  aes(x = factor(fx_etaria2),
      y = ifelse(test = Sexo == "Feminino",  yes = n, no = -n), 
      fill = Sexo) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = (max(piramide$n))* c(-1,1)) +
  scale_fill_brewer(palette = "Pastel1", direction = -1) +
  coord_flip() +
  labs(x = "Faixa etaria (em anos)", y= "Nº de casos notificados",
       title= "Figura 1: Número de casos de SRAG
       segundo Sexo e Faixa etária",
       subtitle = "MRJ, jan-jul de 2022",
       fill = "Sexo do paciente", # titulo para legenda
       caption= "Fonte de dados: SIVEP-GRIPE")+
  theme_minimal(base_size = 12) +
  theme(legend.position= "bottom")



# 10 - Piramide etaria por SRAG POR COVID-19

srag <- srag %>% 
  mutate(fx_etaria2 = cut(Idade.anos, breaks = c(0, 10, 20, 40, 60, 80, Inf), 
                          right = FALSE,
                          labels = c("0 a 9", "10 a 19", "20 a 39", "40 a 59", "60 a 79",
                                     "80 ou mais")))

table(srag$fx_etaria2)

piramide <- srag %>% filter(Sexo != "Ignorado", CLASSI_FIN == "SRAG por COVID-19") %>%
  group_by(fx_etaria2, Sexo) %>%
  tally() %>%
  print()

ggplot(piramide) + 
  aes(x = factor(fx_etaria2),
      y = ifelse(test = Sexo == "Feminino",  yes = n, no = -n), 
      fill = Sexo) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = (max(piramide$n))* c(-1,1)) +
  scale_fill_brewer(palette = "Pastel1", direction = -1) +
  coord_flip() +
  labs(x = "Faixa etaria (em anos)", y= "Nº de casos notificados",
       title= "Figura 1: Número de casos de SRAG-COVID19
       segundo Sexo e Faixa etária",
       subtitle = "MRJ, jan-jul de 2022",
       fill = "Sexo do paciente", # titulo para legenda
       caption= "Fonte de dados: SIVEP-GRIPE")+
  theme_minimal(base_size = 12) +
  theme(legend.position= "bottom")

############
#### BONUS
############

# Grafico de colunas para Cor da Pele (eixo Y em % e ordem decrescente)

# criando dataframe auxiliar p/ gerar % 

tab.aux <- srag %>% group_by(`Cor da Pele`) %>% 
  tally() %>% 
   mutate(porcent = n/sum(n)*100) %>% 
    print()

ggplot(tab.aux, aes(x= reorder(`Cor da Pele`, - porcent), y = porcent)) +   # reorder ; negativo = decrescente
  geom_bar(stat= "identity", fill= "turquoise4")+ # cor turquesa
  geom_text(label= paste0(round(tab.aux$porcent, 1), "%"), # inserindo porcent arredondado p/ 1 casa e o simbolo %
            nudge_x = 0, nudge_y = 0.85, size= 3) + # no eixo x = meio da barra, no eixo y = 0.85 acima da coluna, tamanho 3
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # inserindo eixo Y em %
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  theme_bw() # tema bw


# ORDEM CRESCENTE
ggplot(tab.aux, aes(x= reorder(`Cor da Pele`, + porcent), y = porcent)) +   # reorder ; positivo = crescente
  geom_bar(stat= "identity", fill= "turquoise4")+ # cor turquesa
  geom_text(label= paste0(round(tab.aux$porcent, 1), "%"), # inserindo porcent arredondado p/ 1 casa e o simbolo %
            nudge_x = 0, nudge_y = 0.85, size= 3) + # no eixo x = meio da barra, no eixo y = 0.85 acima da coluna, tamanho 3
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # inserindo eixo Y em %
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  theme_bw() # tema bw


# Grafico de colunas para CLASSI_FIN (eixo Y em % e ordem decrescente)

# criando dataframe auxiliar p/ gerar % - filtrando p diferente de NA

tab.aux <- srag %>% filter(!is.na(CLASSI_FIN)) %>% 
  group_by(CLASSI_FIN) %>% 
  tally() %>% 
  mutate(porcent = n/sum(n)*100) %>% 
  print()

ggplot(tab.aux, aes(x= reorder(CLASSI_FIN, - porcent), y = porcent)) +   # reorder ; negativo = decrescente
  geom_bar(stat= "identity", fill= "turquoise4")+ # cor turquesa
  geom_text(label= paste0(round(tab.aux$porcent, 1), "%"), # inserindo porcent arredondado p/ 1 casa e o simbolo %
            nudge_x = 0, nudge_y = 0.85, size= 3) + # no eixo x = meio da barra, no eixo y = 0.85 acima da coluna, tamanho 3
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # inserindo eixo Y em %
  theme_bw() # tema bw


# resolvendo sobreposicao dos nomes usando comando:
# scale_x_discrete(guide = guide_axis(n.dodge = 2)) 

ggplot(tab.aux, aes(x= reorder(CLASSI_FIN, - porcent), y = porcent)) +   # reorder ; negativo = decrescente
  geom_bar(stat= "identity", fill= "turquoise4")+ # cor turquesa
  geom_text(label= paste0(round(tab.aux$porcent, 1), "%"), # inserindo porcent arredondado p/ 1 casa e o simbolo %
            nudge_x = 0, nudge_y = 0.85, size= 3) + # no eixo x = meio da barra, no eixo y = 0.85 acima da coluna, tamanho 3
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # inserindo eixo Y em %
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  labs(x = "Classificação final do caso",
       y = "")+
  theme_classic() # tema bw


#### grafico de num de casos por mes

# criando variavel mes de dt de inicio de sintomas - pacote lubridate

class(srag$DT_SIN_PRI)

srag <- srag %>%
  mutate(MES = month(DT_SIN_PRI)) # year, epiweek

srag$MES


srag <- srag %>% mutate(MES = as.factor(MES)) # colocando MES como factor

# grafico classico de colunas
ggplot(srag, aes(x= MES)) + 
  geom_bar(fill= "pink3") +
  labs(x = "Mes de inicio de sintomas", 
       y = "Número de casos de SRAG", 
       title = "Meses de inicio dos sintomas dos casos de SRAG",
       subtitle = "MRJ, jan-jul - 2022",
       caption = "Fonte: SIVEP-GRIPE") +  
  theme_minimal(base_size = 12)

# grafico classico de colunas filtrando srag por covid

levels(srag$CLASSI_FIN)


srag %>%
  filter(CLASSI_FIN == "SRAG por COVID-19") %>%
ggplot() +
  aes(x= MES) + 
  geom_bar(fill= "darkorchid") +
  labs(x = "Mes de inicio de sintomas", 
       y = "Número de casos de SRAG-COVID19", 
       title = "Meses de inicio dos sintomas dos casos de SRAG-COVID19",
       subtitle = "MRJ, jan-jul - 2022",
       caption = "Fonte: SIVEP-GRIPE") +  
  theme_minimal(base_size = 12)


# grafico classico de colunas SEGUNDO classi fin


ggplot(srag, aes(x= MES)) + 
  geom_bar(fill= "darkorchid") +
  labs(x = "Mes de inicio de sintomas", 
       y = "Número de casos de SRAG", 
       title = "Meses de inicio dos sintomas dos casos de SRAG segundo Classi Final",
       subtitle = "MRJ, jan-jul - 2022",
       caption = "Fonte: SIVEP-GRIPE") +  
  theme_minimal(base_size = 12) +
  facet_wrap(vars(CLASSI_FIN), scales = "free") # escalas livres



ggplot(srag, aes(x= MES)) + 
  geom_bar(fill= "darkorchid") +
  labs(x = "Mes de inicio de sintomas", 
       y = "Número de casos de SRAG", 
       title = "Meses de inicio dos sintomas dos casos de SRAG",
       subtitle = "MRJ, jan-jul - 2022",
       caption = "Fonte: SIVEP-GRIPE") +  
  theme_minimal(base_size = 12) +
  facet_wrap(vars(CLASSI_FIN)) # escalas iguais



# Casos de SRAG por mes segundo Classi fin em %

tab.aux <- srag %>% group_by(CLASSI_FIN, MES) %>% 
  tally() %>% 
  mutate(porcent = n/sum(n)*100) %>%
  print()

ggplot(tab.aux, aes(x= MES, y = porcent)) +  
  geom_bar(stat= "identity", fill= "violetred2")+ # cor violeta
  geom_text(label= paste0(round(tab.aux$porcent, 1), "%"), # inserindo porcent arredondado p/ 1 casa e o simbolo %
            nudge_x = 0, nudge_y = 2.5, size= 3) + # no eixo x = meio da barra, no eixo y = 0.85 acima da coluna, tamanho 3
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # inserindo eixo Y em %
  labs(x = "Mes de inicio de sintomas", y= "",
       title= "Figura 1: Nº de casos de SRAG por mes de inicio dos sintomas e class final do caso",
       subtitle = "MRJ, jan-jul de 2022",
       caption= "Fonte de dados: SIVEP-GRIPE")+
  theme_bw() +
  theme(text = element_text(size=12)) +
  facet_wrap(vars(CLASSI_FIN)) # escalas iguais


#### barra empilahada 100% - 

### evolucao por mes - coluna empilhada em %

freq <- srag %>% group_by(MES, EVOLUCAO) %>%
  summarise(n = n()) %>% 
  mutate(porcent = n / sum(n) * 100) %>% 
  print()


ggplot(freq) +
  aes(x = MES , y = porcent, fill = EVOLUCAO, label = round(porcent, 1)) +
  geom_col() +
  scale_fill_brewer(palette = "Set3") +  # cor
  geom_text(label = paste0(round(freq$porcent,1), "%"), 
            position = position_stack(vjust = 0.5)) + # colocando % nas colunas
  labs(
    x = "Mes de inicio de sintomas",
    y = "%",
    title = "Distribuicao dos casos de SRAG por
    Mes de inicio de sintomas e Evolucao",
    subtitle = "MRJ, jan-jul de 2022",
    fill = "Evolucao",
    caption = "Fonte de dados: SIVEP-GRIPE"
  ) +
  theme_classic()+
  theme(legend.position= "bottom")



# Grafico de dispersao

# Esse conjunto de dados de temperatura média (TM) e
# Umidade relativa (UR) foi obtido no site do 
# Instituto Agronômico do Paraná 
# (http://www.iapar.br/modules/conteudo/conteudo.php?conteudo=1828) no período de 01/09/2018 a 21/02/2019.

TM=c(23.4,19.8,12.8,16.3,20.8,17.4,20.0,21.8,21.8,20.6,20.3,20.6,20.4,18.1,20.2,19.3,17.2,20.8,
     20.7,17.4,21.8,23.8,25.8,26.3,25.3,24.5,21.4,23.3,24.3,21.2,24.3,23.6,23.5,22.3,21.4,19.7,
     22.1,20.5,22.3,21.8,18.0,21.3,24.1,23.9,23.6,23.3,23.2,22.0,22.4,20.8,20.2,22.6,23.7,19.9,
     19.7,21.4,22.5,21.4,20.4,24.5,22.7,20.3,23.7,24.0,23.6,21.6,22.0,22.6,21.3,22.5,22.2,26.3,
     27.2,28.3,25.9,25.8,26.6,24.9,20.8,19.4,21.6,22.2,23.8,20.9,22.9,25.0,23.7,23.8,24.8,24.8,
     24.8,25.4,24.4,23.5,24.7,25.3,25.2,23.8,22.8,22.2,26.0,27.8,28.1,25.8,26.8,25.3,25.0,26.6,
     26.4,26.7,26.8,25.5,24.0,23.2,22.6,23.4,24.5,25.7,25.0,26.4,26.2,26.2,26.9,24.7,25.6,25.0,
     23.7,22.8,25.5,26.3,26.9,25.1,26.7,25.6,24.5,26.2,26.2,24.4,26.3,25.6,24.4,24.0,26.7,28.2,
     26.3,26.7,25.4,24.8,24.6,26.3,28.7,28.6,26.3,28.6,29.0,28.2,24.3,23.0,22.9,24.6,26.6,28.5,
     28.0,25.5,23.2,23.7,23.0,22.4,23.6,23.6,23.5,23.5,22.9,23.5)

UR=c(68,93,86,55,54,51,45,43,55,54,58,57,64,89,73,80,96,71,86,95,74,62,49,43,51,62,86,73,64,95,
     68,77,86,93,76,63,69,94,88,89,88,67,76,84,71,88,83,83,74,54,51,61,74,97,94,97,66,58,65,56,
     82,93,66,64,67,65,67,67,63,62,76,51,57,54,80,65,65,65,93,88,63,68,65,98,83,64,67,62,59,78,
     75,70,63,62,53,46,42,55,60,51,51,47,42,60,62,77,74,58,63,67,66,83,81,87,95,80,71,68,74,69,
     75,74,75,90,86,91,91,98,84,81,74,82,69,77,84,78,74,87,75,80,89,90,77,73,82,80,82,75,79,70,
     61,63,74,63,58,62,76,76,74,69,64,56,61,86,94,85,78,91,82,80,81,85,89,84)

# UF criada para fins didaticos
UF = sample(c("SC", "PR", "RS"), 174, replace = T)

# criando objeto dados (uma base de dados)
dados = data.frame(Umidade= UR,Temperatura= TM, UF = UF)


# grafico de dispersao entre temp e umidade
ggplot(dados, aes(x = Umidade, y = Temperatura)) +
  geom_point(color = "red") + 
  labs(x = "Umidade (em %)",
       y = "Temperatura (em ºC)",
       title = "Figura 1: Temperatura e Umidade")+
  theme_classic()

# grafico de dispersao entre temp e umidade por UF
ggplot(dados, aes(x = Umidade, y = Temperatura)) +
  geom_point(aes(colour = factor(UF))) + 
  labs(x = "Umidade (em %)",
       y = "Temperatura (em ºC)",
       colour = "Unidade da Federação",
       title = "Figura 1: Temperatura e Umidade")+
  theme_classic()

# grafico de dispersao entre temp e umidade por UF
ggplot(dados, aes(x = Umidade, y = Temperatura)) +
  geom_point(aes(colour = factor(UF))) + 
  scale_color_brewer(palette = "Accent")+ #mudando cores
  labs(x = "Umidade (em %)",
       y = "Temperatura (em ºC)",
       colour = "Unidade da Federação",
       title = "Figura 1: Temperatura e Umidade")+
  theme_classic()



# grafico de dispersao entre temp e umidade por UF
# eixo x indo de 0 a 100 pulando de 20 em 20
ggplot(dados, aes(x = Umidade, y = Temperatura)) +
  geom_point(aes(shape = factor(UF))) + # muda shape
  labs(x = "Umidade (em %)",
       y = "Temperatura (em ºC)",
       shape = "Unidade da Federação",
       title = "Figura 1: Temperatura e Umidade")+
  scale_x_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, by = 20))+ # # eixo x indo de 0 a 100 pulando de 20 em 20
  theme_classic()+
  theme(legend.position = "bottom")


# grafico de dispersao entre temp e umidade por UF
# eixo x indo de 0 a 100 pulando de 20 em 20
# eixo x indo de 0 a 100 pulando de 20 em 20
ggplot(dados, aes(x = Umidade, y = Temperatura)) +
  geom_point(aes(shape = factor(UF))) + # muda shape
  labs(x = "Umidade (em %)",
       y = "Temperatura (em ºC)",
       shape = "Unidade da Federação",
       title = "Figura 1: Temperatura e Umidade")+
  scale_x_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, by = 20))+ # # eixo x indo de 0 a 100 pulando de 20 em 20
  scale_y_continuous(limits = c(0, 30), 
                     breaks = seq(0, 30, by = 5))+ # eixo y de 0 a 30 indo de 5 em 5
  theme_classic()+
  theme(legend.position = "bottom")

