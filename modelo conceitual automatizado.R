#Contém o modelo conceitual de avaliação econômica da polinização agrícola automatizado. 
#O arquivo deve ser usado no programa R.

#A aplicação do modelo consiste em 4 etapas:
#1ª: inserir os coeficientes dos modelos estatístico da produtividade e da qualidade do produto;
#2º: inserir os dados de custo de produção e dos preços;
#3º: montar as equações de produtividade, qualidade, custo de produção, receita e lucro;
#4º: Simulação dos cenários das projeções do lucro em relação às práticas de manejo (gráficos).

#Para replicar o modelo sem alterar as variáveis de manejo consideradas no estudo de caso com o feijão, 
#basta adaptar os valores dos coeficientes (1ª etapa) e dos dados de custo de produção e de preços (2ª etapa)
#Para acrescentar novas variáveis de manejo agrícola (convencional e de polinizadores), 
#terá que adaptar todas as etapas, acrescentando as novas variáveis onde for necessário. 


#Primeira etapa: Inserir os coeficientes das seguintes equações:
#1. Modelo de produtividade;
#2. Modelo de baixa qualidade;
#3. Modelo de alta qualidade. 

#########################
#MODELO DA PRODUTIVIDADE#
#########################

# Este modelo indica o quanto a média da produtividade por flor (g/flor) é afetada por:
# 1. prática de fertilizante (kg de nitrogênio por hectare);
# 2. prática de manejo de abelhas (densidade de abelha do mel - visitantes por flor);
# 3. prática do capital natural (densidade de polinizadores nativos - visitantes por flor);
# 4. e as interações entre essas três práticas. 

#intercepto: produtividade média (g/flow) obtida sem as práticas de manejo consideradas neste estudo. 
#desvio padrão: obtido pela multiplicação entre o erro padrão e a raiz quadrada do grau de liberdade dos resíduos. 
baseY= -7.4024
baseYsd= 2.1232*sqrt(26) 
#fertilizante: efeito do nitrogênio (kg/ha) na produtividade (g/flor). 
#   Nessa variável foi usada a transformação logarítmica. 
#   Em seguida, inserir o erro padrão e o grau de liberdade do resíduo para calcular o desvio padrão. 
eN=1.6145 
eNsd= 0.4663*sqrt(26)
#manejo de abelhas: o efeito da densidade de abelhas do mel (visitantes por flor) na produtividade (g/flor). 
#   Em seguida, inserir o erro padrão e o grau de liberdade do resíduo para calcular o desvio padrão.
eMB=-37.1699
eMBsd=29.1504*sqrt(26)
#capital natural: o efeito da densidade de polinizadores nativos (visitantes por flor) na produtividade (g/flor). 
#   Em seguida, inserir o erro padrão e o grau de liberdade do resíduo para calcular o desvio padrão.
eNC = 2420.3289
eNCsd=1357.9591*sqrt(26) 
#(fertilizante)*(manejo de abelhas): efeito da interação entre nitrogênio (fertilizante) e o manejo de abelha (densidade)
#   na produtividade (g/flor).
#   Em seguida, inserir o erro padrão e o grau de liberdade do resíduo para calcular o desvio padrão. 
iN_MB=0 
iN_MBsd=0
#(fertilizante)*(capital natural): efeito da interação entre nitrogênio (fertilizante) e o capital natural (densidade)
#   na produtividade (g/flor).
#   Em seguida, inserir o erro padrão e o grau de liberdade do resíduo para calcular o desvio padrão. 
iN_NC=-530.7988
iN_NCsd=300.6824*sqrt(26)
#(manejo de abelha)*(capital natural): efeito da interação entre manejo de abelha (densidade) e o capital natural (densidade)
#   na produtividade (g/flor).
#   Em seguida, inserir o erro padrão e o grau de liberdade do resíduo para calcular o desvio padrão. 
iNC_MB=0
iNC_MBsd=0



###########################
#MODELO DA BAIXA QUALIDADE#
###########################

# Este modelo indica o quanto a probabilidade de um feijão ser considerado de baixa qualidade é afetada por:
# 1. prática de fertilizante (kg de nitrogênio por hectare);
# 2. prática de manejo de abelhas (densidade de abelha do mel - visitantes por flor);
# 3. prática do capital natural (riqueza de polinizadores nativos - nº de espécies por flor);
# 4. e as interações entre essas três práticas.

#O primeiro valor se refere ao coeficiente de cada variável explicativa do modelo. 
#O segundo valor se refere ao desvio padrão calculado com base no erro padrão e no grau de liberdade do resíduo. 

#intercepto: probabilidade média obtida sem o efeito das práticas de manejo conisderadas no estudo. 
baseQl=6.403e-01
baseQlsd=3.520*sqrt(26)
#fertilizante: efeito do nitroênio (kg/ha) na probabilidade de ocorrência da baixa qualidade.
eqlN=-5.795e-02 
eqlNsd=2.919e-02*sqrt(26)
#manejo de abelha: efeito da densidade de abelhas (visitantes por flor) na probabilidade de ocorrência da baixa qualidade.  
eqlMB=5.717e+03
eqlMBsd=1.654e+03*sqrt(26)
#capital natural: efeito da riqueza de polinizadores nativos (nª de espécies por flor) na probabilidade de ocorrência da baixa qualidade. 
eqlNC=-6.599e+03 
eqlNCsd=2.299e+03*sqrt(26)
#(fertilizante)*(manejo de abelhas): efeito da interação entre nitrogênio (kg/ha) e o manejo de abelha (densidade) na probabilidade de baixa qualidade.
iqlN_MB=5.362e+01 
iqlN_MBsd=1.615e+01
#(fertilizante)*(capital natural): efeito da interação entre nitrogênio (kg/ha) e o capital natural (riqueza) na probabilidade de baixa qualidade.
iqlN_NC=7.342e+01
iqlN_NCsd=2.476e+01*sqrt(26)
#(manejo de abelhas)*(capital natural): efeito da interação entre abelha do mel (densidade) e o capital natural (riqueza) na probabilidade de baixa qualidade.
iqlNC_MB=-3.906e+05
iqlNC_MBsd=2.166e+05*sqrt(26)

##########################
#MODELO DA ALTA QUALIDADE#
##########################

# Este modelo indica o quanto a probabilidade de um feijão ser considerado de alta qualidade é afetada por:
# 1. prática de fertilizante (kg de nitrogênio por hectare);
# 2. prática de manejo de abelhas (densidade de abelha do mel - visitantes por flor);
# 3. prática do capital natural (densidade de polinizadores nativos - visitantes por flor);
# 4. e as interações entre essas três práticas. 

#O primeiro valor se refere ao coeficiente de cada variável explicativa do modelo. 
#O segundo valor se refere ao desvio padrão calculado com base no erro padrão e no grau de liberdade do resíduo. 


#intercepto: probabilidade obtida sem o efeito das práticas de manejo conisderadas no estudo. 
baseQh=-4.247e+00
baseQhsd=2.412*sqrt(26)
#fertilizante: efeito do nitroênio (kg/ha) na probabilidade de ocorrência da alta qualidade.
eqhN=2.783e-02
eqhNsd=2.290e-02*sqrt(26)
#manejo de abelha: efeito da densidade de abelhas (visitantes por flor) na probabilidade de ocorrência da alta qualidade.  
eqhMB=-1.020e+03
eqhMBsd=3.500e+02*sqrt(26)
#capital natural: efeito da densidade de polinizadores nativos (visitantes por flor) na probabilidade de ocorrência da alta qualidade. 
eqhNC=5.094e+03
eqhNCsd=6.598e+02*sqrt(26)
#(fertilizante)*(manejo de abelhas): efeito da interação entre nitrogênio (kg/ha) e o manejo de abelha (densidade) na probabilidade de alta qualidade.
iqhN_MB=1.156e+01 
iqhN_MBsd=3.363*sqrt(26)
#(fertilizante)*(capital natural): efeito da interação entre nitrogênio (kg/ha) e o capital natural (riqueza) na probabilidade de alta qualidade.
iqhN_NC=-5.538e+01
iqhN_NCsd=7.208*sqrt(26)
#(manejo de abelhas)*(capital natural): efeito da interação entre abelha do mel (densidade) e o capital natural (riqueza) na probabilidade de alta qualidade.
iqhNC_MB=0
iqhNC_MBsd=0



#SEGUNDA ETAPA: Inserir os dados dos custos de produção e dos preços  

#Custo associado com a aplicação de pesticida (R$/ha).
Cpest=0 
#Custo associado com a aplicação de fertilizante (R$ por kg de nitrogenio aplicado por hectare).
#O custo do fertilizante foi baseado no custo do nitrogênio contido na ureia. 
#1 kg de ureia custa R$ 1,56 e contém 0.4 kg de nitrogênio, logo, 1kg de nitrogênio custa R$3.54.
Cfert=3.54
#Custo associado com o manejo de abelhas.
#O custo está associado ao valor do investimento para atingir uma dada densidade de abelha do mel (visitantes por flor). 
#O aluguel de uma colmeia é de R$350,00 e oferece 333 abelhas por hectare. 
#Cada hectare tem em média 38900 flores (durante o pico da floração) (calculado com base em uma área de 50m2 em cada local de amostragem).
#Assim, cada colmeia permite um aumento de densidade de 0.0086 abelhas do mel por flor. 
#Portanto, o custo para se ter 1 abelha do mel por flor é de R$40800 por hectare por mês (350/(333/38900)).
#Este valor será multiplicado pela densidade que se busca atingir.
#Por exemplo, considerando uma densidade de 0.01 visitantes por flor, o custo do manejo de abelha será de R$408/mês (40800*0.01).
#Assumi-se que os produtores aluguarão as colmeias por 1 mes (período da floração).
Cmb=40800
#Custo associado com o manejo do capital natural (R$/ha).
Cnc=0
#Outros custos fixos (R$/ha).
Cother=229
#Custo variável (R$ por kg produzido) 
VC=0.83
#Preços associados a cada categoria de qualidade do produto (R$)
PriceHQ=2.25 # para alta qualidade
PriceMQ=2 # para média qualidade
PriceLQ=1.75 # para baixa qualidade




#TERCEIRA ETAPA: Montagem das equações
#Para as equações, deve-se monstar considerando os códigos dos coeficientes inseridos nas duas primeiras etapas. 

##EQUAÇÃO DA PRODUTIVIDADE

Yield_function = function(NC, MB, N){
  
  #Efeito do capital natural (NC), do manejo de abelhas (MB) e do fertilizante (N) sobre a produtividade (Yo)
  #O modelo de produtividade foi obtido com a transformação logit em (Y/(2-Y)), pois se trata de uma variável sigmóide (formato em s)
  #Para estimat a produtividade é preciso fazer a transoformação reversa, logo Yo=(2/(1/exp(Y)+1))    
        
        Yo=2/ (1/exp(baseY+eN*log(N)+(eMB+iN_MB*log(N)+iNC_MB*NC)*MB+(eNC+iN_NC*log(N))*NC)+1)
        ###Convertendo a estima de g/flor para kg/ha. 
        ### peso por vagem * num flor por m2 /1000(gr para kg)*10000 (m2 para ha); numero vagens médio= 196 (vagens/15*10))
        Y=Yo*196/1000*10000
  return(Y)
}

##EQUAÇÃO DO LUCRO (PF), QUE INCLUI:
    #EQUAÇÃO DA PRODUTIVIDADE;
    #EQUAÇÕES DOS MODELOS DE QUALIDADE;
    #EQUAÇÃO DO CUSTO DE PRODUÇÃO (PC);
    #EQUAÇÃO DA RECEITA (R). 

Pf_function = function(NC, MB, N){
      
        ###EQUAÇÃO DA PRODUTIVIDADE
        #Efeito do capital natural (NC), do manejo de abelhas (MB) e do fertilizante (N) sobre a produtividade (Yo)
        Yo=2/ (1/exp(baseY+eN*log(N)+(eMB+iN_MB*log(N)+iNC_MB*NC)*MB+(eNC+iN_NC*log(N))*NC)+1)
        Y=Yo*196/1000*10000
        
        ###EQUAÇÕES DOS MODELOS DE QUALIDADE
        #Efeito do capital natural (NC), do manejo de abelhas (MB) e do fertilizante (N) 
        #sobre a probabilidade de cada categoria de qualidade
        # ALTA qualidade 
          PHQ=baseQh+(eqhNC+iqhN_NC*N)*NC+eqhN*N+(eqhMB+iqhN_MB*N+iqhNC_MB*NC)*MB
        #Aplicando a transformação reversa no loggit (para modelos que assumem a distribuição binomial)  
          PHQbt=1/(1+1/exp(PHQ))
        #BAIXA qualidade  
          PLQ=baseQl+(eqlNC+iqlN_NC*N)*NC+eqlN*N+(eqlMB+iqlN_MB*N+iqlNC_MB*NC)*MB
        #Aplicando a transformação reversa no loggit (para modelos que assumem a distribuição binomial)  
          PLQbt=1/(1+1/exp(PHQ))    
        #MÉDIA qualidade  
          PMQ=1 - PHQbt-PLQbt 
        
        
        ###EQUAÇÃO DO CUSTO DE PRODUÇÃO (PC)
        FC=Cpest+Cfert*N+Cmb*MB+Cnc*NC+Cother #custo fixo
        VCy= VC*Y  #custo variável
        PC= FC+VCy  #custo total
        
        ###EQUAÇÃO DA RECEITA (R) 
        #R calculada considerndo a produtividade rateada entre as três qualidade do produto 
        #Multiplica pelo preço ajustado pela qualidade. 
        R = PriceLQ*Y*PLQbt+ PriceMQ*Y*PMQ+PriceHQ*Y*PHQbt 
        
        ###EQUAÇÃO DO LUCRO (Pf)
        Pf = R-PC
        return(Pf)
      }
      
#As equações seguintes são usadas para a projeçao do desvio padrão nos gráficos. 
      Pfsd_function = function(NC, MB, N){
        
 
        #formulas SE
        Pfsd_vector=rep(0, length(NC)*length(MB)*length(N))
        for (i in 1:length(N)){
          for (j in 1:length(NC)) {
            for (k in 1:length(MB)){
              
              ###EQUAÇÃO DA PRODUTIVIDADE
              #Efeito do capital natural (NC), do manejo de abelhas (MB) e do fertilizante (N) sobre a produtividade (Yo)
              #Y=baseY+(eNC+iN_NC*log(N[i]))*NC[j]+eN*log(N[i])+(eMB+iN_MB*log(N[i])+iNC_MB*NC[j])*MB[k]
              Yo=2/ (1/exp(baseY+eN*log(N[i])+(eMB+iN_MB*log(N[i])+iNC_MB*NC[j])*MB[k]+(eNC+iN_NC*log(N[i]))*NC[j])+1)
              Y=Yo*196/1000*10000
              
              ###EQUAÇÕES DOS MODELOS DE QUALIDADE
              #Efeito do capital natural (NC), do manejo de abelhas (MB) e do fertilizante (N) 
              #sobre a probabilidade de cada categoria de qualidade 
              # ALTA qualidade
              PHQ=baseQh+(eqhNC+iqhN_NC*N[i])*NC[j]+eqhN*N[i]+(eqhMB+iqhN_MB*N[i]+iqhNC_MB*NC[j])*MB[k]
              #Aplicando a transformação reversa no loggit (para modelos que assumem a distribuição binomial)  
              PHQbt=1/(1+1/exp(PHQ))
              #BAIXA qualidade  
              PLQ=baseQl+(eqlNC+iqlN_NC*N[i])*NC[j]+eqlN*N[i]+(eqlMB+iqlN_MB*N[i]+iqlNC_MB*NC[j])*MB[k]
              #Aplicando a transformação reversa no loggit (para modelos que assumem a distribuição binomial) 
              PLQbt=1/(1+1/exp(PHQ))    
              #MÉDIA qualidade  
              PMQ=1 - PHQbt-PLQbt 
              
              ####CUSTO DE PRODUÇÃO (PC)
              FC=Cpest+Cfert*N[i]+Cmb*MB[k]+Cnc*NC[j]+Cother  
              VCy= VC*Y 
              PC= FC+VCy 
              
              ### RECEITA (R) 
              R = PriceLQ*Y*PLQbt+ PriceMQ*Y*PMQ+PriceHQ*Y*PHQbt 
              
              ###LUCRO (PF)
              Pf = R-PC
              
              
                 
              ###PROJETANDO O DESVIO PADRÃO
              #PRODUTIVIDADE
              Ysdo=2/ (1/exp(rnorm(10000,mean=baseY, sd=baseYsd)+rnorm(10000,mean=eN, sd=eNsd)*log(N[i])+
                            (rnorm(10000,mean=eMB, sd=eMBsd)+rnorm(10000,mean=iN_MB, sd=iN_MBsd)*log(N[i])+
                               rnorm(10000,mean=iNC_MB, sd=iNC_MBsd)*NC[j])*MB[k]+
                            (rnorm(10000,mean=eNC, sd=eNCsd)+rnorm(10000,mean=iN_NC, sd=iN_NCsd)*log(N[i]))*NC[j])+1)
              Ysd=Ysdo*196/1000*10000 #NÚMERO MÉDIO DE FLORES POR HECTARE (196 flores por m2)
              
              #QUALIDADE
              PHQsd=sd(rnorm(10000,mean=baseQh, sd=baseQhsd)+(rnorm(10000,mean=eqhNC, sd=eqhNCsd)+
                                                                rnorm(10000,mean=iqhN_NC, sd=iqhN_NCsd)*N[i])*NC[j]+
                         rnorm(10000,mean=eqhN, sd=eqhNsd)*N[i]+
                         (rnorm(10000,mean=eqhMB, sd=eqhMBsd)+rnorm(10000,mean=iqhN_MB, sd=iqhN_MBsd)*N[i]+
                            rnorm(10000,mean=iqhNC_MB, sd=iqhNC_MBsd)*NC[j])*MB[k])
              
              PHQbt_sd=sd(1/(1+1/exp(rnorm(10000,mean=PHQ, sd=PHQsd))))
              
              
              
              PLQsd=sd(rnorm(10000,mean=baseQl, sd=baseQlsd)+(rnorm(10000,mean=eqlNC, sd=eqlNCsd)+
                                                                rnorm(10000,mean=iqlN_NC, sd=iqlN_NCsd)*N[i])*NC[j]+
                         rnorm(10000,mean=eqlN, sd=eqlNsd)*N[i]+(rnorm(10000,mean=eqlMB, sd=eqlMBsd)+
                                                                rnorm(10000,mean=iqlN_MB, sd=iqlN_MBsd)*N[i]+
                                                                rnorm(10000,mean=iqlNC_MB, sd=iqlNC_MBsd)*NC[j])*MB[k])
              
              PLQbt_sd=sd(1/(1+1/exp(rnorm(10000,mean=PHQ, sd=PHQsd))) )   
              PMQsd=sd(1 - rnorm(10000,mean=PHQbt, sd=PHQbt_sd)-rnorm(10000,mean=PLQbt , sd=PLQbt_sd))
              
              #CUSTO DE PRODUÇÃO (PC)
              FCsd=0
              VCy_sd= sd(VC*rnorm(10000,mean=Y, sd=Ysd) )
              PCsd= sd((FC+VC*rnorm(10000,mean=Y, sd=Ysd))  )
              
              #RECEITA (R)
              Rsd = sd(PriceLQ*rnorm(10000,mean=Y, sd=Ysd)*rnorm(10000,mean=PLQbt, sd=PLQbt_sd)+ 
                         PriceMQ*rnorm(10000,mean=Y, sd=Ysd)*rnorm(10000,mean=PMQ, sd=PMQsd)+
                         PriceHQ*rnorm(10000,mean=Y, sd=Ysd)*rnorm(10000,mean=PHQbt, sd=PHQbt_sd) )
              
              #LUCRO (PF)
              Pfsd = sd(rnorm(10000,mean=R, sd=Rsd)-rnorm(10000,mean=PC, sd=PCsd))
              
              
              if (length(N)>1){Pfsd_vector[i]=Pfsd}
              if (length(NC)>1){Pfsd_vector[j]=Pfsd}
              if (length(MB)>1){Pfsd_vector[k]=Pfsd}
            }}}
        
        Lower_line=Pf-Pfsd_vector
        
        return(Pfsd_vector)
      }
      


#QUARTA ETAPA: Montagem dos gráficos 
#Aqui é possível projetar os gráficos do lucro (R$/ha) associado com o manejo de fertilizantes, de colmeias e de capital natural. 
      par(mfrow=c(1,1))
      
      #GRÁFICO 1 - simula como o lucro varia com o capital natural 
      #Manejo de colmeia:
          #Com manejo de colmeias (0.01 abelha por flor) (MB=0.01);
          #Sem manejo de colmeias (MB=0) 
      #Manejo de fertilizante (N)
          #Uso elevado de nitrogênio (kg/ha) (N=130)
          #Uso reduzido de nitrogênio (kg/ha) (N=60)
      New.function<-function(x){Pf_function(x, MB=0, N=60)}
      New.function_sd<-function(x){Pfsd_function(x, MB=0, N=60)}
      fun_LOWCI <- function(x){
        New.function(x) - 1.96*(New.function_sd(x)/sqrt(26))
      }
      fun_HIGHCI <- function(x){
        New.function(x) + 1.96*(New.function_sd(x)/sqrt(26))
      }
      
      plot(fun_LOWCI,  xlab="", ylab="", main="", xlim=c(0,0.01), ylim=c(-3000,8000), col="grey",type = "h" )
      par(new=TRUE)
      plot(fun_HIGHCI,  xlab="", ylab="", main="", xlim=c(0,0.01), ylim=c(-3000,8000), col="grey",type = "h" )
       par(new=TRUE)
      plot(New.function,  xlab="Capital natural (visitantes por flor)", ylab="Lucro (R$.ha-1)",lwd = 4, main="Baixo Ninput", xlim=c(0,0.01), ylim=c(-3000,8000) )
      x=1:2000/1000-1
      y=0*x
      lines(x,y, col="red")
      
      
      #GRÁFICO 2 - simula como o lucro varia com o manejo de colmeia (densidade de abelhas do mel)
      #Manejo de capital natural (NC):
          #Densidade de polinizadores nativos de 0.001 (NC=0.001)
      #Manejo de fertilizante (N)
          #Uso elevado de nitrogênio (kg/ha) (N=130)
          #Uso reduzido de nitrogênio (kg/ha) (N=60)
      New.function<-function(x){Pf_function(NC=0.001, x, N=130)}
      New.function_sd<-function(x){Pfsd_function(NC=0.001, x, N=130)}
      fun_LOWCI <- function(x){
        New.function(x) - 1.96*(New.function_sd(x)/sqrt(26))
      }
      fun_HIGHCI <- function(x){
        New.function(x) + 1.96*(New.function_sd(x)/sqrt(26))
      }
      
      plot(fun_LOWCI,  xlab="", ylab="", main="", xlim=c(0,0.01), ylim=c(-3000,6000), col="grey",type = "h"  )#, ylim=c(0,1e+05 )
      par(new=TRUE)
      plot(fun_HIGHCI,  xlab="", ylab="", main="", xlim=c(0,0.01), ylim=c(-3000,6000), col="grey",type = "h"  )#, ylim=c(0,1e+05 )
      par(new=TRUE)
      plot(New.function,  xlab="Abelhas do mel (visitantes por flor)", ylab="Lucro (R$/ha)", main="Baixo Ninput" ,lwd = 4, xlim=c(0,0.01), ylim=c(-3000,6000))
      x=1:2000/1000-1
      y=0*x
      lines(x,y, col="red")
      
      #GRÁFICO 3 - simula como o lucro varia com o fertilizante 
      #Manejo de colmeia (MB):
          #Com manejo de colmeias (0.01 abelha por flor) (MB=0.01);
          #Sem manejo de colmeias (MB=0)
      #Manejo de capital natural (NC):
          #Densidade de polinizadores nativos de 0.001 (NC=0.001)
      New.function<-function(x){Pf_function(NC=0.001, MB=0.01, x)}
      New.function_sd<-function(x){Pfsd_function(NC=0.001, MB=0.01, x)}
      fun_LOWCI <- function(x){
        New.function(x) - 1.96*(New.function_sd(x)/sqrt(26))
      }
      fun_HIGHCI <- function(x){
        New.function(x) + 1.96*(New.function_sd(x)/sqrt(26))
      }
      
       plot(fun_LOWCI,  xlab="", ylab="", main="", xlim=c(0,150), ylim=c(-3000,6000), col="grey",type = "h"  )#, ylim=c(0,1e+05 )
      par(new=TRUE)
      plot(fun_HIGHCI,  xlab="", ylab="", main="", xlim=c(0,150), ylim=c(-3000,6000), col="grey",type = "h"  )#, ylim=c(0,1e+05 )
     par(new=TRUE)
      plot(New.function,  xlab="Nitrogênio (kg/ha)", ylab="Lucro (R$/ha)", main="Com colmeia",lwd = 4, xlim=c(0,150), ylim=c(-3000,6000))
      x=1:150-1
      y=0*x
      lines(x,y, col="red")
     
      
