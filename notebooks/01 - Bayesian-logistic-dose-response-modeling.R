require(rstan)
library(boa)
library(bayesplot)
library(rstanarm)
library(ggplot2)
library(dplyr)
dados = read.table("df_lc50.csv", header = T, sep=";", dec = ",")
dados$periodo = as.factor(dados$periodo)
dados <- dados %>% mutate(proporcao =  (dados$resposta)/60)
dados <- dados %>% mutate(logdose = log(dados$concentracao))
dados <- dados %>% mutate(Período = dados$periodo)
dados<- mutate(dados, 
               C_resposta=60-resposta)
dados <- dados %>% dplyr::mutate(Period = ifelse(periodo %in% c("24h","24h","24h","24h","24h",
                                                                    "48h","48h","48h","48h","48h",
                                                                    "72h","72h","72h","72h","72h"),c("Duração do Experimento: 24h",
                                                                                                     "Duração do Experimento: 24h",
                                                                                                     "Duração do Experimento: 24h",
                                                                                                     "Duração do Experimento: 24h",
                                                                                                     "Duração do Experimento: 24h",
                                                                                                     "Duração do Experimento: 48h",
                                                                                                     "Duração do Experimento: 48h",
                                                                                                     "Duração do Experimento: 48h",
                                                                                                     "Duração do Experimento: 48h",
                                                                                                     "Duração do Experimento: 48h",
                                                                                                     "Duração do Experimento: 72h",
                                                                                                     "Duração do Experimento: 72h",
                                                                                                     "Duração do Experimento: 72h",
                                                                                                     "Duração do Experimento: 72h",
                                                                                                     "Duração do Experimento: 72h")))
x11()
ggplot(dados, aes(x = logdose, y = proporcao)) + geom_point(aes(shape=Period),size=3) + facet_wrap(~Period) +
  xlab("log(Concentrações g i.a./L)") + coord_cartesian(ylim=c(0,1), xlim=c(-8,-1)) +
  ylab("Proporção de abelhas mortas") + theme(legend.position = "none",axis.title = element_text(size = 22,color="black"),
                                       axis.text = element_text(size = 22,color="black"),
                                       strip.text.x = element_text(size = 22,color="black"),
                                       legend.title = element_text(size = 22),
                                       legend.text = element_text(size = 22))
                              
main="Experiment Duration: 24h"
########################################################################
########################### Curvas Ajustadas a Posteriori ##############
########################################################################
#########################################################################
############################ Extra ######################################
#########################################################################
configuracao<- theme(
  axis.title.x = element_text(color="black", size=22, face="bold"),
  axis.title.y = element_text(color="black", size=22, face="bold"), 
  strip.text.x = element_text(
    size = 14, color = "black", face = "bold"
  ),
  strip.text.y = element_text(
    size = 14, color = "black", face = "bold"
  ),
  axis.text.x = element_text(
    size = 14, color = "black", face = "bold" ),
  axis.text.y = element_text(
    size = 14, color = "black", face = "bold"
  ) )
########################################################################
############################## 24h #####################################
################# Considerando concentrações puras #####################
########################################################################
modelo_logistico <- "data {
int<lower=0> N;
vector[N] x;
int<lower=0> y[N];
int<lower=0> n[N];
}
parameters {
real beta1;
real beta2;
}
model {
beta1 ~ normal(0,100);
beta2 ~ normal(0,100);
y ~ binomial_logit(n, beta1 + beta2 * x);
}
generated quantities {
vector[N] log_likelihood;
for (i in 1:N) {
log_likelihood[i] = binomial_logit_lpmf(y[i] | n, beta1 + beta2 * x[i]);
}
}"

dados2 = dados[c(1:5),]
n = length(dados2$logdose)
y = dados2$resposta
logistic_example24 <- "data {
int<lower=0> N;
vector[N] x;
int<lower=0> y[N];
int<lower=0> n[N];
}
parameters {
real beta1;
real beta2;
}
model {
beta1 ~ normal(0,100);
beta2 ~ normal(0,100);
y ~ binomial_logit(n, beta1 + beta2 * x);
}"

logistic_fit24 <- stan(model_code = modelo_logistico,
                       data = list(N = dim(dados2)[1], n = dados2$total,
                                   x = dados2$logdose, y = dados2$resposta),
                       chain = 3, iter = 11000, warmup = 1000,
                       thin = 10, refresh = 0)
parameters <- c(paste("beta", 1:2, sep = ""))
CI_theta24 <- summary(logistic_fit24, pars = parameters,
                      probs = c(0.025, 0.975))$summary
print(round(CI_theta24, 3))
##########################################
############# Intervalo HPD ##############
##########################################
cadeia_amostras24 <- extract(logistic_fit24)
betas124 <- cadeia_amostras24$beta1
boa.hpd(x=betas124,alpha=0.1)
quantile(cadeia_amostras24$beta1, c(5/100,95/100))#NOVO
##########################################
############# Intervalo HPD ##############
##########################################
betas224 <- cadeia_amostras24$beta2
boa.hpd(x=betas224,alpha=0.1)
##########################################
############# Cadeias ####################
##########################################
x11()
stan_trace(logistic_fit24)
par(cex=1.5,cex.lab=1.3)
traceplot(logistic_fit24,inc_warmup=T,ncol=1,col="black")+
  xlab("Iterações") +
  theme_bw() + theme(axis.title = element_text(size = 28,color="black"),
                     axis.text = element_text(size = 24,color="black"))
resultado_cadeias24 <- as.array(logistic_fit24)
##########################################
############# Densidade Posteriori #######
##########################################
x11()
stan_dens(logistic_fit24)
mcmc_hist_by_chain(resultado_cadeias24,
                   pars = c("beta1","beta2"))+
  theme_bw() + theme(axis.title = element_text(size = 28,color="black"),
                     axis.text = element_text(size = 24,color="black"))
#######################################################################
############### Calculo com as estimativas do modelo logístico ########
#######################################################################
x = seq(-8,-1,0.1)
m324<-exp(CI_theta24[1,1]+CI_theta24[2,1]*x)/(1+exp(CI_theta24[1,1]+CI_theta24[2,1]*x))
#####################################################################
############################# outro jeito ###########################
#####################################################################
funcao.cristian<- function(theta, x){#NOVO
  beta1<- theta[1]
  beta2<- theta[2]
  exp(beta1+beta2*x)/(1+exp(beta1+beta2*x))
}

aux<- funcao.cristian(theta=c(CI_theta24[1,1],CI_theta24[2,1]),x)
summary(aux)
######################################################################
############################ Concentração Letal ######################
############################## logit #################################
######################################################################
x50.m324 = (-(CI_theta24[1,1]))/CI_theta24[2,1]
x50.m324
#####################################################################
############################# outro jeito ###########################
#####################################################################
funcao.BP<- function(theta){#NOVO
  beta1<- theta[1]
  beta2<- theta[2]
  -beta1/beta2
}
aux2<- funcao.BP(theta=c(CI_theta24[1,1],CI_theta24[2,1]))#NOVO
aux2
##########################################################################
##################################### Gráfico ############################
##########################################################################
par(cex=1.4)
plot(dados2$logdose,dados2$resposta/(dados2$resposta+dados2$C_resposta),pch=15,ylab="Proportion of bees killed",
     xlab="Concentrations",xlim=c(-8,-1),ylim=c(0,1),bty="n",col="black")
lines(x,m324, lty=4,lwd=2, col="black")
lines(c(x50.m324,x50.m324),c(0,0.50),lty=3)
lines(c(x50.m324,-10),c(0.50,0.50),lty=3)
legend(x50.m324,0.52,c(expression(paste(LC[50], "= -3.53"))),bty="n",cex=0.8)
legend(-8,0.95,c(expression(paste(M["24h"],"= logit[2.23 + 0.63xlog(concentrations)]"))),bty="n",cex=0.8)
##########################################################################
############################### ggplot2 ##################################
##########################################################################
teste1<- data.frame(x=x, y=m324)

df1 <- data.frame(x1 = x50.m324, 
                  x2 = x50.m324, 
                  y1 = 0.5,
                  y2 = -10)
fitstanglm24 <- stan_glm(cbind(resposta, C_resposta)~ logdose, 
                         family = binomial(link = "logit"), 
                         data = dados,
                         subset=periodo=="24h")

ajuste1<- data.frame(y1=dados$proporcao[dados$periodo=="24h"],
                     x1=dados$logdose[dados$periodo=="24h"],
                     y2=fitstanglm24$fitted.values,
                     x2=dados$logdose[dados$periodo=="24h"])

Graph24 = ggplot(ajuste1, aes(x1,y1))+
  geom_point(col="red")+
  xlim(c(-8,-1))+
  ylim(c(0,1))+
  xlab("log(Concentrations g a.i./L)")+
  ylab("Proportion of bees killed")+
  configuracao+
  geom_line(data=teste1,aes(x,m324),lty=4,col="black")+
  geom_point(data=df1, aes(x = x50.m324, 
                           y = y1), col="orange", size=3)+
  annotate("text", 
           x = -4.6, 
           y = 0.52, 
           label = expression(paste(LC[50], "= -3.53")),col="blue", size=5)+
  annotate("text", 
           x = -8, 
           y = 0.5, angle=90,
           label = c(expression(paste(M["24h"],"= logit[2.23 + 0.63xlog(concentrations)]"))),col="blue", size=5) +
  geom_point(aes(x2,y2),pch=20, col="gray", lwd=3)+
  geom_line(data=teste1,aes(x,m324),lty=4,col="black")+
  ggtitle("Experiment duration: 24h") + theme(plot.title = element_text(size = 22))
########################################################################
############################## 48h #####################################
################# Considerando concentrações puras #####################
########################################################################
dados3 = dados[c(6:10),]
n = length(dados3$logdose)
y = dados3$resposta
logistic_fit48 <- stan(model_code = modelo_logistico,
                       data = list(N = dim(dados3)[1], n = dados3$total,
                                   x = dados3$logdose, y = dados3$resposta),
                       chain = 3, iter = 11000, warmup = 1000,
                       thin = 10, refresh = 0)
parameters <- c(paste("beta", 1:2, sep = ""))
CI_theta48 <- summary(logistic_fit48, pars = parameters,
                      probs = c(0.025, 0.975))$summary
print(round(CI_theta48, 3))
##########################################
############# Intervalo HPD ##############
##########################################
cadeia_amostras48 <- extract(logistic_fit48)
betas148 <- cadeia_amostras48$beta1
boa.hpd(x=betas148,alpha=0.1)
##########################################
############# Intervalo HPD ##############
##########################################
betas248 <- cadeia_amostras48$beta2
boa.hpd(x=betas248,alpha=0.1)
##########################################
############# Cadeias ####################
##########################################
x11()
stan_trace(logistic_fit48)
par(cex=1.5,cex.lab=1.3)
traceplot(logistic_fit48,inc_warmup=T,ncol=1,col="black")+
  xlab("Iterações") +
  theme_bw() + theme(axis.title = element_text(size = 28,color="black"),
                     axis.text = element_text(size = 24,color="black"))
resultado_cadeias48 <- as.array(logistic_fit48)
##########################################
############# Densidade Posteriori #######
##########################################
x11()
stan_dens(logistic_fit48)
mcmc_hist_by_chain(resultado_cadeias48,
                   pars = c("beta1","beta2"))+
  theme_bw() + theme(axis.title = element_text(size = 28,color="black"),
                     axis.text = element_text(size = 24,color="black"))
#######################################################################
############### Calculo com as estimativas do modelo logístico ########
#######################################################################
x = seq(-8,-1,0.1)
m348<-exp(CI_theta48[1,1]+CI_theta48[2,1]*x)/(1+exp(CI_theta48[1,1]+CI_theta48[2,1]*x))
######################################################################
############################ Concentração Letal ######################
############################## logit #################################
######################################################################
x50.m348 = (-(CI_theta48[1,1]))/CI_theta48[2,1]
x50.m348
##########################################################################
##################################### Gráfico ############################
##########################################################################
par(cex=1.4)
plot(dados3$logdose,dados3$resposta/(dados3$resposta+dados3$C_resposta),pch=16,ylab="Proportion of bees killed",
     xlab="Concentrations",xlim=c(-8,-1),ylim=c(0,1),bty="n",col="black")
lines(x,m348, lty=1,lwd=2, col="black")
lines(c(x50.m348,x50.m348),c(0,0.50),lty=3)
lines(c(x50.m348,-10),c(0.50,0.50),lty=3)
legend(x50.m348,0.52,c(expression(paste(LC[50], "= -3.19"))),bty="n",cex=0.8)
legend(-8,0.95,c(expression(paste(M["48h"],"= logit[2.18 + 0.68xlog(concentrations)]"))),bty="n",cex=0.8)
##########################################################################
############################### ggplot2 ##################################
##########################################################################
teste2<- data.frame(x=x, y=m348)

df2 <- data.frame(x1 = x50.m348, 
                  x2 = x50.m348, 
                  y1 = 0.5,
                  y2 = -10)
fitstanglm48 <- stan_glm(cbind(resposta, C_resposta)~ logdose, 
                         family = binomial(link = "logit"), 
                         data = dados,
                         subset=periodo=="48h")

ajuste2<- data.frame(y1=dados$proporcao[dados$periodo=="48h"],
                     x1=dados$logdose[dados$periodo=="48h"],
                     y2=fitstanglm48$fitted.values,
                     x2=dados$logdose[dados$periodo=="48h"])
Graph48 = ggplot(ajuste2, aes(x1,y1))+
  geom_point(col="red")+
  xlim(c(-8,-1))+
  ylim(c(0,1))+
  xlab("log(Concentrations g a.i./L)")+
  ylab("Proportion of bees killed")+
  configuracao+
  geom_point(aes(x2,y2),pch=20, col="gray", lwd=3)+
  geom_line(data=teste2,aes(x,m348),lty=4,col="black") +
geom_point(data=df1, aes(x = x50.m348, y = y1), col="orange", size=3) +
  annotate("text", 
           x = -4.2, 
           y = 0.52, 
           label = c(expression(paste(LC[50], "= -3.19"))),
           col="blue", size = 5)+
  annotate("text", 
           x = -8, 
           y = 0.5, angle = 90,
           label = c(expression(paste(M["48h"],
    "= logit[2.18 + 0.68xlog(concentrations)]"))),col="blue", size = 5) +
  ggtitle("Experiment duration: 48h") + theme(plot.title = element_text(size = 22))
########################################################################
############################## 72h #####################################
################# Considerando concentrações puras #####################
########################################################################
dados4 = dados[c(11:15),]
n = length(dados4$logdose)
y = dados4$resposta
logistic_fit72 <- stan(model_code = modelo_logistico,
                       data = list(N = dim(dados4)[1], n = dados4$total,
                                   x = dados4$logdose, y = dados4$resposta),
                       chain = 3, iter = 11000, warmup = 1000,
                       thin = 10, refresh = 0)
parameters <- c(paste("beta", 1:2, sep = ""))
CI_theta72 <- summary(logistic_fit72, pars = parameters,
                      probs = c(0.025, 0.975))$summary
print(round(CI_theta72, 3))
##########################################
############# Intervalo HPD ##############
##########################################
cadeia_amostras72 <- extract(logistic_fit72)
betas172 <- cadeia_amostras72$beta1
boa.hpd(x=betas172,alpha=0.1)
##########################################
############# Intervalo HPD ##############
##########################################
betas272 <- cadeia_amostras72$beta2
boa.hpd(x=betas272,alpha=0.1)
##########################################
############# Cadeias ####################
##########################################
x11()
stan_trace(logistic_fit72)
par(cex=1.5,cex.lab=1.3)
traceplot(logistic_fit72,inc_warmup=T,ncol=1,col="black")+
  xlab("Iterações") +
  theme_bw() + theme(axis.title = element_text(size = 28,color="black"),
                     axis.text = element_text(size = 24,color="black"))
resultado_cadeias72 <- as.array(logistic_fit72)
##########################################
############# Densidade Posteriori #######
##########################################
x11()
stan_dens(logistic_fit72)
mcmc_hist_by_chain(resultado_cadeias72,
                   pars = c("beta1","beta2"))+
  theme_bw() + theme(axis.title = element_text(size = 28,color="black"),
                     axis.text = element_text(size = 24,color="black"))
#######################################################################
############### Calculo com as estimativas do modelo logístico ########
#######################################################################
x = seq(-8,-1,0.1)
m372<-exp(CI_theta72[1,1]+CI_theta72[2,1]*x)/(1+exp(CI_theta72[1,1]+CI_theta72[2,1]*x))
######################################################################
############################ Concentração Letal ######################
############################## logit #################################
######################################################################
x50.m372 = (-(CI_theta72[1,1]))/CI_theta72[2,1]
x50.m372
##########################################################################
##################################### Gráfico ############################
##########################################################################
par(cex=1.4)
plot(dados4$logdose,dados4$resposta/(dados4$resposta+dados4$C_resposta),pch=17,ylab="Proportion of bees killed",
     xlab="Concentrations",xlim=c(-8,-1),ylim=c(0,1),bty="n",col="black")
lines(x,m372, lty=2,lwd=2, col="black")
lines(c(x50.m372,x50.m372),c(0,0.50),lty=3)
lines(c(x50.m372,-10),c(0.50,0.50),lty=3)
legend(x50.m372,0.52,c(expression(paste(LC[50], "= -2.75"))),bty="n",cex=0.8)
legend(-8,0.95,c(expression(paste(M["72h"],"= logit[2.17 + 0.78xlog(concentrations)]"))),bty="n",cex=0.8)
##########################################################################
############################### ggplot2 ##################################
##########################################################################
teste3<- data.frame(x=x, y=m372)

df3 <- data.frame(x1 = x50.m372, 
                  x2 = x50.m372, 
                  y1 = 0.5,
                  y2 = -10)
fitstanglm72 <- stan_glm(cbind(resposta, C_resposta)~ logdose, 
                         family = binomial(link = "logit"), 
                         data = dados,
                         subset=periodo=="72h")

ajuste3<- data.frame(y1=dados$proporcao[dados$periodo=="72h"],
                     x1=dados$logdose[dados$periodo=="72h"],
                     y2=fitstanglm72$fitted.values,
                     x2=dados$logdose[dados$periodo=="72h"])
Graph72 = ggplot(ajuste3, aes(x1,y1))+
  geom_point(col="red")+
  xlim(c(-8,-1))+
  ylim(c(0,1))+
  xlab("log(Concentrations g a.i./L)")+
  ylab("Proportion of bees killed")+
  configuracao+
  geom_line(data=teste3,aes(x,m372),
            lty=4,col="black")+
  geom_point(data=df3, aes(x = x50.m372, 
                           y = y1), col="orange", size=3)+
  annotate("text", 
           x = -3.8, 
           y = 0.52, 
           label = c(expression(paste(LC[50], "= -2.75"))),
           col="blue", size=5)+
  annotate("text", 
           x = -8, 
           y = 0.5, angle=90,
           label = c(expression(paste(M["72h"],
  "= logit[2.17 + 0.78xlog(concentrations)]"))),col="blue", size=5) +
  geom_point(aes(x2,y2),pch=20, col="gray", lwd=3)+
  geom_line(data=teste3,aes(x,m372),lty=4,col="black")+
  ggtitle("Experiment duration: 72h") + theme(plot.title = element_text(size = 22))
##########################################################################
########################## Três Juntos ###################################
##########################################################################
x11()
par(mfrow=c(1,3),cex=1.2,cex.lab=1.1)
plot(dados2$logdose,dados2$resposta/(dados2$resposta+dados2$C_resposta),pch=16,ylab="Proportion of bees killed",
     xlab="log(Concentrations g.a.i/L)",xlim=c(-8,-1),ylim=c(0,1),bty="n",col="black", main="Experiment Duration: 24h")
lines(x,m324, lty=4,lwd=2, col="black")
lines(c(x50.m324,x50.m324),c(0,0.50),lty=3)
lines(c(x50.m324,-10),c(0.50,0.50),lty=3)
legend(-7.8,0.52,c(expression(paste(LC[50], "= -3.53"))),bty="n",cex=1.1)
plot(dados3$logdose,dados3$resposta/(dados3$resposta+dados3$C_resposta),pch=17,ylab="Proportion of bees killed",
     xlab="log(Concentrations g.a.i/L)",xlim=c(-8,-1),ylim=c(0,1),bty="n",col="black",main="Experiment Duration: 48h")
lines(x,m348, lty=1,lwd=2, col="black")
lines(c(x50.m348,x50.m348),c(0,0.50),lty=3)
lines(c(x50.m348,-10),c(0.50,0.50),lty=3)
legend(-7.5,0.52,c(expression(paste(LC[50], "= -3.19"))),bty="n",cex=1.1)
plot(dados4$logdose,dados4$resposta/(dados4$resposta+dados4$C_resposta),pch=15,ylab="Proportion of bees killed",
     xlab="log(Concentrations g.a.i/L)",xlim=c(-8,-1),ylim=c(0,1),bty="n",col="black",main="Experiment Duration: 72h")
lines(x,m372, lty=2,lwd=2, col="black")
lines(c(x50.m372,x50.m372),c(0,0.50),lty=3)
lines(c(x50.m372,-10),c(0.50,0.50),lty=3)
legend(-7,0.52,c(expression(paste(LC[50], "= -2.75"))),bty="n",cex=1.1)
##########################################################################
############################### ggplot2 ##################################
##########################################################################
x11()
grid.arrange(Graph24,Graph48,Graph72,ncol=3)