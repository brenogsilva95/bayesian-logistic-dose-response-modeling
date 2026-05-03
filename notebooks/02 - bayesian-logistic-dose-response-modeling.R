rm(list = ls())
library(dplyr)
library(rstanarm)
library(ggplot2)
dados<- read.table("df_lc50.csv", header = T, sep=";", dec = ",")
dados$periodo <-  as.factor(dados$periodo)
dados <- dados %>% mutate(proporcao =  (dados$resposta)/60)
dados <- dados %>% mutate(logdose = log(dados$concentracao))
dados<- mutate(dados, 
               C_resposta=60-resposta)
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
#######################################################################
########################## Res?duos 24h ###############################
#######################################################################
fitstanglm24 <- stan_glm(cbind(resposta, C_resposta)~ logdose, 
                         family = binomial(link = "logit"), 
                         data = dados,
                         subset=periodo=="24h")#NOVO
summary(fitstanglm24)

pi.estimado<- fitstanglm24$fitted.values#estimacao prob.
eta<- fitstanglm24$linear.predictors#estimacao eta
pi.real<- dados$resposta[dados$periodo=="24h"]/dados$total[dados$periodo=="24h"]
residuoPuro<- (pi.real-pi.estimado)/sd(pi.real-pi.estimado)
data.frame(pi.real, pi.estimado, ei=pi.real-pi.estimado)
teste4 <- data.frame(x=pi.estimado,y=residuoPuro)
##########################################################################
############################### ggplot2 ##################################
##########################################################################
ggplot(teste4, aes(x,y))+geom_point(size=3)+configuracao+ ylim(c(-3,3))+xlim(c(0,1))+
   geom_hline(yintercept = c(-3,0,3), lty=2, col="blue")+
   labs(x = "Fitted Probability", y = "Residuals") 

m1 = qqnorm(residuoPuro, pch=20, col="red",ylim=c(-3,3), main="",lwd=10)
mm1<- data.frame(x=m1$x,y=m1$y) 
ggplot(mm1, aes(x,y))+geom_point(size=3, 
col="red")+configuracao+ ylim(c(-3,3))+xlim(c(-2,2))+
   ylab("Sample Quantiles")+xlab(" Theoretical Quantiles")+geom_abline(data = NULL, 
intercept = 0, slope=1, col="blue")
##########################################################################
############################### normal ###################################
##########################################################################
x11()
par(cex=1.4,cex.lab=1.4)
plot(pi.estimado,residuoPuro,
     ylim=c(-3,3),pch=20, ylab="Residuals", xlab="Fitted Probability",lwd=10)
abline(h=c(-3,0,3), lty=2, col="blue")

qqnorm(residuoPuro, pch=20, col="red",ylim=c(-3,3), main="",lwd=10)
qqline(residuoPuro, col="blue")
#######################################################################
########################## Res?duos 48h ###############################
#######################################################################
fitstanglm48 <- stan_glm(cbind(resposta, C_resposta)~ logdose, 
                         family = binomial(link = "logit"), 
                         data = dados,
                         subset=periodo=="48h")#NOVO
summary(fitstanglm48)

pi.estimado<- fitstanglm48$fitted.values#estimacao prob.
eta<- fitstanglm48$linear.predictors#estimacao eta
pi.real<- dados$resposta[dados$periodo=="48h"]/dados$total[dados$periodo=="48h"]
residuoPuro<- (pi.real-pi.estimado)/sd(pi.real-pi.estimado)
data.frame(pi.real, pi.estimado, ei=pi.real-pi.estimado)
teste5 <- data.frame(x=pi.estimado,y=residuoPuro)
##########################################################################
############################### ggplot2 ##################################
##########################################################################
ggplot(teste5, aes(x,y))+geom_point(size=3)+configuracao+ ylim(c(-3,3))+xlim(c(0,1))+
   geom_hline(yintercept = c(-3,0,3), lty=2, col="blue")+
   labs(x = "Fitted Probability", y = "Residuals") 

m2 = qqnorm(residuoPuro, pch=20, col="red",ylim=c(-3,3), main="",lwd=10)
mm2<- data.frame(x=m2$x,y=m2$y) 
ggplot(mm2, aes(x,y))+geom_point(size=3, 
                                 col="red")+configuracao+ ylim(c(-3,3))+xlim(c(-2,2))+
   ylab("Sample Quantiles")+xlab(" Theoretical Quantiles")+geom_abline(data = NULL, 
                                                                       intercept = 0, slope=1, col="blue")
##########################################################################
############################### normal ###################################
##########################################################################
x11()
par(cex=1.4,cex.lab=1.4)
plot(pi.estimado,residuoPuro,
     ylim=c(-3,3),pch=20, ylab="Residuals", xlab="Fitted Probability",lwd=10)
abline(h=c(-3,0,3), lty=2, col="blue")

qqnorm(residuoPuro, pch=20, col="red",ylim=c(-3,3), main="",lwd=10)
qqline(residuoPuro, col="blue")

#######################################################################
########################## Res?duos 72h ###############################
#######################################################################
fitstanglm72 <- stan_glm(cbind(resposta, C_resposta)~ logdose, 
                         family = binomial(link = "logit"), 
                         data = dados,
                         subset=periodo=="72h")#NOVO
summary(fitstanglm72)

pi.estimado<- fitstanglm72$fitted.values#estimacao prob.
eta<- fitstanglm72$linear.predictors#estimacao eta
pi.real<- dados$resposta[dados$periodo=="72h"]/dados$total[dados$periodo=="72h"]
residuoPuro<- (pi.real-pi.estimado)/sd(pi.real-pi.estimado)
data.frame(pi.real, pi.estimado, ei=pi.real-pi.estimado)
teste6 <- data.frame(x=pi.estimado,y=residuoPuro)
##########################################################################
############################### ggplot2 ##################################
##########################################################################
ggplot(teste6, aes(x,y))+geom_point(size=3)+configuracao+ ylim(c(-3,3))+xlim(c(0,1))+
   geom_hline(yintercept = c(-3,0,3), lty=2, col="blue")+
   labs(x = "Fitted Probability", y = "Residuals") 

m3 = qqnorm(residuoPuro, pch=20, col="red",ylim=c(-3,3), main="",lwd=10)
mm3<- data.frame(x=m3$x,y=m3$y) 
ggplot(mm3, aes(x,y))+geom_point(size=3, 
                                 col="red")+configuracao+ ylim(c(-3,3))+xlim(c(-2,2))+
   ylab("Sample Quantiles")+xlab(" Theoretical Quantiles")+geom_abline(data = NULL, 
                                                                       intercept = 0, slope=1, col="blue")
##########################################################################
############################### normal ###################################
##########################################################################
x11()
par(cex=1.4,cex.lab=1.4)
plot(pi.estimado,residuoPuro,
     ylim=c(-3,3),pch=20, ylab="Residuals", xlab="Fitted Probability",lwd=10)
abline(h=c(-3,0,3), lty=2, col="blue")

qqnorm(residuoPuro, pch=20, col="red",ylim=c(-3,3), main="",lwd=10)
qqline(residuoPuro, col="blue")

#######################################################################
########################## Curva Ajustada #############################
############################### 24h ###################################
#######################################################################
plot(dados$proporcao[dados$periodo=="24h"]~dados$logdose[dados$periodo=="24h"], 
     pch=20, col="red", lwd=3, xlim=c(-10,-1),
     ylim=c(0,1),xlab="log(Concentrations g.a.i/L)",
     ylab="Proportion of bees killed")
points(fitstanglm24$fitted.values~dados$logdose[dados$periodo=="24h"], 
     pch=20, col="gray", lwd=3)
curve(plogis(coef(fitstanglm24)[1] + coef(fitstanglm24)[2]*x),-10,-1,
      add=TRUE, col="black",lty=2)
legend("topleft",c("Real Values","Adjusted Values"), 
       col=c("red","gray"),pch=20)
#######################################################################
########################## Curva Ajustada #############################
############################### 48h ###################################
#######################################################################
plot(dados$proporcao[dados$periodo=="48h"]~dados$logdose[dados$periodo=="48h"], 
     pch=20, col="red", lwd=3, xlim=c(-10,-1),
     ylim=c(0,1),xlab="log(Concentrations g.a.i/L)",
     ylab="Proportion of bees killed")
points(fitstanglm48$fitted.values~dados$logdose[dados$periodo=="48h"], 
       pch=20, col="gray", lwd=3)
curve(plogis(coef(fitstanglm48)[1] + coef(fitstanglm48)[2]*x),-10,-1,
      add=TRUE, col="black",lty=2)
legend("topleft",c("Real Values","Adjusted Values"), 
       col=c("red","gray"),pch=20)
#######################################################################
########################## Curva Ajustada #############################
############################### 72h ###################################
#######################################################################
plot(dados$proporcao[dados$periodo=="72h"]~dados$logdose[dados$periodo=="72h"], 
     pch=20, col="black", lwd=3, xlim=c(-8,-1),
     ylim=c(0,1),xlab="log(Concentrations g.a.i/L)",
     ylab="Proportion of bees killed")
lines(c(-2.75,-2.75),c(0,0.50),lty=3)
lines(c(-2.75,-8),c(0.50,0.50),lty=3)
legend(-7,0.52,c(expression(paste(LC[50], "= -2.75"))),bty="n",cex=1.1)
points(fitstanglm72$fitted.values~dados$logdose[dados$periodo=="72h"], 
       pch=20, col="red", lwd=3)
curve(plogis(coef(fitstanglm72)[1] + coef(fitstanglm72)[2]*x),-10,-1,
      add=TRUE, col="black",lty=2)
legend("topleft",c("Real Values","Adjusted Values"), 
       col=c("black","red"),pch=20)
##########################################################################
########################## Tr?s Juntos ###################################
##########################################################################
x11()
par(mfrow=c(1,3),cex=1.2,cex.lab=1.1)
plot(dados$proporcao[dados$periodo=="24h"]~dados$logdose[dados$periodo=="24h"], 
     pch=16, main="Experiment Duration: 24h", col="black", lwd=3, xlim=c(-8,-1),
     ylim=c(0,1),xlab="log(Concentrations g.a.i/L)",
     ylab="Proportion of bees killed")
lines(c(-3.53,-3.53),c(0,0.50),lty=3)
lines(c(-3.53,-8),c(0.50,0.50),lty=3)
legend(-7.8,0.52,c(expression(paste(LC[50], "= -3.53"))),bty="n",cex=1.1)
points(fitstanglm24$fitted.values~dados$logdose[dados$periodo=="24h"], 
       pch=16, col="red", lwd=3)
curve(plogis(coef(fitstanglm48)[1] + coef(fitstanglm48)[2]*x),-10,-1,
      add=TRUE, col="black",lty=2)
legend("topleft",c("Real Values","Adjusted Values"), 
       col=c("black","red"),pch=16)
plot(dados$proporcao[dados$periodo=="48h"]~dados$logdose[dados$periodo=="48h"], 
     pch=17, main="Experiment Duration: 48h", col="black", lwd=3, xlim=c(-8,-1),
     ylim=c(0,1),xlab="log(Concentrations g.a.i/L)",
     ylab="Proportion of bees killed")
lines(c(-3.19,-3.19),c(0,0.50),lty=3)
lines(c(-3.19,-8),c(0.50,0.50),lty=3)
legend(-7.5,0.52,c(expression(paste(LC[50], "= -3.19"))),bty="n",cex=1.1)
points(fitstanglm48$fitted.values~dados$logdose[dados$periodo=="48h"], 
       pch=17, col="red", lwd=3)
curve(plogis(coef(fitstanglm48)[1] + coef(fitstanglm48)[2]*x),-10,-1,
      add=TRUE, col="black",lty=2)
legend("topleft",c("Real Values","Adjusted Values"), 
       col=c("black","red"),pch=17)
plot(dados$proporcao[dados$periodo=="72h"]~dados$logdose[dados$periodo=="72h"], 
     pch=15, main="Experiment Duration: 72h", col="black", lwd=3, xlim=c(-8,-1),
     ylim=c(0,1),xlab="log(Concentrations g.a.i/L)",
     ylab="Proportion of bees killed")
lines(c(-2.75,-2.75),c(0,0.50),lty=3)
lines(c(-2.75,-8),c(0.50,0.50),lty=3)
legend(-7,0.52,c(expression(paste(LC[50], "= -2.75"))),bty="n",cex=1.1)
points(fitstanglm72$fitted.values~dados$logdose[dados$periodo=="72h"], 
       pch=15, col="red", lwd=3)
curve(plogis(coef(fitstanglm72)[1] + coef(fitstanglm72)[2]*x),-10,-1,
      add=TRUE, col="black",lty=2)
legend("topleft",c("Real Values","Adjusted Values"), 
       col=c("black","red"),pch=15)
