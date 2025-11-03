#
# Script para ejecutar el modelo Mack en datos de 
#la base de datos de reserva de pérdidas de CAS
#
# Este script utiliza la función "MackChainLadder"
#
rm(list = ls())      # despejar el espacio de trabajo"
#
# datas
#
insurer.data <- "C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/comauto_pos.csv"
#insurer.data <- "C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/ppauto_pos.csv"
#insurer.data <- "C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/wkcomp_pos.csv"
#insurer.data <- "C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/prodliab_pos.csv"
grpcode <- "353"
losstype <- "incloss"  #"incloss" si se incurrió en pérdida o "cpdloss" si se pagó la pérdida
a <- read.csv(insurer.data)
#
# Función para obtener los datos del triángulo del Anexo P dados el grupo y la línea de negocio
#
ins.line.data <- function(g.code){
  b <- subset(a,a$GRCODE==g.code)
  name <- b$GRNAME
  grpcode <- b$GRCODE
  w <- b$AccidentYear
  d <- b$DevelopmentLag
  cum_incloss <- b[,6]
  cum_pdloss <- b[,7]
  bulk_loss <- b[,8]
  dir_premium <- b[,9]
  ced_premium <- b[,10]
  net_premium <- b[,11]
  single <- b[,12]
  posted_reserve97 <- b[,13]
  # Obtenga pérdidas pagadas incrementales: suponga que los datos están ordenados por ay y lag
  inc_pdloss <- numeric(0)
  for (i in unique(w)){
    s <- (w==i)
    pl <- c(0,cum_pdloss[s])
    ndev <- length(pl)-1
    il <- rep(0,ndev)
    for (j in 1:ndev){            
      il[j] <- pl[j+1]-pl[j]
    }
    inc_pdloss <- c(inc_pdloss,il)
  }
  data.out <- data.frame(grpcode,w,d,net_premium,dir_premium,ced_premium,
                      cum_pdloss,cum_incloss,bulk_loss,inc_pdloss,single,posted_reserve97)
  return(data.out)
}
#
# Leer y agregar los datos de la aseguradora y
# configurar marcos de datos de entrenamiento y prueba
#
cdata <- ins.line.data(grpcode)
w <- cdata$w-1987
d <- cdata$d
premium <- cdata$net_premium
cpdloss <- cdata$cum_pdloss
incloss <- cdata$cum_incloss-cdata$bulk_loss
cpdloss <- pmax(1,cpdloss)
incloss <- pmax(1,incloss)
adata <- data.frame(grpcode,w,d,premium,cpdloss,incloss)
rdata <- subset(adata,(adata$w+adata$d)<12)
maxlevel <- log(2*max(rdata$premium))
numw <- length(unique(rdata$w))
if(losstype=="incloss") rloss=rdata$incloss else rloss=rdata$cpdloss
if(losstype=="incloss") aloss=adata$incloss else aloss=adata$cpdloss
#
# Ejecutar el modelo Mack usando ChainLadder
#
library(ChainLadder)
rtriangle <- as.triangle(rdata,origin="w",dev="d",value=losstype)     #######################
mcl <- MackChainLadder(rtriangle,est.sigma="Mack")
#
# Calcular la estadística de resumen para el modelo de Mack
#
Mack.Estimate <- round(summary(mcl)$ByOrigin[,3])
Mack.S.E <- round(summary(mcl)$ByOrigin[,5])
Mack.CV <- round(Mack.S.E/Mack.Estimate,4)
Mack.total.ult <- round(sum(summary(mcl)$ByOrigin[1:10,3]))
Mack.total.se <- round(summary(mcl)$Totals[5,1])
Mack.total.cv <- round(Mack.total.se/Mack.total.ult,4)
Mack.sig <- sqrt(log(1+Mack.total.cv^2))
Mack.mu <- log(Mack.total.ult)-Mack.sig^2/2
act10d <- subset(aloss,adata$d==10)
acttot <- sum(act10d)
pct.Mack <- round(plnorm(acttot,Mack.mu,Mack.sig)*100,2)
W <- c(1:10,"Total")
Mack.Estimate <- c(Mack.Estimate,Mack.total.ult)
Mack.S.E <- c(Mack.S.E,Mack.total.se)
Mack.CV <- c(Mack.CV,Mack.total.cv)
Actual <- c(act10d,acttot)
Percentile <- c(rep("",10),pct.Mack)
risk <- data.frame(W,Mack.Estimate,Mack.S.E,Mack.CV,Actual,Percentile)
print(risk)


