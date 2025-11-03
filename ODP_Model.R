#
#Script para ejecutar el modelo Bootstrap Chain Ladder en datos de 
#la base de datos de reserva de pérdidas CAS
#
# Este script utiliza la función "BootChainLadder"
#
rm(list = ls())      # despejar el espacio de trabajo"
#
# datas
#
insurer.data <- "C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/comauto_pos.csv"
#insurer.data <- "C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/ppauto_pos.csv"
#insurer.data <- "C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/wkcomp_pos.csv"
#insurer.data <- "C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/prodliab_pos.csv"
grpcode="353"
losstype <- "cpdloss"
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
cdata <- ins.line.data(grpcode)
w <- cdata$w-1987
d <- cdata$d
premium <- cdata$net_premium
cpdloss <- cdata$cum_pdloss
incloss <- cdata$cum_incloss-cdata$bulk_loss
adata <- data.frame(grpcode,w,d,premium,cpdloss,incloss)
rdata <- subset(adata,(adata$w+adata$d)<12)
maxlevel <- log(2*max(rdata$premium))
numw <- length(unique(rdata$w))
if(losstype=="incloss") rloss=rdata$incloss else rloss=rdata$cpdloss
if(losstype=="incloss") aloss=adata$incloss else aloss=adata$cpdloss
#
# Ejecutar el modelo Chainladder de Bootstrap usando ChainLadder
#
library(ChainLadder)
rtriangle <- as.triangle(rdata,origin="w",dev="d",value=losstype)     #######################
set.seed(12345)
numsim <- 10000
odp <- BootChainLadder(rtriangle, R = numsim, process.distr="od.pois")
#
# Calcular estadísticas de resumen para el modelo ODP
#
ODP.Estimate <- round(summary(odp)$ByOrigin[,2])
ODP.S.E <- round(summary(odp)$ByOrigin[,4])
ODP.CV <- round(ODP.S.E/ODP.Estimate,4)
ODP.total.ult <- round(summary(odp)$Totals[2,1])
ODP.total.se <- round(summary(odp)$Totals[4,1])
ODP.total.cv <- round(ODP.total.se/ODP.total.ult,4)
act10d <- subset(aloss,adata$d==10)
acttot <- sum(act10d)
le.act <- (odp$IBNR.Totals+summary(odp)$Totals[1,1])<=acttot
pct.ODP <- round(sum(le.act)/numsim*100,2)
W <- c(1:10,"Total")
ODP.Estimate <- c(ODP.Estimate,ODP.total.ult)
ODP.S.E <- c(ODP.S.E,ODP.total.se)
ODP.CV <- c(ODP.CV,ODP.total.cv)
Actual <- c(act10d,acttot)
Percentile <- c(rep("",10),pct.ODP)
risk <- data.frame(W,ODP.Estimate,ODP.S.E,ODP.CV,Actual,Percentile)
print(risk)
