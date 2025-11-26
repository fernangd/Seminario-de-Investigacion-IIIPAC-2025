#
# Script para ejecutar el modelo Bayesiano Correlated Chain-Ladder (CCL)
#
rm(list = ls())      # despejar el espacio de trabajo
#
# Cargar librerías necesarias
#
library(runjags)
library(coda)
#
# datas
#
insurer.data <- "C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/comauto_pos.csv"
#insurer.data <- "C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/ppauto_pos.csv"
#insurer.data <- "C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/wkcomp_pos.csv"
#insurer.data <- "C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/prodliab_pos.csv"

grpcode <- "353"
losstype <- "incloss"  # Para pérdidas incurridas
a <- read.csv(insurer.data)
#
# Función para obtener los datos del triángulo
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
  
  inc_pdloss <- numeric(0)
  for (i in unique(w)){
    s=(w==i)
    pl=c(0,cum_pdloss[s])
    ndev=length(pl)-1
    il=rep(0,ndev)
    for (j in 1:ndev){            
      il[j]=pl[j+1]-pl[j]
    }
    inc_pdloss <- c(inc_pdloss,il)
  }
  data.out <- data.frame(grpcode,w,d,net_premium,dir_premium,ced_premium,
                      cum_pdloss,cum_incloss,bulk_loss,inc_pdloss,single,posted_reserve97)
  return(data.out)
}
#
# Leer y preparar los datos
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
numw <- length(unique(rdata$w))
rloss <- rdata$incloss
aloss <- adata$incloss

# Crear matriz de triángulo para JAGS
K <- 10  # Número de años de desarrollo
loss_triangle <- matrix(NA, nrow=K, ncol=K)
for(i in 1:nrow(rdata)){
  loss_triangle[rdata$w[i], rdata$d[i]]=log(rdata$incloss[i])
}

# Obtener primas por año de accidente
prem_ay <- tapply(premium, w, mean)

#
# Modelo JAGS para CCL (Correlated Chain-Ladder)
#
ccl_model <- "
model {
  # Priors
  logelr ~ dunif(-1, 0.5)
  
  # Prior para alpha (niveles de año de accidente)
  for(w in 1:K){
    alpha[w] ~ dnorm(log(prem[w]) + logelr, 0.1)  # precision = 1/variance = 0.1
  }
  
  # Prior para beta (factores de desarrollo)
  for(d in 1:(K-1)){
    beta[d] ~ dunif(-5, 5)
  }
  beta[K] <- 0  # Restricción para identificabilidad
  
  # Prior para rho (correlación entre años)
  rho ~ dunif(-1, 1)
  
  # Prior para sigma (volatilidad decreciente)
  for(d in 1:K){
    a[d] ~ dunif(0, 1)
  }
  for(d in 1:K){
    sigma[d] <- sum(a[d:K])
  }
  
  # Likelihood y predicción
  # Primera diagonal (d=1)
  for(w in 1:K){
    mu[w,1] <- alpha[w] + beta[1]
    logloss[w,1] ~ dnorm(mu[w,1], pow(sigma[1], -2))
  }
  
  # Diagonales restantes (d>1)
  for(w in 1:K){
    for(d in 2:K){
      mu[w,d] <- alpha[w] + beta[d] + rho * (logloss[w,d-1] - mu[w,d-1])
      logloss[w,d] ~ dnorm(mu[w,d], pow(sigma[d], -2))
    }
  }
  
  # Calcular pérdidas acumuladas finales
  for(w in 1:K){
    Closs[w] <- exp(logloss[w,K])
  }
}
"

# Preparar datos para JAGS
jags_data <- list(
  K=K,
  prem=prem_ay,
  logloss=loss_triangle
)

# Parámetros a monitorear
params <- c("alpha", "beta", "rho", "sigma", "Closs", "logelr")

# Ejecutar el modelo con runjags
cat("Ejecutando modelo CCL Bayesiano...\n")
set.seed(123)

# Fase adaptativa y burn-in
ccl_result <- run.jags(
  model <- ccl_model,
  data <- jags_data,
  monitor <- params,
  n.chains <- 4,
  adapt <- 1000,
  burnin <- 10000,
  sample <- 10000,
  thin <- 4,
  method <- "parallel"
)


# Extraer muestras posteriores
samples <- as.matrix(as.mcmc.list(ccl_result))

#
# Calcular estadísticas del modelo
#
CCL.Estimate <- numeric(K)
CCL.S.E <- numeric(K)
CCL.CV <- numeric(K)

for(w in 1:K){
  col_name <- paste0("Closs[", w, "]")
  CCL.Estimate[w] <- round(mean(samples[,col_name]))
  CCL.S.E[w] <- round(sd(samples[,col_name]))
  CCL.CV[w] <- round(CCL.S.E[w]/CCL.Estimate[w], 4)
}

# Totales
CCL.total.ult <- round(sum(CCL.Estimate))
CCL.total.se <- round(sqrt(sum(CCL.S.E^2)))
CCL.total.cv <- round(CCL.total.se/CCL.total.ult, 4)

# Obtener pérdidas actuales
act10d <- subset(aloss, adata$d==10)
acttot <- sum(act10d)

# Calcular percentil usando simulaciones
total_sims <- rowSums(samples[,grep("Closs", colnames(samples))])
pct.CCL <- round(mean(total_sims <= acttot)*100, 2)

# Crear tabla de resultados
W <- c(1:K, "Total")
CCL.Estimate <- c(CCL.Estimate, CCL.total.ult)
CCL.S.E <- c(CCL.S.E, CCL.total.se)
CCL.CV <- c(CCL.CV, CCL.total.cv)
Actual <- c(act10d, acttot)
Percentile <- c(rep("", K), pct.CCL)

risk=data.frame(W, CCL.Estimate, CCL.S.E, CCL.CV, Actual, Percentile)
cat("\n=== RESULTADOS MODELO CCL ===\n")
print(risk)