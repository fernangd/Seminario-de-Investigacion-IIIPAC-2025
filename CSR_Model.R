#
# Script to run the CSR Model on data from the CAS Loss Reserve Database
# by Glenn Meyers - VERSIÓN CORREGIDA
#
rm(list = ls())      # clear workspace"
#
# user inputs
#
insurer.data="C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas de automóviles/comauto_pos.csv"
grpcode="353"
losstype="cpdloss"  #"incloss" if incurred loss or "cpdloss" if paid loss
outfile="outCSR.csv"

#
# JAGS script (EXACTO al segundo código)
#
csr_model <- "
model {
  for (i in 1:length(w)){
    mu[i] <- alpha[w[i]] + beta[d[i]] * pow((1 - gamma), (w[i] - 1))
    logloss[i] ~ dnorm(mu[i], 1/sig2[i])
  }
  
  # set up sig2
  for (i in 1:length(w)){
    sig2[i] <- sigd2[d[i]]
  }
  for (j in 1:10){
    sigd2[j] <- sum(a[j:10])
  }
  for (k in 1:10){
    a[k] ~ dunif(0.000001, 1)
  }
  
  # specify priors
  for (i in 1:numlev){
    alpha[i] ~ dnorm(log(premium[i]) + logelr, 0.1)
  }
  logelr ~ dunif(-1.5, 0.5)
  
  for (i in 1:9){
    beta[i] ~ dunif(-5, 5)
  }
  beta[10] <- 0 
  
  gamma ~ dnorm(0, 400)
}
"

#
# get data
#
a <- read.csv(insurer.data)

#
# function to get Schedule P triangle data given ins group and line of business
# (EXACTA al segundo código)
#
ins.line.data <- function(g.code) {
  b <- subset(a, a$GRCODE == g.code)
  if (nrow(b) == 0) {
    return(NULL)
  }
  
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
  
  # get incremental paid losses
  inc_pdloss <- numeric(0)
  for (i in unique(w)) {
    s <- (w == i)
    pl <- c(0, cum_pdloss[s])
    ndev <- length(pl) - 1
    il <- rep(0, ndev)
    for (j in 1:ndev) {            
      il[j] <- pl[j+1] - pl[j]
    }
    inc_pdloss <- c(inc_pdloss, il)
  }
  
  data.out <- data.frame(grpcode, w, d, net_premium, dir_premium, ced_premium,
                         cum_pdloss, cum_incloss, bulk_loss, inc_pdloss, single, posted_reserve97)
  return(data.out)
}

#
# read and aggregate the insurer data and 
# set up training and test data frames
# (EXACTO al segundo código)
#
cdata <- ins.line.data(grpcode)

# Preparar datos (EXACTO al segundo código)
w <- cdata$w - 1987
d <- cdata$d

# Ordenar los datos (EXACTO al segundo código)
o1 <- 100 * d + w
o <- order(o1)
w <- w[o]
d <- d[o]
premium <- cdata$net_premium[o]
cpdloss <- cdata$cum_pdloss[o]
cpdloss <- pmax(cpdloss, 1)
incloss <- cdata$cum_incloss[o] - cdata$bulk_loss[o]
incloss <- pmax(incloss, 1)

adata <- data.frame(grpcode = cdata$grpcode[1], w, d, premium, cpdloss, incloss)
rdata <- subset(adata, (adata$w + adata$d) <= 11)

# Verificar que hay suficientes datos
if (nrow(rdata) < 10) {
  stop("Datos insuficientes para el group code:", grpcode)
}

numw <- length(unique(rdata$w))
if(losstype == "incloss") {
  rloss <- rdata$incloss
  aloss <- adata$incloss
} else {
  rloss <- rdata$cpdloss
  aloss <- adata$cpdloss
}

# Preparar datos para JAGS (EXACTO al segundo código)
if(length(premium) >= 10) {
  premium_data <- premium[1:10]
} else {
  premium_data <- rep(mean(premium, na.rm = TRUE), 10)
}

jags_data <- list(
  premium = premium_data,
  logloss = log(rloss),
  numlev = numw,
  w = rdata$w,
  d = rdata$d
)

#
# run the model (EXACTO al segundo código)
#
library(runjags)
library(coda)

# Parámetros a monitorear
params <- c("alpha", "beta", "sigd2", "gamma")

# Configuración EXACTA del segundo código
inits1 <- list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 12341)
inits2 <- list(.RNG.name = "base::Marsaglia-Multicarry", .RNG.seed = 12342)

tryCatch({
  csr_result <- run.jags(
    model = csr_model,
    data = jags_data,
    monitor = params,
    n.chains = 2,
    adapt = 1000,
    burnin = 2000,
    sample = 1000,
    thin = 2,
    method = "parallel",
    inits = list(inits1, inits2),
    silent.jags = TRUE,
    modules = "glm"
  )
}, error = function(e) {
  stop("Error en JAGS: ", e$message)
})

print(csr_result$timetaken)

# Extraer muestras posteriores (EXACTO al segundo código)
samples <- as.matrix(as.mcmc.list(csr_result))

# Procesar resultados (EXACTO al segundo código)
alpha <- samples[, 1:10]
beta <- cbind(samples[, 11:19], rep(0, nrow(samples)))
gamma <- samples[, 20]
sigd2 <- samples[, 21:30]

# Simular pérdidas finales (EXACTO al segundo código)
set.seed(12345)
n_sim <- nrow(samples)
at.wd10 <- matrix(0, n_sim, 10)

# Inicializar con valores conocidos (EXACTO al segundo código)
for (w in 1:10) {
  max_dev_for_w <- max(rdata$d[rdata$w == w])
  last_known_index <- which(rdata$w == w & rdata$d == max_dev_for_w)[1]
  
  if(!is.na(last_known_index) && is.finite(rloss[last_known_index])) {
    at.wd10[, w] <- rep(rloss[last_known_index], n_sim)
  } else {
    at.wd10[, w] <- rep(1000, n_sim)
  }
}

# Proyectar hacia el desarrollo 10 (EXACTO al segundo código)
for (w in 1:10) {
  for (i in 1:n_sim) {
    if(is.finite(alpha[i, w]) && is.finite(beta[i, 10]) && is.finite(gamma[i])) {
      mu_val <- alpha[i, w] + beta[i, 10] * (1 - gamma[i])^(w-1)
      
      current_max_dev <- max(rdata$d[rdata$w == w])
      if (current_max_dev < 10 && is.finite(mu_val) && is.finite(sigd2[i, 10])) {
        sd_val <- sqrt(max(sigd2[i, 10], 1e-6))
        at.wd10[i, w] <- rlnorm(1, mu_val, sd_val)
      }
    }
  }
}

# Calcular estadísticas (EXACTO al segundo código)
Pred.CSR <- rowSums(at.wd10)
CSR.total.ult <- round(mean(Pred.CSR))
CSR.total.se <- round(sd(Pred.CSR))

# Obtener pérdidas actuales (EXACTO al segundo código)
actual_final <- numeric(10)
for(w in 1:10) {
  max_dev_w <- max(adata$d[adata$w == w])
  actual_val <- subset(aloss, adata$w == w & adata$d == max_dev_w)
  if(length(actual_val) > 0) {
    actual_final[w] <- actual_val
  } else {
    actual_final[w] <- 0
  }
}
acttot <- sum(actual_final)

# Calcular percentil (EXACTO al segundo código)
pct.CSR <- round(mean(Pred.CSR <= acttot) * 100, 2)

# Retornar resultados (EXACTO al segundo código)
results <- data.frame(
  Line = "CA",
  GroupCode = grpcode,
  CSR_Estimate = CSR.total.ult,
  CSR_Standard_Dev = CSR.total.se,
  Outcome_Percentile = pct.CSR,
  Outcome = acttot
)

# Mostrar resultados
cat("\n=== RESULTADO CSR CORREGIDO ===\n")
print(results)

# CALCULAR TABLA RISK (como en el código original)
ms.wd10 <- apply(at.wd10, 2, mean)
ss.wd10 <- apply(at.wd10, 2, sd)
Pred.CSR <- rowSums(at.wd10)
ms.td10 <- mean(Pred.CSR)
ss.td10 <- sd(Pred.CSR)
CSR.Estimate <- round(ms.wd10)
CSR.S.E. <- round(ss.wd10)
CSR.CV <- round(CSR.S.E. / CSR.Estimate, 4)

# Obtener primas por año
Premium_ay <- numeric(10)
for(w in 1:10) {
  premium_val <- subset(rdata$premium, rdata$w == w & rdata$d == 1)
  if(length(premium_val) > 0) {
    Premium_ay[w] <- premium_val[1]
  } else {
    Premium_ay[w] <- NA
  }
}

# Obtener outcomes reales para mostrar
Outcome_ay <- numeric(10)
for(w in 1:10) {
  max_dev_w <- max(adata$d[adata$w == w])
  outcome_val <- subset(aloss, adata$w == w & adata$d == max_dev_w)
  if(length(outcome_val) > 0) {
    Outcome_ay[w] <- outcome_val
  } else {
    Outcome_ay[w] <- NA
  }
}

# Crear tabla risk
W <- c(1:10, "Total")
CSR.Estimate <- c(CSR.Estimate, round(ms.td10))
CSR.S.E. <- c(CSR.S.E., round(ss.td10))
CSR.CV <- c(CSR.CV, round(ss.td10/ms.td10, 4))
Premium <- c(Premium_ay, sum(Premium_ay, na.rm = TRUE))
Outcome <- c(Outcome_ay, sum(Outcome_ay, na.rm = TRUE))
CSR.Pct <- c(rep(NA, 10), pct.CSR)

risk <- data.frame(W, Premium, CSR.Estimate, CSR.S.E., CSR.CV, Outcome, CSR.Pct)

# Mostrar tabla risk
cat("\n=== TABLA RISK ===\n")
print(risk)
