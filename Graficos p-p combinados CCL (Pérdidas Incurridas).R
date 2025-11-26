library(readr)

# Data de aseguradoras (CA, PA, WC, OL)
CCL_Incurrida <- read_csv("C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas para los triangulos p-p/Modelos Bayesianos/Pédidas Incurridas/CCL_Incurrida_gráficos.csv")

# ========== CONFIGURAR LAYOUT DE 3 FILAS X 2 COLUMNAS ==========
windows(width = 12, height = 14)  # En Windows
# quartz(width = 12, height = 14)  # En Mac
# x11(width = 12, height = 14)     # En Linux

par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))

# Definir constantes una sola vez
alpha <- 0.05
c_alpha <- 1.36

# ========== FILA 1: CA Y PA ==========

# CA - CCL Incurridas
predicted_percentiles <- CCL_Incurrida[1:50,]$Outcome_Percentile
n <- length(predicted_percentiles)
predicted_sorted <- sort(predicted_percentiles/100)
expected_ecdf <- (1:n) / n
ks_d_manual <- max(abs(predicted_sorted - expected_ecdf)) * 100
crit_val <- (c_alpha / sqrt(n)) * 100
expected_theoretical <- ppoints(n) * 100
predicted_sorted_plot <- sort(predicted_percentiles)

plot(expected_theoretical, predicted_sorted_plot, 
     type = "p", pch = 19, col = "black",
     xlim = c(0, 100), ylim = c(0, 100),
     xlab = "Expected", ylab = "Predicted",
     main = "CA - CCL Incurridas",
     cex.main = 1.2, cex.lab = 1.0, cex.axis = 0.9,
     frame.plot = FALSE)
abline(a = 0, b = 1, col = "black", lty = 1, lwd = 2)
band_width <- crit_val
abline(a = band_width, b = 1, col = "black", lty = 2, lwd = 1)
abline(a = -band_width, b = 1, col = "black", lty = 2, lwd = 1)
sig <- ifelse(ks_d_manual > crit_val, " *", "")
text(5, 95, paste0("KS D = ", round(ks_d_manual, 1), sig), 
     cex = 1.0, pos = 4, font = 2)
text(65, 5, paste0("Crit. Val. = ", round(crit_val, 1)), 
     cex = 1.0, pos = 4, font = 2)
grid(col = "gray80", lty = 3)
box()

# PA - CCL Incurridas
predicted_percentiles <- CCL_Incurrida[51:100,]$Outcome_Percentile
n <- length(predicted_percentiles)
predicted_sorted <- sort(predicted_percentiles/100)
expected_ecdf <- (1:n) / n
ks_d_manual <- max(abs(predicted_sorted - expected_ecdf)) * 100
crit_val <- (c_alpha / sqrt(n)) * 100
expected_theoretical <- ppoints(n) * 100
predicted_sorted_plot <- sort(predicted_percentiles)

plot(expected_theoretical, predicted_sorted_plot, 
     type = "p", pch = 19, col = "black",
     xlim = c(0, 100), ylim = c(0, 100),
     xlab = "Expected", ylab = "Predicted",
     main = "PA - CCL Incurridas",
     cex.main = 1.2, cex.lab = 1.0, cex.axis = 0.9,
     frame.plot = FALSE)
abline(a = 0, b = 1, col = "black", lty = 1, lwd = 2)
band_width <- crit_val
abline(a = band_width, b = 1, col = "black", lty = 2, lwd = 1)
abline(a = -band_width, b = 1, col = "black", lty = 2, lwd = 1)
sig <- ifelse(ks_d_manual > crit_val, " *", "")
text(5, 95, paste0("KS D = ", round(ks_d_manual, 1), sig), 
     cex = 1.0, pos = 4, font = 2)
text(65, 5, paste0("Crit. Val. = ", round(crit_val, 1)), 
     cex = 1.0, pos = 4, font = 2)
grid(col = "gray80", lty = 3)
box()

# ========== FILA 2: WC Y OL ==========

# WC - CCL Incurridas
predicted_percentiles <- CCL_Incurrida[101:150,]$Outcome_Percentile
n <- length(predicted_percentiles)
predicted_sorted <- sort(predicted_percentiles/100)
expected_ecdf <- (1:n) / n
ks_d_manual <- max(abs(predicted_sorted - expected_ecdf)) * 100
crit_val <- (c_alpha / sqrt(n)) * 100
expected_theoretical <- ppoints(n) * 100
predicted_sorted_plot <- sort(predicted_percentiles)

plot(expected_theoretical, predicted_sorted_plot, 
     type = "p", pch = 19, col = "black",
     xlim = c(0, 100), ylim = c(0, 100),
     xlab = "Expected", ylab = "Predicted",
     main = "WC - CCL Incurridas",
     cex.main = 1.2, cex.lab = 1.0, cex.axis = 0.9,
     frame.plot = FALSE)
abline(a = 0, b = 1, col = "black", lty = 1, lwd = 2)
band_width <- crit_val
abline(a = band_width, b = 1, col = "black", lty = 2, lwd = 1)
abline(a = -band_width, b = 1, col = "black", lty = 2, lwd = 1)
sig <- ifelse(ks_d_manual > crit_val, " *", "")
text(5, 95, paste0("KS D = ", round(ks_d_manual, 1), sig), 
     cex = 1.0, pos = 4, font = 2)
text(65, 5, paste0("Crit. Val. = ", round(crit_val, 1)), 
     cex = 1.0, pos = 4, font = 2)
grid(col = "gray80", lty = 3)
box()

# OL - CCL Incurridas
predicted_percentiles <- CCL_Incurrida[151:200,]$Outcome_Percentile
n <- length(predicted_percentiles)
predicted_sorted <- sort(predicted_percentiles/100)
expected_ecdf <- (1:n) / n
ks_d_manual <- max(abs(predicted_sorted - expected_ecdf)) * 100
crit_val <- (c_alpha / sqrt(n)) * 100
expected_theoretical <- ppoints(n) * 100
predicted_sorted_plot <- sort(predicted_percentiles)

plot(expected_theoretical, predicted_sorted_plot, 
     type = "p", pch = 19, col = "black",
     xlim = c(0, 100), ylim = c(0, 100),
     xlab = "Expected", ylab = "Predicted",
     main = "OL - CCL Incurridas",
     cex.main = 1.2, cex.lab = 1.0, cex.axis = 0.9,
     frame.plot = FALSE)
abline(a = 0, b = 1, col = "black", lty = 1, lwd = 2)
band_width <- crit_val
abline(a = band_width, b = 1, col = "black", lty = 2, lwd = 1)
abline(a = -band_width, b = 1, col = "black", lty = 2, lwd = 1)
sig <- ifelse(ks_d_manual > crit_val, " *", "")
text(5, 95, paste0("KS D = ", round(ks_d_manual, 1), sig), 
     cex = 1.0, pos = 4, font = 2)
text(65, 5, paste0("Crit. Val. = ", round(crit_val, 1)), 
     cex = 1.0, pos = 4, font = 2)
grid(col = "gray80", lty = 3)
box()

# ========== FILA 3: HISTOGRAMA Y GRÁFICO P-P COMBINADO ==========

# Histograma CA+PA+WC+OL
predicted_percentiles <- CCL_Incurrida$Outcome_Percentile

# Ajustar breaks para incluir todos los valores
breaks_seq <- seq(0, 100, by = 10)
# Asegurar que breaks cubra el rango completo de los datos
breaks_seq[1] <- min(0, floor(min(predicted_percentiles)))
breaks_seq[length(breaks_seq)] <- max(100, ceiling(max(predicted_percentiles)))

hist(predicted_percentiles, 
     breaks = breaks_seq,
     main = "CA+PA+WC+OL",
     xlab = "",
     ylab = "Frequency",
     col = "white",
     border = "black",
     xlim = c(0, 100),
     cex.main = 1.2,
     cex.lab = 1.0,
     cex.axis = 0.9)

# Gráfico P-P CA+PA+WC+OL
n <- length(predicted_percentiles)
predicted_sorted <- sort(predicted_percentiles/100)
expected_ecdf <- (1:n) / n
ks_d_manual <- max(abs(predicted_sorted - expected_ecdf)) * 100
crit_val <- (c_alpha / sqrt(n)) * 100
expected_theoretical <- ppoints(n) * 100
predicted_sorted_plot <- sort(predicted_percentiles)

plot(expected_theoretical, predicted_sorted_plot, 
     type = "p", pch = 19, col = "black",
     xlim = c(0, 100), ylim = c(0, 100),
     xlab = "Expected", ylab = "Predicted",
     main = "CA+PA+WC+OL",
     cex.main = 1.2, cex.lab = 1.0, cex.axis = 0.9,
     frame.plot = FALSE)
abline(a = 0, b = 1, col = "black", lty = 1, lwd = 2)
band_width <- crit_val
abline(a = band_width, b = 1, col = "black", lty = 2, lwd = 1)
abline(a = -band_width, b = 1, col = "black", lty = 2, lwd = 1)
sig <- ifelse(ks_d_manual > crit_val, " *", "")
text(5, 95, paste0("KS D = ", round(ks_d_manual, 1), sig), 
     cex = 1.0, pos = 4, font = 2)
text(65, 5, paste0("Crit. Val. = ", round(crit_val, 1)), 
     cex = 1.0, pos = 4, font = 2)
grid(col = "gray80", lty = 3)
box()

# Resetear parámetros gráficos
par(mfrow = c(1, 1))

# ========== INTERPRETACIÓN FINAL ==========
cat("=== INTERPRETACIÓN COMPLETA ===\n")
cat("Grupos individuales:\n")
cat("  CA: n =", nrow(CCL_Incurrida[1:50,]), "\n")
cat("  PA: n =", nrow(CCL_Incurrida[51:100,]), "\n")
cat("  WC: n =", nrow(CCL_Incurrida[101:150,]), "\n")
cat("  OL: n =", nrow(CCL_Incurrida[151:200,]), "\n")
cat("Combinado: n =", nrow(CCL_Incurrida), "\n")
