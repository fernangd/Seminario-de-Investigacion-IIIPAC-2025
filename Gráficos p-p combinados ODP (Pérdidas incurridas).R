library(readr)

# Data de aseguradoras (CA, PA, WC, OL)

ODP_Incurrida <- read_csv("C:/Users/VINROL/Desktop/Clases III PAC 2025/Seminario de Investigación/Datas para los triangulos p-p/ODP/Pérdidas Incurridas/ODP_Incurrida_gráficos.csv")

# ========== CONFIGURAR LAYOUT DE 3 FILAS X 2 COLUMNAS ==========
windows(width = 12, height = 14)  # En Windows
# quartz(width = 12, height = 14)  # En Mac
# x11(width = 12, height = 14)     # En Linux

par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))

# Definir constantes una sola vez
alpha <- 0.05
c_alpha <- 1.36

# ========== FILA 1: CA Y PA ==========

# CA - ODP Incurred
predicted_percentiles <- ODP_Incurrida[1:50,]$OutcomePercentile
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
     main = "CA - ODP Incurridas",
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

# PA - ODP Incurred
predicted_percentiles <- ODP_Incurrida[51:100,]$OutcomePercentile
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
     main = "PA - ODP Incurridas",
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

# WC - ODP Incurred
predicted_percentiles <- ODP_Incurrida[101:150,]$OutcomePercentile
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
     main = "WC - ODP Incurridas",
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

# OL - ODP Incurred
predicted_percentiles <- ODP_Incurrida[151:200,]$OutcomePercentile
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
     main = "OL - ODP Incurridas",
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
predicted_percentiles_combined <- ODP_Incurrida$OutcomePercentile

# Ajustar breaks para incluir todos los valores
breaks_seq <- seq(0, 100, by = 10)
breaks_seq[1] <- min(0, floor(min(predicted_percentiles_combined)))
breaks_seq[length(breaks_seq)] <- max(100, ceiling(max(predicted_percentiles_combined)))

hist(predicted_percentiles_combined, 
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
n_combined <- length(predicted_percentiles_combined)
predicted_sorted_combined <- sort(predicted_percentiles_combined/100)
expected_ecdf_combined <- (1:n_combined) / n_combined
ks_d_combined <- max(abs(predicted_sorted_combined - expected_ecdf_combined)) * 100
crit_val_combined <- (c_alpha / sqrt(n_combined)) * 100
expected_theoretical_combined <- ppoints(n_combined) * 100
predicted_sorted_plot_combined <- sort(predicted_percentiles_combined)

plot(expected_theoretical_combined, predicted_sorted_plot_combined, 
     type = "p", pch = 19, col = "black",
     xlim = c(0, 100), ylim = c(0, 100),
     xlab = "Expected", ylab = "Predicted",
     main = "CA+PA+WC+OL",
     cex.main = 1.2, cex.lab = 1.0, cex.axis = 0.9,
     frame.plot = FALSE)
abline(a = 0, b = 1, col = "black", lty = 1, lwd = 2)
band_width_combined <- crit_val_combined
abline(a = band_width_combined, b = 1, col = "black", lty = 2, lwd = 1)
abline(a = -band_width_combined, b = 1, col = "black", lty = 2, lwd = 1)
sig_combined <- ifelse(ks_d_combined > crit_val_combined, " *", "")
text(5, 95, paste0("KS D = ", round(ks_d_combined, 1), sig_combined), 
     cex = 1.0, pos = 4, font = 2)
text(65, 5, paste0("Crit. Val. = ", round(crit_val_combined, 1)), 
     cex = 1.0, pos = 4, font = 2)
grid(col = "gray80", lty = 3)
box()

# Resetear parámetros gráficos
par(mfrow = c(1, 1))

# ========== INTERPRETACIÓN FINAL ==========
cat("=== INTERPRETACIÓN COMPLETA ===\n")
cat("Grupos individuales:\n")
cat("  CA: n =", nrow(ODP_Incurrida[1:50,]), "\n")
cat("  PA: n =", nrow(ODP_Incurrida[51:100,]), "\n")
cat("  WC: n =", nrow(ODP_Incurrida[101:150,]), "\n")
cat("  OL: n =", nrow(ODP_Incurrida[151:200,]), "\n")
cat("Combinado: n =", nrow(ODP_Incurrida), "\n")

