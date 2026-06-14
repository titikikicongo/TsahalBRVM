# ============================================================
# BACKTEST DE PORTEFEUILLE BRVM
# ============================================================

# 1. Installation et chargement des packages
# -------------------------------------------

# devtools::install_github("Koffi-Fredysessie/BRVM")
# 
# # Or use remotes
# # install.packages("remotes")
# remotes::install_github("Koffi-Fredysessie/BRVM")

packages <- c("tidyverse", "PerformanceAnalytics", "portfolioBacktest", "BRVM", "lubridate")

for(pkg in packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 2. IMPORT DES DONNEES
# -------------------------------------------
# Option A : Depuis le package BRVM (si fonctionnel)
# -------------------------------------------
# Obtenir les données historiques de la BRVM
# data_brvm <- BRVM_list()  # Pour lister les titres disponibles
# 
# Exemple pour un titre spécifique (à adapter)
# mon_actions <- BRVM_cap()  # Ou BRVM_rank("Top", 10)

# Option B : Depuis un fichier CSV/Excel (recommandé)
# -------------------------------------------
# Structure attendue : Date, Valeur_portefeuille
# 
# write.csv(vos_donnees, "portefeuille_brvm.csv", row.names = FALSE)
# 
# OU directement en créant un data frame :

# Exemple avec vos données (remplacez par vos vraies valeurs)
portefeuille_data <- data.frame(
  Date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01",
                   "2024-05-01", "2024-06-01", "2024-07-01", "2024-08-01",
                   "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01")),
  Valeur = c(1000000, 1020000, 1015000, 1035000, 1040000, 1060000,
             1050000, 1070000, 1085000, 1075000, 1090000, 1100000)
)

# 3. CALCUL DES RENDEMENTS
# -------------------------------------------
# Calculer les rendements journaliers/mensuels
portefeuille_data <- portefeuille_data %>%
  arrange(Date) %>%
  mutate(
    Rendement = (Valeur / lag(Valeur) - 1) * 100,
    Rendement_continu = log(Valeur / lag(Valeur))
  )

# Convertir en série temporelle xts (format requis par PerformanceAnalytics)
returns_xts <- xts(portefeuille_data$Rendement_continu, order.by = portefeuille_data$Date)
returns_xts <- na.omit(returns_xts)
colnames(returns_xts) <- "Portefeuille_BRVM"

# Taux sans risque UEMOA (environ 6% annualisé = 0.5% mensuel)
risk_free_rate <- 0.005  # 0.5% par mois

# 4. INDICATEURS DE PERFORMANCE
# -------------------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("                INDICATEURS DE PERFORMANCE\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# Rendement annualisé
annual_return <- Return.annualized(returns_xts, geometric = TRUE)
cat("Rendement annualisé :", sprintf("%.2f%%", annual_return * 100), "\n")

# Volatilité annualisée
annual_vol <- StdDev.annualized(returns_xts)
cat("Volatilité annualisée :", sprintf("%.2f%%", annual_vol * 100), "\n")

# Ratio de Sharpe (avec taux sans risque)
sharpe <- SharpeRatio.annualized(returns_xts, Rf = risk_free_rate, geometric = TRUE)
cat("Ratio de Sharpe :", sprintf("%.3f", sharpe), "\n")

# Interprétation du Sharpe
if(sharpe > 1) {
  interpretation <- "⭐ EXCELLENT - Très bonne performance ajustée du risque"
} else if(sharpe > 0.5) {
  interpretation <- "👍 BON - Performance correcte"
} else if(sharpe > 0) {
  interpretation <- "📈 MOYEN - Perfectible"
} else {
  interpretation <- "⚠️ À AMÉLIORER - Rendement inférieur au taux sans risque"
}
cat("Interprétation :", interpretation, "\n\n")

# Maximum Drawdown (perte maximale)
max_dd <- maxDrawdown(returns_xts)
cat("Perte maximale (Max Drawdown) :", sprintf("%.2f%%", max_dd * 100), "\n")

# Ratio de Sortino (ne pénalise que la volatilité négative)
sortino <- SortinoRatio(returns_xts, MAR = risk_free_rate)
cat("Ratio de Sortino :", sprintf("%.3f", sortino), "\n")

# VaR (Value at Risk) à 95%
VaR_95 <- VaR(returns_xts, p = 0.95)
cat("VaR (95%) :", sprintf("%.2f%%", VaR_95 * 100), 
    " - Perte maximale attendue 1 mois sur 20\n")

# CVaR (Expected Shortfall) à 95%
CVaR_95 <- ES(returns_xts, p = 0.95)
cat("CVaR (95%) :", sprintf("%.2f%%", CVaR_95 * 100), 
    " - Perte moyenne en cas de scénario défavorable\n")

# 5. RATIOS COMPLEMENTAIRES
# -------------------------------------------
cat("\n", paste(rep("-", 40), collapse = ""), "\n")
cat("RATIOS COMPLÉMENTAIRES\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

# Calmar Ratio (rendement / max drawdown)
calmar <- annual_return / abs(max_dd)
cat("Calmar Ratio :", sprintf("%.3f", calmar), 
    " - Plus il est élevé, mieux c'est\n")

# Information Ratio (si vous avez un benchmark)
# À décommenter si vous avez un indice BRVM 10 de référence
# benchmark_returns <- xts(..., order.by = portefeuille_data$Date)
# info_ratio <- InformationRatio(returns_xts, benchmark_returns)
# cat("Information Ratio :", sprintf("%.3f", info_ratio), "\n")

# 6. VISUALISATIONS
# -------------------------------------------

# Graphique 1 : Évolution de la valeur du portefeuille
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# Courbe de valeur
plot(portefeuille_data$Date, portefeuille_data$Valeur, type = "l", col = "steelblue", lwd = 2,
     xlab = "Date", ylab = "Valeur (FCFA)", main = "Évolution du Portefeuille BRVM",
     ylim = c(min(portefeuille_data$Valeur) * 0.95, max(portefeuille_data$Valeur) * 1.02))
grid()
points(portefeuille_data$Date, portefeuille_data$Valeur, pch = 19, col = "steelblue", cex = 0.8)

# Graphique 2 : Rendements mensuels
barplot(portefeuille_data$Rendement[-1], names.arg = format(portefeuille_data$Date[-1], "%b"),
        col = ifelse(portefeuille_data$Rendement[-1] >= 0, "forestgreen", "firebrick"),
        main = "Rendements Mensuels", xlab = "Mois", ylab = "Rendement (%)")
abline(h = 0, lwd = 2)

# Graphique 3 : Drawdown
drawdowns <- data.frame(Date = portefeuille_data$Date[-1],
                        DD = portefeuille_data$Rendement[-1] / 100)
plot(drawdowns$Date, cumprod(1 + drawdowns$DD), type = "l", col = "darkorange", lwd = 2,
     xlab = "Date", ylab = "Facteur d'attrition", 
     main = "Drawdown (Perte depuis le pic)",
     ylim = c(0.9, 1.05))
grid()

# Graphique 4 : Distribution des rendements
hist(portefeuille_data$Rendement[-1], breaks = 8, col = "lightblue", 
     main = "Distribution des Rendements", xlab = "Rendement (%)", 
     border = "white")
abline(v = 0, lwd = 3, col = "red", lty = 2)



# 7. ANALYSE AVEC PORTFOLIOBACKTEST (OPTIONNEL)
# -------------------------------------------
if(require(portfolioBacktest, quietly = TRUE)){
  cat("\n\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("      ANALYSE ROLLING WINDOW (portfolioBacktest)\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # 1. Convert the original VALEUR (prices) into an xts object
  prices_xts <- xts(portefeuille_data$Valeur, order.by = portefeuille_data$Date)
  colnames(prices_xts) <- "Portefeuille_BRVM"
  
  # 2. Build the dataset list
  dataset <- list(
    adjusted = prices_xts
  )
  
  # 3. Define the portfolio function
  my_portfolio <- function(dataset, ...){
    return(rep(1, ncol(dataset$adjusted)))
  }
  
  # 4. Run the backtest with a tiny lookback window for monthly data
  bt <- portfolioBacktest(
    portfolio_funs = list("BRVM" = my_portfolio), 
    dataset_list   = list("BRVM_Data" = dataset),
    lookback       = 6,   # <--- FIX: Keeps the required history down to 6 months
    optimize_every = 1,   # Rebalance every month
    rebalance_every = 1
  )
  
  # Afficher le résumé
  summary_bt <- backtestSummary(bt)
  print(summary_bt$performance)
}

# 8. SAUVEGARDE DES RESULTATS
# -------------------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("              SYNTHESE FINALE\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# Créer un dataframe de synthèse
synthese <- data.frame(
  Indicateur = c("Rendement annualisé", "Volatilité annualisée", 
                 "Ratio de Sharpe", "Maximum Drawdown", "Ratio de Sortino",
                 "VaR (95%)", "CVaR (95%)", "Calmar Ratio"),
  Valeur = c(sprintf("%.2f%%", annual_return * 100),
             sprintf("%.2f%%", annual_vol * 100),
             sprintf("%.3f", sharpe),
             sprintf("%.2f%%", max_dd * 100),
             sprintf("%.3f", sortino),
             sprintf("%.2f%%", VaR_95 * 100),
             sprintf("%.2f%%", CVaR_95 * 100),
             sprintf("%.3f", calmar))
)

print(synthese)

# Optionnel : exporter vers CSV
# write.csv(synthese, "resultats_backtest.csv", row.names = FALSE)