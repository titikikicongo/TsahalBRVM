# ============================================================
# BACKTEST DE PORTEFEUILLE BRVM - VERSION PYTHON (CORRIGÉE)
# ============================================================

# --- 1. Importations des bibliothèques ---
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import quantstats as qs
import warnings

warnings.filterwarnings('ignore')

# --- 2. Chargement des données (exemple avec vos données) ---
print("\n📊 Chargement des données...")
# ⚠️ Remplacez ces données par les vôtres
dates = pd.date_range(start='2024-01-01', end='2024-12-01', freq='MS')
valeurs = [1000000, 1020000, 1015000, 1035000, 1040000, 1060000,
           1050000, 1070000, 1085000, 1075000, 1090000, 1100000]

df = pd.DataFrame({'Date': dates, 'Valeur': valeurs})

# --- 3. Préparation des données et calcul des rendements ---
df = df.sort_values('Date').reset_index(drop=True)
df['Rendement'] = df['Valeur'].pct_change()
df['Rendement_continu'] = np.log(df['Valeur'] / df['Valeur'].shift(1))

# Convertir en série temporelle
returns = df[['Date', 'Rendement_continu']].copy()
returns.set_index('Date', inplace=True)
returns = returns.dropna()

# Paramètres pour les calculs
risk_free_rate_annual = 0.06            # Taux sans risque annualisé (6%)
periods_per_year = 12                   # Pour des rendements mensuels

# --- 4. Calcul des indicateurs de performance ---
print("\n" + "="*60)
print("📈 INDICATEURS DE PERFORMANCE")
print("="*60)

# Récupérer la série de rendements
portfolio_returns = returns['Rendement_continu']

# 1. Rendement annualisé (CAGR)
cagr_return = qs.stats.cagr(portfolio_returns)

# 2. Volatilité (non annualisée) et annualisée
volatility_raw = qs.stats.volatility(portfolio_returns)
volatility_annualized = volatility_raw * np.sqrt(periods_per_year)

# 3. Ratio de Sharpe annualisé
sharpe_ratio = qs.stats.sharpe(portfolio_returns, rf=risk_free_rate_annual)

# 4. Ratio de Sortino
sortino_ratio = qs.stats.sortino(portfolio_returns, rf=risk_free_rate_annual)

# 5. Drawdown maximum
max_drawdown = qs.stats.max_drawdown(portfolio_returns)

# 6. Ratio de Calmar
calmar_ratio = qs.stats.calmar(portfolio_returns)

# --- Affichage des résultats ---
print(f"✅ Rendement annualisé (CAGR) : {cagr_return:.2%}")
print(f"📉 Volatilité annualisée : {volatility_annualized:.2%}")
print(f"📈 Ratio de Sharpe : {sharpe_ratio:.3f}")
print(f"🛡️ Ratio de Sortino : {sortino_ratio:.3f}")
print(f"📉 Drawdown Maximum : {max_drawdown:.2%}")
print(f"⚖️ Ratio de Calmar : {calmar_ratio:.2f}")

if sharpe_ratio > 1:
    print("🎉 Analyse : La performance ajustée au risque est EXCELLENTE.")
elif sharpe_ratio > 0.5:
    print("👍 Analyse : La performance ajustée au risque est BONNE.")
else:
    print("⚠️ Analyse : La performance ajustée au risque peut être améliorée.")

# --- 5. Calcul de la VaR et de la CVaR ---
print("\n" + "-"*40)
print("🎲 MÉTRIQUES DE RISQUE")
print("-"*40)

confidence_level = 0.95
var_historical = np.percentile(portfolio_returns, 100 * (1 - confidence_level))
cvar_historical = portfolio_returns[portfolio_returns <= var_historical].mean()

print(f"Value at Risk (VaR) à {confidence_level*100:.0f}% : {var_historical:.2%}")
print(f"Conditional VaR (CVaR) à {confidence_level*100:.0f}% : {cvar_historical:.2%}")

# --- 6. Génération du rapport HTML ---
print("\n" + "="*60)
print("📄 GÉNÉRATION DU RAPPORT HTML")
print("="*60)

# Sauvegarder le rapport dans un fichier
qs.reports.html(portfolio_returns, title="Analyse de Portefeuille BRVM",
                output="rapport_portefeuille_brvm.html")

print("✅ Rapport généré avec succès : 'rapport_portefeuille_brvm.html'")

# --- 7. Tableau récapitulatif ---
summary_df = pd.DataFrame({
    'Indicateur': ['Rendement annualisé (CAGR)', 'Volatilité annualisée',
                   'Ratio de Sharpe', 'Ratio de Sortino', 'Maximum Drawdown',
                   'Ratio de Calmar', f'VaR ({confidence_level*100:.0f}%)',
                   f'CVaR ({confidence_level*100:.0f}%)'],
    'Valeur': [f'{cagr_return:.2%}', f'{volatility_annualized:.2%}',
               f'{sharpe_ratio:.3f}', f'{sortino_ratio:.3f}',
               f'{max_drawdown:.2%}', f'{calmar_ratio:.3f}',
               f'{var_historical:.2%}', f'{cvar_historical:.2%}']
})

print("\n" + "="*60)
print("📋 SYNTHÈSE FINALE")
print("="*60)
print(summary_df.to_string(index=False))

# Sauvegarder la synthèse
summary_df.to_csv('resultats_backtest.csv', index=False, encoding='utf-8')
print("\n💾 La synthèse a été sauvegardée dans 'resultats_backtest.csv'")