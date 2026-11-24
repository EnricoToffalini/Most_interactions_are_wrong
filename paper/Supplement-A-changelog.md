# Supplement A: registro delle modifiche (revisione novembre 2026)

Tutte le modifiche riguardano solo `paper/Supplement-A-Technical-details.qmd`.

- Il paper (`paper-v2.qmd`) non è stato toccato ed è stato usato come riferimento:
  ogni discrepanza tra paper e Supplement è stata corretta nel Supplement.
- Nessun metodo è cambiato: non ci sono simulazioni da rilanciare né modifiche
  all'atlas (Supplement B).
- Il PDF del Supplement A è stato rigenerato.

I punti che richiedono una decisione comune sono in `Supplement-A-open-issues.md`.

## 1. Allineamento con il paper

- **Terminologia "correct / wrong link".** Nelle sezioni sulla simulazione
  diagnostica si usa ora "correct link" / "wrong link", come nel paper. Cambiano
  di conseguenza:
  - le etichette "AIC favors correct link" / "AIC favors wrong link";
  - la definizione $\Delta\mathrm{AIC} = \mathrm{AIC}(\text{wrong link}) -
    \mathrm{AIC}(\text{correct link})$;
  - il titolo "Calibration under the correct link";
  - la caption della tabella di calibrazione.
- **Etichetta Pregibon nel 2×2.** "N.A." diventa "N.I." (not identifiable), come
  nel paper. Il testo e le caption spiegano che il termine aggiunto è collineare
  con gli effetti fissi nel disegno 2×2 saturo. "--" resta solo per le versioni
  dei controlli DHARMa che non si applicano.
- **S2, sintesi per rivista.** La frase sul paper ora dice correttamente che il
  paper riporta il range tra riviste di due quantità, nominando le riviste agli
  estremi.
- **S6, software.** Aggiunti versione di R (4.5.1) e citazioni di `renv`, `lme4`,
  `glmmTMB`, `psyphy` (prima mancante; serve per il link chance-corrected) e
  `DHARMa`.

## 2. Precisione e completezza del testo

- **Simulazione 1, test di Wald.** La frase "same asymptotic Wald-normal
  calculation" è sostituita da una descrizione esplicita: il prodotto è un singolo
  coefficiente sulla scala del link di ciascun modello, testato con un test z di
  Wald, $z = \hat\beta / \widehat{SE}$.
- **Simulazione 1, `nAGQ`.** Aggiunto il dato della run precedente con il default
  `nAGQ = 1`: 42–47% dei fit chance-corrected erano flagged. Il dato è verificato
  sulla versione git del CSV.
- **Simulazione 3, modello bounded-score.** Spiegato come le predizioni tornano
  alla metrica del punteggio totale ($\text{sum} = (\text{max}+1)\hat q - 0.5$) e
  che il test d'interazione riguarda il prodotto sulla scala probit.
- **Simulazione diagnostica, stima.** Specificato quale funzione stima ogni
  scenario:
  - `glm` in D1 e D5;
  - `glmmTMB` in D3 e D4;
  - `lme4::glmer` in D2, con `nAGQ = 10` per il modello chance-corrected e Laplace
    per lo standard logit.

  La descrizione di D2 è semplificata di conseguenza.
- **Numero dei controlli DHARMa.** Chiarito che sono sempre quattro: il quarto
  controllo è `testQuantiles` sul predittore (D1, D2, D4, D5) oppure
  `testCategorical` sulle celle (D3). "All five DHARMa p-values" diventa "all
  applicable DHARMa p-values".
- **Confronto AIC.** La frase "No inconclusive ΔAIC category is used, so the two AIC
  rows … are complementary" è riscritta: ogni replica è assegnata al link con AIC
  più basso, quindi le righe "favors correct" e "favors wrong" sommano al 100%
  delle repliche con entrambi gli AIC disponibili.
- **Acronimi.** Ora definiti alla prima occorrenza: GLM, GLMM, LMM, ICC, AIC.
  Uniformato "Gaussian-identity model".

## 3. Intervalli di confidenza e numeri calcolati dai dati

- **Intervalli di Wilson ovunque.** Il testo diceva "for every rate", ma alcuni
  tassi ne erano privi. Ora ogni tasso nella tabella diagnostica (Table 19) e
  nella tabella della distribuzione di ΔAIC (Table 20) ha il suo intervallo
  di Wilson al 95%.
- **Numeri della calibrazione.** Prima erano scritti a mano nel testo, ora sono
  calcolati inline da `tables/diagnostic-calibration.csv`, come già fa il paper.
  Ho controllato che i valori siano identici a quelli precedenti (8%, 8%, 8%, 7%,
  14%).

## 4. Struttura e formattazione

- **Marcatori di revisione.** Rimossi i marcatori rossi `\revstart` / `\revend`, con
  le relative definizioni e il pacchetto `xcolor` usato solo per quelli.
- **S5 riordinata.** Le sezioni seguono ora l'ordine delle simulazioni. Titoli
  uniformati con l'indicazione della simulazione:
  1. Implied values, Simulation 1
  2. Deterministic product term on each link scale, Simulation 2 (prima
     "Deterministic link-scale decomposition")
  3. Logit and probit fitted curves
  4. Deterministic pseudo-interaction under the wrong link, Simulation 2
  5. Implied values, Simulation 3
  6. Diagnostic detection rates and AIC strength
  7. Distribution of the AIC difference
  8. Calibration under the correct link
- **Table 19 (diagnostic breakdown).**
  - **Problema:** la tabella andava a capo in punti casuali e nel PDF la caption e
    l'intestazione comparivano due volte.
  - **Font e impaginazione:** ora usa un font più piccolo (solo in PDF) e sta su
    una pagina, con una sola caption.
  - **Celle con conteggi:** il conteggio sta su una riga e "% [CI]" su quella
    sotto. Nessuna informazione è stata tolta.
  - **Caption:** riscritta. Dice che i tassi si riferiscono al modello con link
    sbagliato, descrive le righe e spiega la notazione "correct link → wrong link".
  - **Nota:** aggiunta la frase "where no denominator is shown, it is 3,000".
