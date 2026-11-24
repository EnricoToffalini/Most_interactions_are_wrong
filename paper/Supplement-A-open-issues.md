# Supplement A: punti aperti da discutere con i coautori

## 1. Regola "Explicit link function": deviazione dal preregistrato non dichiarata

**Dove:** Supplement A, S2, `tbl-variables` e `tbl-coding-rules`.

**Problema.**

- `tbl-variables` (definizione preregistrata): "Indicating only which generalized
  linear model or family was used is not enough."
- `tbl-coding-rules` (regola applicata): "'Logistic regression' (logit), 'probit
  regression' (probit), and 'Poisson' or 'log-linear' (log) count as explicit.
  Naming only a family, a 'generalized linear model', or a 'GLMM' does not."
- "Poisson" nomina una famiglia, quindi la regola applicata contraddice quella
  preregistrata. La contraddizione c'è anche dentro la stessa cella di
  `tbl-coding-rules`.
- Il paper (sezione Review Findings) dichiara lo scarto: "We also accepted
  'Poisson regression', although it names a family [...] This rule is looser than
  the preregistered one [...] and under the preregistered rule the count would be
  lower."
- Il Supplement chiama "protocol deviation" solo la restrizione agli outcome
  osservati e non questa.

**Dettaglio:** `tbl-coding-rules` accetta anche "log-linear", che il paper non cita.
Non è incoerente, perché nomina il link, ma va tenuto presente se si allineano i
testi.

**Possibile intervento (da decidere):**

- Dichiarare nel Supplement la seconda deviazione dal protocollo, con la stessa
  motivazione del paper.
- Riformulare la cella di `tbl-coding-rules` per presentare "Poisson" come
  eccezione esplicita.

**Domanda aperta:** né il paper né il Supplement dicono di quanto sarebbe più
basso il conteggio con la regola preregistrata. Per rispondere bisogna ricontare
dai dati di coding: quanti articoli sono "explicit" solo grazie a "Poisson" o
"log-linear"?

## 2. Illustrazione logit/probit su dataset grouped-binomial: tenerla o toglierla?

**Dove:** Supplement A, S4 (Simulation 2, frase "The separate fitted-curve
illustration uses a grouped-binomial dataset…") e S5 ("Logit and probit fitted
curves": `fig-logit-probit-fitted` e `tbl-logit-probit-fitted`).

**Cos'è.** È generata da `scripts/02b-figure-logit-probit-fitted-example.R`, non
da Simulazione 2. Lo script produce un solo dataset con queste caratteristiche:

- 1000 unità, 60 trial per unità;
- un predittore continuo $x \sim U(-2.5, 2.5)$, senza gruppi né interazione;
- link generatore logit, intercetta 0.05, pendenza 1.75, seed 20260601.

Sullo stesso dataset fitta un GLM logit e un GLM probit. Mostra che le curve
sembrano quasi identiche ma differiscono in modo sistematico.

**Problema.**

- Il paper non la cita più (le versioni precedenti sì).
- Nel Supplement è agganciata a Sim 2, che però ha un altro disegno: 2×2,
  GLMM, replicazioni.
- In S5 si regge solo sul rimando "Section S4 notes that…".
- La descrizione è incompleta: mancano link generatore, range di $x$ e
  coefficienti. Per le altre illustrazioni questi dati sono riportati in
  "Generative parameters of the illustrative figures".

**Opzioni (da decidere):**

- **A. Toglierla.** Si eliminano la frase in S4 e il blocco in S5. Lo script resta
  nel repository ma non viene più citato.
- **B. Tenerla come illustrazione autonoma.**
  - Si toglie la frase da Sim 2.
  - I parametri vanno in "Generative parameters of the illustrative figures",
    adattando l'apertura che oggi dice "Two figures in the main paper…".
  - Il blocco in S5 si riscrive in modo che stia in piedi da solo, come
    complemento a Sim 2.

## 3. Soglia |ΔAIC| < 2: non motivata né citata

**Dove:** Supplement A, S5, `tbl-diagnostic-breakdown` (riga "|ΔAIC| < 2") e
`tbl-diagnostic-aic-distribution` (colonna "Within 2 units").

**Problema.**

- Il Supplement usa la soglia 2 per dire quante "vittorie" del link corretto sono
  troppo piccole per essere informative (es. D5: 38.1% delle repliche).
- La soglia non è motivata né citata da nessuna parte nel Supplement.
- Il paper non usa la soglia: riporta solo la proporzione di repliche in cui il
  link corretto ha AIC più basso ("AIC preferred the correct link in … of
  replications"), notando che, a parità di parametri, link indistinguibili
  darebbero circa .50. Non c'è incoerenza, perché nel Supplement la soglia è solo
  descrittiva e aggiuntiva, ma il criterio compare solo lì.

**Motivazione possibile.** È la regola pratica di Burnham e Anderson: modelli con
ΔAIC ≤ 2 hanno supporto empirico simile. Qui i due modelli hanno la stessa formula
e lo stesso numero di parametri, quindi ΔAIC = 2 equivale a una differenza di
log-verosimiglianza di 1, cioè un evidence ratio $e^{\Delta/2} \approx 2.7$.

**Riferimento candidato (non presente in `references.bib`, da verificare pagina e
sezione):** Burnham, K. P., & Anderson, D. R. (2002). *Model selection and
multimodel inference: A practical information-theoretic approach* (2nd ed.).
Springer. https://doi.org/10.1007/b97636

**Possibile intervento (da decidere):**

- Aggiungere una frase nella nota di `tbl-diagnostic-breakdown` o nel paragrafo
  sull'AIC (S4, "The same-formula Akaike information criterion (AIC)
  comparison…"), per esempio: "The threshold of 2 follows the common rule of thumb
  that models within 2 AIC units have similar empirical support [@burnham2002];
  it is used only descriptively."
- In alternativa, motivarla senza citazione, come differenza di log-verosimiglianza
  di 1 unità tra modelli con lo stesso numero di parametri.
