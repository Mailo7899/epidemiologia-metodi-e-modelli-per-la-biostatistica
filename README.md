# 📁 Biostatistics & Clinical Data Analysis Projects

Questo repository raccoglie **progetti accademici di biostatistica e analisi dei dati clinici**, sviluppati durante il corso di laurea magistrale in **Biostatistica** presso l’**Università degli Studi di Milano-Bicocca**.  

I lavori si concentrano su **modelli statistici predittivi**, **analisi di sopravvivenza** e **valutazione delle performance dei modelli**, applicati a contesti clinici reali.

---

## 🧠 Project 1 — Modello predittivo del basso peso alla nascita  
**Metodi e Modelli Biostatistici per la Ricerca Clinica**

📌 *Millone A. (846588), Rossi S. (857183)*  
🎓 Università degli Studi di Milano-Bicocca

### 🔍 Abstract
Il progetto ha l’obiettivo di sviluppare e valutare un **modello predittivo per il basso peso alla nascita**, utilizzando informazioni **socio-sanitarie materne**.  
È stato stimato un **modello di regressione logistica** per analizzare l’associazione tra il basso peso neonatale e variabili quali età, peso materno, etnia, fumo, ipertensione, parto prematuro e irritabilità uterina.

Il dataset è stato suddiviso tramite **Split-Sample** in training e validation set, mantenendo costante la proporzione di eventi e non-eventi.  
Le performance predittive dei modelli sono state valutate attraverso:

- **Errore di predizione** (Brier Score, R² di Nagelkerke)
- **Discriminazione** (curve ROC, AUC, Test di DeLong)
- **Calibrazione** (Hosmer–Lemeshow, NRI, IDI)

Sono stati confrontati modelli con diverso livello di complessità. Il modello con **quattro covariate** (etnia, ipertensione, parto prematuro, peso materno) ha mostrato il **miglior compromesso tra accuratezza, discriminazione e calibrazione**, riducendo il rischio di overfitting rispetto al modello completo.

---

## ⏱️ Project 2 — SOFA e CCScore: analisi di sopravvivenza in pazienti geriatrici  
**Laboratorio di R per la Biostatistica**

📌 *Millone A. (846588), Rossi S. (857183)*  
🎓 Università degli Studi di Milano-Bicocca

### 🔍 Abstract
Questo progetto analizza la **sopravvivenza di una coorte di pazienti geriatrici sottoposti a intervento chirurgico**, valutando il ruolo prognostico degli indici **SOFA (Sequential Organ Failure Assessment)** e **Charlson Comorbidity Score (CCScore)**.

Dopo una fase di **data cleaning**, ricodifica delle variabili e costruzione delle variabili di tempo e status, sono state condotte:

- **Analisi descrittive univariate e bivariate**
- **Curve di sopravvivenza di Kaplan–Meier**
- **Test di Log-rank** per il confronto tra gruppi
- **Modelli di Cox** per la stima degli Hazard Ratio (HR)

Sono stati stimati modelli aggiustati e modelli stratificati, inclusa una versione combinata di SOFA e CCScore.  
I risultati indicano un aumento significativo del rischio di mortalità al crescere dei livelli di severità clinica. Il **modello di Cox** conferma quanto osservato dalle curve di sopravvivenza, pur evidenziando limiti legati alla **ridotta numerosità campionaria** e alla **categorizzazione degli indici clinici**.

---

## 🛠️ Metodi e Strumenti
- Regressione logistica
- Analisi di sopravvivenza
- Curve ROC e Kaplan–Meier
- Modelli di Cox
- Valutazione di discriminazione e calibrazione
- **R**, **SAS**, **SPSS**

---

## 👤 Author
**Andrea Millone**  
MSc in Biostatistics — Università degli Studi di Milano-Bicocca
