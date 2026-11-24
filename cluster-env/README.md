# Ambiente di calcolo del cluster DPSS (env conda `r460`)

Questa cartella descrive l'ambiente in cui sono state eseguite le simulazioni.
Le informazioni sono state raccolte il 24/11/2026, in sola lettura, sul cluster
`slurm.dpss.psy.unipd.it`. Il progetto sul cluster è in
`~/projects/git/Most_interactions_are_wrong`, al commit `2b3e34c`.

Il `renv.lock` nella radice del repository non è stato toccato. È un'istantanea
più vecchia (R 4.5.1) e non corrisponde all'ambiente che ha prodotto i risultati:
per il confronto si veda `package-alignment-report.html`.

## File

| File | Contenuto |
|---|---|
| `renv-cluster.lock` | Lockfile `renv` creato dall'env `r460` con `renv::lockfile_create(type = "implicit")` sul progetto: R 4.6.0 e 114 pacchetti usati dal progetto, con le versioni installate sul cluster. |
| `environment-r460.yml` | `conda env export -n r460 --no-builds`. |
| `conda-list-r460.txt` | `conda list -n r460`, completo di build e canale. |
| `package-alignment-report.html` | Report sull'allineamento tra `renv.lock`, il cluster e l'ambiente locale, con le note di rilascio rilevanti. |

Per ripristinare i pacchetti R da questo lockfile, in un R 4.6.0:

```r
renv::restore(lockfile = "cluster-env/renv-cluster.lock")
```

## Sistema

| | |
|---|---|
| Sistema operativo | Debian GNU/Linux 11 (bullseye) |
| Kernel | Linux 5.10.0-46-amd64 (Debian 5.10.262-1), x86_64 |
| glibc | 2.31 (Debian 2.31-13+deb11u14) |
| Slurm | slurm-wlm 20.11.4 |
| Nodo di login | `slurm-home`: 2 CPU Intel Xeon (Cascade Lake), 1.9 GB di RAM |
| Partizioni | `queue1`: `slurm1`, 94 CPU, ~2 TB di RAM; `queue2`: `stat[1-2]`; `queue3`: `stat3`, 48 CPU |

Le simulazioni girano sui nodi di calcolo, non sul nodo di login. Il sistema
operativo dei nodi di calcolo non è stato verificato: servirebbe un job.

## Conda

| | |
|---|---|
| conda | 26.3.2 (Miniconda, `~/miniconda3`, Python 3.13 nella base) |
| Env usato dagli script `.slurm` | `r460` (`conda activate r460`, poi `Rscript --vanilla`) |
| Origine dell'env | `conda create -n r460 --clone r461-test`, 28/07/2026 |
| Canali | conda-forge (tutti i pacchetti dell'env), defaults |
| Altri env | `base`, `mne`, `r461` (non usati dal progetto) |

Da conda arrivano R e le librerie di sistema; i pacchetti R no. Le dipendenze
principali sono:

- **R:** `r-base` 4.6.0, l'unico pacchetto `r-*` presente.
- **Algebra lineare:** OpenBLAS 0.3.33 (`libblas`/`liblapack` 3.11.0, `openblas` build).
- **Compilatori:** gcc, g++ e gfortran 15.2.0 (conda-forge), più binutils 2.45.1, cmake 4.4.0 e make 4.4.1.
- **Ottimizzatore:** `nlopt` 2.11.0.
- **Grafica e testo:** cairo, pango, harfbuzz, freetype, fontconfig, gsl 2.7, libcurl, openssl 3.6.3, libxml2.

L'elenco completo è in `conda-list-r460.txt`.

## R

| | |
|---|---|
| Versione | R 4.6.0 (2026-04-24), piattaforma `x86_64-conda-linux-gnu` |
| BLAS / LAPACK | OpenBLAS 0.3.33 (`libopenblasp-r0.3.33.so`); LAPACK 3.12.0 |
| RNG | Mersenne-Twister, Inversion, Rejection (default) |
| Libreria | `~/miniconda3/envs/r460/lib/R/library` (unica, con `--vanilla`) |
| Compilatori usati da R | `x86_64-conda-linux-gnu-cc -std=gnu23`, `x86_64-conda-linux-gnu-c++ -std=gnu++20` |
| Pacchetti installati | 183 in totale, installati con `install.packages()` da CRAN (`cloud.r-project.org`) dentro l'env, non con conda |

Pacchetti chiave per le analisi:

| Pacchetto | Versione | Installato il |
|---|---|---|
| lme4 | 2.0-6 | 24/08/2026 |
| glmmTMB | 1.1.14 | 24/08/2026 |
| TMB | 1.9.25 | 24/08/2026 |
| Matrix | 1.7-6 | 24/08/2026 |
| DHARMa | 0.5.0 | 24/08/2026 |
| nloptr | 2.2.1 | 24/08/2026 |
| reformulas | 0.4.4 | 24/08/2026 |
| psyphy | 0.3 | 12/10/2026 |

## Differenze con `renv.lock` (radice del repository)

- **Versione di R:** 4.6.0 invece di 4.5.1.
- **Pacchetti in più nel lockfile del cluster:** 16 pacchetti che `renv` ha trovato
  come dipendenze installate (per esempio `pbkrtest`, `broom`, `sandwich`,
  `RcppArmadillo`).
- **Pacchetti mancanti sul cluster:** 10 pacchetti che non sono installati:
  `patchwork`, `readxl` e le loro dipendenze, più `reactable`/`reactR`. Servono
  solo agli script che girano in locale (`scripts/01`, `scripts/02b`) e all'app
  Shiny.
- **Campo `Hash`:** il lockfile del cluster non lo ha. `renv` lo omette con
  questo metodo di creazione e `renv::restore()` funziona comunque.
