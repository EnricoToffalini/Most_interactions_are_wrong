# Ambiente di calcolo del cluster DPSS (env conda `r460`)

Questa cartella descrive l'ambiente in cui sono state eseguite le simulazioni.
Le informazioni sono state raccolte il 24/11/2026, in sola lettura, sul cluster
`slurm.dpss.psy.unipd.it`. Il progetto sul cluster è in
`~/projects/git/Most_interactions_are_wrong`, al commit `2b3e34c`.

Il `renv.lock` nella radice del repository è stato rigenerato il 25/11/2026 a
partire da questo ambiente (vedi sotto, "Come è costruito `renv.lock`"). La
versione precedente (R 4.5.1) non corrispondeva all'ambiente che ha prodotto i
risultati ed è recuperabile dalla storia di git. Il report
`package-alignment-report.html` confronta quella versione precedente con il
cluster.

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

## Come è costruito `renv.lock` (radice del repository)

Il lock di riferimento del progetto unisce due fonti, con priorità al cluster:

1. **Tutti i pacchetti di `renv-cluster.lock`:** 114 pacchetti, con R 4.6.0 e le
   versioni installate nell'env `r460`. Coprono le simulazioni, la diagnostica
   e l'atlas, cioè tutto ciò che gira su slurm.
2. **28 pacchetti usati solo dagli script che girano in locale**, con le versioni
   installate sulla macchina locale il 25/11/2026. Li ha trovati
   `renv::lockfile_create(type = "implicit")` scansionando il progetto.
   - Per la review e le figure illustrative (`scripts/01`, `scripts/02b`):
     `patchwork` e `readxl`.
   - Per la preparazione del dataset (`Literature_review/`): `readr` e
     `writexl`.
   - Per Supplement B: `here` e `reactable`.
   - Il resto sono dipendenze di questi pacchetti.

Le versioni del lock precedente sono state scartate. I 10 pacchetti che c'erano
solo lì sono tutti inclusi al punto 2, con la versione locale; la versione è
cambiata solo per `patchwork` (1.3.0 → 1.3.2) e `hms` (1.1.3 → 1.1.4).

`renv` non viene attivato dagli script, che girano con `Rscript --vanilla`. Per
ricreare l'ambiente, in R 4.6.0:

```r
renv::restore()
```

Versione di R, librerie di sistema, BLAS e compilatori non sono fissati da
`renv`: per quelli il riferimento è `environment-r460.yml`.

## Note su `renv-cluster.lock`

- **Pacchetti in più rispetto al lock precedente:** 16 pacchetti che `renv` ha
  trovato come dipendenze installate (per esempio `pbkrtest`, `broom`,
  `sandwich`, `RcppArmadillo`).
- **Pacchetti mancanti sul cluster:** `patchwork`, `readxl` e le loro
  dipendenze, più `reactable`/`reactR`. Servono solo agli script locali e a Supplement B;
  nel lock di riferimento sono aggiunti dalla macchina locale.
- **Campo `Hash`:** non c'è, perché `renv` lo omette con questo metodo di
  creazione. `renv::restore()` funziona comunque.
