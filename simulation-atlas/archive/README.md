# Archived: full Simulation Atlas report

Since November 2026, Supplement B is the interactive table explorer
(`../Supplement-B-Simulation-atlas-explorer.qmd`). This folder keeps the earlier
full report, with text, figures and the numerical appendix, in case we decide to
restore it:

- `Supplement-B-Simulation-atlas.qmd`: source, last updated for the B = 3000
  atlas run of 2026-11-12, including the five diagnostic cases and the
  calibration figure;
- `Supplement-B-Simulation-atlas.html`, `.pdf`, `_files/`: last rendered
  versions. They may predate the last edits of the source.

The report is not maintained. It reads the same summaries in `../data/`
(paths via `here()`), so it still renders from this folder:

```bash
quarto render simulation-atlas/archive/Supplement-B-Simulation-atlas.qmd
```

To restore it as Supplement B, check its text against the current atlas code and
summaries, move it back with `git mv`, restore the `.gitignore` exception for its
`_files/` folder, and add it back to `index.qmd`, `AGENTS.md` and
`simulation-atlas/README.md`.
