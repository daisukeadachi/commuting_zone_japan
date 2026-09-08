# The pipeline

Every script is run from the repository root, so that the relative paths inside it resolve: `Rscript code/make_diagnostics.R`, `python code/fetch_boundaries.py`. `config.R` holds the paths, the parameters and the helper functions, and every R script sources it.

## Before the first run

Set `CZ_SHARED_ROOT` to the folder holding the census commuting matrices and the municipality boundary layer, neither of which this repository carries. `data/README.md` describes both and says where they come from. Set `CZ_CENSUS2020_DIR` as well if the 2020 matrices sit outside that folder; it defaults to `census2020` inside it.

Three further variables select a run. Each defaults to the delineation the paper reports, and each writes its own file names, so a variant run never overwrites the baseline.

| Variable | Default | Other values |
| --- | --- | --- |
| `CZ_DELINEATION` | `constrained` | `unconstrained`, the same clustering with the contiguity restriction lifted |
| `CZ_CODES` | `harmonized` | `original`, each census year on the municipality units in force at its own date |
| `CZ_SAMPLE` | `WORK_MAIN` | `WORK_ALL`, which widens the labor force beyond residents who mainly work |

## Run order

Inputs first. These are needed once, and several of them write into `data/`.

1. `fetch_crosswalks.py`, then `build_recent_crosswalks.py` — municipality code crosswalks between census dates.
2. `fetch_boundaries.py` — one municipality boundary layer per census date, for the runs on each date's own codes.
3. `build_scope_and_adjacency.R` — which municipalities enter the delineation, and which pairs adjoin.
4. `build_original_scope_and_adjacency.R` — the same two tables on each census date's own codes.
5. `build_did_table.py`, then `harmonize_did_table.py` — densely inhabited district population by municipality, used by the mechanical core rule.

Then the delineation itself.

6. `build_dissimilarity.R` — the proportional-flow dissimilarity matrix of each census year.
7. `build_constrained_clusters.R` — the contiguity-constrained agglomeration and the zones it yields. `build_clusters.R` does the same without the constraint, and the appendix comparison needs it.
8. `build_full_coverage.R` — the zones extended to every municipality the census reports, offshore islands included.
9. `build_zone_crosswalk.R` — writes the released tables into `delineation/`.

Then the statistics and the figures the paper reports, in any order.

10. `make_diagnostics.R` — zone counts, sizes, areas and compactness.
11. `make_containment.R` — the share of residents working inside their own zone.
12. `make_core.R` — the core measures, and the map of urban areas split across zones.
13. `make_similarity.R` — similarity between the delineations of two census years, and the cutoff sweep.
14. `make_constrained_comparison.R` — the constrained delineation beside the unconstrained one.
15. `build_us_compactness.R` — the same compactness measure on the 2020 United States delineation.

Finally the artifacts the paper prints.

16. `make_paper_tables.R` — the LaTeX table fragments, into `paper/tables/`.
17. `make_figures.R` and `make_coverage_maps.R` — the figures, into `paper/figures/`.

`make_figures.R` draws more than the paper prints: every census pair, both cutoff anchors, and several diagnostic profiles. Only the seven figures the paper prints are committed; the rest are written beside them and ignored by git.
