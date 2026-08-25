# Handoff: replicating Fowler (2024) on the Japanese commuting zones

Written for someone picking this up cold. It states where the work stands, what has been settled and why, what is still open, and the traps already found. The two pieces of work are tracked as issues #11 and #12; this note does not restate their content, it gives the context an issue body cannot carry.

## What the project is

The paper delineates commuting zones for Japan from census commuting flows, 1980 to 2020, by hierarchical agglomerative clustering on a proportional-flow dissimilarity, following Tolbert and Sizer (1996). It was rejected at its last submission and is being reworked for a different outlet. The reworking is not a revision of the old referee round: those reports are set aside, and the new direction, agreed with the coauthors, is to replicate Fowler (2024) on Japanese data as an internal validation, and separately to try the contiguity-constrained clustering that the U.S. Department of Agriculture now uses.

Fowler (2024) is <https://doi.org/10.1038/s41597-024-03829-5>; the replication code that paper points to is <https://github.com/csfowler/CommutingZones2020>, and reading its `CZ2020_Functions.R` settles several definitions the paper leaves implicit. The constrained method is Guénard and Legendre (2022), <https://doi.org/10.18637/jss.v103.i07>, implemented as `constr.hclust` in `adespatial`; the delineation that uses it is documented at <https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/documentation>.

The first deliverable is material for a coauthor meeting, not a draft.

## Where the work stands

Issue #13, collecting the external data the Core measures need, is closed and merged. Everything it produced is on `main`.

Issue #11, the replication, has a branch `11-fowler-replication` with `main` merged into it. It carries one commit of its own, the note on choosing a Japanese counterpart to the U.S. core-based statistical area. No analysis code has been written yet.

Issue #12, the constrained clustering, has not been started. It is sequenced after #11 because it reuses the adjacency construction and the diagnostic code written there.

## Settled decisions and the reasoning behind them

**Commuting matrix.** The baseline is labour-force status "mainly working" only, restricted to ordinary households, which is the `WORK_MAIN` variant of the existing pipeline. This is not a preference; it is the only definition available for 2020, because the 2020 delivery arrived already aggregated and the disaggregated file behind it does not exist locally. A sensitivity check on all employed persons whose workplace is identifiable is wanted but deferred until that file arrives from Fukai-san, who has been asked. Note that the existing `WORK_ALL` variant is not that object: it also sweeps in persons temporarily absent from work, who have no commute.

**Population for the diagnostic table.** Use the resident labour force already carried in the commuting matrix, `tot_pop_living_mun`, and note the definition wherever it is reported. Actual census population by municipality exists only for a single year locally, and the labour force has the advantage of being exactly the quantity the delineation itself uses.

**Comparison pairs.** Baseline is three: 2010 against 2020, 2010 against 2015, and 2005 against 2015. The first is the headline and carries the same decade spacing as the source paper. The other two are pre-pandemic controls, one at the same spacing and one at five years, so that a break at 2020 can be read against comparisons that do not span the pandemic. Every decade pair from 1980 onward is an appendix exercise for completeness and is not the basis for choosing the cutoff.

**Cutoff sweep.** Asymmetric, following Fowler. Fix the earlier year and vary the later one only, at two anchors in turn, 0.980 and 0.977. This is not a stylistic choice. Imposing a common cutoff on both years makes the objective degenerate: similarity tends to one at both ends of the range, because a high cutoff puts everything in one zone in both years and a low cutoff leaves every municipality alone in both years, and either way the partitions agree trivially. The curve is valley-shaped with no interior maximum. Fowler's `cutTest` holds the published 2010 partition fixed and cuts only the 2020 tree, confirming the design at code level. If the objective turns out flat over a range containing both anchors, that flatness is the result and the argument for the cutoff choice.

**Changing municipality universe.** For each pair, restrict to municipalities present in both years and log every drop with its reason. In the harmonized universe of 1,741 the cases are Miyake village, 13381, absent in 2000 after the volcanic evacuation, and five Fukushima municipalities absent under the evacuation orders: Tomioka 07543, Okuma 07545, Futaba 07546, Namie 07547 and Iitate 07564 are all absent in 2015, and Futaba alone is still absent in 2020. Common-set sizes are 1,740 for 2010-2020, 1,736 for 2010-2015 and for 2005-2015.

**Ordinance-designated cities.** Wards stay collapsed to the city. The special wards of Tokyo stay separate. Every dataset assembled so far follows this.

**Geographic scope.** Five blocks: the four main islands plus the main island of Okinawa, which is kept rather than dropped because it holds roughly 1.45 million residents and is a labour market in its own right. Municipalities on other islands are dropped. The same restriction applies to both #11 and #12 so that the comparison isolates the effect of the constraint rather than of the sample.

**Core definition.** Primary source is the Urban Employment Area, the only candidate supplying both the core set and the exogenous areas, and whose two population thresholds coincide with the U.S. ones by design. Secondary is a mechanical rule on densely inhabited district population, reported alongside for the three measures that need only a core set, because it uses no commuting data and so separates urban structure from a re-reading of the flows the delineation was built from. The full argument, including the two rejected candidates, is in `validation/notes/core_definition.md`.

## Data on hand

Committed under `validation/data`:

- `uea/`, all 32 published Urban Employment Area files: metropolitan and micropolitan, suburb lists and central-city lists, for 1980, 1990, 1995, 2000, 2005, 2010, 2015, 2020. There is no 1985 delineation, which affects no comparison pair in scope.
- `did_municipality.csv` and `did_municipality_harmonized.csv`, district population and area by municipality for 1980 to 2020, on survey-year codes and on 2015 codes respectively.
- `crosswalk/`, one municipality code crosswalk per census year from 1980 to 2010 onto 1 October 2015.
- `did_municipality_2020_raw.xlsx`, the census table behind the 2020 rows.
- `MANUAL_DOWNLOADS.md`, provenance and the checks each file passed.

Rebuild scripts are in `validation/code`. `build_did_table.py` re-fetches its bulk sources, which are gitignored.

Two inputs are not in the repository and have to be read from the shared project folder, rooted at `Dropbox/projects/Kawaguchi_Saito/CommutingZone/adachi/clustering`:

- Commuting matrices, `data/raw/commuteCensusData/data/use/WORK_MAIN/commute_<year>_<harmonized|original>.csv`, 1980 to 2020.
- The municipality boundary layer, `data/raw/mmm/shapefiles/mmm20151001_ku_aggregate/mmm20151001.shp`. The folder name matters: the wards are already aggregated, matching the convention above.

## Traps already found, do not rediscover them

**The published input in the repository is on a different convention from the published output.** `data/commute_2015_harmonized.csv` holds 1,891 units with designated-city wards separate; `output/2015_harmonized.csv` holds 1,736 with them collapsed. The collapsing happens inside `codes/MASTER.R`. Read the shared-folder `WORK_MAIN` files, not the repository `data/` folder, or the units will not line up with the delineations or with the district table.

**The floor parameter is not an extra degree of freedom.** The pipeline floors the dissimilarity at 0.001. That is algebraically identical to the cap of 0.999 Fowler places on the proportional flow. Nothing to reconcile.

**Compactness is the Polsby-Popper ratio.** Fowler's paper does not say so; his `checkCompactness` computes four pi times area over perimeter squared on the dissolved zone polygon, then takes the mean across zones.

**The 1990 and 2000 Urban Employment Area files carry values belonging to other rows,** and the 1995 and 2000 metropolitan files and the 2000 micropolitan file repeat rows. Deduplicate on read. Where those files disagree with the district table before 2005, the district table is right; this was checked against 2005, 2010 and 2015, where the two agree exactly.

**Codes 01695, 01696 and 01698** appear as crosswalk targets but never in the census. They are the Northern Territories and hold no district.

**The Ikuta directory is almost entirely gitignored.** `Ikuta_RA/data`, `mapdata` and `output` are excluded except for `output/map_image`. Anything written there will not be committed. New work goes under `validation/`.

**`sf` failed to load a shapefile through `Rscript` on this machine** while the equivalent Python read worked. If it recurs, that is why `build_did_table.py` parses the `.dbf` directly rather than going through `sf`. The R packages `sf`, `igraph` and `adespatial` are installed and `constr.hclust` resolves.

**The GitHub Actions billing on this account is failing,** so workflows do not run. Several were disabled deliberately. Do not expect continuous integration to verify anything.

## Open questions for the user

None blocking. Two worth raising when they become relevant:

Whether the sensitivity check on the sample definition is still wanted once Fukai-san answers, given it cannot cover 2020 unless the disaggregated 2020 file exists.

Whether the repository reorganisation sketched in section 4 of the coauthor memo, published crosswalks and diagnostics with a citable identifier, should become its own issue now or wait until the analysis settles. It is currently tracked nowhere but a comment on #11.

## Conventions to keep

Commit messages and every file in the repository are in English. Chat with the user is in Japanese. Reader-facing prose does not carry invented short tags; write the descriptive name. Each paragraph is one source line in Markdown and LaTeX. One branch per task, deleted after merge, and never force-push. Replies to the user end at the answer asked for, without appended menus of next steps.
