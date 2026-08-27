# The published crosswalk: commuting zones on every census date's municipality codes

A researcher pooling several censuses needs one set of commuting zones held fixed across years. Their panel is keyed by year and municipality code, and the codes move underneath them: municipalities merge, and a town incorporated as a city takes a new code. `validation/output/commuting_zone_crosswalk_cut0.977.csv` and its companion at the other cutoff are the table that joins the two. One row per municipality per census date, with the zone that municipality falls into under each year's delineation.

`validation/code/build_zone_crosswalk.R` builds it.

## What the table holds

`census_year` and `code` are the join key: the municipality code in force on 1 October of that census year. The dates run 1980, 1985, ..., 2020 and 2025, so a user attaching zones to the 2025 census joins on the same table as one attaching them to 1980. `prefecture`, `gun` and `muni_name` are the names as of that date, for checking a join rather than for making one.

`zone_1980` through `zone_2020` are the commuting zone the municipality belongs to under each census year's delineation. A user who wants the zones of their sample's first year takes that column and ignores the rest. The columns are all built from the full-coverage delineation, so the offshore islands are carried and the table reaches the whole census population.

`code_2015` is the municipality on 1 October 2015 that the row's municipality falls into, which is the unit the delineation is built on. `offshore_island` marks the rows whose 2015 unit lies outside the main-island scope the diagnostics report.

## Why the join is exact

Every delineation is built on commuting matrices already harmonized onto the codes in force on 1 October 2015. Each census date's code therefore maps forward onto exactly one 2015 unit, and so onto exactly one zone of any year's delineation. A municipality formed by a merger cannot straddle two zones of an earlier year's delineation, because the harmonization happens before the clustering rather than after it. This is what makes a fixed-anchor table well defined at all: the alternative, delineating each year on its own codes and then reconciling, would leave merged municipalities sitting in two zones at once.

One municipality genuinely splits rather than merges. Kamikuishiki in Yamanashi, code 19341, was divided in 2006 between Kofu and Fujikawaguchiko. It appears twice for each census date up to 2005, once per successor, with the share of its area in the `weight` column: 0.770 to Fujikawaguchiko and 0.230 to Kofu. `largest_share` marks the larger of the two, so a user who needs one row per municipality filters on it. Every other municipality carries a single row with a weight of one.

A zone is missing where the 2015 unit is absent from that year's commuting matrix. That is Shikotan, Tomari and Rubetsu in the Northern Territories, where no census is taken, in every year, and Futaba under its evacuation order in 2020.

## The municipality codes after 2015

The crosswalks for 1980 to 2010 come from Municipality Map Maker, <http://tkirimura.com/mmm/>, whose coverage ends on 1 May 2019. The two later dates are built by `validation/code/build_recent_crosswalks.py`, because what happened after 2015 is small enough to write down and check.

Between 1 October 2015 and 1 October 2020 no municipalities merged, and two towns were incorporated as cities and took new codes: Tomiya in Miyagi on 10 October 2016 and Nakagawa in Fukuoka on 1 October 2018. Between 1 October 2020 and 1 October 2025 nothing changed. Japan held 792 cities, 743 towns and 183 villages on both dates, and any merger, city incorporation or promotion from village to town would move one of those three counts; a change of name, or of a boundary between two municipalities, leaves the code alone. The 2025 codes are therefore the 2020 codes.

## What a fixed anchor costs

Holding the zones at 1980 and using them for a later year is not free, and `validation/output/similarity_to_1980.csv` and `similarity_to_1980_by_year.png` measure it. The municipality-level Jaccard similarity between the 1980 delineation and each later one falls steadily with the gap: 0.832 at five years, 0.780 at ten, 0.711 at twenty, 0.645 at thirty and 0.622 at forty. The 0.980 cutoff traces the same path a little above it.

Read that against the decade comparisons the replication reports, where 2010 against 2020 scores 0.859 and the widest decade pair, 1980 against 1990, scores 0.780. A forty-year anchor costs about as much again as the worst decade in the series. The table is still the right object for a pooled panel, since a zone definition that moves between waves is not a fixed effect at all, but a user should know that the 1980 zones describe the labour markets of 2020 only loosely, and should choose the anchor closest to the middle of their sample where the design allows it.
