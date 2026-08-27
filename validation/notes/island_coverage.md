# Commuting zones for the offshore islands

The delineation the diagnostics run on covers the four main islands, the Okinawa main island, and every municipality joined to one of them by a permanent road link: 1,656 municipalities. The 63 municipalities outside it are offshore islands, and in 2020 they hold 215,526 workers, 0.4635 percent of the national resident labour force. The only other municipalities the 2020 commuting matrix does not carry are Shikotan, Tomari and Rubetsu in the Northern Territories, where no census is taken, and Futaba, still under an evacuation order. A delineation that covers the 63 therefore reaches the whole census population, which is what a user of the published zones needs.

`validation/code/build_full_coverage.R` builds it, and writes `validation/output/full_coverage_zones.csv` and `validation/output/island_zones.csv`.

## How the islands enter

Nothing about the method changes, and no island grouping is written down by hand. Two municipalities on the same island share a boundary, so each road-connected island group is already a connected component of the municipality adjacency graph, and the contiguity-constrained clustering already runs on each component separately. Extending the delineation is a matter of letting the clustering see the components it was previously filtered out of. Of the 58 components of the graph, three carry the main islands and 55 are offshore; three of those 55 are the Northern Territories and carry no census.

The one substantive change is in the denominator of the proportional flow. The resident labour force is the row sum over the destinations inside the universe being clustered, so widening the universe to the whole country widens it. This moves nothing: the main-island part of the full-coverage delineation is identical to the published one, municipality by municipality, in every census year and at both cutoff anchors. The full-coverage table is a strict extension of the delineation the diagnostics report, not a second delineation competing with it.

## What the islands look like

At the baseline cutoff in 2020 the full-coverage delineation gives 275 commuting zones, of which 52 are offshore and hold the 63 island municipalities between them. Six of those zones hold more than one municipality: Rishiri, Shodoshima, Tanegashima, Amami Oshima, Tokunoshima and Okinoerabu. Every road-connected island group comes out as exactly one commuting zone, so the natural assumption about offshore islands is confirmed by the flows rather than imposed on them.

That has not always been true. In 1980 the islands hold 56 zones rather than 52. Rishiri is two zones, Nishinoomote is separate from the rest of Tanegashima, and Amami Oshima is three zones, with Setouchi and Uken each apart from the northern group. Rishiri joins up in 1985, Tanegashima and Uken in 1990 and Setouchi in 2005, after which every group is a single zone in every year. Where the assumption fails it fails in the early years and on the larger islands, which is where within-island commuting was thinnest.

Containment is high, as the assumption that an island is its own labour market predicts. Averaged over the 63 island municipalities it is 0.985 in 2020, and 37 of the 52 island zones sit at 0.99 or above. Four zones fall below 0.95, and all four are islands within a short ferry ride of the mainland: Kamijima in the Geiyo islands at 0.788, Himeshima off the Kunisaki peninsula at 0.860, Osakikamijima at 0.932 and Chiburi in the Oki islands at 0.933. The same average over the 1,655 main-island municipalities is 0.889, so the islands are the more self-contained part of the country by a wide margin.

## What this does not change

The diagnostics, the containment table, the Core measures and the figures stay on the main-island scope, so that the comparison with <https://doi.org/10.1038/s41597-024-03829-5> is a comparison of like with like and the zone counts remain those of the delineation that paper's design would produce. The full-coverage zones are a product built beside them, and they are what the published crosswalk is built from.
