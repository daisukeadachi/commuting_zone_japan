# Fowler (2024) replicated on the Japanese delineation

What follows reports every table and figure of <https://doi.org/10.1038/s41597-024-03829-5> that the available data supports, computed on Japanese census commuting flows for 1980 to 2020. The pairwise wage correlation is absent, since the wage microdata application is pending. Tables are under `validation/output` and figures under `validation/output/figures`; the code that builds them is under `validation/code` and runs in the order `build_scope_and_adjacency.R`, `build_dissimilarity.R`, `build_clusters.R`, `make_diagnostics.R`, `make_containment.R`, `make_similarity.R`, `make_core.R`, `make_reassignments.R`, `make_figures.R`.

## What the delineation is built from

The commuting matrix is the "mainly working" variant restricted to ordinary households, which is the only definition available for 2020. The dissimilarity between two municipalities is one minus the sum of the two directed commuting flows over the smaller of the two resident labour forces, capped so that no dissimilarity falls below 0.001. The tree is built by average linkage and cut at a fixed height. Two cutoffs are carried throughout, 0.980 from the existing Japanese pipeline and 0.977 from the source paper.

The sample is 1,678 municipalities, those on the four main islands or the Okinawa main island together with those joined to one of them by a permanent road link, carrying 99.54 percent of the 2020 resident labour force. Area and compactness are computed on a Japan-wide equal-area conic projection. Population throughout is the resident labour force carried in the commuting matrix, because actual census population by municipality exists locally for one year only; every table that reports population states this.

One departure from the existing Japanese pipeline is deliberate and material. That pipeline builds its bilateral flow table with a join that keeps a pair only when the flow from the first municipality to the second is recorded, so a pair connected only in the reverse direction enters as unconnected. The matrix it clusters is therefore asymmetric. The replication uses the symmetric numerator that the source method specifies. `validation/notes/method_crosscheck.md` measures what that changes: between one and four percent of pairs in each year, up to four commuting zones, and up to 108 municipalities moving between zones.

## Diagnostic statistics

`validation/output/diagnostics_table.csv` carries the full table for every census year at both cutoffs. At the 0.980 cutoff the delineation consolidates steadily, from 353 commuting zones in 1980 to 212 in 2020, while the count of single-municipality zones falls from 58 to 9 and mean zone area grows from 1,030 to 1,715 square kilometres. The largest zone holds 36 municipalities in 2020 and 6.51 million workers: the twenty-three special wards of Tokyo together with Yokohama, Kawasaki, Saitama, Kawaguchi, Ichikawa, Urayasu and the inner Tama cities.

Compactness, the Polsby-Popper ratio of the dissolved zone averaged across zones, sits between 0.29 and 0.33 in every year and drifts slowly downward as zones grow. The choice of projection barely touches it: across the 47 prefecture shapes the ratio moves by at most 0.2 percent between an equal-area conic, an equal-area azimuthal, a Transverse Mercator zone and geodesic measurement on the ellipsoid.

## Containment

`validation/output/containment_table.csv` carries the Containment block. Mean containment across zones, the share of a municipality's residents who work inside its own commuting zone averaged over municipalities and then over zones, sits at 0.899 in 2020 at the 0.980 cutoff and has moved little since 1980, when it was 0.912. Weighting by resident labour force lowers it to 0.874, and the share of the national labour force that lives and works in the same zone is 0.865. Work containment, the share of jobs filled from inside the zone, is 0.914 in 2020.

The zone with the lowest mean containment in 2020 is a five-municipality cluster in western Tokyo, Kodaira, Higashimurayama, Kiyose, Higashikurume and Nishitokyo, at 0.358 over 241,000 workers. `lowest_containment_zone_2020_cut0.980.png` shades every municipality of that zone and of the four commuting zones adjacent to it by its own containment, with the zone boundaries drawn heavy and numbered and the five municipalities of the zone named. These are dormitory suburbs whose residents commute into the Tokyo zone next to them; the proportional flow keeps them together because they resemble one another, and the denominator of the smaller labour force keeps them from joining the far larger neighbour. The same mechanism produces the low-containment cases the source paper reports for the United States.

## Similarity between delineations and the cutoff

`similarity_map_2010_2020_cut0.980.png` maps the municipality-level Jaccard similarity between the 2010 and the 2020 delineations, and `validation/output/similarity_table.csv` reports the summaries. Mean similarity for that pair is 0.832 at the 0.980 cutoff. The two pre-pandemic controls are higher: 0.875 for 2010 against 2015 and 0.863 for 2005 against 2015. Decade churn was larger further back, though, at 0.787 for 1980 against 1990 and 0.825 for 1990 against 2000, so the 2010 to 2020 figure sits inside the historical range rather than outside it.

The cutoff sweep is the central exercise, and its result is the flatness the design anticipated. `similarity_vs_cutoff_2010_2020_anchor0.977.png` shows the curve. Holding 2010 at 0.980 and varying the 2020 cutoff, mean similarity peaks at 0.845 at a cutoff of 0.978, and stays within 0.005 of that peak over the range 0.977 to 0.979, which spans 227 to 219 commuting zones. Anchoring 2010 at 0.977 instead moves the peak to 0.975 and the plateau to 0.974 to 0.977, spanning 240 to 227 zones. The two anchors therefore sit inside or immediately beside each other's plateau, and nothing in the objective distinguishes them. The same pattern holds for both control pairs.

The diagnostic profile over the same range is in `zone_count_vs_cutoff_2020.png`, `containment_vs_cutoff_2020.png` and `noncontiguous_zones_vs_cutoff_2020.png`, and shows what a move in the cutoff costs. Zone count falls steeply as the cutoff rises while containment rises, so the cutoff trades granularity against self-containment with no interior optimum of its own. This supports reporting a defensible range rather than a single value.

## Contiguity

Non-contiguity is rare in Japan. At the 0.980 cutoff the number of zones holding a municipality that shares its zone with none of its neighbours is one in 1980, three in 1985, and zero in 2005 and 2010, rising to two in 2015 and one in 2020. The affected labour force stays below 0.09 percent of the national total in every year but one; the exception is 1985 at the 0.977 cutoff, where Saitama city is itself detached and the figure reaches 0.82 percent. `noncontiguous_municipalities_2020_cut0.980.png` maps the 2020 case and `validation/output/reassignment_table.csv` lists every case with the alternative that would remove it.

The recurring cases are mountain municipalities whose road connections run away from their land neighbours. Gujō and Shirakawa in Gifu appear in every year from 1980 to 2000, Uenohara in Yamanashi in 1985, 1990, 1995 and 2015, and Shimukappu in Hokkaido in 2015 and 2020. In every case the assigned zone receives far more of the municipality's commuters than any adjacent zone does, between 71 and 97 percent against no more than 10.4 percent, so reassignment would trade a contiguous map for a worse fit to the flows. That is the diagnostic's answer: the non-contiguity is a property of the commuting pattern, not a failure of the clustering, and the delineation keeps every municipality where the clustering put it.

The adjacency graph itself was checked for the generalization artifacts that would manufacture false non-contiguity. Among the 4,605 municipality pairs whose polygons lie within one kilometre of each other, 180 do not share a boundary, and none of those sits closer than 200 metres. The layer has no hairline gaps between municipalities that genuinely adjoin, so every detachment reported above is real geography.

## Core

`validation/output/core_table.csv` carries the Core block on two definitions. On the Urban Employment Area central cities, 68 percent of the 2020 commuting zones contain a core, 43.5 percent of workers commute to a core inside their own zone, and 42.2 percent of jobs are held by residents of a core inside the zone. On the mechanical rule that a municipality is a core when its densely inhabited district population reaches 10,000, which uses no commuting data at all, the same three measures read 75.0, 74.6 and 74.2 percent; at the 50,000 threshold they read 48.6, 61.6 and 60.7 percent.

The two definitions agree on direction and on time trend but not on level, and the gap is informative rather than troubling. The Urban Employment Area marks a single central city per area and excludes any municipality that is already a suburb of another, so its core set is smaller and more concentrated than a threshold rule that marks every municipality with a large enough dense population. Reading the two together is the point: the district rule cannot be re-reading the commuting flows the delineation was built from, and it moves the same way over time, so the Core measures are picking up urban structure rather than the delineation's own inputs.

`split_urban_areas_2020_cut0.980.png` maps the fourth measure, laying the split urban areas over the commuting zones that split them so that each area is seen straddling a zone boundary; areas that fall inside one zone are not drawn. Of the 208 Urban Employment Areas in 2020, 32 are split across more than one commuting zone. The count fell from 51 in 2000 to 28 in 2010 and has been roughly flat since, which tracks the consolidation of the zones themselves.

## What is not here

The Connection block, the pairwise wage correlation of the source paper's Table 2 and its Figure 2, needs the Basic Survey on Wage Structure microdata, for which an application is pending. The design is recorded in issue #11 so that it survives the wait.

The sensitivity check on the sample definition, using all employed persons whose workplace is identifiable rather than those mainly working, waits on the disaggregated 2020 file.
