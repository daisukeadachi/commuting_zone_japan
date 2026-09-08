# Commuting zones for Japan, 1980 to 2020

Two tables are released. Both are keyed by census year and municipality code, so one can attach zones to a dataset in a single join.

`harmonized.csv` holds nine delineations at once, each built on the municipality units in force on 1 October 2020. Use it when the analysis pools several censuses. Pick the anchor year closest to the middle of the sample, take that `zone_YYYY` column, and hold it across the whole panel. A zone definition is not a fixed effect if it moves between waves, so one column is held across every wave rather than one column per wave.

Holding the zones at one anchor year and applying them to a later year has a cost, and the cost grows with the gap. The municipality-level Jaccard similarity between the 1980 delineation and each later one falls from 0.83 at 5 years to 0.78 at 10, 0.71 at 20, 0.65 at 30 and 0.62 at 40. The 1980 zones therefore describe the labor markets of 2020 only loosely.

`original.csv` holds one delineation per census year, each built on the municipality units in force on that census date. Use it when the analysis is a cross-section of a single year, since it describes the labor markets of that year in the municipality units of that year. The `zone` column is that year's own delineation.

## Columns

`census_year` and `code` are the join key. `code` is the five-digit municipality code in force on 1 October of that census year, so a user working with the 1980 census joins on 1980 codes and one working with 2020 joins on 2020 codes. The 23 special wards of Tokyo each carry their own code and share a zone, because the delineation treats them as one unit. A government-designated city carries the city code rather than the codes of its wards.

`prefecture`, `gun` and `muni_name` are the names as of that census date, for checking a join rather than for making one. `gun` is the county holding the municipality and is empty for a city.

In `harmonized.csv`, `zone_1980` through `zone_2020` give the zone the municipality falls into under each anchor year's delineation. The column `offshore_island` marks a municipality outside the four main islands, the Okinawa main island and everything joined to one of them by a permanent road link. In `original.csv`, `zone` gives the zone under that census year's own delineation.

Zone numbers identify a zone within one delineation and nothing more. For example, the zone numbered 12 in `zone_1980` and the zone numbered 12 in `zone_2020` are unrelated, and so are the zone numbered 12 in 1990 and the zone numbered 12 in 2000 in `original.csv`.

## How the zones are built

The dissimilarity between two municipalities is one minus the proportional flow of Tolbert and Sizer. The proportional flow is the sum of the two directed commuting flows over the smaller of the two resident workforces, capped at 0.999. A workforce is the row sum of the census commuting matrix over every destination it records, and it counts the residents who mainly work.

Municipalities are then grouped by hierarchical cluster analysis with average linkage. Every merge is restricted to two clusters that share a boundary or a permanent road link, so no commuting zone holds a municipality cut off from the rest of it. The tree is cut at a height of 0.977, and both tables use that cutoff. The delineation covers every municipality the census reports, offshore islands included.

The commuting matrices come from the Population Census. The boundaries and the code crosswalks come from Municipality Map Maker, <http://tkirimura.com/mmm/>.

## Missing zones, and the one municipality with two rows

A municipality has no zone where the census records no commuting for it, and those rows carry `NA`. They are Shikotan, Tomari and Rubetsu in the Northern Territories in every year, Miyake in 2000 under the volcanic evacuation, the municipalities under nuclear evacuation orders in 2015, and Futaba in 2020.

Kamikuishiki in Yamanashi, code 19341, is the only municipality in the period that split rather than merged, divided in 2006 between Kofu and Fujikawaguchiko. Therefore, `harmonized.csv` carries two rows for Kamikuishiki, one per successor, for every census date up to 2005. The two rows can disagree, because the two successors need not fall in the same zone. A user who needs exactly one row per municipality should read `commuting_zone_crosswalk_cut0.977.csv` in this folder, which carries the same rows with the area share of each successor in a `weight` column and the larger of the two marked in `largest_share`. `original.csv` is unaffected, since Kamikuishiki has its own code and its own zone in every year up to 2005.
