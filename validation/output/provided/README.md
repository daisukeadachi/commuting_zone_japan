# Commuting zones for Japan, 1980 to 2020

Two tables, one for each of the two ways a commuting zone gets used. Both are keyed by census year and municipality code, so attaching zones to a dataset is one join.

`harmonized.csv` holds nine delineations at once, each built on the municipalities in force on 1 October 2015. Use it when the analysis pools several censuses. Pick the anchor year closest to the middle of the sample, take that `zone_YYYY` column, and ignore the rest. A zone definition that moves between waves is not a fixed effect, so one column is held across the whole panel rather than one per year.

`original.csv` holds one delineation per census year, each built on the municipalities in force on that census date. Use it when the analysis is a cross-section of a single year, because it describes the labour markets of that year in the units the year itself reports. The `zone` column is that year's own delineation.

## Columns

`census_year` and `code` are the join key. `code` is the municipality code in force on 1 October of that census year, so a user working with the 1980 census joins on 1980 codes and one working with 2020 joins on 2020 codes. The twenty-three special wards of Tokyo each carry their own code and share a zone, because the delineation treats them as one unit. An ordinance-designated city carries the city code, not its ward codes.

`prefecture`, `gun` and `muni_name` are the names as of that census date, for checking a join rather than for making one. `gun` is the county holding the municipality and is empty for a city.

In `harmonized.csv`, `zone_1980` through `zone_2020` are the zone the municipality falls into under each anchor year's delineation, and `offshore_island` marks a municipality outside the four main islands, the Okinawa main island and everything joined to one of them by a permanent road link. In `original.csv`, `zone` is the zone under that census year's own delineation.

Zone numbers identify a zone within one delineation and nothing more. The zone numbered 12 in `zone_1980` and the zone numbered 12 in `zone_2020` are unrelated, as are the zone numbered 12 in 1990 and in 2000 in `original.csv`.

## How the zones are built

The dissimilarity between two municipalities is one minus the proportional flow of Tolbert and Sizer, the sum of the two directed commuting flows over the smaller of the two resident workforces, capped at 0.999. The workforce is the row sum of the census commuting matrix over every destination it records. Residents counted are those whose labour-force status is "mainly working".

Municipalities are then agglomerated by average linkage, restricted at every step to pairs of clusters that share a boundary or a permanent road link, and the tree is cut at a height of 0.977. Both tables use that cutoff. The delineation covers every municipality the census reports, offshore islands included.

The commuting matrices are the Population Census. The boundaries and the code crosswalks come from Municipality Map Maker, <http://tkirimura.com/mmm/>.

## What is missing, and where a municipality appears twice

A municipality has no zone where the census records no commuting for it. That is Shikotan, Tomari and Rubetsu in the Northern Territories in every year, Miyake in 2000 under the volcanic evacuation, the municipalities under nuclear evacuation orders in 2015, and Futaba in 2020. Those rows carry `NA`.

Kamikuishiki in Yamanashi, code 19341, was divided in 2006 between Kofu and Fujikawaguchiko. It is the only municipality in the period that split rather than merged, and it is the one place where `harmonized.csv` carries two rows for one municipality, one per successor, for every census date up to 2005. The two rows can disagree, because the two successors need not fall in the same zone. `commuting_zone_crosswalk_cut0.977.csv` in the directory above carries the same rows with the area share of each successor in a `weight` column and the larger of the two marked in `largest_share`, for a user who needs exactly one row per municipality. `original.csv` is unaffected: Kamikuishiki has its own code and its own zone in every year up to 2005.
