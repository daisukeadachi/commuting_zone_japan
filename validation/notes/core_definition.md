# Choosing a Japanese counterpart to the core-based statistical area

Fowler (2024) reports four Core measures in his Table 2 and one map in his Figure 6. All five need something the commuting-zone delineation does not itself produce: an externally defined set of urban cores, and an externally defined set of areas built around those cores. This note establishes what the U.S. object is, sets out the Japanese candidates with their thresholds and data availability, and recommends which to use.

## What the Core measures require

Two distinct inputs are needed, and no single measure uses both.

A set of **core municipalities** supports three of the four measures: the share of commuting zones that contain a core, the average share of a commuting zone's residents who work in a core, and the average share of a commuting zone's workforce that lives in a core. A set of **exogenous areas** supports the fourth, the count of areas split across more than one commuting zone, and the accompanying map. In the U.S. these come from the same source, so the distinction is easy to miss. In Japan the candidates differ in whether they supply one input or both.

## The U.S. object

A core-based statistical area is a county-level aggregation around an urban nucleus. A metropolitan statistical area requires an urban area of 50,000 or more inhabitants. A micropolitan statistical area requires an urban area of at least 10,000 but fewer than 50,000. A central county is one where at least half the population lives in urban areas of 10,000 or more, or which contains at least 5,000 people inside a single urban area of that size. An outlying county joins if at least 25 percent of its employed residents work in the central counties, or at least 25 percent of the jobs in it are held by residents of the central counties. As of July 2023 there were 387 metropolitan and 538 micropolitan areas, so 925 areas over 3,222 counties.

Two features matter for the Japanese mapping. The delineation is not exhaustive, since many rural counties belong to no area at all, which is the gap commuting zones were invented to fill. The delineation is also itself commuting-based, so using it to validate a commuting-based delineation is partly circular. Fowler does not raise the second point, but it constrains how much weight the Core measures can carry.

## Candidate 1: Urban Employment Area

The Urban Employment Area of Kanemoto and Tokuoka takes the densely inhabited district population of a municipality as the counterpart of the U.S. urban-area population, and a commuting rate of 10 percent as the counterpart of the U.S. employment interchange criterion. A central city needs a densely inhabited district population of at least 10,000 and must not itself be a suburb of another area. A suburb is a municipality sending at least 10 percent of its commuters to the central city, applied recursively.

The two tiers of the Japanese definition line up with the two U.S. tiers exactly. Checking the delineation files directly, the 2015 Metropolitan Employment Area file holds 94 central cities with densely inhabited district populations from 50,088 to 9,272,740, and the Micropolitan Employment Area file holds 93 with populations from 10,236 to 49,683. The break sits at 50,000 and the floor at 10,000, the same two numbers the Office of Management and Budget uses. This is by construction rather than coincidence, since the Japanese definition was written against the U.S. standard.

This candidate supplies both required inputs. The central cities give the core set and the areas give the exogenous partition, so all four Core measures and Figure 6 become computable. Coverage is comparable in spirit to the U.S. case, with 187 areas over 1,741 municipalities in 2015 and roughly 95 percent of the national population inside some area as of 2010. Delineations exist for every census year from 1980 to 2020.

Two objections deserve recording. The earlier version of this paper used the Urban Employment Area as the object of comparison rather than as an input, and the editor asked that claims of superiority over administrative units be softened; using it as an external benchmark is a different use and does not reinstate that claim, but the framing should be explicit. More substantively, the Urban Employment Area is built from commuting flows drawn from the same census, so it shares the circularity noted above in a sharper form than the U.S. case, because the commuting rate enters both constructions directly.

## Candidate 2: Major metropolitan area and metropolitan area of the Statistics Bureau

The official delineation takes the special wards of Tokyo and the ordinance-designated cities as the centres of major metropolitan areas, and cities of 500,000 or more outside those as the centres of metropolitan areas. A surrounding municipality joins if commuters and students aged 15 and over travelling to the centre are at least 1.5 percent of its resident population and it is contiguous with the area, with an enclave rule for municipalities surrounded by qualifying ones.

The institutional standing of this definition is its main attraction, since it is the government's own. Three properties count against it. The 1.5 percent threshold is far looser than the 25 percent used in the U.S., so the areas are large and few, which makes the count of split areas uninformative. The measure counts students as well as workers, which does not match a labour-market concept. The centres are defined by administrative class rather than by any measure of the urban nucleus, so the definition drifts whenever a city is promoted.

## Candidate 3: Institutional city classes

Ordinance-designated cities require a population of 500,000 under the Local Autonomy Act, with recent designations in practice made at around 700,000, and there are 20 of them. Core cities require 200,000 following the 2015 reform that absorbed the former special-case cities, and there are 62.

This candidate is attractive precisely because it is not commuting-based, so it escapes the circularity. It supplies a core set but no exogenous areas, so the count of split areas and Figure 6 remain out of reach. It is also poorly suited to a delineation spanning 1980 to 2020: the core city class was created only in 1996 and revised in 2015, so the earlier census years have no counterpart, and membership changes by political decision rather than by any measured quantity.

## Candidate 4: A mechanical rule on densely inhabited district population

A densely inhabited district is a run of contiguous enumeration units with density of at least 4,000 persons per square kilometre whose combined population reaches 5,000. The concept has been published with every census since 1960.

A core municipality can then be defined mechanically as one whose densely inhabited district population reaches 50,000, with a second tier at 10,000, reproducing the two U.S. thresholds without importing any commuting information. This is the Urban Employment Area stripped of its commuting step. It is fully time-consistent from 1980 to 2020, it is reproducible from published statistics, and it is the only candidate whose core set is independent of the flows used to build the commuting zones. Like candidate 3 it supplies no exogenous areas.

## Recommendation

Use the Urban Employment Area as the primary source. It is the only candidate that supplies both inputs, its two population thresholds coincide with the U.S. ones by design, and it covers every census year in the sample. This makes the Core block and Figure 6 directly comparable to Fowler's, which is the point of the exercise.

Report the mechanical densely inhabited district rule alongside it for the three measures that need only a core set. Because that rule uses no commuting data, agreement between the two versions is evidence that the Core measures are picking up something about urban structure rather than re-reading the flows the delineation was built from. Disagreement localises the circularity. State the circularity explicitly in the text, since it applies to Fowler's own numbers as well and is not otherwise visible to a reader.

Do not use the Statistics Bureau definition or the institutional city classes as the primary source, for the reasons given above. Either can be added as a robustness row if a reviewer asks for an official definition.

## Data to collect

The delineation files held locally cover the Metropolitan Employment Area for 2005 and 2015 and the Micropolitan Employment Area for 2015 only. The remaining years, and the missing 2005 micropolitan file, have to be taken from the published delineation. Densely inhabited district population by municipality and census year has to be assembled from published census tables and mapped onto the harmonized municipality codes with the existing crosswalk.

## Sources

- <https://www.census.gov/programs-surveys/metro-micro/about.html>
- <https://www.federalregister.gov/documents/2021/07/16/2021-15159/2020-standards-for-delineating-core-based-statistical-areas>
- <https://www.csis.u-tokyo.ac.jp/UEA/uea_code.htm>
- <https://ja.wikipedia.org/wiki/%E9%83%BD%E5%B8%82%E9%9B%87%E7%94%A8%E5%9C%8F>
- <https://ja.wikipedia.org/wiki/%E9%83%BD%E5%B8%82%E5%9C%8F_(%E7%B7%8F%E5%8B%99%E7%9C%81)>
- <https://www.stat.go.jp/data/chiri/1-1.html>
- <https://www.soumu.go.jp/main_sosiki/jichi_gyousei/bunken/chihou-koukyoudantai_kubun.html>
