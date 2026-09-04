# The denominator of the proportional flow

The proportional flow between two municipalities is the sum of the two directed commuting flows over the smaller of their two workforces, and that denominator is a choice as much as the numerator is. This records what the choice is, why it is what it is, and what a wider reading of who counts as working does to the delineation.

## What the baseline divides by

The source paper writes the measure as the flows over `min(W_i, W_j)`, where "W_i and W_j are the total workforces of those two counties", and its replication code computes each workforce as the row sum of the commuting matrix, `rowSums(data[, -1])`, over every destination the census records. The baseline here is that same quantity: for each municipality, the residents whose labour-force status is "mainly working", summed over every destination in the commuting matrix. It is a count of workers, not of residents, and not of the population.

The pipeline previously summed only over destinations inside the geographic scope the clustering runs on, which is a slightly smaller number, because a resident commuting to an offshore island fell out of it. The difference is tiny: it is 0.0052 percent of the national workforce in 1980, 0.030 percent in 2015 and 0.011 percent in 2020, the largest figures being the years in which the scope loses the most. It moves the delineation not at all. Every zone assignment, in every census year and at both cutoff anchors, with and without the contiguity constraint, is identical under the two denominators. The baseline is the unrestricted row sum because that is what the source method specifies, not because the results asked for it.

`validation/derived/labour_force_<year>.csv` carries all three counts side by side: the denominator in use, the restricted row sum, and the row sum over every destination.

## A wider count of who is working

The census records three labour-force statuses that involve work. "Mainly working" is the baseline. Beside it are those working alongside housework and those working alongside study, about eight million and one million people respectively in every census year, against roughly fifty million mainly working. Counting all three raises the denominator by between 17 and 24 percent depending on the year, and the increase has grown: it is 1.19 in 1980 and 1.24 in 2015.

Someone working alongside housework is working, so whether the delineation should divide by the wider count is a real question rather than a technicality. It is a question about the denominator alone, and it is asked that way here: the flows in the numerator stay on the residents mainly working, so the two delineations differ only in what they divide by. `validation/code/build_side_work_denominator.R` builds the wider count from the attribute-level census tabulation, which carries labour-force status and so allows the three statuses to be added; `validation/code/make_denominator_comparison.R` compares the two delineations, writing `validation/output/denominator_comparison.csv` and `denominator_movers.csv`.

The comparison reaches 1980 to 2015. The 2020 tabulation was delivered already aggregated into the two samples the pipeline reads, and the statuses inside the wider of them cannot be separated, so 2020 has no wider count.

## What the wider count does

A larger denominator makes every link weaker at an unchanged cutoff, so the delineation breaks into more zones. At the baseline cutoff the count rises from 385 to 434 in 1980 and from 223 to 251 in 2015, a gain of between 8 and 14 percent in every year. The municipality-level Jaccard similarity between the two delineations runs from 0.89 to 0.92, which places the effect of the denominator between two decades of change in the flows themselves, where the similarity is 0.86, and the effect of the contiguity constraint, where it is above 0.98.

Containment is what says which delineation fits the commuting better. It falls under the wider count in every year and at both cutoffs, from 0.903 to 0.884 in 2015 at the baseline cutoff and from 0.910 to 0.899 in 1980. The direction is the same everywhere and the gap is between one and two percentage points. Read that with its caveat: containment is computed from the flows of the residents mainly working, which is the sample the baseline denominator matches, so the comparison is not neutral between the two. What it does establish is that widening the denominator alone, without the flows that would accompany the wider sample, moves the delineation away from the commuting the baseline sample records.

Between 340 and 510 municipalities change zone in each year at the baseline cutoff, holding between 15 and 49 percent of the workforce. That is a large share for a change of this kind, and it is large because the zones that break up are the populous ones: a metropolitan zone that splits in two moves every municipality in it. `denominator_movers.csv` lists each municipality with its zone size on both sides and the share of its commuters reaching each zone.

## Running the other denominators

`CZ_DENOMINATOR` selects the count, defaulting to the baseline. `in_scope` restores the restricted row sum and `with_side_work` uses the wider count, and under either the derived objects, tables and figures carry the choice in their names, so no run overwrites the baseline. The wider count has to be built first with `build_side_work_denominator.R`, which reads the attribute-level tabulation from the shared project folder.
