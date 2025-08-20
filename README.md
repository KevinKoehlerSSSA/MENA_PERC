# Political Elites and Regime Change in the Middle East and North Africa (MENA PERC)

This repository contains data associated with the MENA-PERC project.

## Shape files for the 2022/23 Tunisian legislative election districts

There were 161 districts in the 2022/23 parliamentary elections, 151 in Tunisia and 10 abroad. The 151 domestic districts were based on the 2nd-level administrative divisions (delegations, معتمديات). Some of the 264 delegations doubled as electoral districts, while others were merged with adjacent delegations to form larger constituencies. In rare cases, delegations were split into two separate electoral districts, or delegation boundaries were changed.

The shape file is based on the Open Street Map polygons as obtained via [Overpass Turbo](https://overpass-turbo.eu/) with the following query:

```overpass
[out:json][timeout:60];

// get Tunisia as area
{{geocodeArea:Tunisia}}->.searchArea;

// fetch admin boundaries level 8 (municipalities)
relation["boundary"="administrative"]["admin_level"="5"](area.searchArea);

// return full geometry
out geom;
