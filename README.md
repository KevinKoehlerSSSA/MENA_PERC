# Political Elites and Regime Change in the Middle East and North Africa (MENA PERC)

This repository contains data associated with the MENA-PERC project.

## Shape files for the 2022/23 Tunisian legislative election districts

There were 161 districts in the 2022/23 parliamentary elections, 151 in Tunisia and 10 abroad. The 151 domestic districts were based on the 2nd-level administrative divisions (delegations, معتمديات). Some of the 264 delegations doubled as electoral districts, while others were merged with adjacent delegations to form larger constituencies. In rare cases, delegations were split into two separate electoral districts, or delegation boundaries were changed.

The shape file is based on the Open Street Map polygons as obtained via [Overpass Turbo](https://overpass-turbo.eu/) with the following query:

```overpass
[out:json][timeout:60];
{{geocodeArea:Tunisia}}->.searchArea;
relation["boundary"="administrative"]["admin_level"="5"](area.searchArea);
out geom;
```
The Tunisian [Instance supérieure indépendante pour les élections (ISIE)](https://www.isie.tn/ar/%d8%a7%d9%84%d8%ae%d8%b1%d9%8a%d8%b7%d8%a9-%d8%a7%d9%84%d8%a5%d9%86%d8%aa%d8%ae%d8%a7%d8%a8%d9%8a%d8%a9-%d8%a7%d9%84%d8%a7%d9%86%d8%aa%d8%ae%d8%a7%d8%a8%d8%a7%d8%aa-%d8%a7%d9%84%d8%aa%d8%b4/) published PDF files containing Google Maps images of the 151 domestic districts. I used these images to compare distrcit boundaries to that of (combinations of) delegations and adjusted boundaries if necessary. Please not that given the low resolution of the images contained in the PDF files, adjusted district boundaries may not always be precise. 

Here is a map plotting the number of candidates per district:

![Candidates per district](cand_per_districts.png)

