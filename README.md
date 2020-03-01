# eclsk-mmod

## 1) Create ECLSK2011\_K5PUF.sav

Download ECLS-K:2011 ASCII Data File: [ChildK5p.zip](https://nces.ed.gov/ecls/data/2019/ChildK5p.zip) 

Download SPSS syntax file: [ECLSK2011\_K5PUF.sps](https://nces.ed.gov/ecls/data/2019/ECLSK2011_K5PUF.sps)

Follow the instructions at the [NCES Data Products page](https://nces.ed.gov/ecls/dataproducts.asp)

## 2) Place ECLSK2011\_K5PUF.sav in data/src/eclsk2011k5

```
mkdir -p data/src/eclsk2011k5
mv ECLSK2011_K5PUF.sav data/src/eclsk2011k5
```

## 3) Run pipeline

Launch locally: snakemake --profile profiles/local

Launch on aci cluster (Penn State University): snakemake --profile profiles/aci

---

The research reported here was supported, in whole or in part, by the Institute of Education Sciences, U.S. Department of Education, through grant R305B090007 to The Pennsylvania State University. The opinions expressed are those of the authors and do not represent the views of the Institute or the U.S. Department of Education
