snakemake --use-conda --cluster 'qsub -k eo -m n -A open -l walltime=5:00:00' -j 25 -w 90
