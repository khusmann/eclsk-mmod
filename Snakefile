from snakemake.utils import min_version
min_version("5.2")

configfile: 'config.yml'

localrules: all, eclsk2011data, eclsk2011_study1, eclsk2011_study1_tables

STUDY1_MMOD = expand('data/cache/eclsk2011_study1/{model}_result.rds', model=config['studies']['eclsk2011_study1']['models']),

rule all:
  input:
     'data/eclsk2011_study1/eclsk2011_study1.html',
     'data/eclsk2011_study1/tables'

rule eclsk2011_study1:
   input:
      STUDY1_MMOD,
   output:
     'data/eclsk2011_study1/eclsk2011_study1.html'
   conda:
     'envs/eclsk-analysis.yml'
   script:
     'eclsk2011_study1.Rmd'

rule eclsk2011_study1_tables:
   input:
      STUDY1_MMOD,
   output:
     directory('data/eclsk2011_study1/tables')
   conda:
     'envs/eclsk-analysis.yml'
   script:
     'scripts/eclsk2011_study1_tables.R'

rule eclsk2011data:
  input:
    'data/src/eclsk2011k5/ECLSK2011_K5PUF.sav'
  output:
    'data/eclsk2011.rda'
  conda:
    'envs/eclsk-analysis.yml'
  log:
    'data/cache/eclsk2011_subset.rds',
    'data/cache/eclsk2011_vars.rds',
  script:
    'scripts/eclsk2011data.R'

rule run_mmods:
  input:
    'data/eclsk2011.rda'
  output:
    'data/cache/{study}/{model}_result.rds'
  params:
    measures=lambda wildcards: config['studies'][wildcards.study]['models'][wildcards.model],
    data=lambda wildcards: config['studies'][wildcards.study]['data'],
    subset=lambda wildcards: config['studies'][wildcards.study]['subset'],
    split=lambda wildcards: config['studies'][wildcards.study]['split'],
    model_save='data/cache/{study}/{model}_model.rds',
  resources:
    mem_mb=4000,
    walltime_min=8*60
  threads: 8
  conda:
    'envs/eclsk-analysis.yml'
  script:
    'scripts/run_mmod.R'
