configfile: 'config.yml'

localrules: all, subset_2011, make_mmod_2011

rule all:
  input:
    expand('data/eclsk2011_study1/mmod/{model}_result.rds', model=config['studies']['eclsk2011_study1']['models']),
    expand('data/eclsk2011_study2/mmod/{model}_result.rds', model=config['studies']['eclsk2011_study2']['models'])

rule subset_2011:
  input:
    'data/raw/eclsk_2011_childk4.sav'
  output:
    'data/eclsk_subset_2011.rds'
  conda:
    'envs/eclsk-analysis.yml'
  script:
    'scripts/eclsk2011subset.R'

rule make_mmod_2011:
  input:
    'data/eclsk_subset_2011.rds'
  output:
    'data/{study}/mmod/{model}_model.rds'
  params:
    measures=lambda wildcards: config['studies'][wildcards.study]['models'][wildcards.model],
    occasions=lambda wildcards: config['studies'][wildcards.study]['occasions'],
    data=lambda wildcards: wildcards.study
  conda:
    'envs/eclsk-analysis.yml'
  script:
    'scripts/make_mmod.R'

rule run_mmod_2011:
  input:
    'data/{study}/mmod/{model}_model.rds'
  output:
    'data/{study}/mmod/{model}_result.rds'
  resources:
    mem_mb=4000,
    walltime_min=4*60
  conda:
    'envs/eclsk-analysis.yml'
  threads: 8
  script:
    'scripts/run_mmod.R'
