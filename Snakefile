configfile: 'config.yml'

localrules: all, eclsk2011data

rule all:
  input:
    expand('data/res/eclsk2011_study1/{model}_result.rds', model=config['studies']['eclsk2011_study1']['models']),
    expand('data/res/eclsk2011_study2/{model}_result.rds', model=config['studies']['eclsk2011_study2']['models']),

rule eclsk2011data:
  input:
    'data/src/eclsk_2011_childk4.sav'
  output:
    'data/eclsk2011.rda'
  conda:
    'envs/eclsk-analysis.yml'
  script:
    'scripts/eclsk2011data.R'

rule run_mmods:
  input:
    'data/eclsk2011.rda'
  output:
    'data/res/{study}/{model}_result.rds'
  params:
    measures=lambda wildcards: config['studies'][wildcards.study]['models'][wildcards.model],
    data=lambda wildcards: config['studies'][wildcards.study]['data'],
    subset=lambda wildcards: config['studies'][wildcards.study]['subset'],
    split=lambda wildcards: config['studies'][wildcards.study]['split'],
    model_save='data/res/{study}/{model}_model.rds',
  resources:
    mem_mb=4000,
    walltime_min=8*60
  threads: 8
  conda:
    'envs/eclsk-analysis.yml'
  script:
    'scripts/run_mmod.R'
