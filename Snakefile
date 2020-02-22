configfile: 'config.yml'

localrules: all, eclsk2011data, eclsk2011_study1, eclsk2011_study1_tables

rule all:
  input:
     'data/res/eclsk2011_study1.html',
     'data/res/eclsk2011_study1/tables'

rule eclsk2011_study1:
   input:
     expand('data/res/eclsk2011_study1/{model}_result.rds', model=config['studies']['eclsk2011_study1']['models']),
   output:
     'data/res/eclsk2011_study1.html'
   conda:
     'envs/eclsk-analysis.yml'
   script:
     'eclsk2011_study1.Rmd'

rule eclsk2011_study1_tables:
   input:
      rules.eclsk2011_study1.output
   output:
     directory('data/res/eclsk2011_study1/tables')
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
