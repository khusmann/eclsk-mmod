from snakemake.utils import min_version
min_version("5.2")

import os
import hashlib
from snakemake.remote.HTTP import RemoteProvider as HTTPRemoteProvider

HTTP = HTTPRemoteProvider()

def check_md5(f, md5sum):
  assert hashlib.md5(open(f, 'rb').read()).hexdigest() == md5sum, 'MD5 check failure'
  
configfile: 'config.yml'

localrules: 
  all,
  eclsk2011_rdsfile,
  eclsk2011_datfile,
  eclsk2011_syntaxfile,
  eclsk2011data,
  eclsk2011_study1,
  eclsk2011_study1_tables

STUDY1_MMOD = expand('data/cache/eclsk2011_study1/{model}_result.rds', model=config['studies']['eclsk2011_study1']['models']),

rule all:
  input:
    'data/eclsk2011_study1/eclsk2011_study1.html',
    'data/eclsk2011_study1/tables'

rule eclsk2011_rdsfile:
  input:
    'data/src/eclsk2011k5/childK5p.dat.fwf.gz',
    'data/src/eclsk2011k5/ECLSK2011_K5PUF.sps'
  output:
    'data/src/eclsk2011k5/childK5p.rds'
  conda:
    'envs/eclsk-analysis.yml'
  script:
    'scripts/eclsk2011raw2rds.R'

rule eclsk2011_datfile:
  input:
    HTTP.remote('nces.ed.gov/ecls/data/2019/ChildK5p.zip')
  output:
    'data/src/eclsk2011k5/childK5p.dat.fwf.gz'
  run:
    check_md5(input[0], 'd32a34614dab19a8dc1872b68d636e32')
    shell('unzip -p {input[0]} childK5p.dat | awk \'BEGIN{{RS="\\r\\n"}} {{line=line $0}} NR%27==0{{print line; line=""}}\' | gzip > {output[0]}')

rule eclsk2011_syntaxfile:
  input:
    HTTP.remote('nces.ed.gov/ecls/data/2019/ECLSK2011_K5PUF.sps')
  output:
    'data/src/eclsk2011k5/ECLSK2011_K5PUF.sps'
  run:
    check_md5(input[0], 'cef3909c6ec9ba504fa261c7b8a18d3f')
    shell('cp {input[0]} {output[0]}')

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
    'data/src/eclsk2011k5/childK5p.rds'
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
