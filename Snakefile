from snakemake.utils import min_version
min_version("5.2")

import os
import hashlib
from snakemake.remote.HTTP import RemoteProvider as HTTPRemoteProvider

HTTP = HTTPRemoteProvider()

configfile: 'config.yml'

localrules: 
  all,
  eclsk_rdsfile,
  eclsk_datfile,
  eclsk_syntaxfile,
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
    'data/eclsk2011_study1/tables',

rule eclsk_rdsfile:
  input:
    dat_file = 'data/src/eclsk8/childk8p.dat.fwf.gz',
    sps_file = 'data/src/eclsk8/ECLSK_Kto8_child_SPSS.sps',
  output:
    'data/src/eclsk8/childk8p.rds'
  conda:
    'envs/eclsk-analysis.yml'
  script:
    'scripts/eclskraw2rds.R'

rule eclsk_datfile:
  input:
    HTTP.remote(expand('nces.ed.gov/ecls/data/Childk8p.z{n}', n=['ip', *['0' + str(i) for i in range(1,6)]])),
  output:
    'data/src/eclsk8/childk8p.dat.fwf.gz'
  conda:
    'envs/eclsk-analysis.yml'
  shell:
    '''
    echo "76bd5f6413c6c2597c301b83b1e7ac20  {input[1]}
          12e0a4154bd231cf97f7da6b4eb53bbb  {input[2]}
          3228d623b09b789408f96537d370e75d  {input[3]}
          4c4fcb0b31bc74e0af04c5a749fc2b8c  {input[4]}
          6a0dbc3dcf096eb2688694901e560333  {input[5]}
          d503ba706fd9806af6508c6a47c80f7c  {input[0]}" | md5sum -c &&
    7z e {input[0]} -so | awk \'BEGIN{{RS="\\r\\n"}} {{line=line $0}} NR%15==0{{print line; line=""}}\' | gzip > {output[0]}
    '''

rule eclsk_syntaxfile:
  input:
    HTTP.remote('nces.ed.gov/ecls/data/ECLSK_Kto8_child_SPSS.sps')
  output:
    'data/src/eclsk8/ECLSK_Kto8_child_SPSS.sps'
  shell:
    '''
    echo "a586f4bd35099a4f92b6e9fb99818daf  {input[0]}" | md5sum -c &&
    cp {input[0]} {output[0]}
    '''

rule eclsk2011_rdsfile:
  input:
    dat_file = 'data/src/eclsk2011k5/childK5p.dat.fwf.gz',
    sps_file = 'data/src/eclsk2011k5/ECLSK2011_K5PUF.sps',
  output:
    'data/src/eclsk2011k5/childK5p.rds'
  conda:
    'envs/eclsk-analysis.yml'
  script:
    'scripts/eclskraw2rds.R'

rule eclsk2011_datfile:
  input:
    HTTP.remote('nces.ed.gov/ecls/data/2019/ChildK5p.zip')
  output:
    'data/src/eclsk2011k5/childK5p.dat.fwf.gz'
  shell:
    '''
    echo "d32a34614dab19a8dc1872b68d636e32  {input[0]}" | md5sum -c &&
    unzip -p {input[0]} childK5p.dat | awk \'BEGIN{{RS="\\r\\n"}} {{line=line $0}} NR%27==0{{print line; line=""}}\' | gzip > {output[0]}
    '''

rule eclsk2011_syntaxfile:
  input:
    HTTP.remote('nces.ed.gov/ecls/data/2019/ECLSK2011_K5PUF.sps')
  output:
    'data/src/eclsk2011k5/ECLSK2011_K5PUF.sps'
  shell:
    '''
    echo "cef3909c6ec9ba504fa261c7b8a18d3f  {input[0]}" | md5sum -c &&
    cp {input[0]} {output[0]}
    '''

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
