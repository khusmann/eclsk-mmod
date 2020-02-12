source('paper/scripts/common.R')

(total_sample_size <- n_distinct(eclsk2011$study1$CHILDID))
(training_sample_size <- n_distinct(filter(eclsk2011$study1, split=='train')$CHILDID))
(testing_sample_size <- n_distinct(filter(eclsk2011$study1, split=='test')$CHILDID))
(validation_sample_size <- n_distinct(filter(eclsk2011$study1, split=='val')$CHILDID))

cat(
  sprintf('\\newcommand\\totalsamplesize{%s}\n', total_sample_size),
  sprintf('\\newcommand\\trainingsamplesize{%s}\n', training_sample_size),
  sprintf('\\newcommand\\testingsamplesize{%s}\n', testing_sample_size),
  sprintf('\\newcommand\\validationsamplesize{%s}\n', validation_sample_size),
  file='paper/tables/sample_sizes.tex'
)