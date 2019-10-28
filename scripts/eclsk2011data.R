source('scripts/eclsk2011env.R')

library(OpenMx)
library(labelled)

eclsk2011data <- list()

eclsk2011data$raw <- read_rds(SUBSETFILE)

eclsk2011data$all <- eclsk2011data$raw %>%
                     mutate_if(is.numeric, function(x) {na_range(x) <- c(-Inf, 0); user_na_to_na(x)}) %>% # Neg values to NA
                     mutate_at(vars(one_of(eclsk2011measures$factors)), to_factor) %>%
                     mutate_at(vars(one_of(eclsk2011measures$four_level)), ~ifelse(. > 4, NA, .)) %>%
                     mutate_at(vars(one_of(eclsk2011measures$five_level)), ~ifelse(. > 5, NA, .)) %>%
                     mutate_at(vars(one_of(eclsk2011measures$seven_level)), ~ifelse(. > 7, NA, .)) %>%
#                     mutate_at(vars(one_of(c(eclsk2011measures$four_level,
#                                             eclsk2011measures$five_level,
#                                             eclsk2011measures$seven_level))), scale) %>%
                     mutate_if(is.factor, factor) %>%
                     mutate_if(is.numeric, as.numeric)

eclsk2011data$eclsk2011_study1 <- eclsk2011data$all %>%
                                  filter(X1FIRKDG == '1: YES') %>%
                                  filter(S1_ID == S2_ID) %>%
                                  filter(S1_ID == S4_ID)


eclsk2011data$eclsk2011_study2 <- eclsk2011data$all %>%
                                  filter(X1FIRKDG == '1: YES') %>%
                                  filter(S6_ID == S7_ID) %>%
                                  filter(S6_ID == S8_ID)

set.seed(9001)
eclsk2011data$eclsk2011_study1_train <- eclsk2011data$eclsk2011_study1 %>% sample_frac(0.5)
eclsk2011data$eclsk2011_study1_test  <- eclsk2011data$eclsk2011_study1 %>% anti_join(eclsk2011data$eclsk2011_study1_train, by='CHILDID')

eclsk2011data$eclsk2011_study2_train <- eclsk2011data$eclsk2011_study2 %>% sample_frac(0.5)
eclsk2011data$eclsk2011_study2_test  <- eclsk2011data$eclsk2011_study2 %>% anti_join(eclsk2011data$eclsk2011_study2_train, by='CHILDID')

##############################3

make_tall <- function(df) {
  df %>%
    select(-one_of(eclsk2011measures$strings)) %>%
    gather(meas, value, -one_of(eclsk2011measures$factors)) %>%
    mutate(occasion = as.numeric(str_sub(meas, 2, 2)),
           meas = str_replace(meas, '\\d+', '')) %>%
    spread(meas, value)
}

eclsk2011data$tall <- make_tall(eclsk2011data$all)


eclsk2011data$eclsk2011_study1_tall <- make_tall(eclsk2011data$eclsk2011_study1) %>%
                                       filter(occasion %in% c(1,2,4))

eclsk2011data$eclsk2011_study2_tall <- make_tall(eclsk2011data$eclsk2011_study2) %>%
                                       filter(occasion %in% c(6,7,8))

eclsk2011data$eclsk2011_study1_train_tall <- make_tall(eclsk2011data$eclsk2011_study1_train) %>%
                                             filter(occasion %in% c(1,2,4))

eclsk2011data$eclsk2011_study1_test_tall <- make_tall(eclsk2011data$eclsk2011_study1_test) %>%
                                            filter(occasion %in% c(1,2,4))

eclsk2011data$eclsk2011_study2_train_tall <- make_tall(eclsk2011data$eclsk2011_study2_train) %>%
                                             filter(occasion %in% c(6,7,8))

eclsk2011data$eclsk2011_study2_test_tall <- make_tall(eclsk2011data$eclsk2011_study2_test) %>%
                                            filter(occasion %in% c(6,7,8))

compare_df <- function(df1, df2) {
  for (i in names(df1)) {
    if (i %in% names(df2)) {
      diff_vals <- sum(df1[i] != df2[i], na.rm=T)
      if (is.numeric(df1[i])) {
        diff_vals <- sum(abs(df1[i] - df2[i]) < 1e-7, na.rm=T)  
      }
      diff_nas <- sum(is.na(df1[i]) != is.na(df2[i]), na.rm=T)
      print(paste(i, 'diff_vals:', diff_vals, 'diff_nas:', diff_nas))
    } else {
      print(paste(i, 'not in df2'))
    }
  }
}

run_tests <- function() {
  print("all")
  compare_df(eclsk2011$subset_tall, eclsk2011data$tall)
  print("study 1")
  compare_df(eclsk2011$study1, eclsk2011data$eclsk2011_study1_tall)
  print("study 2")
  compare_df(eclsk2011$study2, eclsk2011data$eclsk2011_study2_tall)
  print("study 1 train")
  compare_df(filter(eclsk2011$study1, split == 'train'), eclsk2011data$eclsk2011_study1_train_tall)
  print("study 1 test")
  compare_df(filter(eclsk2011$study1, split == 'test'), eclsk2011data$eclsk2011_study1_test_tall) 
  print("study 2 train")
  compare_df(filter(eclsk2011$study2, split == 'train'), eclsk2011data$eclsk2011_study2_train_tall)
  print("study 2 test")
  compare_df(filter(eclsk2011$study2, split == 'test'), eclsk2011data$eclsk2011_study2_test_tall)   
}

inspect_diff <- function(df1, df2, item) {
  print(df2[df1[item] != df2[item]])
}
 
###########################################33

mmodModel <- function(data, measures, occasions, modelName, fiml=F) {
  derivName <- function(o, m) {
    # derivName(1, 'TKEEPS) -> d1TKEEPS
    str_c('d', o, m)
  }
  
  # factorStruct: list(F1 = c('d1KEEPS', 'd1SHOWS', ...), F2 = c('d2KEEPS', 'd2SHOWS', ...))
  factorStruct <- unlist(map(occasions, function(o) {
    tmp <- map(measures, ~derivName(o, .)) # Create all derivs under factor
    names(tmp) <- str_c(names(tmp), o) # Append occasion to factor name
    tmp
  }), recursive=F)

  # derivStruct: list(d1KEEPS=c('T1KEEPS', 'T2KEEPS', 'T4KEEPS'), d1SHOWS=c('T1SHOWS', ...))  
  derivStruct <- map(occasions, function(o) {
    measures_flat <- unlist(measures, use.names=F)
    tmp <- map(measures_flat, function(m) {
      map_chr(occasions, ~itemName(., m))
    })
    names(tmp) <- derivName(o, measures_flat)
    tmp
  })
  
  def <- list(
    factorStruct = factorStruct,
    derivStruct = derivStruct,
    factors = names(factorStruct),
    derivatives = unlist(factorStruct, use.names=F),
    manifests = unique(unlist(derivStruct))   
  )

  if (fiml) {
    mxd <- mxData(data[def$manifests], type="raw")
  } else {
    df_subset <- na.omit(data[def$manifests])
    #df_cors <- polychoric(df_subset)$rho
    df_cors <- cor(df_subset)
    mxd <- mxData(df_cors, type="cov", numObs=nrow(df_subset))
  }

  # Make weight matrix without Deboeck’s functions
  weight <- matrix(c(1/3, 1/3, 1/3,
                     -1,   0, 1 ,
                     1/2,  -1, 1/2),
                   nrow=3, ncol=3)
  weightList <- as.list(as.data.frame(t(solve(weight))))
  
  do.call('mxModel', c(list(
    modelName, mxd, type="RAM",
    manifestVars=def$manifests,
    latentVars=c(def$factors, def$derivatives),
    # factor loadings
    unname(imap(def$factorStruct, ~mxPath(from=.y, to=.x, values=0.5, free=T))),
    # factor variances
    mxPath(from=def$factors, arrows=2, values=1, free=F),
    # factor correlations
    mxPath(from=def$factors, arrows=2, connect="unique.bivariate", free=T),
    # residual variances(only for latent derivatives !)
    mxPath(from=def$derivatives, arrows=2, values=1)),
    # transformation
    unname(map2(def$derivStruct, weightList, function (deriv, weight) {
      imap(deriv, ~mxPath(from=.y, to=.x, free=F, values=weight))
    })),
    # saturate model
    if (fiml) mxPath(from = 'one', to = def$manifests) else list()
  ))
}

cfaModel <- function(data, measures, modelName, occasion, fiml=F) {
  factorStruct <- map(measures, ~itemName(occasion, .))
  def <- list(
    factorStruct = factorStruct,
    factors = names(factorStruct),
    manifests = unlist(factorStruct, use.names=F)
  )
  
  if (fiml) {
    mxd <- mxData(data[def$manifests], type='raw')
  } else {
    df_subset <- na.omit(data[def$manifests])
    #df_cors <- polychoric(df_subset)$rho
    df_cors <- cor(df_subset)
    mxd <- mxData(df_cors, type='cov', numObs=nrow(df_subset))
  }

  do.call('mxModel', c(list(
    modelName, mxd, type="RAM",
    manifestVars=def$manifests,
    latentVars=def$factors,
    # factor loadings
    unname(imap(def$factorStruct, ~mxPath(from=.y, to=.x, values=0.5, free=T))),
    # factor variances
    mxPath(from=def$factors, arrows=2, values=1, free=F),
    # factor correlations
    mxPath(from=def$factors, arrows=2, connect="unique.bivariate", free=T),
    # residual variance
    mxPath(from=def$manifests, arrows=2, values=1)),
    # means
    if (fiml) mxPath(from = 'one', to = def$manifests) else list()
  ))
}
