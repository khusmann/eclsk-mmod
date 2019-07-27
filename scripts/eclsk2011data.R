source('eclsk2011env.R')

library(OpenMx)
library(labelled)

df_rawraw <- read_rds(SUBSETFILE)

neg_to_na <- function(x) {na_range(x) <- c(-Inf, 0); user_na_to_na(x)}

df_raw <- df_rawraw %>%
          mutate_if(is.numeric, neg_to_na) %>%
          mutate(
            CHILDID = to_factor(CHILDID),
            X_CHSEX_R = to_factor(X_CHSEX_R),
            X_RACETHP_R = to_factor(X_RACETHP_R),
            X1FIRKDG = to_factor(X1FIRKDG),
          ) %>%
          mutate_if(is.factor, factor) %>%
          mutate_if(is.numeric, as.numeric) %>%
          mutate_at(vars(one_of(allOccasionsFor(four_level_measures))), ~ifelse(. > 4, NA, .)) %>%
          mutate_at(vars(one_of(allOccasionsFor(seven_level_measures))), ~ifelse(. > 8, NA, .)) %>%
          mutate_at(vars(one_of(allOccasionsFor(all_measures))), scale)
         
df_all <- df_raw %>%
  filter(X1FIRKDG == '1: YES') %>%
  filter(S1_ID == S2_ID) %>%
  filter(S1_ID == S4_ID)

set.seed(9001)
df_train <- df_all %>% sample_frac(0.5)
df_test <- df_all %>% anti_join(df_train, by='CHILDID')

mmodModel = function(measures, modelName, fiml=F) {
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
    mxd <- mxData(df_test[def$manifests], type="raw")
  } else {
    df_subset <- na.omit(df_test[def$manifests])
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


