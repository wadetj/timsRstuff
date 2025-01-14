#' Backwards stepwise model  selection using AIC, accounting for missing data
#'
#' Selects best-fitting model, minimizing AIC accounting for missing
#' values. Accounts for missing variables in two stages. 
#' Excludes missing observations from variables in base model, selects model.
#' Excludes missing observations from variables retained in selected model.
#' Refits model and selects model again. Then refits model to full data set.
#' Calls -stepAIC- from MASS library
#'
#' @param model object of class "glm"
#' @param include character vector of variables excluded from selection
#' @param detail 1 or 0 (default 1). Shows selection detail (1=Yes, 0=No, see trace from -stepAIC)
#' @return "glm" object
#' @examples
#' utils::data(survey, package = "MASS")
#' survey2<-survey
#' #add some missing data
#' survey2$Wr.Hnd[1:5]<-NA
#' otemp<-glm(Sex~Wr.Hnd+NW.Hnd+Fold+Pulse+Smoke+Height, data=survey2, family=binomial)
#' summary(otemp)
#' mod1<-select_model(otemp)
#' summary(mod1)
#' mod2<-select_model(otemp, include=c("Smoke", "Pulse"))
#' summary(mod2)
#' @export


select_model<-function(model, include=NULL, detail=1) {
  form1<-model$formula
  fam<-model$family
  dat<-model$data
  mvar1<-all.vars(getCall(model)$formula)
  nomiss1<-dplyr::select(dat, all_of(mvar1))
  nomiss1<-na.omit(nomiss1)
  model1<-glm(form1, family=fam, data=nomiss1)
  if(is.null(include)) {
    ostep1<-MASS::stepAIC(model1, trace=detail)
    form2<-ostep1$formula
    mvar2<-all.vars(getCall(ostep1)$formula)
    nomiss2<-dplyr::select(dat, all_of(mvar1))
    nomiss2<-na.omit(nomiss2)
    model2<-glm(form2, family=fam,data=nomiss2)
    ostep2<-MASS::stepAIC(model2, trace=detail)
   form3<-ostep2$formula
   ofull<-glm(form3, family=fam, data=dat)
   return(ofull)
  }
  if(!is.null(include)) {
    keepform<-as.formula(paste("~", paste(include, collapse="+")))
    ostep1<-MASS::stepAIC(model1,  scope=list(lower=keepform), trace=detail)
    form2<-ostep1$formula
    mvar2<-all.vars(getCall(ostep1)$formula)
    nomiss2<-dplyr::select(dat, all_of(mvar1))
    nomiss2<-na.omit(nomiss2)
    model2<-glm(form2, family=fam,data=nomiss2)
    ostep2<-MASS::stepAIC(model2, scope=list(lower=keepform), trace=detail)
    form3<-ostep2$formula
    ofull<-glm(form3, family=fam, data=dat)                     
    return(ofull)
  }
}


  



