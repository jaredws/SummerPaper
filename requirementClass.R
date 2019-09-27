## Requirement Class

library(FuzzyNumbers)

## Create an object that holds 4 values in less space than FuzzyNumbers

Requirement <- setClass(
  Class = "Requirement",
  slots = c(
    support1 = "numeric",
    core1 = "numeric",
    core2 = "numeric",
    support2 = "numeric"
  )
)

setMethod("initialize","Requirement",
          function(.Object,s1,c1,c2,s2){
            .Object@support1 = s1
            .Object@core1 = c1
            .Object@core2 = c2
            .Object@support2 = s2
            .Object
          })

setMethod("show","Requirement",
          function(object){
            s <- paste0("( ",object@support1, ", ",object@core1, ", ",object@core2, ", ", object@support2, ")")
            print(s,quote= FALSE)
            return(s)
          })

## Create a function to quickly create a PiecewiseLinearFuzzyNumber representaiton of the 
## requirement
setGeneric("as.plFN",
           function(requirement){
             standardGeneric("as.plFN")
           }
)
setMethod("as.plFN",
          signature("Requirement"),
          function(requirement){
            fn <- PiecewiseLinearFuzzyNumber(requirement@support1,requirement@core1,requirement@core2,requirement@support2)
            return (fn)
          })


## A helper function for calculating the market average fuzziness
setGeneric("sumRequirements",
           function(r1, r2){
             standardGeneric("sumRequirements")
           }
)

setMethod("sumRequirements",
          signature("Requirement", "Requirement"),
          function(r1, r2){
            sum <- Requirement(
              r1@support1 + r2@support1,
              r1@core1    + r2@core1,
              r1@core2    + r2@core2,
              r1@support2 + r2@support2
            )
            return (sum)
          })

## After suming many requirments as above,
## Calculate their average here
setGeneric("averageRequirements",
           function(avReq, num){
             standardGeneric("averageRequirements")
           }
)
setMethod("averageRequirements",
          signature("Requirement","numeric"),
          function(avReq, num){
            result <- Requirement(
              avReq@support1/num,
              avReq@core1/num,
              avReq@core2/num,
              avReq@support2/num
            )
            return (result)
          })


## Increase a requirement by a given percentage as a decimal
setGeneric("percentIncrease",
           function(req, num){
             standardGeneric("percentIncrease")
           }
)
setMethod("percentIncrease",
          signature("Requirement","numeric"),
          function(req, num){
            num <- num + 1
            result <- Requirement(
              req@support1*num,
              req@core1*num,
              req@core2*num,
              req@support2*num
            )
            return (req)
          })