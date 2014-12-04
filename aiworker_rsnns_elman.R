#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# -- aiworker.com
# Input: data and list of N inputs parameters
#        name of ONE output parameter
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
aiworker_rsnns_elman = function(Data,InputPar,OutputPar,fcStartPoint,
                                elman.epoch, elman.size)
{
#   elman.epoch = 10000; elman.size = c(5,5)
  #************************************************************
  #************************************************************
  # -- normilize x to range [normLow, normHigh]
  aiworker_data_Normalization = function(x, normLow, normHigh){
    z = ( (x-min(x)) / (max(x) - min(x)) ) * (normHigh - normLow) + normLow
    return(z);
  }
  
  # -- de-normilize x.norm using [normLow, normHigh], min(x) and max(x)
  aiworker_data_DeNormalization = function(x.norm, x, normLow, normHigh){
    z = ( ( (min(x) - max(x)) * x.norm) - normHigh * min(x) + max(x) * normLow) / (normLow - normHigh)          
    return(z);
  }
  
  #************************************************************
  library(RSNNS); library(chron)
  #************************************************************
  start.time = Sys.time()
  #************************************************************
  # -- get index of History/Forecast boundary
  HistMonthEndIndex = which(Data$Date == fcStartPoint) - 1  
  #************************************************************
    # -- build inputs/target dataframes
  inputs    = data.matrix(Data[1:HistMonthEndIndex, InputPar])
  inputs.fc = data.matrix(Data[HistMonthEndIndex+1 : nrow(Data), InputPar])
  targets   = data.frame(as.numeric(data.matrix(Data[1:HistMonthEndIndex, OutputPar])) )
  names(targets) = OutputPar
  
    # -- normilize inputs/target by columns
  inputs.norm = inputs
  for(i in 1:ncol(inputs.fc) ){
    inputs.norm[,i] = t(aiworker_data_Normalization(inputs[,i],0,1) )
  }
  
  inputs.fc.norm = inputs.fc
  for(i in 1:ncol(inputs.fc) ){
    inputs.fc.norm[,i] = t(aiworker_data_Normalization(inputs.fc[,i],0,1) )
  }
  
  targets.norm = aiworker_data_Normalization(targets,0,1) 
  #************************************************************
    # -- run ELMAN based Recurrent Neural Network
  elman.model = elman(inputs.norm, targets.norm, size = elman.size, maxit = elman.epoch,
                      initFuncParams = c(0.0), learnFuncParam = c(0.1), updateFuncParam = c(0.0),
                      initFunc = "JE_Weights", updateFunc = "JE_Order", linOut = T, shufflePatterns = T)
    
  elman.fit = aiworker_data_DeNormalization(elman.model$fitted.values, targets, 0, 1)
  elman.fc  = aiworker_data_DeNormalization(predict(elman.model, inputs.fc.norm), targets, 0, 1)
  
    # -- create resulting dataset
  elman.res = NULL
  elman.res$Date = Data$Date
  elman.res = data.frame(elman.res)
  elman.res$type = "elman"
  elman.res[1:HistMonthEndIndex, OutputPar] = elman.fit
  elman.res[(HistMonthEndIndex+1):nrow(elman.res), OutputPar] = elman.fc
 
  #************************************************************
    # -- calculate function execution time
  end.time = sys.time()
  time.taken = as.chron(as.character(end.time), format='%Y-%m-%d %H:%M:%S') - 
               as.chron(as.character(start.time), format='%Y-%m-%d %H:%M:%S')  
    
  print(paste0("-- aiworker_rsnns_elman: return ELMAN (RNN) FIT/FC | execTime: ", time.taken," (hh:mm:ss)"))
  
  return(elman.res)  
}