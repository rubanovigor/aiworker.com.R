#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# -- aiworker.com
# Input: data and list of N inputs parameters
#        name of ONE output parameter
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
aiworker_rsnns_rbf = function(Data,InputPar,OutputPar,fcStartPoint,
                                rbf.epoch, rbf.size)
{
#   rbf.epoch = 1000; rbf.size = 8
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
    # -- run rbf based Neural Network
  rbf.model = rbf(inputs.norm, targets.norm, size = rbf.size, maxit = rbf.epoch,
                  initFunc = "RBF_Weights", initFuncParam = c(0,1,0,0.1,0.1), inOut = T)
    
  rbf.fit = aiworker_data_DeNormalization(fitted(rbf.model), targets, 0, 1)
  rbf.fc  = aiworker_data_DeNormalization(predict(rbf.model, inputs.fc.norm), targets, 0, 1)
  
    # -- create resulting dataset
  rbf.res = NULL
  rbf.res$Date = Data$Date
  rbf.res = data.frame(rbf.res)
  rbf.res$type = "rbf"
  rbf.res[1:HistMonthEndIndex, OutputPar] = rbf.fit
  rbf.res[(HistMonthEndIndex+1):nrow(rbf.res), OutputPar] = rbf.fc
 
  #************************************************************
    # -- calculate function execution time
  end.time = sys.time()
  time.taken = as.chron(as.character(end.time), format='%Y-%m-%d %H:%M:%S') - 
               as.chron(as.character(start.time), format='%Y-%m-%d %H:%M:%S')  
    
  print(paste0("-- aiworker_rsnns_rbf: return rbf FIT/FC | execTime: ", time.taken," (hh:mm:ss)"))
  
  return(rbf.res)  
}