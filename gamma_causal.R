gammav = function(theta, y, v) {
  
  # This function computes the causal effect of do(Xv = x) on y
  # from the DAG-parameter theta (a collection of node-conditional probabilities)
  
  res = 0
  pa = theta[[v]]$parents
  eventStr = sprintf("(%s == 1)", y)
  if (length(pa)== 0) {
    cp1 = sprintf("cpquery(theta, event = %s, evidence = ((%s == 1)))", eventStr, v)
    cp2 = sprintf("cpquery(theta, event = %s, evidence = ((%s == 0)))", eventStr, v)
    res = eval(parse(text=cp1)) - eval(parse(text=cp2))
  } else { # 2 or more parents
    xxx = theta[[v]]$prob
    tab = apply(xxx, seq(2, length(dim(xxx))), sum)
    if (length(pa)==1) {
      df = data.frame(pa = names(tab))
      paname = pa
    } else {
      df = as.data.frame.table(tab)[,1:length(pa)]
      #paname = names(df[i,1:ncol(df)])
      paname = names(df[,1:ncol(df)])
    }
    for (i in 1:nrow(df)) {
      evidenceStr = ""
      for (j in 1:ncol(df)) {
        val = df[i, j]
        n = paname[j]
        if (evidenceStr == "") {
          evidenceStr = sprintf("(%s == %s)", n, val)
        } else {
          evidenceStr = sprintf("%s & (%s == %s)", evidenceStr, n, val)
        }
      }
      cp1 = sprintf("cpquery(theta, event = %s, evidence = ((%s == 1) & %s))", eventStr, v, evidenceStr)
      cp2 = sprintf("cpquery(theta, event = %s, evidence = ((%s == 0) & %s))", eventStr, v, evidenceStr)
      cp3 = sprintf("cpquery(theta, event = (%s), evidence = TRUE)", evidenceStr)
      res = res + (eval(parse(text=cp1)) - eval(parse(text=cp2))) * eval(parse(text=cp3))
    }
  }
  return(res)
}
