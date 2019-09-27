## Performance all functions.
## loaded separately in file to reduce amount of code need to go through 
## to perform diagnostics

#### Functions ####
get_return_data <- function(x, y = TRUE) {
  r.data <- ssbt.data %>% filter(ssbtID %in% x)
  if (y == TRUE) {r.data <- tbl_xts(r.data)}
  return(r.data)
}
get_bench_data <- function(x, y = TRUE){
  b.id <- benchmark.map %>% 
    filter(ssbtID %in% x) %>%
    select(benchID)
  b.data <- benchmark.data %>% 
    filter(benchID %in% b.id) %>%
    select(Date, Return)
  if (y == TRUE) {b.data <- tbl_xts(b.data)}
  return(b.data)
}
get_shortName <- function(x){
  shortName.df <-   bind_rows(
    policy.tree %>% select(ssbtID, shortName),
    composite.tree %>% transmute(ssbtID = SSBT_Composite_ID, shortName))
  name.vector <- vector()
  for (i in 1:length(x)) {
    name <- shortName.df %>% 
      filter(ssbtID == x[i]) %>% 
      select(shortName) %>% 
      unlist()
    if (length(name) == 0 | sum(!is.na(name)) == 0) {
      name.vector[i] <- "ID not found"
    } else {
      name.vector[i] <- name
    }
  }
  return(name.vector)
}
pestat_multibench=function(cfl,indl) {
  #cfl is a list of cash flows
  #indl is a list of total return indices
  if(length(cfl)!=length(indl)) stop("length of cash flow and index lists not the same")
  n=length(cfl)
  fv1=cfl[[1]]*0
  for(i in 1:n) {
    fv2=coredata(lastinvec(indl[[i]]))/indl[[i]]
    fv3=cfl[[i]]*fv2
    fv1=mergesum.z(fv1,fv3)
  }
  pme.c=-sum(fv1[fv1>0])/sum(fv1[fv1<0])
  alpha.c=log(1+irr.z(fv1,gips = TRUE))
  irr=irr.z(do.call(mergesum.z,cfl), gips = TRUE)
  ind.irr=-1+exp(log(1+irr)-alpha.c)
  pme=-sum(fv1[fv1>0])/sum(fv1[fv1<0])
  ans=list()
  ans$irr=irr
  ans$ind.irr=ind.irr
  ans$pme=pme
  ans$tvpi=tvpi(do.call(mergesum.z,cfl))
  ans$pme.wealthdiff = sum(fv1)
  return(ans)
}
get_nav_cf <- function(x, sumRows = TRUE) {
  #x is a vector of funds
  mv.all <- list()
  cf.all <- list()
  for (i in x) {
    if (is.null(y.hv[[i]]) == TRUE) next
    mv.all[[i]] <- rbind.zoo(y.hv[[i]], y.v[[i]])
    cf.all[[i]] <- y.cf[[i]]
  }
  pcaps <- do.call(merge, mv.all) %>% xts(.)
  pcaps <- na.fill(pcaps, 0)
  if (sumRows == TRUE) {pcaps <- xts(rowSums(pcaps),time(pcaps))}
  cfs <- do.call(merge, cf.all) %>% xts(.)
  cfs <- na.fill(cfs, 0)
  if (sumRows == TRUE) {cfs <- xts(rowSums(cfs), time(cfs))}
  return(list("NAV" = pcaps, "CF" = cfs))
}
get_combineval_irr <- function(cf, val, beg.date = NULL, last.date = valdate){
  #cf is an xts object of cash flows
  #val is an xts object of NAVs
  #if beg.date is null, it calculates ITD
  if(is.null(beg.date) == TRUE) {
    e.mv <- val[time(val) == last.date]
    cf.sub <- cf[time(cf) <= last.date]
    combineval <- mergesum.z(cf.sub, e.mv)
  } else {
    b.mv <- val[time(val) == beg.date]
    e.mv <- val[time(val) == last.date]
    cf.sub <- cf[time(cf) <= last.date & time(cf) > beg.date]
    combineval <- mergesum.z(-b.mv, cf.sub, e.mv)}
  return(combineval)
}
bw.cone <- function(return, exp.return, exp.sd){
  start.date = index(return)[1]
  
  # Calculate vectors for the actual return, expected return and expected StdDev
  r.act <- cumprod(1 + return)
  r.exp <- cumprod(rep((exp.return+1) ^ (1/12),nrow(return)))
  sd.one <- exp.sd*sqrt((1:nrow(return)/12))
  
  #merge data and add data for +/- StdDev bands
  fund.data = cbind(r.act,r.exp)
  colnames(fund.data) = c("Actual", "Expected")
  fund.data$up.one = fund.data$Expected * (1 + sd.one)
  fund.data$up.two = fund.data$Expected * (1 + 2*sd.one)
  fund.data$dn.one = fund.data$Expected / (1 + sd.one)
  fund.data$dn.two = fund.data$Expected / (1 + 2*sd.one)
  
  # Make the first month start with 1's
  one.row = xts(matrix(rep(1, ncol(fund.data)), ncol = ncol(fund.data)), start.date + 1 - months(1) -1)
  fund.data = rbind(fund.data, one.row)
  
  # Run actual return last to paint over bands
  plot.cone = ggplot(
    data = fortify.zoo(fund.data[,c(2:6,1)], melt = TRUE),
    aes(x = Index, y = Value, stat= Series, colour = Series)
  )
  x = plot.cone + geom_line(size=1) + labs(x="",y ="Cumulative Excess Return")+ 
    scale_color_manual(values = c("black","orange","red","orange","red",IMD.palette()[1])) +
    theme(legend.position = 0)
  return(x)
}
get_xts_end <- function(x){
  if (length(policy.tree$Defunding[which(policy.tree$ssbtID == x)]) == 0) {
    xts.end <- end.date
  } else if (is.na(policy.tree$Defunding[which(policy.tree$ssbtID == x)])) {
    xts.end <- end.date
  } else {
    xts.end <- policy.tree$Defunding[which(policy.tree$ssbtID == x)]
  }
  return(paste0("/", xts.end))
}
get_return_xts <- function(x, y = FALSE){
  data  = do.call(merge, r.all.list[c(x)]) %>% na.fill(.,0)
  if (y == TRUE) {
    colnames(data) <- get_shortName(x)
  }
  return(data)
}
get_bench_xts <- function(x, y = FALSE){
  data  = do.call(merge, b.all.list[c(x)]) %>% na.fill(.,0)
  if (y == TRUE) {
    colnames(data) <- get_shortName(x)
  }
  return(data)
}
get_endMV_xts <- function(x, y = FALSE){
  data  = do.call(merge, endMV.all.list[c(x)]) %>% na.fill(.,0)
  if (y == TRUE) {
    colnames(data) <- get_shortName(x)
  }
  return(data)
}
get_bgnMV_xts <- function(x, y = FALSE){
  data  = do.call(merge, bgnMV.all.list[c(x)]) %>% na.fill(.,0)
  if (y == TRUE) {
    colnames(data) <- get_shortName(x)
  }
  return(data)
}
get_selection_xts <- function(x, y = FALSE){
  data  = do.call(merge, sel.all.list[c(x)]) %>% na.fill(.,0)
  colnames(data) <- x
  if (y == TRUE) {
    colnames(data) <- get_shortName(x)
  }
  return(data)
}
cone.pallette <- c("olivedrab4","goldenrod3","darkred","goldenrod3","darkred","dodgerblue4")
get_cone_plot <- function(return.xts, expExcess, expTE){
  cone.pallette <- c("olivedrab4","goldenrod3","darkred","goldenrod3","darkred","dodgerblue4")
  start.date = index(return.xts)[1]
  
  # Calculate vectors for the actual return, expected return and expected StdDev
  r.act <- cumprod(1 + (return.xts[, 1] - return.xts[, 2]))
  r.exp <- cumprod(rep((1 + expExcess/10000) ^ (1/12), nrow(return.xts)))
  sd.one <- expTE/10000 * sqrt((1:nrow(return.xts)/12))
  
  #merge data and add data for +/- StdDev bands
  cone.data = cbind(r.act, r.exp)
  colnames(cone.data) = c("Actual", "Expected")
  cone.data$up.one = cone.data$Expected * (1 + sd.one)
  cone.data$up.two = cone.data$Expected * (1 + 2*sd.one)
  cone.data$dn.one = cone.data$Expected / (1 + sd.one)
  cone.data$dn.two = cone.data$Expected / (1 + 2*sd.one)
  
  # Make the first month start with 1's
  one.row = xts(matrix(rep(1, ncol(cone.data)), ncol = ncol(cone.data)), start.date + 1 - months(1) - 1)
  cone.data = rbind(cone.data, one.row)
  
  # Run actual return last to paint over bands
  conePlot = ggplot(
    data = fortify.zoo(cone.data[,c(2:6,1)] - 1, melt = TRUE),
    aes(x = Index, y = Value, stat = Series, colour = Series)) + 
    scale_color_manual(values = cone.pallette) +
    labs(x = "", y = "Excess Return") +
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10),
          axis.title.y = element_text(size = 6, face = "italic"), 
          legend.text = element_text(size = 7), legend.key.size = unit(.25, "cm"),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6),
          legend.position = 0)
  return(conePlot)
}
get_dailyCone <- function(return.xts, expExcess, expTE){
  cone.pallette <- c("olivedrab4","goldenrod3","darkred","goldenrod3","darkred","dodgerblue4")
  start.date = index(return.xts)[1]
  
  # Calculate vectors for the actual return, expected return and expected StdDev
  r.act <- cumprod(1 + (return.xts[, 1] - return.xts[, 2]))
  r.exp <- cumprod(rep((1 + expExcess/10000) ^ (1/253), nrow(return.xts)))
  sd.one <- expTE/10000 * sqrt((1:nrow(return.xts)/253))
  
  #merge data and add data for +/- StdDev bands
  cone.data = cbind(r.act, r.exp)
  colnames(cone.data) = c("Actual", "Expected")
  cone.data$up.one = cone.data$Expected * (1 + sd.one)
  cone.data$up.two = cone.data$Expected * (1 + 2*sd.one)
  cone.data$dn.one = cone.data$Expected / (1 + sd.one)
  cone.data$dn.two = cone.data$Expected / (1 + 2*sd.one)
  
  # Make the first month start with 1's
  one.row = xts(matrix(rep(1, ncol(cone.data)), ncol = ncol(cone.data)), start.date - days(1))
  cone.data = rbind(cone.data, one.row)
  
  # Run actual return last to paint over bands
  conePlot = ggplot(
    data = fortify.zoo(cone.data[,c(2:6,1)] - 1, melt = TRUE),
    aes(x = Index, y = Value, stat = Series, colour = Series)) + 
    scale_color_manual(values = cone.pallette) +
    scale_x_bd(
      business.dates = index(cone.data),
      max.minor.breaks = 3,
      labels = date_format("%b '%y")) + 
    labs(x = "", y = "Excess Return") +
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10),
          axis.title.y = element_text(size = 6, face = "italic"), 
          legend.text = element_text(size = 7), legend.key.size = unit(.25, "cm"),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6))
  return(conePlot)
}

get_futures <- function(x){
  if (is.na(futures.df %>% filter(grepl(x, Name)) %>% select(Notional) %>% as.numeric()) == TRUE) {
    as.numeric(0)
  } else {
    futures.df %>% filter(grepl(x, Name)) %>% select(Notional) %>% as.numeric()
  }
}
get_creditCone <- function(return.xts, expExcess, expTE){
  cone.pallette <- c("olivedrab4","goldenrod3","darkred","goldenrod3","darkred","dodgerblue4")
  start.date = index(return.xts)[1]
  
  # Calculate vectors for the actual return, expected return and expected StdDev
  r.act <- cumprod(1 + (return.xts[, 1] - return.xts[, 2]))
  r.exp <- cumprod(rep((1 + expExcess/10000) ^ (1/12), nrow(return.xts)))
  sd.one <- expTE/10000 * sqrt((1:nrow(return.xts)/12))
  
  #merge data and add data for +/- StdDev bands
  cone.data = cbind(r.act, r.exp)
  colnames(cone.data) = c("Actual", "Expected")
  cone.data$up.one = cone.data$Expected * (1 + sd.one)
  cone.data$up.two = cone.data$Expected * (1 + 2*sd.one)
  cone.data$dn.one = cone.data$Expected / (1 + sd.one)
  cone.data$dn.two = cone.data$Expected / (1 + 2*sd.one)
  
  # Make the first month start with 1's
  one.row = xts(matrix(rep(1, ncol(cone.data)), ncol = ncol(cone.data)), start.date + 1 - months(1) - 1)
  cone.data = rbind(cone.data, one.row)
  
  # Run actual return last to paint over bands
  cone.data = ggplot(
    data = fortify.zoo(cone.data[,c(2:6,1)] - 1, melt = TRUE),
    aes(x = Index, y = Value, stat = Series, colour = Series)) + 
    scale_y_continuous(name = "Excess Return", labels = scales::percent) +
    scale_color_manual(values = cone.pallette) +
    labs(x = "", y = "Excess Return") +
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10),
          axis.title.y = element_text(size = 6, face = "italic"), 
          legend.text = element_text(size = 7), legend.key.size = unit(.25, "cm"),
          axis.text.x = element_text(size = 6, face = "bold"),
          axis.text.y = element_text(size = 6, face = "bold")) + 
    theme(legend.position = 0)
  return(cone.data)
}
to_millions = function(x) {x/1000000}
to_billions = function(x) {x/1000000000}
