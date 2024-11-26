## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height = 3, fig.width = 4)
col1 = rgb(0, 0.5, 1, alpha = 0.5)
col2 = rgb(1, 0, 0, alpha = 0.5)
col3 = rgb(0.5, 0.5, 1, alpha = 0.5)
col4 = rgb(1, 0, 1, alpha = 0.5)
kernel = Sys.info()[2]
all = FALSE

## ----echo=FALSE---------------------------------------------------------------
f = c("/home/jr/Documents/Ulaval/ALS data/BCTS/092L073113_BCTS_2.laz", 
"/home/jr/Documents/Ulaval/ALS data/BCTS/092L073114_BCTS_2.laz", 
"/home/jr/Documents/Ulaval/ALS data/BCTS/092L073131_BCTS_2.laz", 
"/home/jr/Documents/Ulaval/ALS data/BCTS/092L073132_BCTS_2.laz")

all = FALSE

read_benchmarks = function(pkg, test)
{
  if (all)
  {
    list(
    read_benchmark(paste0(pkg, "_test_", test, "_jr-ThinkPad-T450s.data")),
    read_benchmark(paste0(pkg, "_test_", test, "_multifile_false_FFGG-1009803.data")),
    read_benchmark(paste0(pkg, "_test_", test, "_multifile_true_jr-ThinkPad-T450s.data")),
    read_benchmark(paste0(pkg, "_test_", test, "_multifile_true_FFGG-1009803.data")))
  }
  else
  {
    list(
    read_benchmark(paste0(pkg, "_test_", test, "_multifile_false_FFGG-1009803.data")),
    read_benchmark(paste0(pkg, "_test_", test, "_multifile_true_FFGG-1009803.data")))
  }
}

read_benchmark = function(file)
{
  f = system.file("extdata/benchmarks", file, package = "lasR")
  if (!file.exists(f)) return(NULL)
  read.table(f)$V1  
}

plot_benchmark = function(lidr, lasr, tmax = NULL, max = NULL)
{
  vlidr = "lidR 4.1.1"
  vlasr = paste("lasR", packageVersion("lasR"))
  
  cpu_name = c("i7-5600U - 1 core", "i7-1355U - 1 core", "i7-5600U - 4 cores", "i7-1355U - 4 cores")
  if (!all) cpu_name = cpu_name[c(2,4)]


  if (is.null(tmax))
  {
    mmax = 0
    tmax = 0
    for (i in seq_along(lidr))
    {
      m_lidr = lidr[[i]]
      m_lasr = lasr[[i]]
      t_lasr = (1:length(m_lasr)-1)*2/60
      t_lidr = (1:length(m_lidr)-1)*2/60
      tmax = max(t_lidr, t_lasr, tmax)
      mmax = max(m_lidr, m_lasr, mmax)
    }
    
    mmax = mmax/1000
  }
  
  unit = "min"
  divider = 60
  if (tmax < 1.5)
  {
    tmax = tmax*60
    unit = "sec"
    divider = 1
  }
  
  par(mar = c(4,4,1,1), cex = 0.8)
    
  for (i in seq_along(lidr))
  {
    plot(0, 0, type = "n", xlim = c(0, tmax), ylim = c(0, mmax), ylab = "Memory (GB)", xlab = paste0("Time (", unit, ")") , main = cpu_name[i])
    if (is.null(lidr[[i]]) && is.null(lasr[[i]])) next
    
    if (!is.null(lidr[[i]]))
    {
      m_lidr = lidr[[i]]
      t_lidr = (1:length(m_lidr)-1)*2/divider
      
      x = t_lidr
      y = m_lidr/1000
      
      polygon(c(x, rev(x)), c(rep(0, length(x)), rev(y)), col = col1, border = NA)
      lines(x, y, col = "blue", lwd = 2)
    }
    
    if (!is.null(lasr[[i]]))
    {
      m_lasr = lasr[[i]]
      t_lasr = (1:length(m_lasr)-1)*2/divider
      
      x = t_lasr
      y = m_lasr/1000
      polygon(c(x, rev(x)), c(rep(0, length(x)), rev(y)), col = col2 , border = NA)
      lines(x, y, col = "red", lwd = 2)
    }
    
    legend("topright", legend = c(vlidr, vlasr), fill = c(col1, col2), border = NA)
  }
}

## ----warning=F, echo=-1, echo=FALSE, fig.show="hold", fig.width=4-------------
m_lasr = read_benchmarks("lasR", 1)
m_lidr = read_benchmarks("lidR", 1)
plot_benchmark(m_lidr, m_lasr)

## ----warning=F, echo=-1, echo=FALSE, fig.show="hold", fig.width=4-------------
m_lasr = read_benchmarks("lasR", 2)
m_lidr = read_benchmarks("lidR", 2)
plot_benchmark(m_lidr, m_lasr)

## ----warning=F, echo=-1, echo=FALSE, fig.show="hold", fig.width=4-------------
lasr = read_benchmarks("lasR", 2)
lidr = read_benchmarks("lidR", 2)

mmax = 0
tmax = 0
for (i in seq_along(lidr))
{
  m_lidr = lidr[[i]]
  m_lasr = lasr[[i]]
  t_lasr = (1:length(m_lasr)-1)*2/60
  t_lidr = (1:length(m_lidr)-1)*2/60
  tmax = max(t_lidr, t_lasr, tmax)
  mmax = max(m_lidr, m_lasr, mmax)
}
mmax = mmax/1000


m_lasr = read_benchmarks("lasR", 7)
m_lidr = read_benchmarks("lidR", 7)
plot_benchmark(m_lidr, m_lasr, tmax, mmax)

## ----warning=F, echo=-1, echo=FALSE, fig.show="hold", fig.width=4-------------
m_lasr = read_benchmarks("lasR", 3)
m_lidr = read_benchmarks("lidR", 3)
plot_benchmark(m_lidr, m_lasr)

## ----warning=F, echo=-1, echo=FALSE, fig.show="hold", fig.width=4-------------
m_lasr = read_benchmarks("lasR", 5)
m_lidr = read_benchmarks("lidR", 5)
plot_benchmark(m_lidr, m_lasr)

## ----warning=F, echo=-1, echo=FALSE, fig.show="hold", fig.width=4-------------
m_lasr = read_benchmarks("lasR", 6)
m_lidr = read_benchmarks("lidR", 6)
plot_benchmark(m_lidr, m_lasr)

## ----warning=F, echo=-1, echo=FALSE, fig.show="hold", fig.width=4-------------
m_lasr = read_benchmarks("lasR", 4)
m_lidr = read_benchmarks("lidR", 4)
plot_benchmark(m_lidr, m_lasr)

