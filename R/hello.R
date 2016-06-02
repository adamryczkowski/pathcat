# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#Uniwersalna, wysokiego poziomu funkcja łącząca w inteligentny sposób ścieżki.
#Łączy ścieżki tak samo jak file.path, tyle że potrafi sama interpretować ścieżkę "..", oraz
#zidentyfikować, która czy ścieżka jest bezwzględna (tj. zaczynająca się od '/' lub 'X:/').
#Jeśli napotka ścieżkę bezwzględną, to zapomina o całej dotych zbudowanej ścieżce i zaczyna tworzyć
#od początku, zaczynając od podanej ścieżki bezwzględnej.
#W ten sposób, użytkownik może w ten sposób dodać sufiks ścieżki (mogący odwoływać się do rodziców przez "..")
#jak i ścieżkę bezwzględną (jeśli poda ścieżkę bezwzględną)
path.cat<-function(fsep=.Platform$file.sep, ...)
{
  elems<-list(...)
  elems<-as.character(elems)
  elems<-elems[elems!='']
  relems<-rev(elems)
  starts<-grep('^[/\\]',relems)[1]
  if (!is.na(starts) && !is.null(starts))
  {
    relems<-relems[1:starts]
  }
  starts<-grep(':',relems,fixed=TRUE)
  if (length(starts)==0){
    starts=length(elems)-length(relems)+1
  }else{
    starts=length(elems)-starts[[1]]+1}
  elems<-elems[starts:length(elems)]
  sciezka<-do.call(function(...) file.path(fsep=fsep, ...),as.list(elems))
  elems<-strsplit(sciezka,'[/\\]',fixed=FALSE)[[1]]
  it<-ihasNext(iter(elems))
  out<-rep(NA,length(elems))
  i<-1
  while(hasNext(it))
  {
    item<-nextElem(it)
    if(item=='..')
    {
      i<-i-1
    } else if (item=='' & i!=1) {
      # do nothing
    } else   {
      out[i]<-item
      i<-i+1
    }
  }
  do.call(function(...) file.path(fsep=fsep, ...),as.list(out[1:i-1]))
}

make.path.relative<-function(base.path, target.path, fsep=.Platform$file.sep)
{
  base.s<-strsplit(path.cat(base.path,fsep='/'),'/',fixed=TRUE)[[1]]
  target.s<-strsplit(path.cat(target.path,fsep='/'),'/',fixed=TRUE)[[1]]
  idx<-1
  maxidx<-min(length(target.s),length(base.s))
  while(idx<=maxidx)
  {
    if (base.s[[idx]]!=target.s[[idx]])
      break
    idx<-idx+1
  }
  dotscount<-length(base.s)-idx+1
  ans1<-paste0(paste(rep('..',times=dotscount),collapse='/'))
  if (idx<=length(target.s))
    ans2<-paste(target.s[idx:length(target.s)],collapse='/')
  else
    ans2<-''
  ans<-character(0)
  if (ans1!='')
    ans[[length(ans)+1]]<-ans1
  if (ans2!='')
    ans[[length(ans)+1]]<-ans2
  ans<-paste(ans,collapse=fsep)
  return(ans)
}
