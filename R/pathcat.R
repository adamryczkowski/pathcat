
#' Provides path.cat and make.path.relative functions, that enhance \code{base::system.path()}.
#'
#' \code{path.cat} concatenates paths in a way that mimmics OS behaviour when
#' inputing subsequent path components into separate \code{setwd} commands.
#'
#' All functions allow for mixing '/' with '\' on each path, treating both characters
#'  equally (so you can't have a filename with backslash even if you are on Linux)
#'
#' @docType package
#' @name pathcat
NULL

#' Returns indices of paths that are absolute
which.paths.are.absolute<-function(paths)
{
  grep("^([^/^\\^:]+:)?[/\\]",paths)
}

#' Splits the absolute path into the absolute part (e.g. '/' or 'H:\\') and the rest.
#' If not absolute path, return undefined
split.absolute.prefix<-function(abs.path)
{
  ans<-stringi::stri_match_all(str=abs.path, regex="^(([^\\\\/^\\\\^:]+:)?)[\\\\/\\\\]([^\\\\/^\\\\]+.*)$")
  return(list(prefix=ans[[1]][[2]],dirs=ans[[1]][[4]]))
}

#' High level function that concatenates paths.
#'
#' It tries to concatenate each argument in order from left to right
#' trying to emulate standard OS behaviour when user would put each argument in a separate \code{cd} (or in R \code{setwd(...)}) command. It also tries to be as platfrom-agnostic, as possible, treating '/' and '\\' equally.
#'
#'   This way it enhances \code{file.path} with two features:
#' \itemize{
#'  \item It understands that \code{..} means "go one directory up". When encountering \code{..} the last (the youngest)
#'    directory so far parsed gets deleted, or nothing happens if we are already on root.
#'  \item It understands when a path is absolute. Absolute path resets parsing from start with the path, just like you would expect from the command line. Path is treated as absolute if it either starts with '/' (or '\\'), or with regex \code{^([^/^\\]+:)[/\\]}
#' }
#'
#' @param ... character string with each directory component
#' @param fsep Custom path separator. Defaults to \code{.Platform$file.sep}
#'
#' @return Character string with concatenated path
#'
#' @export
#' @seealso \code{\link[base]{file.path}}
path.cat<-function(...,fsep=.Platform$file.sep)
{
  elems<-list(...)
  elems<-as.character(elems)
  # relems<-rev(elems)
  # starts<-grep('^[/\\]',relems)[1]
  # if (!is.na(starts) && !is.null(starts))
  # {
  #   relems<-relems[1:starts]
  # }
  # starts<-grep(':',relems,fixed=TRUE)
  # if (length(starts)==0){
  #   starts=length(elems)-length(relems)+1
  # }else{
  #   starts=length(elems)-starts[[1]]+1}
  start<-which.paths.are.absolute(elems)
  if (length(start)==0) {
    relpath<-TRUE
    start<-1
    prefix<-''
  } else
  {
    start<-max(start)
    relpath<-FALSE
    prefix<-split.absolute.prefix(elems[[start]])
    elems[[start]]<-prefix[['dirs']]
    prefix<-paste0(prefix[['prefix']], fsep)
  }

  elems<-elems[start:length(elems)]
  sciezka<-do.call(function(...) file.path(fsep=fsep, ...),as.list(elems))
  elems<-strsplit(sciezka,'[/\\]',fixed=FALSE)[[1]]
  elems<-elems[elems!='']

  out<-rep(NA,length(elems))
  i<-1
  for(item in elems)
  {
    if(item=='..')   {
      if (i <= 1) {
        out[i] <- item
        i<-i+1
      } else {
        if(out[[i-1]] != '..') {
          i<-max(1,i-1)
        } else {
          out[i] <- item
          i<-i+1
        }
      }
    } else  {
      out[i]<-item
      i<-i+1
    }
  }

  paste0(prefix, do.call(function(...) file.path(fsep=fsep, ...),as.list(out[1:i-1])))
}

#' Converts one absolute path into relative
#'
#' The algorithm produces a relative path, that addresses \code{target.path} from
#' \code{base.path} using folder names and special name \code{..} for parent folder.
#'
#' @param base.path character string with starting directory.
#' @param target character string with target directory.
#' @param fsep Custom path separator. Defaults to \code{.Platform$file.sep}.
#'
#' @return Character string that represents a relative path that leads to \code{target.path}
#' from \code{base.path}.
#'
#' @export
make.path.relative<-function(base.path, target.path, fsep=.Platform$file.sep)
{
  if(length(pathcat:::which.paths.are.absolute(c(base.path, target.path)))<2) {
    return(target.path)
  }

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
