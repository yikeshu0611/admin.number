#' To Look for Area from Codes
#'
#' @param codes id codes
#' @param data data after lookfor_area() function
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \donttest{
#' df=get_data()
#' codes=c(320324,320323,320381)
#' lookfor_area(codes,df)
#' }

lookfor_area <-function(codes,data){
    for (i in 1:length(codes)) {
        if (i==1) df=NULL
        code.i=codes[i]
        dd1=data[data[,4]==do::left(code.i,2),]
        dd2=dd1[dd1[,5]==do::mid(code.i,3,2),]
        df.i=dd2[dd2[,6]==do::mid(code.i,5,2),]
        if (nrow(df.i)==0){
            message(code.i,tmcn::toUTF8(' \u6CA1\u6709\u67E5\u5230'))
        }else{
            df=rbind(df,df.i)
        }
    }
    rownames(df)=NULL
    return(df)
}
