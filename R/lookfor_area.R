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
#' codes=c(32999999,320324,320323,320381)
#' lookfor_area(codes,df)
#' }

lookfor_area <-function(codes,data){
    for (i in 1:length(codes)) {
        if (i==1) df=NULL
        code.i=codes[i]
        dd1=data[data[,4]==do::left(code.i,2),]
        if (nrow(dd1)==0){
            message(code.i,tmcn::toUTF8(' \u6CA1\u6709\u67E5\u5230'))
            next(i)
        }
        dd2=dd1[dd1[,5]==do::mid(code.i,3,2),]
        if (nrow(dd2)==0){
            df=plyr::rbind.fill(df,unique(dd1[,c(1,4)]))
            message(code.i,tmcn::toUTF8(' \u6CA1\u6709\u67E5\u5230 \u5E02'))
            next(i)
        }
        df.i=dd2[dd2[,6]==do::mid(code.i,5,2),]
        if (nrow(df.i)==0){
            df=plyr::rbind.fill(df,unique(dd2[,-c(3,6)]))
            message(code.i,tmcn::toUTF8(' \u6CA1\u6709\u67E5\u5230 \u53BF'))
            next(i)
        }else{
            df=plyr::rbind.fill(df,df.i)
        }
    }
    rownames(df)=NULL
    df=cbind(df[,!grepl(tmcn::toUTF8('\u7F16\u7801'),colnames(df))],
        df[,grepl(tmcn::toUTF8('\u7F16\u7801'),colnames(df))])

    df=cbind(code=codes,df)
    colnames(df)[1]=tmcn::toUTF8('\u6240\u67E5\u7F16\u7801')
    return(df)
}
