library(RMariaDB)
library(DBI)

con <- dbConnect(RMariaDB::MariaDB(), 
                 dbname = "pcancer", 
                 username = 'suer',
                 password = '123')

path='/home/data';
case_nm='pancreatic_cancer'
study='';
filepath=paste(c(path,case_nm,'report'),collapse="/");
files=dir(filepath, pattern='.par')

read_to_DB <- function(file) 
{
    person_id=as.numeric(gsub('\\.par$', '', file))
    file=paste(filepath,file,sep='/');
    data=scan(file,what='character',sep="\n", fileEncoding='GBK',encoding='UTF-8')
    cls_idx=which(grepl('CLASS NAMES',data,fixed=TRUE))
    fea_idx=which(grepl('FEATURES',data,fixed=TRUE))
    meta_data=data[1:cls_idx-1]    
    fea_data=data[-(1:fea_idx-1)]
    if (fea_idx > cls_idx+1) { cls_data=data[(cls_idx+1):(fea_idx-1)] } else {
            cls_data='';
        }
    
    #extract  meta data
    df_meta=data.frame()
    for (i in seq_along(meta_data)) {
        s=meta_data[i];
        x=trimws(strsplit(s,'[,:=]')[[1]]);
        mat=matrix(x,ncol = 2,byrow = TRUE)
        colnames(mat) <- c('feature','value')
        df_meta=rbind(df_meta,mat);    
    }
    df_meta=rbind(df_meta,data.frame(feature='CLASS NAMES',value=cls_data))
    df_meta=cbind(case_nm,study,person_id,df_meta,file)

    #extract  feature data
    df_feature=read.table(text=fea_data,skip=1)
    names(df_feature)[names(df_feature)=='V1'] <- 'feature';
    # extract valid ROI
    if (nchar(cls_data)!=0) {
        cidx=as.numeric(gsub('\\D|\\s.*','',cls_data))+1
    } else {
        cidx=which(colSums(df_feature!=0,na.rm=TRUE)!=0)[-1]
    }
    df_fea=data.frame()
    for (i in cidx) {
        df_fea=rbind(df_fea,cbind(df_feature[1],ROI=i-1,value=unlist(df_feature[i])))
    }        
        
    df_fea=cbind(case_nm,study,person_id,df_fea)
    df_fea[is.na(df_fea)] <- -9999;

    if(dbExistsTable(con, 'meta')) {    
        dbWriteTable(con,'meta',df_meta,append=TRUE); 
    } else {
        dbWriteTable(con,'meta',df_meta);
    }
    if(dbExistsTable(con, 'feature')) {
        dbWriteTable(con,'feature',df_fea,append=TRUE)
    } else {
        dbWriteTable(con,'feature',df_fea);
    }
    
    sql = sprintf("INSERT INTO file_tb (path, study, case_nm, person_id) values ('%s', '%s', '%s', %s)",
                  file,study,case_nm,as.character(person_id));
    mariadbExecQuery(con,sql);
}

for (file in files) {
    read_to_DB(file);
}