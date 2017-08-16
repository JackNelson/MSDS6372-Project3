/*************** IMPORTING DATA SET & DATA CLEAN UP ***************/

*Import in BreastCancer.xls data;
proc import datafile='/home/nelsonjohn0/sasuser.v94/MSDS6372/BreastCancer.xls' out=cancer_data dbms=xls replace;
getnames=yes;
datarow=2;
run;

*remove labels;
proc datasets;
  modify cancer_data;
  attrib _all_ label='';
quit;

*Create sample group variable;
data breast_cancer;
set cancer_data;
if _n_ <= 367 then samp_group = 1;
else if _n_ <= 437 then samp_group = 2;
else if _n_ <= 468 then samp_group = 3;
else if _n_ <= 485 then samp_group = 4;
else if _n_ <= 533 then samp_group = 5;
else if _n_ <= 582 then samp_group = 6;
else if _n_ <= 613 then samp_group = 7;
else samp_group = 8; 





/*************** ADDRESSING MISSING VALUES ***************/

*Reformatting bare_nuc variable as numeric with missing values as 0;
data breast_cancer1;
set breast_cancer; 
if bare_nuc = '?' then bare_nuc1=1;
else bare_nuc1=0;
drop bare_nuc;
run;

*Sorting bare_nuc1 variable in ascending order;
proc sort data=breast_cancer1;
by bare_nuc1 sample_id;
run;

*putting original data in long format;
proc transpose data=breast_cancer1 out=breast_cancer_long name=Variable prefix=Score_;
by bare_nuc1 sample_id;
run;

*removing labels from long formatted data;
proc datasets;
   modify breast_cancer_long;
     attrib _all_ label=' ';
     attrib _all_ format=;
run;

*generating a panel of histogram plots for explanatory variables with non-missing values;
*grouped by if that observation contains a missing value in bare_nuc;
ods graphics on/ reset=all height=10in width=10in;
proc template; run;
proc sgpanel data=breast_cancer_long;
panelby Variable / rows=4 columns=3;
histogram Score_1 / group=bare_nuc1 scale=count binwidth=1;
rowaxis label="Frequency" labelpos=center labelattrs=(size=10 weight=bold) values=(0,100,200,300,400);
colaxis label="Score" labelpos=center labelattrs=(size=10 weight=bold) values=(0,1,2,3,4,5,6,7,8,9,10);
title 'Histogram of Variables Colored by if bare_nuc Missing';
run;
ods graphics off;

*Subset of data with missing bare_nuc values;
data breast_cancer_missing;
set breast_cancer; 
if bare_nuc ^= '?' then delete;
run;

proc freq data=breast_cancer_missing;
title 'Summary Statistics for Missing Values';
run;

*Reformatting bare_nuc variable as numeric bare_nuc2 variable with missing data removed;
data breast_cancer2;
set breast_cancer; 
if bare_nuc = '?' then delete;
bare_nuc2 = input(bare_nuc, best12.);
run;





/*************** SUMMARY STATISTICS ***************/

*Summary statistics on cleaned data set;
proc means data=breast_cancer2;
title 'Summary Statistics of Cleaned Data';
var clump_thick uni_cell_size uni_cell_shape marg_adh
	sing_ep_size bland_chrom norm_nuc mitoses;
output out=means;
run;
proc print data=means;
run;

*Summary statistics on original data set;
proc means data=breast_cancer1;
title 'Summary Statistics of Original Data';
output out=means_original;
run;
proc print data=means_original;
run;

/** COMPARING SUMMARY DATA **/
*Generating unique _STAT_ name for original data when combined with clean;
data means_original;
set means_original;
_STAT_1 = catt(_STAT_,'-O');
drop _STAT_;
rename _STAT_1 = _STAT_; 
run;

*Combining means tables together;
data means_combine;
set means means_original;
drop class sample_id samp_group _TYPE_ _FREQ_ bare_nuc1;
run;

*transposing summary statistics to compare to original data set;
proc transpose data=means_combine out=means_combine2 name=Variable;
id _STAT_;
run;

*taking differences in summary statistics;
data means_diff;
set means_combine2;
'Mean-Diff'n = MEAN - 'MEAN-O'n;
'Stdev-Diff'n = STD - 'STD-O'n;
if Variable = 'bare_nuc2' then delete;
keep 'Mean-Diff'n 'Stdev-Diff'n Variable;
run;
proc print data=means_diff;
title 'Summary Statistics Difference: Original vs. Clean';
run;

/**  GENERATING HISTOGRAMS **/

*sorting breast_cancer2 data set;
proc sort data=breast_cancer2;
by sample_id samp_group class;
run;

*putting explanatory variables for breast_cancer2 in long format;
proc transpose data=breast_cancer2 out=breast_cancer2_long name=Variable prefix=Score_;
by sample_id samp_group class;
run;
proc print data=breast_cancer2_long;
run;

*removing labels from long formatted data;
proc datasets;
   modify breast_cancer2_long;
     attrib _all_ label=' ';
     attrib _all_ format=;
run;

*generating a panel of histogram plots for explanatory variables;
*grouped by if that observation was cancerous; 
ods graphics on/ reset=all height=10in width=10in;
proc sgpanel data=breast_cancer2_long;
styleattrs datacontrastcolors=(red blue) datacolors=(red blue);
panelby Variable / rows=3 columns=3;
histogram Score_1 / group=class scale=count binwidth=1;
rowaxis label="Frequency" labelpos=center labelattrs=(size=10 weight=bold) values=(0,100,200,300,400);
colaxis label="Score" labelpos=center labelattrs=(size=10 weight=bold) values=(0,1,2,3,4,5,6,7,8,9,10);
title 'Histogram of Variables Colored by Class';
run;
ods graphics off;





/*************** CORRELATION ***************/

*matrix scatterplot of explanatory variables colored by response (class);
data myattrmap;                                                                                                                         
   length value markercolor $10;                                                                                                        
   retain id "myid";                                                                                                                    
   input value $ markersymbol $ markercolor $;                                                                                          
   datalines;                                                                                                                           
2  circle   blue                                                                                                          
4  circle   red                                                                                                                                                                                                                     
;                                                                                                                                       
run;                                                                                                                                    

ods graphics on/ reset=all height=10in width=10in;                                                                                                                                       
proc sgscatter data=breast_cancer2 dattrmap=myattrmap;
matrix clump_thick uni_cell_size uni_cell_shape marg_adh 
	sing_ep_size bare_nuc2 bland_chrom norm_nuc mitoses / group=class attrid=myid ;
title 'Scatterplot Matrix of Variables Colored by Class';
run;
ods graphics off;

*correlation table of explanatory variables for clean data;
proc corr data=breast_cancer2 plots=matrix(histogram) noprob out=corr_clean;
var clump_thick uni_cell_size uni_cell_shape marg_adh
	sing_ep_size bare_nuc2 bland_chrom norm_nuc mitoses;
title 'Explanatory Variable Correlation';
run;

*make a correlation plot heatmap;
data corr_clean2;
set corr_clean (where=(_TYPE_="CORR"));


*correlation table of explanatory variables for original data;
proc corr data=breast_cancer1 plots=matrix(histogram) noprob out=corr_original;
var clump_thick uni_cell_size uni_cell_shape marg_adh
	sing_ep_size bland_chrom norm_nuc mitoses;	
run;

/** Compare Correlations **/

*rename variables in original data correlation plot;
data corr_original2;
set corr_original;
'clump_thick-O'n = clump_thick;
'uni_cell_size-O'n = uni_cell_size;
'uni_cell_shape-O'n = uni_cell_shape;
'marg_adh-O'n = marg_adh;
'sing_ep_size-O'n = sing_ep_size;
'bland_chrom-O'n = bland_chrom;
'norm_nuc-O'n = norm_nuc;
'mitoses-O'n = mitoses;
drop clump_thick uni_cell_size uni_cell_shape marg_adh
	sing_ep_size bland_chrom norm_nuc mitoses;
run;

*remove missing value variable from clean data correlation plot;
data corr_clean2;
set corr_clean;
if _NAME_ = 'bare_nuc2' then delete;
drop bare_nuc2;
run;

*combine the two tables together;
proc sql;
   title 'SQL Table Combined';
   create table corr_combine as
      select * 
      from corr_clean2, corr_original2
      where corr_clean2._NAME_ = corr_original2._NAME_;
quit;

*calculate differences and rename variables;
data corr_diff;
set corr_combine;
clump_thick_diff = clump_thick - 'clump_thick-O'n;
uni_cell_size_diff = uni_cell_size - 'uni_cell_size-O'n;
uni_cell_shape_diff = uni_cell_shape - 'uni_cell_shape-O'n;
marg_adh_diff = marg_adh - 'marg_adh-O'n;
sing_ep_size_diff = sing_ep_size - 'sing_ep_size-O'n;
bland_chrom_diff = bland_chrom - 'bland_chrom-O'n;
norm_nuc_diff = norm_nuc - 'norm_nuc-O'n;
mitoses_diff = mitoses - 'mitoses-O'n;
drop clump_thick uni_cell_size uni_cell_shape marg_adh
	sing_ep_size bland_chrom norm_nuc mitoses _TYPE_; 
drop 'clump_thick-O'n 'uni_cell_size-O'n 'uni_cell_shape-O'n 
	'marg_adh-O'n 'sing_ep_size-O'n 'bland_chrom-O'n 'norm_nuc-O'n 'mitoses-O'n;
rename clump_thick_diff=clump_thick;
rename uni_cell_size_diff=uni_cell_size; 
rename uni_cell_shape_diff = uni_cell_shape;
rename marg_adh_diff = marg_adh;
rename sing_ep_size_diff = sing_ep_size;
rename bland_chrom_diff = bland_chrom; 
rename norm_nuc_diff = norm_nuc; 
rename mitoses_diff = mitoses;
if _NAME_ = '' then delete;
rename _NAME_ = Variable;
run;

proc print data=corr_diff;
format _numeric_ 8.5;
title 'Correlation Difference: Original vs. Clean';
run;




/*************** PRINCIPAL COMPONENT ANALYSIS ***************/

*principle component analysis with the explanatory variables;
proc princomp plots=all data=breast_cancer2 out=bc_pca; 
var clump_thick uni_cell_size uni_cell_shape marg_adh
	sing_ep_size bare_nuc2 bland_chrom norm_nuc mitoses;
id sample_id;
title 'Principal Component Analysis';
run; quit;

/** GENERATE HISTOGRAMS **/

*sorting bc_pca data set;
proc sort data=bc_pca;
by sample_id class;

*putting explanatory variables for bc_pca in long format;
proc transpose data=bc_pca out=bc_pca_long name=Variable prefix=Score_;
var Prin1-Prin9;
by sample_id class;
run;

*removing labels from long formatted data;
proc datasets;
   modify bc_pca_long;
     attrib _all_ label=' ';
     attrib _all_ format=;
run;

*generating a panel of histogram plots for Principal Components;
*grouped by if that observation was cancerous; 
ods graphics on/ reset=all height=10in width=10in;
proc sgpanel data=bc_pca_long;
styleattrs datacontrastcolors=(red blue) datacolors=(red blue);
panelby Variable / rows=3 columns=3;
histogram Score_1 / group=class scale=count binwidth=1;
rowaxis label="Frequency" labelpos=center labelattrs=(size=10 weight=bold);
colaxis label="Score" labelpos=center labelattrs=(size=10 weight=bold);
title 'Histogram of Principal Components Colored by Class';
run;
ods graphics off;

/** GENERATE SCATTERPLOTS **/

*scatterplot of prin1-prin4;
proc sgplot data=bc_pca;
scatter x=Prin1 y=Prin2 / group=class;
xaxis labelpos=center labelattrs=(size=12 weight=bold) label='PC 1';
yaxis labelpos=center labelattrs=(size=12 weight=bold) label='PC 2';
styleattrs datacontrastcolors=(red blue) datacolors=(red blue);
run;
proc sgplot data=bc_pca;
scatter x=Prin1 y=Prin3 / group=class;
xaxis labelpos=center labelattrs=(size=12 weight=bold) label='PC 1';
yaxis labelpos=center labelattrs=(size=12 weight=bold) label='PC 3';
styleattrs datacontrastcolors=(red blue) datacolors=(red blue);
run;
proc sgplot data=bc_pca;
scatter x=Prin1 y=Prin4 / group=class;
xaxis labelpos=center labelattrs=(size=12 weight=bold) label='PC 1';
yaxis labelpos=center labelattrs=(size=12 weight=bold) label='PC 4';
styleattrs datacontrastcolors=(red blue) datacolors=(red blue);
run;
proc sgplot data=bc_pca;
scatter x=Prin2 y=Prin3 / group=class;
xaxis labelpos=center labelattrs=(size=12 weight=bold) label='PC 2';
yaxis labelpos=center labelattrs=(size=12 weight=bold) label='PC 3';
styleattrs datacontrastcolors=(red blue) datacolors=(red blue);
run;
proc sgplot data=bc_pca;
scatter x=Prin2 y=Prin4 / group=class;
xaxis labelpos=center labelattrs=(size=12 weight=bold) label='PC 2';
yaxis labelpos=center labelattrs=(size=12 weight=bold) label='PC 4';
styleattrs datacontrastcolors=(red blue) datacolors=(red blue);
run;
proc sgplot data=bc_pca;
scatter x=Prin3 y=Prin4 / group=class;
xaxis labelpos=center labelattrs=(size=12 weight=bold) label='PC 3';
yaxis labelpos=center labelattrs=(size=12 weight=bold) label='PC 4';
styleattrs datacontrastcolors=(red blue) datacolors=(red blue);
run;





/*************** LOGISTIC REGRESSION ***************/

*logistic regression using PC1-4 and samp_group;
proc logistic data=bc_pca ;	    
model class(event='4')= Prin1 Prin2 Prin3 Prin4 samp_group / scale=none details lackfit influence ;
output out=myoutput predprobs=I p=probpred resdev=resdev reschi=reschi;
run;

*logistic regression using PC1, PC3, PC4, samp_group; 
proc logistic data=bc_pca;	    
model class(event='4')= Prin1 Prin3 Prin4 samp_group / scale=none details lackfit influence ;
output out=myoutput predprobs=I p=probpred resdev=resdev reschi=reschi;
run;

*logistic regression using PC1, PC3, PC4; 
proc logistic data=bc_pca;	    
model class(event='4')= Prin1 Prin3 Prin4 / scale=none details lackfit influence ;
output out=myoutput predprobs=I p=probpred resdev=resdev reschi=reschi;
run;

*ROC Curves; 
proc logistic data=bc_pca;	    
model class(event='4')= Prin1 Prin2 Prin3 Prin4 samp_group / scale=none details lackfit influence ;
output out=myoutput predprobs=I p=probpred resdev=resdev reschi=reschi;
roc 'PC1,PC3,PC4' Prin1 Prin3 Prin4;
roc 'PC1,PC3,PC4,samp_group' Prin1 Prin2 Prin3 Prin4;
roc 'PC1 only' Prin1;
run;





	