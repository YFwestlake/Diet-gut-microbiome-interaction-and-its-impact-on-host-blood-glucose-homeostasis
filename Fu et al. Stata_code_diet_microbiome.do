******normalize the taxonomic data (from counts to relative abundance) *******
*********phylum level*******
clear
import excel "source_data_001.xlsx", sheet("phylum") firstrow
forvalues i = 1/317{
egen b`i'=sum(a`i')
}

forvalues i = 1/317{
replace a`i'=a`i'/b`i'
}

forvalues i = 1/317{
egen c`i'=sum(a`i')
}  
drop b1-c317
export delimited using "source_data_002.csv", replace


*********species level*******
clear
import excel "source_data_001.xlsx", sheet("species") firstrow
forvalues i = 1/317{
egen b`i'=sum(a`i')
}

forvalues i = 1/317{
replace a`i'=a`i'/b`i'
}

forvalues i = 1/317{
egen c`i'=sum(a`i')
}   //test
drop b1-c317
export delimited using "source_data_002_without_codebook_explaination.csv", replace // source_data_002 merged with codebook information (namely source_data_002_unfiltered.xslx) will be used in the subsequent species-level analysis, such as alpha diversity calculations, including shannon, observed features and simpsonn index, in the R software.


***Based on the "source_data_002_filtered", we formated the dataset stratified by each timepoint, namely source_data_003_for_foldchange_calculations.
***Identification of specific species to the interventional diets in the R softare
***We performed this analysis only for those who completed all 3 cycles of HC or LC dietary interventions. 
***For each participant, we calculate the fold-change of each species after each dietary intervention. 
***If the fold-changes of a species were consistently larger than 2 or smaller than 0.5 across the 3 cycles, then this species was defined as a responsive species to the interventional diet for this participant. 

//after filtering, prevalence>10% and mean relative abundance >0.01%
clear
import excel "Source_data_002_unfiltered.xlsx", sheet("Sheet1") firstrow
save "source_data_002_unfiltered.dta", replace

local i=0
foreach var of varlist s1-s395 {
local i=`i'+1
}
dis `i'   

foreach var of varlist s1-s395{
count if `var'>0 
if r(N)<32{
drop `var'
}
}

foreach var of varlist s1-s395{
sum `var'
if r(mean)<0.0001{
drop `var' 
}
}
 
//then we get the filtered dataset namely, "source_data_002_filtered.dta" 
//based on the filtered dataset, we generated the average value for each filtered species during each intervention period for each participant, independently.
//for those who had repeatedly provided stool samples at the begaining and end of an intervention period, the average value was generated for each species to represent its level during this period;
// for those who had only provided stool sample once at the begaining or end of an intervention period, the only existed value of each species was used to represent its level during this period;

clear
use "source_data_002_filtered.dta"

foreach var of varlist s3-s395 {
preserve
keep id period `var'
order id period `var'
sort id period


replace period="1" if period=="S1W1"
replace period="2" if period=="S1G1"
replace period="3" if period=="S1W2"
replace period="4" if period=="S1G2"
replace period="5" if period=="S2W1"
replace period="6" if period=="S2G1"
replace period="7" if period=="S2W2"
replace period="8" if period=="S2G2"
replace period="9" if period=="S3W1"
replace period="10" if period=="S3G1"
replace period="11" if period=="S3W2"
replace period="12" if period=="S3G2"
destring period, replace
drop if period==.
reshape wide `var', i(id) j(period)


replace `var'1=`var'2 if `var'1==.
replace `var'2=`var'1 if `var'2==.

replace `var'3=`var'4 if `var'3==.
replace `var'4=`var'3 if `var'4==.

replace `var'5=`var'6 if `var'5==.
replace `var'6=`var'5 if `var'6==.

replace `var'7=`var'8 if `var'7==.
replace `var'8=`var'7 if `var'8==.

replace `var'9=`var'10 if `var'9==.
replace `var'10=`var'9 if `var'10==.

replace `var'11=`var'12 if `var'11==.
replace `var'12=`var'11 if `var'12==.

gen seq_1=(`var'1+`var'2)/2
gen seq_2=(`var'3+`var'4)/2

gen seq_3=(`var'5+`var'6)/2
gen seq_4=(`var'7+`var'8)/2

gen seq_5=(`var'9+`var'10)/2
gen seq_6=(`var'11+`var'12)/2

order id seq_1  seq_2  seq_3  seq_4 seq_5  seq_6  
keep id-seq_6

reshape long seq_, i(id) j(seq)
rename seq_ `var'

tostring id, replace
gen x="pn_00"
gen ID=x+id
gen y=id
destring y, replace 
gen z="pn_0"
replace ID=z+child if y >9
drop x y z 
gen set="1" if seq==1 | seq==2

replace set="2" if seq==3 | seq==4

replace set="3" if seq==5 | seq==6

gen fat=0 if seq==1 | seq==4 | seq==6 //HC diet was implemented during the first, fourth and sixth period. fat=0 means HC
replace fat=1 if seq==2 | seq==3 | seq==5 //LC diet was implemented during the second, third and fifth period. fat=1 means LC

order ID set fat `var'
tostring fat, replace
gen ID2=ID+"_"+set+"_"+fat
order ID2 set fat `var'
keep ID2 `var'
rename ID2 id
save "averaged_species_during_each_period_species`var'.dta", replace
restore
}

*********Merge the generated average data for each species **************************
clear
use "averaged_species_during_each_period_speciess3.dta"
foreach var in s20 s23	s24	s25	s28	s29	s35	s40	s44	s45	s46	s47	s53	s54	s55	s56	s57	s58	s59	s60	s61	s62	s63	s64	s65	s67	s68	s69	s72	s73	s74	s75	s76	s77	s78	s79	s80	s81	s83	s84	s85	s86	s87	s94	s95	s96	s100	s105	s107	s108	s109	s110	s111	s112	s113	s115	s159	s170	s175	s177	s183	s186	s187	s188	s190	s192	s198	s210	s211	s215	s216	s218	s219	s221	s222	s223	s224	s225	s227	s230	s231	s232	s234	s235	s237	s238	s239	s240	s241	s242	s243	s244	s245	s247	s248	s250	s251	s256	s257	s258	s259	s261	s262	s264	s268	s270	s271	s272	s273	s274	s275	s277	s278	s282	s284	s290	s293	s299	s303	s305	s306	s307	s308	s309	s311	s312	s316	s318	s319	s333	s336	s340	s341	s343	s350	s351	s353	s364	s365	s366	s368	s384	s395 {

merge 1:1 id using "averaged_species_during_each_period_species`var'.dta"
drop _merge
}

merge 1:1 id using  "每日PMG数据.dta" 
drop if _merge!=3
drop _merge
gen ID=substr(id, 1, 6)
order ID
merge 1:1 id using "baseline_covariates.dta"
drop if _merge==2
bysort ID: replace sex=sex[1]
bysort ID: replace age=age[1]
bysort ID: replace bmi=bmi[1]
drop _merge
order ID id finalid-bmi
drop finalid time mean height weight
save "Figure_S1a&1b_source_data_for_PCA.dta", replace // this dataset was exported to R for PCA analysis


*******import the PCA results from R*************
clear 
import delimited "PCA_results.csv"
save "PCA_results.dta"
******merge the PCA data with glucose phenotypes
clear
use "Source_data_MAGE.dta" 
keep id mean_MAGE
merge 1:1 id using "Source_data_MPG.dta" 
keep id id1 set fat mean_MAGE mean_MPG
merge 1:1 id using "PCA_results.dta"
drop _merge
merge 1:1 id using "baseline_covariates.dta"
drop if _merge==2
gen ID=substr(id, 1, 6)
bysort ID: replace sex=sex[1]
bysort ID: replace age=age[1]
bysort ID: replace bmi=bmi[1]
drop _merge
drop height weight
merge 1:1 id using  "activity_data.dta"
drop if _merge==2
drop _merge

merge 1:1 id using "Source_data_HPT.dta"
order ID id id1 set fat mean_MAGE mean_MPG mean_HPT dim1 dim2 dim3 age sex bmi activity
keep ID id id1 set fat mean_MAGE mean_MPG mean_HPT dim1 dim2 dim3 age sex bmi activity
preserve
drop if fat=="1" //fat=0 high-carb;  fat=1 low-carb
replace mean_HPT=mean_HPT*100
mixed mean_MAGE dim1 dim2 dim3 age i.sex bmi activity || ID:, reml dfmethod(repeated)  
mixed mean_MPG dim1 dim2 dim3 age i.sex bmi activity || ID:, reml dfmethod(repeated)  
mixed mean_HPT dim1 dim2 dim3 age i.sex bmi activity || ID:, reml dfmethod(repeated) 
restore
drop if fat=="0" //fat=0 high-carb;  fat=1 low-carb
replace mean_HPT=mean_HPT*100
mixed mean_MAGE dim1 dim2 dim3 age i.sex bmi activity || ID:, reml dfmethod(repeated)  
mixed mean_MPG dim1 dim2 dim3 age i.sex bmi activity || ID:, reml dfmethod(repeated)  
mixed mean_HPT dim1 dim2 dim3 age i.sex bmi activity || ID:, reml dfmethod(repeated) 


*******Base on the PCA results, extrac the leading species contributing to the PC1, PC2 and PC3, respectively. See source data "contributions_to_the_top_PCs.xls" or  "Source_data_contributions_to_top_PCs.dta"*************
*****match the id of selcted species in the GNHS cohort
clear
use "GD_codebook.dta"
merge 1:1 species using "n_of_1_codebook.dta"
drop _merge 
drop if n_of_1_code==""
merge 1:1 n_of_1_code using  "Source_data_contributions_to_top_PCs.dta"
keep if _merge==3
drop _merge
order gd_code n_of_1_code seq_for_PC1 seq_for_PC2 seq_for_PC3

******top 10 GNHS id to PC3: s405 s348 s366 s295 s326 s123 s95 s314 s578 s575
******top 10 GNHS id to PC1: s295 s140 s338 s304 s341 s416 s299 s413 s344 s327
******top 10 GNHS id to PC2: s124 s350 s384 s358 s338 s341 s369 s49 s140 s126

****generate the scores based on the relative abundance of above species
clear
use "Source_data_GNHS_miceobiota_profile.dta"
**** dim3
order id SampleID s405 s348 s366 s295 s326 s123 s95 s314 s578 s575
egen LF_score3 = rowtotal(s405-s575)
keep id SampleID LF_score3
save "GNHS_PC3_score.dta"

****dim1 score
clear
use "Source_data_GNHS_miceobiota_profile.dta"
//dim1 
order id SampleID s295 s140 s338 s304 s341 s416 s299 s413 s344 s327
egen dim1_score3 = rowtotal(s295-s327)
keep id SampleID dim1_score3 
save "GNHS_PC1_score.dta"

****dim2 score
clear
use "Source_data_GNHS_miceobiota_profile.dta"
//dim2
order id SampleID s124 s350 s384 s358 s338 s341 s369 s49 s140 s126
egen dim2_score3 = rowtotal(s124-s126)
keep id SampleID dim2_score3  
save "GNHS_PC2_score.dta"

*****PCoA analysis of the taxonomic data grouped by quartiles of the score (Q1 vs. Q4) 
clear
use "Source_data_GNHS_miceobiota_merged_with_phnotypes_covariates_diets.dta"
//deleting the 505 samples which were repeatedly collected 
duplicates tag id, generate (tag)
duplicates report id 
gen follow=substr(SampleID,1,2)
drop if tag==1 & follow=="F3"
drop tag follow

//filtering, prevalence>10% and relative abundance >0.01%
 
foreach var of varlist s4-s642 {
count if `var'>0 
if r(N)<122{  // 
drop `var'
}
}


foreach var of varlist s9-s642 {
sum `var'
if r(N)<0.01 {  
drop `var'
}
}

xtile score1_median=LF_score3, nq(4)
order id SampleID score1_median 
drop if score1_median ==2 | score1_median==3 //Keep Q1 and Q4
keep 
save "GNHS_microbiota_PCoA_analysis_grouped_by_dim3score.dta"


*********validate the score-glycemic traits association, stratified by the dietary habits
clear
use "Source_data_GNHS_microbiota_merged_with_phnotypes_covariates_diets.dta"
gen ratio1=EA_FAT_f_P/EA_CHO_f_P     
gen ratio2=EA_FAT_f/EA_CHO_f
drop if ratio1==. //609 remained
xtile ratio2_median=ratio2, nq(2) 
tab ratio2_median 
//gladder GLU_x
gen new_GLU_x=1/((GLU_x)*(GLU_x))  //
replace new_GLU_x=new_GLU_x*-100

sum dim1_score3
gen dim1_z=(dim1_score3-r(mean))/r(sd)
sum dim2_score3
gen dim2_z=(dim2_score3-r(mean))/r(sd)
sum LF_score3
gen dim3_z=(LF_score3-r(mean))/r(sd)

regress new_GLU_x age_x sex BMI_x dim3_z EA_protein_f_P smoke_x alc_x  MET_x if ratio2_median==1   
regress new_GLU_x age_x sex BMI_x dim3_z EA_protein_f_P smoke_x alc_x  MET_x if ratio2_median==2   
regress hbalc_x age_x sex BMI_x dim3_z EA_protein_f_P smoke_x alc_x  MET_x if ratio2_median==1   
regress hbalc_x age_x sex BMI_x dim3_z EA_protein_f_P smoke_x alc_x  MET_x if ratio2_median==2   


********ko_data,********RPK (reads per kilo), see "functional_ko_data_RPK.dta"

clear
use "functional_ko_data_RPK.dta"
//transform the RPK data to relative abundance data
egen total = rowtotal(k00001-k19659)  
foreach var of varlist k00001-k19659 {
	replace `var' = 100*`var'/total
}
drop total 
//filtering
local i=0
foreach var of varlist k00001-k19659 {
local i=`i'+1
}
dis `i'  // 

 
foreach var of varlist k00001-k19659 {
count if `var'>0 
if r(N)<173{   
drop `var'
}
}

local i=0
foreach var of varlist k00001-k19611 {
local i=`i'+1
}
dis `i'  // 
save "functional_ko_data_filtered_RA.dta" 


**************PCoA analysis of the functional data grouped by quartiles of the score (Q1 vs. Q4) 

clear
use "functional_ko_data_filtered_RA.dta" 
merge 1:1 SampleID using "Source_data_GNHS_miceobiota_merged_with_phnotypes_covariates_diets.dta"
drop _merge
duplicates tag id, generate (tag)
duplicates report id 
gen follow=substr(SampleID,1,2)
drop if tag==1 & follow=="F3"
drop tag follow
drop s4-s642
xtile score1_median=LF_score3, nq(4)
//xtile score1_median=dim1_score3, nq(4)
//xtile score1_median=dim2_score3, nq(4)

order id SampleID score1_median  
drop if score1_median ==2 | score1_median==3
save "GNHS_function_PCoA_analysis_grouped_by_dim3score.dta" 
//save "GNHS_function_PCoA_analysis_grouped_by_dim1score.dta"
//save "GNHS_function_PCoA_analysis_grouped_by_dim2score.dta"

********spearman associations between KOs and the scores (PC1, PC2  or PC3)  
clear
use "functional_ko_data_filtered_RA.dta" 
merge 1:1 SampleID using "Source_data_GNHS_miceobiota_merged_with_phnotypes_covariates_diets.dta"
drop _merge
duplicates tag id, generate (tag)
duplicates report id 
gen follow=substr(SampleID,1,2)
drop if tag==1 & follow=="F3"
drop tag follow
drop s4-s642
keep SampleID k00001-k19611 dim1_score3 dim2_score3 LF_score3
 

//spearman association between dim3_score and KOs
tempname coef
tempfile res

postfile `coef'  str20(var score) double(n rho p) using "`res'", replace
foreach KO of varlist k00001-k19611 {
spearman `KO' LF_score3 // 
post `coef' ("`KO'")  ("dim3_score3") (`r(N)') (`r(rho)') (`r(p)')
}

postclose `coef'
preserve
use "`res'", clear
export excel using "spearman_dim3score_KOs.xls", sheetreplace firstrow(variables)
restore



//spearman association between dim1_score and KOs
tempname coef
tempfile res

postfile `coef'  str20(var score) double(n rho p) using "`res'", replace
foreach KO of varlist k00001-k19611 {
spearman `KO' dim1_score3  
post `coef' ("`KO'")  ("dim1_score3") (`r(N)') (`r(rho)') (`r(p)')
}

postclose `coef'
preserve
use "`res'", clear
export excel using "spearman_dim1score_KOs.xls", sheetreplace firstrow(variables)
restore

//spearman association between dim2_score and KOs
tempname coef
tempfile res

postfile `coef'  str20(var score) double(n rho p) using "`res'", replace
foreach KO of varlist k00001-k19611 {
spearman `KO' dim2_score3  
post `coef' ("`KO'")  ("dim2_score3") (`r(N)') (`r(rho)') (`r(p)')
}

postclose `coef'
preserve
use "`res'", clear
export excel using "spearman_dim2score_KOs.xls", sheetreplace firstrow(variables)
restore

**********KO  mapping to pathway*************
//firstly, mapping the KOs to the jason file from KEGG, see KO_pathway_matchtable.xls
clear
import excel "KO_pathway_matchtable_2023.xlsx", sheet("Sheet1") firstrow
bysort user_id: gen seq=_n
order user_id seq
tostring seq, replace
gen ID=user_id+"_"+seq
order ID
rename query_ko KO
save "KO_pathway_matchtable_2023_4439.dta" //

//check the KOs which were successfully mapped to pathways
clear
use "KO_pathway_matchtable_2023_4439.dta"
duplicates drop KO, force
keep KO user_id seq
order KO //
save "KO_pathway_successful_KO_2023_4439.dta" 
 
clear
use "functional_ko_data_filtered_RA_transposed_2023_4439.dta"  
rename sampleid KO
merge 1:1 KO using "KO_pathway_successful_KO_2023_4439.dta"
drop if _merge==1 //deleting 36 KOs which failed to be mapped to the pathway
gen id=user_id+"_"+seq
order id user_id seq 
drop _merge seq
rename id ID
save "mapped_KO_with_abundance_data_2023_4439.dta"  //


//abundance mapped to pathways
clear
use "mapped_KO_with_abundance_data_2023_4439.dta"  
merge 1:1 ID using "KO_pathway_matchtable_2023_4439.dta" 
order ID user_id seq
sort user_id seq
drop _merge 
foreach var of varlist f2nl0105-f3nl2466 {
by user_id: replace `var'=`var'[1]
} //
save "mapping_data.dta" 

//quantifications of the KOs mapped to metabolism pathways
clear
use "mapping_data.dta" 
keep level1_pathway_name level2_pathway_name pathway_count level1_pathway_id user_id seq KO f2nl0105-f3nl2466
order level1_pathway_name level2_pathway_name pathway_count level1_pathway_id user_id seq KO f2nl0105-f3nl2466
keep if level1_pathway_name=="Metabolism"  

preserve
keep if level2_pathway_name=="Carbohydrate metabolism" //1
duplicates drop KO, force 
export delimited using "KO_mapped_to_Carbohydrate_metabolism_2023_4439.csv" 
restore

preserve
keep if level2_pathway_name=="Lipid metabolism" //2
duplicates drop KO, force 
export delimited using "KO_mapped_to_Lipid_metabolism_2023_4439.csv" 
restore

preserve
keep if level2_pathway_name=="Amino acid metabolism" //3
duplicates drop KO, force 
export delimited using "KO_mapped_to_Amino_acid_metabolism_2023_4439.csv"
restore

preserve
keep if level2_pathway_name=="Metabolism of cofactors and vitamins" //4
duplicates drop KO, force 
export delimited using "KO_mapped_to_vitamins_metabolism_2023_4439.csv" 
restore

preserve
keep if level2_pathway_name=="Biosynthesis of other secondary metabolites" //5
duplicates drop KO, force 
export delimited using "KO_mapped_to_secondary_metabolites_metabolism_2023_4439.csv" 
restore


preserve
keep if level2_pathway_name=="Nucleotide metabolism" //6
duplicates drop KO, force 
export delimited using "KO_mapped_to_Nucleotide_metabolism_2023_4439.csv" 
restore

preserve
keep if level2_pathway_name=="Energy metabolism" //7
duplicates drop KO, force 
export delimited using "KO_mapped_to_Energy_metabolism_2023_4439.csv" 
restore

preserve
keep if level2_pathway_name=="Xenobiotics biodegradation and metabolism" //8
duplicates drop KO, force 
export delimited using "KO_mapped_to_Xenobiotics_metabolism_2023_4439.csv" 
restore

preserve
keep if level2_pathway_name=="Metabolism of terpenoids and polyketides" //9
duplicates drop KO, force 
export delimited using "KO_mapped_to_polyketides_metabolism_2023_4439.csv" 
restore

preserve
keep if level2_pathway_name=="Glycan biosynthesis and metabolism" //10
duplicates drop KO, force 
export delimited using "KO_mapped_to_Glycan_metabolism_2023_4439.csv" 
restore

preserve
keep if level2_pathway_name=="Metabolism of other amino acids" //11
duplicates drop KO, force 
export delimited using "KO_mapped_to_other_amino_metabolism_2023_4439.csv" 
restore

 
clear
import excel "KO_mapped_to_the_metabolism_pathways_2023_4439.xlsx", sheet("Sheet1") firstrow clear
merge 1:1 SampleID using "PC3_based_score_data_1724.dta"
keep if _merge==3
drop _merge
//normalize the data within metabolism categroy
egen total = rowtotal(Carbohydratemetabolism-Metabolismofotheraminoacids)
foreach var of varlist Carbohydratemetabolism-Metabolismofotheraminoacids {
	replace `var' = 100*`var'/total
}
drop total 
 
//deleting those 505 samples which were repeatedly collected after about 3 years.
duplicates tag id, generate (tag)
duplicates report id 
gen follow=substr(SampleID,1,2)
drop if tag==1 & follow=="F3"
drop tag follow

spearman LF_score3 Carbohydratemetabolism
sum LF_score3
gen dim3_z=(LF_score3-r(mean))/r(sd)
regress Carbohydratemetabolism age_x BMI_x sex smoke_x alc_x dim3_z 

sum Carbohydratemetabolism
gen carb_z=(Carbohydratemetabolism-r(mean))/r(sd)
regress carb_z age_x BMI_x sex smoke_x alc_x dim3_z 

********* CAZYmes analysis, among the 1219 unique samples, CAZYmes quantification were successfully performed for 1215 samples. we performed subseqent analysis only among 609 participants with availabe dietary habits data. 

clear
use "microbiome_score_diet_glycemics_and_covariates_609.dta" 
merge 1:1 SampleID using "CAZYmes_data_per_reads_1215.dta" 
keep if _merge==3 // all matched, n=609
drop _merge
drop na  
gen ratio1=EA_FAT_f_P/EA_CHO_f_P     
xtile ratio1_median=ratio1, nq(2) 
tab ratio1_median 
//normalize the data to counts per 1 million reads.
foreach var of varlist aa0-pl9 {
replace `var'= `var'*1000000
}  

//test the relationship between CAZYmes and glycemic traits, in two subgroups by dietary habits. 
//z-score transformation
foreach cazy of varlist aa0-pl9 {
sum `cazy'
gen `cazy'_z=(`cazy'-r(mean))/r(sd)
}

drop cbm49_z cbm59_z //zero data only

tempname coef
tempfile res

postfile `coef'  str20(var genus) double(n b se lb ub  p)    using "`res'", replace
foreach GO of varlist aa0_z-pl9_z {
regress hbalc_x age_x sex BMI_x `GO' EA_protein_f_P smoke_x alc_x  MET_x  if ratio1_median==1
post `coef' ("`GO'")  ("hbalc_x") (`e(N)') (_b[`GO']) (_se[`GO'])  (_b[`GO']-1.96*_se[`GO']) (_b[`GO']+1.96*_se[`GO']) (2*ttail(e(df_r), abs(_b[`GO']/_se[`GO'])))
}
postclose `coef'
preserve
use "`res'", clear
export excel using "CAZYmes_and_hba1c_regression_maong_HC_habits3.xls", sheetreplace firstrow(variables)
restore

tempname coef
tempfile res
postfile `coef'  str20(var genus) double(n b se lb ub  p)    using "`res'", replace
foreach GO of varlist aa0_z-pl9_z {
regress hbalc_x age_x sex BMI_x `GO' EA_protein_f_P smoke_x alc_x  MET_x  if ratio1_median==2
post `coef' ("`GO'")  ("hbalc_x") (`e(N)') (_b[`GO']) (_se[`GO'])  (_b[`GO']-1.96*_se[`GO']) (_b[`GO']+1.96*_se[`GO']) (2*ttail(e(df_r), abs(_b[`GO']/_se[`GO'])))
}
postclose `coef'
preserve
use "`res'", clear
export excel using "CAZYmes_and_hba1c_regression_maong_LC_habits3.xls", sheetreplace firstrow(variables)
restore

//check and normalization
//hist gt19 
//hist ce4
//hist gh4 
//hist gt1 
//hist pl4   
gen log_pl4=log(pl4)
//hist gt55  
replace gt55=0.001 if gt55==0
gen log_gt55=log(gt55)
//hist gh8  
gen log_gh8=log(gh8)
//hist gh5 
//hist gh108 
gen log_gh108=sqrt(gh108)
//hist gt56 
replace gt56=0.001 if gt56==0
gen log_gt56=log(gt56)
//hist gt25
gen log_gt25=log(gt25)
//hist gt18
replace gt18=0.001 if gt18==0
gen log_gt18=log(gt18)
//hist gh93
replace gh93=0.001 if gh93==0
gen log_gh93=log(gh93)
//hist cbm69
replace cbm69=0.001 if cbm69==0
gen log_cbm69=log(cbm69)
//hist gt107
gen log_gt107=log(gt107)

keep SampleID id gt19 ce4 gh4 gt1 log_pl4 log_gt55 log_gh8 gh77 gh5 log_gh108 log_gt56 log_gt25 log_gt18 log_gh93 log_cbm69 log_gt107 age_x sex BMI_x LF_score3 EA_protein_f_P smoke_x alc_x  MET_x ratio2_median new_GLU_x hbalc_x
order SampleID id new_GLU_x hbalc_x age_x sex BMI_x LF_score3 EA_protein_f_P smoke_x alc_x MET_x ratio2_median gt19 ce4 gh4 gt1 log_pl4 log_gt55 log_gh8 gh77 gh5 log_gh108 log_gt56 log_gt25 log_gt18 log_gh93 log_cbm69 log_gt107

foreach cazy of varlist gt19-log_gt107  {
sum `cazy'
gen `cazy'_z=(`cazy'-r(mean))/r(sd)
}
save "data_for_potential_mediation_analysis.dta", replace 

**********as significant CAZYmes-glycemics associations were identified only among participants with HC habits, so mediation analyses were then performed among this subgroup. 
clear
use "data_for_potential_mediation_analysis.dta"
keep if ratio2_median ==1 //HC subgroup
drop if hbalc_x ==. //40 with missing data
drop if BMI_x ==. //1 with missing data
sum LF_score3 
gen LF_score3_z=(LF_score3 -r(mean))/r(sd)
save "Mediation_analysis_data_for_R_264.dta", replace 




************import serum metabolomics data，test the associations between gut microbial score and metabolites in the two subgroups by dietary habits, respectively. 
clear
use "microbiome_score_diet_glycemics_and_covariates_609.dta" 
merge 1:1 SampleID using "Serum_metabolomics_data.dta"  //
keep if _merge==3 //539 remained
drop _merge
gen ratio1=EA_FAT_f_P/EA_CHO_f_P
//subgroups
xtile ratio1_median=ratio1, nq(2)

keep if ratio1_median==1 //HC
keep SampleID LF_score3 m1-m199 
order SampleID LF_score3 m1-m199 
preserve
tempname coef
tempfile res
postfile `coef' str20(var metabolites) float(n rho p) using "`res'", replace
foreach metabolites of varlist m1-m199 { 
  spearman `metabolites' LF_score3, stats(rho obs p)
  post `coef' ("`metabolites'") ("score3") (r(N)) (r(rho)) (r(p))
 }
postclose `coef'
use "`res'", clear
export excel using "score_metabolites_associations_HC.xlsx",  firstrow(variables) sheet("Sheet1") sheetreplace  
restore


keep if ratio1_median==2 //LC
keep SampleID LF_score3 m1-m199 
order SampleID LF_score3 m1-m199 
preserve
tempname coef
tempfile res
postfile `coef' str20(var metabolites) float(n rho p) using "`res'", replace
foreach metabolites of varlist m1-m199 { 
  spearman `metabolites' LF_score3, stats(rho obs p)
  post `coef' ("`metabolites'") ("score3") (r(N)) (r(rho)) (r(p))
 }
postclose `coef'
use "`res'", clear
export excel using "score_metabolites_associations_LC.xlsx",  firstrow(variables) sheet("Sheet1") sheetreplace  
restore




