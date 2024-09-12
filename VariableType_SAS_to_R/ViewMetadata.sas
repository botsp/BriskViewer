/*A sas program that used to view metadat and summary variables*/

options validvarname=v7;
PROC IMPORT DATAFILE= "C:\Users\kevin\OneDrive - The University Of Hong Kong\Desktop\R\SDTMIG_v3.3.xlsx" 
OUT= WORK.sdtm
DBMS=XLSX
REPLACE;
SHEET="Variables"; 
GETNAMES=YES;
RUN;

data sdtm_x;
	set sdtm(where=(Type="Char" and (
	Variable_Name not in ("STUDYID" "DOMAIN" "USUBJID" "SUBJID" "SITEID" "INVID" "INVNAM")
	and Variable_label not in ("Specimen ID" "Group ID" "Sponsor-Defined Identifier" "Link ID" "Link Group ID" "Reference ID" "Dose Description")
	and (INDEX(UPCASE(Variable_label), 'REASON NOT DONE') =0 and
	INDEX(UPCASE(Variable_label), 'REASON NOT PERFORMED') =0 and
	INDEX(UPCASE(Variable_label), 'REASON PARAMETER NOT CALCULATED') =0)
	and (FINDW(UPCASE(Variable_label), 'UNIT') = 0 and FINDW(UPCASE(Variable_label), 'UNITS') = 0)
	and PRXMATCH("/(DTC|DTM|DUR|ENTPT|ORRES|ORRESU|ORNRLO|ORNRHI|STRESC|STRESU|STNRC|STREFC|STTPT)$/i", trim(Variable_Name)) = 0
	and (PRXMATCH("/^COVAL/i", trim(Variable_Name)) = 0)
	)));
run;

PRXMATCH("/(VISIT|EPOCH|CAT|SCAT|TEST|TESTCD|ARM|ARMCD|DECOD|STRF|ENRF|LOC|LAT|STAT|TPT)$/i", trim(Variable_Name))=0 AND INDEX(VARIABLE_LABEL,"Flag")=0
options validvarname=v7;
PROC IMPORT DATAFILE= "C:\Users\kevin\OneDrive - The University Of Hong Kong\Desktop\R\ADaMIG_v1.3.xlsx" 
OUT= WORK.adam
DBMS=XLSX
REPLACE;
SHEET="Variables"; 
GETNAMES=YES;
RUN;
data adam_x;
	set adam(where=(Type="Char" and (
	Variable_Name not in ("STUDYID" "DOMAIN" "USUBJID" "SUBJID" "SITEID" "INVID" "INVNAM")
	and Variable_label not in ("Specimen ID" "Group ID" "Sponsor-Defined Identifier" "Link ID" "Link Group ID" "Reference ID" "Dose Description")
	and (INDEX(UPCASE(Variable_label), 'REASON NOT DONE') =0 and
	INDEX(UPCASE(Variable_label), 'REASON NOT PERFORMED') =0 and
	INDEX(UPCASE(Variable_label), 'REASON PARAMETER NOT CALCULATED') =0)
	and (FINDW(UPCASE(Variable_label), 'UNIT') = 0 and FINDW(UPCASE(Variable_label), 'UNITS') = 0)
	and PRXMATCH("/(DTC|DTM|DUR|ENTPT|ORRES|ORRESU|ORNRLO|ORNRHI|STRESC|STRESU|STNRC|STREFC|STTPT)$/i", trim(Variable_Name)) = 0
	and (PRXMATCH("/^COVAL/i", trim(Variable_Name)) = 0)
	)));
run;


