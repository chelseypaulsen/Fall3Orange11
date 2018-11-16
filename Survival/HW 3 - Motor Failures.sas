%let path = C:\Users\derri\Documents\NC State Classes\Survival Analysis\Data\survivalcsv;
libname Survival "&path.";


/*Importing data*/

%macro import_data (file=katrina, sheet=katrina);
PROC IMPORT OUT= Survival.&sheet. DATAFILE= "&path.\&file..csv" 
            DBMS=csv REPLACE;
     		GETNAMES=YES;
RUN;
%mend import_data;

%import_data(file=katrina, sheet=katrina)

/*Add a ID Variable*/
data Survival.Katrina2;
	length ID 4;
	set Survival.Katrina;
	ID + 1;
run;

/*Create Macros that I will use in the next step*/
%let _13 = h12+h11+h10+h9+h8+h7+h6+h5+h4+h3+h2+h1;
%let _14 = h13+h12+h11+h10+h9+h8+h7+h6+h5+h4+h3+h2;
%let _15 = h14+h13+h12+h11+h10+h9+h8+h7+h6+h5+h4+h3;
%let _16 = h15+h14+h13+h12+h11+h10+h9+h8+h7+h6+h5+h4;
%let _17 = h16+h15+h14+h13+h12+h11+h10+h9+h8+h7+h6+h5;
%let _18 = h17+h16+h15+h14+h13+h12+h11+h10+h9+h8+h7+h6;
%let _19 = h18+h17+h16+h15+h14+h13+h12+h11+h10+h9+h8+h7;
%let _20 = h19+h18+h17+h16+h15+h14+h13+h12+h11+h10+h9+h8;
%let _21 = h20+h19+h18+h17+h16+h15+h14+h13+h12+h11+h10+h9;
%let _22 = h21+h20+h19+h18+h17+h16+h15+h14+h13+h12+h11+h10;
%let _23 = h22+h21+h20+h19+h18+h17+h16+h15+h14+h13+h12+h11;
%let _24 = h23+h22+h21+h20+h19+h18+h17+h16+h15+h14+h13+h12;
%let _25 = h24+h23+h22+h21+h20+h19+h18+h17+h16+h15+h14+h13;
%let _26 = h25+h24+h23+h22+h21+h20+h19+h18+h17+h16+h15+h14;
%let _27 = h26+h25+h24+h23+h22+h21+h20+h19+h18+h17+h16+h15;
%let _28 = h27+h26+h25+h24+h23+h22+h21+h20+h19+h18+h17+h16;
%let _29 = h28+h27+h26+h25+h24+h23+h22+h21+h20+h19+h18+h17;
%let _30 = h29+h28+h27+h26+h25+h24+h23+h22+h21+h20+h19+h18;
%let _31 = h30+h29+h28+h27+h26+h25+h24+h23+h22+h21+h20+h19;
%let _32 = h31+h30+h29+h28+h27+h26+h25+h24+h23+h22+h21+h20;
%let _33 = h32+h31+h30+h29+h28+h27+h26+h25+h24+h23+h22+h21;
%let _34 = h33+h32+h31+h30+h29+h28+h27+h26+h25+h24+h23+h22;
%let _35 = h34+h33+h32+h31+h30+h29+h28+h27+h26+h25+h24+h23;
%let _36 = h35+h34+h33+h32+h31+h30+h29+h28+h27+h26+h25+h24;
%let _37 = h36+h35+h34+h33+h32+h31+h30+h29+h28+h27+h26+h25;
%let _38 = h37+h36+h35+h34+h33+h32+h31+h30+h29+h28+h27+h26;
%let _39 = h38+h37+h36+h35+h34+h33+h32+h31+h30+h29+h28+h27;
%let _40 = h39+h38+h37+h36+h35+h34+h33+h32+h31+h30+h29+h28;
%let _41 = h40+h39+h38+h37+h36+h35+h34+h33+h32+h31+h30+h29;
%let _42 = h41+h40+h39+h38+h37+h36+h35+h34+h33+h32+h31+h30;
%let _43 = h42+h41+h40+h39+h38+h37+h36+h35+h34+h33+h32+h31;
%let _44 = h43+h42+h41+h40+h39+h38+h37+h36+h35+h34+h33+h32;
%let _45 = h44+h43+h42+h41+h40+h39+h38+h37+h36+h35+h34+h33;
%let _46 = h45+h44+h43+h42+h41+h40+h39+h38+h37+h36+h35+h34;
%let _47 = h46+h45+h44+h43+h42+h41+h40+h39+h38+h37+h36+h35;
%let _48 = h47+h46+h45+h44+h43+h42+h41+h40+h39+h38+h37+h36;

/*Create a dataset that calculates the number of hours the 
pump was running before failure and outputs the result in 
the variable hour_count_b4_fail*/

data Survival.Katrina3;
	set Survival.Katrina2;
	if hour=13 then hour_count_b4_fail = &_13.;
	else if hour=14 then hour_count_b4_fail = &_14.;
	else if hour=15 then hour_count_b4_fail = &_15.;
	else if hour=16 then hour_count_b4_fail = &_16.;
	else if hour=17 then hour_count_b4_fail = &_17.;
	else if hour=18 then hour_count_b4_fail = &_18.;
	else if hour=19 then hour_count_b4_fail = &_19.;
	else if hour=20 then hour_count_b4_fail = &_20.;
	else if hour=21 then hour_count_b4_fail = &_21.;
	else if hour=22 then hour_count_b4_fail = &_22.;
	else if hour=23 then hour_count_b4_fail = &_23.;
	else if hour=24 then hour_count_b4_fail = &_24.;
	else if hour=25 then hour_count_b4_fail = &_25.;
	else if hour=26 then hour_count_b4_fail = &_26.;
	else if hour=27 then hour_count_b4_fail = &_27.;
	else if hour=28 then hour_count_b4_fail = &_28.;
	else if hour=29 then hour_count_b4_fail = &_29.;
	else if hour=30 then hour_count_b4_fail = &_30.;
	else if hour=31 then hour_count_b4_fail = &_31.;
	else if hour=32 then hour_count_b4_fail = &_32.;
	else if hour=33 then hour_count_b4_fail = &_33.;
	else if hour=34 then hour_count_b4_fail = &_34.;
	else if hour=35 then hour_count_b4_fail = &_35.;
	else if hour=36 then hour_count_b4_fail = &_36.;
	else if hour=37 then hour_count_b4_fail = &_37.;
	else if hour=38 then hour_count_b4_fail = &_38.;
	else if hour=39 then hour_count_b4_fail = &_39.;
	else if hour=40 then hour_count_b4_fail = &_40.;
	else if hour=41 then hour_count_b4_fail = &_41.;
	else if hour=42 then hour_count_b4_fail = &_42.;
	else if hour=43 then hour_count_b4_fail = &_43.;
	else if hour=44 then hour_count_b4_fail = &_44.;
	else if hour=45 then hour_count_b4_fail = &_45.;
	else if hour=46 then hour_count_b4_fail = &_46.;
	else if hour=47 then hour_count_b4_fail = &_47.;
	else if hour=48 then hour_count_b4_fail = &_48.;
	else hour_count_b4_fail = 0;
run;


/*Some quick sql to see the number of failures that were*/
/*running for 12 hours before failure*/

title "Motor Failures that had 12 Consecutive Hours runninng";
proc sql;
select count(ID) as Count
from Survival.Katrina3
where hour_count_b4_fail = 12 and reason = 2;
; quit;

title "Total Motor Failures";
proc sql;
select count(ID) as Count
from Survival.Katrina3
where reason = 2;
; quit;

title "Non Motor Failures that had 12 Consecutive Hours runninng";
proc sql;
select 	count(ID) as Count
from Survival.Katrina3
where hour_count_b4_fail = 12 and reason in (1, 3, 4);
; quit;

title "Total Non Motor Failures";
proc sql;
select 	count(ID) as Count
from Survival.Katrina3
where reason in (1, 3, 4)
; quit;
