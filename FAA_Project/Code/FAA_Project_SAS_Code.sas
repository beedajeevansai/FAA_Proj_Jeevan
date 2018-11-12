/*------------ Step 1: Importing data in to SAS and combining datsets ------------------*/
/*------------ Import data set in to SAS ------------------------------------------------*/

proc import out=FAA1_DATA
            datafile="/folders/myfolders/STAT Computing/FAA1.xls" 
            dbms=xls replace;
   sheet='FAA1';
run;

PROC PRINT DATA=FAA1_DATA;
RUN;

/*--------------- There might be cases where exact duplicates are present ---------------*/
/*--------------- Remove exact duplicates from imported data set ------------------------*/

proc sort data=FAA1_DATA
   out=FAA1_DATA
   NODUPRECS;
   by aircraft duration no_pasg speed_ground speed_air height pitch distance;
run;

PROC PRINT DATA=FAA1_DATA;
RUN;

/*---------------- Import FAA2 in to SAS ------------------------------------------------*/
proc import out=FAA2_DATA
            datafile="/folders/myfolders/STAT Computing/FAA2.xls" 
            dbms=xls replace;
   sheet='FAA2'; 
run;

PROC PRINT DATA=faa2_data;
RUN;

/*--------------- Remove exact duplicates from imported data set ------------------------*/

proc sort data=FAA2_DATA
   out=FAA2_DATA
   NODUPRECS;
   by aircraft no_pasg speed_ground speed_air height pitch distance;
run;

PROC PRINT DATA=FAA2_DATA;
RUN;

/*-------------- Upon observing, FAA2 doesn't have duration. Hence first let's combine 
data sets with out duration column -----------------------------------------------------*/
/*-------------- Create another data set without taking duration ------------------------*/
DATA FAA1_DATA_V2;
SET FAA1_DATA;
DROP DURATION;
RUN;

PROC PRINT DATA=faa1_data_v2;
RUN;

/*------- Combining FAA1 and FAA2 data sets. Here we are doing simple concatenation ------*/
DATA FAA1_FAA2_COMBINED;
	SET FAA1_DATA_V2 FAA2_DATA;
RUN;

PROC PRINT DATA=FAA1_FAA2_COMBINED;
RUN;

/*------- Removing exact duplicates from combined data set ------------------------------*/
proc sort data=FAA1_FAA2_COMBINED 
   out=FAA1_FAA2_COMBINED
   NODUPRECS;
   by aircraft no_pasg speed_ground speed_air height pitch distance;
run;

PROC PRINT DATA=FAA1_FAA2_COMBINED;
RUN;

/*--------- Creating Primary Key for combined data set --------------------------------*/

DATA FAA1_FAA2_COMBINED_V2;
SET FAA1_FAA2_COMBINED;
length PRIM_KEY $ 5000;
PRIM_KEY = catx('__', AIRCRAFT, NO_PASG, SPEED_GROUND, SPEED_AIR, HEIGHT, PITCH, DISTANCE);
put _all_;
RUN;

PROC PRINT DATA=faa1_faa2_combined_v2;
RUN;

/*------- There is duration column in FAA1_DATA. Need to extract the same
and join back to above created data set ----------------------------------------------*/

DATA FAA1_DATA_PRIM_KEY;
SET FAA1_DATA;
length PRIM_KEY $ 5000;
PRIM_KEY = catx('__', AIRCRAFT, NO_PASG, SPEED_GROUND, SPEED_AIR, HEIGHT, PITCH, DISTANCE);
put _all_;
KEEP PRIM_KEY DURATION;
RUN;

PROC PRINT DATA = FAA1_DATA_PRIM_KEY;
RUN;

/*------- Sort two data sets before doing merge ---------------------------------------*/

PROC SORT DATA=faa1_faa2_combined_v2;
BY PRIM_KEY;
RUN;

PROC SORT DATA=faa1_data_prim_key;
BY PRIM_KEY;
RUN;

/*------- Merge two data sets by PRIM_KEY created ---------------------------------------*/
DATA FAA1_FAA2_COMBINED_V3;
MERGE FAA1_FAA2_COMBINED_V2 FAA1_DATA_PRIM_KEY;
BY PRIM_KEY;
RUN;

PROC PRINT DATA=faa1_faa2_combined_v3;
RUN;

/*---------- Removing row which has all missing values -----------------------------*/
DATA FAA1_FAA2_N_MISS;
SET FAA1_FAA2_COMBINED_V3;
WHERE PRIM_KEY ^= '.__.__.__.__.__.';
RUN;

PROC PRINT DATA=faa1_faa2_n_miss;
RUN;

/*Step 2: Checking for Missing values, outliers and any abnormalities in data ---------*/
/*---------- Doing univariate analysis. This helps us understand missing values -------*/

/*---------- Univariate would give us missing percentage as well as summary stats 
for all numerical variables ------------------------------------------------------------*/

PROC UNIVARIATE DATA=FAA1_FAA2_N_MISS PLOT;
RUN;

PROC PRINT DATA=FAA1_FAA2_N_MISS;
RUN;

PROC CORR DATA=FAA1_FAA2_N_MISS;
VAR DISTANCE NO_PASG SPEED_GROUND SPEED_AIR HEIGHT PITCH DURATION;
RUN;


/*----- Do frequency distribution to understand missing values for categorical variables*/
/*----- Only aircraft is categorical variable ----------------------------------------*/

PROC FREQ DATA=FAA1_FAA2_N_MISS;
RUN;

/*---------- Below are missing value percentages --------------------------------------*/
/*---------- speed_air - 643 ~ 75% ----------------------------------------------------*/
/*---------- Duration - 50 ~ 5% --------------------------------------------------------*/


/*---------- Using limits provided, checking for abnormalities in data -----------------*/
/*---------- Since speed_air has so many missing values, make sure that only non missing
values are taken in to consideration --------------------------------------------------*/


DATA FAA1_FAA2_N_MISS;
SET FAA1_FAA2_N_MISS;
IF DURATION > 40 OR (DURATION = .) THEN DURATION_MEASURE = 'NORMAL';
ELSE DURATION_MEASURE = 'ABNORMAL';
IF SPEED_GROUND >= 30 AND SPEED_GROUND <= 140 THEN SPEED_GROUND_MEASURE = 'NORMAL';
ELSE SPEED_GROUND_MEASURE = 'ABNORMAL';
IF (SPEED_AIR >= 30 AND SPEED_AIR <= 140) OR (SPEED_AIR = .) THEN SPEED_AIR_MEASURE = 'NORMAL';
ELSE SPEED_AIR_MEASURE = 'ABNORMAL';
IF HEIGHT >= 6 THEN HEIGHT_MEASURE = 'NORMAL';
ELSE HEIGHT_MEASURE = 'ABNORMAL';
IF DISTANCE <= 6000 THEN DISTANCE_MEASURE = 'NORMAL';
ELSE DISTANCE_MEASURE = 'ABNORMAL';
RUN;

PROC PRINT DATA=FAA1_FAA2_N_MISS;
RUN;

PROC FREQ DATA=FAA1_FAA2_N_MISS;
TABLES DURATION_MEASURE SPEED_GROUND_MEASURE SPEED_AIR_MEASURE HEIGHT_MEASURE DISTANCE_MEASURE;
RUN;

/*----------- Now let's delete abnormal values -----------------------------------*/

DATA FAA1_FAA2_N_MISS_V2;
SET FAA1_FAA2_N_MISS;
IF DURATION_MEASURE = 'ABNORM' OR SPEED_GROUND_MEASURE = 'ABNORM' OR SPEED_AIR_MEASURE 
= 'ABNORM' OR HEIGHT_MEASURE = 'ABNORM' OR DISTANCE_MEASURE = 'ABNORM' THEN DELETE;
RUN;

/*---------- Rechecking for missing values ----------------------------------------*/

PROC FREQ DATA=FAA1_FAA2_N_MISS_V2;
TABLES SPEED_GROUND_MEASURE SPEED_AIR_MEASURE HEIGHT_MEASURE DISTANCE_MEASURE;
RUN;

/*---------- Handling Missing Values for duration ----------------------------------*/
proc stdize data = FAA1_FAA2_N_MISS_V2 
reponly method = MEDIAN out = FAA1_FAA2_N_MISS_V3;
var DURATION;
run;

/*--------- Checking distribtuons of data. Done after cleaning data ----------------*/

PROC CORR DATA=FAA1_FAA2_N_MISS_V3;
VAR DISTANCE NO_PASG SPEED_GROUND SPEED_AIR HEIGHT PITCH DURATION;
RUN;

PROC UNIVARIATE data= FAA1_FAA2_N_MISS_V3 PLOT;
RUN;

PROC PRINT DATA=FAA1_FAA2_N_MISS_V3;
RUN;

title "Landing Distance vs No of passengers";
proc plot data=FAA1_FAA2_N_MISS_V3; 
plot distance*no_pasg = '*';
run;

title "Landing Distance vs Speed Ground";
proc plot data=FAA1_FAA2_N_MISS_V3; 
plot distance*speed_ground = '*';
run;

title "Landing Distance vs Speed Air";
proc plot data=FAA1_FAA2_N_MISS_V3; 
plot distance*speed_air = '*';
run;

title "Landing Distance vs Height";
proc plot data=FAA1_FAA2_N_MISS_V3; 
plot distance*height = '*';
run;

title "Landing Distance vs Pitch";
proc plot data=FAA1_FAA2_N_MISS_V3; 
plot distance*pitch = '*';
run;

title "Landing Distance vs Duration";
proc plot data=FAA1_FAA2_N_MISS_V3; 
plot distance*duration = '*';
run;


title "Landing Distance vs No of passengers";
proc sgplot data=FAA1_FAA2_N_MISS_V3; 
scatter x=no_pasg y=distance;
run;

title "Landing Distance vs No of passengers";
proc sgplot data=FAA1_FAA2_N_MISS_V3; 
scatter x=no_pasg y=distance;
run;

title "Landing Distance vs Ground Speed";
proc sgplot data=FAA1_FAA2_N_MISS_V3; 
scatter x=speed_ground y=distance;
run;

title "Landing Distance vs Air Speed";
proc sgplot data=FAA1_FAA2_N_MISS_V3; 
scatter x=speed_air y=distance;
run;

title "Landing Distance vs Height";
proc sgplot data=FAA1_FAA2_N_MISS_V3; 
scatter x=height y=distance;
run;

title "Landing Distance vs Pitch";
proc sgplot data=FAA1_FAA2_N_MISS_V3; 
scatter x=pitch y=distance;
run;


/*--- To plot box-plot of distance across types of aircrafts -----------------*/
PROC BOXPLOT DATA=FAA1_FAA2_COMBINED_V3;
PLOT DISTANCE*AIRCRAFT;
TITLE TO UNDERSTAND DIFFERENCES BETWEEN AIRBUS AND BOEING;
RUN;

/*--- Performing Two Sample T-Test ------------------------------------------*/
PROC TTEST DATA=FAA1_FAA2_COMBINED_V3;
CLASS AIRCRAFT;
VAR DISTANCE;
RUN;

/*--- Generating correlation matrix for Airbus ------------------------------*/
PROC CORR DATA=FAA1_FAA2_N_MISS_V3;
VAR DISTANCE NO_PASG SPEED_GROUND SPEED_AIR HEIGHT PITCH DURATION;
WHERE AIRCRAFT = 'airbus';
RUN;

/*--- Generating correlation matrix for Boeing ------------------------------*/
PROC CORR DATA=FAA1_FAA2_N_MISS_V3;
VAR DISTANCE NO_PASG SPEED_GROUND SPEED_AIR HEIGHT PITCH DURATION;
WHERE AIRCRAFT = 'boeing';
RUN;

/*--- Model Iterations and Selection of Model -----------------------------*/
/*--- Iter 1 ---------------------------------------------------------------*/
PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = NO_PASG SPEED_GROUND HEIGHT PITCH DURATION;
WHERE AIRCRAFT = 'airbus';
RUN;

/*--- Iter 2 -----------------------------------------------------------------*/
PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND HEIGHT PITCH DURATION;
WHERE AIRCRAFT = 'airbus';
RUN;

/*--- Iter 3 ----------------------------------------------------------------*/
PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND HEIGHT DURATION;
WHERE AIRCRAFT = 'airbus';
RUN;

/*--- Iter 4 ----------------------------------------------------------------*/
PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND HEIGHT;
WHERE AIRCRAFT = 'airbus';
RUN;

/*--- Iter 5 ----------------------------------------------------------------*/
PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND;
WHERE AIRCRAFT = 'airbus';
RUN;

/*--- Iter 1 ---------------------------------------------------------------*/
PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = NO_PASG SPEED_GROUND HEIGHT PITCH DURATION;
WHERE AIRCRAFT = 'boeing';
RUN;

/*--- Iter 2 -----------------------------------------------------------------*/
PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND HEIGHT PITCH DURATION;
WHERE AIRCRAFT = 'boeing';
RUN;

/*--- Iter 3 ----------------------------------------------------------------*/
PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND HEIGHT DURATION;
WHERE AIRCRAFT = 'boeing';
RUN;

/*--- Iter 4 ----------------------------------------------------------------*/
PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND HEIGHT;
WHERE AIRCRAFT = 'boeing';
RUN;

/*--- Iter 5 ----------------------------------------------------------------*/
PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND;
WHERE AIRCRAFT = 'boeing';
RUN;














PROC CORR DATA=FAA1_FAA2_N_MISS_V3;
VAR DISTANCE NO_PASG SPEED_GROUND SPEED_AIR HEIGHT PITCH DURATION;
WHERE AIRCRAFT = 'airbus';
RUN;

PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = NO_PASG SPEED_GROUND HEIGHT PITCH DURATION;
WHERE AIRCRAFT = 'airbus';
RUN;

PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND HEIGHT PITCH DURATION;
WHERE AIRCRAFT = 'airbus';
RUN;

PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND HEIGHT DURATION;
WHERE AIRCRAFT = 'airbus';
RUN;

PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND HEIGHT;
WHERE AIRCRAFT = 'airbus';
RUN;

PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND;
WHERE AIRCRAFT = 'airbus';
RUN;








PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND HEIGHT;
RUN;

PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = SPEED_GROUND;
RUN;




PROC CORR DATA=FAA1_FAA2_N_MISS_V3;
VAR DISTANCE NO_PASG SPEED_GROUND SPEED_AIR HEIGHT PITCH;
WHERE AIRCRAFT = 'boeing';
RUN;

PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = NO_PASG SPEED_GROUND HEIGHT PITCH;
RUN;

PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = NO_PASG SPEED_GROUND HEIGHT PITCH;
WHERE AIRCRAFT = 'airbus';
RUN;

PROC REG DATA=FAA1_FAA2_N_MISS_V3;
MODEL DISTANCE = NO_PASG SPEED_GROUND HEIGHT PITCH;
WHERE AIRCRAFT = 'boeing';
RUN;


