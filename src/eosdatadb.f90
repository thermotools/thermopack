
 module eosdatadb

    type :: kijdatadb
       character (len=10) :: eosid
       character (len=12) :: mruleid
       integer setno
       character (len=8) :: uid1, uid2
       real :: kijvalue
    end type kijdatadb


   integer, parameter :: maxkij = 419+31+78+239+91+78+212+4+17
   ! 31 from Patel-Teja article
   ! 78 SimSci TDM2.0 / ProII 9.1 (PR Set no 2)
   ! 239 Hysys (PR set no 3) + 17 (Updated for N2)
   ! 91 from SIMSCI/SRK (SRK Set no 2)
   ! 78 from HSYS (SRK Set no 3)
   ! 212 from SPUNG/SRK, SPUNG/SRKGB and SPUNG/PR from TPlib
   ! 4 for mixtures with R1234yf (PR set 1) 

   type (kijdatadb), dimension (maxkij), parameter :: kijdb = (/ &
         kijdatadb("srk","classic", 1,"C1","C2", -0.0078), &
         kijdatadb("srk","classic", 1,"C1","C2_1",  0.02), &
         kijdatadb("srk","classic", 1,"C1","C3",  0.009), &
         kijdatadb("srk","classic", 1,"C1","IC4",  0.0241), &
         kijdatadb("srk","classic", 1,"C1","NC4",  0.0056), &
         kijdatadb("srk","classic", 1,"C1","NC5",  0.019), &
         kijdatadb("srk","classic", 1,"C1","H2S",  0.091), &
         kijdatadb("srk","classic", 1,"C2","C2_1",  0.0112), &
         kijdatadb("srk","classic", 1,"C2","C3", -0.0022), &
         kijdatadb("srk","classic", 1,"C2","IC4", -0.01), &
         kijdatadb("srk","classic", 1,"C2","NC4",  0.0067), &
         kijdatadb("srk","classic", 1,"C2","NC5",  0.0056), &
         kijdatadb("srk","classic", 1,"NC4","C2_1",  0.1), &
         kijdatadb("srk","classic", 1,"C3","IC4", -0.01), &
         kijdatadb("srk","classic", 1,"C3","NC5",  0.023), &
         kijdatadb("srk","classic", 1,"IC4","NC4",  0.0011), &
         kijdatadb("srk","classic", 1,"NC4","NC5",  0.0204), &
         kijdatadb("srk","classic", 1,"H2O","C1",  0.), &
         kijdatadb("srk","classic", 1,"H2O","MEG",  -0.063), &
         kijdatadb("srk","classic", 1,"CO2","MEG",  0.05), &
         kijdatadb("srk","classic", 1,"CO2","O2",  0.104), &
         kijdatadb("srk","classic", 1,"CO2","NH3",  0.), &
         kijdatadb("srk","classic", 1,"CO2","N2O",  0.004), &
         kijdatadb("srk","classic", 1,"CO2","N2O4",  0.), &
         kijdatadb("srk","classic", 1,"CO2","CO", -0.08), &
         kijdatadb("srk","classic", 1,"CO2","C1",  0.095), &
         kijdatadb("srk","classic", 1,"CO2","C2",  0.15), &
         kijdatadb("srk","classic", 1,"CO2","C2_1",  0.15), &
         kijdatadb("srk","classic", 1,"CO2","C3",  0.15), &
         kijdatadb("srk","classic", 1,"CO2","H2O",  0.074), &
         kijdatadb("srk","classic", 1,"CO2","IC4",  0.15), &
         kijdatadb("srk","classic", 1,"CO2","IC5",  0.15), &
         kijdatadb("srk","classic", 1,"CO2","NC10",  0.15), &
         kijdatadb("srk","classic", 1,"CO2","NC11",  0.15), &
         kijdatadb("srk","classic", 1,"CO2","NC4",  0.15), &
         kijdatadb("srk","classic", 1,"CO2","NC5",  0.15), &
         kijdatadb("srk","classic", 1,"CO2","H2S",  0.1), &
         kijdatadb("srk","classic", 1,"CO2","N2", -0.051), &
         kijdatadb("srk","classic", 1,"CO2","AR",  0.088), &
         kijdatadb("srk","classic", 1,"CO2","SO2",  0.071), &
         kijdatadb("srk","classic", 1,"CO2","H2",  0.009), &
         kijdatadb("srk","classic", 1,"H2S","NC4",  0.06), &
         kijdatadb("srk","classic", 1,"H2S","NC5",  0.06), &
         kijdatadb("srk","classic", 1,"H2S","NC6",  0.05), &
         kijdatadb("srk","classic", 1,"H2S","NC7",  0.04), &
         kijdatadb("srk","classic", 1,"H2S","NC8",  0.04), &
         kijdatadb("srk","classic", 1,"H2S","NC9",  0.03), &
         kijdatadb("srk","classic", 1,"MEOH","CO2",  0.017), &
         kijdatadb("srk","classic", 1,"N2","C1",  0.034), &
         kijdatadb("srk","classic", 1,"N2","C2",  0.06), &
         kijdatadb("srk","classic", 1,"N2","C2_1",  0.075), &
         kijdatadb("srk","classic", 1,"N2","C3",  0.09), &
         kijdatadb("srk","classic", 1,"N2","IC4",  0.113), &
         kijdatadb("srk","classic", 1,"N2","IC5",  0.087), &
         kijdatadb("srk","classic", 1,"N2","NC10",  0.08), &
         kijdatadb("srk","classic", 1,"N2","NC11",  0.08), &
         kijdatadb("srk","classic", 1,"N2","NC4",  0.113), &
         kijdatadb("srk","classic", 1,"N2","NC5",  0.14), &
         kijdatadb("srk","classic", 1,"N2","NC6",  0.15), &
         kijdatadb("srk","classic", 1,"N2","NC7",  0.142), &
         kijdatadb("srk","classic", 1,"N2","NC8",  0.08), &
         kijdatadb("srk","classic", 1,"N2","NC9",  0.08), &
         kijdatadb("srk","classic", 1,"N2","O2", -0.011), &
         kijdatadb("srk","classic", 1,"R12","R11",  0.0054), &
         kijdatadb("srk","classic", 1,"R12","R114",  0.0015), &
         kijdatadb("srk","classic", 1,"R12","R152a",  0.0867), &
         kijdatadb("srk","classic", 1,"R13","R11",  0.0262), &
         kijdatadb("srk","classic", 1,"R13","R12",  0.0299), &
         kijdatadb("srk","classic", 1,"R14","R13",  0.0304), &
         kijdatadb("srk","classic", 1,"R14","R23",  0.1008), &
         kijdatadb("srk","classic", 1,"R22","R11",  0.0466), &
         kijdatadb("srk","classic", 1,"R22","R114",  0.0399), &
         kijdatadb("srk","classic", 1,"R22","R12",  0.0564), &
         kijdatadb("srk","classic", 1,"R22","R142b",  0.0057), &
         kijdatadb("srk","classic", 1,"R22","R115",  0.089), &
         kijdatadb("srk","classic", 1,"R23","R13",  0.1032), &
         kijdatadb("srk","classic", 1,"R218","R152a",  0.12), &
         kijdatadb("srk","classic", 1,"R125","R143a", -0.0111), &
         kijdatadb("srk","classic", 1,"R125","R134a", -0.0024), &
         kijdatadb("srk","classic", 1,"R143a","R134a",  0.0013), &

! Set 2: From SIMCSCI TDM2.0 / PROII 9.1 "?" replaced by 0.0
         kijdatadb("srk","classic",2,"C1","C3",0.009), &
         kijdatadb("srk","classic",2,"C1","NC6",0.0374), &
         kijdatadb("srk","classic",2,"C1","C2",-0.0078), &
         kijdatadb("srk","classic",2,"C1","CO2",0.0933), &
         kijdatadb("srk","classic",2,"C1","H2S",0.09), &
         kijdatadb("srk","classic",2,"C1","N2",0.03), &
         kijdatadb("srk","classic",2,"C1","H2O",0.52), &
         kijdatadb("srk","classic",2,"C1","NC4",0.0056), &
         kijdatadb("srk","classic",2,"C1","IC4",0.0241), &
         kijdatadb("srk","classic",2,"C1","NC5",0.019), &
         kijdatadb("srk","classic",2,"C1","NC7",0.0307), &
         kijdatadb("srk","classic",2,"C1","NC8",0.0448), &
         kijdatadb("srk","classic",2,"C1","IC5",-0.0078), &
         kijdatadb("srk","classic",2,"C3","NC6",-0.0022), &
         kijdatadb("srk","classic",2,"C3","C2",-0.0022), &
         kijdatadb("srk","classic",2,"C3","CO2",0.1289), &
         kijdatadb("srk","classic",2,"C3","H2S",0.076), &
         kijdatadb("srk","classic",2,"C3","N2",0.09), &
         kijdatadb("srk","classic",2,"C3","H2O",0.53), &
         kijdatadb("srk","classic",2,"C3","NC4",0), &
         kijdatadb("srk","classic",2,"C3","IC4",-0.01), &
         kijdatadb("srk","classic",2,"C3","NC5",0.0233), &
         kijdatadb("srk","classic",2,"C3","NC7",0.0044), &
         kijdatadb("srk","classic",2,"C3","NC8",0.0), &
         kijdatadb("srk","classic",2,"C3","IC5",0.0078), &
         kijdatadb("srk","classic",2,"NC6","C2",-0.0156), &
         kijdatadb("srk","classic",2,"NC6","CO2",0.112), &
         kijdatadb("srk","classic",2,"NC6","H2S",0.068), &
         kijdatadb("srk","classic",2,"NC6","N2",0.166), &
         kijdatadb("srk","classic",2,"NC6","H2O",0.5), &
         kijdatadb("srk","classic",2,"NC6","NC4",-0.0111), &
         kijdatadb("srk","classic",2,"NC6","IC4",0.0), &
         kijdatadb("srk","classic",2,"NC6","NC5",0.0), &
         kijdatadb("srk","classic",2,"NC6","NC7",-0.0011), &
         kijdatadb("srk","classic",2,"NC6","NC8",0.0), &
         kijdatadb("srk","classic",2,"NC6","IC5",0.0), &
         kijdatadb("srk","classic",2,"C2","CO2",0.1363), &
         kijdatadb("srk","classic",2,"C2","H2S",0.085), &
         kijdatadb("srk","classic",2,"C2","N2",0.06), &
         kijdatadb("srk","classic",2,"C2","H2O",0.55), &
         kijdatadb("srk","classic",2,"C2","NC4",0.0067), &
         kijdatadb("srk","classic",2,"C2","IC4",-0.01), &
         kijdatadb("srk","classic",2,"C2","NC5",0.0056), &
         kijdatadb("srk","classic",2,"C2","NC7",0.0041), &
         kijdatadb("srk","classic",2,"C2","NC8",0.017), &
         kijdatadb("srk","classic",2,"C2","IC5",0.0), &
         kijdatadb("srk","classic",2,"CO2","H2S",0.1), &
         kijdatadb("srk","classic",2,"CO2","N2",-0.03), &
         kijdatadb("srk","classic",2,"CO2","H2O",0.23), &
         kijdatadb("srk","classic",2,"CO2","NC4",0.143), &
         kijdatadb("srk","classic",2,"CO2","IC4",0.1285), &
         kijdatadb("srk","classic",2,"CO2","NC5",0.131), &
         kijdatadb("srk","classic",2,"CO2","NC7",0.11), &
         kijdatadb("srk","classic",2,"CO2","NC8",0.12), &
         kijdatadb("srk","classic",2,"CO2","IC5",0.131), &
         kijdatadb("srk","classic",2,"H2S","N2",0.17), &
         kijdatadb("srk","classic",2,"H2S","H2O",0.135), &
         kijdatadb("srk","classic",2,"H2S","NC4",0.06), &
         kijdatadb("srk","classic",2,"H2S","IC4",0.06), &
         kijdatadb("srk","classic",2,"H2S","NC5",0.068), &
         kijdatadb("srk","classic",2,"H2S","NC7",0.068), &
         kijdatadb("srk","classic",2,"H2S","NC8",0.06), &
         kijdatadb("srk","classic",2,"H2S","IC5",0.065), &
         kijdatadb("srk","classic",2,"N2","H2O",0.53), &
         kijdatadb("srk","classic",2,"N2","NC4",0.113), &
         kijdatadb("srk","classic",2,"N2","IC4",0.113), &
         kijdatadb("srk","classic",2,"N2","NC5",0.14), &
         kijdatadb("srk","classic",2,"N2","NC7",0.2), &
         kijdatadb("srk","classic",2,"N2","NC8",0.0), &
         kijdatadb("srk","classic",2,"N2","IC5",0.14), &
         kijdatadb("srk","classic",2,"H2O","NC4",0.52), &
         kijdatadb("srk","classic",2,"H2O","IC4",0.52), &
         kijdatadb("srk","classic",2,"H2O","NC5",0.5), &
         kijdatadb("srk","classic",2,"H2O","NC7",0.5), &
         kijdatadb("srk","classic",2,"H2O","NC8",0.5), &
         kijdatadb("srk","classic",2,"H2O","IC5",0.5), &
         kijdatadb("srk","classic",2,"NC4","IC4",0.0011), &
         kijdatadb("srk","classic",2,"NC4","NC5",0.0204), &
         kijdatadb("srk","classic",2,"NC4","NC7",-0.0004), &
         kijdatadb("srk","classic",2,"NC4","NC8",0.0), &
         kijdatadb("srk","classic",2,"NC4","IC5",0.0), &
         kijdatadb("srk","classic",2,"IC4","NC5",0.0), &
         kijdatadb("srk","classic",2,"IC4","NC7",0.0), &
         kijdatadb("srk","classic",2,"IC4","NC8",0.0), &
         kijdatadb("srk","classic",2,"IC4","IC5",0.0), &
         kijdatadb("srk","classic",2,"NC5","NC7",0.0019), &
         kijdatadb("srk","classic",2,"NC5","NC8",-0.0022), &
         kijdatadb("srk","classic",2,"NC5","IC5",0.0), &
         kijdatadb("srk","classic",2,"NC7","NC8",0.0), &
         kijdatadb("srk","classic",2,"NC7","IC5",0.0), &
         kijdatadb("srk","classic",2,"NC8","IC5",0.0), &
! From Hysys 
         kijdatadb("srk","classic",3,"C1","C2",2.24E-03), &
         kijdatadb("srk","classic",3,"C1","C3",6.83E-03), &
         kijdatadb("srk","classic",3,"C1","IC4",1.31E-02), &
         kijdatadb("srk","classic",3,"C1","NC4",1.23E-02), &
         kijdatadb("srk","classic",3,"C1","IC5",1.76E-02), &
         kijdatadb("srk","classic",3,"C1","NC5",1.79E-02), &
         kijdatadb("srk","classic",3,"C1","NC6",2.35E-02), &
         kijdatadb("srk","classic",3,"C1","NC7",2.89E-02), &
         kijdatadb("srk","classic",3,"C1","NC8",3.42E-02), &
         kijdatadb("srk","classic",3,"C1","H2S",8.88E-02), &
         kijdatadb("srk","classic",3,"C1","H2O",0.5), &
         kijdatadb("srk","classic",3,"C1","CO2",9.56E-02), &
         kijdatadb("srk","classic",3,"C2","C3",1.26E-03), &
         kijdatadb("srk","classic",3,"C2","IC4",4.57E-03), &
         kijdatadb("srk","classic",3,"C2","NC4",4.10E-03), &
         kijdatadb("srk","classic",3,"C2","IC5",7.41E-03), &
         kijdatadb("srk","classic",3,"C2","NC5",7.61E-03), &
         kijdatadb("srk","classic",3,"C2","NC6",1.14E-02), &
         kijdatadb("srk","classic",3,"C2","NC7",1.53E-02), &
         kijdatadb("srk","classic",3,"C2","NC8",1.93E-02), &
         kijdatadb("srk","classic",3,"C2","H2S",8.62E-02), &
         kijdatadb("srk","classic",3,"C2","H2O",0.5), &
         kijdatadb("srk","classic",3,"C2","CO2",0.140100002), &
         kijdatadb("srk","classic",3,"C3","IC4",1.04E-03), &
         kijdatadb("srk","classic",3,"C3","NC4",8.19E-04), &
         kijdatadb("srk","classic",3,"C3","IC5",2.58E-03), &
         kijdatadb("srk","classic",3,"C3","NC5",2.70E-03), &
         kijdatadb("srk","classic",3,"C3","NC6",5.14E-03), &
         kijdatadb("srk","classic",3,"C3","NC7",7.89E-03), &
         kijdatadb("srk","classic",3,"C3","NC8",1.09E-02), &
         kijdatadb("srk","classic",3,"C3","H2S",9.25E-02), &
         kijdatadb("srk","classic",3,"C3","H2O",0.481900007), &
         kijdatadb("srk","classic",3,"C3","CO2",0.136800006), &
         kijdatadb("srk","classic",3,"IC4","NC4",1.34E-05), &
         kijdatadb("srk","classic",3,"IC4","IC5",3.46E-04), &
         kijdatadb("srk","classic",3,"IC4","NC5",3.90E-04), &
         kijdatadb("srk","classic",3,"IC4","NC6",1.57E-03), &
         kijdatadb("srk","classic",3,"IC4","NC7",3.22E-03), &
         kijdatadb("srk","classic",3,"IC4","NC8",5.21E-03), &
         kijdatadb("srk","classic",3,"IC4","H2S",5.60E-02), &
         kijdatadb("srk","classic",3,"IC4","H2O",0.518000007), &
         kijdatadb("srk","classic",3,"IC4","CO2",0.136800006), &
         kijdatadb("srk","classic",3,"NC4","IC5",4.95E-04), &
         kijdatadb("srk","classic",3,"NC4","NC5",5.47E-04), &
         kijdatadb("srk","classic",3,"NC4","NC6",1.87E-03), &
         kijdatadb("srk","classic",3,"NC4","NC7",3.65E-03), &
         kijdatadb("srk","classic",3,"NC4","NC8",5.75E-03), &
         kijdatadb("srk","classic",3,"NC4","H2S",6.26E-02), &
         kijdatadb("srk","classic",3,"NC4","H2O",0.518000007), &
         kijdatadb("srk","classic",3,"NC4","CO2",0.141200006), &
         kijdatadb("srk","classic",3,"IC5","NC5",1.23E-06), &
         kijdatadb("srk","classic",3,"IC5","NC6",4.40E-04), &
         kijdatadb("srk","classic",3,"IC5","NC7",1.46E-03), &
         kijdatadb("srk","classic",3,"IC5","NC8",2.88E-03), &
         kijdatadb("srk","classic",3,"IC5","H2S",6.50E-02), &
         kijdatadb("srk","classic",3,"IC5","H2O",0.5), &
         kijdatadb("srk","classic",3,"IC5","CO2",0.129700005), &
         kijdatadb("srk","classic",3,"NC5","NC6",3.93E-04), &
         kijdatadb("srk","classic",3,"NC5","NC7",1.37E-03), &
         kijdatadb("srk","classic",3,"NC5","NC8",2.76E-03), &
         kijdatadb("srk","classic",3,"NC5","H2S",7.09E-02), &
         kijdatadb("srk","classic",3,"NC5","H2O",0.5), &
         kijdatadb("srk","classic",3,"NC5","CO2",0.1347), &
         kijdatadb("srk","classic",3,"NC6","NC7",2.97E-04), &
         kijdatadb("srk","classic",3,"NC6","NC8",1.07E-03), &
         kijdatadb("srk","classic",3,"NC6","H2S",5.70E-02), &
         kijdatadb("srk","classic",3,"NC6","H2O",0.510900021), &
         kijdatadb("srk","classic",3,"NC6","CO2",0.142000005), &
         kijdatadb("srk","classic",3,"NC7","NC8",2.41E-04), &
         kijdatadb("srk","classic",3,"NC7","H2S",7.87E-02), &
         kijdatadb("srk","classic",3,"NC7","H2O",0.5), &
         kijdatadb("srk","classic",3,"NC7","CO2",0.109200001), &
         kijdatadb("srk","classic",3,"NC8","H2S",5.50E-02), &
         kijdatadb("srk","classic",3,"NC8","H2O",0.5), &
         kijdatadb("srk","classic",3,"NC8","CO2",0.135000005), &
         kijdatadb("srk","classic",3,"H2S","H2O",8.30E-02), &
         kijdatadb("srk","classic",3,"H2S","CO2",0.115000002), &
         kijdatadb("srk","classic",3,"H2O","CO2",3.92E-02), &

! From TPlib:
         kijdatadb("gdsrk","classic", 1,"CO2","C1",  0.12), &
         kijdatadb("gdsrk","classic", 1,"CO2","C2",  0.15), &
         kijdatadb("gdsrk","classic", 1,"CO2","C2_1",  0.15), &
         kijdatadb("gdsrk","classic", 1,"CO2","C3",  0.15), &
         kijdatadb("gdsrk","classic", 1,"CO2","H2O",  0.074), &
         kijdatadb("gdsrk","classic", 1,"CO2","IC4",  0.15), &
         kijdatadb("gdsrk","classic", 1,"CO2","IC5",  0.15), &
         kijdatadb("gdsrk","classic", 1,"CO2","NC10",  0.15), &
         kijdatadb("gdsrk","classic", 1,"CO2","NC11",  0.15), &
         kijdatadb("gdsrk","classic", 1,"CO2","NC4",  0.15), &
         kijdatadb("gdsrk","classic", 1,"CO2","NC5",  0.15), &
         kijdatadb("gdsrk","classic", 1,"CO2","O2",  0.116), &
         kijdatadb("gdsrk","classic", 1,"CO2","H2S",  0.106), &
         kijdatadb("gdsrk","classic", 1,"CO2","N2", -0.014), &
         kijdatadb("gdsrk","classic", 1,"CO2","AR",  0.18), &
         kijdatadb("gdsrk","classic", 1,"CO2","SO2",  0.048), &
         kijdatadb("gdsrk","classic", 1,"H2S","NC4",  0.06), &
         kijdatadb("gdsrk","classic", 1,"H2S","NC4",  0.06), &
         kijdatadb("gdsrk","classic", 1,"H2S","NC5",  0.06), &
         kijdatadb("gdsrk","classic", 1,"H2S","NC6",  0.05), &
         kijdatadb("gdsrk","classic", 1,"H2S","NC7",  0.04), &
         kijdatadb("gdsrk","classic", 1,"H2S","NC8",  0.04), &
         kijdatadb("gdsrk","classic", 1,"H2S","NC9",  0.03), &
         kijdatadb("gdsrk","classic", 1,"MEOH","CO2",  0.017), &
         kijdatadb("gdsrk","classic", 1,"N2","C1",  0.028), &
         kijdatadb("gdsrk","classic", 1,"N2","C2",  0.041), &
         kijdatadb("gdsrk","classic", 1,"N2","C2_1",  0.08), &
         kijdatadb("gdsrk","classic", 1,"N2","C3",  0.076), &
         kijdatadb("gdsrk","classic", 1,"N2","IC4",  0.094), &
         kijdatadb("gdsrk","classic", 1,"N2","IC5",  0.087), &
         kijdatadb("gdsrk","classic", 1,"N2","NC10",  0.08), &
         kijdatadb("gdsrk","classic", 1,"N2","NC11",  0.08), &
         kijdatadb("gdsrk","classic", 1,"N2","NC4",  0.07), &
         kijdatadb("gdsrk","classic", 1,"N2","NC5",  0.088), &
         kijdatadb("gdsrk","classic", 1,"N2","NC6",  0.15), &
         kijdatadb("gdsrk","classic", 1,"N2","NC7",  0.142), &
         kijdatadb("gdsrk","classic", 1,"N2","NC8",  0.08), &
         kijdatadb("gdsrk","classic", 1,"N2","NC9",  0.08), &
         kijdatadb("gdsrk","classic", 1,"N2","O2", -0.01), &
         kijdatadb("gdsrk","classic", 1,"R12","R11",  0.0054), &
         kijdatadb("gdsrk","classic", 1,"R12","R114",  0.0015), &
         kijdatadb("gdsrk","classic", 1,"R12","R152a",  0.0867), &
         kijdatadb("gdsrk","classic", 1,"R13","R11",  0.0262), &
         kijdatadb("gdsrk","classic", 1,"R13","R12",  0.0299), &
         kijdatadb("gdsrk","classic", 1,"R14","R13",  0.0304), &
         kijdatadb("gdsrk","classic", 1,"R14","R23",  0.1008), &
         kijdatadb("gdsrk","classic", 1,"R22","R11",  0.0466), &
         kijdatadb("gdsrk","classic", 1,"R22","R114",  0.0399), &
         kijdatadb("gdsrk","classic", 1,"R22","R12",  0.0564), &
         kijdatadb("gdsrk","classic", 1,"R22","R142b",  0.0057), &
         kijdatadb("gdsrk","classic", 1,"R22","R115",  0.089), &
         kijdatadb("gdsrk","classic", 1,"R23","R13",  0.1032), &
         kijdatadb("gdsrk","classic", 1,"R218","R152a",  0.12), &
         kijdatadb("gdsrk","classic", 1,"R125","R143a", -0.0111), &
         kijdatadb("gdsrk","classic", 1,"R125","R134a", -0.0024), &
         kijdatadb("gdsrk","classic", 1,"R143a","R134a",  0.0013), &
         kijdatadb("pr","classic", 1,"C1","H2S",  0.093), &
         kijdatadb("pr","classic", 1,"CO2","O2",  0.102), &
         kijdatadb("pr","classic", 1,"CO2","NH3",  0.), &
         kijdatadb("pr","classic", 1,"CO2","N2O",  0.007), &
         kijdatadb("pr","classic", 1,"CO2","N2O4",  0.), &
         kijdatadb("pr","classic", 1,"CO2","C1",  0.092), &
         kijdatadb("pr","classic", 1,"CO2","C2",  0.15), &
         kijdatadb("pr","classic", 1,"CO2","C2_1",  0.15), &
         kijdatadb("pr","classic", 1,"CO2","C3",  0.15), &
         kijdatadb("pr","classic", 1,"CO2","H2O",  0.074), &
         kijdatadb("pr","classic", 1,"CO2","IC4",  0.15), &
         kijdatadb("pr","classic", 1,"CO2","IC5",  0.15), &
         kijdatadb("pr","classic", 1,"CO2","NC10",  0.15), &
         kijdatadb("pr","classic", 1,"CO2","NC11",  0.15), &
         kijdatadb("pr","classic", 1,"CO2","NC4",  0.15), &
         kijdatadb("pr","classic", 1,"CO2","NC5",  0.15), &
         kijdatadb("pr","classic", 1,"CO2","H2S",  0.099), &
         kijdatadb("pr","classic", 1,"CO2","N2", -0.036), &
         kijdatadb("pr","classic", 1,"CO2","AR",  0.086), &
         kijdatadb("pr","classic", 1,"CO2","SO2",  0.072), &
         kijdatadb("pr","classic", 1,"CO2","H2",  0.104), &
         kijdatadb("pr","classic", 1,"CO2","CO", -0.066), &
         kijdatadb("pr","classic", 1,"CO2","NH3",  0.), &
         kijdatadb("pr","classic", 1,"H2O","CO2",  -0.065), &
         kijdatadb("pr","classic", 1,"H2S","NC4",  0.06), &
         kijdatadb("pr","classic", 1,"H2S","NC4",  0.06), &
         kijdatadb("pr","classic", 1,"H2S","NC5",  0.06), &
         kijdatadb("pr","classic", 1,"H2S","NC6",  0.05), &
         kijdatadb("pr","classic", 1,"H2S","NC7",  0.04), &
         kijdatadb("pr","classic", 1,"H2S","NC8",  0.04), &
         kijdatadb("pr","classic", 1,"H2S","NC9",  0.03), &
         kijdatadb("pr","classic", 1,"MEOH","CO2",  0.017), &
         kijdatadb("pr","classic", 1,"N2","C1",  0.035), &
         kijdatadb("pr","classic", 1,"N2","C2",  0.041), &
         kijdatadb("pr","classic", 1,"N2","C2_1",  0.08), &
         kijdatadb("pr","classic", 1,"N2","C3",  0.076), &
         kijdatadb("pr","classic", 1,"N2","IC4",  0.094), &
         kijdatadb("pr","classic", 1,"N2","IC5",  0.087), &
         kijdatadb("pr","classic", 1,"N2","NC10",  0.08), &
         kijdatadb("pr","classic", 1,"N2","NC11",  0.08), &
         kijdatadb("pr","classic", 1,"N2","NC4",  0.07), &
         kijdatadb("pr","classic", 1,"N2","NC5",  0.088), &
         kijdatadb("pr","classic", 1,"N2","NC6",  0.15), &
         kijdatadb("pr","classic", 1,"N2","NC7",  0.142), &
         kijdatadb("pr","classic", 1,"N2","NC8",  0.08), &
         kijdatadb("pr","classic", 1,"N2","NC9",  0.08), &
         kijdatadb("pr","classic", 1,"N2","O2", -0.014), &
         kijdatadb("pr","classic", 1,"R12","R11",  0.0054), &
         kijdatadb("pr","classic", 1,"R12","R114",  0.0015), &
         kijdatadb("pr","classic", 1,"R12","R152a",  0.0867), &
         kijdatadb("pr","classic", 1,"R13","R11",  0.0262), &
         kijdatadb("pr","classic", 1,"R13","R12",  0.0299), &
         kijdatadb("pr","classic", 1,"R14","R13",  0.0304), &
         kijdatadb("pr","classic", 1,"R14","R23",  0.1008), &
         kijdatadb("pr","classic", 1,"R22","R11",  0.0466), &
         kijdatadb("pr","classic", 1,"R22","R114",  0.0399), &
         kijdatadb("pr","classic", 1,"R22","R12",  0.0564), &
         kijdatadb("pr","classic", 1,"R22","R142b",  0.0057), &
         kijdatadb("pr","classic", 1,"R22","R115",  0.087), &
         kijdatadb("pr","classic", 1,"R23","R13",  0.1032), &
         kijdatadb("pr","classic", 1,"R218","R152a",  0.12), &
         kijdatadb("pr","classic", 1,"R125","R143a", -0.0111), &
         kijdatadb("pr","classic", 1,"R125","R134a", -0.0024), &
         kijdatadb("pr","classic", 1,"R143a","R134a",  0.0013), &
! Some HFO-refrigerants R1234yf 
         kijdatadb("pr","classic", 1,"R1234yf","R32",  0.037), &
         kijdatadb("pr","classic", 1,"R1234yf","R125", 0.004), &
         kijdatadb("pr","classic", 1,"R1234yf","R134a",0.02), &
         kijdatadb("pr","classic", 1,"R1234yf","CO2", 0.020), &
         
! -- Set # 2: SIMSCI TDM 2.0 - Pro/II v9.1 Database - undefined valued '?' assigned to 0.0
         kijdatadb("pr","Classic",2,"C1","C3",0.014), &
         kijdatadb("pr","Classic",2,"C1","NC6",0.0422), &
         kijdatadb("pr","Classic",2,"C1","C2",-0.0026), &
         kijdatadb("pr","Classic",2,"C1","CO2",0.0919), &
         kijdatadb("pr","Classic",2,"C1","H2S",0.085), &
         kijdatadb("pr","Classic",2,"C1","H2O",0.5), &
         kijdatadb("pr","Classic",2,"C1","NC4",0.0133), &
         kijdatadb("pr","Classic",2,"C1","IC4",0.0256), &
         kijdatadb("pr","Classic",2,"C1","NC5",0.023), &
         kijdatadb("pr","Classic",2,"C1","NC7",0.0352), &
         kijdatadb("pr","Classic",2,"C1","NC8",0.0496), &
         kijdatadb("pr","Classic",2,"C1","IC5",-0.0056), &
         kijdatadb("pr","Classic",2,"C3","NC6",0.0007), &
         kijdatadb("pr","Classic",2,"C3","C2",0.0011), &
         kijdatadb("pr","Classic",2,"C3","CO2",0.1241), &
         kijdatadb("pr","Classic",2,"C3","H2S",0.075), &
         kijdatadb("pr","Classic",2,"C3","H2O",0.48), &
         kijdatadb("pr","Classic",2,"C3","NC4",0.0033), &
         kijdatadb("pr","Classic",2,"C3","IC4",-0.0078), &
         kijdatadb("pr","Classic",2,"C3","NC5",0.0267), &
         kijdatadb("pr","Classic",2,"C3","NC7",0.0056), &
         kijdatadb("pr","Classic",2,"C3","NC8",0.0), &
         kijdatadb("pr","Classic",2,"C3","IC5",0.0111), &
         kijdatadb("pr","Classic",2,"NC6","C2",-0.01), &
         kijdatadb("pr","Classic",2,"NC6","CO2",0.11), &
         kijdatadb("pr","Classic",2,"NC6","H2S",0.06), &
         kijdatadb("pr","Classic",2,"NC6","H2O",0.48), &
         kijdatadb("pr","Classic",2,"NC6","NC4",-0.0056), &
         kijdatadb("pr","Classic",2,"NC6","IC4",0.0), &
         kijdatadb("pr","Classic",2,"NC6","NC5",0.0), &
         kijdatadb("pr","Classic",2,"NC6","NC7",-0.0078), &
         kijdatadb("pr","Classic",2,"NC6","NC8",0.0), &
         kijdatadb("pr","Classic",2,"NC6","IC5",0.0), &
         kijdatadb("pr","Classic",2,"C2","CO2",0.1322), &
         kijdatadb("pr","Classic",2,"C2","H2S",0.085), &
         kijdatadb("pr","Classic",2,"C2","H2O",0.5), &
         kijdatadb("pr","Classic",2,"C2","NC4",0.0096), &
         kijdatadb("pr","Classic",2,"C2","IC4",-0.0067), &
         kijdatadb("pr","Classic",2,"C2","NC5",0.0078), &
         kijdatadb("pr","Classic",2,"C2","NC7",0.0074), &
         kijdatadb("pr","Classic",2,"C2","NC8",0.0185), &
         kijdatadb("pr","Classic",2,"C2","IC5",0.0), &
         kijdatadb("pr","Classic",2,"CO2","H2S",0.1), &
         kijdatadb("pr","Classic",2,"CO2","H2O",0.21), &
         kijdatadb("pr","Classic",2,"CO2","NC4",0.1333), &
         kijdatadb("pr","Classic",2,"CO2","IC4",0.12), &
         kijdatadb("pr","Classic",2,"CO2","NC5",0.122), &
         kijdatadb("pr","Classic",2,"CO2","NC7",0.1), &
         kijdatadb("pr","Classic",2,"CO2","NC8",0.107), &
         kijdatadb("pr","Classic",2,"CO2","IC5",0.122), &
         kijdatadb("pr","Classic",2,"H2S","H2O",0.164), &
         kijdatadb("pr","Classic",2,"H2S","NC4",0.06), &
         kijdatadb("pr","Classic",2,"H2S","IC4",0.06), &
         kijdatadb("pr","Classic",2,"H2S","NC5",0.06), &
         kijdatadb("pr","Classic",2,"H2S","NC7",0.06), &
         kijdatadb("pr","Classic",2,"H2S","NC8",0.05), &
         kijdatadb("pr","Classic",2,"H2S","IC5",0.06), &
         kijdatadb("pr","Classic",2,"H2O","NC4",0.48), &
         kijdatadb("pr","Classic",2,"H2O","IC4",0.48), &
         kijdatadb("pr","Classic",2,"H2O","NC5",0.48), &
         kijdatadb("pr","Classic",2,"H2O","NC7",0.48), &
         kijdatadb("pr","Classic",2,"H2O","NC8",0.48), &
         kijdatadb("pr","Classic",2,"H2O","IC5",0.48), &
         kijdatadb("pr","Classic",2,"NC4","IC4",-0.0004), &
         kijdatadb("pr","Classic",2,"NC4","NC5",0.0174), &
         kijdatadb("pr","Classic",2,"NC4","NC7",0.0033), &
         kijdatadb("pr","Classic",2,"NC4","NC8",0.0), &
         kijdatadb("pr","Classic",2,"NC4","IC5",0.0), &
         kijdatadb("pr","Classic",2,"IC4","NC5",0.0), &
         kijdatadb("pr","Classic",2,"IC4","NC7",0.0), &
         kijdatadb("pr","Classic",2,"IC4","NC8",0.0), &
         kijdatadb("pr","Classic",2,"IC4","IC5",0.0), &
         kijdatadb("pr","Classic",2,"NC4","NC7",0.0074), &
         kijdatadb("pr","Classic",2,"NC4","NC8",0), &
         kijdatadb("pr","Classic",2,"NC4","IC5",0.0), &
         kijdatadb("pr","Classic",2,"NC7","NC8",0.0), &
         kijdatadb("pr","Classic",2,"NC7","IC5",0.0), &
         kijdatadb("pr","Classic",2,"NC8","IC5",0.0), &

! -- Set # 3: Aspen HYSYS  
         kijdatadb("pr","classic",3,"C1","C2",0.002241387), &
         kijdatadb("pr","classic",3,"C1","C3",0.006828805), &
         kijdatadb("pr","classic",3,"C1","IC4",0.013113427), &
         kijdatadb("pr","classic",3,"C1","NC4",0.012304616), &
         kijdatadb("pr","classic",3,"C1","IC5",0.017627502), &
         kijdatadb("pr","classic",3,"C1","NC5",0.01792537), &
         kijdatadb("pr","classic",3,"C1","NC6",0.023474067), &
         kijdatadb("pr","classic",3,"C1","NC7",0.028864292), &
         kijdatadb("pr","classic",3,"C1","NC8",0.034159049), &
         kijdatadb("pr","classic",3,"C1","H2O",0.5), &
         kijdatadb("pr","classic",3,"C1","H2S",0.085000001), &
         kijdatadb("pr","classic",3,"C1","CO2",0.100000001), &
         kijdatadb("pr","classic",3,"C2","C3",0.00125796), &
         kijdatadb("pr","classic",3,"C2","IC4",0.004573491), &
         kijdatadb("pr","classic",3,"C2","NC4",0.004096392), &
         kijdatadb("pr","classic",3,"C2","IC5",0.007413303), &
         kijdatadb("pr","classic",3,"C2","NC5",0.007609447), &
         kijdatadb("pr","classic",3,"C2","NC6",0.011413809), &
         kijdatadb("pr","classic",3,"C2","NC7",0.015324294), &
         kijdatadb("pr","classic",3,"C2","NC8",0.019318692), &
         kijdatadb("pr","classic",3,"C2","H2O",0.5), &
         kijdatadb("pr","classic",3,"C2","H2S",0.083999), &
         kijdatadb("pr","classic",3,"C2","CO2",0.129800007), &
         kijdatadb("pr","classic",3,"C3","IC4",0.001040541), &
         kijdatadb("pr","classic",3,"C3","NC4",0.000818964), &
         kijdatadb("pr","classic",3,"C3","IC5",0.002583363), &
         kijdatadb("pr","classic",3,"C3","NC5",0.002700525), &
         kijdatadb("pr","classic",3,"C3","NC6",0.005141959), &
         kijdatadb("pr","classic",3,"C3","NC7",0.007887419), &
         kijdatadb("pr","classic",3,"C3","NC8",0.010850275), &
         kijdatadb("pr","classic",3,"C3","H2O",0.5), &
         kijdatadb("pr","classic",3,"C3","H2S",0.075000003), &
         kijdatadb("pr","classic",3,"C3","CO2",0.135000005), &
         kijdatadb("pr","classic",3,"IC4","NC4",1.3372E-05), &
         kijdatadb("pr","classic",3,"IC4","IC5",0.000346209), &
         kijdatadb("pr","classic",3,"IC4","NC5",0.000390038), &
         kijdatadb("pr","classic",3,"IC4","NC6",0.001565269), &
         kijdatadb("pr","classic",3,"IC4","NC7",0.003221282), &
         kijdatadb("pr","classic",3,"IC4","NC8",0.00521418), &
         kijdatadb("pr","classic",3,"IC4","H2O",0.5), &
         kijdatadb("pr","classic",3,"IC4","H2S",0.050000001), &
         kijdatadb("pr","classic",3,"IC4","CO2",0.129800007), &
         kijdatadb("pr","classic",3,"NC4","IC5",0.000495155), &
         kijdatadb("pr","classic",3,"NC4","NC5",0.000547225), &
         kijdatadb("pr","classic",3,"NC4","NC6",0.001866333), &
         kijdatadb("pr","classic",3,"NC4","NC7",0.003646394), &
         kijdatadb("pr","classic",3,"NC4","NC8",0.005750192), &
         kijdatadb("pr","classic",3,"NC4","H2O",0.5), &
         kijdatadb("pr","classic",3,"NC4","H2S",0.059999), &
         kijdatadb("pr","classic",3,"NC4","CO2",0.129800007), &
         kijdatadb("pr","classic",3,"IC5","NC5",1.23366E-06), &
         kijdatadb("pr","classic",3,"IC5","NC6",0.000439948), &
         kijdatadb("pr","classic",3,"IC5","NC7",0.001459151), &
         kijdatadb("pr","classic",3,"IC5","NC8",0.002882791), &
         kijdatadb("pr","classic",3,"IC5","H2O",0.5), &
         kijdatadb("pr","classic",3,"IC5","H2S",0.059999), &
         kijdatadb("pr","classic",3,"IC5","CO2",0.125), &
         kijdatadb("pr","classic",3,"NC5","NC6",0.000393425), &
         kijdatadb("pr","classic",3,"NC5","NC7",0.001373304), &
         kijdatadb("pr","classic",3,"NC5","NC8",0.002761862), &
         kijdatadb("pr","classic",3,"NC5","H2O",0.479999989), &
         kijdatadb("pr","classic",3,"NC5","H2S",0.064998001), &
         kijdatadb("pr","classic",3,"NC5","CO2",0.125), &
         kijdatadb("pr","classic",3,"NC6","NC7",0.000297245), &
         kijdatadb("pr","classic",3,"NC6","NC8",0.001073321), &
         kijdatadb("pr","classic",3,"NC6","H2O",0.5), &
         kijdatadb("pr","classic",3,"NC6","H2S",0.059999), &
         kijdatadb("pr","classic",3,"NC6","CO2",0.125), &
         kijdatadb("pr","classic",3,"NC7","NC8",0.000241262), &
         kijdatadb("pr","classic",3,"NC7","H2O",0.5), &
         kijdatadb("pr","classic",3,"NC7","H2S",0.059999), &
         kijdatadb("pr","classic",3,"NC7","CO2",0.119900003), &
         kijdatadb("pr","classic",3,"NC8","H2O",0.5), &
         kijdatadb("pr","classic",3,"NC8","H2S",0.054999001), &
         kijdatadb("pr","classic",3,"NC8","CO2",0.115000002), &
         kijdatadb("pr","classic",3,"H2O","H2S",0.081900001), &
         kijdatadb("pr","classic",3,"H2O","CO2",0.044500001), &
         kijdatadb("pr","classic",3,"H2S","CO2",0.100000001), &
         kijdatadb("pr","classic",3,"CO2","BENZENE",0.0806), &
         kijdatadb("pr","classic",3,"CO2","TOLU",0.0936), &
         kijdatadb("pr","classic",3,"CO2","EBZN",0.101), &
         kijdatadb("pr","classic",3,"CO2","MXYL",0.0879), &
         kijdatadb("pr","classic",3,"CO2","OXYL",0.09), &
         kijdatadb("pr","classic",3,"CO2","NC9",0.101), &
         kijdatadb("pr","classic",3,"N2","H2S",0.1676), &
         kijdatadb("pr","classic",3,"N2","BENZENE",0.1597), &
         kijdatadb("pr","classic",3,"N2","TOLU",0.1932), &
         kijdatadb("pr","classic",3,"N2","EBZN",0.1), &
         kijdatadb("pr","classic",3,"N2","MXYL",0.2169), &
         kijdatadb("pr","classic",3,"N2","OXYL",0.214), &
         kijdatadb("pr","classic",3,"N2","H2O",-0.3156), &
!++ Added 2019-08-22 (GS)
         kijdatadb("pr","classic", 3,"N2","C1",  0.036), &
         kijdatadb("pr","classic", 3,"N2","C2",  0.05), &
         kijdatadb("pr","classic", 3,"N2","C2_1",  0.0722), &
         kijdatadb("pr","classic", 3,"N2","C3",  0.080), &
         kijdatadb("pr","classic", 3,"N2","C3_1",  0.0673), &
         kijdatadb("pr","classic", 3,"N2","IC5",  0.095), &
         kijdatadb("pr","classic", 3,"N2","IC4",  0.095), &
         kijdatadb("pr","classic", 3,"N2","IC5",  0.095), &
         kijdatadb("pr","classic", 3,"N2","NC4",  0.09), &
         kijdatadb("pr","classic", 3,"N2","NC5",  0.10), &
         kijdatadb("pr","classic", 3,"N2","NC6",  0.15), &
         kijdatadb("pr","classic", 3,"N2","NC7",  0.142), &
         kijdatadb("pr","classic", 3,"N2","NC8",  0.08), &
         kijdatadb("pr","classic", 3,"N2","NC9",  0.08), &
         kijdatadb("pr","classic", 3,"N2","NC10",  0.08), &
         kijdatadb("pr","classic", 3,"N2","NC11",  0.08), &
         kijdatadb("pr","classic", 3,"N2","O2", -0.014), &
!--         

         kijdatadb("pr","classic",3,"H2S","BENZENE",0.009), &
         kijdatadb("pr","classic",3,"H2S","TOLU",0.0081), &
         kijdatadb("pr","classic",3,"H2S","EBZN",0.045), &
         kijdatadb("pr","classic",3,"H2S","MXYL",0.0171), &
         kijdatadb("pr","classic",3,"H2S","OXYL",-0.0231), &
         kijdatadb("pr","classic",3,"H2S","NC10",0.045), &
         kijdatadb("pr","classic",3,"H2S","NC11",0.045), &
         kijdatadb("pr","classic",3,"C1","BENZENE",0.04), &
         kijdatadb("pr","classic",3,"C1","TOLU",0.0649), &
         kijdatadb("pr","classic",3,"C1","EBZN",0.02404), &
         kijdatadb("pr","classic",3,"C1","MXYL",0.0491), &
         kijdatadb("pr","classic",3,"C1","OXYL",0.05), &
         kijdatadb("pr","classic",3,"C1","NC9",0.03893), &
         kijdatadb("pr","classic",3,"C1","NC10",0.04361), &
         kijdatadb("pr","classic",3,"C1","NC11",0.04799), &

         kijdatadb("pr","classic",3,"C2","IC5",0.00741), &
         kijdatadb("pr","classic",3,"C2","BENZENE",0.02), &
         kijdatadb("pr","classic",3,"C2","TOLU",0.0344), &
         kijdatadb("pr","classic",3,"C2","EBZN",0.01182), &
         kijdatadb("pr","classic",3,"C2","MXYL",0.0295), &
         kijdatadb("pr","classic",3,"C2","OXYL",0.033), &
         kijdatadb("pr","classic",3,"C2","NC9",0.02302), &
         kijdatadb("pr","classic",3,"C2","NC10",0.02673), &
         kijdatadb("pr","classic",3,"C2","NC11",0.03026), &
         kijdatadb("pr","classic",3,"C3","BENZENE",0.02), &
         kijdatadb("pr","classic",3,"C3","TOLU",0.031), &
         kijdatadb("pr","classic",3,"C3","EBZN",0.00542), &
         kijdatadb("pr","classic",3,"C3","MXYL",0.03), &
         kijdatadb("pr","classic",3,"C3","OXYL",0.03), &
         kijdatadb("pr","classic",3,"C3","NC8",0.01085), &
         kijdatadb("pr","classic",3,"C3","NC9",0.0137), &
         kijdatadb("pr","classic",3,"C3","NC10",0.01663), &
         kijdatadb("pr","classic",3,"C3","NC11",0.01948), &
         kijdatadb("pr","classic",3,"IC4","IC5",0.00035), &
         kijdatadb("pr","classic",3,"IC4","NC5",0.00039), &
         kijdatadb("pr","classic",3,"IC4","BENZENE",0.0000018), &
         kijdatadb("pr","classic",3,"IC4","TOLU",0.00047), &
         kijdatadb("pr","classic",3,"IC4","EBZN",0.00172), &
         kijdatadb("pr","classic",3,"IC4","MXYL",0.00176), &
         kijdatadb("pr","classic",3,"IC4","OXYL",0.00159), &
         kijdatadb("pr","classic",3,"IC4","NC6",0.00157), &
         kijdatadb("pr","classic",3,"IC4","NC7",0.00322), &
         kijdatadb("pr","classic",3,"IC4","NC8",0.00521), &
         kijdatadb("pr","classic",3,"IC4","NC9",0.00725), &
         kijdatadb("pr","classic",3,"IC4","NC10",0.00945), &
         kijdatadb("pr","classic",3,"IC4","NC11",0.01164), &
         kijdatadb("pr","classic",3,"NC4","IC5",0.0005), &
         kijdatadb("pr","classic",3,"NC4","NC5",0.00055), &
         kijdatadb("pr","classic",3,"NC4","BENZENE",0.00001), &
         kijdatadb("pr","classic",3,"NC4","TOLU",0.00064), &
         kijdatadb("pr","classic",3,"NC4","EBZN",0.00203), &
         kijdatadb("pr","classic",3,"NC4","MXYL",0.00208), &
         kijdatadb("pr","classic",3,"NC4","OXYL",0.0019), &
         kijdatadb("pr","classic",3,"NC4","NC6",0.00187), &
         kijdatadb("pr","classic",3,"NC4","NC7",0.00365), &
         kijdatadb("pr","classic",3,"NC4","NC8",0.00575), &
         kijdatadb("pr","classic",3,"NC4","NC9",0.00788), &
         kijdatadb("pr","classic",3,"NC4","NC10",0.01016), &
         kijdatadb("pr","classic",3,"NC4","NC11",0.01243), &
         kijdatadb("pr","classic",3,"IC5","NC5",0.0000013), &
         kijdatadb("pr","classic",3,"IC5","BENZENE",0.0004), &
         kijdatadb("pr","classic",3,"IC5","TOLU",0.00001), &
         kijdatadb("pr","classic",3,"IC5","EBZN",0.00052), &
         kijdatadb("pr","classic",3,"IC5","MXYL",0.00055), &
         kijdatadb("pr","classic",3,"IC5","OXYL",0.00046), &
         kijdatadb("pr","classic",3,"IC5","NC6",0.00044), &
         kijdatadb("pr","classic",3,"IC5","NC7",0.00146), &
         kijdatadb("pr","classic",3,"IC5","NC8",0.00288), &
         kijdatadb("pr","classic",3,"IC5","NC9",0.00445), &
         kijdatadb("pr","classic",3,"IC5","NC10",0.00621), &
         kijdatadb("pr","classic",3,"IC5","NC11",0.00801), &
         kijdatadb("pr","classic",3,"NC5","BENZENE",0.016), &
         kijdatadb("pr","classic",3,"NC5","TOLU",0.0000036), &
         kijdatadb("pr","classic",3,"NC5","EBZN",0.00047), &
         kijdatadb("pr","classic",3,"NC5","MXYL",0.0005), &
         kijdatadb("pr","classic",3,"NC5","OXYL",0.00041), &
         kijdatadb("pr","classic",3,"NC5","NC6",0.00039), &
         kijdatadb("pr","classic",3,"NC5","NC7",0.00137), &
         kijdatadb("pr","classic",3,"NC5","NC8",0.00276), &
         kijdatadb("pr","classic",3,"NC5","NC9",0.0043), &
         kijdatadb("pr","classic",3,"NC5","NC10",0.00603), &
         kijdatadb("pr","classic",3,"NC5","NC11",0.00781), &
         kijdatadb("pr","classic",3,"BENZENE","TOLU",0.00053), &
         kijdatadb("pr","classic",3,"BENZENE","EBZN",0.00183), &
         kijdatadb("pr","classic",3,"BENZENE","MXYL",0.00188), &
         kijdatadb("pr","classic",3,"BENZENE","OXYL",0.0017), &
         kijdatadb("pr","classic",3,"BENZENE","NC6",0.007), &
         kijdatadb("pr","classic",3,"BENZENE","NC7",-0.002), &
         kijdatadb("pr","classic",3,"BENZENE","NC8",0.003), &
         kijdatadb("pr","classic",3,"BENZENE","NC9",0.00749), &
         kijdatadb("pr","classic",3,"BENZENE","NC10",0.01), &
         kijdatadb("pr","classic",3,"BENZENE","NC11",0.01193), &
         kijdatadb("pr","classic",3,"BENZENE","H2O",0.5), &
         kijdatadb("pr","classic",3,"TOLU","EBZN",0.00039), &
         kijdatadb("pr","classic",3,"TOLU","MXYL",0.00042), &
         kijdatadb("pr","classic",3,"TOLU","OXYL",0.00034), &
         kijdatadb("pr","classic",3,"TOLU","NC6",0.00032), &
         kijdatadb("pr","classic",3,"TOLU","NC7",0.006), &
         kijdatadb("pr","classic",3,"TOLU","NC8",0.01), &
         kijdatadb("pr","classic",3,"TOLU","NC9",0.00406), &
         kijdatadb("pr","classic",3,"TOLU","NC10",0.01), &
         kijdatadb("pr","classic",3,"TOLU","NC11",0.00749), &
         kijdatadb("pr","classic",3,"TOLU","H2O",0.5), &
         kijdatadb("pr","classic",3,"EBZN","MXYL",0.0000002), &
         kijdatadb("pr","classic",3,"EBZN","OXYL",0.0000002), &
         kijdatadb("pr","classic",3,"EBZN","NC6",0.000004), &
         kijdatadb("pr","classic",3,"EBZN","NC7",0.00024), &
         kijdatadb("pr","classic",3,"EBZN","NC8",-0.0018), &
         kijdatadb("pr","classic",3,"EBZN","NC9",0.00193), &
         kijdatadb("pr","classic",3,"EBZN","NC10",0.00314), &
         kijdatadb("pr","classic",3,"EBZN","NC11",0.00446), &
         kijdatadb("pr","classic",3,"EBZN","H2O",0.5), &
         kijdatadb("pr","classic",3,"MXYL","OXYL",0.000004), &
         kijdatadb("pr","classic",3,"MXYL","NC6",0.00001), &
         kijdatadb("pr","classic",3,"MXYL","NC7",0.00022), &
         kijdatadb("pr","classic",3,"MXYL","NC8",0.00092), &
         kijdatadb("pr","classic",3,"MXYL","NC9",0.00188), &
         kijdatadb("pr","classic",3,"MXYL","NC10",0.00308), &
         kijdatadb("pr","classic",3,"MXYL","NC11",0.00439), &
         kijdatadb("pr","classic",3,"MXYL","H2O",0.5), &
         kijdatadb("pr","classic",3,"OXYL","NC6",0.0000002), &
         kijdatadb("pr","classic",3,"OXYL","NC7",0.00029), &
         kijdatadb("pr","classic",3,"OXYL","NC8",0.00105), &
         kijdatadb("pr","classic",3,"OXYL","NC9",0.00207), &
         kijdatadb("pr","classic",3,"OXYL","NC10",0.00331), &
         kijdatadb("pr","classic",3,"OXYL","NC11",0.00467), &
         kijdatadb("pr","classic",3,"OXYL","H2O",0.5), &
         kijdatadb("pr","classic",3,"NC6","NC7",0.0003), &
         kijdatadb("pr","classic",3,"NC6","NC8",0.00107), &
         kijdatadb("pr","classic",3,"NC6","NC9",0.0021), &
         kijdatadb("pr","classic",3,"NC6","NC10",0.00335), &
         kijdatadb("pr","classic",3,"NC6","NC11",0.00472), &
         kijdatadb("pr","classic",3,"NC6","H2O",0.5), &
         kijdatadb("pr","classic",3,"NC7","NC8",0.00024), &
         kijdatadb("pr","classic",3,"NC7","NC9",0.00082), &
         kijdatadb("pr","classic",3,"NC7","NC10",0.00166), &
         kijdatadb("pr","classic",3,"NC7","NC11",0.00266), &
         kijdatadb("pr","classic",3,"NC7","H2O",0.5), &
         kijdatadb("pr","classic",3,"NC8","NC9",0.00017), &
         kijdatadb("pr","classic",3,"NC8","NC10",0.00064), &
         kijdatadb("pr","classic",3,"NC8","NC11",0.0013), &
         kijdatadb("pr","classic",3,"NC8","H2O",0.5), &
         kijdatadb("pr","classic",3,"NC9","NC10",0.0013), &
         kijdatadb("pr","classic",3,"NC9","NC11",0.00053), &
         kijdatadb("pr","classic",3,"NC9","H2O",0.5), &
         kijdatadb("pr","classic",3,"NC10","NC11",0.00012), &
         kijdatadb("pr","classic",3,"NC10","H2O",0.5), &
         kijdatadb("pr","classic",3,"NC11","H2O",0.5), &

! -- testing ...         
         kijdatadb("srk","reid", 1,"C1","CO2", -0.0078), &
         kijdatadb("srk","reid", 1,"CO2","C1",  0.02), &
         kijdatadb("srk","reid", 1,"C1","N2",  0.009), &
         kijdatadb("srk","reid", 1,"N2","C1",  0.0241), &
         kijdatadb("srk","reid", 1,"CO2","N2",  0.0056), &
         kijdatadb("srk","reid", 1,"N2","CO2",  0.019), &
         kijdatadb("srk","classic", 2,"C1","CO2", 0.01), &
         kijdatadb("srk","classic", 2,"C1","N2", 0.02), &
         kijdatadb("srk","classic", 2,"CO2","N2",  0.03), &
         kijdatadb("sw","classic", 1,"C1","CO2", 0.01), &
         kijdatadb("sw","classic", 1,"C1","N2", 0.02), &
         kijdatadb("sw","classic", 1,"CO2","N2",  0.03), &
! -- just for testing
         kijdatadb("vdw","classic", 1,"C1","CO2", 0.01), &
         kijdatadb("vdw","classic", 1,"C1","N2", 0.02), &
         kijdatadb("vdw","classic", 1,"CO2","N2",  0.03), &
         kijdatadb("rk","classic", 1,"C1","CO2", 0.01), &
         kijdatadb("rk","classic", 1,"C1","N2", 0.02), &
         kijdatadb("rk","classic", 1,"CO2","N2",  0.03), &

! --- copied from SRK for testing
         kijdatadb("rk","classic", 1,"C1","C2", -0.0078), &
         kijdatadb("rk","classic", 1,"C1","C2_1",  0.02), &
         kijdatadb("rk","classic", 1,"C1","C3",  0.009), &
         kijdatadb("rk","classic", 1,"C1","IC4",  0.0241), &
         kijdatadb("rk","classic", 1,"C1","NC4",  0.0056), &
         kijdatadb("rk","classic", 1,"C1","NC5",  0.019), &
         kijdatadb("rk","classic", 1,"C1","H2S",  0.091), &
         kijdatadb("rk","classic", 1,"C2","C2_1",  0.0112), &
         kijdatadb("rk","classic", 1,"C2","C3", -0.0022), &
         kijdatadb("rk","classic", 1,"C2","IC4", -0.01), &
         kijdatadb("rk","classic", 1,"C2","NC4",  0.0067), &
         kijdatadb("rk","classic", 1,"C2","NC5",  0.0056), &
         kijdatadb("rk","classic", 1,"NC4","C2_1",  0.1), &
         kijdatadb("rk","classic", 1,"C3","IC4", -0.01), &
         kijdatadb("rk","classic", 1,"C3","NC5",  0.023), &
         kijdatadb("rk","classic", 1,"IC4","NC4",  0.0011), &
         kijdatadb("rk","classic", 1,"NC4","NC5",  0.0204), &
         kijdatadb("rk","classic", 1,"H2O","C1",  0.), &
         kijdatadb("rk","classic", 1,"CO2","O2",  0.104), &
         kijdatadb("rk","classic", 1,"CO2","NH3",  0.), &
         kijdatadb("rk","classic", 1,"CO2","N2O",  0.004), &
         kijdatadb("rk","classic", 1,"CO2","N2O4",  0.), &
         kijdatadb("rk","classic", 1,"CO2","CO", -0.08), &
         kijdatadb("rk","classic", 1,"CO2","C1",  0.095), &
         kijdatadb("rk","classic", 1,"CO2","C2",  0.15), &
         kijdatadb("rk","classic", 1,"CO2","C2_1",  0.15), &
         kijdatadb("rk","classic", 1,"CO2","C3",  0.15), &
         kijdatadb("rk","classic", 1,"CO2","H2O",  0.074), &
         kijdatadb("rk","classic", 1,"CO2","IC4",  0.15), &
         kijdatadb("rk","classic", 1,"CO2","IC5",  0.15), &
         kijdatadb("rk","classic", 1,"CO2","NC10",  0.15), &
         kijdatadb("rk","classic", 1,"CO2","NC11",  0.15), &
         kijdatadb("rk","classic", 1,"CO2","NC4",  0.15), &
         kijdatadb("rk","classic", 1,"CO2","NC5",  0.15), &
         kijdatadb("rk","classic", 1,"CO2","H2S",  0.1), &
         kijdatadb("rk","classic", 1,"CO2","N2", -0.051), &
         kijdatadb("rk","classic", 1,"CO2","AR",  0.088), &
         kijdatadb("rk","classic", 1,"CO2","SO2",  0.071), &
         kijdatadb("rk","classic", 1,"CO2","H2",  0.009), &
         kijdatadb("rk","classic", 1,"H2S","NC4",  0.06), &
         kijdatadb("rk","classic", 1,"H2S","NC5",  0.06), &
         kijdatadb("rk","classic", 1,"H2S","NC6",  0.05), &
         kijdatadb("rk","classic", 1,"H2S","NC7",  0.04), &
         kijdatadb("rk","classic", 1,"H2S","NC8",  0.04), &
         kijdatadb("rk","classic", 1,"H2S","NC9",  0.03), &
         kijdatadb("rk","classic", 1,"MEOH","CO2",  0.017), &
         kijdatadb("rk","classic", 1,"N2","C1",  0.034), &
         kijdatadb("rk","classic", 1,"N2","C2",  0.06), &
         kijdatadb("rk","classic", 1,"N2","C2_1",  0.075), &
         kijdatadb("rk","classic", 1,"N2","C3",  0.09), &
         kijdatadb("rk","classic", 1,"N2","IC4",  0.113), &
         kijdatadb("rk","classic", 1,"N2","IC5",  0.087), &
         kijdatadb("rk","classic", 1,"N2","NC10",  0.08), &
         kijdatadb("rk","classic", 1,"N2","NC11",  0.08), &
         kijdatadb("rk","classic", 1,"N2","NC4",  0.113), &
         kijdatadb("rk","classic", 1,"N2","NC5",  0.14), &
         kijdatadb("rk","classic", 1,"N2","NC6",  0.15), &
         kijdatadb("rk","classic", 1,"N2","NC7",  0.142), &
         kijdatadb("rk","classic", 1,"N2","NC8",  0.08), &
         kijdatadb("rk","classic", 1,"N2","NC9",  0.08), &
         kijdatadb("rk","classic", 1,"N2","O2", -0.011), &
         kijdatadb("rk","classic", 1,"R12","R11",  0.0054), &
         kijdatadb("rk","classic", 1,"R12","R114",  0.0015), &
         kijdatadb("rk","classic", 1,"R12","R152a",  0.0867), &
         kijdatadb("rk","classic", 1,"R13","R11",  0.0262), &
         kijdatadb("rk","classic", 1,"R13","R12",  0.0299), &
         kijdatadb("rk","classic", 1,"R14","R13",  0.0304), &
         kijdatadb("rk","classic", 1,"R14","R23",  0.1008), &
         kijdatadb("rk","classic", 1,"R22","R11",  0.0466), &
         kijdatadb("rk","classic", 1,"R22","R114",  0.0399), &
         kijdatadb("rk","classic", 1,"R22","R12",  0.0564), &
         kijdatadb("rk","classic", 1,"R22","R142b",  0.0057), &
         kijdatadb("rk","classic", 1,"R22","R115",  0.089), &
         kijdatadb("rk","classic", 1,"R23","R13",  0.1032), &
         kijdatadb("rk","classic", 1,"R218","R152a",  0.12), &
         kijdatadb("rk","classic", 1,"R125","R143a", -0.0111), &
         kijdatadb("rk","classic", 1,"R125","R134a", -0.0024), &
         kijdatadb("rk","classic", 1,"R143a","R134a",  0.0013), &
         
! --- Default values for Patel-Teja from table 5 in 
! --- "A new cubic equation of state for fluids and fluid mixtures"
! --- Navin C. Patel and Amyn S. Teja. Obs: kij = 1 - ksi_ij
! --- Chemical Engineering Science Vol 37 no 3, 1982         
         kijdatadb("pt","classic", 1,"C1","C2",1.0-0.995), &
         kijdatadb("pt","classic", 1,"C1","IC4",1.0-1.008),&
         kijdatadb("pt","classic", 1,"C1","NC4",1.0-0.985),&
         kijdatadb("pt","classic", 1,"C1","NC5",1.0-0.98),&
         kijdatadb("pt","classic", 1,"C1","NC6",1.0- 0.990),&
         kijdatadb("pt","classic", 1,"C2","C3",  1.0-0.994),&
         kijdatadb("pt","classic", 1,"C2","PRLN",1.0- 0.996),&
         kijdatadb("pt","classic", 1,"C2","NC4",1.0-  0.999),&
         kijdatadb("pt","classic", 1,"C2","NC5",1.0-  1.001),&
         kijdatadb("pt","classic", 1,"C2","NC7",1.0-  1.015),&
         kijdatadb("pt","classic", 1,"C2_1","C1",1.0- 0.974),&
         kijdatadb("pt","classic", 1,"C2_1","C2",1.0- 0.982),&
         kijdatadb("pt","classic", 1,"C2_1","C3",1.0- 0.981),&
         kijdatadb("pt","classic", 1,"C2_1","NC4",1.0-0.938),&
         kijdatadb("pt","classic", 1,"C3","NC4",1.0-  0.987),&
         kijdatadb("pt","classic", 1,"C3","NC5",1.0-  0.987),&
         kijdatadb("pt","classic", 1,"C3","IC5",1.0-  0.979),&
         kijdatadb("pt","classic", 1,"PRLN","C3",1.0- 0.970),&
!      kijdatadb("pt","classic", 1,"PRLN","IC4_1",1.0-1.002),&
         kijdatadb("pt","classic", 1,"NC4","NC10",1.0-0.995),&
         kijdatadb("pt","classic", 1,"IC4","NC4",1.0- 1.003),&
         kijdatadb("pt","classic", 1,"CO2","C1",1.0-  0.907),&
         kijdatadb("pt","classic", 1,"CO2","C2",1.0-  0.872),&
         kijdatadb("pt","classic", 1,"CO2","C2_1",1.0-0.943),&
         kijdatadb("pt","classic", 1,"CO2","C3",1.0-  0.869),&
         kijdatadb("pt","classic", 1,"CO2","NC4",1.0- 0.891),&
         kijdatadb("pt","classic", 1,"CO2","IC4",1.0- 0.873),&
         kijdatadb("pt","classic", 1,"CO2","NC5",1.0- 0.865),&
         kijdatadb("pt","classic", 1,"H2S","C1",1.0-  0.920),&
         kijdatadb("pt","classic", 1,"H2S","C2",1.0-  0.911),&
         kijdatadb("pt","classic", 1,"H2S","IC4",1.0- 0.954),&
         kijdatadb("pt","classic", 1,"H2S","NC7",1.0- 0.947),&
! -- 213 values kij, retrieved from TPlib for the SPUNG EOS, 
! -- One set each for SPUNG/SRK, SPUNG/SRKGB and SPUNG/PR
! -- 
         kijdatadb("csp-srk","classic",1,"C1", "H2S", 0.101), &
         kijdatadb("csp-srk","classic",1,"C1", "C2",  -0.00780), &
         kijdatadb("csp-srk","classic",1,"C1", "C2_1",  0.020), &
         kijdatadb("csp-srk","classic",1,"C1", "C3",     0.0090), &
         kijdatadb("csp-srk","classic",1,"C1", "IC4",    0.02410), &
         kijdatadb("csp-srk","classic",1,"C1", "NC4",    0.00560), &
         kijdatadb("csp-srk","classic",1,"C1", "NC5",    0.0190), &
         kijdatadb("csp-srk","classic",1,"C1", "H2S",    0.0000), &
         kijdatadb("csp-srk","classic",1,"C2", "C2_1",   0.0112), &
         kijdatadb("csp-srk","classic",1,"C2", "C3",     -0.0022), &
         kijdatadb("csp-srk","classic",1,"C2", "IC4",    -0.010), &
         kijdatadb("csp-srk","classic",1,"C2", "NC4",    0.00670), &
         kijdatadb("csp-srk","classic",1,"C2", "NC5",    0.0056), &
         kijdatadb("csp-srk","classic",1,"NC4", "C2_1",   0.100), &
         kijdatadb("csp-srk","classic",1,"C3", "IC4",    -0.010), &
         kijdatadb("csp-srk","classic",1,"C3", "NC5",    0.0230), &
         kijdatadb("csp-srk","classic",1,"IC4", "NC4",    0.00110), &
         kijdatadb("csp-srk","classic",1,"NC4", "NC5",    0.0204), &
         kijdatadb("csp-srk","classic",1,"H2O", "C1",     0.00), &
         kijdatadb("csp-srk","classic",1,"CO2", "O2", 0.118), &
         kijdatadb("csp-srk","classic",1,"CO2", "NH3", 0.000), &
         kijdatadb("csp-srk","classic",1,"CO2", "N2O", 0.010), &
         kijdatadb("csp-srk","classic",1,"CO2", "N2O4", 0.00), &
         kijdatadb("csp-srk","classic",1,"CO2", "CO", -0.068), &
         kijdatadb("csp-srk","classic",1,"CO2", "C1",     0.1060), &
         kijdatadb("csp-srk","classic",1,"CO2", "C2",     0.15), &
         kijdatadb("csp-srk","classic",1,"CO2", "C2_1",   0.15), &
         kijdatadb("csp-srk","classic",1,"CO2", "C3",     0.15), &
         kijdatadb("csp-srk","classic",1,"CO2", "H2O",    0.074), &
         kijdatadb("csp-srk","classic",1,"CO2", "IC4",    0.15), &
         kijdatadb("csp-srk","classic",1,"CO2", "IC5",    0.15), &
         kijdatadb("csp-srk","classic",1,"CO2", "NC10",   0.15), &
         kijdatadb("csp-srk","classic",1,"CO2", "NC11",   0.15), &
         kijdatadb("csp-srk","classic",1,"CO2", "NC4",    0.15), &
         kijdatadb("csp-srk","classic",1,"CO2", "NC5",    0.15), &
         kijdatadb("csp-srk","classic",1,"CO2", "H2S",    0.099), &
         kijdatadb("csp-srk","classic",1,"CO2", "N2",     -0.042), &
         kijdatadb("csp-srk","classic",1,"CO2", "AR",     0.0940), &
         kijdatadb("csp-srk","classic",1,"CO2", "SO2",    0.0800), &
         kijdatadb("csp-srk","classic",1,"CO2", "H2",     0.1080), &
         kijdatadb("csp-srk","classic",1,"H2S", "NC4",    0.06), &
         kijdatadb("csp-srk","classic",1,"H2S", "NC5",    0.06), &
         kijdatadb("csp-srk","classic",1,"H2S", "NC6",    0.05), &
         kijdatadb("csp-srk","classic",1,"H2S", "NC7",    0.04), &
         kijdatadb("csp-srk","classic",1,"H2S", "NC8",    0.04), &
         kijdatadb("csp-srk","classic",1,"H2S", "NC9",    0.03), &
         kijdatadb("csp-srk","classic",1,"MEG", "CO2",    0.0), &
         kijdatadb("csp-srk","classic",1,"MEG", "H2O",    0.0), &
         kijdatadb("csp-srk","classic",1,"MEG", "C1",     0.0), &
         kijdatadb("csp-srk","classic",1,"MEOH", "CO2",    0.017), &
         kijdatadb("csp-srk","classic",1,"N2", "C1",     0.042), &
         kijdatadb("csp-srk","classic",1,"N2", "C2",     0.060), &
         kijdatadb("csp-srk","classic",1,"N2", "C2_1",   0.075), &
         kijdatadb("csp-srk","classic",1,"N2", "C3",     0.090), &
         kijdatadb("csp-srk","classic",1,"N2", "IC4",    0.113), &
         kijdatadb("csp-srk","classic",1,"N2", "IC5",    0.087), &
         kijdatadb("csp-srk","classic",1,"N2", "NC10",   0.08), &
         kijdatadb("csp-srk","classic",1,"N2", "NC11",   0.08), &
         kijdatadb("csp-srk","classic",1,"N2", "NC4",    0.113), &
         kijdatadb("csp-srk","classic",1,"N2", "NC5",    0.140), &
         kijdatadb("csp-srk","classic",1,"N2", "NC6",    0.150), &
         kijdatadb("csp-srk","classic",1,"N2", "NC7",    0.142), &
         kijdatadb("csp-srk","classic",1,"N2", "NC8",    0.08), &
         kijdatadb("csp-srk","classic",1,"N2", "NC9",    0.08), &
         kijdatadb("csp-srk","classic",1,"N2", "O2",    -0.008), &
         kijdatadb("csp-srk","classic",1,"NC6", "BENZENE",   0.011), &
         kijdatadb("csp-srk","classic",1,"R12", "R11",    0.0054), &
         kijdatadb("csp-srk","classic",1,"R12", "R114",   0.0015), &
         kijdatadb("csp-srk","classic",1,"R12", "R152a",  0.0867), &
         kijdatadb("csp-srk","classic",1,"R13", "R11",    0.0262), &
         kijdatadb("csp-srk","classic",1,"R13", "R113",   0.0243), &
         kijdatadb("csp-srk","classic",1,"R13", "R12",    0.0299), &
         kijdatadb("csp-srk","classic",1,"R13B", "R12",   -0.0032), &
         kijdatadb("csp-srk","classic",1,"R13B", "R152a",  0.0799), &
         kijdatadb("csp-srk","classic",1,"R14", "R13",    0.0304), &
         kijdatadb("csp-srk","classic",1,"R14", "R23",    0.1008), &
         kijdatadb("csp-srk","classic",1,"R22", "R11",    0.0466), &
         kijdatadb("csp-srk","classic",1,"R22", "R114",   0.0399), &
         kijdatadb("csp-srk","classic",1,"R22", "R12",    0.0564), &
         kijdatadb("csp-srk","classic",1,"R22", "R142b",  0.0057), &
         kijdatadb("csp-srk","classic",1,"R22", "R115",   0.0890), &
         kijdatadb("csp-srk","classic",1,"R23", "R13",    0.1032), &
         kijdatadb("csp-srk","classic",1,"R218", "R152a",  0.12), &
         kijdatadb("csp-srk","classic",1,"R125", "R143a", -0.0111), &
         kijdatadb("csp-srk","classic",1,"R125", "R134a", -0.0024), &
         kijdatadb("csp-srk","classic",1,"R143a", "R134a",  0.0013),&
         kijdatadb("csp-srkgb","classic",1,"CO2", "C1",     0.12), &
         kijdatadb("csp-srkgb","classic",1,"CO2", "C2",     0.15), &
         kijdatadb("csp-srkgb","classic",1,"CO2", "C2_1",   0.15), &
         kijdatadb("csp-srkgb","classic",1,"CO2", "C3",     0.15), &
         kijdatadb("csp-srkgb","classic",1,"CO2", "H2O",    0.074), &
         kijdatadb("csp-srkgb","classic",1,"CO2", "IC4",    0.15), &
         kijdatadb("csp-srkgb","classic",1,"CO2", "IC5",    0.15), &
         kijdatadb("csp-srkgb","classic",1,"CO2", "NC10",   0.15), &
         kijdatadb("csp-srkgb","classic",1,"CO2", "NC11",   0.15), &
         kijdatadb("csp-srkgb","classic",1,"CO2", "NC4",    0.15), &
         kijdatadb("csp-srkgb","classic",1,"CO2", "NC5",    0.15), &
         kijdatadb("csp-srkgb","classic",1,"H2S", "NC4",    0.06), &
         kijdatadb("csp-srkgb","classic",1,"H2S", "NC5",    0.06), &
         kijdatadb("csp-srkgb","classic",1,"H2S", "NC6",    0.05), &
         kijdatadb("csp-srkgb","classic",1,"H2S", "NC7",    0.04), &
         kijdatadb("csp-srkgb","classic",1,"H2S", "NC8",    0.04), &
         kijdatadb("csp-srkgb","classic",1,"H2S", "NC9",    0.03), &
         kijdatadb("csp-srkgb","classic",1,"MEG", "CO2",    0.0), &
         kijdatadb("csp-srkgb","classic",1,"MEG", "H2O",    0.0), &
         kijdatadb("csp-srkgb","classic",1,"MEG", "C1",     0.0), &
         kijdatadb("csp-srkgb","classic",1,"MEOH", "CO2",    0.017), &
         kijdatadb("csp-srkgb","classic",1,"N2", "C1",     0.028), &
         kijdatadb("csp-srkgb","classic",1,"N2", "C2",     0.041), &
         kijdatadb("csp-srkgb","classic",1,"N2", "C2_1",   0.08), &
         kijdatadb("csp-srkgb","classic",1,"N2", "C3",     0.076), &
         kijdatadb("csp-srkgb","classic",1,"N2", "IC4",    0.094), &
         kijdatadb("csp-srkgb","classic",1,"N2", "IC5",    0.087), &
         kijdatadb("csp-srkgb","classic",1,"N2", "NC10",   0.08), &
         kijdatadb("csp-srkgb","classic",1,"N2", "NC11",   0.08), &
         kijdatadb("csp-srkgb","classic",1,"N2", "NC4",    0.070), &
         kijdatadb("csp-srkgb","classic",1,"N2", "NC5",    0.088), &
         kijdatadb("csp-srkgb","classic",1,"N2", "NC6",    0.150), &
         kijdatadb("csp-srkgb","classic",1,"N2", "NC7",    0.142), &
         kijdatadb("csp-srkgb","classic",1,"N2", "NC8",    0.08), &
         kijdatadb("csp-srkgb","classic",1,"N2", "NC9",    0.08), &
         kijdatadb("csp-srkgb","classic",1,"N2", "O2",    -0.01), &
         kijdatadb("csp-srkgb","classic",1,"NC6", "BENZENE",   0.011), &
         kijdatadb("csp-srkgb","classic",1,"R12", "R11",    0.0054), &
         kijdatadb("csp-srkgb","classic",1,"R12", "R114",   0.0015), &
         kijdatadb("csp-srkgb","classic",1,"R12", "R152a",  0.0867), &
         kijdatadb("csp-srkgb","classic",1,"R13", "R11",    0.0262), &
         kijdatadb("csp-srkgb","classic",1,"R13", "R113",   0.0243), &
         kijdatadb("csp-srkgb","classic",1,"R13", "R12",    0.0299), &
         kijdatadb("csp-srkgb","classic",1,"R13B", "R12",   -0.0032), &
         kijdatadb("csp-srkgb","classic",1,"R13B", "R152a",  0.0799), &
         kijdatadb("csp-srkgb","classic",1,"R14", "R13",    0.0304), &
         kijdatadb("csp-srkgb","classic",1,"R14", "R23",    0.1008), &
         kijdatadb("csp-srkgb","classic",1,"R22", "R11",    0.0466), &
         kijdatadb("csp-srkgb","classic",1,"R22", "R114",   0.0399), &
         kijdatadb("csp-srkgb","classic",1,"R22", "R12",    0.0564), &
         kijdatadb("csp-srkgb","classic",1,"R22", "R142b",  0.0057), &
         kijdatadb("csp-srkgb","classic",1,"R22", "R115",   0.0890), &
         kijdatadb("csp-srkgb","classic",1,"R23", "R13",    0.1032), &
         kijdatadb("csp-srkgb","classic",1,"R218", "R152a",  0.12), &
         kijdatadb("csp-srkgb","classic",1,"R125", "R143a", -0.0111), &
         kijdatadb("csp-srkgb","classic",1,"R125", "R134a", -0.0024), &
         kijdatadb("csp-srkgb","classic",1,"R143a", "R134a",  0.0013), &
         kijdatadb("csp-pr","classic",1,"C1", "H2S", 0.095), &
         kijdatadb("csp-pr","classic",1,"CO2", "O2", 0.1090), &
         kijdatadb("csp-pr","classic",1,"CO2", "NH3", 0.000), &
         kijdatadb("csp-pr","classic",1,"CO2", "N2O", 0.010), &
         kijdatadb("csp-pr","classic",1,"CO2", "N2O4", 0.00), &
         kijdatadb("csp-pr","classic",1,"CO2", "C1",     0.100), &
         kijdatadb("csp-pr","classic",1,"CO2", "C2",     0.15), &
         kijdatadb("csp-pr","classic",1,"CO2", "C2_1",   0.15), &
         kijdatadb("csp-pr","classic",1,"CO2", "C3",     0.15), &
         kijdatadb("csp-pr","classic",1,"CO2", "H2O",    0.074), &
         kijdatadb("csp-pr","classic",1,"CO2", "IC4",    0.15), &
         kijdatadb("csp-pr","classic",1,"CO2", "IC5",    0.15), &
         kijdatadb("csp-pr","classic",1,"CO2", "NC10",   0.15), &
         kijdatadb("csp-pr","classic",1,"CO2", "NC11",   0.15), &
         kijdatadb("csp-pr","classic",1,"CO2", "NC4",    0.15), &
         kijdatadb("csp-pr","classic",1,"CO2", "NC5",    0.15), &
         kijdatadb("csp-pr","classic",1,"CO2", "H2S",    0.1020), &
         kijdatadb("csp-pr","classic",1,"CO2", "N2",     -0.0360), &
         kijdatadb("csp-pr","classic",1,"CO2", "AR",     0.0880), &
         kijdatadb("csp-pr","classic",1,"CO2", "SO2",    0.075), &
         kijdatadb("csp-pr","classic",1,"CO2", "H2",    0.1210), &
         kijdatadb("csp-pr","classic",1,"CO2", "CO",     -0.0590), &
         kijdatadb("csp-pr","classic",1,"CO2", "NH3",    0.00), &
         kijdatadb("csp-pr","classic",1,"H2S", "NC4",    0.06), &
         kijdatadb("csp-pr","classic",1,"H2S", "NC4",    0.06), &
         kijdatadb("csp-pr","classic",1,"H2S", "NC5",    0.06), &
         kijdatadb("csp-pr","classic",1,"H2S", "NC6",    0.05), &
         kijdatadb("csp-pr","classic",1,"H2S", "NC7",    0.04), &
         kijdatadb("csp-pr","classic",1,"H2S", "NC8",    0.04), &
         kijdatadb("csp-pr","classic",1,"H2S", "NC9",    0.03), &
         kijdatadb("csp-pr","classic",1,"MEG", "CO2",    0.0), &
         kijdatadb("csp-pr","classic",1,"MEG", "H2O",    0.0), &
         kijdatadb("csp-pr","classic",1,"MEG", "C1",     0.0), &
         kijdatadb("csp-pr","classic",1,"MEOH", "CO2",    0.017), &
         kijdatadb("csp-pr","classic",1,"N2", "C1",     0.038), &
         kijdatadb("csp-pr","classic",1,"N2", "C2",     0.041), &
         kijdatadb("csp-pr","classic",1,"N2", "C2_1",   0.08), &
         kijdatadb("csp-pr","classic",1,"N2", "C3",     0.076), &
         kijdatadb("csp-pr","classic",1,"N2", "IC4",    0.094), &
         kijdatadb("csp-pr","classic",1,"N2", "IC5",    0.087), &
         kijdatadb("csp-pr","classic",1,"N2", "NC10",   0.08), &
         kijdatadb("csp-pr","classic",1,"N2", "NC11",   0.08), &
         kijdatadb("csp-pr","classic",1,"N2", "NC4",    0.070), &
         kijdatadb("csp-pr","classic",1,"N2", "NC5",    0.088), &
         kijdatadb("csp-pr","classic",1,"N2", "NC6",    0.150), &
         kijdatadb("csp-pr","classic",1,"N2", "NC7",    0.142), &
         kijdatadb("csp-pr","classic",1,"N2", "NC8",    0.08), &
         kijdatadb("csp-pr","classic",1,"N2", "NC9",    0.08), &
         kijdatadb("csp-pr","classic",1,"N2", "O2",    -0.013), &
         kijdatadb("csp-pr","classic",1,"NC6", "BENZENE",   0.011), &
         kijdatadb("csp-pr","classic",1,"R12", "R11",    0.0054), &
         kijdatadb("csp-pr","classic",1,"R12", "R114",   0.0015), &
         kijdatadb("csp-pr","classic",1,"R12", "R152a",  0.0867), &
         kijdatadb("csp-pr","classic",1,"R13", "R11",    0.0262), &
         kijdatadb("csp-pr","classic",1,"R13", "R113",   0.0243), &
         kijdatadb("csp-pr","classic",1,"R13", "R12",    0.0299), &
         kijdatadb("csp-pr","classic",1,"R13B", "R12",   -0.0032), &
         kijdatadb("csp-pr","classic",1,"R13B", "R152a",  0.0799), &
         kijdatadb("csp-pr","classic",1,"R14", "R13",    0.0304), &
         kijdatadb("csp-pr","classic",1,"R14", "R23",    0.1008), &
         kijdatadb("csp-pr","classic",1,"R22", "R11",    0.0466), &
         kijdatadb("csp-pr","classic",1,"R22", "R114",   0.0399), &
         kijdatadb("csp-pr","classic",1,"R22", "R12",    0.0564), &
         kijdatadb("csp-pr","classic",1,"R22", "R142b",  0.0057), &
         kijdatadb("csp-pr","classic",1,"R22", "R115",   0.0870), &
         kijdatadb("csp-pr","classic",1,"R23", "R13",    0.1032), &
         kijdatadb("csp-pr","classic",1,"R218", "R152a",  0.12), &
         kijdatadb("csp-pr","classic",1,"R125", "R143a", -0.0111), &
         kijdatadb("csp-pr","classic",1,"R125", "R134a", -0.0024), &
         
! --- Default Lee-Kesler interaction is 1.0. Data taken from
! --- Pl{\¨o}cker, Knapp and Prausnitz 1978
         kijdatadb("lk","classic", 1,"C1","C2", 1.052), &
         kijdatadb("lk","classic", 1,"C1","C2_1",  1.014), &
         kijdatadb("lk","classic", 1,"C1","C3",  1.113), &
         kijdatadb("lk","classic", 1,"C1","PRLN",  1.089), &
         kijdatadb("lk","classic", 1,"C1","IC4",  1.155), &
         kijdatadb("lk","classic", 1,"C1","NC4",  1.171), &
         kijdatadb("lk","classic", 1,"C1","NC5",  1.240), &
         kijdatadb("lk","classic", 1,"C1","IC5",  1.228), &
         kijdatadb("lk","classic", 1,"C1","NC6",  1.304), &
         kijdatadb("lk","classic", 1,"C1","NC7",  1.367), &
         kijdatadb("lk","classic", 1,"C1","NC8",  1.423), &
         kijdatadb("lk","classic", 1,"C1","NC9",  1.484), &
         kijdatadb("lk","classic", 1,"C1","NC10",  1.533), &
         kijdatadb("lk","classic", 1,"C1","BENZENE",  1.234), &
         kijdatadb("lk","classic", 1,"C2","C2_1",  0.991), &
         kijdatadb("lk","classic", 1,"C2","C3", 1.010), &
         kijdatadb("lk","classic", 1,"C2","PRLN",  1.002), &
         kijdatadb("lk","classic", 1,"C2","IC4", 1.036), &
         kijdatadb("lk","classic", 1,"C2","NC4",  1.029), &
         kijdatadb("lk","classic", 1,"C2","NC5",  1.064), &
         kijdatadb("lk","classic", 1,"C2","IC5",  1.070), &
         kijdatadb("lk","classic", 1,"C2","NC6",  1.106), &
         kijdatadb("lk","classic", 1,"C2","NC7",  1.143), &
         kijdatadb("lk","classic", 1,"C2","NC8",  1.165), &
         kijdatadb("lk","classic", 1,"C2","NC9",  1.214), &
         kijdatadb("lk","classic", 1,"C2","NC10",  1.237), &
         kijdatadb("lk","classic", 1,"C2","BENZENE",  1.066), &
         kijdatadb("lk","classic", 1,"C2_1","NC4",  0.998), &
         kijdatadb("lk","classic", 1,"C2_1","BENZENE",  1.094), &
         kijdatadb("lk","classic", 1,"C2_1","NC7",  1.163), &
         kijdatadb("lk","classic", 1,"C3","PRLN", 0.992), &
         kijdatadb("lk","classic", 1,"C3","NC4",  1.003), &
         kijdatadb("lk","classic", 1,"C3","IC4", 1.003), &
         kijdatadb("lk","classic", 1,"C3","NC5",  1.006), &
         kijdatadb("lk","classic", 1,"C3","IC5", 1.009), &
         kijdatadb("lk","classic", 1,"C3","NC6",  1.047), &
         kijdatadb("lk","classic", 1,"C3","BENZENE", 1.011), &
         kijdatadb("lk","classic", 1,"C3","NC7",  1.067), &
         kijdatadb("lk","classic", 1,"C3","NC8",  1.090), &
         kijdatadb("lk","classic", 1,"C3","NC9",  1.0115), &
         kijdatadb("lk","classic", 1,"C3","NC10",  1.139), &
         kijdatadb("lk","classic", 1,"PRLN","NC4",  1.010), &
         kijdatadb("lk","classic", 1,"PRLN","IC4", 1.009), &
         kijdatadb("lk","classic", 1,"NC4","IC4",  1.001), &
         kijdatadb("lk","classic", 1,"NC4","NC5",  0.994), &
         kijdatadb("lk","classic", 1,"NC4","IC5",  0.998), &
         kijdatadb("lk","classic", 1,"NC4","NC6",  1.018), &
         kijdatadb("lk","classic", 1,"NC4","BENZENE", 0.999), &
         kijdatadb("lk","classic", 1,"NC4","NC7",  1.027), &
         kijdatadb("lk","classic", 1,"NC4","NC8",  1.046), &
         kijdatadb("lk","classic", 1,"NC4","NC9",  1.064), &
         kijdatadb("lk","classic", 1,"NC4","NC10",  1.078), &
         kijdatadb("lk","classic", 1,"NC5","IC5",  0.987), &
         kijdatadb("lk","classic", 1,"NC5","NC6",  0.996), &
         kijdatadb("lk","classic", 1,"NC5","BENZENE", 0.977), &
         kijdatadb("lk","classic", 1,"NC5","NC7",  1.004), &
         kijdatadb("lk","classic", 1,"NC5","NC8",  1.020), &
         kijdatadb("lk","classic", 1,"NC5","NC9",  1.033), &
         kijdatadb("lk","classic", 1,"NC5","NC10",  1.045), &
         kijdatadb("lk","classic", 1,"NC6","BENZENE", 0.978), &
         kijdatadb("lk","classic", 1,"NC6","NC7",  1.008), &
         kijdatadb("lk","classic", 1,"NC6","NC8",  1.005), &
         kijdatadb("lk","classic", 1,"NC6","NC9",  1.015), &
         kijdatadb("lk","classic", 1,"NC6","NC10",  1.025), &
         kijdatadb("lk","classic", 1,"BENZENE","NC7",  0.985), &
         kijdatadb("lk","classic", 1,"BENZENE","NC8",  0.987), &
         kijdatadb("lk","classic", 1,"BENZENE","NC9",  1.034), &
         kijdatadb("lk","classic", 1,"BENZENE","NC10",  1.047), &
         kijdatadb("lk","classic", 1,"NC7","NC8",  0.993), &
         kijdatadb("lk","classic", 1,"NC7","NC9",  1.002), &
         kijdatadb("lk","classic", 1,"NC7","NC10",  1.010), &
         kijdatadb("lk","classic", 1,"NC8","NC9",  0.993), &
         kijdatadb("lk","classic", 1,"NC8","NC10",  0.999), &
         kijdatadb("lk","classic", 1,"NC9","NC10",  0.991), &
         kijdatadb("lk","classic", 1,"N2","C1", 0.977), &
         kijdatadb("lk","classic", 1,"N2","C2_1",  1.032), &
         kijdatadb("lk","classic", 1,"N2","C2", 1.082), &
         kijdatadb("lk","classic", 1,"N2","C3",  1.177), &
         kijdatadb("lk","classic", 1,"N2","PRLN",  1.151), &
         kijdatadb("lk","classic", 1,"N2","NC4",  1.276), &
         kijdatadb("lk","classic", 1,"N2","NC5",  1.372), &
         kijdatadb("lk","classic", 1,"N2","NC6",  1.442), &
         kijdatadb("lk","classic", 1,"N2","O2",  0.997), &
         kijdatadb("lk","classic", 1,"N2","CO",  0.987), &
         kijdatadb("lk","classic", 1,"N2","H2S",  0.983), &
         kijdatadb("lk","classic", 1,"N2","CO2",  1.110), &
         kijdatadb("lk","classic", 1,"N2","N2O",  1.073), &
         kijdatadb("lk","classic", 1,"N2","NH3",  1.033), &
         kijdatadb("lk","classic", 1,"CO2","C1", 0.975), &
         kijdatadb("lk","classic", 1,"CO2","C2", 0.938), &
         kijdatadb("lk","classic", 1,"CO2","C3",  0.925), &
         kijdatadb("lk","classic", 1,"CO2","NC4",  0.955), &
         kijdatadb("lk","classic", 1,"CO2","IC4",  0.946), &
         kijdatadb("lk","classic", 1,"CO2","NC5",  1.002), &
         kijdatadb("lk","classic", 1,"CO2","NC6",  1.018), &
         kijdatadb("lk","classic", 1,"CO2","BENZENE",  1.018), &
         kijdatadb("lk","classic", 1,"CO2","NC7",  1.058), &
         kijdatadb("lk","classic", 1,"CO2","NC8",  1.090), &
         kijdatadb("lk","classic", 1,"CO2","NC9",  1.126), &
         kijdatadb("lk","classic", 1,"CO2","NC10",  1.160), &
         kijdatadb("lk","classic", 1,"CO2","H2S",  0.922), &
         kijdatadb("lk","classic", 1,"CO2","R12",  0.969), &
         kijdatadb("lk","classic", 1,"CO2","MEOH",  1.069), &
         kijdatadb("lk","classic", 1,"H2","C1",  1.216), &
         kijdatadb("lk","classic", 1,"H2","C2",  1.604), &
         kijdatadb("lk","classic", 1,"H2","PRLN",  1.498), &
         kijdatadb("lk","classic", 1,"H2","C3",  1.826), &
         kijdatadb("lk","classic", 1,"H2","NC4", 2.093), &
         kijdatadb("lk","classic", 1,"H2","NC5",  2.335), &
         kijdatadb("lk","classic", 1,"H2","NC6",  2.456), &
         kijdatadb("lk","classic", 1,"H2","NC7",  2.634), &
         kijdatadb("lk","classic", 1,"H2","N2",  1.080), &
         kijdatadb("lk","classic", 1,"H2","CO",  1.085), &
         kijdatadb("lk","classic", 1,"H2","CO2",  1.624), &
         kijdatadb("lk","classic", 1,"O2","N2O",  1.057), &
         kijdatadb("lk","classic", 1,"CO","C1",  0.974), &
         kijdatadb("lk","classic", 1,"H2S","IC4",  0.947), &
         kijdatadb("lk","classic", 1,"N2O","C1",  1.017), &
         kijdatadb("lk","classic", 1,"H2O","CO2",  0.920), &
         kijdatadb("lk","classic", 1,"H2O","NH3",  1.152), &
         kijdatadb("lk","classic", 1,"H2O","MEOH", 0.979), &
         kijdatadb("lk","classic", 1,"CO2","AR", 0.992), & ! Mazzoccoli et. al. 2013, dx.doi.org/10.3303/CET1332311
         kijdatadb("lk","classic", 1,"CO2","O2", 1.032) & ! Visually tuned 1.032
        /)


    type :: interGEdatadb
       character (len=8) :: eosid
       character (len=12) :: mruleid
       integer setno
       character (len=8) :: uid1, uid2
       real :: kijvalue                   ! used if alpha_ij and alpha_ji = 0.0
       integer :: correlation             ! 1: default, 2: Maribo-Mogensen (2015) - 10.1002/aic.14829
       real :: alphaijvalue(2)            ! alpha_ij != alpha_ji
       real :: polyij1(2),polyji1(2)    ! uid1 uid2: poly_ij != poly_ji
       real :: polyij2(3),polyji2(3)    ! 1st and 2nd order polynominal for NRTL-model Gibbs energy interaction
     end type interGEdatadb


     ! eos     mrule    set   i    j      kij    alphaij/ji   poly1d_ji  polyid_ji      poly2d_ij       poly2d_ij
     ! (g21-g11)/R = a1 + b1*T + c1*T*T  (= tau21*T)
     ! (g12-g22)/R = a2 + b2*T + c1*T*T  (= tau12*T)

   integer, parameter :: maxinterGEij =  13
   type (interGEdatadb), dimension (maxinterGEij), parameter :: interGEdb = (/ &
        interGEdatadb("srk","huronvidal", 1,"C1","H2O",&
!        kij   alphaij,ji     a1      b1        a2    b2
        0.52,1,(/0.15,0.15/),(/5035.7,-7.15/),(/-162.8,2.16/),&
!         a1     b1      c1          a2     b2      c2
        (/8404.0,-28.031,0.03142/),(/-1149.0,8.019,-0.00861/)), &
        interGEdatadb("srk","huronvidal", 1,"C1","MEG", &
        0.134,1,(/0.07,0.07/),(/2274.0,0.0/),(/181.0,0.0/),&
        (/2274.0,0.0,0.0/),(/181.0,0.0,0.0/)), &
        interGEdatadb("srk","huronvidal", 1,"CO2","H2O",&
        0.193,1,(/0.03,0.03/),(/6563.0,-5.12/),(/-3741.0,1.55/),&
        (/5858.39,2.4112,-0.01713/),(/-1237.41,-15.5058,0.02896/)), &
        interGEdatadb("srk","huronvidal", 1,"CO2","MEG",&
        0.000,0,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
        (/0.0,0.0,0.0/),(/0.0,0.0,0.0/)), &
        interGEdatadb("srk","huronvidal", 1,"H2O","MEG",& ! From Folas
        -0.063,1,(/0.95,0.95/),(/59.0,0.0/),(/105.0, 0.0/),&
        (/59.0,0.0,0.0/),(/105.0,0.0,0.0/)), & ! From Folas (2006), kij from Hemmingsen (2012)
!        -0.063,(/0.004,0.004/),(/-957.6,0.0/),(/2673.0, 0/),(/-957.6,0.0,0.0/),(/2673.0,0.0,0.0/)), & ! from Hemmingsen (2012)
        interGEdatadb("pr","huronvidal", 1,"H2O","MEG",& ! From Hemmingsen et. al gij = polyjij(2)
        -0.065,1,(/0.4,0.4/),(/0.0,218.3/),(/0.0,72.02/),&
        (/0.0,0.0,0.0/),(/0.0,0.0,0.0/)), &
        interGEdatadb("csp-srk","huronvidal", 1,"C1","H2O",&
        0.52,1,(/0.15,0.15/),(/5035.7,-7.15/),(/-162.8,2.16/),&
        (/8404.0,-28.031,0.03142/),(/-1149.0,8.019,-0.00861/)), &
        interGEdatadb("csp-srk","huronvidal", 1,"C1","MEG", &
        0.134,1,(/0.07,0.07/),(/2274.0,0.0/),(/181.0,0.0/),&
        (/2274.0,0.0,0.0/),(/181.0,0.0,0.0/)), &
        interGEdatadb("csp-srk","huronvidal", 1,"CO2","H2O",&
        0.193,1,(/0.03,0.03/),(/6563.0,-5.12/),(/-3741.0,1.55/),&
        (/5858.39,2.4112,-0.01713/),(/-1237.41,-15.5058,0.02896/)), &
        interGEdatadb("csp-srk","huronvidal", 1,"CO2","MEG",&
        0.000,0,(/0.0,0.0/),(/0.0,0.0/),(/0.0,0.0/),&
        (/0.0,0.0,0.0/),(/0.0,0.0,0.0/)), &
        interGEdatadb("csp-srk","huronvidal", 1,"H2O","MEG",& ! From Folas
        -0.063,1,(/0.95,0.95/),(/59.0,0.0/),(/105.0, 0.0/),&
        (/59.0,0.0,0.0/),(/105.0,0.0,0.0/)), & ! From Folas (2006), kij from Hemmingsen (2012)
        interGEdatadb("csp-pr","huronvidal", 1,"H2O","MEG",& ! From Hemmingsen et. al gij = polyjij(2)
        -0.065,1,(/0.4,0.4/),(/0.0,218.3/),(/0.0,72.02/),&
        (/0.0,0.0,0.0/),(/0.0,0.0,0.0/)), &
        interGEdatadb("pr","NRTL", 1,"H2S","C3",&
        0.075,1,(/0.3,0.3/),(/345.712,0.0/),(/-29.3377,0.0/),&
        (/345.712,0.0,0.0/),(/-29.3377,0.0,0.0/)) & ! Dicko, Coqulet, Theveneau, Mougin (2012)
        /)

    type :: alphaTWUdatadb
       character (len=8) :: eosid
       character (len=8) :: uid
       integer :: setno
       real :: coeff(3)
    end type alphaTWUdatadb

    type :: alphaMCdatadb ! Mathias-Copeman formulation for polar fluids H2O,MEG,...
       character (len=8) :: eosid
       character (len=8) :: uid
       integer :: setno
       real :: coeff(3)
    end type alphaMCdatadb

    type :: alphaGERGdatadb ! Gerg-Water, EOS (PR with special alpha corr
       character (len=8) :: eosid
       character (len=8) :: uid
       real :: coeff(3)
    end type alphaGERGdatadb

    ! These pure data constants are fitted to the vapour pressure curve for the selected equation of state.
    ! The values given below are for SRK.
    ! TWU also has general parameters that scale linearly with the acentric factor.
    ! One set for sub-critical and one set for super-critical region.
    ! Should only be used for sub-critial conditions.

    integer, parameter :: maxalphaTWUparam = 38
    type (alphaTWUdatadb),dimension (maxalphaTWUparam), parameter :: alphaTWUdb = (/ &
! ... old values ...
         alphaTWUdatadb("SRK","CO2", 1, (/1.2341,1.3268,0.6499/)), &
         alphaTWUdatadb("SRK","C1" , 1, (/0.5144,0.9903,1.0/)),    &
         alphaTWUdatadb("SRK","H2S", 1, (/0.5039,0.8855,1.1161/)), &
         alphaTWUdatadb("SRK","H2O", 1, (/0.3569,0.8743,2.4807/)),  &
! ... Regressed for subcritical vapour pressure against the reference equations between the
! triple point and critical point for SRK and PR EOS
! By GS 2013-02-09
!
!         alphaTWUdatadb("SRK","CO2",(/1.022457,0.843957,1.049826/)), &
!         alphaTWUdatadb("SRK","C1",(/0.746029,0.247115,0.627029/)), &
!         alphaTWUdatadb("SRK","C3",(/0.973275,0.432097,0.87074/)), &
!         alphaTWUdatadb("SRK","H2S",(/0.0,0.0,0.773518/)), &
!         alphaTWUdatadb("SRK","H2O",(/1.443182,0.607805,1.424787/)), &
!         alphaTWUdatadb("SRK","N2",(/0.0,0.0,0.586285/)), &
!         alphaTWUdatadb("PR","CO2",(/0.0,0.0,0.651497/)), &
!         alphaTWUdatadb("PR","C1",(/0.483727,0.084682,0.318364/)), &
!         alphaTWUdatadb("PR","C3",(/0.0,0.0,0.542362/)), &
!         alphaTWUdatadb("PR","H2S",(/0.0,0.0,0.464801/)), &
!         alphaTWUdatadb("PR","H2O",(/0.0,0.0,1.385915/)), &
!         alphaTWUdatadb("PR","N2",(/0.0,0.0,0.305595/)) &
         ! Guennec 2016 (10.1016/j.fluid.2016.09.003)
         alphaTWUdatadb("PR","C1", 2, (/0.1471,0.9074,1.8253/)),  &
         alphaTWUdatadb("PR","C2", 2, (/0.1459,0.8780,2.1536/)),  &
         alphaTWUdatadb("PR","C3", 2, (/0.1596,0.8681,2.2820/)),  &
         alphaTWUdatadb("PR","N2", 2, (/0.1240,0.8897,2.0138/)),  &
         alphaTWUdatadb("PR","H2", 2, (/1.5147,-3.7959,-0.1377/)),  &
         alphaTWUdatadb("PR","CO2", 2, (/0.1783,0.8590,2.4107/)),  &
         alphaTWUdatadb("PR","AR", 2, (/0.1228,0.9045,1.8539/)),  &
         alphaTWUdatadb("PR","He", 2, (/0.0063,1.2175,1.0909/)),  &
         alphaTWUdatadb("PR","Ne", 2, (/0.1887,0.9470,1.4698/)),  &
         alphaTWUdatadb("PR","D2", 2, (/0.1486,0.9968,1.0587/)),  &
         alphaTWUdatadb("PR","O2", 2, (/0.2129,0.8913,1.4005/)),  &
         alphaTWUdatadb("PR","CO", 2, (/0.0983,0.8777,2.1568/)),  &
         alphaTWUdatadb("PR","H2S", 2, (/0.1120,0.8688,2.2735/)),  &
         alphaTWUdatadb("PR","NH3", 2, (/0.2274,0.8645,2.3320/)),  &
         alphaTWUdatadb("PR","H2O", 2, (/0.3865,0.8720,1.9693/)),  &
         alphaTWUdatadb("PR","SO2", 2, (/0.4184,0.8238,1.4068/)),  &
         alphaTWUdatadb("PR","NO", 2, (/0.8815,0.9552,1.4047/)),  &
         !
         alphaTWUdatadb("SRK","C1", 2, (/0.2171,0.9082,1.8172/)),  &
         alphaTWUdatadb("SRK","C2", 2, (/0.2240,0.8816,2.1090/)),  &
         alphaTWUdatadb("SRK","C3", 2, (/0.2453,0.8737,2.2088/)),  &
         alphaTWUdatadb("SRK","N2", 2, (/0.1902,0.8900,2.0106/)),  &
         alphaTWUdatadb("SRK","H2", 2, (/0.9444,3.0087,0.1762/)),  &
         alphaTWUdatadb("SRK","CO2", 2, (/0.2807,0.8685,2.2778/)),  &
         alphaTWUdatadb("SRK","AR", 2, (/0.2023,0.9086,1.8129/)),  &
         alphaTWUdatadb("SRK","He", 2, (/-0.0466,1.2473,0.5401/)),  &
         alphaTWUdatadb("SRK","Ne", 2, (/0.3275,0.9699,1.2893/)),  &
         alphaTWUdatadb("SRK","D2", 2, (/0.2150,0.9921,1.1079/)),  &
         alphaTWUdatadb("SRK","O2", 2, (/0.2118,0.9020,1.8798/)),  &
         alphaTWUdatadb("SRK","CO", 2, (/0.1625,0.8778,2.1562/)),  &
         alphaTWUdatadb("SRK","H2S", 2, (/0.1749,0.8686,2.2761/)),  &
         alphaTWUdatadb("SRK","NH3", 2, (/0.2981,0.8651,2.3245/)),  &
         alphaTWUdatadb("SRK","H2O", 2, (/0.4163,0.8756,2.1842/)),  &
         alphaTWUdatadb("SRK","SO2", 2, (/0.4014,0.8358,1.7355/)),  &
         alphaTWUdatadb("SRK","NO", 2, (/0.8681,0.9320,1.5954/))  &

! ... Regressed for subcritical vapour pressure against the reference equations between the
! triple point and critical point for SRK and PR EOS
! By GS 2013-02-09
         /)

    integer, parameter :: maxalphaMCparam = 25+40

    type (alphaMCdatadb),dimension (maxalphaMCparam), parameter :: alphaMCdb = (/ &
         alphaMCDatadb("PR","CO2", 1, (/0.704606, -0.314862, 1.89083/) ), &
         alphaMCDatadb("PR","AR",  1, (/0.397483, -0.282393, 0.796288/) ),  &
         !CO2, AR:Heidermann, Khalil Aiche J. 26 (1980) 769-779
         alphaMCDatadb("PR","CO", 1, (/0.7050, -0.3185, 1.9012/) ), &
         !O. Chiavone-Filho, P.G. Amaral http://dx.doi.org/10.1021/ie001134o   
         !Ind. Eng. Chem. Res., 2001, 40 (26), pp 6240–6244
         
         !Written by Coquelet, Dieu, Richmin, Arpentiner & LockWood, Fluid PHase Equl. 293 (2008) 38-43
!         alphaMCDatadb("PR","N2", (/0.5867, -0.4459, 0.8926/) ), &
         alphaMCDatadb("PR","N2", 1, (/0.404606, 0.391057, -0.963495/) ), &
         alphaMCDatadb("PR","C1", 1, (/0.5857, -0.7206, 1.2899/) ), &
         alphaMCDatadb("PR","C2", 1, (/0.7178, -0.7644, 1.6396/) ), &
         alphaMCDatadb("PR","C3", 1, (/0.7863, -0.7459, 1.8454/) ), &
         alphaMCDatadb("PR","IC4", 1, (/0.8288, -0.8285, 2.3201/) ), &
         alphaMCDatadb("PR","NC4", 1, (/0.8787, -0.9399, 2.2666/) ), &
         alphaMCDatadb("PR","IC5", 1, (/0.8767, -0.6043, 1.4025/) ), &
         alphaMCDatadb("PR","NC5", 1, (/0.9820, -1.1695, 2.7523/) ), &
         alphaMCDatadb("PR","NC6", 1, (/1.0430, -1.1553, 2.9235/) ), &
         alphaMCDatadb("PR","NC7", 1, (/1.2278, -1.5558, 3.9361/) ), &
         alphaMCDatadb("PR","NC8", 1, (/1.2798, -1.3822, 3.3933/) ), &
         alphaMCDatadb("SRK","N2", 1, (/0.5867, -0.4459, 0.8926/) ), &
         alphaMCDatadb("SRK","C1", 1, (/0.5857, -0.7206, 1.2899/) ), &
         alphaMCDatadb("SRK","C2", 1, (/0.7178, -0.7644, 1.6396/) ), &
         alphaMCDatadb("SRK","C3", 1, (/0.7863, -0.7459, 1.8454/) ), &
         alphaMCDatadb("SRK","IC4", 1, (/0.8288, -0.8285, 2.3201/) ), &
         alphaMCDatadb("SRK","NC4", 1, (/0.8787, -0.9399, 2.2666/) ), &
         alphaMCDatadb("SRK","IC5", 1, (/0.8767, -0.6043, 1.4025/) ), &
         alphaMCDatadb("SRK","NC5", 1, (/0.9820, -1.1695, 2.7523/) ), &
         alphaMCDatadb("SRK","NC6", 1, (/1.0430, -1.1553, 2.9235/) ), &
         alphaMCDatadb("SRK","NC7", 1, (/1.2278, -1.5558, 3.9361/) ), &
         alphaMCDatadb("SRK","NC8", 1, (/1.2798, -1.3822, 3.3933/) ), &
         !For N2, C1,...NC8. Made for SRK, (called SRMC in article). Must use curve fitting with PR
         !Ø Mørch et al, 2005, "Measurement and modeling of hydrocarbon dew points for five synthetic natural gas mixtures"
         !Fluid Phase Equilibria 239 (2006) 138-145.

         ! Set 2: Thesis by Antonin Chapoy (2005): Phase behaviour in water/hydrocarbon mixtures
         ! involved in gas production systems.
         ! Table 5.1, tuned agains DIPPR database.
         alphaMCDatadb("SRK","H2", 2, (/0.161, -0.225, -0.232/) ), &
         alphaMCDatadb("SRK","C1", 2, (/0.549, -0.409, 0.603/) ), &
         alphaMCDatadb("SRK","O2", 2, (/0.545, -0.235, 0.292/) ), &
         alphaMCDatadb("SRK","N2", 2, (/0.584, -0.396, 0.736/) ), &
         alphaMCDatadb("SRK","C2_1", 2, (/0.652, -0.315, 0.563/) ), &
         alphaMCDatadb("SRK","H2S", 2, (/0.641, -0.183, 0.513/) ), &
         alphaMCDatadb("SRK","C2", 2, (/0.711, -0.573, 0.894/) ), &
         alphaMCDatadb("SRK","C3", 2, (/0.775, -0.476, 0.815/) ), &
         alphaMCDatadb("SRK","IC4", 2, (/0.807, -0.432, 0.91/) ), &
         alphaMCDatadb("SRK","NC4", 2, (/0.823, -0.267, 0.402/) ), &
         alphaMCDatadb("SRK","BENZENE", 2, (/0.84, -0.389, 0.917/) ), &
         alphaMCDatadb("SRK","CO2", 2, (/0.867, -0.674, 2.471/) ), &
         alphaMCDatadb("SRK","IC5", 2, (/0.876, -0.386, 0.66/) ), &
         alphaMCDatadb("SRK","NC5", 2, (/0.901, -0.305, 0.542/) ), &
         alphaMCDatadb("SRK","NH3", 2, (/0.916, -0.369, 0.417/) ), &
         alphaMCDatadb("SRK","TOLU", 2, (/0.923, -0.301, 0.494/) ), &
         alphaMCDatadb("SRK","NC6", 2, (/1.005, -0.591, 1.203/) ), &
         alphaMCDatadb("SRK","H2O", 2, (/1.095, -0.678, 0.7/) ), &
         alphaMCDatadb("SRK","NC7", 2, (/1.036, -0.258, 0.488/) ), &
         alphaMCDatadb("SRK","NC8", 2, (/1.15, -0.587, 1.096/) ), &
!
         alphaMCDatadb("PR","H2", 2, (/0.095, -0.275, -0.029/) ), &
         alphaMCDatadb("PR","C1", 2, (/0.416, -0.173, 0.348/) ), &
         alphaMCDatadb("PR","O2", 2, (/0.413, -0.017, 0.092/) ), &
         alphaMCDatadb("PR","N2", 2, (/0.448, -0.157, 0.469/) ), &
         alphaMCDatadb("PR","C2_1", 2, (/0.512, -0.087, 0.349/) ), &
         alphaMCDatadb("PR","H2S", 2, (/0.507, 0.008, 0.342/) ), &
         alphaMCDatadb("PR","C2", 2, (/0.531, -0.062, 0.214/) ), &
         alphaMCDatadb("PR","C3", 2, (/0.6, -0.006, 0.174/) ), &
         alphaMCDatadb("PR","IC4", 2, (/0.652, -0.149, 0.599/) ), &
         alphaMCDatadb("PR","NC4", 2, (/0.677, -0.081, 0.299/) ), &
         alphaMCDatadb("PR","BENZENE", 2, (/0.701, -0.252, 0.976/) ), &
         alphaMCDatadb("PR","CO2", 2, (/0.705, -0.315, 1.89/) ), &
         alphaMCDatadb("PR","IC5", 2, (/0.724, -0.166, 0.515/) ), &
         alphaMCDatadb("PR","NC5", 2, (/0.763, -0.224, 0.669/) ), &
         alphaMCDatadb("PR","NH3", 2, (/0.748, -0.025, 0.001/) ), &
         alphaMCDatadb("PR","TOLU", 2, (/0.762, -0.042, 0.271/) ), &
         alphaMCDatadb("PR","NC6", 2, (/0.87, -0.588, 1.504/) ), &
         alphaMCDatadb("PR","H2O", 2, (/0.919, -0.332, 0.317/) ), &
         alphaMCDatadb("PR","NC7", 2, (/0.878, -0.031, 0.302/) ), &
         alphaMCDatadb("PR","NC8", 2, (/0.958, -0.134, 0.487/) ) &
         /)

    type (alphaGERGdatadb) , parameter :: alphaGERGIcedb = alphaGERGDatadb("PR","H2O",(/0.106025,2.683845,-4.75638/)) ! Gerg-PR EOS over ice
    type (alphaGERGdatadb) ,parameter :: alphaGERGWaterdb = alphaGERGDatadb ("PR","H2O",(/0.905436,-0.213781,0.26005/)) ! Gerg-PR EOS over water
    

 end module eosdatadb
