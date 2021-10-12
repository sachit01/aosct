XMLs for Test Scenario 1:
1. Train consist: 1 Loco 80 Cars of type BHP-CAR(BHP production Loco and Cars) B-side connected.
2. Train Length 1000m
3.a. Dyn. weight Loco: 195Tons
  b. Dyn. weight car loaded: 160 Tons
  c. Dyn. weight car empty: 21 Tons
  d. Brk. weight Loco: 50 Tons
  e. Brk. weight loaded car: 18 Tons
  f. Brk. weight empty car: 7 Tons
4. loco Type: EMD
5. Balise antenna pos. front: 19m, rear:1m - Use "BHPV9_AntPos19TrainLen1000" .ini file in P:\tools_bhp\VSim\cfg folder.
6. 1 Track 5km long
7. 10 balises - Use "AOS_5Km_10Balises.tdg" in P:\tools_bhp\VSim\tdg folder.
8. 2 speed targets(30km/h, 60km/h) at 2km and 3km respectively.
9. 5 Gradient targets(+1,-2,-4,-2,0) at 2.2km, 2.5km, 3.5km, 4.3km, 4.7km respectively.


Registration:
- Use DefaultPRR_Reg to start with Registration Process.
- TSetup_80Car, MA_Reg_80Cars_All_Targets OR TSetup_80Car, MA_Reg_80Cars, MA_Forw_80Cars be used.
- When the ATP goes into idle state MA_Forw_Scratch_80Cars to send MA from scratch.
- MA_Rev_Scratch_80Cars be used to get reverse MA back to the start on same track.

Re-registration:
- Use DefaultPRR_ReReg to start with Registration Process.
- TSetup_ReReg_80Car, MA_ReReg_80Cars_All_Targets

VSIM:
- While running on the VSIM, in Control Window, there is a need to set the Start Position to 891 and End Position to 5000 as the train is assumed to be at the start of the track 5000m long and antenna is
positioned 19m from the front of the train.