import re
import pandas as pd
import os
import sys, getopt
import pprint
import numpy as np

'''
BXD Test Data Parser
Rev 1.0 : Released with A0 93K Rev1.1 CP program Date : 8/16/2020
Rev B1.2 : Released with B0 93K Rev1.0 CP program Date : 10/2/2020
'''

def yieldCount(data) :
    # Print a summary table with the count per bin
    numDie = len(data)
    data = data[["SBIN", "SBINName"]]
    binSummary = pd.DataFrame(data.groupby(['SBINName', "SBIN"])["SBINName"].count())
    binSummary = binSummary.rename(columns={'SBINName':'Diecount'})
    binSummary["Yields"] = round(binSummary*100/numDie, 2)
    binSummary['numbers'] = pd.to_numeric((binSummary.index.get_level_values(1)))

    binSummary = binSummary.sort_values(['numbers'])
    del binSummary['numbers']
    return binSummary

"""
BIST |   BIRA     | Need     | Become (min of BIST and BIRA)
0        Nan(2)     0            0
1        Nan(2)     1            1
1          0        0            0
Nan(3)   Nan(2)     Nan          2(Nan)
1          1         1           1
"""

def biraAnalysisPerTest(data, mBistMaps) :

    # For each BIST test, collapse the BIST and BIRA into a final BIST name column, add a SKU column
    # if only BIST, add a SKU column
    for index,row in mBistMaps.iterrows():
        bistName = row['BIST']; biraName = row['BIRA']
        for testVoltage in ["HV", "LV"] :
            df_bist = data.loc[:, data.columns.str.contains(bistName+".*:"+testVoltage)]
            df_bira = data.loc[:, data.columns.str.contains(biraName+".*:"+testVoltage)]

            # remove SKU from the column name now

            if df_bira.empty :
                if not df_bist.empty : # Case when Bira patterns do not exist or nonrepairable
                    # Copy over the column with SKU_
                    colname = df_bist.columns.tolist()[0]
                    newcolname = colname.replace("SKU_", "")
                    data.rename(columns={colname:newcolname}, inplace = True)
                    data[colname] = data[newcolname]

                else : # Case when this test is not run in the program for some reason
                    print (" tests not run ", df_bist.columns.tolist()[0])
                    pass
            elif df_bist.empty and not df_bira.empty : # Case when BIRA is run, but no BIST ?
                print ("ERROR : BIST is not run but BIRA is ???", biraName)
                exit(1)
            else : # Case when BIRA and BIST are run, have to account for NAN in both

                bistcolname = df_bist.columns.tolist()[0]
                biracolname = df_bira.columns.tolist()[0]
                df_bist = df_bist.fillna(3)
                df_bira = df_bira.fillna(2)

                newbistcolname = bistcolname.replace("SKU_", "")
                newbiracolname = biracolname.replace("SKU_", "")

                #print("oldname, newname ", bistcolname, newbistcolname)
                data.rename({bistcolname: newbistcolname}, axis=1, inplace=True)
                data.rename({biracolname: newbiracolname}, axis=1, inplace=True)

                # now only without SKU

                df = pd.DataFrame()
                df[bistcolname] = df_bist[bistcolname]
                df[biracolname] = df_bira[biracolname]

                data[bistcolname] = df.min(axis=1)
                data[bistcolname] = data[bistcolname].replace(2,np.nan)
                #print("oldname, newname ", bistcolname, newbistcolname)


    return data

def addlwxyPerRow(data, summary) :
    summary[['lotId', "Wafer", "X", "Y"]] = data[['lotId',"Wafer","X","Y"]]
    col = summary.pop("Y"); summary.insert(0,col.name, col)
    col = summary.pop("X"); summary.insert(0,col.name, col)
    col = summary.pop("Wafer"); summary.insert(0,col.name, col)
    col = summary.pop("lotId"); summary.insert(0,col.name, col)
    return summary


def yieldLossPerTest(data, testTypes) :

    summary = pd.DataFrame()
    for test in testTypes.Type.unique().tolist():
        if re.match(test,"BIRA"):
            continue;
        testnames = testTypes.loc[testTypes["Type"] == test].index.values
        testnames = [s for s in testnames if "SKU_" in s]

        testdf = data[testnames]
        # Replace all NAN with 2
        testdf.fillna(-2)
        # Then find max across all columns
        tempdf = pd.DataFrame()
        tempdf[test] = testdf.max(axis=1).replace(-2,np.nan)
        summary[test] = tempdf[test]

    summary.loc['Totals'] = summary.sum(numeric_only = True, axis=0)
    summary = addlwxyPerRow(data,summary)

    print(summary)

    #print(summary)

    return summary

def yieldLossPerTestBlock(data, testTypes) :

    summary = pd.DataFrame()
    testgroups = testTypes.groupby(['Type', 'Block']).size()
    testgroups = testgroups.reset_index()

    for index,row in testgroups.iterrows():
        if re.match(row["Type"], "BIRA"):
            continue
        print(row["Type"],row["Block"])
        # get all the rownames in testtypes, with Type and Block
        testnames = testTypes.loc[(testTypes["Type"] == row["Type"]) & (testTypes["Block"] == row["Block"])].index.values

        testnames = [s for s in testnames if "SKU_" in s]

        testdf = data[testnames]
        # Replace all NAN with -2
        testdf.fillna(-2)
        # Then find max across all columns
        tempdf = pd.DataFrame()
        name = row["Type"]+"_"+row["Block"]
        tempdf[name] = testdf.max(axis=1).replace(-2,np.nan)
        summary[name] = tempdf[name]

    summary.loc['Totals'] = summary.sum(numeric_only = True, axis=0)

    summary = addlwxyPerRow(data,summary)
    print(summary)

    return summary

def yieldLossPerTestBlockQuad(data, testTypes) :

    summary = pd.DataFrame()
    testgroups = testTypes.groupby(['Type', 'Block', 'Quad']).size()
    testgroups = testgroups.reset_index()

    for index,row in testgroups.iterrows():
        if re.match(row["Type"], "BIRA"):
            continue
        print(row["Type"],row["Block"],row['Quad'])
        # get all the rownames in testtypes, with Type and Block
        testnames = testTypes.loc[(testTypes["Type"] == row["Type"]) & (testTypes["Block"] == row["Block"]) & (testTypes["Quad"] == row["Quad"])].index.values

        testnames = [s for s in testnames if "SKU_" in s]

        testdf = data[testnames]
        # Replace all NAN with -2
        testdf.fillna(-2)
        # Then find max across all columns
        tempdf = pd.DataFrame()
        name = row["Type"]+"_"+row["Block"]+"_"+row["Quad"]
        tempdf[name] = testdf.max(axis=1).replace(-2,np.nan)
        summary[name] = tempdf[name]

    summary.loc['Totals'] = summary.sum(numeric_only = True, axis=0)

    summary = addlwxyPerRow(data,summary)
    #print(summary)

    return summary

def yieldLossPerBlockQuad(data, testTypes) :

    summary = pd.DataFrame()
    testgroups = testTypes.groupby(['Block', 'Quad']).size()
    testgroups = testgroups.reset_index()

    for index,row in testgroups.iterrows():
        print(row["Block"],row['Quad'])
        # get all the rownames in testtypes, with Type and Block
        testnames = testTypes.loc[ (testTypes["Block"] == row["Block"]) & (testTypes["Quad"] == row["Quad"])].index.values

        testnames = [s for s in testnames if (("SKU_" in s) & ("BIRA" not in s))]

        testdf = data[testnames]
        # Replace all NAN with -2
        testdf.fillna(-2)
        # Then find max across all columns
        tempdf = pd.DataFrame()
        name = row["Block"]+"_"+row["Quad"]
        tempdf[name] = testdf.max(axis=1).replace(-2,np.nan)
        summary[name] = tempdf[name]

    summary.loc['Totals'] = summary.sum(numeric_only = True, axis=0)

    summary = addlwxyPerRow(data,summary)
    #print(summary)

    return summary



def parse_CP_atdf(filepath, data, testTypes, mBistMaps) :
    dflinenum = -1
    datafile = os.path.basename(filepath)
    pindict = {}
    preset = set()

    postset = set()

    if not "Type" in testTypes :
        testTypes["Type"] = pd.Series(dtype=str)
        testTypes["Block"] = pd.Series(dtype=str)
        testTypes["Quad"] = pd.Series(dtype=str)
        testTypes["Stage"] = pd.Series(dtype=str)
        testTypes["Voltage"] = pd.Series(dtype=str)

    #pprint.pprint(testTypes.head().values)

    with open(filepath, 'r') as file_object:
        line = file_object.readline()
        partStart = 0
        debug = 0;


        # header parameters
        fileStartTime, lotId, testerId, testProgram, MIRTemperature = ("",)*5
        pinname = ""
        waferNum = ""

        while line :
            #print(line)

            matches = line.rstrip().split("|")
            if re.match("MIR", line):
                fileStartTime = matches[2]
                lotId=matches[9]
                testerId=matches[11]
                testProgram=matches[13]
                MIRTemperature=matches[20]

            if re.match("PMR\|", line):
                #print("here with PMR", line)
                pinname = matches[3]
                pinnumber = matches[1]
                #print("found pin", pinname, pinnumber)
                pindict[pinnumber] = pinname

            if re.match("DTR\|", line):
                testtype = matches[1]
                #if re.match("Continuity",testtype, re.IGNORECASE):
                #    # get the MPR here
                #    ###### REUSE OF MATCHES and LINE
                #    line = file_object.readline()
                #    matches = line.rstrip().split("|")
                #    values = matches[9].split(",")
                #    pinOrder = matches[20].split(",")
                #    #print("here is pinorder", pinOrder)
                #    pinValues = dict(zip(pinOrder,values))
                #    data.at[dflinenum,"ContinuityPF"] = "Fail" if int(matches[4])>0 else "Pass"
                #    # TBD : Dump the per pin if you need to see which pin failed


            if re.match("WIR", line): waferNum = matches[4]

            if re.match("PIR", line) and partStart == 0:

                partStart = 1;
                dflinenum += 1
                data = data.append(pd.Series(name=dflinenum) )
                if "fileName" not in data.columns :
                    data["fileName"] = ""
                    data["fileStartTime"] = ""
                    data["lotId"] = ""
                    data["testerId"] = ""
                    data["testProgram"] = ""
                    data["MIRTemperature"] = ""
                data.at[dflinenum,"fileName"] = datafile
                data.at[dflinenum,"fileStartTime"] = fileStartTime
                data.at[dflinenum,"lotId"] = lotId
                data.at[dflinenum,"testerId"] = testerId
                data.at[dflinenum,"testProgram"] = testProgram
                data.at[dflinenum,"MIRTemperature"] = MIRTemperature

            if (re.match("PTR",line) or re.match("FTR", line)) and partStart > 0 :

                #parse
                testnum  = matches[1]
                testname = matches[7]
                passfail = matches[4]
                testvalue = matches[6]
                LSL = matches[13]
                USL = matches[14]
                Units = matches[15]

                # Really need V3.8 to pretty this up, should i migrate
                m0 = re.match("Read_Globals",testname, re.IGNORECASE)
                m1 = re.match("dc_([a-z,_]*)_short_pre", testname, re.IGNORECASE)
                m2 = re.match("dc_([a-z,_]*)_short_post", testname, re.IGNORECASE)
                m3 = re.match("iddq_test_pre_nom", testname, re.IGNORECASE)
                m4 = re.match("pmro_ts", testname, re.IGNORECASE)
                m5 = re.match("pmro_sv", testname, re.IGNORECASE)
                m6 = re.match("pmro_lv", testname, re.IGNORECASE)
                m7 = re.match("pmro_uv", testname, re.IGNORECASE)
                m8 = re.match(".*UDR_write.*", testname, re.IGNORECASE)
                m9 = re.match("Final_Binning", testname, re.IGNORECASE)

                if m0 :
                    pass
                elif m1 :
                    # Power shorts Pre
                    testname = "PS_PRE_" + m1.group(1)
                    data.at[dflinenum, testname] = testvalue
                    data.at[dflinenum, testname+":USL"] = USL
                    data.at[dflinenum, testname+":LSL"] = LSL
                    data.at[dflinenum, testname+":Unit"] = Units
                    # parse test name and store
                elif m2 :
                    # Power shorts Post not very interesting for now
                    pass
                elif m3 :
                    testname = "IDDQ_1"
                    data.at[dflinenum, testname] = testvalue
                    data.at[dflinenum, testname+":LSL"] = LSL
                    data.at[dflinenum, testname+":Unit"] = Units
                elif m4 :
                    testname = "PVT_TEMP"
                    data.at[dflinenum, testname] = testvalue
                    data.at[dflinenum, testname+":Unit"] = Units
                elif m5 :
                    testname = "PMRO_SVT"
                    data.at[dflinenum, testname] = testvalue
                elif m6 :
                     testname = "PMRO_LVT"
                     data.at[dflinenum, testname] = testvalue
                elif m7 :
                     testname = "PMRO_UVT"
                     data.at[dflinenum, testname] = testvalue
                elif m8 :
                     # Get the prefuses in a set and compare with postfuses # for TP validation
                     if re.search("read_udr.*UDR_write",matches[7], re.IGNORECASE) :
                         match = re.search("write_([0-9]+)", matches[7], re.IGNORECASE)
                         if match is not None:
                            fusenum = match.group(0)
                            fuseval = matches[6]
                            preset.add(str(fusenum)+":"+str(fuseval))
                            #print("adding pre", fusenum, fuseval)
                     if re.search("update_udr.*UDR_write",matches[7], re.IGNORECASE) :
                         match = re.search("write_([0-9]+)", matches[7], re.IGNORECASE)
                         if match is not None:
                            fusenum = match.group(0)
                            fuseval = matches[6]
                            postset.add(str(fusenum)+":"+str(fuseval))
                            #print("adding post", fusenum, fuseval)
                elif m9 :
                    match = re.search(":([A-Z,0-9,_]+)", matches[7], re.IGNORECASE)
                    if match is not None:
                        sBinName = match.group(1)
                elif int(passfail) >= 0 and re.search("INIT|PLL_|ATPG|BIST|BIRA", line, re.IGNORECASE) and re.match("PTR",line):
                     origtestname = re.sub("@.*","",testname)
                     origtestname = re.sub("\[.*","",origtestname)
                     #print("Here with origtestname", origtestname)
                     testname = re.sub("^[^:]*:","",testname)
                     testname = re.match("([^@]+)",testname)
                     testname = testname.group(1)
                     if (int(passfail) >0) : passfail = 1

                     match = re.search("_max", origtestname, re.IGNORECASE)
                     if match is not None:
                        testVoltage = "HV"
                     else :
                        testVoltage = "LV"

                     data.at[dflinenum, "SKU_"+testname+":"+testVoltage] = int(passfail)
                     # Interpret the testname as

                     testType, testBlock, testQuad, testStage =  ("",) * 4
                     if re.search("_BIST_", testname, re.IGNORECASE) and not re.search("INIT|PLL", testname, re.IGNORECASE): testType = "BIST"
                     elif re.search("_BIRA_", testname, re.IGNORECASE) : testType = "BIRA"
                     elif re.search("CHAIN", testname, re.IGNORECASE) : testType = "CHAIN"
                     elif re.search("SACMP", testname, re.IGNORECASE) : testType = "ATPG"
                     elif re.search("SAUNC", testname, re.IGNORECASE) : testType = "ATPG"
                     else : testType = "OTHER"

                     # Reorder block parsing because group name in test is wrong (contains PAR)
                     if re.search("ETH400GMAC", origtestname, re.IGNORECASE) :
                         testBlock = "MAC"
                         match = re.search("(Q[0-3])", origtestname, re.IGNORECASE)
                         testQuad = match.group(1)
                     elif re.search("ETH400GPCS", origtestname, re.IGNORECASE) :
                         testBlock = "PCS"
                         match = re.search("(Q[0-3])", origtestname, re.IGNORECASE)
                         testQuad = match.group(1)
                     elif re.search("MAU", origtestname, re.IGNORECASE) :
                         testBlock = "MAU"
                         match = re.search("(Q[0-3])", origtestname, re.IGNORECASE)
                         testQuad = match.group(1)
                     elif re.search("PAR", origtestname, re.IGNORECASE) or re.search("tcam", origtestname, re.IGNORECASE):
                         testBlock = "PAR"
                         match = re.search("(Q[0-3])", origtestname, re.IGNORECASE)
                         testQuad = match.group(1)
                     elif re.search("_tm_bist_", origtestname) or re.search("scan.*_tm", origtestname, re.IGNORECASE):
                         testBlock = "TM"
                         testQuad = "TM"
                     elif re.search("extest", origtestname, re.IGNORECASE) :
                         testBlock = "EXTEST"
                         match = re.search("_(Q[0-3])", origtestname, re.IGNORECASE)
                         testQuad = match.group(1)
                     else :
                         testBlock = "OTHER"
                         testQuad = "OTHER"

                     if testBlock == "MAU" and (testType == "BIST" or testType == "BIRA") :
                         match = re.search("MAU([0-9]+)", origtestname, re.IGNORECASE)
                         if match is not None:
                             testStage = match.group(1)
                         else : print("here 1 with error for ", origtestname)
                     elif testBlock == "MAU" :
                         match = re.search("_[0-9]{2}([0-9]+)$", origtestname)
                         if match is not None:
                             testStage = match.group(1)
                         else : print("here 2 with error for ", origtestname)
                     else : pass

                     if "SKU_"+testname+":"+testVoltage not in testTypes.head().values :
                         testTypes.at["SKU_"+testname+":"+testVoltage, "Type"] = testType
                         testTypes.at["SKU_"+testname+":"+testVoltage, "Block"] = testBlock
                         testTypes.at["SKU_"+testname+":"+testVoltage, "Quad"] = testQuad
                         testTypes.at["SKU_"+testname+":"+testVoltage, "Stage"] = testStage
                         testTypes.at["SKU_"+testname+":"+testVoltage, "Voltage"] = testVoltage


                else :
                    #print("unknown test : ", testname)
                    pass

            if re.match("PRR", line) and partStart>0:

                hBin = matches[5]
                sBin = matches[6]
                xCoord = matches[7]
                yCoord = matches[8]
                testTime = matches[9]

                data.at[dflinenum,"HBIN"] = hBin
                data.at[dflinenum,"SBIN"] = sBin
                data.at[dflinenum,"SBINName"] = sBinName
                sBinName = ""
                data.at[dflinenum,"Wafer"] = waferNum
                data.at[dflinenum,"X"] = xCoord
                data.at[dflinenum,"Y"] = yCoord
                data.at[dflinenum,"TestTime(mS)"] = testTime

                #pprint.pprint(data)
                print(xCoord,yCoord," is done")

                partStart = 0;
                # Reset part information
                # Check for fuse value differences
                #print(sorted(postset.difference(preset)))
                #data.at[dflinenum, "CheckBin"] = skuCheck(data)


            line = file_object.readline()

    prelen = len(data)
    data = data.drop_duplicates(subset=['X','Y'], keep='last')
    postlen = len(data)

    data = biraAnalysisPerTest(data, mBistMaps)

    postpostlen = len(data)
    print("here with ", prelen, postlen, postpostlen)
    return (data, testTypes)
    # get hte list of columns for TM, then get the dataset from the DF
    #TMnames = testTypes[testTypes['Block']=='TM']
    #for x in TMnames.head().values : print(x)
    #print(TMnames)

def main (argv) :

    infile = ""
    analysis = "Yield"

    try:
        opts, args = getopt.getopt(argv, "hi:a:m:")
    except :
        print("Need both the input atdf and the analysis type")
        exit(1)
    for opt, arg in opts:
        if opt in ("-i") :
            infile = arg
        elif opt in ("-a") :
            analysis = arg
        elif opt in ("-m") :
            mbistMappingFile = arg
        elif opt in ("-h") :
            print('''Usage : python3 BXDParser.py -i test.atdf -a perBlockQuad -m T4WP29/BIST_BIRA_Mapping.csv
                     Options : -i  The input atdf file 
                               -a  The analysis type 
                                   perTest : eg. Chain, BIST,..
                                   perTestBlock : eg. TM Chain, PAR BIST, ...
                                   perTestBlockQuad : eg. TM Chain, PAR BIST Q0, ...
                                   perBlockQuad  : eg. TM, PAR Q0, MAU Q1 etc
                               -m  The file containing BIST to BIRA regex mapping
                               -h  This help
                               -w  Parse and format WAT data, with basic analysis
                ''')



    #infile = sys.argv[1]
    data = pd.DataFrame()
    testTypes = pd.DataFrame()
    # load in the BIST/BIRA maps statically
    #mBistMaps = pd.read_csv('T4WP29/BIST_BIRA_Mapping.csv')
    mBistMaps = pd.read_csv(mbistMappingFile)
    # Removing the #(old) pattern from list
    mBistMaps = mBistMaps[~mBistMaps["BIST"].str.startswith('#')]


    if os.path.exists(infile):

        data, testTypes = parse_CP_atdf(infile, data, testTypes, mBistMaps)
        # Now do the analysis for

        if analysis == "Yield":
            datafileout = infile + ".yield.csv"
            returnval = yieldCount(data)
            returnval.to_csv(datafileout)

        elif analysis == "PatternList":
            pass
        elif analysis == "perTest":
            datafileout = infile + ".perTest.csv"
            returnval = yieldLossPerTest(data,testTypes)
            returnval.to_csv(datafileout)

        elif analysis == "perTestBlock":
            datafileout = infile + ".perTestBlock.csv"
            returnval = yieldLossPerTestBlock(data,testTypes)
            returnval.to_csv(datafileout)

        elif analysis == "perTestBlockQuad":
            datafileout = infile + ".perTestBlockQuad.csv"
            returnval = yieldLossPerTestBlockQuad(data,testTypes)
            returnval.to_csv(datafileout)
            pass
        elif analysis == "perBlockQuad":
            datafileout = infile + ".perBlockQuad.csv"
            returnval = yieldLossPerBlockQuad(data,testTypes)
            returnval.to_csv(datafileout)
            pass
        else: # Do all the analysis and print out the data as well
            datafileout = infile + ".yield.csv"
            returnval = yieldCount(data)
            returnval.to_csv(datafileout)
            datafileout = infile + ".perTest.csv"
            returnval = yieldLossPerTest(data,testTypes)
            returnval.to_csv(datafileout)
            datafileout = infile + ".perTestBlock.csv"
            returnval = yieldLossPerTestBlock(data,testTypes)
            returnval.to_csv(datafileout)
            datafileout = infile + ".perTestBlockQuad.csv"
            returnval = yieldLossPerTestBlockQuad(data,testTypes)
            returnval.to_csv(datafileout)
            datafileout = infile + ".perBlockQuad.csv"
            returnval = yieldLossPerBlockQuad(data,testTypes)
            returnval.to_csv(datafileout)
            pass

        datafileout = infile + ".csv"
        data.to_csv(datafileout)
        datafileout = infile + ".testtypes.csv"
        testTypes.to_csv(datafileout)

if __name__ == "__main__":
    main(sys.argv[1:])


'''
CP STDF file name format : "_" separator
STDF..| Tester Number | Lot | Wafer | Product | Product Rev | Step | Program version | DateTime

FT STDF file name format : "_" separator
'''

'''
NOTES : 
1. Get the yield of other bins : These are unidentified tests or not in known buckets (eg. IDDQ fail)
2. Note there can be multiple MAU patterns for a single MAU0 stage 2, so for SKU or for MAU calculations, need to collapse
'''
