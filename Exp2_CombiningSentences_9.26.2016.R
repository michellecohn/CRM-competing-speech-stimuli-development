#############################
# 
# Experiment 2: Monotone CRM sentences
#
# Combining sentences together
#
# 9.26.2016
#
############################

library(phonTools)
library(PraatR)
library(audio)

dB = 70

# Define number of pitch levels
npitch = 6

# Define conditions (i.e., number vs. color trials)
cond = c("COLOR", "NUMBER")
ncond = length(cond)

# Color ID labels
colornum = c(0:3)
numberIDs = c(1, 2, 3, 4)
nnumbers = length(numberIDs)
ncolors = length(colornum)
colorIDs = c("blue", "red", "white", "green")


callsignIDs = c("charlie", "ringo", "laker", "hopper", "arrow", "tiger", "eagle")
callsignNUMs = c(0, 1, 2, 3, 4, 5, 6)



# Dataframes to store information about the number-callsign, color, and # correspondences
callsign.dataframe = data.frame(callsignIDs, callsignNUMs)
color.dataframe = data.frame(colornum, colorIDs)


#### Define directories #######
targetdirectory = "/Users/michellecohn/Desktop/Exp2_Stimuli/Individual_Sentences/Cond1_Target(MONO)"
maskerdirectory = "/Users/michellecohn/Desktop/Exp2_Stimuli/Individual_Sentences/Cond1_Mask(MONO)"

# Combined sentence output dir
combinedsentencedirectory = "/Users/michellecohn/Desktop/Exp2_Stimuli/Combined_Sentences"

# Eprime logfile output directory
eprimedirectory = "/Users/michellecohn/Desktop/Exp2_Stimuli/EPRIME"
eprimepracticeprocedure = "practice"
eprimeprocedure = "exp"


# Define functions (for later use in praatR)
targetPath = function(FileName){ return( paste( sprintf("%s/", targetdirectory), FileName, sep="") ) }
maskerPath = function(FileName){ return( paste( sprintf("%s/", maskerdirectory), FileName, sep="") ) }

SentenceOutputPath = function(FileName){ return( paste( sprintf("%s/", combinedsentencedirectory), FileName, sep="") ) }



#################


# Get target filenames to loop
filenames = noquote(list.files(path = targetdirectory, pattern = ".wav"))
nfiles = length(filenames)

# Get masker filenames
maskerfiles = noquote(list.files(path = maskerdirectory, pattern = ".wav"))
nmaskers = length(maskerfiles)




###############################################
#
# Get target soundfile information and save to a matrix (to later access for eprime logging)
#
###########################################################################################

# iterations = nfiles
# variables = 5 # Soundfile ID, callsign, color, number, targetpitch

#target_matrix =  matrix(ncol=variables, nrow=iterations)


# Eprime logging
setwd(eprimedirectory)
hdr = "Weight\tNested\tProcedure\tSoundFile\tCorrectResponse\tPitchLevel\tExperimentVersion\tTargetCallSign\tTargetColor\tTargetNumber\tMaskerCallSign\tMaskerColor\tMaskerNumber\t"
header = noquote(hdr)

single_hdr = "Weight\tNested\tProcedure\tSoundFile\tCorrectResponse\tPitchLevel\tExperimentVersion\tTargetCallSign\tTargetColor\tTargetNumber\t"
single_header = noquote(single_hdr)


write(header, file = sprintf("E-prime_Combined_Sentences_COLOR.txt"), append = FALSE) # For colors
write(header, file = sprintf("E-prime_Combined_Sentences_NUMBER.txt"), append = FALSE) # For numbers
write(single_header, file = sprintf("E-prime_Single_Talker_NUMBER.txt"), append = FALSE) # For numbers
write(single_header, file = sprintf("E-prime_Single_Talker_COLOR.txt"), append = FALSE) # For numbers

# Create 6 separate e-prime lists for color/number for the practice trials
for(icond in 1:ncond){ # For each condition 
  for(ipitch in 1:npitch){
    
    condition_ID = cond[icond]
    write(header, file = sprintf("E-prime_Single_Talker_Practice_%s_Pitch%s.txt", condition_ID, ipitch), append = FALSE) # For numbers
  

  }#endfor icond
} #endfor



# Loop through all target
for(s in 1:nfiles){ # loop through target directory
  
  setwd(targetdirectory)
  
  currTarget = filenames[s]
  
  # Save information about each file
  targetID = substr(currTarget, 1, 13) # Filename minus .wav
  targetCallSign = substr(currTarget, 2, 2) # Get current call sign
  targetColor = substr(currTarget, 4, 4) # Get current color
  eprime_targetColor = as.numeric(targetColor) + 1
  targetNumber = substr(currTarget, 6, 6) # Get current number
  eprime_targetNumber = as.numeric(targetNumber)+1
  targetPitch = substr(currTarget, 13, 13)
  
 # target_matrix[s,] = c(targetID, targetCallSign, targetColor, targetNumber, targetPitch)
  
  
  #### Log information to EPrime .txt file #####
  setwd(eprimedirectory)
  single_procedure = noquote(eprimepracticeprocedure)
  
  for(iversion in 1:2){
    condition_ID = cond[iversion]
    
    if(iversion == 1){
      CorrectResponse = eprime_targetColor
      line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", single_procedure, currTarget, CorrectResponse, targetPitch, condition_ID, targetCallSign, eprime_targetColor, eprime_targetNumber)
      write(line, file = "E-prime_Single_Talker_COLOR.txt", append = TRUE)
      
    } #end if version 1
    ############################################################################
    if(iversion == 2){ # For number
      
      CorrectResponse = eprime_targetNumber
      # WordID = numberIDs
      line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", single_procedure, currTarget, CorrectResponse, targetPitch, condition_ID, targetCallSign, eprime_targetColor, eprime_targetNumber)
      write(line, file = "E-prime_Single_Talker_NUMBER.txt", append = TRUE)
      
      
      # Log practice trials
      
    } #endif number
    
  
  
  } # endfor iversion

} # endfor file loop




###############################################
#
# Get subset of masker files 
#
# Organize in a dataframe
#
###########################################################################################

iterations = nmaskers
variables = 4 # Soundfile ID, callsign, color, number

masker_matrix =  matrix(ncol=variables, nrow=iterations)


# Loop through all maskers
for(r in 1:nmaskers){ # loop through masker directory
  
  setwd(maskerdirectory)
  
  currMasker = maskerfiles[r]
  
  # Save information about each file
  maskerID = substr(currMasker, 1, 6) # Filename minus .wav
  maskerCallSign = substr(currMasker, 2, 2) # Get current call sign
  maskerColor = substr(currMasker, 4, 4) # Get current color
  maskerNumber = substr(currMasker, 6, 6) # Get current number
  
      
    masker_matrix[r,] = c(maskerID, maskerCallSign, maskerColor, maskerNumber)
  

} # endfor


########################################################################################

# LOGGING #

######
# Keep track of what soundfiles have already been paired
used_maskers = zeros(1,1)
#####


########################################################################################



setwd(targetdirectory)
for(q in 1:nfiles){
  
  setwd(targetdirectory)
  currTarget = filenames[q]  # replace '20' with q for full loop
  # target = loadsound(currTarget)  # Had issues with unused connections, readChar
  
  
  # Save information about each file
  targetID = substr(currTarget, 1, 13) # Filename minus .wav
  targetCallSign = substr(currTarget, 2, 2) # Get current call sign
  targetColor = substr(currTarget, 4, 4) # Get current color
  eprime_targetColor = as.numeric(targetColor)+1
  targetNumber = substr(currTarget, 6, 6) # Get current number
  eprime_targetNumber = as.numeric(targetNumber)+1
  currPitch = substr(currTarget, 13, 13)

  
  # Load the soundfile if the number is #1-4
  
if(targetNumber <4){
#  if((targetNumber = 0) | (targetNumber = 1) | (targetNumber = 2) | (targetNumber = 3)){
    
    target = load.wave(currTarget)
    
    
    ##########################################
    # Load the masker soundfile if
    #     1) the call sign is not 'charlie' (0)
              callsign_logical = masker_matrix[,2] != 0
              
    #     2) the number is from #1-4 (0-3)
              number_logical = masker_matrix[,4] <4
            
    #     3) The color is not the same as the target
              color_compare_logical = masker_matrix[,3] != targetColor
   
    #     4) The number is not the same as the target
              number_compare_logical = masker_matrix[,4] != targetNumber
              
              
    masker_selection = which (callsign_logical & number_logical & color_compare_logical & number_compare_logical,  arr.ind=T )
    
    randomasker = sample(masker_selection, 1) # Get a random sample
    
    # Get the soundfile name from the masker_matrix (first column)
    currMaskerFile= masker_matrix[randomasker[1],1]
    
    
    # Get info from Masker file for e-prime logging
    MaskerCallSign = substr(currMaskerFile, 2, 2)
    maskerColor = substr(currMaskerFile, 4, 4)
    eprime_maskerColor = as.numeric(maskerColor)+1
    maskerNumber = substr(currMaskerFile, 6, 6)
    eprime_maskerNumber = as.numeric(maskerNumber)+1

    
      
    ### Make sure to get a masker that hasn't previously been paired
    while (subset(used_maskers) == currMaskerFile){
      
      randomasker = sample(masker_selection, 1) # Get another random sample
      currMaskerFile= masker_matrix[randomasker[1],1] # Index masker filename
      if (subset(used_maskers) != currMaskerFile) break

    } # end while 
  

    # Add the .wav
    currMasker = paste (currMaskerFile, ".wav", sep = "", collapse = NULL)
    
        # Load the masker soundfile
        setwd(maskerdirectory)
        masker = load.wave(currMasker)
        
        combinedsound = target + masker # Combine the two sounds
        
       
        setwd(combinedsentencedirectory)
        combinedfilename = sprintf("%s_%s.wav", targetID, currMaskerFile)
        writesound(combinedsound,filename = sprintf("%s_%s.wav", targetID, currMaskerFile), fs = 40000)
        
        
        # Scale intensity of each soundfile
        praat("Scale intensity...", arguments=list(dB), input=SentenceOutputPath(sprintf("%s_%s.wav", targetID, currMaskerFile)), output=SentenceOutputPath(sprintf("%s_%s.wav", targetID, currMaskerFile)), filetype="WAV", overwrite=TRUE )
              
        
        used_maskers = rbind(used_maskers, currMaskerFile) # Add file name to used list 
        
        setwd(maskerdirectory)
        
        
        #################################################
        # Write info to logfiles (to later use in Eprime)
        
        setwd(eprimedirectory)
        combinedSoundfile = sprintf("%s_%s.wav", targetID, currMaskerFile)
        currPitch = substr(currTarget, 13, 13)
        procedure = noquote(eprimeprocedure)
     
        
      
        for(iversion in 1:2){ # Keep track of colors
          condition_ID = cond[iversion]
          
            if(iversion == 1){
              CorrectResponse = eprime_targetColor
             # WordID = colorIDs

            line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", procedure, combinedfilename, CorrectResponse, currPitch, condition_ID, targetCallSign, eprime_targetColor, eprime_targetNumber, MaskerCallSign, eprime_maskerColor, eprime_maskerNumber)
            write(line, file = "E-prime_Combined_Sentences_COLOR.txt", append = TRUE)
      
            # Log files and attributes for practice (6 levels)
            write(line, file = sprintf("E-prime_Single_Talker_Practice_%s_Pitch%s.txt", condition_ID, currPitch), append = TRUE) 
            
      
            } # endif
          
          
            if(iversion == 2){ #Keep track of numbers
              CorrectResponse = eprime_targetNumber
              # WordID = numberIDs
              line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", procedure, combinedfilename, CorrectResponse, currPitch, condition_ID, targetCallSign, eprime_targetColor, eprime_targetNumber, MaskerCallSign, eprime_maskerColor, eprime_maskerNumber)
              write(line, file = "E-prime_Combined_Sentences_NUMBER.txt", append = TRUE)
              
              # Log files and attributes for practice (6 levels)
              write(line, file = sprintf("E-prime_Single_Talker_Practice_%s_Pitch%s.txt", condition_ID, currPitch), append = TRUE) 
              
              
            } # endif
          
       
   
        } #endfor versions
        
        
      } #end if
  
      
    } #endfor
