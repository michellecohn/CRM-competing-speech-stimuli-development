#############################
# 
# Experiment 2: Monotone CRM sentences
#
# Combining sentences together -- CRM Screen selection (with mouse)
#
# By Michelle Cohn 
# Last updated 11.10.2016 - Updated for waveform addition (no recycling)
#
############################

# Loop through until each masker is used *only* once


repeat{ # 96 maskers

# Clear all variables 
rm(list=ls())
  

library(phonTools)
library(PraatR)
library(audio)

###############################
#
# Define variables 
# 
###############################

dB = 70
samplingrate = 40000
npitch = 6
nvars = 3 # Callsign, color, number 

# Color & # ID info
colornum = c(0:3)
numberIDs = c(1, 2, 3, 4)
nnumbers = length(numberIDs)
ncolors = length(colornum)
colorIDs = c("blue", "red", "white", "green")
targetCallSignID = "baron"
callsignIDs = c("charlie", "ringo", "laker", "hopper", "arrow", "tiger", "eagle")
callsignNUMs = c(0, 1, 2, 3, 4, 5, 6)

# Dataframes to store information about the number-callsign, color, and # correspondences
callsign.dataframe = data.frame(callsignIDs, callsignNUMs)
color.dataframe = data.frame(colornum, colorIDs)


###############################
#
# Define directory paths 
# 
###############################

targetdirectory = "/Users/michellecohn/Desktop/Exp2_CRM_Mouse/Individual_Sentences/Cond1_Target(MONO)"
maskerdirectory = "/Users/michellecohn/Desktop/Exp2_CRM_Mouse/Individual_Sentences/Cond1_Mask(MONO)"

# Combined sentence output dirCRM
combinedsentencedirectory = "/Users/michellecohn/Desktop/Exp2_CRM_Mouse/Combined_Sentences"

# Eprime logfile output directory
eprimedirectory = "/Users/michellecohn/Desktop/Exp2_CRM_Mouse/EPRIME"
eprimepracticeprocedure = "practice"
eprimeprocedure = "exp"


# Define functions (for later use in praatR)
targetPath = function(FileName){ return( paste( sprintf("%s/", targetdirectory), FileName, sep="") ) }
maskerPath = function(FileName){ return( paste( sprintf("%s/", maskerdirectory), FileName, sep="") ) }
SentenceOutputPath = function(FileName){ return( paste( sprintf("%s/", combinedsentencedirectory), FileName, sep="") ) }

###############################
#
# Get target & masker filenames (to later loop)
# 
###############################

filenames = noquote(list.files(path = targetdirectory, pattern = ".wav"))
nfiles = length(filenames)

# Get masker filenames
maskerfiles = noquote(list.files(path = maskerdirectory, pattern = ".wav"))
nmaskers = length(maskerfiles)


##############################################################
#
# Initialize Eprime logging .txt files
#
##############################################################

eprimedirectory = "/Users/michellecohn/Desktop/Exp2_CRM_Mouse/EPRIME"
eprimepracticeprocedure = "practice"
eprimeprocedure = "exp"

setwd(eprimedirectory)
hdr = "Weight\tNested\tProcedure\tSoundFile\tCorrectResponse\tPitchLevel\tTargetCallSign\tTargetColor\tTargetNumber\tMaskerCallSign\tMaskerColor\tMaskerNumber\tSelectedColor\tSelectedNumber\tColorCorrect\tNumberCorrect\tSelectedMaskerColor\tSelectedMaskerNumber\tTargetDuration\tMaskerDuration\tAdjustment\tFinalDuration\t"
header = noquote(hdr)

single_hdr = "Weight\tNested\tProcedure\tSoundFile\tCorrectResponse\tPitchLevel\tTargetCallSign\tTargetColor\tTargetNumber\tSelectedColor\tSelectedNumber\tColorCorrect\tNumberCorrect\tTargetDuration\t"
single_header = noquote(single_hdr)


## Single sentence trials ##
single_sentence_textfile = sprintf("E-prime_Single_Sentences.txt")
write(single_header, file = single_sentence_textfile, append = FALSE) 

## Combined sentence trials ##
combined_sentence_textfile = sprintf("E-prime_Combined_Sentences.txt")
write(header, file = combined_sentence_textfile, append = FALSE) 

## Create 6 separate e-prime lists for the practice trials (6 pitch levels) ##
for(ipitch in 1:npitch){
  write(header, file = sprintf("E-prime_Single_Talker_Practice_Pitch%s.txt", ipitch), append = FALSE) # For numbers
} #endfor


##############################################################
#
# Pre-loop through all maskers, saving their info to a matrix
#
##############################################################

masker_matrix =  matrix(ncol=nvars+1, nrow=nmaskers) # variables = 3 (callsign, color, #) & Soundfile ID

possible_masker_matrix =  matrix(ncol=nvars+1, nrow=nmaskers) # variables = 3 (callsign, color, #) & Soundfile ID
possible_masker_counter = 0

for(r in 1:nmaskers){ # loop through masker directory
  
  setwd(maskerdirectory)
  currMasker = maskerfiles[r]
  
  # Save information about each file
  maskerID = substr(currMasker, 1, 6) # Filename minus .wav
  maskerCallSign = substr(currMasker, 2, 2) # Get current call sign
  maskerColor = substr(currMasker, 4, 4) # Get current color
  maskerNumber = substr(currMasker, 6, 6) # Get current number
  
  masker_matrix[r,] = c(maskerID, maskerCallSign, maskerColor, maskerNumber)
  

    if((maskerCallSign != 0) & (maskerNumber < 4)){

    # Add masker to the list of possible maskers
    possible_masker_counter = possible_masker_counter + 1
    
    possible_masker_matrix[possible_masker_counter,] = c(maskerID, maskerCallSign, maskerColor, maskerNumber)
    
  }#endif
    
  
} # endfor


##############################################################
#
# Keep track of what soundfiles have already been paired
#
##############################################################

used_maskers = zeros(nmaskers,1)

# Start a counter to use to index the appropriate row to add in the used masker filenames
counter = 0

# while(unique(used_maskers < 97)){
  
ntimes_maskers_used = table(used_maskers[1:96,1])


###################################################
#
# LOOP through target files
#
###################################################


for(itarget in 1:nfiles){
  
  
    setwd(targetdirectory)
  
    currTarget = sprintf("%s", filenames[itarget])
    targetNumber = substr(currTarget, 6, 6) # Get current number

    if(targetNumber < 4){ # Only include target # from 1-4 (coded 0-3)
      

      ###################################################
      #
      # Load the target files, get their info 
      #
      ###################################################
      
      singletarget = load.wave(sprintf("%s",currTarget))
      nsamples_target = length(singletarget)  # 67729 samples
      singletarget_duration = length(singletarget)/samplingrate # Duration = 1.693225 seconds
      
      # Target file info
      targetID = substr(currTarget, 1, 13) # Filename minus .wav
      targetCallSign = substr(currTarget, 2, 2) # Get current call sign
      targetColor = substr(currTarget, 4, 4) # Get current color
      eprime_targetColor = as.numeric(targetColor)+1
      targetColorID = colorIDs[eprime_targetColor]
      targetNumber = substr(currTarget, 6, 6) # Get current number
      eprime_targetNumber = as.numeric(targetNumber)+1
      targetNumberID = eprime_targetNumber
      currPitch = substr(currTarget, 13, 13)
      
      #Target color & number info (e.g., blue4) to use later when indexing the images from Eprime slides (e.g., blue4.)
      targetColorNumberJPG= sprintf("%s%s", targetColorID, targetNumberID)
      CorrectResponse = targetColorNumberJPG
    
      
      
      ###################################################
      #
      # Log single sentence info to EPRIME .txt file 
      #
      ###################################################
    
      setwd(eprimedirectory)
      single_procedure = noquote(eprimepracticeprocedure)
      singletarget_duration = length(singletarget)/samplingrate
        
      line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t\t\t\t%s\n", single_procedure, currTarget, CorrectResponse, currPitch, targetCallSignID, targetColorID, targetNumberID, singletarget_duration)
      write(line, file = single_sentence_textfile, append = TRUE)
        
      
        ###################################################
        #
        # Find and load in the appropriate maskers
        #
        ###################################################
        
        setwd(maskerdirectory)
 
        
        # Load the masker soundfile if
        #     1) the call sign is not 'charlie' (0)
        #    callsign_logical = masker_matrix[,2] != 0
        
        #     2) the number is from #1-4 (0-3)
        # number_logical = masker_matrix[,4] < 4
        
        #     3) The color is not the same as the target
        #color_compare_logical = masker_matrix[,3] != targetColor
        
        #     4) The number is not the same as the target
        #number_compare_logical = masker_matrix[,4] != targetNumber
        
        #     5) If the masker hasn't already been used (Indicate which maskers fm the file have already been paired)
        # not_used_masker_logical = (!(masker_matrix[,1] %in% used_maskers[,1])) 
        
        # masker_selection = which(callsign_logical & number_logical & color_compare_logical & number_compare_logical & not_used_masker_logical,  arr.ind=T )
        

        #     (1) The color is not the same as the target
        color_compare_logical = possible_masker_matrix[,3] != targetColor
        
        #     (2) The number is not the same as the target
        number_compare_logical = possible_masker_matrix[,4] != targetNumber
        
        #     (3) If the masker hasn't already been used (Indicate which maskers fm the file have already been paired)
         not_used_masker_logical = (!(possible_masker_matrix[,1] %in% used_maskers[,1])) 
        


        masker_selection = which(color_compare_logical & number_compare_logical & not_used_masker_logical,  arr.ind=T )
       
        nmasker_options = length(masker_selection)
        
        
        # If there aren't any options, repeat the loop
        if(nmasker_options == 0){ 
          next
        }
          
      
        
        print(sprintf("masker selection: %s options available", nmasker_options))
        
        
        # Get a random index from the eligible maskers
         randomasker = sample(masker_selection, 1)
         currMaskerFile = possible_masker_matrix[randomasker]
         
         # Add the .wav
         currMasker = paste(currMaskerFile, ".wav", sep = "", collapse = NULL)
         maskerID = substr(currMasker, 1, 6) # Filename minus .wav
      
        
        
        ##### Load the masker ########
        singlemasker = load.wave(sprintf("%s",currMasker))
        nsamples_masker = length(singlemasker) # 64702 samples
        
        masker_duration = length(singlemasker)/samplingrate # Duration = 1.61755
        
        #### Log the info to the 'used_maskers' matrix #####
        counter = counter + 1
        used_maskers[counter] = maskerID
    
        
          ###################################################
          #
          # Adjust number of samples to be equal (to avoid recycling) 
          #
          ###################################################
          
          if(nsamples_target > nsamples_masker){
            nzeros_to_add = nsamples_target - nsamples_masker  # 3027 zeros to add 
            difference_in_seconds = nzeros_to_add/samplingrate
            zeros_vector = zeros(nzeros_to_add)
            adjusted_masker = c(singlemasker, zeros_vector)
            length_adjusted_masker = length(adjusted_masker)
            adjusted_sound = "masker"
            
          
            if(nsamples_target == length_adjusted_masker){
              print("Lengths are equal")
            } else {
              print("Not equal samples - TARGET longer")
            } #end if
            
           # play(singletarget + adjusted_masker)
            
            combined_sentences = singletarget + adjusted_masker
            final_duration = length(combined_sentences)/samplingrate
            adjusted = sprintf("added %s sec. to %s", difference_in_seconds, adjusted_sound) # Keep track of which sound was adjusted and by how many sec.
            
          } #endif
          
          
          
          if(nsamples_target < nsamples_masker){
            nzeros_to_add = nsamples_masker - nsamples_target
            difference_in_seconds = nzeros_to_add/samplingrate
            zeros_vector2 = zeros(nzeros_to_add)
            adjusted_target = c(singletarget, zeros_vector2)
            length_adjusted_target= length(adjusted_target)
            adjusted_sound = "target"
            
            
            if(nsamples_masker == length_adjusted_target){
              print("Lengths are equal")
            } else {
              print("Not equal samples - MASKER longer")
            } #end if
            
            # play(singletarget + adjusted_masker)
            
            combined_sentences = singlemasker + adjusted_target
            final_duration = length(combined_sentences)/samplingrate
            adjusted = sprintf("added %s sec. to %s", difference_in_seconds, adjusted_sound) # Keep track of which sound was adjusted and by how many sec.
            
          } #endif
    
          
        if(nsamples_target == nsamples_masker){
          nzeros_to_add = nsamples_masker - nsamples_target
          difference_in_seconds = nzeros_to_add/samplingrate
          adjusted_sound = "neither"
          
  
          combined_sentences = singlemasker + singletarget
          final_duration = length(combined_sentences)/samplingrate
          adjusted = sprintf("added %s sec. to %s", difference_in_seconds, adjusted_sound) # Keep track of which sound was adjusted and by how many sec.
          
        } #endif
        
        
        
        
      ###################################################
      #
      # Write the adjusted sound to a new .wav file
      #
      ###################################################
        setwd(combinedsentencedirectory)
        combinedfilename = sprintf("%s_%s.wav", targetID, maskerID) 
        writesound(combined_sentences,filename = sprintf("%s_%s.wav", targetID, maskerID), fs = 40000)
        
        praat("Scale intensity...", arguments=list(dB), input=SentenceOutputPath(sprintf("%s_%s.wav", targetID, maskerID)), output=SentenceOutputPath(sprintf("%s_%s.wav", targetID, maskerID)), filetype="WAV", overwrite=TRUE )
        
        
      ###################################################
      #
      # Log combined sentence info to EPRIME .txt file 
      #
      ###################################################

      setwd(eprimedirectory)
      # eprimeprocedure
      MaskerCallSignNum = as.numeric(noquote(masker_matrix[randomasker,2]))+1
      MaskerCallSignID = callsignIDs[MaskerCallSignNum]
      
      maskerColor = as.numeric(noquote(masker_matrix[randomasker,3]))+1
      maskerColorID = colorIDs[maskerColor]      

      maskerNumber = as.numeric(noquote(masker_matrix[randomasker,4]))+1
      maskerNumberID = maskerNumber
      
        
      line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t\t\t\t\t\t\t%s\t%s\t%s\t%s\n", eprimeprocedure, combinedfilename, CorrectResponse, currPitch, targetCallSignID, targetColorID, targetNumberID, MaskerCallSignID, maskerColorID, maskerNumberID, singletarget_duration, masker_duration, adjusted, final_duration)
      write(line, file = combined_sentence_textfile, append = TRUE)
      
      # Write a separate log file for each pitch leve for practice trials
      write(line, file = sprintf("E-prime_Single_Talker_Practice_Pitch%s.txt", currPitch), append = TRUE) 
      
      
    } #endif target # <4
} #endfor target file loop

# if(unique(used_maskers >= 97)) break 

# } #endif each masker is used once


# Check the number of maskers used (Make sure each is only paired 1x)
# ntimes_maskers_used = table(used_maskers[1:96,1])

unique_maskers = unique(used_maskers)
n_unique_maskers = length(unique_maskers)


if(n_unique_maskers < 96){
  print("Re-run script; a masker has been paired more than once")
  setwd(combinedsentencedirectory)
  combined_filenames = noquote(list.files(path = combinedsentencedirectory, pattern = ".wav"))
  ncombinedfiles = length(combined_filenames)
  
  # Loop through 
  for(ifile in 1:ncombinedfiles){
    currCombinedFile = combined_filenames[ifile]
    if(file.exists(currCombinedFile)) file.remove(currCombinedFile)
  }# endfor
  

} #endif

if(n_unique_maskers >= 96){
  break
  print("Each masker has been paired just once")
  
  print(table(used_maskers[1:96,1]))
  used_masker_frequency = as.data.frame(table(used_maskers[1:96,1]))
  
}


} #end repeat






# 
# 
# 
# # Get a summary of the used maskers 
# count_used_maskers =  matrix(ncol=nvars+1, nrow=nmaskers) # variables = 3 (callsign, color, #) & Soundfile ID
# used_masker_counter = 0
# 
# # Loop through used maskers 
# for(iusedmasker in 1:n_unique_maskers){
#   currUsedMasker = used_maskers[iusedmasker]
#   
#   if(currUsedMasker != 0){
#       used_masker_counter = used_masker_counter +1
#       
#       maskerID = substr(currUsedMasker, 1, 6) # Filename minus .wav
#       maskerCallSign = substr(currUsedMasker, 2, 2) # Get current call sign
#       maskerColor = substr(currUsedMasker, 4, 4) # Get current color
#       maskerNumber = substr(currUsedMasker, 6, 6) # Get current number
#       
#       count_used_maskers[used_masker_counter,] = c(maskerID, maskerCallSign, maskerColor, maskerNumber)
# 
#     
#   }#endif
#   
# } #endfor
# 
# # Get info on # of Call Signs
# used_masker_callsigns = count_used_maskers[,2]
# n_used_masker_callsigns = as.data.frame(table(used_masker_callsigns))
# 
# # Get info on # of each Color
# used_masker_colors = count_used_maskers[,3]
# n_used_masker_colors = as.data.frame(table(used_masker_colors))
# 
# # See if colors have been evenly selected
# n_usedColor1 = n_used_masker_colors[1,]$Freq
# n_usedColor2 = n_used_masker_colors[2,]$Freq
# n_usedColor3 = n_used_masker_colors[3,]$Freq
# n_usedColor4 = n_used_masker_colors[4,]$Freq
# 
# 
# 
# # Get info on # of #s used
# used_masker_numbers = count_used_maskers[,4]
# n_used_masker_numbers = as.data.frame(table(used_masker_numbers))
# 
# # See if colors have been evenly selected
# n_usedNumber1 = n_used_masker_numbers[1,]$Freq
# n_usedNumber2 = n_used_masker_numbers[2,]$Freq
# n_usedNumber3 = n_used_masker_numbers[3,]$Freq
# n_usedNumber4 = n_used_masker_numbers[4,]$Freq


